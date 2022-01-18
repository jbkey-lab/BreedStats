
xgblinearBV = function(){

  library(data.table)
  library(tidyverse)
  library(doParallel)
  library(caret)
  library(caretEnsemble)
  library(stats)
  library(impute)
  library(keras)
  library(RcppCNPy)
  #####################################################

  cores=detectCores()
  cl <- makeCluster(cores[1]-1, outfile="")
  registerDoParallel(cl)

  hdp = "C:/Users/jake.lamkey/Documents/"

  dp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Training/"
  tdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Test Inputs/"
  gdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/"

  #####################################################

  o.train <- fread(paste0(dp,"inputs_others_train.csv"))[,-1]
  #w.train <- fread(paste0(dp,"inputs_weather_train.csv"))[,-1]
  y.train <- fread(paste0(dp,"yield_train.csv"))[,-1]

  o.test <- fread(paste0(tdp,"inputs_others_test.csv"))[,-1]
  #w.test <- fread(paste0(tdp,"inputs_weather_test.csv"))[,-1]
  geno <- fread(paste0(gdp,"clusterID_genotype.csv"))[-1788,-1]

  ######################################################


  BV.HSIdentical.df = fread(paste0(hdp,"BV.HSIdentical.df.csv"))
  BV.HSIdentical.df = BV.HSIdentical.df %>% filter(Plot.Discarded != "Yes",
                                                   Plot.Status != "3 - Bad" )

  BV.HSIdentical.df.2 = BV.HSIdentical.df[,c(1:29,31,30,32:33)]
  colnames(BV.HSIdentical.df.2)[c(30:31)] = c("MALE","FEMALE")

  #trainingx2 = data.frame(BV.HSIdentical.df)

  trainingx2 = rbind(BV.HSIdentical.df.2,BV.HSIdentical.df)
  trainingx2 = data.frame(trainingx2)


  field = (trainingx2[!duplicated(trainingx2$FIELD), c("FIELD",'LINE')])
  field$num = c(1:(nrow(field)+0))
  trainingx2 = left_join(trainingx2, field[,c(1,3)], by="FIELD")
  colnames(trainingx2)[34] = "field"

  field = trainingx2[!duplicated(trainingx2$LINE), c("LINE",'MALE')]
  field$num = c(1:(nrow(field)+0))
  trainingx2 = left_join(trainingx2, field[,c(1,3)], by="LINE")
  colnames(trainingx2)[35] = "ID"

  field = trainingx2[!duplicated(trainingx2$MALE), c("MALE",'LINE')]
  field$num = c(1:(nrow(field)+0))
  trainingx2 = left_join(trainingx2, field[,c(1,3)], by="MALE")
  colnames(trainingx2)[36] = "male"

  field = trainingx2[!duplicated(trainingx2$FEMALE), c("FEMALE",'LINE')]
  field$num = c(1:(nrow(field)+0))
  trainingx2 = left_join(trainingx2, field[,c(1,3)], by="FEMALE")
  colnames(trainingx2)[37] = "female"

  field = trainingx2[!duplicated(trainingx2$YEAR), c("YEAR",'LINE')]
  field$num = c(1:(nrow(field)+0))
  trainingx2 = left_join(trainingx2, field[,c(1,3)], by="YEAR")
  colnames(trainingx2)[38] = "Year"


  trainingx2 = na.omit(trainingx2[,c(22,36,37,35,34,38)]) #yield = 22, plt.height=13

  ID = trainingx2[!duplicated(trainingx2[,4]), 4]
  ID = data.frame(ID)

  idx = sample(nrow(ID), nrow(ID) * .95) #.25

  Loc_Validate = data.frame(ID=ID[-idx, ])
  Loc_Train = data.frame(ID=ID[idx, ])

  ID = o.train[!duplicated(o.train[,2]), 2]
  ID = data.frame(ID)

  idx = sample(nrow(ID), nrow(ID) * 0.9) #.9

  Loc_Validatesoy = data.frame(ID=ID[-idx, ])
  Loc_Trainsoy = data.frame(ID=ID[idx, ])

  trainx1 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Trainsoy, by="ID")

  trainx2 = inner_join(trainingx2, Loc_Train, by="ID") #%>% rbind(trainx1)
  validatex2 = inner_join(trainingx2, Loc_Validate, by="ID")
  validatex3 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Validatesoy, by="ID")

  trainx1 = data.frame(trainx1)  %>% mutate_all(as.numeric)
  trainx2 = data.frame(trainx2)    %>% mutate_all(as.numeric) #%>% unique()
  validatex2 = data.frame(validatex2)    %>% mutate_all(as.numeric)
  validatex3 = data.frame(validatex3)  %>% mutate_all(as.numeric)


  ##################################################################
  final_grid1=expand.grid(nrounds=550, eta=.5, max_depth=3, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)
  #final_grid2=expand.grid(nrounds=100, eta=.5, max_depth=5, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)
  #final_grid1 <- expand.grid(nrounds = 500, eta = .3, lambda = .5, alpha=.9)
  final_grid2 <- expand.grid(nrounds = 550, eta = .5, lambda = .9, alpha=.8)

  final_grid3 <- expand.grid(nrounds = 500, eta = .7, lambda = .5, alpha=.9)
  final_grid4 <- expand.grid(nrounds = 550, eta = .9, lambda = .9, alpha=.8)

  # final_grid3 <- expand.grid(mstop = 500, maxdepth = 2, nu = 0.1)
  # final_grid4 <- expand.grid(committees = 10, neighbors = 20)

  models.list2 <- caretList(
    x=trainx2[ , -1],
    y=(trainx2$Yield),
    continue_on_fail = T,
    trControl=trainControl(method="cv",
                           number=1, #1
                           index = createFolds((trainx2$Yield),k=2), #2
                           savePredictions = TRUE,
                           classProbs=T,
                           allowParallel = TRUE,
                           verboseIter = TRUE
                           #preProcOptions =  c( method = c("center", "scale"))
                           #na.remove = TRUE,
                           # k = 5,
                           # knnSummary = mean,
                           # outcome = NULL,
                           # fudge = 0.2,
                           # numUnique = 3,
                           # verbose = FALSE,
                           # freqCut = 95/5,
                           # uniqueCut = 10,
                            #cutoff = 0.9)
                           # rangeBounds = c(0, 1))
                           #p=.75
                           # seeds=c(1,2,3,4,5,6,7,8,9),
                           # indexFinal = length(sample(nrow(trainx2), (nrow(trainx2))*.3))
    ),
    tuneList=list(
      #  qrf1=caretModelSpec(method="qrf", ntree=500, tuneLength = 1), #11
      #  qrf2=caretModelSpec(method="qrf", ntree=7, tuneLength = 1), #11
      #  qrf3=caretModelSpec(method="qrf", ntree=10, tuneLength = 1), #9
      # # #qrf4=caretModelSpec(method="qrf", ntree = 150, tuneLength = 1), #7
      #qrf5=caretModelSpec(method="qrf", ntree=10, tuneLength = 1), #5
      #qrf6=caretModelSpec(method="xgbTree", tuneGrid = final_grid1), #5

      qrf7=caretModelSpec(method="xgbLinear", tuneGrid = final_grid3), #5
      qrf8=caretModelSpec(method="xgbLinear", tuneGrid = final_grid4) #5

      #qrf9=caretModelSpec(method="BstLm") #5
      #qrf8=caretModelSpec(method="cubist") #5
      # qrf6=caretModelSpec(method="qrf", ntree=2, tuneLength = 1) #5
    )
    # ),
    # methodList = c(
    #   "cubist",
    #   "xgbLinear"
    #
    # )
  )

  invisible(gc())

  models.list2

  # my_control <- trainControl(method = "boot", # for "cross-validation"
  #                            number = 500, # number of k-folds
  #                            #savePredictions = "final",
  #                            allowParallel = TRUE, verbose=T)
  #

  NCAA.stacked<-caretEnsemble(models.list2, # + 95
                              trControl = trainControl(
                                number=2,
                                method="cv",
                                verboseIter =TRUE,
                                allowParallel = T
                              )
  );NCAA.stacked # + 95

  invisible(gc())

  #######Validate Corn  52
  preds = predict(NCAA.stacked, validatex2[,-1])
  cor(validatex2[, 1], preds)^2
  sqrt(mean((validatex2[, 1] -  preds)^2))

  #######training set + 97  68
  preds.t = predict(NCAA.stacked, trainx2[,-1])
  cor(trainx2[, 1], preds.t)^2
  sqrt(mean((trainx2[, 1] -  preds.t)^2))

  #######Soybeans + 99
  preds = predict(NCAA.stacked,  trainx1)
  cor(data.frame(trainx1[,1]), (preds))^2
  sqrt(mean((trainx1[, 1] -  preds)^2))

  #######Validate Soybean 75.1
  preds = predict(NCAA.stacked,  validatex3)
  cor(data.frame(validatex3[,1]), (preds))^2
  sqrt(mean((validatex3[, 1] -  preds)^2))

}







# "blassoAveraged",
# "bridge",
# "gcvEarth",
# "rqnc",
# "rqlasso",
# "relaxo",
# "blasso"
#"bayesglm"
#"ctree2"
#"brnn"
#"glmboost"
#"rpart1SE"
#"rpart2"
# "enet"



#"treebag"
#"bagEarth"
# "bagEarthGCV"
#"gamboost"
#"bstSm"
#"rpart"
#"xgbDART"
