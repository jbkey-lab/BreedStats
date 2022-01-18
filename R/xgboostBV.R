
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


  #BV.HSIdentical.df = fread(paste0(hdp,"BV.HSIdentical.df.all.csv"))
  BV.HSIdentical.df = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

  male = trainingx2[!duplicated(trainingx2$MALE), c("MALE",'LINE')]
  male$num = c(1:(nrow(male)+0))

  female = trainingx2[!duplicated(trainingx2$FEMALE), c("FEMALE",'LINE')]
  female$num = c(1:(nrow(female)+0))

  field = trainingx2[!duplicated(trainingx2$FIELD), c("FIELD",'LINE')]
  field$num = c(1:(nrow(field)+0))

  BV.HSIdentical.df = BV.HSIdentical.df %>% filter(Plot.Discarded != "Yes",
                                                   Plot.Status != "3 - Bad" )

  BV.HSIdentical.df.2 = BV.HSIdentical.df[,c(1:29,31,30,32:33)]
  colnames(BV.HSIdentical.df.2)[c(30:31)] = c("MALE","FEMALE")

  trainingx2 = data.frame(BV.HSIdentical.df)

  # trainingx2 = rbind(BV.HSIdentical.df.2,BV.HSIdentical.df)
  # trainingx2 = data.frame(trainingx2)

  field.2 = (trainingx2[!duplicated(trainingx2$FIELD), c("FIELD",'LINE')])
  field.2$num = c(1:(nrow(field.2)+0))
  trainingx2 = left_join(trainingx2, field.2[,c(1,3)], by="FIELD")
  colnames(trainingx2)[34] = "field"

  id = trainingx2[!duplicated(trainingx2$LINE), c("LINE",'MALE')]
  id$num = c(1:(nrow(id)+0))
  trainingx2 = left_join(trainingx2, id[,c(1,3)], by="LINE")
  colnames(trainingx2)[35] = "ID"

  male.2 = trainingx2[!duplicated(trainingx2$MALE), c("MALE",'LINE')]
  male.2$num = c(1:(nrow(male.2)+0))
  trainingx2 = left_join(trainingx2, male.2[,c(1,3)], by="MALE")
  colnames(trainingx2)[36] = "male"

  female.2 = trainingx2[!duplicated(trainingx2$FEMALE), c("FEMALE",'LINE')]
  female.2$num = c(1:(nrow(female.2)+0))
  trainingx2 = left_join(trainingx2, female.2[,c(1,3)], by="FEMALE")
  colnames(trainingx2)[37] = "female"

  Year = trainingx2[!duplicated(trainingx2$YEAR), c("YEAR",'LINE')]
  Year$num = c(1:(nrow(Year)+0))
  trainingx2 = left_join(trainingx2, Year[,c(1,3)], by="YEAR")
  colnames(trainingx2)[38] = "Year"

  variety = (trainingx2[!duplicated(trainingx2$Variety), c("Variety",'LINE')])
  variety$num = c(1:(nrow(variety)+0))
  trainingx2 = left_join(trainingx2, variety[,c(1,3)], by="Variety")
  colnames(trainingx2)[39] = "variety"

  #########################
  testx2 = expand.grid(male$num, female$num, field$num)
  testx2 = data.frame(testx2, Year=21, ID=paste0(testx2$Var1," + ",testx2$Var2 ))
  testx2= left_join(testx2, variety[,-1], by=c("ID"="LINE"))
  colnames(testx2)=c("male","female","field","Year","ID","variety")
  # field = (trainingx2[!duplicated(trainingx2$Pedigree), c("Pedigree",'LINE')])
  # field$num = c(1:(nrow(field)+0))
  # trainingx2 = left_join(trainingx2, field[,c(1,3)], by="Pedigree")
  # colnames(trainingx2)[40] = "ID"

  trainingx2 = na.omit(trainingx2[,c(22,39,37,35,36,38,34)]) #yield = 22, plt.height=13, ear=10

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

  #final_grid1 <- expand.grid(nrounds = 500, eta = .3, lambda = .5, alpha=1.5)
  final_grid2 <- expand.grid(nrounds = 550, eta = .5, lambda = .9, alpha=2)

  final_grid3 <- expand.grid(nrounds = 500, eta = .7, lambda = .5, alpha=.9)
  final_grid4 <- expand.grid(nrounds = 550, eta = .9, lambda = .9, alpha=.8)

  # final_grid3 <- expand.grid(mstop = 500, maxdepth = 2, nu = 0.1)
  # final_grid4 <- expand.grid(committees = 10, neighbors = 20)

  models.list2 <- caretList(
    x=trainx2[ , -1],
    y=(trainx2[,1]),
    continue_on_fail = T,
    trControl=trainControl(method="cv",
                           number=1, #1
                           index = createFolds((trainx2[,1]),k=2), #2
                           savePredictions = TRUE,
                           #classProbs=T,
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
      qrf6=caretModelSpec(method="xgbTree", tuneGrid = final_grid1), #5
      #qrf5=caretModelSpec(method="xgbLinear", tuneGrid = final_grid1), #5
      #qrf6=caretModelSpec(method="xgbLinear", tuneGrid = final_grid2), #5

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

  NCAA.stacked<-caretEnsemble(models.list2, # + 95
                              trControl = trainControl(
                                number=2,
                                method="boot",
                                verboseIter =TRUE,
                                allowParallel = T
                              )
  );NCAA.stacked # + 95

  invisible(gc())
  #626063 + 36001
  #----Yield Val = 55
  #----Yield tra = 66
  #~~~~~~~~~~~~~~~~~~
  #
  #----pltht Val = 77
  #----pltht tra = 86
  #~~~~~~~~~~~~~~~~~~
  #95958 + 5087
  #----earht Val = 53
  #----earht tra = 68

  #######Validate Corn
  preds = predict(NCAA.stacked, validatex2[,-1])
  cor(validatex2[, 1], preds)^2
  sqrt(mean((validatex2[, 1] -  preds)^2))

  #######training set
  preds.t = predict(NCAA.stacked, trainx2[,-1])
  cor(trainx2[, 1], preds.t)^2
  sqrt(mean((trainx2[, 1] -  preds.t)^2))


   # preds = data.frame(validatex2, round(preds,0))
   # preds = left_join(preds, field[,-2], by=c("variety"="num"))
   # preds = left_join(preds, field[,-2], by=c("round.preds..0."="num"))

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
