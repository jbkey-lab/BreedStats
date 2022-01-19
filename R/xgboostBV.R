
xgblinearBV = function(){
  library(BreedStats)
  library(data.table)
  library(tidyverse)
  library(doParallel)
  library(caret)
  library(caretEnsemble)
  library(lme4)
  # library(stats)
  # library(impute)
  # library(keras)
  # library(RcppCNPy)
  # #####################################################
  s0=T
  s1 =T
  s2 =T
  s3 =F
  s4 =F
  s5 =F
  seas0 = 21
  seas1 = 20
  seas2 = 19
  seas3 = ""
  seas4 = ""
  seas5 = ""
  season0=as.numeric(seas0)
  season1=as.numeric(seas1)
  season2=as.numeric(seas2)
  season3=as.numeric(seas3)
  season4=as.numeric(seas4)
  season5=as.numeric(seas5)

  cores=detectCores()
  cl <- makeCluster(cores[1]-1, outfile="")
  registerDoParallel(cl)

  hdp = "C:/Users/jake.lamkey/Documents/"

  dp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Training/"
  tdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Test Inputs/"
  gdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/"

  #####################################################

  # o.train <- fread(paste0(dp,"inputs_others_train.csv"))[,-1]
  # #w.train <- fread(paste0(dp,"inputs_weather_train.csv"))[,-1]
  # y.train <- fread(paste0(dp,"yield_train.csv"))[,-1]
  #
  # o.test <- fread(paste0(tdp,"inputs_others_test.csv"))[,-1]
  # #w.test <- fread(paste0(tdp,"inputs_weather_test.csv"))[,-1]
  # geno <- fread(paste0(gdp,"clusterID_genotype.csv"))[-1788,-1]

  ######################################################


  trainingx2 = fread(paste0(hdp,"BV.HSIdentical.df.all.csv"))
  #BV.MC.Entry.data.test = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

  trainingx2 = trainingx2 %>% filter(Plot.Discarded != "Yes",
                                     Plot.Status != "3 - Bad" )%>%data.frame()

  #BV.HSIdentical.df.2 = BV.HSIdentical.df[,c(1:29,31,30,32:33)]
  #colnames(BV.HSIdentical.df.2)[c(30:31)] = c("MALE","FEMALE")

  #trainingx2 = data.frame(BV.MC.Entry.data)

  # trainingx2 = rbind(BV.HSIdentical.df.2,BV.HSIdentical.df)
  # trainingx2 = data.frame(trainingx2)
  randomEffect = function(CNN, CN,trainingx2){
    CS = CN[1]
    field.2 = (trainingx2[!duplicated(trainingx2[,CS]), CN])
    field.2$num = c(1:(nrow(field.2)+0))
    colnames(field.2)[-ncol(field.2)] = CN
    trainingx2 = left_join(trainingx2, field.2[,c(CS,"num")], by=CS)
    colnames(trainingx2)[ncol(trainingx2)] = CNN
    gc()
    return(list(data.frame(field.2), data.frame(trainingx2)))
  }

  RE = randomEffect(CNN= "field", CN=c("FIELD","LINE"),trainingx2)
  field.2 = RE[[1]]
  trainingx2 = RE[[2]]

  RE = randomEffect(CNN= "male", CN=c("MALE","LINE"),trainingx2)
  male.2 = RE[[1]]
  trainingx2 = RE[[2]]

  RE = randomEffect(CNN= "female", CN=c("FEMALE","LINE"),trainingx2)
  female.2 = RE[[1]]
  trainingx2 = RE[[2]]

  RE = randomEffect(CNN= "ID", CN=c("LINE","MALE","FEMALE"),trainingx2)
  ID.2 = RE[[1]]
  trainingx2 = RE[[2]]

  RE = randomEffect(CNN= "Year", CN=c("YEAR","LINE"),trainingx2)
  Year.2 = RE[[1]]
  trainingx2 = RE[[2]]

  RE = randomEffect(CNN= "variety", CN=c("Variety","LINE"),trainingx2)
  variety.2 = RE[[1]]
  trainingx2 = RE[[2]]

  nullvarnum = variety.2 %>% filter((Variety)=="") %>% select(num) %>% as.integer()

  BV.MC.Entry.data = trainingx2
  BV.HSIdentical.df.A = levelSelector(level="A",trainingx2)
  BV.HSIdentical.df.Prop = pcSelector(commericalType = "Prop", altCommericalType = "PET")
  BV.HSIdentical.df = rbind(BV.HSIdentical.df.A,
                            BV.HSIdentical.df.Prop)

  male = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$male),"male"]
  female = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$female),"female"]
  field = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$field),"field"]
  variety = trainingx2[!duplicated(trainingx2$variety), c("variety","ID")]
  id = trainingx2[!duplicated(trainingx2$ID), c("male","female","ID")]


  aprop= na.omit(BV.HSIdentical.df[,c(22,39,37,35,36,38,34)])

  rm(BV.HSIdentical.df.A, BV.HSIdentical.df.Prop, BV.MC.Entry.data.test, BV.MC.Entry.data,RE)
  gc()


  #########################
  testx2 = expand.grid(male, female, Year.2$num)
  testx2 = left_join(testx2, id, by=c("Var1"="male","Var2"="female"))
  testx2= left_join(testx2, variety, by=c("ID"="ID"))
  colnames(testx2)=c("male","female","Year","ID","variety")
  testx2$ID.cat = paste0(testx2$female, " + ", testx2$male)

  id.unk = testx2 %>% filter(is.na(ID)) %>%
    mutate(ID.concat = paste0(female, " + ", male)) %>%
    distinct(ID.concat) %>%
    mutate(num = ((max(trainingx2$ID)+1):(length(ID.concat)+(max(trainingx2$ID))) ) )

  testx2 = left_join(testx2, id.unk, by=c("ID.cat"="ID.concat"))
  testx2$ID = ifelse(is.na(testx2$ID), testx2$num,testx2$ID)
  testx2$variety = ifelse(is.na(testx2$variety), nullvarnum, testx2$variety)
  testx2$field = 9

  testx2 = testx2[,c(5,4,1,2,3,8)]
  trainingx2 = na.omit(trainingx2[,c(22,39,37,35,36,38,34)]) #yield = 22, plt.height=13, ear=10

  # id.unk.all = id.unk %>%
  #   separate(col=ID.concat, sep=" \\+ ", into=c("female","male"),remove=F ) %>%
  #   transform(male = as.numeric(male),
  #            female=as.numeric(female))  %>%
  #   left_join(male.2[,-2], by=c("male"="num")) %>%
  #   left_join(female.2[-2], by=c("female"="num")) %>%
  #   mutate(LINE = paste0(FEMALE, " + ", MALE)) %>%
  #   select(-ID.concat)
  # colnames(id.unk.all) = c("female","male","line",'MALE',"FEMALE","LINE")



  # field = (trainingx2[!duplicated(trainingx2$Pedigree), c("Pedigree",'LINE')])
  # field$num = c(1:(nrow(field)+0))
  # trainingx2 = left_join(trainingx2, field[,c(1,3)], by="Pedigree")
  # colnames(trainingx2)[40] = "ID"


  ID = trainingx2[!duplicated(trainingx2[,3]), 3]
  ID = data.frame(ID)

  idx = sample(nrow(ID), nrow(ID) * .95) #.25

  Loc_Validate = data.frame(ID=ID[-idx, ])
  Loc_Train = data.frame(ID=ID[idx, ])

  # ID = o.train[!duplicated(o.train[,2]), 2]
  # ID = data.frame(ID)
  #
  # idx = sample(nrow(ID), nrow(ID) * 0.9) #.9
  #
  # Loc_Validatesoy = data.frame(ID=ID[-idx, ])
  # Loc_Trainsoy = data.frame(ID=ID[idx, ])
  #
  # trainx1 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Trainsoy, by="ID")

  trainx2 = inner_join(trainingx2, Loc_Train, by="ID") #%>% rbind(trainx1)
  validatex2 = inner_join(trainingx2, Loc_Validate, by="ID")
  # validatex3 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Validatesoy, by="ID")

  # trainx1 = data.frame(trainx1)  %>% mutate_all(as.numeric)
  trainx2 = data.frame(trainx2)    %>% mutate_all(as.numeric) #%>% unique()
  validatex2 = data.frame(validatex2)    %>% mutate_all(as.numeric)
  # validatex3 = data.frame(validatex3)  %>% mutate_all(as.numeric)


  ##################################################################
  #final_grid1=expand.grid(nrounds=550, eta=.5, max_depth=3, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)
  #final_grid2=expand.grid(nrounds=100, eta=.5, max_depth=5, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)

  #final_grid1 <- expand.grid(nrounds = 500, eta = .3, lambda = .5, alpha=1.5)
  final_grid2 <- expand.grid(nrounds = 450, eta = .5, lambda = .9, alpha=2)

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
      qrf6=caretModelSpec(method="xgbLinear", tuneGrid = final_grid2), #5
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
  #----Yield Val = 58
  #----Yield tra = 65
  #----Yield apr = 58

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

  #######AProp set
  preds.ap = predict(NCAA.stacked, aprop[,-1])
  cor(aprop$Yield, preds.ap)^2
  sqrt(mean((aprop$Yield -  preds.ap)^2))
  preds.ap = data.table(aprop[,-1], preds.ap)

  preds.test.agg.FEMALE = preds.ap %>%
    group_by(female) %>%
    summarize(preds.ap = mean(preds.ap))
  preds.test.agg.MALE = preds.ap %>%
    group_by(male) %>%
    summarize(preds.ap = mean(preds.ap))

  preds.test.agg.FEMALE = left_join(preds.test.agg.FEMALE, female.2[,-2],by=c("female"="num"))
  preds.test.agg.MALE = left_join(preds.test.agg.MALE, male.2[,-2],by=c("male"="num"))
  colnames(preds.test.agg.MALE) = c("female","preds.ap","FEMALE")
  preds.test.agg = rbind(preds.test.agg.FEMALE,preds.test.agg.MALE)

  preds.test.agg = preds.test.agg %>%
    group_by(FEMALE) %>%
    summarize(preds.ap = mean(preds.ap)) %>%
    mutate(BV = (preds.ap- 228)/2 )


  #######expand.grind set male.female.year
  preds.test = predict(NCAA.stacked, testx2)
  preds.test = data.table(testx2,preds.test)
  preds.test.bind = preds.test %>%
    left_join( male.2[,-2],by=c("male"="num")) %>%
    left_join( female.2[,-2],by=c("female"="num")) %>%
    mutate(LINE = paste0(FEMALE, " + ", MALE)) %>%
    left_join( Year.2[,-2],by=c("Year"="num")) %>%
    left_join( field.2[,-2],by=c("field"="num")) %>%
    left_join( variety.2[,-2],by=c("variety"="num")) %>%
    select(-c(1:6)) #%>%
  #filter(preds.test > 250)

  preds.test.bind.2 = preds.test.bind[,c(1,3,2,4,5,6,7)]
  colnames(preds.test.bind.2)[c(2,3)] = c("MALE","FEMALE")
  #
  BV.HSIdentical.df.3 = rbind(preds.test.bind.2,preds.test.bind)
  BV.HSIdentical.df.3 = data.frame(BV.HSIdentical.df.3)
  preds.test.agg.FEMALE = BV.HSIdentical.df.3 %>%
    group_by(FEMALE) %>%
    summarize(preds.test = mean(preds.test))



  DIBV = lmer(formula = preds.test ~ (1|FEMALE) + (1|MALE) + (1|YEAR) ,
              na.action='na.exclude', REML = T,
              control = lmerControl(
                sparseX = T),
              data = BV.HSIdentical.df.3[,c("YEAR","MALE","FEMALE","preds.test")] )

  # 161
  # 1073
  sum.DIBV=print(summary(DIBV))
  Blup = ranef(DIBV)
  Blup=data.frame(Blup)
  Blup = Blup %>% filter(grpvar != "MALE")
  Blup = Blup %>% filter(grpvar != "LINE")
  Blup = Blup %>% filter(grpvar != "YEAR")

  df5 = data.frame(FEMALE = Blup$grp,
                   PREDS = Blup$condval,
                   BV = (Blup$condval*2) + sum.DIBV$coefficients[1],
                   stderror = Blup$condsd)

  write.csv(df5,"E-EK-Prop.csv")
  rm(preds.test,preds.test.bind,id.unk.all,df5,Blup)
  gc()
  #cor(trainx2[, 1], preds.test)^2
  #sqrt(mean((trainx2[, 1] -  preds.test)^2))

  # preds = data.frame(validatex2, round(preds,0))
  # preds = left_join(preds, field[,-2], by=c("variety"="num"))
  # preds = left_join(preds, field[,-2], by=c("round.preds..0."="num"))

  # #######Soybeans + 99
  # preds = predict(NCAA.stacked,  trainx1)
  # cor(data.frame(trainx1[,1]), (preds))^2
  # sqrt(mean((trainx1[, 1] -  preds)^2))
  #
  # #######Validate Soybean 75.1
  # preds = predict(NCAA.stacked,  validatex3)
  # cor(data.frame(validatex3[,1]), (preds))^2
  # sqrt(mean((validatex3[, 1] -  preds)^2))




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
