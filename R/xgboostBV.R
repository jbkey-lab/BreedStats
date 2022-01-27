trainVal = function(data, colToInd, sample ){

  ID = data[!duplicated(data[, colToInd]),  colToInd]
  ID = data.frame(ID)

  idx = sample(nrow(ID), nrow(ID) * sample) #.25

  Loc_Validate = data.frame(ID=ID[-idx, ])
  colnames(Loc_Validate) = colToInd

  Loc_Train = data.frame(ID=ID[idx, ])
  colnames(Loc_Train) = colToInd

  trainx2 = dplyr::inner_join(data, Loc_Train, by=colToInd) #%>% rbind(trainx1)
  validatex2 = dplyr::inner_join(data, Loc_Validate, by=colToInd)
  trainx2 = data.frame(trainx2)    %>% dplyr::mutate_all(as.numeric) #%>% unique()
  validatex2 = data.frame(validatex2)    %>% dplyr::mutate_all(as.numeric)


  return(list(data.frame(trainx2),data.frame(validatex2)))
}

randomEffect = function(CNN, CN,trainingx2, startVal=1){
  CS = CN[1]
  field.2 = (trainingx2[!duplicated(trainingx2[,CS]), CN])
  field.2$num = c(startVal:(nrow(field.2)+(startVal-1)))
  colnames(field.2)[-ncol(field.2)] = CN
  trainingx2 = left_join(trainingx2, field.2[,c(CS,"num")], by=CS)
  colnames(trainingx2)[ncol(trainingx2)] = CNN
  gc()
  return(list(data.frame(field.2), data.frame(trainingx2)))
}




xgblinearBV = function(){
  library(BreedStats)

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

  ######################################################


  trainingx2 = fread(paste0(hdp,"BV.HSIdentical.df.all.csv"))
  #BV.MC.Entry.data.test = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

  trainingx2 = trainingx2 %>% filter(Plot.Discarded != "Yes",
                                     Plot.Status != "3 - Bad" )%>%data.frame()



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

  male.3 = data.frame(male=c('BSQ033',	'GP734GTCBLL',	'BEX905',	'R08072HT',
                             'BRQ529',	'8D2',	'SGI193',	'8SY',
                             'GP718',	'FC2',	'BSR095',	'BRU059',
                             'BRQ291',	'BRP251',	'TR6254RR2',	'BSQ033-PWRA',
                             'BUR070',	'BRS312',	'8SY-AM',	'GP717Hx1',
                             'BAC020',	'TPCJ6605',	'BSU151',	'SGI193-V2P',
                             'BRQ064',	'GP6823Hx1',	'I10516',	'W8039RPGJZ',
                             'FB6455',	'BSU311',	'GP717',	'BSQ002',
                             'BAA441',	'GP738Hx1',	'BHH069',	'BQR042/BRQ064)-B-18-B',
                             '84Z',	'TR4949',	'GP695Hx1',	'BSU313',
                             'BHA493',	'R2846-NS6408DGV2P',	'I12003',	'R2846',
                             'BSR273',	'BSQ941',	'BUR032',	'PRW-AM',
                             'GP718Hx1',	'24AED-D02',"BAA419","bAA411","BHB075","BHJ471","GP702"))

  male.3 = left_join(male.3,male.2[,-2],by=c("male"="MALE") )

  #########################
  testx2 = expand.grid(male.3$num, female, Year.2$num, field )
  testx2 = left_join(testx2, id, by=c("Var1"="male","Var2"="female"))
  testx2= left_join(testx2, variety, by=c("ID"="ID"))
  colnames(testx2)[1:6]=c("male","female","Year","field","ID","variety")
  testx2$ID.cat = paste0(testx2$female, " + ", testx2$male)

  id.unk = testx2 %>% filter(is.na(ID)) %>%
    mutate(ID.concat = paste0(female, " + ", male)) %>%
    distinct(ID.concat) %>%
    mutate(num = ((max(trainingx2$ID)+1):(length(ID.concat)+(max(trainingx2$ID))) ) )

  testx2 = left_join(testx2, id.unk, by=c("ID.cat"="ID.concat"))
  testx2$ID = ifelse(is.na(testx2$ID), testx2$num,testx2$ID)
  testx2$variety = ifelse(is.na(testx2$variety), nullvarnum, testx2$variety)
  #testx2$field = 9

  testx2 = testx2[,c(6,5,1,2,3,4)]
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


  datasets = trainVal(data = trainingx2, colToInd= "ID", sample = 0.95)
  trainx2 = rbind(datasets[[1]], trainx1)
  validatex2 = datasets[[2]]


  ##################################################################
  #final_grid1=expand.grid(nrounds=550, eta=.5, max_depth=3, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)
  #final_grid2=expand.grid(nrounds=100, eta=.5, max_depth=5, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)

  #final_grid1 <- expand.grid(nrounds = 500, eta = .3, lambda = .5, alpha=1.5)
  #final_grid2 <- expand.grid(nrounds = 450, eta = .5, lambda = .9, alpha=2)

  #final_grid3 <- expand.grid(nrounds = 500, eta = .7, lambda = .5, alpha=.9)
  final_grid4 <- expand.grid(nrounds = c(1000,500,450), eta = 1, lambda = 0.0003, alpha=0.0003)

  # final_grid3 <- expand.grid(mstop = 500, maxdepth = 2, nu = 0.1)
  # final_grid4 <- expand.grid(committees = 10, neighbors = 20)

  trainx2$norm = (trainx2$Yield - mean(trainx2$Yield))/(max(trainx2$Yield)-min(trainx2$Yield))

  models.list2 <- caretList(
    x=trainx2[ , -c(1,8)],
    y=(trainx2[,8]),
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
      #qrf6=caretModelSpec(method="xgbLinear", tuneGrid = final_grid2), #5
      #qrf5=caretModelSpec(method="qrf",ntree=10, tuneLength = 1), #5
      #qrf6=caretModelSpec(method="xgbLinear", tuneGrid = final_grid2), #5

      #qrf7=caretModelSpec(method="xgbLinear", tuneGrid = final_grid3), #5
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
  #----Yield Val = 59
  #----Yield tra = 69
  #----Yield apr = 63

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

  # preds.test.bind.2 = preds.test.bind[,c(1,3,2,4,5,6,7)]
  # colnames(preds.test.bind.2)[c(2,3)] = c("MALE","FEMALE")
  # #
  # BV.HSIdentical.df.3 = rbind(preds.test.bind.2,preds.test.bind)
  # BV.HSIdentical.df.3 = data.frame(BV.HSIdentical.df.3)

  preds.test.agg.FIELD = preds.test.bind %>%
    group_by(FIELD,LINE) %>%
    summarize(preds.test = mean(preds.test))

  # preds.test.agg.FEMALE = preds.test.bind %>%
  #   group_by(FEMALE) %>%
  #   summarize(preds.test = mean(preds.test))
  #
  # preds.test.agg.MALE = preds.test.bind %>%
  #   group_by(MALE) %>%
  #   summarize(preds.test = mean(preds.test))
  #
  # colnames(preds.test.agg.MALE) = c("FEMALE","preds.test")
  # preds.test.agg = rbind(preds.test.agg.FEMALE,preds.test.agg.MALE)
  #
  # preds.test.agg.FEMALE = preds.test.agg %>%
  #   group_by(FEMALE) %>%
  #   summarize(preds.test = mean(preds.test))


  write.csv(preds.test.agg.FEMALE, "E-EK-Prop.csv")
  rm(preds.test,preds.test.bind,id.unk.all,df5,Blup,preds.test.bind.2, preds.test.agg.FEMALE,
     BV.HSIdentical.df.3)
  gc()



}


