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
  trainingx2 = dplyr::left_join(trainingx2, field.2[,c(CS,"num")], by=CS)
  colnames(trainingx2)[ncol(trainingx2)] = CNN
  gc()
  return(list(data.frame(field.2), data.frame(trainingx2)))
}




xgblinearBV = function(  hdp,
                         fdp,
                         s0,
                         s1,
                         s2,
                         s3,
                         s4 ,
                         s5 ,
                         seas0,
                         seas1 ,
                         seas2 ,
                         seas3 ,
                         seas4 ,
                         seas5,
                         inbred,
                         rounds,
                         eta,
                         lambda,
                         alpha,
                         male
){

  # #####################################################
  # s0=T
  # s1 =T
  # s2 =F
  # s3 =F
  # s4 =F
  # s5 =F
  # seas0 = 21
  # seas1 = 20
  # seas2 = ""
  # seas3 = ""
  # seas4 = ""
  # seas5 = ""
  # sdp = "C:/Users/jake.lamkey/Documents/"
  # fdp= "C:/Users/jake.lamkey/Documents/"
  # library(BreedStats)
  # library(tidyverse)
  # library(doParallel)
  # library(caretEnsemble)
  # library(caret)
  # library(data.table)
  # folder="21S"
  # rounds = 3000
  # eta=1
  # alpha = .0003
  # lambda=.0003
  # male = data.frame(male=c('BSQ033',	'GP734GTCBLL',	'BEX905',	'R08072HT',
  #                   'BRQ529',	'8D2',	'SGI193',	'8SY',
  #                   'GP718',	'FC2',	'BSR095',	'BRU059',
  #                   'BRQ291',	'BRP251',	'TR6254RR2',	'BSQ033-PWRA',
  #                   'BUR070',	'BRS312',	'8SY-AM',	'GP717Hx1',
  #                   'BAC020',	'TPCJ6605',	'BSU151',	'SGI193-V2P',
  #                   'BRQ064',	'GP6823Hx1',	'I10516',	'W8039RPGJZ',
  #                   'FB6455',	'BSU311',	'GP717',	'BSQ002',
  #                   'BAA441',	'GP738Hx1',	'BHH069',
  #                   '84Z',	'TR4949',	'GP695Hx1',	'BSU313',
  #                   'BHA493',	'R2846-NS6408DGV2P',	'I12003',	'R2846',
  #                   'BSR273',	'BSQ941',	'BUR032',	'PRW-AM',
  #                   'GP718Hx1',	'24AED-D02',"BAA419","BAA411","BHB075","BHJ471","GP702",
  #                   "40QHQ-E07", "BQS941","BRS313","BSS009","GP738","BRR553",
  #                   "BQR042/BRQ064)-B-25-3", "BJH031/BHA011.DHB-045-1","BCA509",
  #                   "F8994", "BJH031/I11054)-B.DH042","T1874",
  #                   "BUR011", "(054530/016360)/ID5754-B-18", "BJH031/BHA011)-B-B-17",
  #                   "(LH212Ht/LH185//CJ7008)/(KDL6289/LH185)).DHB-21","BQR334",
  #                   "R6076","BRQ041","BBH030",
  #                   "F9898","85E","LFX7508","FC2YHR"
  # ))

  season0=as.numeric(seas0)
  season1=as.numeric(seas1)
  season2=as.numeric(seas2)
  season3=as.numeric(seas3)
  season4=as.numeric(seas4)
  season5=as.numeric(seas5)
  male.3=male

  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-1, outfile="")
  doParallel::registerDoParallel(cl)


  ######################################################


  trainingx2 = data.table::fread(paste0(sdp,"BV.HSIdentical.df.csv"))
  #linked.peds = openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/linked.peds.updated_21S_all.xlsx"),1)

  linked.peds = openxlsx::read.xlsx(paste0(sdp, "linked.peds.xlsx"),1)
  industryNames = InbredNameLibrary()
  industryNames = industryNames[[2]]

  linked.peds[,"match"] <- suppressWarnings(suppressMessages(plyr::revalue(as.character(linked.peds[,"match"]), industryNames)))  #industry name to inbred name conversion

  group_and_concat <- linked.peds %>%
    dplyr::select(uniqued_id, match, Gender) %>%
    dplyr::group_by(match) %>%
    dplyr::mutate(Prism_Mped_Fped_HId_Ped_IName_Var = paste(uniqued_id, collapse = " , "),
                  HG = paste(Gender, collapse = " , "))

  group_and_concat$HG = gsub(group_and_concat$HG, pattern="/", replacement = " , ")
  group_and_concat$HetGrp <- sapply(group_and_concat$HG, function(x) paste(unique(unlist(str_split(x," , "))),
                                                                           collapse = " , "))
  group_and_concat$HetGrp = gsub(group_and_concat$HetGrp, pattern="FEMALE , Male", replacement = "Female/Male")
  group_and_concat$HetGrp = gsub(group_and_concat$HetGrp, pattern="Male , FEMALE", replacement = "Female/Male")

  group_and_concat = group_and_concat[!duplicated(group_and_concat$match),]

  trainingx2 = dplyr::left_join(trainingx2, group_and_concat[,c(2,4,6)], by=c("FEMALE"="match"))

  #BV.MC.Entry.data.test = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

  trainingx2 = trainingx2 %>% dplyr::filter(Plot.Discarded != "Yes",
                                            Plot.Status != "3 - Bad" ) %>% data.frame()



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

RE = randomEffect(CNN= "hetgrp", CN=c("HetGrp","LINE"),trainingx2)
gender.2 = RE[[1]]
trainingx2 = RE[[2]]

nullvarnum = variety.2 %>% dplyr::filter((Variety)=="") %>% dplyr::select(num) %>% as.integer()


BV.HSIdentical.df.A = levelSelector(level="A",BV.MC.Entry.data=trainingx2,s0=s0,s1=s1,s2=s2,s3=s3,s4=s4,s5=s5,
                                    season0=season0,season1=season1,season2=season2,
                                    season3=season3,season4=season4,season5=season5)
BV.HSIdentical.df.Prop = pcSelector(commericalType = "Prop", altCommericalType = "PET",
                                    BV.MC.Entry.data=trainingx2,s0=s0,s1=s1,s2=s2,s3=s3,s4=s4,s5=s5,
                                    season0=season0,season1=season1,season2=season2,
                                    season3=season3,season4=season4,season5=season5)
BV.HSIdentical.df = rbind(BV.HSIdentical.df.A,
                          BV.HSIdentical.df.Prop)

male = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$male),"male"]
female = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$female),"female"]
field = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$field),c("field","FIELD","YEAR")]
field = field %>% filter(YEAR == 2021) %>% select(field,FIELD)
variety = trainingx2[!duplicated(trainingx2$variety), c("variety","ID")]
id = trainingx2[!duplicated(trainingx2$ID), c("male","female","ID")]
gender = trainingx2[!duplicated(trainingx2$hetgrp), c("hetgrp","ID")]



rm(BV.HSIdentical.df.A, BV.HSIdentical.df.Prop, BV.MC.Entry.data.test, BV.MC.Entry.data,RE,group_and_concat,
   linked.peds)
gc()

male.3 =male.3
male.3 = data.frame(male= male.3[!duplicated(male.3),])

male.3 = dplyr::left_join(male.3,male.2[,-2],by=c("male"="MALE") )

male.3 = na.omit(male.3)
#########################
field = field %>% data.frame() %>%
  dplyr::filter(FIELD != c("Contract - SSR-Garden City"),
                FIELD != c("(HOLDING)"),
                !grepl(FIELD, pattern = "Contract"),
                !grepl(FIELD, pattern = "Beck - H")) %>% select(field)


gc()
testx2 = expand.grid(male.3$num, female, Year.2$num, field$field )
testx2 = dplyr::left_join(testx2, id, by=c("Var1"="male","Var2"="female"))
testx2= dplyr::left_join(testx2, variety, by=c("ID"="ID"))
testx2= dplyr::left_join(testx2, gender, by=c("ID"="ID"))

colnames(testx2)=c("male","female","Year","field","ID","variety","hetgrp")
testx2$ID.cat = paste0(testx2$female, " + ", testx2$male)

id.unk = testx2 %>% dplyr::filter(is.na(ID)) %>%
  dplyr::mutate(ID.concat = paste0(female, " + ", male)) %>%
  dplyr::distinct(ID.concat) %>%
  dplyr::mutate(num = ((max(trainingx2$ID)+1):(length(ID.concat)+(max(trainingx2$ID))) ) )

testx2 = dplyr::left_join(testx2, id.unk, by=c("ID.cat"="ID.concat"))
testx2$ID = ifelse(is.na(testx2$ID), testx2$num,testx2$ID)
testx2$variety = ifelse(is.na(testx2$variety), nullvarnum, testx2$variety)
#testx2$field = 9

testx2 = testx2[,c(6,5,1,2,3,4,7)]


l<-length(trainingx2)#; l
names<-names(trainingx2[,c(10:18,20:22)]); names
classes<-sapply(trainingx2[c(10:22)], class); classes
#cat(paste0(fdp),"\n")
#cat("F", "\n")
if(!dir.exists(paste0(fdp,folder))){
  dir.create(paste0(fdp,folder))
}

sink(file=paste0(fdp,folder,"/XGBlinearBV_",folder,
                 if(A){print("A")},
                 if(B){print("B")},
                 if(C){print("C")},
                 if(Prop){print("Prop")},
                 if(Choice){print("Choice")},
                 if(D){print("D")},
                 if(E){print("E")},
                 if(Q){print("Q")},
                 if(R){print("R")},
                 if(V){print("V")},
                 if(X){print("X")}
                 ,".txt"),split=TRUE)

pdf(file = paste0(fdp,folder,"/XGBlinearBV_",folder,
                  if(A){print("A")},
                  if(B){print("B")},
                  if(C){print("C")},
                  if(Prop){print("Prop")},
                  if(Choice){print("Choice")},
                  if(D){print("D")},
                  if(E){print("E")},
                  if(Q){print("Q")},
                  if(R){print("R")},
                  if(V){print("V")},
                  if(X){print("X")}
                  ,".pdf"), paper="special",width = 11, height = 8.5,
    family="Times", pointsize=11,bg="white",fg="black")
#name='yield'
for(name in names){
  print(name)
}
#
# cat("G", "\n")

# cl=parallel::detectCores()
# cl <- makePSOCKcluster(cl-1)
# registerDoParallel(cl)
#
# bind.linked.male.peds=foreach(name=names,
#                               .packages=c("dplyr","asreml","stats","data.table"),
#                               .export=c("mutate","filter","setNames","group_by","summarise",
#                                         "asreml","setDT","left_join","fitted","data.table","transform","data.table")
#  ) %dopar% {
#library(asreml)
name="Yield"
cat("G", "\n")

for(name in names){
  cat(paste0("--------------------------------------",name,"--------------------------------------"), "\n")

  if( "feature" %in% colnames(trainingx2)){
    trainingx2 = trainingx2 %>% dplyr::select(-feature)
  }
  nameCol = paste0(name)
  trainingx2 = data.frame(trainingx2)
  trainingx2$feature = trainingx2[,name]


  #aprop= na.omit(BV.HSIdentical.df[,c(name,41,39,37,38,40,36,42)])

  if( "feature" %in% colnames(BV.HSIdentical.df)){
    BV.HSIdentical.df = BV.HSIdentical.df %>% dplyr::select(-feature)
  }
  nameCol = paste0(name)
  BV.HSIdentical.df = data.frame(BV.HSIdentical.df)
  BV.HSIdentical.df$feature = BV.HSIdentical.df[,name]
  ap.prop = na.omit( BV.HSIdentical.df[,36:43] )
  #trainingx2 = na.omit(trainingx2[,c(name,41,39,37,38,40,36,42)]) #yield = 22, plt.height=13, ear=10

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
  trainx2 = na.omit(rbind(datasets[[1]])[,36:43])
  validatex2 =na.omit( datasets[[2]][,36:43] )


  ##################################################################
  #final_grid1=expand.grid(nrounds=550, eta=.5, max_depth=3, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)
  #final_grid2=expand.grid(nrounds=100, eta=.5, max_depth=5, gamma=0,colsample_bytree=0.95,min_child_weight=1,subsample = 1)

  #final_grid1 <- expand.grid(nrounds = 500, eta = .3, lambda = .5, alpha=1.5)
  #final_grid2 <- expand.grid(nrounds = 2500, eta = .5, lambda = 0.0003, alpha=0.0003)

  final_grid3 <- expand.grid(nrounds = rounds, eta = eta, lambda = lambda, alpha=alpha)
  #final_grid4 <- expand.grid(nrounds = c(5000), eta = 1, lambda = 0.0003, alpha=0.0003)

  # final_grid3 <- expand.grid(mstop = 500, maxdepth = 2, nu = 0.1)
  # final_grid4 <- expand.grid(committees = 10, neighbors = 20)

  #trainx2$norm = (trainx2$Yield - mean(trainx2$Yield))/(max(trainx2$Yield)-min(trainx2$Yield))

  models.list2 <- caretEnsemble::caretList(
    x=as.matrix(trainx2[,-8]),
    y=(trainx2[,8]),
    continue_on_fail = T,
    trControl=caret::trainControl(method="cv",
                                  number=1, #1
                                  index = createFolds((trainx2[,8]),k=2), #2
                                  savePredictions = TRUE,
                                  #classProbs=T,
                                  allowParallel = TRUE,
                                  verboseIter = TRUE
                                  # preProcOptions =list(
                                  #  # method = c("knnImpute"),
                                  #   k = 7,
                                  #   knnSummary = mean)
                                  #na.remove = TRUE #method = c("center", "scale"))
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

      qrf7=caretModelSpec(method="xgbLinear", tuneGrid = final_grid3) #5
      #qrf8=caretEnsemble::caretModelSpec(method="xgbLinear", tuneGrid = final_grid4) #5

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

  NCAA.stacked<-caretEnsemble::caretEnsemble(models.list2, # + 95
                                             trControl = caret::trainControl(
                                               number=2,
                                               method="boot",
                                               verboseIter =TRUE,
                                               allowParallel = T
                                             )
  );NCAA.stacked # + 95

  invisible(gc())
  #626063 + 36001
  #----Yield Val = 59
  #----Yield tra = 72
  #----Yield apr = 68

  #~~~~~~~~~~~~~~~~~~
  #
  #----pltht Val = 77
  #----pltht tra = 86
  #~~~~~~~~~~~~~~~~~~
  #95958 + 5087
  #----earht Val = 53
  #----earht tra = 68

  #######Validate Corn
  preds = predict(NCAA.stacked, validatex2[,-8])
  cat("Corr for Validate ALL is: ",cor(validatex2[, 8], preds)^2, "\n")
  sqrt(mean((validatex2[, 8] -  preds)^2))

  #######training set
  preds.t = predict(NCAA.stacked, trainx2[,-8])
  cat("Corr for Train ALL is: ",cor(trainx2[, 8], preds.t)^2, "\n")
  sqrt(mean((trainx2[, 8] -  preds.t)^2))

  #######AProp set
  preds.ap = predict(NCAA.stacked, ap.prop[,-8])
  cat("Corr for Prop and A level is: ",cor(ap.prop[,8], preds.ap)^2, "\n")
  sqrt(mean((ap.prop[,8] -  preds.ap)^2))
  preds.ap = data.table(ap.prop[,-8], preds.ap)

  preds.test.agg.FEMALE = preds.ap %>%
    dplyr::group_by(female) %>%
    dplyr::summarize(preds.ap = mean(preds.ap))

  preds.test.agg.MALE = preds.ap %>%
    dplyr::group_by(male) %>%
    dplyr::summarize(preds.ap = mean(preds.ap))

  preds.test.agg.FEMALE = dplyr::left_join(preds.test.agg.FEMALE, female.2[,-2],by=c("female"="num"))
  preds.test.agg.MALE = dplyr::left_join(preds.test.agg.MALE, male.2[,-2],by=c("male"="num"))
  colnames(preds.test.agg.MALE) = c("female","preds.ap","FEMALE")
  preds.test.agg = rbind(preds.test.agg.FEMALE,preds.test.agg.MALE)

  preds.test.agg = preds.test.agg %>%
    dplyr::group_by(FEMALE) %>%
    dplyr::summarize(preds.ap = mean(preds.ap)) %>%
    dplyr::mutate(BV = (preds.ap- 228)/2 )

  # rm(id.unk.all,df5,Blup, datasets, aprop,id.unk, preds.ap, id, preds.t, preds, models.list2,trainingx2,variety,
  #    BV.HSIdentical.df.3, male.3, validatex2, trainx2, BV.HSIdentical.df)
  # gc()
  #######expand.grind set male.female.year
  cat("Predicting A and Prop test level for all combinations over Years,Locations,Male,Female", "\n")
  preds.test = predict(NCAA.stacked, testx2[,c(6,3,4,2,5,1,7)])

  #filter(preds.test > 250)

  # preds.test.bind.2 = preds.test.bind[,c(1,3,2,4,5,6,7)]
  # colnames(preds.test.bind.2)[c(2,3)] = c("MALE","FEMALE")
  # #
  # BV.HSIdentical.df.3 = rbind(preds.test.bind.2,preds.test.bind)
  # BV.HSIdentical.df.3 = data.frame(BV.HSIdentical.df.3)
  gc()

  preds.test = data.table(testx2,preds.test)
  gc()
  preds.test.bind = preds.test %>%
    dplyr::left_join( Year.2[,-2],by=c("Year"="num")) %>%
    dplyr::group_by(FIELD,LINE) %>%
    dplyr::summarize(preds.test = mean(preds.test)) %>%
    dplyr::select(preds.test) %>%
    data.frame()

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

  #rm(preds.test.agg.FIELD);gc()
  ###############################

  assign(paste0(name,"_XGBlinearBV_",folder,"SbyFemale"), preds.test.agg)

  assign(paste0(name,"_XGBlinearBV_",folder,"SbyField"), (preds.test.bind))

  rm(ap.prop,datasets, id.unk,models.list2, NCAA.stacked, preds.ap,preds.test.agg,preds.test.agg.FEMALE,preds.test.agg.MALE,
     trainx2, validatex2, preds,preds.t,preds.test,preds.test.bind)
  rm(gender,field,id,ID.2,variety)
  gc()
}

sink()
dev.off()
###############################
gc()
male = data.frame(male.2[,1]); female = data.frame(female.2[,1])
colnames(male)="FEMALE"
colnames(female)="FEMALE"

inbreds = rbind(male,female)

preds.testFemale = dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(
  dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(
    inbreds,
    eval(as.name(paste0("EarHt_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("GS.Late_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("PCT.HOH_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("Plt.Height_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("RL.._XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("RL.Count_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
    eval(as.name(paste0("SL.._XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
  eval(as.name(paste0("SL.Count_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
  eval(as.name(paste0("StandCnt..Final._XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
  eval(as.name(paste0("Test.WT_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
  eval(as.name(paste0("Y.M_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE"),
  eval(as.name(paste0("Yield_XGBlinearBV_",folder,"SbyFemale"))), by="FEMALE")


colnames(preds.testFemale) = c("Female","EarHT_BV","EarHT_GCA","GS.Late_BV","GS.Late_GCA","PCT.HOH_BV","PCT.HOH_GCA",
                               "Plt.Height_BV","Plt.Height_GCA","RL.._BV","RL.._GCA","RL.Count_BV","RL.Count_GCA",
                               "SL.._BV","SL.._GCA","SL.Count_BV","SL.Count_GCA","StandCnt..Final._BV","StandCnt..Final._GCA",
                               "Test.WT_BV","Test.WT_GCA","Y.M_BV","Y.M_GCA","Yield_BV","Yield_GCA")

rm(list=grep(pattern = paste0("*_XGBlinearBV_",folder,"SbyFemale"), x=ls(), value=TRUE))

preds.test = cbind(eval(as.name(paste0("EarHt_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("GS.Late_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("PCT.HOH_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("Plt.Height_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("RL.._XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("RL.Count_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("SL.._XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("SL.Count_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("StandCnt..Final._XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("Test.WT_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("Y.M_XGBlinearBV_",folder,"SbyField"))),
                   eval(as.name(paste0("Yield_XGBlinearBV_",folder,"SbyField"))))

colnames(preds.test) = names

rm(list=grep(pattern = paste0("*_XGBlinearBV_",folder,"SbyField"), x=ls(), value=TRUE))

gc()

preds.test = data.table(testx2,preds.test)
gc()

preds.test.bind = preds.test %>%
  dplyr::left_join( male.2[,-2],by=c("male"="num")) %>%
  dplyr::left_join( female.2[,-2],by=c("female"="num")) %>%
  dplyr::mutate(LINE = paste0(FEMALE, " + ", MALE)) %>%
  dplyr::left_join( Year.2[,-2],by=c("Year"="num")) %>%
  dplyr::left_join( field.2[,-2],by=c("Field"="num")) %>%
  dplyr::left_join( variety.2[,-2],by=c("variety"="num")) %>%
  dplyr::select(-c(1:6))

preds.test.agg.FIELD = preds.test.bind %>%
  dplyr::group_by(FIELD,LINE) %>%
  dplyr::summarize(preds.test = mean(preds.test))

rm(preds.test, preds.test.bind)
gc()

preds.test.agg.FIELD = tidyr::separate(preds.test.agg.FIELD, sep= " \\+ " ,col = LINE, into=c("FEMALE","MALE"), remove=F)
cat("Printing Colnames ", colnames(preds.test.agg.FIELD), "\n")

preds.test.agg.FIELD = preds.test.agg.FIELD %>%
  dplyr::filter(FIELD != c("Contract - SSR-Garden City"),
                FIELD != c("(HOLDING)"),
                !grepl(FIELD, pattern = "Contract"),
                !grepl(FIELD, pattern = "Beck - H"))

preds.test.agg.FIELD.select = preds.test.agg.FIELD %>% dplyr::filter(MALE == inbred)

preds.test.agg.FIELD.LINE = preds.test.agg.FIELD %>%
  dplyr::group_by(LINE) %>%
  dplyr::summarize(preds.test = mean(preds.test))

gc()
preds.test.agg.FIELD.LINE = tidyr::separate(preds.test.agg.FIELD.LINE, sep= " \\+ " ,col = LINE, into=c("FEMALE","MALE"), remove=F)

openxlsx::write.xlsx(preds.test.agg.FIELD.LINE, paste0(fdp,"A.Prop",folder,"_predsByLine.xlsx"),rowNames=F,overwrite=T)
rm(preds.test.agg.FIELD.LINE);gc()
cat("Finished writing by LINE")
openxlsx::write.xlsx(preds.test.agg.FIELD.select, paste0(fdp,"A.Prop",folder,"_predsbyLine",inbred,".xlsx"),rowNames=F,overwrite=T)
rm(preds.test.agg.FIELD.select);gc()
cat("Finished writing by LINE MALE")
openxlsx::write.xlsx(preds.testFemale, paste0(fdp,"A.Prop",folder,"_predsByFemale.xlsx"),rowNames=F,overwrite=T)
rm(preds.test.agg);gc()
cat("Finished writing by FEMALE")
cat("DONE", "\n")

Field= "Field"
if(!dir.exists(paste0(fdp,Field))){
  dir.create(paste0(fdp,Field))
}

field.index = preds.test.agg.FIELD[!duplicated(preds.test.agg.FIELD$FIELD), "FIELD"]
field.index = as.matrix(field.index)

for(i in field.index){
  field.subset = subset(x=preds.test.agg.FIELD, FIELD == i )
  openxlsx::write.xlsx(field.subset, paste0(fdp,"Field","/A.Prop",folder,"_predsbyLINE",i,".xlsx"),rowNames=F,overwrite=T)
}

return(data.frame(preds.test.agg.FIELD))
}







