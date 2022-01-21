

RR_IBD = function(df=df){

  df=df
  ibdExp=df

  Row = data.frame(rep(1:(nrow(ibdExp)/12), each = 12) )
  ibdExp$Row = Row$rep.1..nrow.ibdExp..12...each...12.


  Range = data.frame(rep(1:12, (nrow(ibdExp)/12)) )
  ibdExp$Range = Range$rep.1.12...nrow.ibdExp..12..

  df=ibdExp
  rm(ibdExp)
  return(data.frame(df))
}


mergeFiles = function(path = "R:/Breeding/MT_TP/Models/Alpha-Lattice",
                      merge_file_name = "R:/Breeding/MT_TP/Models/Alpha-Lattice/AL21S.xlsx"){
  #f=list.files(pattern="R:/Breeding/MT_TP/Models/Alpha-Lattice/*.csv")
  path <- path
  merge_file_name <- merge_file_name

  filenames_list <- list.files(path= path,pattern="*.csv" ,full.names=TRUE)



  All <- lapply(filenames_list,function(filename){
    print(paste("Merging",filename,sep = " "))
    read.csv(filename)

  })

  All_RR=lapply(All, RR_IBD)


  df <- do.call(rbind.data.frame, All_RR)

  rm(All,All_RR)
  gc()
  return(data.frame(df))

}




#asreml.options(gammaPar = TRUE)

#fit the separable autoregressive error model
model1 =function(){

  BV.HSIdentical.model = asreml(fixed = feature ~ Pedigree,
                                #random = ~  ,#+ Pedigree*EXP + `User Rep`*FIELD,
                                residual = ~ar1v(Row):ar1(Range),
                                #sparse = ~
                                data =subsetSA
                                #equate.levels=c("FEMALE","MALE"),
                                #workspace="31gb",
                                #na.action=na.method(y=c("include"),x=c("include"))
  )

  BV.HSIdentical.model$loglik
  summary(BV.HSIdentical.model)$varcomp
  wald(BV.HSIdentical.model)

  invisible(gc())

  BV.HSIdentical.model.predicted<- predict(BV.HSIdentical.model,
                                           classify="Pedigree",
                                           #pworkspace="32gb",
                                           #parallel=T,
                                           #aliased = T
                                           #maxit=1
                                           #vcov = T,
                                           #avsed=T,


  )

  plot(varioGram(BV.HSIdentical.model))


  return(list(data.frame(BV.HSIdentical.model.predicted), BV.HSIdentical.model))

}

# an extension to this model includes a measurement error or nugget effect term
model2 =function(){

  BV.HSIdentical.model = asreml(fixed = feature ~ Pedigree ,
                                random = ~ idv(units),#+ Pedigree*EXP + `User Rep`*FIELD,
                                residual = ~ar1v(Row):ar1(Range),
                                sparse = ~ `User.Rep` ,
                                data = subsetSA,
                                #equate.levels=c("FEMALE","MALE"),
                                #workspace="31gb",
                                na.action=na.method(y=c("include"),x=c("include"))
  )
  print(summary(BV.HSIdentical.model)$bic)

  BV.HSIdentical.model$loglik
  summary(BV.HSIdentical.model)$varcomp
  wald(BV.HSIdentical.model)

  invisible(gc())

  BV.HSIdentical.model.predicted<- predict(BV.HSIdentical.model,
                                           classify="Pedigree:User.Rep",
                                           #pworkspace="32gb",
                                           #parallel=T,
                                           # aliased = T
                                           #maxit=1
                                           #vcov = T,
                                           #avsed=T,


  )

  plot(varioGram(BV.HSIdentical.model))


  return(list(data.frame(BV.HSIdentical.model.predicted), BV.HSIdentical.model))

}

#incomplete block analysis (with recovery of inter-block information)
model3 =function(subsetSA=subsetSA, EBN=EBN, name=name, subsetSA.discard=subsetSA.discard){
  subsetSA.discard=subsetSA.discard
  subsetSA=subsetSA
  EBN=EBN
  name=name
  #########################################################
  #Map
  #########################################################
  subsetSA.index = order(subsetSA$User.Rep)




  subsetSA = subsetSA[subsetSA.index,]

  subsetSARect = subsetSA %>%
    group_by(as.factor(User.Rep)) %>%
    mutate(

      maxRange = max(as.numeric(as.character(Range)), na.rm=T),
      minRange = min(as.numeric(as.character(Range)), na.rm=T),
      maxRow = max(as.numeric(as.character(Row)), na.rm=T),
      minRow = min(as.numeric(as.character(Row)), na.rm=T),

    )

  #distinct(User.Rep, maxRange, minRange, maxRow,minRow)
  par(mar=c(5, 4, 4, 8), xpd=TRUE)

  EBNMap <- ggplot(subsetSA, aes(x=Row, y=Range, fill = feature)) + geom_tile(aes(width = 1, height=1)) +
    geom_text(aes(label=Entry), size=3)

  EBNMap <- EBNMap + scale_fill_gradient2(low='salmon',
                                          mid ='thistle',
                                          high ='seagreen3',
                                          midpoint = mean(subsetSA$feature, na.rm=TRUE),
                                          guide = "colourbar")

  EBNMap <- EBNMap + annotate("rect", size=1, xmin=subsetSARect$minRow-0.5, xmax=subsetSARect$maxRow+0.5,
                              ymin=subsetSARect$minRange-0.5, ymax=subsetSARect$maxRange+0.5,
                              fill=as.numeric(as.character(subsetSARect$User.Rep)),
                              color="black", linejoin = "mitre",alpha=0)


  BN = (subsetSARect[!duplicated(subsetSARect$Book.Name),as.character("Book.Name")])
  MR = subsetSARect[!duplicated(subsetSARect$maxRow), "maxRow"]
  MRR =  subsetSARect[!duplicated( subsetSARect$maxRange/2), "maxRange"]

  MR$maxRow = as.numeric(MR$maxRow)


  BN = gsub(pattern= "Beck - ", replacement = "", x = BN$Book.Name)


  EBNMap <- EBNMap + annotate("text",  x = MR$maxRow - 1, y = rep(MRR$maxRange, nrow(MR)) + 1,label = BN,
                              angle = 0, size = 3) +
     coord_cartesian(ylim = c(1, MRR$maxRange+1), clip = "off")


  EBNMap <- EBNMap  + labs(title = paste0(EBN, " Raw Trial of ", name)
                           )

  plot(EBNMap)

  hist(subsetSA$feature, main =paste0("Histogram of Raw",name), xlab = paste0(name), breaks = nrow(subsetSA)/2)

  #########################################################
  #Model
  #########################################################

  #add check as fixed effect
  #dat$Exp   <- ifelse(dat$Entry > 443, 0, 1)                      #| 0 = Check  ; 1 = Experimental Line
  #dat$Check <- ifelse(dat$Exp   > 0, 999, dat$Entry)              #| 999 = Experimental Line  ;  Else = Check Entry Number
  #str(dat)
  subsetSA$Checks = ifelse(nchar(as.character(subsetSA$Pedigree)) < 8,subsetSA$Entry, 1)

  subsetSA$Checks = as.factor(subsetSA$Checks)
  subsetSA.index = order(subsetSA$User.Rep,subsetSA$Row, subsetSA$Range)
  subsetSA = subsetSA[subsetSA.index, ]

  RR = max(subsetSARect$maxRange) * max(subsetSARect$maxRow)

  if(RR == nrow(subsetSA)){

    BV.HSIdentical.model =   try(asreml(fixed = feature ~ Pedigree + `User.Rep` ,
                                        random = ~ idv(units) ,#+ Pedigree*EXP + `User Rep`*FIELD,
                                        residual = ~ ar1v(Row):ar1(Range),
                                        sparse = ~ `User.Rep` ,
                                        data = subsetSA,
                                        #equate.levels=c("FEMALE","MALE"),
                                        #workspace="31gb",
                                        na.action=na.method(y=c("include"),x=c("include"))

    ),TRUE
    )

    if(class(BV.HSIdentical.model)=="try-error"){
      cat("Singularities in first try of model...trying model 2...")
      BV.HSIdentical.model = asreml(fixed = feature ~ Pedigree ,
                                    random = ~ `User.Rep` + Row + Range,#Row + Range  ,#+ Pedigree*EXP + `User Rep`*FIELD,
                                    # # residual = ~ar1v(Checks),
                                    sparse = ~`User.Rep`,
                                    data = subsetSA
                                    #  #equate.levels=c("FEMALE","MALE"),
                                    #  #workspace="31gb",
                                    #  na.action=na.method(y = c("include"), x = c("include") )
      )

    }

  }else{

    BV.HSIdentical.model = asreml(fixed = feature ~ Pedigree ,
                                  random = ~ `User.Rep` + Row + Range,#Row + Range  ,#+ Pedigree*EXP + `User Rep`*FIELD,
                                  # # residual = ~ar1v(Checks),
                                  sparse = ~`User.Rep`,
                                  data = subsetSA
                                  #  #equate.levels=c("FEMALE","MALE"),
                                  #  #workspace="31gb",
                                  #  na.action=na.method(y = c("include"), x = c("include") )
    )

  }

  invisible(gc())

  BV.HSIdentical.model.predicted<- try(predict(BV.HSIdentical.model,
                                               classify="Pedigree:User.Rep",
                                               #pworkspace="32gb",
                                               #parallel=T,
                                               #aliased = T
                                               #maxit=1
                                               #vcov = T,
                                               # avsed=T


  ),TRUE)

  if(class(BV.HSIdentical.model.predicted)=="try-error"){

    BV.HSIdentical.model = asreml(fixed = feature ~ Pedigree ,
                                  random = ~ `User.Rep` + Row + Range,#Row + Range  ,#+ Pedigree*EXP + `User Rep`*FIELD,
                                  # # residual = ~ar1v(Checks),
                                  sparse = ~`User.Rep`,
                                  data = subsetSA
                                  #  #equate.levels=c("FEMALE","MALE"),
                                  #  #workspace="31gb",
                                  #  na.action=na.method(y = c("include"), x = c("include") )
    )
    BV.HSIdentical.model.predicted<-predict(BV.HSIdentical.model,
                                            classify="Pedigree:User.Rep",
                                            #pworkspace="32gb",
                                            #parallel=T,
                                            #aliased = T
                                            #maxit=1
                                            #vcov = T,
                                            # avsed=T
    )

  }


  print(summary(BV.HSIdentical.model)$bic)
  print(BV.HSIdentical.model$loglik)
  print(summary(BV.HSIdentical.model)$varcomp)
  print(wald(BV.HSIdentical.model))
  plot(varioGram(BV.HSIdentical.model))

  subsetSA = subsetSA[,-11]

  #########################################################
  #Blups
  #########################################################
   #BV.HSIdentical.blues <- fitted(BV.HSIdentical.model.predicted)$random #determine the fixed effects (blues)
   #qualdat.blups<-fitted(BV.HSIdentical.model.predicted)$fixed #determine the fixed effects (BLUPS)
  # BV.HSIdentical.model$
  # BV.HSIdentical.blues$
  # BV.HSIdentical.model.predicted
  #########################################################
  #Obs
  #########################################################
  Unique.Pedigree <- as.matrix(subsetSA.discard[,"Pedigree"])
  Unique.Pedigree<-data.table(Unique.Pedigree)
  counts.adjusted <- Unique.Pedigree[, .(rowCount = .N), by = V1 ]; colnames(counts.adjusted)=c("Pedigree",paste0(name,"_Observations"))
  counts.adjusted.index = order(counts.adjusted$Pedigree)
  counts.adjusted = counts.adjusted[counts.adjusted.index,]

  Unique.Pedigree.re <-subsetSA.discard %>% dplyr::filter(feature > 0, Plot.Discarded != "Yes" , Plot.Status != "3 - Bad") %>% select(Pedigree)
  Unique.Pedigree.re<-data.table(Unique.Pedigree.re)
  counts.adjusted.re <- Unique.Pedigree.re[, .(rowCount = .N), by = Pedigree ]; colnames(counts.adjusted.re)=c("Pedigree",paste0(name,"_Observations"))
  counts.adjusted.re.index = order(counts.adjusted.re$Pedigree)
  counts.adjusted.re = counts.adjusted.re[counts.adjusted.re.index,]

  Counts.total = data.frame(counts.adjusted[,1], counts.adjusted.re[,2], (counts.adjusted.re[,2]/counts.adjusted[,2]))
  colnames(Counts.total)[3] = paste0(name,"_PctObsCollected")

  #########################################################
  #Preds
  #########################################################
  preds = left_join(BV.HSIdentical.model.predicted[["pvals"]], subsetSA, by=c("Pedigree","User.Rep"))
  #preds$predRank = (preds$predicted.value-mean(preds$predicted.value))*3.095958 #Blup
  preds = data.frame(preds)
  preds = left_join(preds, Counts.total, by="Pedigree")
  preds.index = order(preds$User.Rep)
  preds=preds[preds.index,]
  preds$Checks = ifelse(nchar(as.character(preds$Pedigree))<8,17,1)

  hist(as.numeric(preds$predicted.value), main =paste0("Histogram of Preds ",name), xlab = paste0(name),
       breaks = nrow(subsetSA)/2)

  plot(preds$predicted.value,preds$feature,  main =paste0("Correlation Between Pred and Raw for",name),
       ylab = paste0(name), xlab=paste0("Pred"), col=as.numeric(as.character(subsetSARect$User.Rep)),
       pch=preds$Checks, sub=  print(paste0("Correlation is ",round(cor(preds$predicted.value, preds$feature,use="pairwise.complete.obs"),4),".")))

  legend( "topright", inset=c(-0.2, 0),  as.character(preds[!duplicated(preds$User.Rep),"User.Rep"]) ,
          col =   as.character(preds[!duplicated(preds$User.Rep),"User.Rep"]), pch=20)


  EBNMapPred <- ggplot( preds , aes(x=Row, y=Range, fill = predicted.value)) + geom_tile() + geom_text(aes(label=Entry), size = 3)

  EBNMapPred <- EBNMapPred + scale_fill_gradient2(low='salmon', mid ='thistle',
                                                  high ='seagreen3',
                                                  midpoint = mean( preds$predicted.value, na.rm=TRUE)
                                                  ,guide = "colourbar")


  EBNMapPred <- EBNMapPred + annotate("rect", size=1, xmin=subsetSARect$minRow-0.5, xmax=subsetSARect$maxRow+0.5,
                                      ymin=subsetSARect$minRange-0.5, ymax=subsetSARect$maxRange+0.5,
                                      fill=as.numeric(as.character(subsetSARect$User.Rep)),
                                      color="black", linejoin = "mitre",alpha=0)


  EBNMapPred <- EBNMapPred + annotate("text",
                                      x = MR$maxRow - 1,
                                      y = rep(MRR$maxRange, nrow(MR)) + 1,
                                      label = BN, size = 3) +
    coord_cartesian(ylim = c(1, MRR$maxRange + 1), clip = "off")


  EBNMapPred <- EBNMapPred  + labs(title = paste0(EBN, " Adjusted Trial of ",name))

  plot(EBNMapPred)


  rm(subsetSA, subsetSA.discard)
  gc()

  return(list(data.frame(preds), BV.HSIdentical.model))

}

simData = function(spaDF=spaDF,seed=seed){

  seed=seed

  set.seed(seed)

  spaDF=spaDF

  spaDF=data.frame(spaDF)

  names<-names(spaDF[c(20:27,29:32)]); names

  spaDFEBN.index = spaDF[!duplicated(spaDF$Entry.Book.Name),"Entry.Book.Name"]


  for(name in names){
    for(i in 1:length(spaDFEBN.index)){

      EBNt = spaDFEBN.index[[i]]

      subsetSA = subset(
        spaDF[,c("Pedigree",paste0(name),"User.Rep","Entry.Book.Name")],
        Entry.Book.Name==EBNt)

      subsetSA$Pedigree = as.character(subsetSA$Pedigree)
      spaDF$Pedigree = as.character(spaDF$Pedigree)

      if(name == "Yield"){

        subsetSA[,name] = abs(rnorm(n=nrow(subsetSA), mean=250, sd=20))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }

      if(name == "EarHt"){

        spaDF$EarHt  = abs(rnorm(n=nrow(spaDF), mean=50, sd=6))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "GS.Late"){

        spaDF$GS.Late = abs(rbinom(n=nrow(spaDF), size=20, prob=.02))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "PCT.HOH"){

        spaDF$PCT.HOH = abs(rnorm(n=nrow(spaDF), mean=19, sd=0.5))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "Plt.Height"){

        spaDF$Plt.Height = abs(rnorm(n=nrow(spaDF), mean=105, sd=5))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "RL.."){

        spaDF$RL.. = abs(rbinom(n=nrow(spaDF), size=10, prob=.04))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "RL.Count"){

        spaDF$RL.Count = abs(rbinom(n=nrow(spaDF), size=10, prob=.05))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "Test.WT"){

        spaDF$Test.WT = abs(rnorm(n=nrow(spaDF), mean=56, sd=3.5))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "SL.."){

        spaDF$SL.. = abs(rbinom(n=nrow(spaDF), size=10, prob=.11))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "SL.Count"){

        spaDF$SL.Count = abs(rbinom(n=nrow(spaDF), size=10, prob=.10))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "Y.M"){

        spaDF$Y.M = abs(rpois(n=nrow(spaDF),lambda=13))
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }
      if(name == "StandCnt..UAV"){

        spaDF$StandCnt..UAV = abs(rnorm(n=nrow(spaDF), mean=12, sd=5))*250
        ID = which(spaDF$Entry.Book.Name == EBNt)
        spaDF[ID, name] <- subsetSA[ name]
      }

      #hist(spaDF$Y.M)
    }
  }

  return(data.frame(spaDF))

}

load_Data = function(x="7_26_2021",simulate=F,seed=8042){
  # wdp = paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/")
  # vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'
  x=x
  if(simulate){
    #source("R:/Breeding/MT_TP/Models/R-Scripts/sim_yielddata.R")
    data("Yielddata")
    sim=T
  }
  else{
  ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")

  BV.MC.Entry<-fread(paste0(ws))#all varieties to build the model
  sim = F
  }
  colnames(BV.MC.Entry)
  dim(BV.MC.Entry)
  BV.MC.Entry<-as.data.table(BV.MC.Entry)

  #Clean the Entry list file
  colnames(BV.MC.Entry)[4] = "Entry.Book.Name"
  colnames(BV.MC.Entry)[13] = "Plot.Discarded"
  colnames(BV.MC.Entry)[14] = "Plot.Status"

  BV.MC.Entry.data = BV.MC.Entry[,-c(7,17,6,19,26,27,28,30,31,32,35,37,38,42,45,21,22,12)]
  BV.MC.Entry.data = data.table(BV.MC.Entry.data)
  dim(BV.MC.Entry.data)
  #rm(BV.MC.Entry)
  BV.MC.Entry.data$Plot.Discarded = as.character(BV.MC.Entry.data$Plot.Discarded)
  BV.MC.Entry.data$Plot.Status = as.character(BV.MC.Entry.data$Plot.Status)


  dim(BV.MC.Entry.data)



  BV.MC.Entry.data=data.frame(BV.MC.Entry.data)



  # num_row=nrow(subsetSA[!duplicated(subsetSA$Row),])
  # num_range=nrow(subsetSA[!duplicated(subsetSA$Range),])
  # num_row*num_range

  dfMerged=mergeFiles()
  dfMerged = data.frame(dfMerged)
  spaDF = left_join(BV.MC.Entry.data,dfMerged,by=c("Entry.Book.Name","User.Rep","Entry.."))

  dim(spaDF)

  spaDF = spaDF[,c(1:35)]
  colnames(spaDF)[c(7,8,34,35)] = c("FRange","FRow","Row","Range")


  spaDF = spaDF %>% filter(!is.na(Range))

  dim(spaDF)
  #spaDFEBN

  spaDF=data.frame(spaDF)
  spaDF = transform(spaDF,
                    Entry.Book.Name = factor(Entry.Book.Name),
                    Book.Name = factor(Book.Name),
                    #feature = as.numeric(feature),
                    Pedigree = factor(Pedigree),
                    User.Rep=factor(`User.Rep`),
                    Row=factor(Row),
                    Range=factor(Range),
                    Entry = factor(Entry..)
                    #FEMALE = factor(FEMALE)
                    #REP=factor(REP)
  )

  #spaDF=spaDF[,-c(26,27)]

  return(data.frame(spaDF))

}



spaEBN = function(year="21",fdp="R:/Breeding/MT_TP/Models/AL_Adjustments/",spaDF=spaDF, seed=8042
                  ,namesSeq){

  seed=seed
  year = year
  fdp=fdp
  spaDF=spaDF

  # i=76
  # seed=8042
  # year="21"
  # sim=T
  # fdp="R:/Breeding/MT_TP/Models/AL_Adjustments/"
  # name="EarHt"
  #

  # if(sim){
  #   spaDF=simData(spaDF, seed=seed)
  # }

  spaDF = data.frame(spaDF)

  spaDFEBN.index = spaDF[!duplicated(spaDF$Entry.Book.Name),"Entry.Book.Name"]


  #spaDFEBN.index = spaDFEBN[!duplicated(spaDFEBN$Entry.Book.Name),]
  ##################################################################################
  #Run Model
  ##################################################################################
  names<-names(spaDF[c(20,23,30,31,32,21,22,24,25,26,27)]); names
  names = names[namesSeq]
  classes<-sapply(spaDF[c(20,23,30,31,32,21,22,24,25,26,27)], class); classes

  pdf(file = paste0(fdp,"B_ALAdjustment_",year,"S.pdf"), paper="special", width = 8.5,
      height = 11, family="Times", pointsize=11, bg="white", fg="black")
  sink(file=paste0(fdp,"B_ALAdjustment_",year,
                   "S.txt"),split=TRUE)

  for(name in names){

    if("feature" %in% colnames(spaDF)){
      spaDF = spaDF %>% select(-feature)
    }

    cat("C","\n")

    cat(paste0("--------------------------------------",name,"--------------------------------------"), "\n")

    spaDF$feature = spaDF[, name]

    spaDF = transform(spaDF,
                      Entry.Book.Name = factor(Entry.Book.Name),
                      Book.Name = factor(Book.Name),
                      feature = as.numeric(feature),
                      Pedigree = factor(Pedigree),
                      User.Rep = factor(`User.Rep`),
                      Row = factor(Row),
                      Range = factor(Range),
                      Entry = factor(Entry..)
                      #FEMALE = factor(FEMALE)
                      #REP=factor(REP)
    )

    spaEBNlist = list()

    # bind.linked.male.peds=foreach(i=(1:length(spaDFEBN.index)),
    #                               .packages=c("dplyr","DiGGer","stats"),
    #                               .export=c("mutate","filter","setNames","group_by","summarise",
    #                                         "ibDiGGer","getDesign","corDiGGer","desPlot")
    # ) %dopar% {
    #


    for (i in 1:length(spaDFEBN.index) ){

      EBN = spaDFEBN.index[[i]]
      cat(paste0("--------------------",name,"_",EBN,"--------------------"),"\n")


      subsetSA = subset(
        spaDF[,c("Pedigree","feature","Row","Range","User.Rep","Entry.Book.Name","Entry","Book.Name","FRow","FRange","Plot.Discarded","Plot.Status")],
        Entry.Book.Name==EBN)

      subsetSA.discard = subsetSA

      subsetSA <- subsetSA %>% dplyr::filter(Plot.Discarded != "Yes", Plot.Status != "3 - Bad"
                                                      #                                                # Pedigree != "FILL", Variety != "FILL",
                                                      #                                                # Pedigree != "placeholder", Variety != "placeholder",
                                                      #                                                # Entry.Book.Name != "Filler", Entry.Book.Name != "INBRED-GW_Prop"

      )


      subsetSA = subsetSA[,-c(11:12)]

      m3 = model3(subsetSA, EBN=EBN, name=name, subsetSA.discard=subsetSA.discard)

      spaEBNlist[[length(spaEBNlist)+1]] = data.frame(m3[[1]])

      #rbind(paste0(EBN),data.frame(m3[[1]]))

    }


    spaENB <- rbindlist(spaEBNlist)

    spaENB = spaENB %>% mutate(predicted.value = as.numeric(predicted.value),
                               feature = as.numeric(feature)
    )

    cat("Corr between preds and raw is ",cor(spaENB$predicted.value, spaENB$feature, use="pairwise.complete.obs"))

    rm(BV.MC.Entry,BV.MC.Entry.data, cl, dfMerged, m3, spaEBNlist, subsetSA)

    spaENB=data.frame(spaENB)
    colnames(spaENB) = c("Pedigree","User.Rep",paste0(name,"_ALPrediction"),
                         paste0(name,"_StdError"),paste0(name,"_status"),paste0(name),
                         paste0("Row"),paste0("Range"),paste0("Entry.Book.Name"),paste0("Entry"),"Book.Name","FieldRow",
                         "FieldRange",paste0(name,"_ObsCollected"),paste0(name,"_PctObsCollected"),"Checks")


    sigfigs.traits<-c( paste0(name,"_ALPrediction"), paste0(name,"_StdError"),paste0(name) ,paste0(name,"_PctObsCollected"))
    #for(i in round.traits){df7[,i]<-round(df7[,i] ,4)}
    spaENB = na.omit(spaENB)

    for(r in sigfigs.traits){
      for(i in 1:nrow(spaENB)){
        if(nchar(signif(spaENB[i,r] ,3)) > 6 ){
          spaENB[i,r]<-round(spaENB[i,r] ,4)}else
            spaENB[i,r]<-signif(spaENB[i,r] ,3)
      }
    }

    assign(paste0(name,"_AL",year,"S"), spaENB)

    rm(spaENB)

    gc()



  }

  BookName.index = eval(as.name(paste0("spaDF")))[!duplicated(eval(as.name(paste0("spaDF")))$Book.Name), "Book.Name"]

  for(i in 1:length(BookName.index)){

    for(name in names){

      BN = BookName.index[[i]]


      BNsubset = subset(eval(as.name(paste0(name,"_AL",year,"S")))[,c("FieldRow","FieldRange",paste0(name,"_ALPrediction"),
                                                                      paste0(name), "Book.Name")],
                        Book.Name == paste0(BN) )



      BNsubset=   data.frame(BNsubset)
      BNsubset = transform( BNsubset,
                            FieldRow = factor(FieldRow),
                            FieldRange = factor(FieldRange)
                            #FeaturePred = as.numeric(paste0(name,"_ALPrediction")),
                            # Feature = as.numeric(paste0(name))
      )

      BNsubset.index = order(BNsubset$FieldRange, BNsubset$FieldRow)
      BNsubset = BNsubset[BNsubset.index,]
      #####################################Raw
      EBNMapPred <- ggplot(BNsubset, aes(x=FieldRow,
                                         y=FieldRange,
                                         fill = as.numeric(BNsubset[,paste0(name)] ))) +
        geom_tile(hjust=1,vjust=.5)

      EBNMapPred <- EBNMapPred + scale_fill_gradient2(low='salmon', mid ='thistle',
                                                      high ='seagreen3',
                                                      midpoint = mean(as.numeric(BNsubset[,paste0(name)] )
                                                                      ,na.rm=TRUE
                                                      )
                                                      ,guide = "colourbar")

      #EBNMapPred <- EBNMapPred + annotate("rect", size=1, xmin=subsetSARect$minRow-0.5,
      #                                    xmax=subsetSARect$maxRow+0.5,
      #                                    ymin=subsetSARect$minRange-0.5,
      #                                    ymax=subsetSARect$maxRange+0.5,
      #                                    fill=as.numeric(as.character(subsetSARect$User.Rep)),
      #                                    color="black", linejoin = "mitre",alpha=0)

      EBNMapPred <- EBNMapPred  + labs(title = paste0(BN, " Raw Field Location of ",name), fill=paste0(name))

      EBNMapPred=EBNMapPred+ theme(axis.text=element_text(size=10),
                                   axis.text.x = element_text(angle = 90))

      plot(EBNMapPred)

      #####################################Adjusted
      EBNMapPred <- ggplot(BNsubset, aes(x=FieldRow,
                                         y=FieldRange,
                                         fill = as.numeric(BNsubset[,paste0(name,"_ALPrediction")] ))) +
        geom_tile(hjust=1,vjust=.5)

      EBNMapPred <- EBNMapPred + scale_fill_gradient2(low='salmon', mid ='thistle',
                                                      high ='seagreen3',
                                                      midpoint = mean(as.numeric(BNsubset[,paste0(name,"_ALPrediction")] )
                                                                      ,na.rm=TRUE
                                                      )
                                                      ,guide = "colourbar")

      #EBNMapPred <- EBNMapPred + annotate("rect", size=1, xmin=subsetSARect$minRow-0.5,
      #                                    xmax=subsetSARect$maxRow+0.5,
      #                                    ymin=subsetSARect$minRange-0.5,
      #                                    ymax=subsetSARect$maxRange+0.5,
      #                                    fill=as.numeric(as.character(subsetSARect$User.Rep)),
      #                                    color="black", linejoin = "mitre",alpha=0)

      EBNMapPred <- EBNMapPred  + labs(title = paste0(BN, " Adjusted Field Location of ",name), fill=paste0("Preds"))

      EBNMapPred=EBNMapPred+ theme(axis.text=element_text(size=10),
                                   axis.text.x = element_text(angle = 90))

      plot(EBNMapPred)

    }
  }


  dev.off()
  sink()

  if(!exists(paste0("Yield_AL",year,"S"))){

    spaENB = data.frame(spaDF[,c(13,6)], Yield_AL = ""
                        , Yield_StdError = ""
                        , Yield_status =  ""
                        , Yield= ""

                         ,spaDF[, c( 34, 35, 4, 9, 5,8,7)]
                        , Yield_ObsCollected = ""
                        , Yield_PctObsCollected = ""

                        , Checks =  ""
    )

    spaENB = spaENB %>% mutate(Entry.. = factor(spaENB$Entry..))


    colnames(spaENB)[c(12:13,10)] = c("FieldRow","FieldRange","Entry")

    assign(paste0("Yield_AL",year,"S"), spaENB)

    rm(spaENB)
  }

  if(!exists(paste0("Y.M_AL",year,"S"))){

    spaENB = data.frame(spaDF[,c(13,6)], Y.M_AL = ""
                        , Y.M_StdError = ""
                        , Y.M_status =  ""
                        , Y.M = ""

                        , spaDF[, c( 34, 35, 4, 9, 5,8,7)]
                        , Y.M_ObsCollected = ""
                        , Y.M_PctObsCollected = ""

                        , Checks =  ""
    )
    spaENB = spaENB %>% mutate(Entry.. = factor(spaENB$Entry..))


    colnames(spaENB)[c(12:13,10)] = c("FieldRow","FieldRange","Entry")

    assign(paste0("Y.M_AL",year,"S"), spaENB)

    rm(spaENB)
  }

  if(!exists(paste0("Plt.Height_AL",year,"S"))){

    spaENB = data.frame(spaDF[,c(13,6)], Plt.Height_AL = ""
                        , Plt.Height_StdError = ""
                        , Plt.Height_status =  ""
                        , Plt.Height = ""

                        , spaDF[, c( 34, 35, 4, 9, 5,8,7)]
                        , Plt.Height_ObsCollected = ""
                        , Plt.Height_PctObsCollected = ""

                        , Checks =  ""
    )

    spaENB = spaENB %>% mutate(Entry.. = factor(spaENB$Entry..))

    colnames(spaENB)[c(12:13,10)] = c("FieldRow","FieldRange","Entry")

    assign(paste0("Plt.Height_AL",year,"S"), spaENB)

    rm(spaENB)
  }

  if(!exists(paste0("EarHt_AL",year,"S"))){

    spaENB = data.frame(spaDF[,c(13,6)], EarHt_AL = ""
                        , EarHt_StdError = ""
                        , EarHt_status =  ""
                        , EarHt= ""

                        , spaDF[, c( 34, 35, 4, 9, 5,8,7)]
                        , EarHt_ObsCollected = ""
                        , EarHt_PctObsCollected = ""

                        , Checks =  ""
    )

    spaENB = spaENB %>% mutate(Entry.. = factor(spaENB$Entry..))

    colnames(spaENB)[c(12:13,10)] = c("FieldRow","FieldRange","Entry")

    assign(paste0("EarHt_AL",year,"S"), spaENB)

    rm(spaENB)
  }

  if(!exists( paste0("Test.WT_AL",year,"S") )){

    spaENB = data.frame(spaDF[,c(13,6)], Test.WT_AL = ""
                        , Test.WT_StdError = ""
                        , Test.WT_status =  ""
                        , Test.WT = ""

                        , spaDF[, c( 34, 35, 4, 9, 5,8,7)]
                        , Test.WT_ObsCollected = ""
                        , Test.WT_PctObsCollected = ""

                        , Checks =  ""
    )

    spaENB = spaENB %>% mutate(Entry.. = factor(spaENB$Entry..))

    colnames(spaENB)[c(12:13,10)] = c("FieldRow","FieldRange","Entry")

    assign(paste0("Test.WT_AL",year,"S"), spaENB)

    rm(spaENB)

  }

  AL.traits <- left_join( eval(as.name(paste0("Yield_AL",year,"S")))[, c(11,9,10,2,12,13,1,3,4,6,14,15)],
                          eval(as.name(paste0("Y.M_AL",year,"S")))[, c(9,10,2,1,3,4,6,14,15)],
                          by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits)


  #AL.traits.2 <- left_join(AL.traits.1,eval(as.name(paste0("Y.M_AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.2)
  AL.traits <- left_join(AL.traits, eval(as.name(paste0("Plt.Height_AL",year,"S")))[,c(9,10,2,1,3,4,6,14,15)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits)


  AL.traits <- left_join(AL.traits, eval(as.name(paste0("EarHt_AL",year,"S")))[,c(9,10,2,1,3,4,6,14,15)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits)


  AL.traits <- left_join(AL.traits, eval(as.name(paste0("Test.WT_AL",year,"S")))[,c(9,10,2,1,3,4,6,14,15)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits)


  #AL.traits.6 <- left_join(AL.traits.5,eval(as.name(paste0("RL.._AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.5)
  #AL.traits.7 <- left_join(AL.traits.6,eval(as.name(paste0("RL.Count_AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.6)
  #AL.traits.8 <- left_join(AL.traits.7,eval(as.name(paste0("SL.._AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.7)
  #AL.traits.9 <- left_join(AL.traits.8,eval(as.name(paste0("SL.Count_AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.8)
  #AL.traits <- left_join(AL.traits.5,eval(as.name(paste0("GS.Late_AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("Pedigree","User.Rep","Entry.Book.Name","Entry"));dim(AL.traits.9)
  #AL.traits.11 <- left_join(AL.traits.10,eval(as.name(paste0("StandCnt..Final._AL_",year,"S")))[,c(9,10,2,1,3,4,6,5)],by=c("FEMALE"));dim(AL.traits.10)
  #AL.traits <- left_join(AL.traits.10,eval(as.name(paste0("StandCnt..UAV._AL",year,"S")))[,c(9,10,2,1,3,4,6)],by=c("FEMALE"));colnames(AL.traits)[1]<-"Pedigree";dim(AL.traits)
  #AL.traits.dup<-AL.traits[duplicated(AL.traits$INBRED),];dim(AL.traits.dup)
  names(AL.traits) <- gsub("\\.", "", names(AL.traits))

  #AL.traits.index.BN = order(AL.traits$BookName, AL.traits$EntryBookName, AL.traits$FieldRow, AL.traits$FieldRange)

  #AL.traits = AL.traits[AL.traits.index.BN,]

  spaDF$Entry.. = as.factor(spaDF$Entry..)

  AL.traits = left_join(AL.traits, spaDF[,c(13,6,4,9,5,1)], by=c("Pedigree", "UserRep"="User.Rep",
                                                                 "EntryBookName"="Entry.Book.Name",
                                                                 "Entry"="Entry..", "BookName"="Book.Name"))

  AL.traits=AL.traits[,c(which(colnames(AL.traits)=="RecId"),which(colnames(AL.traits)!="RecId"))]

  AL.traits <- dplyr::filter(function(x)!all(x==""), AL.traits)

  AL.traits.index = order(AL.traits$Entry, AL.traits$EntryBookName, AL.traits$BookName)

  AL.traits = AL.traits[AL.traits.index, ]

  AL.traits =  AL.traits[!duplicated(AL.traits$RecId),]

  write.xlsx(AL.traits, paste0(fdp, "AL_EBN_Traits.xlsx"),overwrite=T)


  return(data.frame(AL.traits))

}




