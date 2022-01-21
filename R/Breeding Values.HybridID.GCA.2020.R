######################################################################################################
######Rscript for Breeding Values (GCA)
######################################################################################################
#rm(list=ls()) #remove environment vairalbes
invisible(gc(reset=T)) #cleans memory "garbage collector"
#memory.limit(size=8071)
#setwd("R:/Breeding/MT_TP/Models/Breeding Values")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################
#library("dplyr")
#library("data.table")
#library("pastecs")
#library("asreml")
#library("tidyr")
#library("tidyverse")
#library("xlsx")



HybridID = function(
  fdp ,
  BV.HSIdentical.df ,
  folder ,
  doYear ,
  A ,
  B ,
  C ,
  Prop ,
  Choice ,
  D,
  R,
  X,
  E,
  Q,
  V,
  GEM,
  ptm,
  doLmer,
  doField
){
  #sink("Debugger.Hybrid.txt")

  fdp = fdp
  #wdp=wdp
  BV.HSIdentical.df = BV.HSIdentical.df
  folder = folder
  doYear = doYear

  #library(asreml)
  #season = as.numeric(year) + 2000
  #fdp = "R:/Breeding/MT_TP/Models/Breeding Values/2020/Hybrid"
  wd="_withkeithanalysis"
  #BV.MC.Entry.data.AB.DONE <- df
  #BV.HSIdentical.df = BV.HSIdentical.df[,-c(18,19,1)]
  #BV.HSIdentical.df = data.frame(BV.MC.Entry.data.AB.DONE) #choose your dataset for the model
  #colnames(BV.HSIdentical.df)
  #head(BV.HSIdentical.df)
  #dim(BV.HSIdentical.df)

  #cat("A","\n")
  ############################
  ######use a for loop to do mulitple traits at once
  #setwd(paste0(fdp))
  #str(BV.HSIdentical.df)
  #l<-length(BV.HSIdentical.df); l
  #names<-names(BV.HSIdentical.df[c(13:25)]); names
  #classes<-sapply(BV.HSIdentical.df[c(13:25)],class); classes
  #setwd(paste0(fdp,"/Hybrid"))
  #str(BV.HSIdentical.df)
  #l<-length(BV.HSIdentical.df); l

  names<-names(BV.HSIdentical.df[,c(10:18,20:22)]); names
  classes<-sapply(BV.HSIdentical.df[c(10:22)],class); classes
  #cat("B","\n")
  #sink()
  #sink()

  sink(file=paste0(fdp,"/",folder,"/","BV_Hybrid",folder,
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

  pdf(file = paste0(fdp,"/",folder,"/","BV_Hybrid",folder,
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

  #name='Yield'
  #cat("----------------------------Loading Hybrid Adjustments----------------------------", "\n")
  for(name in names){

    cat(paste0("----------------------------",name,"----------------------------"), "\n")

    if("feature" %in% colnames(BV.HSIdentical.df)){
      BV.HSIdentical.df = BV.HSIdentical.df %>% select(-feature)
    }
    cat("C","\n")

    nameCol = paste0(name)
    BV.HSIdentical.df = data.frame(BV.HSIdentical.df)

    BV.HSIdentical.df$feature = BV.HSIdentical.df[,name]

    #BV.HSIdentical.df = cbind(BV.HSIdentical.df,BV.HSIdentical.df[,name])
    #$BV.l=length(BV.HSIdentical.df)
    #colnames(BV.HSIdentical.df)[[BV.l]] = "feature"
    BV.HSIdentical.df=data.frame(BV.HSIdentical.df)
    BV.HSIdentical.df = transform(BV.HSIdentical.df,
                                  #male   = factor(male),
                                  #female = factor(female),
                                  feature = as.numeric(feature),
                                  LINE = factor(LINE),
                                  FIELD=factor(FIELD),
                                  EXP=factor(EXP),
                                  YEAR=factor(YEAR)
                                  #MALE = factor(MALE),
                                  #FEMALE = factor(FEMALE)
                                  #REP=factor(REP)
    )

    dim(BV.HSIdentical.df)
    #BV.HSIdentical.df = BV.HSIdentical.df %>% dplyr::filter(feature > 0)
    dim(BV.HSIdentical.df);head(BV.HSIdentical.df)


    #Unique.Female.Pedigree <- as.matrix(BV.HSIdentical.df[,"FEMALE"])
    Unique.Male.Pedigree <- as.matrix(BV.HSIdentical.df[,"LINE"])
    Unique.Pedigree<-data.table(Unique.Male.Pedigree)
    counts.adjusted <- Unique.Pedigree[, .(rowCount = .N), by = V1 ]; colnames(counts.adjusted)=c("LINE",paste0(name,"_Observations"))
    counts.adjusted.index = order(counts.adjusted$LINE)
    counts.adjusted = counts.adjusted[counts.adjusted.index,]

    BV.HSIdentical.df = BV.HSIdentical.df %>% dplyr::filter(Plot.Discarded != "Yes",
                                                     Plot.Status != "3 - Bad" )

    BV.HSIdentical.df.counts = BV.HSIdentical.df %>% dplyr::filter(feature >= 0)
    #Unique.Female.Pedigree <- as.matrix(BV.HSIdentical.df.counts[,"FEMALE"])
    Unique.Male.Pedigree <- as.matrix(BV.HSIdentical.df.counts[,"LINE"])
    Unique.Pedigree<-data.table(Unique.Male.Pedigree)
    counts.adjusted.raw <- Unique.Pedigree[, .(rowCount = .N), by = V1 ]; colnames(counts.adjusted)=c("LINE","Observations")
    counts.adjusted.raw.index = order(counts.adjusted.raw$V1)
    counts.adjusted.raw = counts.adjusted.raw[counts.adjusted.raw.index,]

    rm(BV.HSIdentical.df.counts,counts.index,BV.HSIdentical.df.counts.miss,counts.womissing,counts.womissing.index, Unique.Male.Pedigree, Unique.Pedigree)
    invisible(gc())

    #######################################
    ######Graphs and Figures
    #######################################
    attach(BV.HSIdentical.df)
    boxplot(feature~FIELD, xlab="FIELD", ylab=paste0(name), main=paste0(name," by FIELD"), col="pink")
    #boxplot(trait~EXP, xlab="EXP", ylab=paste0(name), main=paste0(name," by EXP"), col="pink")
    #boxplot(trait~LINE, xlab="LINE", ylab=paste0(name), main=paste0(name," by LINE"), col="pink")
    detach(BV.HSIdentical.df)

    BV.HSIdentical.df.gmean = aggregate(BV.HSIdentical.df[,c("feature")], by=list(LINE = BV.HSIdentical.df$LINE),mean,na.rm=T); colnames(BV.HSIdentical.df.gmean) = c("LINE","g_mean"); BV.HSIdentical.df.gmean=data.frame(BV.HSIdentical.df.gmean)

    head(BV.HSIdentical.df.gmean)
    dim(BV.HSIdentical.df.gmean)
    head(BV.HSIdentical.df)
    dim(BV.HSIdentical.df)
    #str(BV.HSIdentical_lables)
    #str(BV_data.filter)
    #######################################
    ######Run the model
    #######################################
    #trait1 <- paste0(name,"~","FIELD") #Set the model
    #QUALDAT = lmer(trait1, qualdat.data, REML = T) #run the model
    #BV.ped<-BV.HSIdentical.df[,c(2,5,6)]
    #index <- which(duplicated(BV.ped$LINE))
    #BV.ped <- BV.ped[-index,]
    #ainv<-ainverse(pedigree=BV.ped)
    rm(df3,df3.gmean,df5,ear_height,gs_early,gs_late,pct_hoh,plot_wt,plt_height,qual.count,QUALDAT.SUM,rl_early_count,rl_percent,
       sl_cound,sl_percent,stand_cnt_early,stant_cnt_final,test_wt,Y_m,yield,BV,BV,
       BV.EC,BV.EC.filter,BV.EC.filter.check,BV.filter.Var,BV.MC,BV.MC.filter.check,BV.MC.filter,BV.ped,BV.Var,z,
       BV.check,index, df3,df3.gmean,df5,ear_height,gs_early,gs_late,pct_hoh,plot_wt,plt_height,qual.count,QUALDAT.SUM,rl_early_count,rl_percent,
       sl_cound,sl_percent,stand_cnt_early,stant_cnt_final,test_wt,Y_m,yield,BV,BV,
       BV.EC,BV.EC.filter,BV.EC.filter.check,BV.filter.Var,BV.MC,BV.MC.filter.check,BV.MC.filter,BV.ped,BV.Var,z,
       BV.check,index, Unique.Pedigree, Unique.Female.Pedigree, Unique.Male.Pedigree)
    invisible(gc(reset=T)) #cleans memory "garbage collector"
    cat("D","\n")

    #asreml.options(dense = ~ vm(ainv))
    if(doYear){
      BV.HSIdentical.model = asreml(fixed = feature ~ YEAR,
                                    random = ~ LINE ,#+ LINE*EXP + REP*FIELD,
                                    #residual = ~units,
                                    data = BV.HSIdentical.df[,c("YEAR","LINE","feature")],
                                    #equate.levels=c("FEMALE","MALE"),
                                    workspace="31gb",
                                    na.action=na.method(y=c("include"),x=c("include"))
      )
      cat("E","\n")
      plot(BV.HSIdentical.model)
      print(name)
      QUALDAT.SUM <- print(summary(BV.HSIdentical.model))
      BV.HSIdentical.model.predicted <- predict(BV.HSIdentical.model,classify="LINE"
                                                #,pworkspace=812e6
                                                ,aliased=T
      )
      cat("F","\n")

      ######CREATE A HERITABILITY
      HTvca<-summary(BV.HSIdentical.model)$varcomp$component
      Va<-HTvca[1]*4
      Vp<-sum(HTvca[-2])
      h2=Va/Vp
      #print(paste0("Heritability = ", h2))
      #######################################
      ######BLUPs
      #######################################
      qualdat.blups<-coef(BV.HSIdentical.model)$fixed#determine the fixed effects (BLUPS)
      #qualdat.blups<-data.frame(qualdat.blups$hybridID)
      #qualdat.blups<-setDT(qualdat.blups,keep.rownames=T)[];colnames(qualdat.blups)=c("hybridID",paste0(name,"BLUP"))
      #qualdat.blups.export<-left_join(qualdat.blups, qualdat.gmean, by="hybridID")
      #write.csv(qualdat.blups.export,paste0(name,"_BLUPs_19S.csv")) #write blups to spreadsheet
      cat("G","\n")

      #######################################
      ######BLUEs
      #######################################
      # Create a numeric vector with the BLUP for each line
      BV.HSIdentical.blues <- coef(BV.HSIdentical.model)$random #determine the fixed effects (blues)
      BV.HSIdentical.blues <- setDT(data.frame(BV.HSIdentical.blues),keep.rownames=T)[]; colnames(BV.HSIdentical.blues)=c("LINE",paste0(name,"_BV")); word.remove.fem="LINE_"; word.remove.mal="MALE_"
      #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues$LINE)
      #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.mal,collapse = "|"), "", BV.HSIdentical.blues$LINE)
      BV.HSIdentical.blues.line <- BV.HSIdentical.blues %>% dplyr::filter(grepl("LINE_", LINE));dim(BV.HSIdentical.blues.line);colnames(BV.HSIdentical.blues.line)[1]="LINE"
      BV.HSIdentical.blues.line$LINE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues.line$LINE)
      BV.HSIdentical.blues.field <- BV.HSIdentical.blues %>% dplyr::filter(grepl("FIELD_", LINE));dim(BV.HSIdentical.blues.field)
      BV.HSIdentical.blues.exp <- BV.HSIdentical.blues %>% dplyr::filter(grepl("EXP_", LINE));dim(BV.HSIdentical.blues.exp)

      #BV.HSIdentical.blues.female
      BV.HSIdentical.blues.export <- left_join(BV.HSIdentical.blues.line, BV.HSIdentical.df.gmean, by="LINE")
      head(BV.HSIdentical.blues.export); dim(BV.HSIdentical.blues.export)
      ######FIND SE, etc...
      #write.csv(BV.HSIdentical.blues.export,paste0(name,"_BV_19S.csv")) #write blues to spreadsheet
      mu.idx = which(BV.HSIdentical.model$factor.names=='(Intercept)')
      mu=BV.HSIdentical.model$coefficients$random[mu.idx]
      mu.idx.line = which(BV.HSIdentical.model$factor.names=='LINE')
      mu.line=BV.HSIdentical.model$coefficients$random[mu.idx.line]
      cat("H","\n")

      #fam.idx=BV.HSIdentical.model$coefficients=='FEMALE'
      pred<-BV.HSIdentical.model.predicted$pvals
      pred.line<-BV.HSIdentical.model.predicted$pvals$LINE
      BV=pred$predicted.value
      se.BV = BV.HSIdentical.model.predicted$pvals$std.error
      accuracy<-round(sqrt(abs(1-(se.BV/HTvca[1]))),2)
      std.errors<-cbind(data.frame(pred.line),BV,se.BV,accuracy)
      std.errors=data.frame(std.errors);colnames(std.errors)=c("LINE",paste0(name,"_BLUE"),paste0(name,"_standard.error.BLUE"),paste0(name,"_accuracy.BLUE"))
      cat("I","\n")

      #######################################
      ######predicted values with fitted() function
      #######################################
      #BV.HSIdentical.blues = data.frame(predict(BV.HSIdentical)) #predict the dataset to get BLUE Values 2866
      #df3<-cbind(data.frame(BV.HSIdentical.data$EXP),data.frame(BV.HSIdentical.data$hybridID),data.frame(BV.HSIdentical.blues)); colnames(df3)=c("EXP","hybridID","BLUE")
      #df3.gmean = aggregate(df3[,c("BLUE")], by=list(hybridID = df3$hybridID),mean,na.rm=T);colnames(df3.gmean) = c('hybridID',paste0(name)); df3.gmean=data.frame(df3.gmean)
      #df3<-df3[!duplicated(df3[,1]),]
      #BV.HSIdentical.blues.filtered<-na.omit(df3)
      #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet

      BV.HSIdentical.fitted = fitted(BV.HSIdentical.model) #predict the dataset
      df3<-cbind(BV.HSIdentical.fitted,BV.HSIdentical.df); colnames(df3)[1]=c(paste0("fitted_",name))
      df3.gmean = aggregate(df3[,c(paste0("fitted_",name))], by=list(LINE = df3$LINE),mean,na.rm=T);colnames(df3.gmean) = c('LINE',paste0("fitted_",name)); df3.gmean=data.frame(df3.gmean)
      #df3.gmean.female = aggregate(df3[,c(paste0("fitted_",name))], by=list(FEMALE = df3$FEMALE),mean,na.rm=T);colnames(df3.gmean.female) = c('LINE',paste0("fitted_",name)); df3.gmean.female=data.frame(df3.gmean.female)
      #df3.gmean <- rbind(df3.gmean.male,df3.gmean.female)
      #df3<-df3[!duplicated(df3[,1]),]
      #BV.HSIdentical.blues.filtered<-na.omit(df3)
      #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet
      cat("J","\n")

      #######################################
      ######Graphs and Figures
      #######################################
      df5<-left_join(BV.HSIdentical.blues.export,df3.gmean,by="LINE");dim(df5)
      #df5<-df5[!duplicated(df5$LINE),]
      counts = left_join(counts.adjusted, counts.adjusted.raw,by=c("LINE"="V1"))
      counts = data.frame(counts)
      counts$Observations = as.numeric(counts$Observations )
      counts$rowCount = as.numeric(counts$rowCount )

      counts[is.na(counts)] = 0

      counts$ObsPercent = counts$rowCount / counts$Observations
      counts = counts[, c(1,2,4)]

      df6<-left_join(df5, counts, by="LINE")
      #df6<-left_join(df6,counts,by="LINE")
      df7<-left_join(df6,std.errors,by="LINE")
      df7<-df7[,-c(3,4)];colnames(df7)[2]=paste0(name,"_BLUP")
      #sigfigs to three digits
      colnames(df7)[3:4]=c(paste0(name,"_Observations"),paste0(name,"_PctPlotObsCollected"))

      #for(i in round.traits){df7[,i]<-round(df7[,i] ,4)}
      df7=data.frame(df7)

    }

    if(doField){
      BV.HSIdentical.model = asreml(fixed = feature ~ FIELD,
                                    random = ~ LINE ,#+ LINE*EXP + REP*FIELD,
                                    #residual = ~units,
                                    data = BV.HSIdentical.df[,c("FIELD","LINE","feature")],
                                    #equate.levels=c("FEMALE","MALE"),
                                    workspace="31gb"#,
                                    #na.action=na.method(y=c("include"),x=c("omit"))
      ) #run the model
      plot(BV.HSIdentical.model)
      print(name)
      QUALDAT.SUM <- print(summary(BV.HSIdentical.model))
      BV.HSIdentical.model.predicted <- predict(BV.HSIdentical.model,classify="LINE"
                                                #,pworkspace=812e6
                                                ,aliased=T
      )
      cat("F","\n")

      ######CREATE A HERITABILITY
      HTvca<-summary(BV.HSIdentical.model)$varcomp$component
      Va<-HTvca[1]*4
      Vp<-sum(HTvca[-2])
      h2=Va/Vp
      #print(paste0("Heritability = ", h2))
      #######################################
      ######BLUPs
      #######################################
      qualdat.blups<-coef(BV.HSIdentical.model)$fixed#determine the fixed effects (BLUPS)
      #qualdat.blups<-data.frame(qualdat.blups$hybridID)
      #qualdat.blups<-setDT(qualdat.blups,keep.rownames=T)[];colnames(qualdat.blups)=c("hybridID",paste0(name,"BLUP"))
      #qualdat.blups.export<-left_join(qualdat.blups, qualdat.gmean, by="hybridID")
      #write.csv(qualdat.blups.export,paste0(name,"_BLUPs_19S.csv")) #write blups to spreadsheet
      cat("G","\n")

      #######################################
      ######BLUEs
      #######################################
      # Create a numeric vector with the BLUP for each line
      BV.HSIdentical.blues <- coef(BV.HSIdentical.model)$random #determine the fixed effects (blues)
      BV.HSIdentical.blues <- setDT(data.frame(BV.HSIdentical.blues),keep.rownames=T)[]; colnames(BV.HSIdentical.blues)=c("LINE",paste0(name,"_BV")); word.remove.fem="LINE_"; word.remove.mal="MALE_"
      #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues$LINE)
      #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.mal,collapse = "|"), "", BV.HSIdentical.blues$LINE)
      BV.HSIdentical.blues.line <- BV.HSIdentical.blues %>% dplyr::filter(grepl("LINE_", LINE));dim(BV.HSIdentical.blues.line);colnames(BV.HSIdentical.blues.line)[1]="LINE"
      BV.HSIdentical.blues.line$LINE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues.line$LINE)
      BV.HSIdentical.blues.field <- BV.HSIdentical.blues %>% dplyr::filter(grepl("FIELD_", LINE));dim(BV.HSIdentical.blues.field)
      BV.HSIdentical.blues.exp <- BV.HSIdentical.blues %>% dplyr::filter(grepl("EXP_", LINE));dim(BV.HSIdentical.blues.exp)

      #BV.HSIdentical.blues.female
      BV.HSIdentical.blues.export <- left_join(BV.HSIdentical.blues.line, BV.HSIdentical.df.gmean, by="LINE")
      head(BV.HSIdentical.blues.export); dim(BV.HSIdentical.blues.export)
      ######FIND SE, etc...
      #write.csv(BV.HSIdentical.blues.export,paste0(name,"_BV_19S.csv")) #write blues to spreadsheet
      mu.idx = which(BV.HSIdentical.model$factor.names=='(Intercept)')
      mu=BV.HSIdentical.model$coefficients$random[mu.idx]
      mu.idx.line = which(BV.HSIdentical.model$factor.names=='LINE')
      mu.line=BV.HSIdentical.model$coefficients$random[mu.idx.line]
      cat("H","\n")

      #fam.idx=BV.HSIdentical.model$coefficients=='FEMALE'
      pred<-BV.HSIdentical.model.predicted$pvals
      pred.line<-BV.HSIdentical.model.predicted$pvals$LINE
      BV=pred$predicted.value
      se.BV = BV.HSIdentical.model.predicted$pvals$std.error
      accuracy<-round(sqrt(abs(1-(se.BV/HTvca[1]))),2)
      std.errors<-cbind(data.frame(pred.line),BV,se.BV,accuracy)
      std.errors=data.frame(std.errors);colnames(std.errors)=c("LINE",paste0(name,"_BLUE"),paste0(name,"_standard.error.BLUE"),paste0(name,"_accuracy.BLUE"))
      cat("I","\n")

      #######################################
      ######predicted values with fitted() function
      #######################################
      #BV.HSIdentical.blues = data.frame(predict(BV.HSIdentical)) #predict the dataset to get BLUE Values 2866
      #df3<-cbind(data.frame(BV.HSIdentical.data$EXP),data.frame(BV.HSIdentical.data$hybridID),data.frame(BV.HSIdentical.blues)); colnames(df3)=c("EXP","hybridID","BLUE")
      #df3.gmean = aggregate(df3[,c("BLUE")], by=list(hybridID = df3$hybridID),mean,na.rm=T);colnames(df3.gmean) = c('hybridID',paste0(name)); df3.gmean=data.frame(df3.gmean)
      #df3<-df3[!duplicated(df3[,1]),]
      #BV.HSIdentical.blues.filtered<-na.omit(df3)
      #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet

      BV.HSIdentical.fitted = fitted(BV.HSIdentical.model) #predict the dataset
      df3<-cbind(BV.HSIdentical.fitted,BV.HSIdentical.df); colnames(df3)[1]=c(paste0("fitted_",name))
      df3.gmean = aggregate(df3[,c(paste0("fitted_",name))], by=list(LINE = df3$LINE),mean,na.rm=T);colnames(df3.gmean) = c('LINE',paste0("fitted_",name)); df3.gmean=data.frame(df3.gmean)
      #df3.gmean.female = aggregate(df3[,c(paste0("fitted_",name))], by=list(FEMALE = df3$FEMALE),mean,na.rm=T);colnames(df3.gmean.female) = c('LINE',paste0("fitted_",name)); df3.gmean.female=data.frame(df3.gmean.female)
      #df3.gmean <- rbind(df3.gmean.male,df3.gmean.female)
      #df3<-df3[!duplicated(df3[,1]),]
      #BV.HSIdentical.blues.filtered<-na.omit(df3)
      #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet
      cat("J","\n")

      #######################################
      ######Graphs and Figures
      #######################################
      df5<-left_join(BV.HSIdentical.blues.export,df3.gmean,by="LINE");dim(df5)
      #df5<-df5[!duplicated(df5$LINE),]
      counts = left_join(counts.adjusted, counts.adjusted.raw,by=c("LINE"="V1"))
      counts = data.frame(counts)
      counts$Observations = as.numeric(counts$Observations )
      counts$rowCount = as.numeric(counts$rowCount )

      counts[is.na(counts)] = 0

      counts$ObsPercent = counts$rowCount / counts$Observations
      counts = counts[, c(1,2,4)]

      df6<-left_join(df5, counts, by="LINE")
      #df6<-left_join(df6,counts,by="LINE")
      df7<-left_join(df6,std.errors,by="LINE")
      df7<-df7[,-c(3,4)];colnames(df7)[2]=paste0(name,"_BLUP")
      #sigfigs to three digits
      colnames(df7)[3:4]=c(paste0(name,"_Observations"),paste0(name,"_PctPlotObsCollected"))

      sigfigs.traits<-c( paste0(name,"_BLUE"),  paste0(name,"_BLUP"),
                         paste0(name,"_standard.error.BLUE"), paste0(name,"_accuracy.BLUE"),
                         paste0(name,"_PctPlotObsCollected"))
      #for(i in round.traits){df7[,i]<-round(df7[,i] ,4)}
      df7=data.frame(df7)

    }

    if(doLmer){

      DIBV = lmer(formula = feature ~ (1|LINE) + (1|YEAR) ,
                  na.action='na.exclude', REML = T,
                  control = lmerControl(
                    sparseX = T),
                  data = BV.HSIdentical.df[,c("YEAR","FIELD","MALE","FEMALE","feature","LINE")] )

      # 161
      # 1073
      sum.DIBV=print(summary(DIBV))
      Blup = ranef(DIBV)
      Blup=data.frame(Blup)
      Blup = Blup %>% dplyr::filter(grpvar != "MALE")
      #Blup = Blup %>% dplyr::filter(grpvar != "LINE")
      Blup = Blup %>% dplyr::filter(grpvar != "YEAR")

      #Blup.index = order(Blup$condval, decreasing=T)
      #Blup = Blup[Blup.index,]
      head(Blup,10)

      # results1=left_join(results, Blup, by=c("FEMALE"="grp"))
      # results1 = results1[,c(1,3,73)]
      # cor(results1$Yield_GCA, results1$condval,use="pairwise.complete.obs", method="pearson")^2
      #.9999

      VC<- vc(DIBV)
      N = length(levels(as.factor(BV.HSIdentical.df$YEAR)))
      FA = length(levels(as.factor(BV.HSIdentical.df$FIELD)))

      cat("Heritability = ",(as.numeric(VC$vcov[1]))/((as.numeric(VC$vcov[1]))+
                                                        (as.numeric(VC$vcov[3])/N)), "\n")


      #FEMALE/(FEMALE + Error)/NumofYears)
      #pred.DIBV = predict(DIBV)
      df5 = data.frame(LINE = Blup$grp,
                       PREDS = Blup$condval,
                       stderror = Blup$condsd)

      counts = dplyr::left_join(counts.adjusted, counts.adjusted.raw,by=c("LINE"="V1"))
      counts = data.frame(counts)
      counts$Observations = as.numeric(counts$Observations )
      counts$rowCount = as.numeric(counts$rowCount )

      counts[is.na(counts)] = 0

      counts$ObsPercent = counts$rowCount / counts$Observations
      counts = counts[, c(1,2,4)]
      #counts[is.na(counts) ] = 0

      #colnames(counts) = c("FEMALE", paste0(name,"_pctCounts"))
      accuracy<-round(sqrt(abs(1-(df5$stderror/sum.DIBV$varcor$LINE[1]))),2)

      df6<-dplyr::left_join(df5,counts,by="LINE")

      df6 = data.frame(LINE = df6$LINE,
                       GCA = df6$PREDS,
                       obs = df6$Observations,
                       obsperc = df6$ObsPercent,
                       BV = (df6$PREDS*2) + sum.DIBV$coefficients[1],
                       sterror = df6$stderror*2,
                       accuracy)

      colnames(df6)[2:7]=c(paste0(name,"_BLUP"),paste0(name,"_Observations"),paste0(name,"_PctPlotObsCollected"),
                           paste0(name,"_BLUE"),paste0(name,"_standard.error.BLUE"),paste0(name, "_accuracy.BLUE"))

    df7=df6

    rm(df6,VC,N,FA,Blup,DIBV,p95,p05,MALE,FEMALE,BV.HSIdentical.df.2,Blup.index,BV.HSIdentical.df.3)
    gc()
    }

    sigfigs.traits<-c( paste0(name,"_BLUE"),  paste0(name,"_BLUP"),
                       paste0(name,"_standard.error.BLUE"), paste0(name,"_accuracy.BLUE"),
                       paste0(name,"_PctPlotObsCollected"))

    for(r in sigfigs.traits){
      for(i in 1:nrow(df7)){
        if(nchar(signif(df7[i,r] ,3)) > 6 ){
          df7[i,r]<-round(df7[i,r] ,4)}else(
            df7[i,r]<-signif(df7[i,r] ,3))
      }
    }
    cat("K","\n")

    #write.csv(df7, file=paste0(fdph,name,"_BV-HybridID_",year,"S.csv")) #write BLUEs to spreadsheet
    assign(paste0(name,"_BV-HybridID_",folder,"S"), df7)
    #hist(df5[4], col="gold", main=paste0(name," Histogram"), xlab=paste0(name))
    #hist(df5[,2], col="blue", main=paste0("GCA Values For ", name), xlab=paste0("GCA Values"))
    #hist(df5[,4],main=paste0("BV Values For ", name), col="brown",xlab=paste0("Breeding Value (BV)"))
    ## Compare BLUEs to line averages on a scatterplot
    #plot(df5[,2],df5[,3], main=paste0("GCA Values By ", name," Plot"),xlab=paste0(name),ylab="GCA Values")
    #xy<-qqmath(ranef(BV.HSIdentical))
    #print(xy)
    rm(df5,df7,df6,df3.gmean,df3,std.errors, accuracy, se.BV, BV, pred.female, pred, mu.female,mu.idx.female,
       mu,BV.HSIdentical.fitted,BV.HSIdentical.df.gmean,BV.HSIdentical.blues.exp,BV.HSIdentical.blues.field,
       BV.HSIdentical.blues.line,BV.HSIdentical.blues, counts, BV.HSIdentical.blues.export,qualdat.blups,
       BV.HSIdentical.model.predicted,BV.HSIdentical.model,counts.adjusted.raw,pctCounts, QUALDAT.SUM)
    invisible(gc())

  }

  sink()
  dev.off()
  cat("L","\n")

  ######################################################################################################
  ######Submit report
  ######################################################################################################
  #setwd("P:/Breeding Values")
  #yld = read.csv(paste0(fdph,"Yield_BV-HybridID_",year,"S.csv"))
  #pw = read.csv("plot_wt_BV_20S.csv")
  #phho = read.csv(paste0(fdph,"PCT.HOH_BV-HybridID_",year,"S.csv"))
  #y_m = read.csv(paste0(fdph,"Y.M_BV-HybridID_",year,"S.csv"))
  #ph = read.csv(paste0(fdph,"Plt.Height_BV-HybridID_",year,"S.csv"))
  #eh = read.csv(paste0(fdph,"EarHt_BV-HybridID_",year,"S.csv"))
  #tstwt = read.csv(paste0(fdph,"Test.WT_BV-HybridID_",year,"S.csv"))
  #rlec = read.csv("rl_early_count_BV_20S.csv")
  #rlp = read.csv(paste0(fdph,"RL.._BV-HybridID_",year,"S.csv"))
  #slp = read.csv(paste0(fdph,"SL.._BV-HybridID_",year,"S.csv"))
  #slc = read.csv(paste0(fdph,"SL.Count_BV-HybridID_",year,"S.csv"))
  #gse = read.csv("gs_early_BV_20S.csv")
  #gsl = read.csv(paste0(fdph,"GS.Late_BV-HybridID_",year,"S.csv"))
  #scf = read.csv(paste0(fdph,"StandCnt..Final._BV-HybridID_",year,"S.csv"))
  #scuav = read.csv(paste0(fdph,"StandCnt..UAV._BV-HybridID_",year,"S.csv"))

  BV.traits.1 <- left_join(eval(as.name(paste0("Yield_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],eval(as.name(paste0("PCT.HOH_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)], by=c("LINE"));dim(BV.traits.1)
  BV.traits.2 <- left_join(BV.traits.1,eval(as.name(paste0("Y.M_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.2)
  BV.traits.3 <- left_join(BV.traits.2,eval(as.name(paste0("Plt.Height_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.3)
  BV.traits.4 <- left_join(BV.traits.3,eval(as.name(paste0("EarHt_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.4)
  BV.traits.5 <- left_join(BV.traits.4,eval(as.name(paste0("Test.WT_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.5)
  #BV.traits.6 <- left_join(BV.traits.5,rlec[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.6)
  BV.traits.6 <- left_join(BV.traits.5,eval(as.name(paste0("RL.._BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.5)
  BV.traits.7 <- left_join(BV.traits.6,eval(as.name(paste0("RL.Count_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.6)
  BV.traits.8 <- left_join(BV.traits.7,eval(as.name(paste0("SL.._BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.7)
  BV.traits.9 <- left_join(BV.traits.8,eval(as.name(paste0("SL.Count_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.8)
  #BV.traits.11 <- left_join(BV.traits.10,gse[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.11)
  BV.traits <- left_join(BV.traits.9,eval(as.name(paste0("GS.Late_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits)
  #BV.traits.11 <- left_join(BV.traits.10,eval(as.name(paste0("StandCnt..Final_BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));dim(BV.traits.10)
  #BV.traits <- left_join(BV.traits.10,eval(as.name(paste0("StandCnt..UAV._BV-HybridID_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("LINE"));colnames(BV.traits)[1]<-"INBRED";dim(BV.traits)
  #BV.traits.dup<-BV.traits[duplicated(BV.traits$INBRED),];dim(BV.traits.dup)
  BV.traits$SL.._BLUE = ifelse(BV.traits$SL.._BLUE <0, 0 , BV.traits$SL.._BLUE)
  BV.traits$RL.._BLUE = ifelse(BV.traits$RL.._BLUE <0, 0 , BV.traits$RL.._BLUE)
  BV.traits$SL.Count_BLUE = ifelse(BV.traits$SL.Count_BLUE <0, 0 , BV.traits$SL.Count_BLUE)
  BV.traits$RL.Count_BLUE = ifelse(BV.traits$RL.Count_BLUE <0, 0 , BV.traits$RL.Count_BLUE)
  BV.traits$GS.Late_BLUE = ifelse(BV.traits$GS.Late_BLUE <0, 0 , BV.traits$GS.Late_BLUE)

  BV.traits$SL.Count_BLUE = round(BV.traits$SL.Count_BLUE,0)
  BV.traits$RL.Count_BLUE = round(BV.traits$RL.Count_BLUE,0)
  BV.traits$GS.Late_BLUE = round(BV.traits$GS.Late_BLUE,0)



  BV.traits = data.table(BV.traits)
  BV.traits.long=BV.traits %>%
    tidyr::pivot_longer(cols = -LINE, names_to = c(".value", "metric"),
                        names_sep="_")

  #BV.traits.long = read.xlsx(paste0(fdp,"/Traits.BV-HybridID.",year,"S.xlsx"), 1)
  BV.traits.long$INBRED = as.character(BV.traits.long$LINE)

  setDT(BV.traits.long)[, paste0("INBRED", 1:2) := tstrsplit(INBRED, " + ", fixed = T)]

  BV.traits.long$female = BV.traits.long$INBRED1
  BV.traits.long$male = BV.traits.long$INBRED2
  colnames(BV.traits.long)[c(1,15,16)] = c("LINE","linkInbred", "crossedWith")
  cat("----------------------------Writing To File----------------------------", "\n")
  #write.xlsx(BV.traits.long, paste0(fdp,"/Traits.BV-HybridID.",year,"S.xlsx"), row.names=F, showNA=F)
  names(BV.traits.long) <- gsub("\\.", "", names(BV.traits.long))

  # wb<-createWorkbook(type="xlsx")
  # CellStyle(wb, dataFormat=NULL, alignment=NULL,
  #           border=NULL, fill=NULL, font=NULL)
  # Lines <- createSheet(wb, sheetName = "Lines")
  # addDataFrame(data.frame(BV.traits.long),Lines, startRow=1, startColumn=1,row.names=F)
  write.xlsx(BV.traits.long, paste0(fdp,"/",folder,"/",".Traits.BV-HybridID.",folder,
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
                                    ,".xlsx"),overwrite=T)
  cat("----------------------------Done!----------------------------", "\n")
  cat("No bugs for Hybrid Breeding Values")

  #sink(file=paste0(fdp,"BV_time.txt"),split=TRUE)

  cat(proc.time() - ptm)
  #sink()
}



