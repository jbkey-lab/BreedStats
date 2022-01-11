
asremlBV = function(name,BV.HSIdentical.df,counts,df3.gmean){

  BV.HSIdentical.model = asreml(fixed = feature ~ YEAR,
                                random = ~ FEMALE + and(MALE,1),
                                residual = ~units,
                                data = BV.HSIdentical.df[,c("YEAR","FIELD","MALE","FEMALE","feature")],
                                equate.levels = c("FEMALE","MALE"),
                                workspace = "31gb",  #,
                                na.action = na.method(y = c("include"), x = c("include"))
  )
  #rm(BV.HSIdentical.df.data)
  plot(BV.HSIdentical.model)
  print(name)
  QUALDAT.SUM <- print(summary(BV.HSIdentical.model))
  invisible(gc())

  # BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.df, fitted(BV.HSIdentical.model))
  # df3.gmean.male = aggregate(BV.HSIdentical.model.predicted[,c(paste0("fitted.BV.HSIdentical.model."))],
  #                            by=list(MALE = BV.HSIdentical.model.predicted$MALE), mean, na.rm=T)
  # colnames(df3.gmean.male)[1] = c('FEMALE')
  #
  # df3.gmean.female = aggregate(BV.HSIdentical.model.predicted[,c(paste0("fitted.BV.HSIdentical.model."))],
  #                            by=list(FEMALE = BV.HSIdentical.model.predicted$FEMALE), mean, na.rm=T)
  #
  # BV.HSIdentical.model.predicted = rbind(df3.gmean.female, df3.gmean.male)
  #
  # BV.HSIdentical.model.predicted = aggregate(BV.HSIdentical.model.predicted[,c(paste0("fitted.BV.HSIdentical.model."))],
  #                              by=list(FEMALE = BV.HSIdentical.model.predicted$FEMALE), mean, na.rm=T)
  #
  # options(pworkspace = "24gb", fail='soft')

  cat("J", "\n")
  invisible(gc())

  BV.HSIdentical.model.predicted<- predict.asreml(BV.HSIdentical.model,
                                                  classify="MALE:FEMALE",
                                                  #pworkspace="32gb",
                                                  parallel=T,
                                                  aliased = T
                                                  #maxit=1
                                                  #vcov = T,
                                                  #avsed=T,


  )


  # BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)
  cat("K", "\n")
  HTvca<-summary(BV.HSIdentical.model)$varcomp$component
  Va<-HTvca[1]*2
  Vp<-sum(HTvca[-c(3)])
  h2=Va/Vp
  print(paste0("Heritability = ", h2))


  #######################################
  ######BLUPs
  #######################################
  qualdat.blups<-coef(BV.HSIdentical.model)$fixed#determine the fixed effects (BLUPS)
  #qualdat.blups<-data.frame(qualdat.blups$hybridID)
  #qualdat.blups<-setDT(qualdat.blups,keep.rownames=T)[];colnames(qualdat.blups)=c("hybridID",paste0(name,"BLUP"))
  #qualdat.blups.export<-left_join(qualdat.blups, qualdat.gmean, by="hybridID")
  #write.csv(qualdat.blups.export,paste0(name,"_BLUPs_19S.csv")) #write blups to spreadsheet

  #######################################
  ######GCAS
  #######################################
  # Create a numeric vector with the BLUP for each line
  BV.HSIdentical.blues <- coef(BV.HSIdentical.model)$random #determine the fixed effects (blues)
  BV.HSIdentical.blues <- setDT(data.frame(BV.HSIdentical.blues),keep.rownames=T)[]; colnames(BV.HSIdentical.blues)=c("LINE",paste0(name,"_BV")); word.remove.fem="FEMALE_"; word.remove.mal="MALE_"
  #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues$LINE)
  #BV.HSIdentical.blues$LINE = gsub(paste0(word.remove.mal,collapse = "|"), "", BV.HSIdentical.blues$LINE)
  BV.HSIdentical.blues.female <- BV.HSIdentical.blues %>% filter(grepl("FEMALE_", LINE));dim(BV.HSIdentical.blues.female);colnames(BV.HSIdentical.blues.female)[1]="FEMALE"
  BV.HSIdentical.blues.female$FEMALE = gsub(paste0(word.remove.fem,collapse = "|"), "", BV.HSIdentical.blues.female$FEMALE)
  BV.HSIdentical.blues.field <- BV.HSIdentical.blues %>% filter(grepl("FIELD_", LINE));dim(BV.HSIdentical.blues.field)
  BV.HSIdentical.blues.exp <- BV.HSIdentical.blues %>% filter(grepl("EXP_", LINE));dim(BV.HSIdentical.blues.exp)

  #BV.HSIdentical.blues.female
  BV.HSIdentical.blues.export <- left_join(BV.HSIdentical.blues.female, BV.HSIdentical.df.gmean, by="FEMALE")
  head(BV.HSIdentical.blues.export); dim(BV.HSIdentical.blues.export)
  #write.csv(BV.HSIdentical.blues.export,paste0(name,"_BV_19S.csv")) #write blues to spreadsheet

  ######FIND SE, etc...
  mu.idx = which(BV.HSIdentical.model$factor.names=='(Intercept)')
  mu=BV.HSIdentical.model$coefficients$random[mu.idx]
  mu.idx.female = which(BV.HSIdentical.model$factor.names=='FEMALE')
  mu.female=BV.HSIdentical.model$coefficients$random[mu.idx.female]

  #fam.idx=BV.HSIdentical.model$coefficients=='FEMALE'
  pred<-BV.HSIdentical.model.predicted$pvals
  pred.female<-BV.HSIdentical.model.predicted$pvals$FEMALE
  BV=pred$predicted.value
  se.BV = BV.HSIdentical.model.predicted$pvals$std.error
  accuracy<-round(sqrt(abs(1-(se.BV/HTvca[1]))),2)
  std.errors<-cbind(data.frame(pred.female), BV, se.BV, accuracy)
  std.errors=data.frame(std.errors);colnames(std.errors)=c("FEMALE",paste0(name,"_Breeding.Value"),paste0(name,"_standard.error.Breeding.Value"),paste0(name,"_accuracy.Breeding.Value"))
  #############################################
  ######predicted values with fitted() function
  #############################################
  #BV.HSIdentical.blues = data.frame(predict(BV.HSIdentical)) #predict the dataset to get BLUE Values 2866
  #df3<-cbind(data.frame(BV.HSIdentical.data$EXP),data.frame(BV.HSIdentical.data$hybridID),data.frame(BV.HSIdentical.blues)); colnames(df3)=c("EXP","hybridID","BLUE")
  #df3.gmean = aggregate(df3[,c("BLUE")], by=list(hybridID = df3$hybridID),mean,na.rm=T);colnames(df3.gmean) = c('hybridID',paste0(name)); df3.gmean=data.frame(df3.gmean)
  #df3<-df3[!duplicated(df3[,1]),]
  #BV.HSIdentical.blues.filtered<-na.omit(df3)
  #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet

  BV.HSIdentical.fitted = fitted(BV.HSIdentical.model) #predict the dataset
  df3<-cbind(BV.HSIdentical.fitted,BV.HSIdentical.df); colnames(df3)[1]=c(paste0("fitted_",name))
  df3.gmean.male = aggregate(df3[,c(paste0("fitted_",name))], by=list(MALE = df3$MALE),mean,na.rm=T);colnames(df3.gmean.male) = c('FEMALE',paste0("fitted_",name)); df3.gmean.male=data.frame(df3.gmean.male)
  df3.gmean.female = aggregate(df3[,c(paste0("fitted_",name))], by=list(FEMALE = df3$FEMALE),mean,na.rm=T);colnames(df3.gmean.female) = c('FEMALE',paste0("fitted_",name)); df3.gmean.female=data.frame(df3.gmean.female)
  df3.gmean <- rbind(df3.gmean.male,df3.gmean.female)

  #df3<-df3[!duplicated(df3[,1]),]
  #BV.HSIdentical.blues.filtered<-na.omit(df3)
  #write.csv(df3.gmean, file=paste0(name,"_BLUEs_19S.csv")) #write BLUEs to spreadsheet

  #######################################
  ######Graphs and Figures
  #######################################
  df5<-left_join(BV.HSIdentical.blues.export,df3.gmean,by="FEMALE");dim(df5)
  df5<-df5[!duplicated(df5$FEMALE),]

  counts = left_join(counts.adjusted, counts.adjusted.raw,by=c("FEMALE"="V1"))
  counts = data.frame(counts)
  counts$Observations = as.numeric(counts$Observations )
  counts$rowCount = as.numeric(counts$rowCount )

  counts[is.na(counts)] = 0

  counts$ObsPercent = counts$rowCount / counts$Observations
  counts = counts[, c(1,2,4)]
  #counts[is.na(counts) ] = 0

  #colnames(counts) = c("FEMALE", paste0(name,"_pctCounts"))

  df6<-left_join(df5,counts,by="FEMALE")
  df7<-left_join(df6,std.errors,by="FEMALE")
  df7<-df7[,-c(3,4)];colnames(df7)[2]=paste0(name,"_GCA")
  df7 = data.frame(df7)
  #sigfigs to three digits

  # for(i in sigfigs.traits){df7[,i]<-signif(df7[,i] ,3)}
  #df7 = left_join(df7, counts[,c(1,2)], by=c("FEMALE"="FEMALE"))
  colnames(df7)[3:4]=c(paste0("Observations_",name),paste0("PctPlotObsCollected_",name))

  return(data.frame(df7))
}
