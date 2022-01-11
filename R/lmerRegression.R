# library(data.table)
# library(tidyverse)
# #library(stats)
# library(lme4)
# library("lucid")
# library(BreedStats)
# library(asreml)
# hdp = "C:/Users/jake.lamkey/Documents/"
# name="Yield"
# results = openxlsx::read.xlsx("R:/Breeding/MT_TP/Models/Breeding Values/21/.Traits.BV.21AProp.xlsx",1)
# BV.HSIdentical.df = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

lmerBV = function(name,BV.HSIdentical.df,df3.gmean,counts){
  ######################################################
  # BV.HSIdentical.df = BV.HSIdentical.df %>% filter(Plot.Discarded != "Yes",
  #                                                  Plot.Status != "3 - Bad", !is.na(Yield) )

  #  BV.HSIdentical.df.expmean = aggregate(BV.HSIdentical.df[,"Yield"],
  #                                        list(BV.HSIdentical.df$Book.Name),
  #                                        mean,na.rm=T)
  # # #bv.mean = mean(BV.HSIdentical.df$Yield,na.rm=T)
  #  colnames(BV.HSIdentical.df.expmean) = c("Book.Name","Yieldmean")
  #  BV.HSIdentical.df = left_join(BV.HSIdentical.df, BV.HSIdentical.df.expmean, by="Book.Name" )
  #BV.HSIdentical.df$Yieldmean = NA
  #BV.HSIdentical.df$Yield = ifelse(is.na(BV.HSIdentical.df$Yield), BV.HSIdentical.df$Yieldmean,BV.HSIdentical.df$Yield)
  BV.HSIdentical.df = data.frame(BV.HSIdentical.df)
  BV.HSIdentical.df$feature = BV.HSIdentical.df[,name]

  # male.rmdups = data.frame(inbred = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$MALE), c("MALE","LINE" )] )
  # female.rmdups = data.frame(inbred = BV.HSIdentical.df[!duplicated(BV.HSIdentical.df$FEMALE), c("FEMALE","LINE" )] )
  # maleinbreds = anti_join(male.rmdups,female.rmdups)
  # maleinbreds.2 = anti_join(male.rmdups,maleinbreds)
  #
  # malesubset <- BV.HSIdentical.df[which(BV.HSIdentical.df$MALE %in% maleinbreds$inbred.MALE),]

  BV.HSIdentical.df.2 = BV.HSIdentical.df[,c(1:29,31,30,32:34)]
  colnames(BV.HSIdentical.df.2)[c(30:31)] = c("MALE","FEMALE")

  BV.HSIdentical.df = rbind(BV.HSIdentical.df.2,BV.HSIdentical.df)
  BV.HSIdentical.df = data.frame(BV.HSIdentical.df)
  FEMALE = length(levels(as.factor(BV.HSIdentical.df$FEMALE)))
  MALE = length(levels(as.factor(BV.HSIdentical.df$MALE)))

  p95 <- quantile(BV.HSIdentical.df$feature, 0.9999, na.rm=T)
  p05 <- quantile(BV.HSIdentical.df$feature, 0.0001, na.rm=T)

  # BV.HSIdentical.df <- BV.HSIdentical.df %>% filter(feature <= p95,
  #                                                    feature >= p05)

  DIBV = lmer(formula = feature ~ (1|FEMALE) + (1|MALE) + (1|YEAR) ,
              na.action='na.exclude', REML = T,
              control = lmerControl(
                sparseX = T),
              data = BV.HSIdentical.df[,c("YEAR","FIELD","MALE","FEMALE","feature","LINE")] )

  # 161
  # 1073
  sum.DIBV=print(summary(DIBV))
  Blup = ranef(DIBV)
  Blup=data.frame(Blup)
  Blup = Blup %>% filter(grpvar != "MALE")
  Blup = Blup %>% filter(grpvar != "LINE")
  Blup.index = order(Blup$condval, decreasing=T)
  Blup = Blup[Blup.index,]
  head(Blup,10)

  # results1=left_join(results, Blup, by=c("FEMALE"="grp"))
  # results1 = results1[,c(1,3,73)]
  # cor(results1$Yield_GCA, results1$condval,use="pairwise.complete.obs", method="pearson")^2
  #.9999

  VC<- vc(DIBV)
  N = length(levels(as.factor(BV.HSIdentical.df$YEAR)))
  FA = length(levels(as.factor(BV.HSIdentical.df$FIELD)))

  cat("Heritability = ",(as.numeric(VC$vcov[1]))/((as.numeric(VC$vcov[1]))+
                                  (as.numeric(VC$vcov[4])/N)))


  #FEMALE/(FEMALE + Error)/NumofYears)
  #pred.DIBV = predict(DIBV)
  df5 = data.frame(FEMALE = Blup$grp,
                         PREDS = Blup$condval,
                         stderror = Blup$condsd)

  counts = dplyr::left_join(counts.adjusted, counts.adjusted.raw,by=c("FEMALE"="V1"))
  counts = data.frame(counts)
  counts$Observations = as.numeric(counts$Observations )
  counts$rowCount = as.numeric(counts$rowCount )

  counts[is.na(counts)] = 0

  counts$ObsPercent = counts$rowCount / counts$Observations
  counts = counts[, c(1,2,4)]
  #counts[is.na(counts) ] = 0

  #colnames(counts) = c("FEMALE", paste0(name,"_pctCounts"))
  accuracy<-round(sqrt(abs(1-(df5$stderror/sum.DIBV$varcor$FEMALE[1]))),2)

  df6<-dplyr::left_join(df5,counts,by="FEMALE")
  df6 = data.frame(FEMALE = df6$FEMALE,
                   GCA = df6$PREDS,
                   obs = df6$Observations,
                   obsperc = df6$ObsPercent,
                   BV = (df6$PREDS*2) + sum.DIBV$coefficients[1],
                   sterror = df6$stderror*2,
                   accuracy)

  colnames(df6)[2:7]=c(paste0(name,"_GCA"),paste0("Observations_",name),paste0("PctPlotObsCollected_",name),
                       paste0(name,"_Breeding.Value"),paste0(name,"standard.error.Breeding.Value"),paste0(name,
                        "accuracy.Breeding.Value"))

  rm(VC,N,FA,Blup,DIBV,p95,p05,MALE,FEMALE,BV.HSIdentical.df.2,Blup.index)
  gc()

  return(data.frame(pred.DIBV))


}



