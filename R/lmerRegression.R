
# library(data.table)
# library(tidyverse)
# library(stats)
# library(lme4)
# library("lucid")
# library(asreml)

lmerBV = function(){
  ######################################################
  hdp = "C:/Users/jake.lamkey/Documents/"

  trainingx2 = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

  trainingx2 = transform(trainingx2,
                         #male   = factor(male),
                         #female = factor(female),
                         Yield = as.numeric(Yield),
                         LINE = factor(LINE),
                         FIELD=factor(FIELD),
                         EXP=factor(EXP),
                         YEAR=factor(YEAR),
                         MALE=factor(MALE),
                         FEMALE = factor(FEMALE)
                         # PopID=factor(PopID)
                         #REP=factor(REP)
  )

  BV.HSIdentical.model = asreml(fixed = Yield ~ YEAR,
                                random =  ~ FEMALE + and(MALE,1) ,
                                residual = ~units,
                                data = trainingx2[,c("YEAR","FIELD","MALE","FEMALE","Yield")],
                                equate.levels = c("FEMALE","MALE"),
                                workspace = "31gb",  #,
                                na.action = na.method(y = c("include"), x = c("include"))
  )


  #rm(BV.HSIdentical.df.data)
  plot(BV.HSIdentical.model)
  print(name)
  QUALDAT.SUM <- print(summary(BV.HSIdentical.model))
  invisible(gc())
  preds.asreml = data.frame(trainingx2$LINE ,BV.HSIdentical.model[["linear.predictors"]])
  HTvca<-summary(BV.HSIdentical.model)$varcomp$component
  Va<-HTvca[1]*2
  Vp<-sum(HTvca[-c(3)])
  h2=Va/Vp
  print(paste0("Heritability = ", h2))


  #####

  PRED = predict(BV.HSIdentical.model, classify="MALE:FEMALE",
                                       parallel = T,
                                        aliased =T)

  preds = data.frame(PRED[["pvals"]])

  DIBV = lmer(formula = Yield ~ (1|MALE) + (1|FEMALE) + YEAR,
              na.action='na.exclude', REML = T, data=trainingx2 )

  VC<- vc(DIBV)
  N = length(levels(trainingx2$YEAR))
  FA = length(levels(trainingx2$FIELD))

  print((as.numeric(VC$vcov[1]))/((as.numeric(VC$vcov[1]))+(as.numeric(VC$vcov[3]))/(as.numeric(N))))

  pred.DIBV = predict(DIBV)
  pred.DIBV = data.frame(pred.DIBV, trainingx2$LINE)



  totalInbreds = full_join(Male.rmdups,FEMale.rmdups, by=character())
  pred.DIBV=aggregate(pred.DIBV, by="")



  all.preds = data.frame(preds.asreml, pred.DIBV, trainingx2$Yield)
  colnames(all.preds) = c("Line","ASRemlPreds", "lmerPreds","line2","Yield")
  all.preds = all.preds %>% transform(ASRemlPreds = as.numeric(ASRemlPreds),
                                      lmerPreds = as.numeric(lmerPreds),
                                      Yield = as.numeric(Yield))
  all.preds = na.omit(all.preds)
  sqrt(mean((all.preds$ASRemlPreds - all.preds$Yield)^2))

  cor(all.preds[,c(2,3,5)])









}
