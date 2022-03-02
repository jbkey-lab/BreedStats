######################################################################################################
######Rscript for Breeding Values (GCA)
######################################################################################################
#rm(list=ls()) #remove environment vairalbes
invisible(gc(reset=T)) #cleans memory "garbage collector"
#memory.limit(size=32000)
#setwd("R:/Breeding/MT_TP/Models/Breeding Values")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################

# cat("----------------------------Loading Packages----------------------------", "\n")

# if(!(suppressWarnings(suppressMessages(require(openxlsx, warn.conflicts = FALSE))))){
#   install.packages("openxlsx")
#   suppressWarnings(suppressMessages(library(openxlsx, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(dplyr, warn.conflicts = FALSE))))){
#   install.packages("dplyr")
#   suppressWarnings(suppressMessages(library(dplyr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(data.table, warn.conflicts = FALSE))))){
#   install.packages("data.table")
#   suppressWarnings(suppressMessages(library(data.table, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(pastecs, warn.conflicts = FALSE))))){
#   install.packages("pastecs")
#   suppressWarnings(suppressMessages(library(pastecs, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(tidyr, warn.conflicts = FALSE))))){
#   install.packages("tidyr")
#   suppressWarnings(suppressMessages(library(tidyr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(stringr, warn.conflicts = FALSE))))){
#   install.packages("stringr")
#   suppressWarnings(suppressMessages(library(stringr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(tidyverse, warn.conflicts = FALSE))))){
#   install.packages("tidyverse")
#   (suppressWarnings(suppressMessages(library(tidyverse, warn.conflicts = FALSE))))
# }
#
# if(!(suppressWarnings(suppressMessages(require(doParallel, warn.conflicts = FALSE))))){
#   install.packages("doParallel")
#   (suppressWarnings(suppressMessages(library(doParallel, warn.conflicts = FALSE))))
# }
#
# if(!(suppressWarnings(suppressMessages(require(ggplot2, warn.conflicts = FALSE))))){
#   install.packages("ggplot2")
#   (suppressWarnings(suppressMessages(library(ggplot2, warn.conflicts = FALSE))))
# }
#folder="Test"
# x="10_19_2021"
# # ws = paste0('/home/jacoblamkey/Downloads/Peds/YT_BV Yield Trial Master Catalog ',x, ".csv")
# # fdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder)
# # fdph = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/Hybrid")
# # wdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/")
# # vdp = '/home/jacoblamkey/Downloads/Peds/Variety.male.female.csv'
# #
#  ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
#  fdp = paste0("C:/Users/jake.lamkey/Documents/")
# # # fdph = paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder,"/Hybrid")
#  wdp = paste0("C:/Users/jake.lamkey/Documents/")
#  vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'
# # # # #
# # # # ws = ws
# # # doHybridID=T
# # # doPedigreeChange =F
# # # doPedigreeToBecksChange=F
# # # doGCABV = T
# # # doWriteFinalPedigrees = F
# # year= "2021"
# # #
# A = T
# B = F
# C = F
# Prop = T
# Choice = F
# D=F
# R=F
# X=F
# E=F
# Q=F
# V=F
# GEM=F
# #
# s0=T
# s1=T
# s2=T
# s3=T
# s4=F
# s5=F
# doDNN=F
# #
# seas0=21
# seas1=20
# seas2=19
# seas3=18
# seas4=""
# seas5=""
# doYear=T
# #
# #
# # doYear = doYear
# # doHybridID = doHybridID
# # doPedigreeChange = doPedigreeChange
# # doPedigreeToBecksChange = doPedigreeToBecksChange
# # doGCABV = doGCABV
# # doWriteFinalPedigrees = doWriteFinalPedigrees
# A = A
# B = B
# C = C
# Prop = Prop
# Choice = Choice
# D=D
# R=R
# X=X
# E=E
# Q=Q
# V=V
# GEM=GEM
# s0=s0
# s1=s1
# s2=s2
# s3=s3
# s4=s4
# s5=s5
# season0=seas0
# season1=seas1
# season2=seas2
# season3=seas3
# season4=seas4
# season5=seas5
# folder=folder
# fdp=as.character(fdp)

BV = function(fdp ,
              #fdph = fdph,
              wdp ,
              ws ,
            #  vdp,
              doHybridID,
              doPedigreeChange,
              doPedigreeToBecksChange,
              doGCABV ,
              doWriteFinalPedigrees,
              year,
              #season = varYear,
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
              doDNN ,
              GEM,
              s0,
              s1,
              s2,
              s3,
              s4,
              s5,
              seas0,
              seas1,
              seas2,
              seas3,
              seas4,
              seas5,
              doYear,
              folder,
            simulate,
            InventoryPedigree,
            ytData,
            date,
            doReduceNonCodes,
            doField,
            doLmer
              )
{
  #sink(file=paste0("C:/Users/jake.lamkey/Desktop/BV_debugger.txt"),split=TRUE)


  #year=year
  #season=season
  # ws=ws
  # wdp=wdp
  # doYear = doYear
  # doHybridID = doHybridID
  # doPedigreeChange = doPedigreeChange
  # doPedigreeToBecksChange = doPedigreeToBecksChange
  # doGCABV = doGCABV
  # doWriteFinalPedigrees = doWriteFinalPedigrees
  # A = A
  # B = B
  # C = C
  # Prop = Prop
  # Choice = Choice
  # D=D
  # R=R
  # X=X
  # E=E
  # Q=Q
  # V=V
  # GEM=GEM
  # s0=s0
  # s1=s1
  # s2=s2
  # s3=s3
  # s4=s4
  # s5=s5

  season0=as.numeric(seas0)
  season1=as.numeric(seas1)
  season2=as.numeric(seas2)
  season3=as.numeric(seas3)
  season4=as.numeric(seas4)
  season5=as.numeric(seas5)
  folder=folder
  fdp=as.character(fdp)

  #fdp = "R:/Breeding/MT_TP/Models/Breeding Values/2020"
  #ws =
  wd="_withkeithanalysis"
  cat("A", "\n")
  # testit <- function(x)
  # {
  #   p1 <- proc.time()
  #   Sys.sleep(x)
  #   proc.time() - p1 # The cpu usage should be negligible
  # }

  if(doPedigreeChange){
    cat("B", "\n")

    ptm <- proc.time()

    cat("Changing pedigrees...","\n")
    #source("R:/Breeding/MT_TP/Models/R-Scripts/PedigreeEngine.R")
    BV.HSIdentical.df=pedigreeEngine(ws,
                                     season0=season0,
                                     season1=season1,
                                     season2=season2,
                                     season3=season3,
                                     season4=season4,
                                     season5=season5,
                                     A=A ,
                                     B=B ,
                                     C=C ,
                                     Prop=Prop ,
                                     Choice =Choice ,
                                     D=D,
                                     R=R,
                                     X=X,
                                     E=E,
                                     Q=Q,
                                     V=V,
                                     GEM=GEM,
                                     s0=s0,
                                     s1=s1,
                                     s2=s2,
                                     s3=s3,
                                     s4=s4,
                                     s5=s5,
                                     simulate=simulate,
                                     InventoryPedigree=InventoryPedigree,
                                     ytData=ytData,
                                     date=date,
                                     doReduceNonCodes=doReduceNonCodes)
    #BV.HSIdentical.df = BV.HSIdentical.df[,-c(2,3,4,5,6,7,8,22,23,24,17,18)]
    write.csv(BV.HSIdentical.df, paste0(wdp,"/BV.HSIdentical.df.csv"))
    BV.HSIdentical.df = data.table::fread(paste0(wdp,"/BV.HSIdentical.df.csv"))

    cat(proc.time() - ptm )
    cat("Read in ASReml ready file", "\n")

    sink()

  }

  if(!doPedigreeChange){
    #BV.HSIdentical.df<-fread(paste0(fdp,"nested.linked.peds.xlsx"))#all varieties to build the model
    #R:/Breeding/MT_TP/Models/Data/Department Data/
    cat("B", "\n")
    ptm <- proc.time()


    BV.HSIdentical.df = data.table::fread(paste0("R:/Breeding/MT_TP/Models/Data/Department Data","/BV.HSIdentical.df.csv"))
    #BV.HSIdentical.df = BV.HSIdentical.df[,-c(2,3,4,5,6,7,8,22,23,24,17,18)]

    cat("Read in ASReml ready file", "\n")
    #testit(3.7)

  }

  #testit(3.7)

  cat("C", "\n")

  #testit(3.7)


  #colnames(BV.HSIdentical.df)

  #head(BV.HSIdentical.df)

  #dim(BV.HSIdentical.df)

  cat("----------------------------Breeding Values----------------------------", "\n")

  #library(asreml);name="Yield"\
  ############################
  ######use a for loop to do mulitple traits at once
  ############################
  if(doGCABV){
    # testit <- function(x)
    #  cat("E", "\n")

    #setwd(paste0(fdp))
    #str(BV.HSIdentical.df)
    l<-length(BV.HSIdentical.df)#; l
    names<-names(BV.HSIdentical.df[,c(10:18,20:22)]); names
    classes<-sapply(BV.HSIdentical.df[c(10:22)], class); classes
    #cat(paste0(fdp),"\n")
    cat("F", "\n")


    sink(file=paste0(fdp,"/",folder,"/BV_",folder,
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

    pdf(file = paste0(fdp,"/",folder,"/BV_",folder,
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
    #name="Yield"
    cat("G", "\n")

    for(name in names){

      if( "feature" %in% colnames(BV.HSIdentical.df)){
        BV.HSIdentical.df = BV.HSIdentical.df %>% dplyr::select(-feature)
      }
      cat("H", "\n")
      nameCol = paste0(name)
      BV.HSIdentical.df = data.frame(BV.HSIdentical.df)
      BV.HSIdentical.df$feature = BV.HSIdentical.df[,name]
      #BV.HSIdentical.df = cbind( BV.HSIdentical.df, BV.HSIdentical.df[,name] )
      #BV.l=length(BV.HSIdentical.df)
      #colnames(BV.HSIdentical.df)[[BV.l]] = "feature"
      BV.HSIdentical.df = transform(BV.HSIdentical.df,
                                    #male   = factor(male),
                                    #female = factor(female),
                                    feature = as.numeric(feature),
                                    LINE = factor(LINE),
                                    FIELD=factor(FIELD),
                                    EXP=factor(EXP),
                                    YEAR=factor(YEAR),
                                    MALE=factor(MALE),
                                    FEMALE = factor(FEMALE)
                                    # PopID=factor(PopID)
                                    #REP=factor(REP)
      )

      dim(BV.HSIdentical.df)
      colnames(BV.HSIdentical.df);head(BV.HSIdentical.df)


      Unique.Female.Pedigree <- as.matrix(BV.HSIdentical.df[,"FEMALE"])
      Unique.Male.Pedigree <- as.matrix(BV.HSIdentical.df[,"MALE"])
      Unique.Pedigree <- rbind(Unique.Female.Pedigree, Unique.Male.Pedigree);Unique.Pedigree<-data.table(Unique.Pedigree)
      counts.adjusted <- Unique.Pedigree[, .(rowCount = .N), by = V1 ]; colnames(counts.adjusted)=c("FEMALE",paste0(name,"_Observations"))
      counts.adjusted.index = order(counts.adjusted$FEMALE)
      counts.adjusted = counts.adjusted[counts.adjusted.index,]

      BV.HSIdentical.df = BV.HSIdentical.df %>% dplyr::filter(Plot.Discarded != "Yes",
                                                       Plot.Status != "3 - Bad" )

      BV.HSIdentical.df.counts = BV.HSIdentical.df %>% dplyr::filter(feature >= 0)
      Unique.Female.Pedigree <- as.matrix(BV.HSIdentical.df.counts[,"FEMALE"])
      Unique.Male.Pedigree <- as.matrix(BV.HSIdentical.df.counts[,"MALE"])
      Unique.Pedigree <- rbind(Unique.Female.Pedigree, Unique.Male.Pedigree);Unique.Pedigree<-data.table(Unique.Pedigree)
      counts.adjusted.raw <- Unique.Pedigree[, .(rowCount = .N), by = V1 ]; colnames(counts.adjusted)=c("FEMALE","Observations")
      counts.adjusted.raw.index = order(counts.adjusted.raw$V1)
      counts.adjusted.raw = counts.adjusted.raw[counts.adjusted.raw.index,]

      #######################################
      ######Graphs and Figures
      #######################################
      attach(BV.HSIdentical.df)
      boxplot(feature~FIELD, xlab="FIELD", ylab=paste0(name), main=paste0(name," by FIELD"), col="pink")
      #boxplot(trait~EXP, xlab="EXP", ylab=paste0(name), main=paste0(name," by EXP"), col="pink")
      #boxplot(trait~LINE, xlab="LINE", ylab=paste0(name), main=paste0(name," by LINE"), col="pink")
      detach(BV.HSIdentical.df)

      BV.HSIdentical.df.gmean.female = aggregate(BV.HSIdentical.df[,c("feature")], by=list(FEMALE = BV.HSIdentical.df$FEMALE),mean,na.rm=T); colnames(BV.HSIdentical.df.gmean.female) = c("FEMALE","g_mean"); BV.HSIdentical.df.gmean.female=data.frame(BV.HSIdentical.df.gmean.female)
      BV.HSIdentical.df.gmean.male = aggregate(BV.HSIdentical.df[,c("feature")], by=list(MALE = BV.HSIdentical.df$MALE),mean,na.rm=T); colnames(BV.HSIdentical.df.gmean.male) = c("FEMALE","g_mean"); BV.HSIdentical.df.gmean.male=data.frame(BV.HSIdentical.df.gmean.male)
      BV.HSIdentical.df.gmean=rbind(BV.HSIdentical.df.gmean.female,BV.HSIdentical.df.gmean.male)

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
      #BV.ped<-BV.HSIdentical.df[,c("MALE","FEMALE","LINE")]
      #index <- which(duplicated(BV.ped$LINE))
      #BV.ped <- BV.ped[-index,]
      #ainv<-ainverse(pedigree=BV.ped)
      rm(df3,df3.gmean,df5,ear_height,gs_early,gs_late,pct_hoh,plot_wt,plt_height,qual.count,QUALDAT.SUM,rl_early_count,rl_percent,
         sl_cound,sl_percent,stand_cnt_early,stant_cnt_final,test_wt,Y_m,yield,BV,BV,
         BV.EC,BV.EC.filter,BV.EC.filter.check,BV.filter.Var,BV.MC,BV.MC.filter.check,BV.MC.filter,BV.ped,BV.Var,z,
         BV.check,index, Unique.Pedigree, Unique.Female.Pedigree, Unique.Male.Pedigree)
      invisible(gc(reset=T)) #cleans memory "garbage collector"
      cat(paste0("--------------------------------------",name,"--------------------------------------"), "\n")
      #asreml.options(dense = ~ vm(ainv))
      #BV.HSIdentical.df.data = BV.HSIdentical.df %>% dplyr::filter(feature>0)
      if(doYear){
        cat("I", "\n")
        #memory.size(max=64071)
        #asreml.options(pworkspace="31gb",workspace="31gb"	)

        df7 = asremlBV(name, BV.HSIdentical.df,counts=counts,df3.gmean=df3.gmean)


      }
      if(doField){
        BV.HSIdentical.model = asreml(fixed = feature ~ FIELD,
                                      random = ~ FEMALE + and(MALE,1),
                                      residual = ~units,
                                      data = BV.HSIdentical.df,
                                      equate.levels=c("FEMALE","MALE"),
                                      workspace="31gb",  #,
                                      na.action=na.method(y=c("include"),x=c("include"))
        )  #run the model

        plot(BV.HSIdentical.model)
        print(name)
        QUALDAT.SUM <- print(summary(BV.HSIdentical.model))
        invisible(gc())

        #BV.HSIdentical.model.predicted = BV.HSIdentical.model$coefficients$random

        BV.HSIdentical.model.predicted<- predict(BV.HSIdentical.model,
                                                 classify="MALE:FEMALE",
                                                 #pworkspace="31gb",
                                                 parallel=T,
                                                 aliased = T
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

      }

      if(doLmer){
        df7 = lmerBV(name=name,
                     BV.HSIdentical.df=BV.HSIdentical.df,
                     counts.adjusted=counts.adjusted,

                     counts.adjusted.raw=counts.adjusted.raw)
        df7 = data.frame(df7)
        #df8 = df7[!duplicated(df7$FEMALE),]
        invisible(gc())

        #BV.HSIdentical.model.predicted = BV.HSIdentical.model$coefficients$random

        }


      if(doDNN){


        datasets=xgblinearBV(
          hdp = "C:/Users/jake.lamkey/Documents/",
          fdp= "C:/Users/jake.lamkey/Documents/",
          s0=T,
          s1 =T,
          s2 =F,
          s3 =F,
          s4 =F,
          s5 =F,
          seas0 = 21,
          seas1 = 20,
          seas2 = "",
          seas3 = "",
          seas4 = "",
          seas5 = "",

          inbred = "BRS312",
          rounds = 3000,
          eta = 1,
          lambda = 0.0003,
          alpha = 0.0003

        )



        BV.HSIdentical.model.predicted = model %>% predict(BV.HSIdentical.model.feature)

      }




      ######CREATE A HERITABILITY

       sigfigs.traits<-c( paste0(name,"_Breeding.Value"),  paste0(name,"_GCA"),
                         paste0(name,"_standard.error.Breeding.Value"),
                         paste0(name,"_accuracy.Breeding.Value"),
                         paste0("PctPlotObsCollected_",name))
      #for(i in round.traits){df7[,i]<-round(df7[,i] ,4)}


      for(r in sigfigs.traits){
        for(i in 1:nrow(df7)){
          if(nchar(signif(df7[i,r] ,3)) > 6 ){
            df7[i,r]<-round(df7[i,r] ,4)}else
              df7[i,r]<-signif(df7[i,r] ,3)
        }
      }
      #write.csv(df7, file=paste0(wdp,name,"_BV_",year,"S.csv")) #write BLUEs to spreadsheet
      assign(paste0(name,"_BV_",folder,"S"), df7)
      #df5=data.frame(df5)
      cat(pastecs::stat.desc(df7[,paste0(name,"_Breeding.Value")]),"\n")
      #hist(as.numeric(df5[4]), col="gold", main=paste0(name," Histogram"), xlab=paste0(name))
      # hist(df5$EarHt_BV, col="blue", main=paste0("GCA Values For ",name), xlab="GCA Values")
      #  hist(df5$fitted_EarHt,main=paste0("BV Values For ", name), col="brown",xlab=paste0("Breeding Value (BV)"))
      ## Compare BLUEs to line averages on a scatterplot
      #  plot(df5$EarHt_BV,df5$g_mean, main=paste0("GCA Values By ", name," Plot"),ylab=paste0(name),xlab="GCA Values")
      #xy<-qqmath(ranef(BV.HSIdentical))
      #print(xy)
      rm(df5,df7,df6,df3.gmean,df3,std.errors, accuracy, se.BV, BV, pred.female, pred, mu.female,mu.idx.female,
         mu, BV.HSIdentical.blues.export,qualdat.blups, BV.HSIdentical.model.predicted,BV.HSIdentical.model,counts.adjusted.raw,
         BV.HSIdentical.blues, BV.HSIdentical.blues.exp,BV.HSIdentical.blues.female, BV.HSIdentical.blues.field,BV.HSIdentical.df.counts,
         BV.HSIdentical.gmean,BV.HSIdentical.df.gmean.female,BV.HSIdentical.df.gmean.male,BV.HSIdentical.df.gmean,counts.adjusted,counts,
         df3.gmean.female,df3.gmean.male,counts.adjusted.index,counts.adjusted.raw.index, BV.HSIdentical.fitted)
      invisible(gc())

    }
    #cat("L", "\n")

    sink()
    dev.off()
    # stopCluster(cl)
    ######################################################################################################
    ######Submit report
    ######################################################################################################
    #setwd("P:/Breeding Values")
    #yld = read.csv(paste0(wdp,"Yield_BV_",year,"S.csv"))
    #pw = read.csv("plot_wt_BV_20S.csv")
    #phho = read.csv(paste0(wdp,"PCT.HOH_BV_",year,"S.csv"))
    #y_m = read.csv(paste0(wdp,"Y.M_BV_",year,"S.csv"))
    #ph = read.csv(paste0(wdp,"Plt.Height_BV_",year,"S.csv"))
    #eh = read.csv(paste0(wdp,"EarHt_BV_",year,"S.csv"))
    #tstwt = read.csv(paste0(wdp,"Test.WT_BV_",year,"S.csv"))
    #rlec = read.csv("rl_early_count_BV_20S.csv")
    #rlp = read.csv(paste0(wdp,"RL.._BV_",year,"S.csv"))
    #rlc = read.csv(paste0(wdp,"RL.Count_BV_",year,"S.csv"))
    #slp = read.csv(paste0(wdp,"SL.._BV_",year,"S.csv"))
    #slc = read.csv(paste0(wdp,"SL.Count_BV_",year,"S.csv"))
    #gse = read.csv("gs_early_BV_20S.csv"))
    #gsl = read.csv(paste0(wdp,"GS.Late_BV_",year,"S.csv"))
    #scf = read.csv(paste0(wdp,"StandCnt..Final._BV_",year,"S.csv"))
    #scuav = read.csv(paste0(wdp,"StandCnt..UAV._BV_",year,"S.csv"))
    #sink("Debugger.txt")

    BV.traits.1 <- dplyr::left_join(eval(as.name(paste0("Yield_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)], eval(as.name(paste0("PCT.HOH_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)], by=c("FEMALE"));dim(BV.traits.1)
    BV.traits.2 <- dplyr::left_join(BV.traits.1,eval(as.name(paste0("Y.M_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.2)
    BV.traits.3 <- dplyr::left_join(BV.traits.2,eval(as.name(paste0("Plt.Height_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.3)
    BV.traits.4 <- dplyr::left_join(BV.traits.3,eval(as.name(paste0("EarHt_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.4)
    BV.traits.5 <- dplyr::left_join(BV.traits.4,eval(as.name(paste0("Test.WT_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.5)
    #BV.traits.6 <- left_join(BV.traits.5,rlec[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.6)
    BV.traits.6 <- dplyr::left_join(BV.traits.5,eval(as.name(paste0("RL.._BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.5)
    BV.traits.7 <- dplyr::left_join(BV.traits.6,eval(as.name(paste0("RL.Count_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.6)
    BV.traits.8 <- dplyr::left_join(BV.traits.7,eval(as.name(paste0("SL.._BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.7)
    BV.traits.9 <- dplyr::left_join(BV.traits.8,eval(as.name(paste0("SL.Count_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.8)
    #BV.traits.11 <- left_join(BV.traits.10,gse[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.11)
    BV.traits <- dplyr::left_join(BV.traits.9,eval(as.name(paste0("GS.Late_BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits)
    #BV.traits.11 <- left_join(BV.traits.10,eval(as.name(paste0("StandCnt..Final._BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));dim(BV.traits.10)
    #BV.traits <- left_join(BV.traits.10,eval(as.name(paste0("StandCnt..UAV._BV_",folder,"S")))[,c(1,3,2,4,5,6,7)],by=c("FEMALE"));colnames(BV.traits)[1]<-"INBRED";dim(BV.traits)
    #BV.traits.dup<-BV.traits[duplicated(BV.traits$INBRED),];dim(BV.traits.dup)
    names(BV.traits) <- gsub("\\.", "", names(BV.traits))

    BV.traits$SL_BreedingValue = ifelse(BV.traits$SL_BreedingValue <0, 0 , BV.traits$SL_BreedingValue)
    BV.traits$RL_BreedingValue = ifelse(BV.traits$RL_BreedingValue <0, 0 , BV.traits$RL_BreedingValue)
    BV.traits$SLCount_BreedingValue = ifelse(BV.traits$SLCount_BreedingValue <0, 0 , BV.traits$SLCount_BreedingValue)
    BV.traits$RLCount_BreedingValue = ifelse(BV.traits$RLCount_BreedingValue <0, 0 , BV.traits$RLCount_BreedingValue)
    BV.traits$GSLate_BreedingValue = ifelse(BV.traits$GSLate_BreedingValue <0, 0 , BV.traits$GSLate_BreedingValue)

    BV.traits$SLCount_BreedingValue = round(as.numeric(BV.traits$SLCount_BreedingValue), digits=0)
    BV.traits$RLCount_BreedingValue = round(as.numeric(BV.traits$RLCount_BreedingValue),digits=0)
    BV.traits$GSLate_BreedingValue = round(as.numeric(BV.traits$GSLate_BreedingValue),digits=0)

    # wb<-createWorkbook(type="xlsx")
    # CellStyle(wb, dataFormat=NULL, alignment=NULL,
    #           border=NULL, fill=NULL, font=NULL)
    # Lines <- createSheet(wb, sheetName = "Lines")
    # addDataFrame(data.frame(BV.traits),Lines, startRow=1, startColumn=1,row.names=F)

    #linked.peds.updated = read.xlsx( paste0(fdp,"linked.peds.updated.xlsx"), 2)
    #BV.traits = left_join(BV.traits, linked.peds.updated, by = c("FEMALE"="match"))
    BV.traits$INBRED = as.character(BV.traits$FEMALE)
    #BV.traits$pedigree = as.character(BV.traits$pedigree)


    ped_info = openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data", "/linked.peds.updated.xlsx"),1)
    industryNames = InbredNameLibrary()
    industryNames = industryNames[[2]]

    ped_info[,"match"] <- suppressWarnings(suppressMessages(plyr::revalue(as.character(ped_info[,"match"]), industryNames)))  #industry name to inbred name conversion

    group_and_concat <- ped_info %>%
      dplyr::select(uniqued_id, match, Gender) %>%
      dplyr::group_by(match) %>%
      dplyr::mutate(Prism_Mped_Fped_HId_Ped_IName_Var = paste(uniqued_id, collapse = " , "),
             HG = paste(Gender, collapse = " , "))

    group_and_concat$HG = gsub(group_and_concat$HG, pattern="/", replacement = " , ")
    group_and_concat$HetGrp <- sapply(group_and_concat$HG, function(x) paste(unique(unlist(stringr::str_split(x," , "))),
                                                                       collapse = " , "))
    group_and_concat$HetGrp = gsub(group_and_concat$HetGrp, pattern="FEMALE , Male", replacement = "Female/Male")
    group_and_concat$HetGrp = gsub(group_and_concat$HetGrp, pattern="Male , FEMALE", replacement = "Female/Male")

    group_and_concat = group_and_concat[!duplicated(group_and_concat$match),]

    BV.traits = dplyr::left_join(BV.traits, group_and_concat[,c(2,4,6)], by=c("FEMALE"="match"))
    #BV.traits$FEMALE = ifelse(is.na(BV.traits$pedigree) ,
    #                          BV.traits$FEMALE, BV.traits$pedigree)

    #BV.traits = BV.traits[,-c(68)]

    openxlsx::write.xlsx(BV.traits, paste0(fdp,"/",folder,
                                 "/",".Traits.BV.",
                                 folder,
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
                                 ,".xlsx"), overwrite=T)



    cat("----------------------------Finished Breeding Values!----------------------------", "\n")
    #cat("-------------------------Time was-",proc.time() - ptm , "----------------------------\n")

    #sink()
    #cat("No bugs for Breeding Values")
  }


  if(doHybridID){
    cat("----------------------------Loading Hybrid Data----------------------------", "\n")

    #source("R:/Breeding/MT_TP/Models/R-Scripts/Breeding Values.HybridID.GCA.2020.R")
    HybridID(#fdph = fdph,
      fdp = fdp,
      #wdp=wdp,
      BV.HSIdentical.df = BV.HSIdentical.df,
      folder = folder,
      doYear = doYear,
      A = A,
      B = B,
      C = C,
      Prop = Prop,
      Choice = Choice,
      D=D,
      R=R,
      X=X,
      E=E,
      Q=Q,
      V=V,
      ptm=ptm,
      doField=doField,
      doLmer = doLmer,
      GEM=GEM
      )

  }

}



