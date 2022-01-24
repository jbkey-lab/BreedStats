######################################################################################################
######Rscript for Alpha Lattice Designs
######################################################################################################
rm(list=ls()) #remove environment vairalbes
invisible(gc(reset=T)) #cleans memory "garbage collector"
memory.limit(size=16071)
#setwd("R:/Breeding/MT_TP/Models/Breeding Values")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
#source("R:/Breeding/MT_TP/Models/R-Scripts/YT_Alpha Lattice.R")
library(BreedStats)
library(tidyverse)
library(data.table)

cl= parallel::detectCores()
cl <- parallel::makePSOCKcluster(cl)
doParallel::registerDoParallel(cl)

######################################################################################################

#dimField = 48*50 #2400 plots
#num of plots in Entry List
fdp = "C:/Users/jake.lamkey/Documents/"

plotListOneRep = c(36,36,36,48, 48,36,36,36, 72,60,36,60, 36,36,72) #,36,36,72,48,12,36,48,72)
RepListOneRep = c(8,6,4,9, 10,8,10,4, 4,4,4,4, 3,4,4) #,9,6,12,14,16,8,9,2)
ebn = c("TP21B681","TP21B682","TP21B683","TP21B721", "TP21B722","TP21B723","TP21B725","TP21B727",
        "TP21C543","TP21C601","TP21C609","TP21C632", "TP21C643","TP21C645","TP21C704")
numOfChecks = c(4,4,4,4, 4,4,5,4 ,3,3,3,3, 5,3,3)

#plotListTwoRep = c(36,48)
#repListTwoRep = c(10,11)
length(plotListOneRep)
length(RepListOneRep)
length(ebn)
length(numOfChecks)

data.files=as.factor(ebn)

bind.linked.male.peds=foreach(i=(1:length(plotListOneRep)),
                              .packages=c("dplyr","DiGGer","stats"),
                              .export=c("mutate","filter","setNames","group_by","summarise",
                                        "ibDiGGer","getDesign","corDiGGer","desPlot")
) %dopar% {

#for(i in 1:length(plotListOneRep)){

  EBN = ebn[[i]]

  pdf(file = paste0(fdp,"B_YTDesign_",i ,EBN,"OneReplicated.pdf"), paper="special", width = 8.5,
                          height = 11, family="Times", pointsize=11, bg="white", fg="black")

  plot = plotListOneRep[[i]]
  rep = RepListOneRep[[i]]
  Checks = numOfChecks[[i]]
  oneRepIBDSpatial(numOfReps = rep, numOfEntry = plot, ebn = EBN, numOfChecks=Checks)

  dev.off()

}

stopCluster(cl)







#for(i in 1:length(data.files)){
#  bind.linked.male.peds=foreach(i=1:(length(plotListOneRep))-1
                                #.packages=c("dplyr","DiGGer","stats"),
                                #.export=c("mutate","unnest","filter","setNames","group_by","summarise","separate",
                                #          "ibDiGGer","getDesign","corDiGGer","desPlot")
#  ) %dopar% {
#  dataFiles = data.files[[i]]
#  write.csv(get(dataFiles), paste0(fdp,EBNi ,"_AL.csv"), row.names = F)

#}



