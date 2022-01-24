######################################################################################################
######Rscript for Spatial Analysis
######################################################################################################
rm(list=ls()) #remove environment vairalbes
invisible(gc(reset=T)) #cleans memory "garbage collector"
memory.limit(size=18071)
#setwd("R:/Breeding/MT_TP/Models/Breeding Values")

######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################

#source("R:/Breeding/MT_TP/Models/R-Scripts/Spatial Analysis_ASReml.R")
library(BreedStats)
#library(tidyverse)
#library(data.table)
library(asreml)
library(DiGGer)

spaDF = load_Data(x = "11_8_2021",simulate=F )





AL.traits = spaEBN(year = "21",
                   fdp = "C:/Users/jake.lamkey/Documents/",#"R:/Breeding/MT_TP/Models/AL_Adjustments/",
                   spaDF = spaDF,
                   namesSeq =   c(3:5) #ear, plt, tw, ym, yield,####not included yet gs, moist, rl%, rl count, sl%, sl count,
                  )





#fit the separable autoregressive error model
#m1 = model1()

# an extension to this model includes a measurement error or nugget effect term
#m2 = model2()

#incomplete block analysis (with recovery of inter-block information)









