library(BreedStats)
library(tidyverse)
library(data.table)

#cat("Date? ")
#x <- readLines(file("stdin"),1)
#print(x)

args<-commandArgs(trailingOnly=T)
var1<-as.character(args[1])

var2<-as.character(args[2])
var3<-as.character(args[3])
var4<-as.character(args[4])

cat(var1 ,"\n")
cat(var2 ,"\n")
cat(var3 ,"\n")
cat(var4 ,"\n")


#varlYear = as.numeric(var2)
#varhYear = as.numeric(var3)

x = var1


# x = "9_2_2021"

hdp = paste0('R:/Breeding/MT_TP/Models/Data/Shd_slk/Becks_OBS ',x, ".csv")
gdp = paste0('R:/Breeding/MT_TP/Models/Data/Shd_slk/Becks_OBS GOSS ',x, ".csv")

 # var4=2021
 # var2 = 2017
 # var3=2021
 #dpf = "C:/Users/jake.lamkey/Documents/Inbred Data_" #"R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_",
 dpf = "R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_"
 # year= var4
 # l_year = var2
 # h_year = var3

suppressWarnings(suppressMessages(
inbredChar(
          dpf = dpf, #"C:/Users/jake.lamkey/Documents/Inbred Data_", #"R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_",
          gdp = gdp,
          hdp = hdp,
          year= var4,
          l_year = var2,
          h_year = var3,
          doAdjInbredData = T,
          simulate=F
)))


















