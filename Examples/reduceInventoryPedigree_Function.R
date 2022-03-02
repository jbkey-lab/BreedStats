
library(BreedStats)
 library(tidyverse)
library(data.table)

#########################################################
fdp = paste0("C:/Users/jake.lamkey/Documents") #FINAL DATA PATH TO WRITE PEDIGREES
wdp = paste0("C:/Users/jake.lamkey/Documents") #WORKING DATA PATH TO CHANGE PEDIGREES

#x="11_8_2021"
#ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
date="3_2_2022"
ws = paste0("P:/Temp/PedAdjust ",date,".csv")

#YEARS IN INVENTORY CATALOG
seas0 = "21"
seas1 = ""
seas2 = ""
seas3 = ""
seas4 = ""
seas5 = ""

#########################################################
suppressWarnings(suppressMessages(BV(fdp = fdp, #paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder),
                                     wdp = wdp,#paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/"),
                                     ws = ws, date=date,  #LOAD DATA
                                     doPedigreeChange =T, doReduceNonCodes=T, #PEDIGREE SETTINGS
                                     InventoryPedigree = T, ytData=F, simulate=F, #TYPE OF DATA LOADING
                                     doHybridID=F, doGCABV = F,doYear=T, #TYPE OF ANALYSIS
                                     doPedigreeToBecksChange=F,doWriteFinalPedigrees =F,
                                     year= "21S", folder="21S",
                                     A = T,B = F,C = F,Prop = F,Choice = F,D=F,R=F,X=F,E=F,Q=F,V=F,GEM=F, #TESTING LEVEL
                                     doDNN = F,
                                     s0=if(seas0 == ""){s0=F}else(s0=T),
                                     s1=if(seas1 == ""){s1=F}else(s1=T),
                                     s2=if(seas2 == ""){s2=F}else(s2=T),
                                     s3=if(seas3 == ""){s3=F}else(s3=T),
                                     s4=if(seas4 == ""){s4=F}else(s4=T),
                                     s5=if(seas5 == ""){s5=F}else(s5=T),
                                     seas0=seas0,seas1=seas1, seas2=seas2, seas3=seas3,  seas4=seas4, seas5=seas5
                                     #if(doYear == "True"){doYear=T}else(doYear=F) #doYear,
)))

#sink()
