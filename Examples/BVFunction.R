
library(BreedStats)
# library(tidyverse)
# library(data.table)
# library(asreml)

#cat("Date? ")
#x <- readLines(file("stdin"),1)
#print(x)
#sink("Debugger.txt")
args<-commandArgs(trailingOnly=T)
cat(args)
var1<-as.character(args[1])
varA<-as.character(args[2])
varB<-as.character(args[3])
varC<-as.character(args[4])
varProp<-as.character(args[5])
varChoice<-as.character(args[6])
#varYear<-as.character(args[7])
varFolder = as.character(args[7])
# s1<-as.character(args[9])
# s2<-as.character(args[10])
# s3<-as.character(args[11])
# s4<-as.character(args[12])
# s5<-as.character(args[13])
seas1<-as.character(args[8])
seas2<-as.character(args[9])
seas3<-as.character(args[10])
seas4<-as.character(args[11])
seas5<-as.character(args[12])
seas0 = as.character(args[23])

if(seas0!="NA"){s0=T}else(s0=F)
if(seas1!="NA"){s1=T}else(s1=F)
if(seas2!="NA"){s2=T}else(s2=F)
if(seas3!="NA"){s3=T}else(s3=F)
if(seas4!="NA"){s4=T}else(s4=F)
if(seas5!="NA"){s5=T}else(s5=F)

if(seas0!="NA"){seas0=seas0}else(seas0="")
if(seas1!="NA"){seas1=seas1}else(seas1="")
if(seas2!="NA"){seas2=seas2}else(seas2="")
if(seas3!="NA"){seas3=seas3}else(seas3="")
if(seas4!="NA"){seas4=seas4}else(seas4="")
if(seas5!="NA"){seas5=seas5}else(seas5="")

doYear<-as.character(args[13])
varD = as.character(args[14])
varR = as.character(args[15])
varX = as.character(args[16])
varE = as.character(args[17])
varQ = as.character(args[18])
varV = as.character(args[19])
varGEM = as.character(args[20])
#fdp = as.character(args[27])
#fdph = as.character(args[28])
directory= as.character(args[21])

fdp = "R:/Breeding/MT_TP/Models/Breeding Values"

#varYear = as.numeric(varYear) - 2000
folder=as.character(varFolder)
wdp=as.character(directory)
x = var1

ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, '.csv')#
#ws = paste0('C:/Users/jake.lamkey/Desktop/YT_BV Yield Trial Master Catalog ',x, '.csv')

#
# if(!dir.exists(paste0(directory,"/", folder))){
#   dir.create(paste0(directory,"/", folder))
# }
#
# if(!dir.exists(paste0(directory,"/", folder, "/Hybrid"))){
#   dir.create(paste0(directory,"/", folder, "/Hybrid"))
# }

#directory="C:/Users/jake.lamkey/Desktop"
#folder="Test"

#fdpt = paste0(wdp,"/",folder)
cat(args,"\n")
#fdp=as.character(fdp)
#########################################################
#########################################################
#########################################################
# fdp = paste0("C:/Users/jake.lamkey/Documents")
# year= "hybrid"
# if(!dir.exists(paste0(fdp,"/",year))){
#   dir.create(paste0(fdp,"/",year))
# }
#
# x="11_8_2021"
# #ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
# ws=paste0("C:/Users/jake.lamkey/Documents/YT_BV Yield Trial Master Catalog ",x, ".csv")
#
# #date="12_22_2021"
# #ws = paste0("P:/Temp/PedAdjust ",date,".csv")
# wdp = paste0("C:/Users/jake.lamkey/Documents")
# varA ="True"
# varB ="False"
# varC ="False"
# varProp ="True"
# varChoice ="False"
# varD ="False"
# varR ="False"
# varX ="False"
# varE ="False"
# varQ ="False"
# varV ="False"
# varGEM ="False"
# s0=T
# s1 =T
# s2 =T
# s3 =F
# s4 =F
# s5 =F
#
# seas0 = 21
# seas1 = 20
# seas2 = 19
# seas3 = ""
# seas4 = ""
# seas5 = ""
#
# doYear="True"
# folder=year
#
# season0=as.numeric(seas0)
# season1=as.numeric(seas1)
# season2=as.numeric(seas2)
# season3=as.numeric(seas3)
# season4=as.numeric(seas4)
# season5=as.numeric(seas5)
# folder=folder
# fdp=as.character(fdp)
#########################################################
#########################################################
#########################################################
#vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'
#folder= "21S"
A = if(varA== "True"){varA=T}else(varA=F)#varA,
B = if(varB== "True"){varB=T}else(varB=F)#varB,
C = if(varC==T){varC=T}else(varC=F)#varC,
Prop =if(varProp== "True"){varProp=T}else(varProp=F)# varProp,
Choice =if(varChoice== "True"){varChoice=T}else(varChoice=F)# varChoice,
D=if(varD== "True"){varD=T}else(varD=F)#varD,
R=if(varR== "True"){varR=T}else(varR=F)#varR,
X=if(varX== "True"){varX=T}else(varX=F)#varX,
E=if(varE== "True"){varE=T}else(varE=F)#varE,
Q=if(varQ== "True"){varQ=T}else(varQ=F)#varQ,
V=if(varV== "True"){varV=T}else(varV=F)#varV,
GEM=if(varGEM=="True"){varGEM=T}else(varGEM=F)#VarGEM,

cat("var1=",var1,"varA=",varA,"varB=",varB,"varC=",varC,"varProp=",varProp,"varChoice=",varChoice,"varFolder=",varFolder,
    "seas1=",seas1,"seas2=",seas2,"seas3=",seas3,"seas4=",seas4,"seas5=",seas5,"seas0=",seas0,"s0=",s0,"s1=",s1,"s2=",s2,"s3=",
    s3,"s4=",s4,"s5=",s5,"\n")

cat(A, B, C, Prop, Choice, D, R, X, E, Q, V, GEM,"\n")
#sink()

suppressWarnings(suppressMessages(BV(fdp = fdp, #paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder),
                                     #fdph = fdph,#paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder,"/Hybrid"),
                                     wdp = wdp, #paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/"),
                                     ws = ws,
                                     #vdp = vdp,
                                     doHybridID=T,
                                     doPedigreeChange =F,
                                     doPedigreeToBecksChange=F,
                                     doGCABV = T,
                                     doWriteFinalPedigrees =F,
                                     InventoryPedigree = F,
                                     date="12_22_2021",
                                     ytData=T,
                                     doReduceNonCodes=T,
                                     simulate=F,
                                     year= folder,
                                     folder=folder,
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
                                     GEM=GEM,
                                     doDNN = F,
                                     s0=s0,
                                     s1=s1,
                                     s2=s2,
                                     s3=s3,
                                     s4=s4,
                                     s5=s5,
                                     seas0=seas0,
                                     seas1=seas1,
                                     seas2=seas2,
                                     seas3=seas3,
                                     seas4=seas4,
                                     seas5=seas5,
                                     doYear=if(doYear == "True"){doYear=T}else(doYear=F) #doYear,


)))

#sink()
