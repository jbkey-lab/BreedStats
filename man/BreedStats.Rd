\name{BreedStats}
\alias{BreedStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Genetic values and predictions for breeding
%%  ~~Breeding Statistics for R ... ~~
}
\description{
A group of functions for breeding statistics. It uses ASReml algorithms for spatial adjustments and breeding value types of analysis. Simulated (simulate=T) data is avaiable to utilize the functions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
 BV(fdp,wdp, ws, doHybridID, doPedigreeChange, InventoryPedigree, doReduceNonCodes, doField, doLmer, ytData, doGCABV, year, A, B, C, Prop, Choice, D, R, X, E, Q, V, doDNN, GEM, date, s0, s1, s2, s3, s4, s5, seas0, seas1, seas2, seas3, seas4, seas5, doYear, folder, simulate)

 inbredChar(dpf, gdp, hdp, year, l_year, h_year, doAdjInbredData, simulate, prepImages)

 sim_yielddata(BV.MC.Entry,seed)

 oneRepIBDSpatial(numOfReps = rep, numOfEntry = plot, ebn = EBN, numOfChecks=Checks)

 spaEBN(year="21",fdp="R:/Breeding/MT_TP/Models/AL_Adjustments/",spaDF=load_Data(x,simulate),namesSeq)

 xgblinearBV(hdp, fdp, s0, s1, s2, s3, s4, s5, seas0, seas1, seas2, seas3, seas4, seas5, inbred,
 rounds, eta, lambda, alpha, male )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doLmer}{
  uses lme4 to run breeding values analysis (0.9999 r2 to ASReml)
  }
  \item{doField}{
  Switches from year to field effect
  }
  \item{doHybridID}{
  Runs BV analysis with LINE as random
  }
  \item{doGCABV}{
  Runs BV analysis with MALE and FEMALE as random
  }
  \item{x}{
  date in "11_8_2021" format, export out of prism must be used with a spatial analysis design setup from function oneRepIBDSpatial()
  }
  \item{simulate}{
  T or F, when T sample data will be used
  }
  \item{fdp}{
  final data path of output file
  }
  \item{wdp}{
  working data path for breeding value pedigrees
  }
  \item{ws}{
  working sheet for yield trial master catalog output from PRISM
  }
  \item{dpf}{
  path file for inbred data_ (5 output files)
  }
  \item{gdp}{
  goss's dath path for inbred data exported from PRISM yield trial master catalog
  }
  \item{hdp}{
  inbred data path for inbred data exported from PRISM nursery master catalog
  }
  \item{folder}{
  year or folder to put files into fdp
  }
  \item{numOfReps}{
  list of numbers of reps for entry list book
  }
  \item{numOfEntry}{
  list of number of entries per entry list book
  }
  \item{ebn}{
  list of names for entry books
  }
  \item{numOfChecks}{
  list of checks per entry list book
  }
  \item{inbred}{
  list of male inbreds to filter with
  }
  \item{male}{
  list of male inbreds to include with predicitons file
  }


%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 https://www.github.com/jbkey730/BreedStats

%% ~put references to the literature/web site here ~
}
\author{Jacob Lamkey
%%  ~~Jacob Lamkey~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#########################
#SIMULATED DATA DEMO
#########################
######################################################################################################
######Rscript for Alpha Lattice Designs
######################################################################################################
invisible(gc(reset=T)) #cleans memory "garbage collector"
memory.limit(size=16071)
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
library(BreedStats)
library(tidyverse)
library(data.table)
library(asreml)

cl=parallel::detectCores()
cl <- makePSOCKcluster(cl)
registerDoParallel(cl)

#dimField = 48*50 #2400 plots
#num of plots in Entry List
fdp = "C:/Users/jake.lamkey/Documents/"

plotListOneRep = c(36,36,36,48, 48,36,36,36)
RepListOneRep = c(8,6,4,9, 10,8,10,4)
ebn = c("TP21B681","TP21B682","TP21B683","TP21B721", "TP21B722","TP21B723","TP21B725","TP21B727")
numOfChecks = c(4,4,4,4, 4,4,5,4)

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



###############################Examine Data##################################

spaDF = load_Data(x = "11_8_2021",simulate=T ) #merge incomplete block analysis from onerepIBDspatial with data

AL.traits = spaEBN(year = "21",
                   fdp = "C:/Users/jake.lamkey/Documents/",#"R:/Breeding/MT_TP/Models/AL_Adjustments/",
                   spaDF = spaDF,
                   namesSeq =   c(3:5) #ear, plt, tw, ym, yield####not included yet gs, moist, rl
                  )


######################################################################################################
######Inbred Characterization Analysis
######################################################################################################
 var4=2021
 var2 = 2017
 var3=2021

suppressWarnings(suppressMessages(
inbredChar(
          dpf = "C:/Users/jake.lamkey/Documents/Inbred Data_", #"R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_",
          gdp = gdp,
          hdp = hdp,
          year= var4,
          l_year = var2,
          h_year = var3,
          doAdjInbredData = T,
          simulate=T
)))

fdp = paste0("C:/Users/jake.lamkey/Documents")
year= "Simulate"
if(!dir.exists(paste0(fdp,"/",year))){
  dir.create(paste0(fdp,"/",year))
}
######################################################################################################
######diallel breeding value analysis
######################################################################################################

x="11_8_2021"
ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
wdp = paste0("C:/Users/jake.lamkey/Documents")
varA ="True"
varB ="False"
varC ="False"
varProp ="True"
varChoice ="False"
varD ="False"
varR ="False"
varX ="False"
varE ="False"
varQ ="False"
varV ="False"
varGEM ="False"
s0=T
s1 =T
s2 =T
s3 =F
s4 =F
s5 =F

seas0 = 21
seas1 = 20
seas2 = 19
seas3 = ""
seas4 = ""
seas5 = ""

doYear="True"
folder=year

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
sink()

suppressWarnings(suppressMessages(BV(fdp = fdp, #paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder),
                                     #fdph = fdph,#paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder,"/Hybrid"),
                                     wdp = wdp,#paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/"),
                                     ws = ws,
                                     #vdp = vdp,
                                     doHybridID=T,
                                     doPedigreeChange =T,
                                     doPedigreeToBecksChange=F,
                                     doGCABV = T,
                                     doWriteFinalPedigrees =F,
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
                                     doYear=if(doYear == "True"){doYear=T}else(doYear=F), #doYear,
                                     simulate=T

)))

######################################################################################################
######Pedigree Adjustments from Inventory
######################################################################################################

fdp = paste0("C:/Users/jake.lamkey/Documents") #FINAL DATA PATH TO WRITE PEDIGREES
wdp = paste0("C:/Users/jake.lamkey/Documents") #WORKING DATA PATH TO CHANGE PEDIGREES

#x="11_8_2021"
#ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
date="12_22_2021"
ws = paste0("P:/Temp/PedAdjust ",date,".csv")

#YEARS IN INVENTORY CATALOG
seas0 = 21
seas1 = ""
seas2 = ""
seas3 = ""
seas4 = ""
seas5 = ""


suppressWarnings(suppressMessages(BV(fdp = fdp, #paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder),
                                     wdp = wdp,#paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/"),
                                     ws = ws, date=date,  #LOAD DATA

                                     doPedigreeChange =T, doReduceNonCodes=F, #PEDIGREE SETTINGS
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

########################################################################################################
######Predictions with xgblinear for a expand.grid type of dataframe including male, female, year, field
########################################################################################################
xgblinearBV(
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
  alpha = 0.0003,
  male =   data.frame(male=c('BSQ033',	'BEX905',
                    'BRQ529',	'BSR095',	'BRU059',
                    'BRQ291',	'BRP251',"BRS312"

  ))

)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
