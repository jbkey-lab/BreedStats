G2P=function(X,h2,NQTN,distribution, me,s,sz,p,lm,name){
  n=nrow(X)
  m=ncol(X)
  #Sampling QTN
  QTN.position=sample(NQTN,NQTN*.01,replace=F)
  SNPQ=as.matrix(X)
  QTN.position
  #QTN effects
  if(distribution=="rnorm"){
    addeffect= abs(rnorm(n=NQTN, mean=me, sd=s))
    effectvar=var(addeffect)
    residualvar=(effectvar-h2*effectvar)/h2
    residual=rnorm(NQTN,0,sqrt(residualvar))
    y=addeffect+residual
  }

  if(distribution=="rbinom"){
    addeffect = abs(rbinom(n=NQTN, size=sz, prob=p))
    effectvar=var(addeffect)
    residualvar=(effectvar-h2*effectvar)/h2
    residual=abs(rnorm(NQTN,0,sqrt(residualvar)))
    y=addeffect+residual
    y=round(y,0)

  }

  if(distribution=="rpois"){
    addeffect = abs(rpois(n=NQTN,lambda=lm))
    effectvar=var(addeffect)
    residualvar=(effectvar-h2*effectvar)/h2
    residual=abs(rnorm(NQTN,0,sqrt(residualvar)))
    y=addeffect+residual
    y=round(y,0)
  }

  #Simulate phenotype
  rm(X,residual,residualvar,addeffect,effectvar,QTN.position,SNPQ,n,m,NQTN)
  gc()

  return(list( y=y))
}



sim_yielddata = function(BV.MC.Entry=BV.MC.Entry,seed=seed ){


  seed=seed

  #set.seed(seed)

  #BV.MC.Entry.Choice=BV.MC.Entry %>% filter( `Book Season` == "21S: Corn")

  # BV.MC.Entry.index = sample(nrow(BV.MC.Entry), nrow(BV.MC.Entry) * 0.5)
  # BV.MC.Entry.sample = BV.MC.Entry[BV.MC.Entry.index,]
  BV.MC.Entry.sample=data.frame(BV.MC.Entry)


  names<-names(BV.MC.Entry.sample); names

  BV.MC.Entry.sampleEBN.index = BV.MC.Entry.sample[!duplicated(BV.MC.Entry.sample$Pedigree),"Pedigree"]

  traitList = c("Yield",	"Plt Height",	"Y.M",	"Silk.Color",
                "EarHt",	"EarHt..Rating.",	"StandCnt..UAV.",	"Tassel.Branch..",	"NCLB",
                "GS.Late",	"Plt.Height..Rating.",	"SL.Rating..Early.",	"Tassel.Extension",	"NCLB.2",
                "PCT.HOH",	"Glume.Color",	"TillerCnt",
                "Plt.Height",	"Glume.Ring",	"Ears.Per.Stalk",
                "RL..",	"Leaf.Angle..Inb.Obs.",	"Anther.Color",
                "RL.Count",	"Leaf.Color",	"Pollen.Duration..GDUs.10..to.90..",	"Brace.Root.Color",
                "Test.WT",	"Leaf.Texture",	"SL.Rating",
                "SL..",	"RL.Rating","SL.Count",	"Pollen.Shed.Rating",
                "Cob.Color", "StandCnt..Final.", "GOSS","GLS",
                "Silk.GDU.s.to.10." ,"Silk.GDU.s.to.50.","Silk.GDU.s.to.90.",
                "Pollen.GDU.s.to.10.", "Pollen.GDU.s.to.50.", "Pollen.GDU.s.to.90.")


  for(name in names){
    # bind.linked.male.peds=foreach(i=(1:length(BV.MC.Entry.sampleEBN.index)),
    #                               .combine = 'cbind',
    #                               .inorder=F
    #
    #                               #.packages=c(),
    #                              # .export=c()
    # ) %dopar% {

    if( name %in% (traitList) ){
      cat("Simulating ", name,"... \n")

      for(i in 1:length(BV.MC.Entry.sampleEBN.index)){

        EBNt = BV.MC.Entry.sampleEBN.index[[i]]

        subsetSA = subset(
          BV.MC.Entry.sample[,c("Pedigree",paste0(name),"Entry.Book.Name")],
          Pedigree==EBNt)

        subsetSA$Pedigree = as.character(subsetSA$Pedigree)
        BV.MC.Entry.sample$Pedigree = as.character(BV.MC.Entry.sample$Pedigree)

        if(name == "Yield"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.23, distribution = "rnorm", me=100:300, s=20)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "EarHt"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=20:70, s=.28)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "GS.Late"){
          subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=1:60, p=.02)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "PCT.HOH"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.27, distribution = "rnorm", me=15:30, s=.5)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "Plt.Height"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.23, distribution = "rnorm", me=80:120, s=15)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "RL.."){
          subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.11, distribution = "rbinom", sz=1:50, p=.04)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "RL.Count"){
          subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.10, distribution = "rbinom", sz=1:70, p=.05)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "Test.WT"){
          subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.40, distribution = "rnorm", me=55:62, s=10)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "SL.."){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.44, distribution = "rbinom", sz=1:50, p=.11)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "SL.Count"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.34, distribution = "rbinom", sz=1:50, p=.1)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "Y.M"){
          subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rpois",lm=9:15)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name == "StandCnt..UAV."){
          subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.37, distribution = "rnorm", me=60:70, s=5)$y
          ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
          BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }

        if(name=="SL.Rating..Early."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.2, distribution = "rnorm", me=50, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Silk.GDU.s.to.10."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1250, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Silk.GDU.s.to.50."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1350, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Silk.GDU.s.to.90."){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1450, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Pollen.GDU.s.to.10."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1250, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Pollen.GDU.s.to.50."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1350, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Pollen.GDU.s.to.90."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=1450, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Cob.Color"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "EarHt"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=45, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Plt.Height"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.4, distribution = "rnorm", me=75, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "EarHt..Rating."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=35, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Plt.Height..Rating."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=75, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Glume.Color"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Glume.Ring"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Leaf.Angle..Inb.Obs."){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=25, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Leaf.Color"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Leaf.Texture"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Pollen.Duration..GDUs.10..to.90.."){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.61, distribution = "rnorm", me=65, s=5)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Pollen.Shed.Rating"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Silk.Color"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Tassel.Branch.."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=5, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Tassel.Extension"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=5, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "TillerCnt"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=0, p=.1)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Ears.Per.Stalk"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=1, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Anther.Color"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "Brace.Root.Color"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "SL.Rating"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=50, p=.1)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "RL.Rating"){subsetSA[,name] =G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=50, p=.1)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "StandCnt..Final."){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=75, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "NCLB"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "NCLB.2"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "GOSS"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=3, p=.9)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }
        if(name==     "GLS"){subsetSA[,name] = G2P(X=subsetSA,NQTN=nrow(subsetSA), h2=.48, distribution = "rbinom", sz=10, p=.1)$y
        ID = which(BV.MC.Entry.sample$Pedigree == EBNt)
        BV.MC.Entry.sample[ID, name] <- subsetSA[ name]
        }


        rm(ID)
        invisible(gc())
        #hist(BV.MC.Entry.sample$Y.M)
      }

    }
  }

  #stopCluster(cl)
  cat("Done simulating!")
  return(data.frame(BV.MC.Entry.sample))

}

# BV.MC.Entry.Choice=BV.MC.Entry %>% filter(grepl(x=`Entry Book Name`, pattern="TP"))
#
 #BV.MC.Entry = sim_yielddata(BV.MC.Entry = BV.MC.Entry, seed=8042)
#
# #  BV.MC.NUR = sim_yielddata(BV.MC.Entry = BV.MC.NUR, seed=8042)
# #  BV.MC.GOSS = sim_yielddata(BV.MC.Entry = BV.MC.GOSS, seed=8042)
# # #
# #  save(BV.MC.GOSS,file="inbredgossdata.rda")
# #  save(BV.MC.NUR,file="inbreddata.rda")
#
# save(BV.MC.Entry,file="bvdata.rda")








