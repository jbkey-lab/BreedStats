
genoReady = function(sdp, inbreds, linked.peds, trainingx2){
  `GAPIT.Numericalization` <-
    function(x,bit=2,effect="Add",impute="None", Create.indicator = FALSE, Major.allele.zero = FALSE, byRow=TRUE){
      #Object: To convert character SNP genotpe to numerical
      #Output: Coresponding numerical value
      #Authors: Feng Tian and Zhiwu Zhang
      # Last update: May 30, 2011
      ##############################################################################################

     # x=Genos[,3] %>% as.matrix()

      if(bit==1)  {
        x[x=="X"]="N"
        x[x=="-"]="N"
        x[x=="+"]="N"
        x[x=="/"]="N"
        x[x=="K"]="Z" #K (for GT genotype)is replaced by Z to ensure heterozygose has the largest value
      }

      if(bit==2)  {
        x[x=="XX"]="N"
        x[x=="--"]="N"
        x[x=="++"]="N"
        x[x=="//"]="N"
        x[x=="NN"]="N"
        x[x=="00"]="N"
        x[is.na(x)]="N"
        x[x==""]="N"


      }

      n=length(x)
      lev=levels(as.factor(x))
      lev=setdiff(lev,"N")
      #print(lev)
      len=length(lev)
      #print(len)
      #Jiabo creat this code to convert AT TT to 1 and 2. 2018.5.29
      if(bit==2)
      {
        inter_store=c("AT","AG","AC","TA","GA","CA","GT","TG","GC","CG","CT","TC")
        inter=intersect(lev,inter_store)

        if(length(inter)>1)
        {
          x[x==inter[2]]=inter[1]
          n=length(x)
          lev=levels(as.factor(x))
          lev=setdiff(lev,"N")
          #print(lev)
          len=length(lev)
        }
        # if(len==2)
        # { #inter=intersect(lev,inter_store)
        #   if(!setequal(character(0),inter))
        #   {
        #     lev=union(lev,"UU")
        #     len=len+1
        #   }
        # }
        if(len==3&bit==2)
        {
          inter=intersect(lev,inter_store)
        }
      }
      #print(lev)
      #print(len)
      #Jiabo code is end here

      #Genotype counts
      count=1:len
      for(i in 1:len){
        count[i]=length(x[(x==lev[i])])
      }

      #print(count)


      if(Major.allele.zero){
        if(len>1 & len<=3){
          #One bit: Make sure that the SNP with the major allele is on the top, and the SNP with the minor allele is on the second position
          if(bit==1){
            count.temp = cbind(count, seq(1:len))
            if(len==3) count.temp = count.temp[-3,]
            count.temp <- count.temp[order(count.temp[,1], decreasing = TRUE),]
            if(len==3)order =  c(count.temp[,2],3)else order = count.temp[,2]

          }
          #Two bit: Make sure that the SNP with the major allele is on the top, and the SNP with the minor allele is on the third position
          if(bit==2){
            count.temp = cbind(count, seq(1:len))
            if(len==3) count.temp = count.temp[-2,]
            count.temp <- count.temp[order(count.temp[,1], decreasing = TRUE),]
            if(len==3) order =  c(count.temp[1,2],2,count.temp[2,2])else order = count.temp[,2]

          }

          count = count[order]
          # print(count)
          lev = lev[order]
          # print(lev)

        }   #End  if(len<=1 | len> 3)
      } #End  if(Major.allele.zero)

      #print(x)

      #make two  bit order genotype as AA,AT and TT, one bit as A(AA),T(TT) and X(AT)
      if(bit==1 & len==3){
        temp=count[2]
        count[2]=count[3]
        count[3]=temp
      }

      #print(lev)
      #print(count)
      position=order(count)

      #Jiabo creat this code to convert AT TA to 1 and 2.2018.5.29

      # lev1=lev
      # if(bit==2&len==3)
      # {
      # lev1[1]=lev[count==sort(count)[1]]
      # lev1[2]=lev[count==sort(count)[2]]
      # lev1[3]=lev[count==sort(count)[3]]
      # position=c(1:3)
      # lev=lev1
      # }
      #print(lev)
      #print(position)
      #print(inter)
      #Jiabo code is end here
      if(bit==1){
        lev0=c("R","Y","S","W","K","M")
        inter=intersect(lev,lev0)
      }

      #1status other than 2 or 3
      if(len<=1 | len> 3)x=0

      #2 status
      if(len==2)
      {

        if(!setequal(character(0),inter))
        {
          x=ifelse(x=="N",NA,ifelse(x==inter,1,0))
        }else{
          x=ifelse(x=="N",NA,ifelse(x==lev[1],0,2))     # the most is set 0, the least is set 2
        }
      }

      #3 status
      if(bit==1){
        if(len==3)x=ifelse(x=="N",NA,ifelse(x==lev[1],0,ifelse(x==lev[3],1,2)))
      }else{
        if(len==3)x=ifelse(x=="N",NA,ifelse(x==lev[lev!=inter][1],0,ifelse(x==inter,1,2)))
      }

      #print(paste(lev,len,sep=" "))
      #print(position)

      #missing data imputation
      if(impute=="Middle") {x[is.na(x)]=1 }

      if(len==3){
        if(impute=="Minor")  {x[is.na(x)]=position[1]  -1}
        if(impute=="Major")  {x[is.na(x)]=position[len]-1}

      }else{
        if(impute=="Minor")  {x[is.na(x)]=2*(position[1]  -1)}
        if(impute=="Major")  {x[is.na(x)]=2*(position[len]-1)}
      }

      #alternative genetic models
      if(effect=="Dom") x=ifelse(x==1,1,0)
      if(effect=="Left") x[x==1]=0
      if(effect=="Right") x[x==1]=2

      if(byRow) {
        result=matrix(x,n,1)
      }else{
        result=matrix(x,1,n)
      }

      return(result)
    }#end of GAPIT.Numericalization function

  `GAPIT.HapMap` <-
    function(G,SNP.effect="Add",SNP.impute="Middle",heading=TRUE, Create.indicator = FALSE, Major.allele.zero = FALSE){
      #Object: To convert character SNP genotpe to numerical
      #Output: Coresponding numerical value
      #Authors: Feng Tian and Zhiwu Zhang
      # Last update: May 30, 2011
      ##############################################################################################
      print(paste("Converting HapMap format to numerical under model of ", SNP.impute,sep=""))
      #gc()
      #GAPIT.Memory.Object(name.of.trait="HapMap.Start")

      #GT=data.frame(G[1,-(1:11)])
      if(heading){
        GT= t(G[1,])
        #GI= G[-1,c(1,3,4)]
      }else{
        GT=t(G)
        #GI= G[,c(1,3,4)]
      }


      #Set column names
      if(heading)colnames(GT)="taxa"
      #colnames(GI)=c("SNP","Chromosome","Position")

      #Initial GD
      GD=NULL
      bit=2#=nchar(as.character(G[2,12])) #to determine number of bits of genotype
      #print(paste("Number of bits for genotype: ", bit))

      print("Perform numericalization")

      if(heading){
        if(!Create.indicator) GD= apply(GT,1,function(one) GAPIT.Numericalization(one,bit=bit,effect=SNP.effect,impute=SNP.impute, Major.allele.zero=Major.allele.zero))
        if(Create.indicator) GD= t(G)
      }else{
        if(!Create.indicator) GD= apply(GT,1,function(one) GAPIT.Numericalization(one,bit=bit,effect=SNP.effect,impute=SNP.impute, Major.allele.zero=Major.allele.zero))
        if(Create.indicator) GD= t(G)
      }

      #set GT and GI to NULL in case of null GD
      if(is.null(GD)){
        GT=NULL
       # GI=NULL
      }

      #print("The dimension of GD is:")
      #print(dim(GD))


      if(!Create.indicator) {print(paste("Succesfuly finished converting HapMap which has bits of ", bit,sep="")) }
      return(data.frame(GD))
    }#end of GAPIT.HapMap function


  Genos = openxlsx::read.xlsx(paste0(sdp,"exportmarkers.xlsx"), 1 )#; colnames(earht.prism.norm)[1] = "Female Pedigree"
  trimpeds = read.csv(paste0(sdp,"BV.HSIdentical.df.trimpeds.csv") )#; colnames(earht.prism.norm)[1] = "Female Pedigree"
#
#   Genos[Genos == "CA"] = 1
#   Genos[Genos == "TG"] = 2
#   Genos[Genos == "GA"] = 3
#   Genos[Genos == "GC"] = 4
#   Genos[Genos == "TA"] = 5
#   Genos[Genos == "CT"] = 6
#   Genos[Genos == "AC"] = 7
#   Genos[Genos == "GT"] = 8
#   Genos[Genos == "AG"] = 9
#   Genos[Genos == "CG"] = 10
#   Genos[Genos == "AT"] = 11
#   Genos[Genos == "TC"] = 12
#   Genos[Genos == "AA"] = 13
#   Genos[Genos == "TT"] = 14
#   Genos[Genos == "CC"] = 15
#   Genos[Genos == "GG"] = 16

  Genos =  Genos[, which(colMeans(is.na(Genos)) < 0.8)]
  Genos =  Genos[ which(rowMeans(is.na(Genos)) < 0.8), ]
  Genos.markers = colnames(Genos)[-c(1,2)]


  hm=GAPIT.HapMap(G=Genos[,-c(1,2)],
                  SNP.effect="Add",SNP.impute="Middle",heading=FALSE,
                  Create.indicator = FALSE, Major.allele.zero = FALSE)


  hm = hm[vapply(hm, function(x) length(unique(x)) > 1, logical(1L))]
  Genos.markers = colnames(hm)

  hm[hm == "1"] = NA

  X.knn= impute::impute.knn(as.matrix(hm), k=10, colmax = .9)
  Genos.impute = X.knn$data
  Genos.impute = data.frame(Genos.impute)
  #Genos.impute$dummy1 = ""
  #Genos.impute$dummy2 = ""
  colnames(Genos.impute) = Genos.markers
  Genos.impute = Genos.impute %>% dplyr::mutate_all(as.numeric)

  cat("Calculating PCA","\n")
  Genos.pca = stats::prcomp(x=Genos.impute, rank. = 3)

  Genos.pca = (Genos.pca[["x"]])
  Genos.impute = cbind(Genos.pca, Genos.impute)

  Genos.impute = cbind(Genos[,1:2], Genos.impute)

  Genos.impute = Genos.impute[!duplicated(Genos.impute$X1),]

  #trimPeds = data.frame(linked.peds[!duplicated(linked.peds$match),"match"])
  trimPeds = data.frame(trimpeds[!duplicated(trimpeds$FEMALE),"FEMALE"])

  Genos.impute.inbreds = dplyr::left_join(trimPeds, Genos.impute, by = c("trimpeds..duplicated.trimpeds.FEMALE....FEMALE.." = "X2"))
  Genos.impute.inbreds.trimpeds = Genos.impute.inbreds[!is.na(Genos.impute.inbreds$X1), ]

  orginalPeds = data.frame(linked.peds$pedigree)

  Genos.impute.inbreds = dplyr::left_join(orginalPeds, Genos.impute, by = c("linked.peds.pedigree" = "X2"))
  Genos.impute.inbreds.fullpeds = Genos.impute.inbreds[!is.na(Genos.impute.inbreds$X1), ]
  colnames(Genos.impute.inbreds.fullpeds)[1] = "trimpeds..duplicated.trimpeds.FEMALE....FEMALE.."

  Genos.impute.inbreds = rbind(Genos.impute.inbreds.trimpeds,Genos.impute.inbreds.fullpeds)

  Genos.impute.inbreds = Genos.impute.inbreds[!duplicated(Genos.impute.inbreds$trimpeds..duplicated.trimpeds.FEMALE....FEMALE..),]



  trainingx2.1 = dplyr::inner_join(trainingx2, Genos.impute.inbreds, by=c("pedigree"="trimpeds..duplicated.trimpeds.FEMALE....FEMALE.."))
  trainingx2.2 = dplyr::inner_join(trainingx2, Genos.impute.inbreds, by=c("FEMALE"="trimpeds..duplicated.trimpeds.FEMALE....FEMALE.."))
  trainingx2.3 = dplyr::inner_join(trainingx2, Genos.impute.inbreds, by=c("MALE"="trimpeds..duplicated.trimpeds.FEMALE....FEMALE.."))

   trainingx2 = rbind(trainingx2.1,trainingx2.2,trainingx2.3)


  rm(Genos, Genos.markers,X.knn,Genos.impute.inbreds.fullpeds,Genos.impute.inbreds.trimpeds,trimPeds,trimpeds,
     orginalPeds,Genos.impute,Genos.pca,trainingx2.1,trainingx2.2,trainingx2.3,Genos.impute.inbreds )

  gc()


  trainingx2 = trainingx2[!duplicated(trainingx2$RecId),]

  gc()


  return(data.frame(trainingx2))


}














