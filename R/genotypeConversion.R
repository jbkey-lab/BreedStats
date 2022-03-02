
genoReady = function(wdp){

  Genos = fread(paste0(wdp,"Geno_QTL_plthtearht_2020.csv"),header = F)#; colnames(earht.prism.norm)[1] = "Female Pedigree"
  #genoMap = fread(paste0("R:/Breeding/MT_TP/Models/QTL/Geno_Map.csv"),header = F)

  #Genos = read.table("R:/Breeding/MT_TP/Models/QTL/Geno_QTL_plthtearht_2020.txt" , head = F, sep = '\t')
  #colnames(Genos)[1]="taxa"
  Genos$taxa = as.factor(Genos$taxa)
  #geno.col = data.frame(Genos[, 1])
  Genos.filter = data.frame(Genos)
  #Genos.filter=data.frame(Genos[-1, ])
  #Genos.filter=data.frame(Genos.filter[ ,-1])
  Genos.filter[Genos.filter == "TC"] = "CT"
  Genos.filter[Genos.filter == "CA"] = "AC"
  Genos.filter[Genos.filter == "TG"] = "GT"
  Genos.filter[Genos.filter == "GA"] = "AG"
  Genos.filter[Genos.filter == "GC"] = "CG"
  Genos.filter[Genos.filter == "TA"] = "AT"
  Genos.filter =  Genos.filter[, which(colMeans(Genos.filter == "NN") < 0.8)]
  Genos.filter = t(Genos.filter)
  Genos.filter = data.frame(Genos.filter)

  Genos.filter = left_join(genoMap, Genos.filter, by = c("V1" = "X1"))

  colsna = list(
    "alleles",
    "strand",
    "assembly",
    "center",
    "protLSID",
    "assayLSID",
    "panel",
    "QCcode"
  )
  for (i in colsna) {
    Genos.filter[, i] = ""
  }
  alleles = Genos.filter$alleles
  strand = Genos.filter$strand
  assembly = Genos.filter$assembly
  center = Genos.filter$center
  protLSID = Genos.filter$protLSID
  assayLSID = Genos.filter$assayLSID
  panel = Genos.filter$panel
  QCcode = Genos.filter$QCcode

  Genos.filter.done = cbind(
    Genos.filter[, 1],
    alleles,
    Genos.filter[, c(2, 3)],
    strand,
    assembly,
    center,
    protLSID,
    assayLSID,
    panel,
    QCcode

  )
  Genos.filter.done  = left_join(data.frame(Genos.filter.done),
                                 Genos.filter ,
                                 by="V1" )

  Genos.filter.done=Genos.filter.done[,-c(12,13,5685,5686,5687,5688,5689,5690,5691,5692)]
  Genos.filter.done[1,2] = "alleles"
  Genos.filter.done[1,5] = "strand"
  Genos.filter.done[1,6] = "assembly"
  Genos.filter.done[1,7] = "center"
  Genos.filter.done[1,8] = "protLSID"
  Genos.filter.done[1,9] = "assayLSID"
  Genos.filter.done[1,10] = "panel"
  Genos.filter.done[1,11] = "QCcode"

  #Genos.filter = as.matrix(Genos.filter)
  setwd("C:/Users/jake.lamkey/Documents")
  #Genos.filter.tab = unlist(Genos.filter)
  #table(Genos.filter.tab)
  `GAPIT.Numericalization` <-function(x, bit, effect,impute, Create.indicator ,
                                      Major.allele.zero, byRow){
    #Object: To convert character SNP genotpe to numerical
    #Output: Coresponding numerical value
    #Authors: Feng Tian and Zhiwu Zhang
    # Last update: May 30, 2011
    ##############################################################################################
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

    return(as.matrix(result))
  }#end of GAPIT.Numericalization function


  genoNum  = GAPIT.Numericalization(x,bit=2, effect="Add",impute="None", Create.indicator = FALSE,
                                    Major.allele.zero = FALSE, byRow=TRUE)




  #
  #   myGAPIT <- GAPIT(G = Genos.filter.done,
  #                    #GM = genoMap,
  #                    Y = myY.ear,
  #                    output.numerical = TRUE)
  #

  ####convert hapmap to numeric
  #GD = apply(Genos.filter,1,function(one) GAPIT.Numericalization(one))

  #hapToNum <- GAPIT.Numericalization(x=Genos.filter[1:10],impute="Middle",bit=2, Major.allele.zero = F, byRow=T)
  #hapToNum


  #Geno.numeric = rbind(geno.row, hapToNum)

  #Geno.numeric = cbind(geno.col, Geno.numeric)
  Geno.numeric = fread(paste0(fdp, "Geno_QTL_plthtearht_2020_numerical.csv")) #move file from desktop computer to server

  Geno.numeric=Geno.numeric[-1,]
  Geno.numeric=Geno.numeric[,-1]

  genos.row = Genos[1, ]
  genos.col = Genos[, 1] ; genos.col = genos.col[-1,]

  Geno.numeric.done = cbind(genos.col,Geno.numeric)
  Geno.numeric.done = rbind(genos.row,Geno.numeric.done)

  write.csv(Geno.numeric.done,
            paste0(fdp, "Geno_QTL_plthtearht_2020_Numeric.csv"))
}
