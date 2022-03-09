
genoReady = function(sdp, inbreds, linked.peds, trainingx2){

  Genos = openxlsx::read.xlsx(paste0(sdp,"exportmarkers.xlsx"), 1 )#; colnames(earht.prism.norm)[1] = "Female Pedigree"
  trimpeds = read.csv(paste0(sdp,"BV.HSIdentical.df.trimpeds.csv") )#; colnames(earht.prism.norm)[1] = "Female Pedigree"

  Genos[Genos == "CA"] = 1
  Genos[Genos == "TG"] = 2
  Genos[Genos == "GA"] = 3
  Genos[Genos == "GC"] = 4
  Genos[Genos == "TA"] = 5
  Genos[Genos == "CT"] = 6
  Genos[Genos == "AC"] = 7
  Genos[Genos == "GT"] = 8
  Genos[Genos == "AG"] = 9
  Genos[Genos == "CG"] = 10
  Genos[Genos == "AT"] = 11
  Genos[Genos == "TC"] = 12
  Genos[Genos == "AA"] = 13
  Genos[Genos == "TT"] = 14
  Genos[Genos == "CC"] = 15
  Genos[Genos == "GG"] = 16

  Genos =  Genos[, which(colMeans(is.na(Genos)) < 0.8)]
  Genos =  Genos[ which(rowMeans(is.na(Genos)) < 0.8), ]
  Genos.markers = colnames(Genos)[-c(1,2)]

  X.knn= impute::impute.knn(as.matrix(Genos[,-c(1:2)]), k=10, colmax = .9)
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














