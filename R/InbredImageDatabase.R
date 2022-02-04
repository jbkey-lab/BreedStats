#if(!require(xlsx, warn.conflicts = FALSE)){
#  install.packages("xlsx")
#  library(xlsx, warn.conflicts = FALSE)
#}

#if(!require(fs, warn.conflicts = FALSE)){
#  install.packages("fs")
#  library(fs, warn.conflicts = FALSE)
#}

prepImages = function( ){

  dp="R:/Research General/Marshalltown_IA/Inbred Pictures/" #working directory
  #dp="C:/Users/jake.lamkey/Desktop/"
  #setwd("P:/Temp")
  #function to collect traited pictures
  all_imgs <- fs::dir_ls(
    paste0(dp,"Inbreds_original/"),
    recurse = TRUE,
    type = "file",
    glob = "*_E*"
  )
  images.path.ear<-data.frame(all_imgs); dim(images.path.ear)

  all_imgs <- fs::dir_ls(
    paste0(dp,"Inbreds_original/"),
    recurse = TRUE,
    type = "file",
    glob = "*_T*"
  )
  images.path.tassel<-data.frame(all_imgs); dim(images.path.tassel)

  all_imgs <- fs::dir_ls(
    paste0(dp,"Inbreds_original/"),
    recurse = TRUE,
    all=T,
    type = "file",
    glob = "*_S*"
  )
  images.path.silk<-data.frame(all_imgs); dim(images.path.silk)

  all_imgs <- fs::dir_ls(
    paste0(dp,"Inbreds_original/"),
    recurse = TRUE,
    all=T,
    type = "file",
    glob =  "*_B*"
  )
  images.path.br<-data.frame(all_imgs); dim(images.path.br)

  #folder name
  images.dir=list.dirs(path=paste0(dp,"Inbreds_original/")); images.dir=data.frame(images.dir)

  #check
  head(images.path.ear); head(images.path.tassel); head(images.path.silk); head(images.path.br); head(images.dir)
  #gsub to get only variety and image name
  images.dir$Inbreds = gsub(paste0(dp,"Inbreds_original/"),"",as.character(images.dir$images.dir))

  images.path.tassel$Inbreds = sub(paste0(dp,"Inbreds_original/"),"",as.character(images.path.tassel$all_imgs))
  images.path.tassel$Inbreds = sub("/.*","",as.character(images.path.tassel$Inbreds));head(images.path.tassel)

  images.path.silk$Inbreds = sub(paste0(dp,"Inbreds_original/"),"",as.character(images.path.silk$all_imgs))
  images.path.silk$Inbreds = sub("/.*","",as.character(images.path.silk$Inbreds));head(images.path.silk)

  images.path.br$Inbreds = sub(paste0(dp,"Inbreds_original/"),"",as.character(images.path.br$all_imgs))
  images.path.br$Inbreds = sub("/.*","",as.character(images.path.br$Inbreds));head(images.path.br)

  images.path.ear$Inbreds = sub(paste0(dp,"Inbreds_original/"),"",as.character(images.path.ear$all_imgs))
  images.path.ear$Inbreds = sub("/.*","",as.character(images.path.ear$Inbreds));head(images.path.ear)

  #joing the tables into one
  images.path.t = dplyr::left_join(images.dir, images.path.tassel,by="Inbreds")
  images.path.ts = dplyr::left_join(images.path.t, images.path.silk,by="Inbreds")
  images.path.tsbr = dplyr::left_join(images.path.ts, images.path.br,by="Inbreds")
  images.path.tsbre = dplyr::left_join(images.path.tsbr, images.path.ear,by="Inbreds")
  colnames(images.path.tsbre) = c("Path","Inbred","Tassel","Silk","BraceRoot","Ear")

  #remove duplicates
  images.path.tsbre=images.path.tsbre[!duplicated(images.path.tsbre$Inbred),]
  #write.csv(images.path.tsbre,"Images.Path.Inbredspics.csv",na="",row.names=F)

  #qualdat=read.csv("P:/Temp/Images.Path.Inbredspics.csv")

  industryNames = InbredNameLibrary()
  industryNames = industryNames[[2]]

  images.path.tsbre$Inbred <- dplyr::revalue(as.character(images.path.tsbre$Inbred),industryNames  ) #industry name to inbred name conversion

  qualdat = images.path.tsbre[!duplicated(images.path.tsbre$Inbred),]
  openxlsx::write.xlsx(qualdat,paste0("R:/Research General/Marshalltown_IA/Inbred Pictures/Images.Path.Inbredspics.xlsx"),row.names=F,overwrite=T)



  imageReduce(dp=paste0(dp,"Inbreds_original/"),ndp="R:/Research General/Marshalltown_IA/Inbred Pictures/Inbreds/")



  #Images.Path.Inbredpics=read.xlsx("P:/Temp/Images.Path.Inbredspics.xlsx",1);colnames(Images.Path.Inbredpics)[2]="Inbred";dim(Images.Path.Inbredpics)
  #inbred_list=read.xlsx("P:/Temp/InbredListData_rpt_Final6.11.20.xlsx",1)
  #dim(inbred_list)
  #InbredListData_rpt_Final.xlsx.paths = dplyr::left_join(inbred_list, Images.Path.Inbredpics, by="Inbred");dim(InbredListData_rpt_Final.xlsx.paths)
  #InbredListData_rpt_Final.xlsx.paths = InbredListData_rpt_Final.xlsx.paths[!duplicated(InbredListData_rpt_Final.xlsx.paths$Inbred),]
  #dim(InbredListData_rpt_Final.xlsx.paths)
  #write.xlsx(InbredListData_rpt_Final.xlsx.paths, "InbredListData_rpt_Final.xlsx",row.names=F,showNA=F)

}

