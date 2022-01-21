

##########################################################################
#PEDIGREE ADJUSTMENT
##########################################################################
# cat("----------------------------Loading Packages----------------------------", "\n")
#
# if(!(suppressWarnings(suppressMessages(require(openxlsx, warn.conflicts = FALSE))))){
#   install.packages("openxlsx")
#   suppressWarnings(suppressMessages(library(openxlsx, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(dplyr, warn.conflicts = FALSE))))){
#   install.packages("dplyr")
#   suppressWarnings(suppressMessages(library(dplyr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(data.table, warn.conflicts = FALSE))))){
#   install.packages("data.table")
#   suppressWarnings(suppressMessages(library(data.table, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(pastecs, warn.conflicts = FALSE))))){
#   install.packages("pastecs")
#   suppressWarnings(suppressMessages(library(pastecs, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(tidyr, warn.conflicts = FALSE))))){
#   install.packages("tidyr")
#   suppressWarnings(suppressMessages(library(tidyr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(stringr, warn.conflicts = FALSE))))){
#   install.packages("stringr")
#   suppressWarnings(suppressMessages(library(stringr, warn.conflicts = FALSE)))
# }
#
# if(!(suppressWarnings(suppressMessages(require(tidyverse, warn.conflicts = FALSE))))){
#   install.packages("tidyverse")
#   (suppressWarnings(suppressMessages(library(tidyverse, warn.conflicts = FALSE))))
# }
#
# if(!(suppressWarnings(suppressMessages(require(doParallel, warn.conflicts = FALSE))))){
#   install.packages("doParallel")
#   (suppressWarnings(suppressMessages(library(doParallel, warn.conflicts = FALSE))))
# }
#
# if(!(suppressWarnings(suppressMessages(require(ggplot2, warn.conflicts = FALSE))))){
#   install.packages("ggplot2")
#   (suppressWarnings(suppressMessages(library(ggplot2, warn.conflicts = FALSE))))
# }
#
# ############
# folder="Test"
# x="5_27_2021"
# ws = paste0('/home/jacoblamkey/Downloads/Peds/YT_BV Yield Trial Master Catalog ',x, ".csv")
# fdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder)
# fdph = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/Hybrid")
# wdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/")
# vdp = '/home/jacoblamkey/Downloads/Peds/Variety.male.female.csv'
#
# ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
# fdp = paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder)
# fdph = paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder,"/Hybrid")
# wdp = paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/")
# vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'
# #
# ws = ws
# doHybridID=F
# doPedigreeChange =F
# doPedigreeToBecksChange=F
# doGCABV = F
# doWriteFinalPedigrees = F
# year= "2020"
#
# A = T
# B = T
# C = T
# Prop = T
# Choice = T
# D=T
# R=T
# X=T
# E=T
# Q=T
# V=T
# GEM=T
#
# s0=T
# s1=T
# s2=T
# s3=T
# s4=T
# s5=T
# doDNN=F
#
# seas0=21
# seas1=20
# seas2=19
# seas3=18
# seas4=17
# seas5=16
# doYear=T
# BV.MC.Entry.data.AB = read.table(paste0("C:/Users/jake.lamkey/Desktop/BV.MC.Entry.data.",
#                                         if(A){print("A")},
#                                         if(B){print("B")},
#                                         if(C){print("C")},
#                                         if(Prop){print("Prop")},
#                                         if(Choice){print("Choice")},
#                                         if(D){print("D")},
#                                         if(E){print("E")},
#                                         if(Q){print("Q")},
#                                         if(R){print("R")},
#                                         if(V){print("V")},
#                                         if(X){print("X")}
#                                         ,".txt"))

PedAdjust = function(  data = BV.MC.Entry.data.AB,doReduceNonCodes  ){

   BV.MC.Entry.data.AB = data.frame(data)
  #
  # rm(BV.MC.Entry, BV.MC.Entry.data.AB.clean, BV.MC.Entry.data.AB.checkfilter, BV.MC.Entry.data.AB.checkfilter.variety.duplciate,
  #    BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped, BV.MC.Entry.data.AB.checkfilter.variety)
  # invisible(gc())

  to_search_in.female <- data.table(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique_female_id),c(39,21)])
  colnames(to_search_in.female)=c("unique","pedigree")

  to_search_with.female <- tibble(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique_female_id),c(39)])
  colnames(to_search_with.female) = "unique_female_id"

  to_search_in.male <- data.table(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique_male_id),c(40,20)])
  colnames(to_search_in.male)=c("unique","pedigree")

  to_search_with.male <- tibble(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique_male_id),c(40)])
  colnames(to_search_with.male) = "unique_male_id"

  to_search_in.female.order = order(to_search_in.female$pedigree)
  to_search_in.female = to_search_in.female[to_search_in.female.order,]

  to_search_with.male.order = order(to_search_with.male$unique_male_id)
  to_search_with.male = to_search_with.male[to_search_with.male.order,]

  to_search_in.male.order = order(to_search_in.male$pedigree)
  to_search_in.male = to_search_in.male[to_search_in.male.order,]

  to_search_with.female.order = order(to_search_with.female$unique_female_id)
  to_search_with.female = to_search_with.female[to_search_with.female.order,]

  rm(data)

  #################################################
  #destfile7 = paste0(wdp,"linked.male.female.nestedpeds.xlsx")

  to_search_in.male$Gender = "Male"; colnames(to_search_in.male)[1] = "uniqued_id"
  to_search_in.female$Gender = "FEMALE"; colnames(to_search_in.female)[1] = "uniqued_id"

  linked.peds = full_join(to_search_in.female, to_search_in.male, by=c("uniqued_id","pedigree"))
  linked.peds$pedigree = as.character(linked.peds$pedigree)
  linked.peds$Gender = paste0(linked.peds$Gender.x, "/", linked.peds$Gender.y)
  linked.peds$Gender = gsub(pattern="\\/\\N\\A" , x = linked.peds$Gender, replacement="")
  linked.peds$Gender = gsub(pattern="\\N\\A\\/" , x = linked.peds$Gender, replacement="")

  linked.peds=linked.peds[,-c(3,4)]
  linked.peds$pedigree = ifelse(linked.peds$pedigree=="",
                                as.character(linked.peds$uniqued_id),
                                linked.peds$pedigree)

  linked.peds$pedigree = ifelse(is.na(linked.peds$pedigree),
                                as.character(linked.peds$uniqued_id),
                                linked.peds$pedigree)

  linked.peds = linked.peds[!duplicated(linked.peds$uniqued_id), ]
  linked.peds$match = linked.peds$pedigree
  linked.peds = linked.peds[,c(1,2,4,3)]
  #linked.peds=data.table(linked.peds)

  #b/c (JC6794/LH185)/I10516-53 = ((JC6794/LH185)/I10516-53.2)/I10516-1
  # linked.peds = data.frame(linked.peds)
  # for(i in 1:nrow(linked.peds)){
  #   #if(!grepl(linked.peds[i,1], pattern="\\.")){
  #   linked.peds[i,3] = ifelse( nchar(linked.peds[i,1]) < nchar(linked.peds[i,3]), linked.peds[i,1], linked.peds[i,3])
  # }



  #linked.peds = write.xlsx(linked.peds,"C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx")

  #linked.peds = read.xlsx("C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx",1)

  #linked.peds$match = linked.peds$match1
  #linked.peds = linked.peds[,c(1,2,20,3:19)]
  #linked.peds.update=foreach(i = 1:nrow(linked.peds),
  #                            .combine = cbind
  #.packages=c("dplyr","tidyr","stringr"),
  #.export=c("sub","grepl")
  #  ) %dopar% {
  linked.peds$match <- gsub("[[:space:]]", "", linked.peds$match)
  linked.peds$match <- gsub("[[:space:]]", "", linked.peds$match)
  linked.peds$match <- gsub("[[:space:]]", "", linked.peds$match)

  linked.peds$match <- as.character(linked.peds$match)





  ############################################################
  #Coded line import
  ############################################################


  #linked.peds.save = linked.peds


  #linked.peds = linked.peds.save


  BV.MC.Inbred <- openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.xlsx"),1)
  BV.MC.Inbred$Pedigre_Backup = BV.MC.Inbred$PEDIGREE
  BV.MC.Inbred = BV.MC.Inbred[,c(1:3,21,4:20)]
  BV.MC.Inbred = BV.MC.Inbred[!is.na(BV.MC.Inbred$PEDIGREE),]

  to_search_in <- data.table(linked.peds[!duplicated(linked.peds$match),c(3)])
  colnames(to_search_in)=c("unique")

  to_search_with <- tibble(BV.MC.Inbred[!duplicated(BV.MC.Inbred$PEDIGREE),c(2,3)])
  colnames(to_search_with) = c("Code","unique_ped_id")
  #to_search_with.male <- tibble(BV.MC.Male[!duplicated(BV.MC.Male$match1),c(2)])
  #colnames(to_search_with.male) = c("unique_male_id")

  dim(to_search_with);dim(to_search_in)

  # rm(BV.MC.Entry.data.AB)
  # invisible(gc())
  #
  #n = nrow(to_search_with.male)

  #for(batch in 1:nrow(to_search_with)){

  #to_search_with.batch = to_search_with[batch,]
  invisible(gc(reset=T)) #cleans memory "garbage collector"
  memory.limit(size=15071)
  digitDH = "((-)?B\\.DHB[0-9]*|(-)?B\\.DH[0-9]*|(-)?\\.DH-B[0-9]*|(-)?\\.DHB[0-9]*|(-)?\\.DH[0-9]*)"
  cat("N")
  linked.peds.beck = to_search_with %>%
    plyr::mutate(data = list(to_search_in) ) %>%
    tidyr::unnest(data) %>%
    plyr::mutate(unique_ped_id_nchar = nchar(unique_ped_id) ) %>%
    plyr::mutate(unique_nchar = nchar(unique) ) %>%
    plyr::mutate(unique_ped_id_DH = (stringr::str_extract(digitDH, string = unique_ped_id) ) ) %>%
    #plyr::mutate(unique_ped_id_DH2 = gsub(".*?\\.(.*?)\\.*", x=unique_ped_id, value=T) ) %>%
    plyr::mutate(unique_DH = stringr::str_extract(digitDH, string=unique) ) %>%
    dplyr::filter(unique_nchar <= (unique_ped_id_nchar+15)  )

  linked.peds.beck2 = linked.peds.beck %>%
    plyr::mutate(unique_ped_id_DH_1 = ifelse((unique_ped_id_nchar < 10), as.matrix(grepl("\\/", x = unique )), F))

  linked.peds.beck3 =  linked.peds.beck2 %>%
    dplyr::filter(  str_detect(unique, (stringr::coll(unique_ped_id)  ) ) )  %>%
    dplyr::filter(unique_ped_id_DH_1 != TRUE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 =   linked.peds.beck3 %>%
    plyr::mutate(DH_Match = ifelse(unique_ped_id_DH == unique_DH , TRUE,FALSE) ) %>%
    plyr::mutate(DH_Match = ifelse(is.na(DH_Match), TRUE, DH_Match)) %>%
    dplyr::filter(DH_Match != FALSE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 = linked.peds.beck4 %>%
    dplyr::select(Code,unique_ped_id,unique) %>%
    dplyr::group_by(Code,unique) %>%
    dplyr::summarise(strings = (stringr::str_c(unique_ped_id, collapse = ", ")))

  cat("M")

  rm(linked.peds.beck, linked.peds.beck2, linked.peds.beck3)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck = linked.peds.beck4 %>% tidyr::separate("strings", sep="[, ][ ]",
                                                           c("match1", "match2","match3", "match4","match5", "match6","match7", "match8","match9", "match10","match11", "match12","match13", "match14" ,"match15", "match16"),
                                                           extra="merge",
                                                           remove=F)
  rm(linked.peds.beck4)


  linked.peds.beck = dplyr::left_join(linked.peds.beck, BV.MC.Inbred[,c(2,3)], by=c("Code"="NEW.CODE"))
  linked.peds.beck = linked.peds.beck[,c(20,1:19)]



  linked.peds = dplyr::left_join(linked.peds[, c(1,2,3,4)], linked.peds.beck[, c(2,3)], by = c("match"="unique"))
  #bind.linked.female.peds[[length(bind.linked.female.peds)+1]] = linked.female.peds
  # rm(linked.female.peds, to_search_with.batch)
  # }

  # linked.female.peds = rbindlist(bind.linked.female.peds)
  linked.peds$match = ifelse(!is.na(linked.peds$Code) , as.character(linked.peds$Code), linked.peds$match)

  #############################################################
  #############################################################
  #############################################################
  BV.MC.Inbred <- openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.xlsx"),1)
  BV.MC.Inbred$Pedigre_Backup = BV.MC.Inbred$PEDIGREE
  BV.MC.Inbred = BV.MC.Inbred[,c(1:3,21,4:20)]
  BV.MC.Inbred = BV.MC.Inbred[!is.na(BV.MC.Inbred$PEDIGREE),]

  ###run pedigree reduction function
  #source("R:/Breeding/MT_TP/Models/R-Scripts/greplPeds.R")

  newData=pedigreeReduce( data=BV.MC.Inbred, Codes=T)

  BV.MC.Inbred = newData
  # rm(data,newData)

  #BV.MC.Inbred.slash = BV.MC.Inbred %>% dplyr::filter(grepl(, pattern="/") == TRUE)

  to_search_in <- data.table(linked.peds[!duplicated(linked.peds$match),c(3)])
  colnames(to_search_in)=c("unique")

  to_search_with <- tibble(BV.MC.Inbred[!duplicated(BV.MC.Inbred$PEDIGREE),c(2,3)])
  colnames(to_search_with) = c("Code","unique_ped_id")
  #to_search_with.male <- tibble(BV.MC.Male[!duplicated(BV.MC.Male$match1),c(2)])
  #colnames(to_search_with.male) = c("unique_male_id")

  dim(to_search_with);dim(to_search_in)

  rm(to_search_with.male, to_search_with.female, to_search_in.female, to_search_in.male, linked.peds.beck)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  #n = nrow(to_search_with.male)

  #for(batch in 1:nrow(to_search_with)){

  #to_search_with.batch = to_search_with[batch,]
  linked.peds.save = linked.peds


  linked.peds = linked.peds.save
  cat("O")

  linked.peds.beck = to_search_with %>%
    plyr::mutate(data = list(to_search_in) ) %>%
    tidyr::unnest(data) %>%
    plyr::mutate(unique_ped_id_nchar = nchar(unique_ped_id) ) %>%
    plyr::mutate(unique_nchar = nchar(unique) ) %>%
    plyr::mutate(unique_ped_id_DH = (stringr::str_extract(digitDH, string = unique_ped_id) )) %>%
    #plyr::mutate(unique_ped_id_DH2 = gsub(".*?\\.(.*?)\\.*", x=unique_ped_id, value=T) ) %>%
    plyr::mutate(unique_DH = stringr::str_extract(digitDH, string=unique) ) %>%
    dplyr::filter(unique_nchar <= (unique_ped_id_nchar+15)  )

  linked.peds.beck2 = linked.peds.beck %>%
    plyr::mutate(unique_ped_id_DH_1 = ifelse((unique_ped_id_nchar < 10), as.matrix(grepl("\\/", x = unique )), F))
  rm(linked.peds.beck); invisible(gc(reset=T))

  linked.peds.beck3 =  linked.peds.beck2 %>%
    dplyr::filter(  str_detect(unique, (stringr::coll(unique_ped_id))  )  )  %>%
    dplyr::filter(unique_ped_id_DH_1 != TRUE)
  rm(linked.peds.beck2)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 =   linked.peds.beck3 %>%
    plyr::mutate(DH_Match = ifelse(unique_ped_id_DH == unique_DH , TRUE, FALSE) ) %>%
    plyr::mutate(DH_Match = ifelse(is.na(DH_Match), TRUE, DH_Match)) %>%
    dplyr::filter(DH_Match != FALSE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 = linked.peds.beck4 %>%
    dplyr::select(Code,unique_ped_id,unique) %>%
    dplyr::group_by(Code,unique) %>%
    dplyr::summarise(strings = (stringr::str_c(unique_ped_id, collapse = ", ")))


  rm(linked.peds.beck, linked.peds.beck2, linked.peds.beck3)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck = linked.peds.beck4 %>% tidyr::separate("strings", sep="[, ][ ]",
                                                           c("match1", "match2","match3", "match4","match5", "match6","match7", "match8","match9", "match10","match11", "match12","match13", "match14" ,"match15", "match16"),
                                                           extra="merge",
                                                           remove=F)
  rm(linked.peds.beck4)


  linked.peds.beck = dplyr::left_join(linked.peds.beck, BV.MC.Inbred[,c(2,3)], by=c("Code"="NEW.CODE"))
  linked.peds.beck = linked.peds.beck[,c(20,1:19)]
  colnames(linked.peds.beck)[2] = "CodeV2"


  linked.peds = dplyr::left_join(linked.peds[, c(1,2,3,4,5)], linked.peds.beck[, c(2,3)], by = c("match"="unique"))
  #bind.linked.female.peds[[length(bind.linked.female.peds)+1]] = linked.female.peds
  # rm(linked.female.peds, to_search_with.batch)
  # }

  # linked.female.peds = rbindlist(bind.linked.female.peds)
  linked.peds$match = ifelse(!is.na(linked.peds$CodeV2) , as.character(linked.peds$CodeV2), linked.peds$match)
  linked.peds$match = ifelse(!is.na(linked.peds$Code) , as.character(linked.peds$Code), linked.peds$match)

  linked.peds$Code = ifelse(is.na(linked.peds$Code ), as.character(linked.peds$CodeV2), linked.peds$Code)


  cat("P")


  linked.peds = linked.peds[!duplicated(linked.peds$uniqued_id), ]


  # write.xlsx(linked.peds,"C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx")
  #
  # #write.csv(x=BV.MC.Entry.data.AB, file="C:/Users/jake.lamkey/Desktop/BV.MC.Entry.data.AB.xlsx")
  #
  # #write.xlsx(x=BV.MC.Entry.data.AB, file="C:/Users/jake.lamkey/Desktop/BV.MC.Entry.data.AB.xlsx")
  #
  #
  # linked.peds = read.xlsx("C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx")
  #
  linked.peds.save = linked.peds


  linked.peds = linked.peds.save

  linked.peds$match = as.character(linked.peds$match)

  #######################################################
  #PEDIGREE CHANGE TO F4 REDUCITON
  #######################################################

  linked.peds$match <- gsub("[[:space:]]", "", linked.peds$match)
  linked.peds$match1 = linked.peds$match
  linked.peds$match2 = linked.peds$match
  linked.peds$match3 = linked.peds$match
  linked.peds$match4 = linked.peds$match
  linked.peds$match5 = linked.peds$match
  linked.peds$match6 = linked.peds$match
  linked.peds$match7 = linked.peds$match
  linked.peds$match8 = linked.peds$match
  linked.peds$match9 = linked.peds$match
  linked.peds$match10 = linked.peds$match
  linked.peds$match11 = linked.peds$match
  linked.peds$match12 = linked.peds$match
  linked.peds$match13 = linked.peds$match
  linked.peds$match14 = linked.peds$match
  linked.peds$match15 = linked.peds$match
  linked.peds$match16 = linked.peds$match

  linked.peds$proccessed = ""

  #linked.peds.match = data.frame(linked.peds$match)

  #linked.peds <- data.table(linked.peds)

  patterns = InbredNameLibrary()
  patterns = patterns[[1]]

  #inbreds##################################################################3
  match=stringr::str_detect(string = linked.peds$match, pattern = paste(patterns, collapse = "|"), negate=T)
  match=data.frame(match)
  for(i in 1:nrow(linked.peds)) {
    if(match[i,1]==TRUE){
      #print("yes")
      linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
      linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
      linked.peds[i,23] = "Proccessed" # add a period for using wildcards

    }
  }

  # match=str_detect(string = linked.peds$match, pattern = paste(patterns, collapse = "|"), negate=F)
  # match=data.frame(match)
  # for(i in 1:nrow(linked.peds)) {
  #   if(match[i,1]==T){
  #     #print("yes")
  #     #linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
  #     #linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
  #     linked.peds[i,23] = "Proccessed" # add a period for using wildcards
  #
  #   }
  # }
  # match=(!grepl(x = linked.peds$match, pattern = paste(patterns2, collapse = "|")))
  # match=data.frame(match)
  # for(i in 1:nrow(linked.peds)) {
  #   if(match[i,1]==TRUE){
  #     #print("yes")
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
  #   }
  # }
  #
  # match=(!grepl(x = linked.peds$match, pattern = paste(patterns3, collapse = "|")))
  # match=data.frame(match)
  # for(i in 1:nrow(linked.peds)) {
  #   if(match[i,1]==TRUE){
  #     #print("yes")
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
  #   }
  # }
  #
  # match=(!grepl(x = linked.peds$match, pattern = paste(patterns4, collapse = "|")))
  # match=data.frame(match)
  # for(i in 1:nrow(linked.peds)) {
  #   if(match[i,1]==TRUE){
  #     #print("yes")
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
  #     linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
  #   }
  # }
  #write.xlsx(linked.peds,"C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx")

  #linked.peds = read.xlsx("C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.xlsx",1)


  #source("R:/Breeding/MT_TP/Models/R-Scripts/greplPeds.R")

  if(doReduceNonCodes){
  newData=pedigreeReduce(data=linked.peds, Codes=F)

  nrow(newData)

  newDataRmDups = newData[!duplicated(newData$match),]
  nrow(newDataRmDups)

  newData$changed = ifelse(newData$pedigree != newData$match, T, F)
  newData.changed = newData %>% dplyr::filter(changed==T)

  nrow(newData.changed)


  linked.peds = newData
  #rm(data,newData)
}


cat("Done with adjusting Pedigrees")

  #linked.peds = newData
  openxlsx::write.xlsx(linked.peds, paste0(wdp,"/","linked.peds.xlsx"), overwrite=T)
  #write.xlsx(linked.peds, paste0(wdp,"/","linked.peds.xlsx"), overwrite=T)

  #write.xlsx(linked.peds, paste0(fdp,"linked.peds.updated.xlsx"))

  return(data.frame(linked.peds))

}
