######################################################################################################
######Rscript for The Flower Catalog
######################################################################################################
#rm(list=ls()) #remove environment vairalbes
invisible(gc(reset=T)) #cleans memory "garbage collector"
#memory.limit(size=8071)
#setwd("R:/Breeding/MT_TP/Models/shed_silk/")

######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################

#
# dpf = "R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_"
#
#    hdp = "R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/Becks_OBS 8_23_2021 (2).csv"
#     year= "2021"
#     l_year = 2017
#    h_year=2021
#   doPedigreeChange = F
#     doAdjInbredData = T

#year= "2020"
inbredChar = function(#dp = dp,
  dpf, #=  "R:/Breeding/MT_TP/Models/shed_silk/Inbred Data_",
  gdp, #= "R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/Becks_OBS GOSS 8_26_2021.csv",
  hdp, #= "R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/Becks_OBS 9_2_2021.csv",
  #oldfd = oldfd,
  year, #= "2021",
  l_year, #= 2017,
  h_year, #=2021,
  doAdjInbredData,
  simulate #= T
){

  if(simulate){
    data("inbreddata")
    data("inbredgossdata")
  }
  else{
    hpd = hdp
    dpf=dpf
    BV.MC.NUR<-fread(paste0(hdp))#all varieties to build the model
    BV.MC.GOSS<-fread(paste0(gdp))#all varieties to build the model
  }
  #doPedigreeChange = doPedigreeChange
  doAdjInbredData = doAdjInbredData

  BV.MC.GOSS = data.frame(BV.MC.GOSS)
  BV.MC.GOSS = data.table(BV.MC.GOSS)
  BV.MC.NUR = data.frame(BV.MC.NUR)
  BV.MC.NUR = data.table(BV.MC.NUR)
  BV.MC.GOSS$LocID = ""
  BV.MC.GOSS$Decision = ""
  BV.MC.GOSS$`Planting.` = ""
  BV.MC.GOSS$`Cob.Color` = ""
  BV.MC.GOSS$`EarHt..Rating.` = ""
  BV.MC.GOSS$`Plt.Height..Rating.` = ""
  BV.MC.GOSS$`Glume.Ring` = ""
  BV.MC.GOSS$`Leaf.Color` = ""
  BV.MC.GOSS$`Leaf.Texture` = ""
  BV.MC.GOSS$`Pollen.Duration..GDUs.10..to.90..` = ""
  BV.MC.GOSS$`Pollen.Shed.Rating` = ""
  BV.MC.GOSS$`Tassel.Branch..` = ""
  BV.MC.GOSS$`Tassel.Extension` = ""
  BV.MC.GOSS$`Ears.Per.Stalk` = ""
  BV.MC.GOSS$`Field..` = "Olivia"

  BV.MC.GOSS = BV.MC.GOSS[,c(1,2,3,5,4,6,7,49,8,9,10,11,12,13,14,50,51,15:19,42,21,22,23,39,25:30,52,31,32,53,54,33,55,34,56,57,58,
                             59,35,60,61,36,62,37,38,24,40,41,20,43,63,44,45,46,47,48)]
  BV.MC.GOSS = data.table(BV.MC.GOSS)

  colnames(BV.MC.GOSS)[41] = "Leaf.Angle..Inb.Obs."
  colnames(BV.MC.GOSS)[49] = "TillerCnt"

  BV.MC.Entry = rbind(BV.MC.NUR,BV.MC.GOSS)

  l_year = as.numeric(l_year)
  h_year = as.numeric(h_year)
  season = as.numeric(year) - 2000
  season = as.character(season)
  colnames(BV.MC.Entry)
  #'cat(l_year)
  #'cat(h_year)
  #'cat(season)
  #Clean the Entry list file

  colnames(BV.MC.Entry)[5] = "Entry.Book.Name"
  colnames(BV.MC.Entry)[21] = "Plot.Discarded"
  colnames(BV.MC.Entry)[22] = "Plot.Status"

  BV.MC.Entry.data = BV.MC.Entry[,-c(1,4,6,11,16,17,18,26)]
  dim(BV.MC.Entry.data)

  BV.MC.Entry.data <- BV.MC.Entry.data %>% filter(
    Plot.Discarded != "Yes",
    Plot.Status != "3 - Bad",
    Entry.Book.Name != "TP20S_Filler",
    Entry.Book.Name != "Filler",
    Entry.Book.Name != "MBS20S_obs",
    Entry.Book.Name != "FILL",
    Entry.Book.Name != "TF_INB_OBS",
    Pedigree != "placeholder",
    RecId != "12606653",
    RecId != "12606654"
  )
  BV.MC.Entry.data$`Book.Name` =  plyr::revalue(as.character(BV.MC.Entry.data$`Book.Name`),  c("ATL_inb_obs"= "Atlanta"  ,
                                                                                               "INBRED OBS"= "Olivia"    ,
                                                                                               "QA20SM_obs"="Marshalltown" ) )
  BV.MC.Entry.data.AB<-data.frame(BV.MC.Entry.data)


  #Add adjusted pedigrees to data file
  #seperate hybrid ID to obtain line id for male and female pedigrees

  #seperate numberic strings, character strings, and decimaled lineIDs can be unique to 8 digits in scietific notation

  BV.MC.Entry.data.AB$unique.Line.ID=round(as.numeric(BV.MC.Entry.data.AB$Line.ID), digits=8)

  BV.MC.Entry.data.AB$unique.Line.ID = format(BV.MC.Entry.data.AB$unique.Line.ID, nsmall=8,digits=9 ,na.encode	=FALSE)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID < 0,
                                              "needsdata",BV.MC.Entry.data.AB$unique.Line.ID)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID=="needsdata",
                                              BV.MC.Entry.data.AB$Line.ID,
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  #merge the male or female pedigree for NA values of unique columns
  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID=="",
                                              BV.MC.Entry.data.AB$`InbName`,
                                              BV.MC.Entry.data.AB$unique.Line.ID)
  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(is.na(BV.MC.Entry.data.AB$unique.Line.ID),
                                              BV.MC.Entry.data.AB$`InbName`,
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID=="",
                                              as.character(BV.MC.Entry.data.AB$Pedigree),
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(is.na(BV.MC.Entry.data.AB$unique.Line.ID),
                                              as.character(BV.MC.Entry.data.AB$Pedigree),
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID=="",
                                              BV.MC.Entry.data.AB$`Variety`,
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(is.na(BV.MC.Entry.data.AB$unique.Line.ID),
                                              BV.MC.Entry.data.AB$`Variety`,
                                              BV.MC.Entry.data.AB$unique.Line.ID)
  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(is.na(BV.MC.Entry.data.AB$unique.Line.ID),
                                              as.character(BV.MC.Entry.data.AB$Female.Pedigree),
                                              BV.MC.Entry.data.AB$unique.Line.ID)
  BV.MC.Entry.data.AB$unique.Line.ID = ifelse(BV.MC.Entry.data.AB$unique.Line.ID=="",
                                              as.character(BV.MC.Entry.data.AB$Female.Pedigree),
                                              BV.MC.Entry.data.AB$unique.Line.ID)

  #remove spaces in all mathcing columns
  BV.MC.Entry.data.AB$Pedigree = ifelse(BV.MC.Entry.data.AB$Pedigree=="",
                                        as.character(BV.MC.Entry.data.AB$InbName),
                                        BV.MC.Entry.data.AB$Pedigree)
  BV.MC.Entry.data.AB$Pedigree = ifelse(is.na(BV.MC.Entry.data.AB$Pedigree),
                                        as.character(BV.MC.Entry.data.AB$InbName),
                                        BV.MC.Entry.data.AB$Pedigree)

  BV.MC.Entry.data.AB$Pedigree = ifelse(is.na(BV.MC.Entry.data.AB$Pedigree),
                                        as.character(BV.MC.Entry.data.AB$Variety),
                                        BV.MC.Entry.data.AB$Pedigree)
  BV.MC.Entry.data.AB$Pedigree = ifelse(BV.MC.Entry.data.AB$Pedigree=="",
                                        as.character(BV.MC.Entry.data.AB$Variety),
                                        BV.MC.Entry.data.AB$Pedigree)

  BV.MC.Entry.data.AB$Pedigree = ifelse(BV.MC.Entry.data.AB$Pedigree=="",
                                        as.character(BV.MC.Entry.data.AB$Female.Pedigree),
                                        BV.MC.Entry.data.AB$Pedigree)
  BV.MC.Entry.data.AB$Pedigree = ifelse(is.na(BV.MC.Entry.data.AB$Pedigree),
                                        as.character(BV.MC.Entry.data.AB$Female.Pedigree),
                                        BV.MC.Entry.data.AB$Pedigree)

  BV.MC.Entry.data.AB$unique.Line.ID <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$unique.Line.ID)
  BV.MC.Entry.data.AB$Pedigree <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$Pedigree)

  ######################################################
  #import previous years peds and merge do this year
  ######################################################
  # shdSlk1519<-read.xlsx(paste0(oldfd),1)
  # shdSlk1519$InbName <- gsub("[[:space:]]", "", shdSlk1519$InbName)
  # shdSlk1519$Variety <- gsub("[[:space:]]", "", shdSlk1519$Variety)
  #
  # shdSlk1519$pedigree <- gsub("[[:space:]]", "", shdSlk1519$pedigree)
  #
  # shdSlk1519$pedigree = ifelse(shdSlk1519$pedigree=="",
  #                              as.character(shdSlk1519$InbName),
  #                              shdSlk1519$pedigree)
  #
  # shdSlk1519$pedigree = ifelse(is.na(shdSlk1519$pedigree),
  #                              as.character(shdSlk1519$InbName),
  #                              shdSlk1519$pedigree)
  # colnames(shdSlk1519)
  # colnames(BV.MC.Entry.data.AB)
  #
  # df1=data.frame(BV.MC.Entry.data.AB[,c(5,40)]); colnames(df1)= c("Line.ID","unique.Line.ID")
  # df2=data.frame(shdSlk1519[,c(9,7)]);colnames(df2)= c("Line.ID","unique.Line.ID")
  #

  #lines2020=rbind(df1, df2)
  lines2020=BV.MC.Entry.data.AB[,c(6,56)]
  colnames(lines2020)[1] = "Line.ID"

  ############################################################
  #Coded line import
  ############################################################
  to_search_in.female <- data.table(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique.Line.ID),c(56,6)])
  colnames(to_search_in.female)=c("unique","pedigree")

  to_search_with.female <- tibble(BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$unique.Line.ID),c(56)])
  colnames(to_search_with.female) = "unique_female_id"

  to_search_in.female.order = order(to_search_in.female$pedigree)
  to_search_in.female = to_search_in.female[to_search_in.female.order,]

  to_search_with.female.order = order(to_search_with.female$unique_female_id)
  to_search_with.female = to_search_with.female[to_search_with.female.order,]

  linked.peds = to_search_in.female
  linked.peds$pedigree = as.character(linked.peds$pedigree)

  linked.peds$pedigree = ifelse(linked.peds$pedigree=="",
                                as.character(linked.peds$unique),
                                linked.peds$pedigree)

  linked.peds$pedigree = ifelse(is.na(linked.peds$pedigree),
                                as.character(linked.peds$unique),
                                linked.peds$pedigree)

  linked.peds = linked.peds[!duplicated(linked.peds$unique), ]
  linked.peds$match = linked.peds$pedigree


  BV.MC.Inbred <- read.csv(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.csv"))
  BV.MC.Inbred$Pedigre_Backup = BV.MC.Inbred$PEDIGREE
  BV.MC.Inbred = BV.MC.Inbred[,c(1:3,22,4:21)]

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

  linked.peds.beck = to_search_with %>%
    mutate(data = list(to_search_in) ) %>%
    unnest(data) %>%
    mutate(unique_ped_id_nchar = nchar(unique_ped_id) ) %>%
    mutate(unique_nchar = nchar(unique) ) %>%
    mutate(unique_ped_id_DH = str_extract(digitDH, string = unique_ped_id) ) %>%
    #mutate(unique_ped_id_DH2 = gsub(".*?\\.(.*?)\\.*", x=unique_ped_id, value=T) ) %>%
    mutate(unique_DH = str_extract(digitDH, string=unique) ) %>%
    dplyr::filter(unique_nchar <= (unique_ped_id_nchar+15)  )

  linked.peds.beck2 = linked.peds.beck %>%
    mutate(unique_ped_id_DH_1 = ifelse((unique_ped_id_nchar < 10), as.matrix(grepl("\\/", x = unique )), F))

  linked.peds.beck3 =  linked.peds.beck2 %>%
    filter(  str_detect(unique, coll(unique_ped_id)  )  )  %>%
    dplyr::filter(unique_ped_id_DH_1 != TRUE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 =   linked.peds.beck3 %>%
    mutate(DH_Match = ifelse(unique_ped_id_DH == unique_DH , TRUE,FALSE) ) %>%
    mutate(DH_Match = ifelse(is.na(DH_Match), TRUE, DH_Match)) %>%
    dplyr::filter(DH_Match != FALSE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 = linked.peds.beck4 %>%
    select(Code,unique_ped_id,unique) %>%
    group_by(Code,unique) %>%
    summarise(strings = str_c(unique_ped_id, collapse = ", "))


  rm(linked.peds.beck, linked.peds.beck2, linked.peds.beck3)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck = linked.peds.beck4 %>% tidyr::separate("strings", sep="[, ][ ]",
                                                           c("match1", "match2","match3", "match4","match5", "match6","match7", "match8","match9", "match10","match11", "match12","match13", "match14" ,"match15", "match16"),
                                                           extra="merge",
                                                           remove=F)
  rm(linked.peds.beck4)


  linked.peds.beck = left_join(linked.peds.beck, BV.MC.Inbred[,c(2,3)], by=c("Code"="NEW.CODE"))
  linked.peds.beck = linked.peds.beck[,c(20,1:19)]



  linked.peds = left_join(linked.peds[, c(1,2,3)], linked.peds.beck[, c(2,3)], by = c("match"="unique"))
  #bind.linked.female.peds[[length(bind.linked.female.peds)+1]] = linked.female.peds
  # rm(linked.female.peds, to_search_with.batch)
  # }

  # linked.female.peds = rbindlist(bind.linked.female.peds)
  linked.peds$match = ifelse(!is.na(linked.peds$Code) , as.character(linked.peds$Code), linked.peds$match)

  #############################################################
  #############################################################
  #############################################################
  BV.MC.Inbred <- read.csv(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.csv"))
  BV.MC.Inbred$Pedigre_Backup = BV.MC.Inbred$PEDIGREE
  BV.MC.Inbred = BV.MC.Inbred[,c(1:3,22,4:21)]


  ###run pedigree reduction function
  #source("R:/Breeding/MT_TP/Models/R-Scripts/greplPeds.R")

  newData=pedigreeReduce(data=BV.MC.Inbred, Codes=T)

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

  linked.peds.beck = to_search_with %>%
    mutate(data = list(to_search_in) ) %>%
    unnest(data) %>%
    mutate(unique_ped_id_nchar = nchar(unique_ped_id) ) %>%
    mutate(unique_nchar = nchar(unique) ) %>%
    mutate(unique_ped_id_DH = str_extract(digitDH, string = unique_ped_id) ) %>%
    #mutate(unique_ped_id_DH2 = gsub(".*?\\.(.*?)\\.*", x=unique_ped_id, value=T) ) %>%
    mutate(unique_DH = str_extract(digitDH, string=unique) ) %>%
    dplyr::filter(unique_nchar <= (unique_ped_id_nchar+15)  )

  linked.peds.beck2 = linked.peds.beck %>%
    mutate(unique_ped_id_DH_1 = ifelse((unique_ped_id_nchar < 10), as.matrix(grepl("\\/", x = unique )), F))
  rm(linked.peds.beck); invisible(gc(reset=T))

  linked.peds.beck3 =  linked.peds.beck2 %>%
    filter(  str_detect(unique, coll(unique_ped_id)  )  )  %>%
    dplyr::filter(unique_ped_id_DH_1 != TRUE)
  rm(linked.peds.beck2)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 =   linked.peds.beck3 %>%
    mutate(DH_Match = ifelse(unique_ped_id_DH == unique_DH , TRUE, FALSE) ) %>%
    mutate(DH_Match = ifelse(is.na(DH_Match), TRUE, DH_Match)) %>%
    dplyr::filter(DH_Match != FALSE)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck4 = linked.peds.beck4 %>%
    select(Code,unique_ped_id,unique) %>%
    group_by(Code,unique) %>%
    summarise(strings = str_c(unique_ped_id, collapse = ", "))


  rm(linked.peds.beck, linked.peds.beck2, linked.peds.beck3)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  linked.peds.beck = linked.peds.beck4 %>% tidyr::separate("strings", sep="[, ][ ]",
                                                           c("match1", "match2","match3", "match4","match5", "match6","match7", "match8","match9", "match10","match11", "match12","match13", "match14" ,"match15", "match16"),
                                                           extra="merge",
                                                           remove=F)
  rm(linked.peds.beck4)


  linked.peds.beck = left_join(linked.peds.beck, BV.MC.Inbred[,c(2,3)], by=c("Code"="NEW.CODE"))
  linked.peds.beck = linked.peds.beck[,c(20,1:19)]
  colnames(linked.peds.beck)[2] = "CodeV2"


  linked.peds = left_join(linked.peds[, c(1,2,3,4)], linked.peds.beck[, c(2,3)], by = c("match"="unique"))
  #bind.linked.female.peds[[length(bind.linked.female.peds)+1]] = linked.female.peds
  # rm(linked.female.peds, to_search_with.batch)
  # }

  # linked.female.peds = rbindlist(bind.linked.female.peds)
  linked.peds$match = ifelse(!is.na(linked.peds$CodeV2) , as.character(linked.peds$CodeV2), linked.peds$match)
  linked.peds$match = ifelse(!is.na(linked.peds$Code) , as.character(linked.peds$Code), linked.peds$match)

  linked.peds$Code = ifelse(is.na(linked.peds$Code) , as.character(linked.peds$CodeV2), linked.peds$Code)




  # linked.peds = linked.peds[!duplicated(linked.peds$uniqued_id), ]


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

  linked.peds$match = as.character(linked.peds$match)

  ######################################################
  #match and nest linked pedigress for export and review
  ######################################################
  #if(doPedigreeChange){
  # to_search_in.line <- data.table(linked.peds[!duplicated(linked.peds$match),c(1,3)])
  # #colnames(to_search_in.female)=c("unique_female","female.pedigree")
  #
  # to_search_with.line <- tibble(linked.peds[!duplicated(linked.peds$match),c(3)])
  # colnames(to_search_with.line) = "unique_line"
  #
  # #dim(to_search_in.female);dim(to_search_in.male);dim(to_search_with.male);dim(to_search_with.female)
  #
  # linked.line.peds = to_search_with.line %>%
  #   mutate(data = list(to_search_in.line)) %>%
  #   unnest(data) %>%
  #   dplyr::filter(str_detect(unique_line, fixed(unique))) %>% #comparing with  to in
  #   #select(unique_female,female.pedigree,unique_female_id) %>%
  #   group_by(unique) %>% #should be the fixed effect
  #   summarise(strings = str_c(unique_line, collapse = ", "))
  #
  # linked.line.peds = linked.line.peds %>% tidyr::separate("strings", sep="[, ][ ]",
  #                                                         c("match1", "match2","match3", "match4","match5", "match6","match7", "match8","match9", "match10","match11", "match12","match13", "match14" ,"match15", "match16"),
  #                                                         extra="merge",
  #                                                         remove=F)
  # #destfile7 = paste0(dp,year,"/linked.lines.nestedpeds.xlsx")
  #
  # linked.line.peds$match1 = ifelse(linked.line.peds$match1=="",
  #                             as.character(linked.line.peds$uniqued.Line.ID),
  #                             linked.line.peds$match1)
  #


  #linked.peds$match = linked.peds$match1
  #linked.line.peds = linked.line.peds[,c(1,19,3,4:18)]

  #linked.line.peds$match <- gsub("[[:space:]]", "", linked.line.peds$match)
  linked.peds$match <- gsub("[[:space:]]", "", linked.peds$match)

  #source("R:/Breeding/MT_TP/Models/R-Scripts/BreedStats/R/InbredNameLibrary.R")
  patterns = InbredNameLibrary()
  patterns = patterns[[1]]
  #inbreds###################################################################
  linked.peds$match = as.character(linked.peds$match)
  match=str_detect(string = linked.peds$match, pattern = paste(patterns, collapse = "|"), negate=T)
  match=data.frame(match)
  for(i in 1:nrow(linked.peds)) {
    if(match[i,1]==TRUE){
      #print("yes")
      linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("-.*"), replacement="") # add a period for using wildcards
      linked.peds[i,3] = gsub(x=linked.peds[i,3], pattern=fixed("\\)"), replacement="") #escape parathesis with \\
    }
  }
  #########################################################################################################
  ###################################Merge data with adjusted lines########################################
  #########################################################################################################
  BV.MC.Entry.data.AB$Level = ""
  BV.MC.Entry.data.AB$Rep = ""
  BV.MC.Entry.data.AB$`Book.Season` = gsub(x=BV.MC.Entry.data.AB$`Book.Season`,pattern="S: Corn",replacement="", fixed=T)
  BV.MC.Entry.data.AB$`Book.Season` = as.numeric(BV.MC.Entry.data.AB$`Book.Season`) + 2000
  #BV.MC.Entry$Yr = ""
  #BV.MC.Entry$Loc = ""

  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[ ,c(57,58,14,3,7,6,24,21,49,50,9,8,10,44,20,22,23,25,42,28,
                                                27,41,33,45,46,48,37,47,1,21,24,36,38,40,39,43,31,32,34,
                                                35,51,54,52,53,55,56)]

  BV.MC.Entry.data.AB2020 = BV.MC.Entry.data.AB %>% dplyr::filter(Book.Season == 2020)
  #BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% dplyr::filter(Book.Season != 2020)

  # qualdatbyYear = read.csv(paste0(fdp))
  #
  # qualdatbyYear2020 = qualdatbyYear %>% dplyr::filter(Yr == 2020)
  # qualdatbyYear = qualdatbyYear %>% dplyr::filter(Yr != 2020)
  #
  # #qualday <- left_join(qualdatbyYear, BV.MC.Entry.data.AB[,c(11,12,4,10,41)],
  # #                     by = c("Range"="Range", "Row"="Row", "Loc"="Field..", "EBN"="Entry.Book.Name"))
  #
  #
  #
  # ##############################################
  # ##############################################
  # #qualdatbyYear <- qualdatbyYear %>% filter((Plot.Discarded) == "")
  # qualdatbyYear <- qualdatbyYear %>% dplyr::filter(EBN != "TP20S_Filler",
  #                                           EBN != "FILL",
  #                                           EBN != "Filler")
  #
  # dim(qualdatbyYear)
  #
  # # qualday <- left_join(qualdatbyYear, BV.MC.Entry.data.AB[,c(3,2,8,9,44,1)], by = c("Range"="Range", "Row"="Row", "Loc"="Book.Name","EBN"="Entry.Book.Name"))
  #
  #
  #
  #
  #

  empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
    ifelse(as.character(x)!="", x, NA)
  }
  #qualday$unique.Line.ID = ""
  #qualday$InbName <- gsub("[[:space:]]", "", qualday$InbName)
  #qualday$pedigree <- gsub("[[:space:]]", "", qualday$pedigree)
  # qualdatbyYear$unique.Line.ID = ""
  # qualdatbyYear = qualdatbyYear %>% mutate_each(funs(empty_as_na)) #make sure cells are na
  #
  # qualdatbyYear$unique.Line.ID = ifelse(is.na(qualdatbyYear$unique.Line.ID),
  #                                       as.character(qualdatbyYear$InbName),
  #                                       qualdatbyYear$unique.Line.ID)

  colNameRe = c(   "Level"                ,
                   "Rep"                  ,
                   "Plot.Discarded"       ,
                   "EBN"                  ,
                   "InbName"              ,
                   "Pedigree"             ,
                   "Shd_50"               ,
                   "Slk_50"               ,
                   "Yr"                   ,
                   "Loc"                  ,
                   "Range"                ,
                   "Row"                  ,
                   "Entry"                ,
                   "Brace.Root"           ,
                   "SLK10"                ,
                   "SLK90"                ,
                   "SHD10"                ,
                   "SHD90"                ,
                   "Ear.Stalk"            ,
                   "Plt.Height"           ,
                   "EarHt"                ,
                   "Tillers"              ,
                   "Leaf.Angle"           ,
                   "SL.Rating"            ,
                   "RL.Rating"            ,
                   "Comments"             ,
                   "Shed.Rating"          ,
                   "Stand.Count"          ,
                   "RecId"                ,
                   "SLK50"                ,
                   "SHD50"                ,
                   "Pollen.Duration..GDUs.10..to.90..",
                   "Silk.Color"           ,
                   "Tassel.Extension"     ,
                   "Tassel.Branches"      ,
                   "Anther.Color"         ,
                   "Glume.Color"          ,
                   "Glume.Ring"           ,
                   "Leaf.Color"           ,
                   "Leaf.Texture"         ,
                   "NCLB"                 ,
                   "GLS"                  ,
                   "NCLB.2"               ,
                   "GOSS"                 ,
                   "Variety",
                   "unique.Line.ID")

  colnames(BV.MC.Entry.data.AB) = colNameRe




  #qualday = rbind(qualdatbyYear, BV.MC.Entry.data.AB)
  qualday =  BV.MC.Entry.data.AB
  #qualday = data.frame(qualday)
  ######################################################################################################
  ###################################REVIEW LINKED AND NESTED PEDIGREES BEFORE CONTINUING#########
  #########################################################################################################
  #IMPORT REVIEWED MALE AND FEMALE PEDIGREES
  ##########################################
  #BV.MC.LINE<-read.xlsx2(paste0(dp, year, '/linked.lines.nestedpeds.update.xlsx'), 1, na="", header=TRUE)#all varieties to build the model

  #BV.MC.Female$unique_female_id <- gsub("[[:space:]]", "", BV.MC.Female$unique_female_id)
  #BV.MC.Male$unique_male_id <- gsub("[[:space:]]", "", BV.MC.Male$unique_male_id)

  BV.MC.LINE = linked.peds[,c(1,3)]
  #write.csv(BV.MC.Entry.data.AB,"")
  ######create male and female merge reduced and line_id list to us

  rm(InbName.checknames,BV.MC.Entry.data.AB.checkfilter.InbName.duplciate,BV.MC.Entry.data.AB.checkfilter.InbName,
     BV.MC.Entry.data.AB.checkfilter,BV.MC.Female.Line_ID,BV.MC.Female.Line_ID.entry,BV.MC.Female.reduced,BV.MC.Female.reduced.entry,
     BV.MC.Male.Line_ID,BV.MC.Male.Line_ID.entry,BV.MC.Male.reduced,BV.MC.Male.reduced.entry,
     to_search_in.female,to_search_in.male,to_search_with.female,to_search_with.male,linked.female.peds,linked.male.peds)
  rm(BV.MC.Entry.filterA, BV.MC.Entry.filterB)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  qualday$unique.Line.ID <- suppressWarnings(suppressMessages(plyr::revalue(as.character(qualday$unique.Line.ID), c('65125'=	'065125',' 65125'=	'065125', '56460'=	'056460', '54530'=	'054530', '56460' = '056460',
                                                                                                                    '54530'=	'054530', '54245'=	'054245', '54233'=	'054233','46358'=	'046358',
                                                                                                                    '46358'=	'046358', '37186'=	'037186', '26507'=	'026507','26076'=	'026076',
                                                                                                                    '16360'=	'016360', '16360'=	'016360', '16088'=	'016088','16088'=	'016088', '50'='050' ))))

  lines2020 <- left_join(qualday, BV.MC.LINE, by=c("unique.Line.ID"="unique"),keep=F);dim(lines2020)



  #write.csv(lines2020,"P:/Temp/lines2020.csv" ,row.names=F)

  #check for missing male or female pedigree the can
  #BV.MC.Entry.data.AB.forReview = BV.MC.Entry.data.AB[,-c(3,5,6,32,33,14,15,8,9)]
  #lines2020.forReview =lines2020[is.na(lines2020$match1.1),]
  #BV.MC.Entry.data.AB.forReview.female=BV.MC.Entry.data.AB.forReview.female[!duplicated(BV.MC.Entry.data.AB.forReview.female$Pedigree),]
  #write.csv(BV.MC.Entry.data.AB.forReview,"R:/Breeding/MT_TP/Models/Data/Department Data/2020/BV.MC.Entry.data.AB.forReview.csv")

  #wb<-createWorkbook(type="xlsx")
  #CellStyle(wb, dataFormat=NULL, alignment=NULL,
  #          border=NULL, fill=NULL, font=NULL)
  #Male <- createSheet(wb, sheetName = "Male")
  #Female <- createSheet(wb, sheetName = "Female")
  #addDataFrame(data.frame(BV.MC.Entry.data.AB.forReview.female),Female, startRow=1, startColumn=1,row.names=F)
  #addDataFrame(data.frame(BV.MC.Entry.data.AB.forReview.male),Male, startRow=1, startColumn=1,row.names=F)
  #saveWorkbook(wb, "R:/Breeding/MT_TP/Models/Data/Department Data/2020/BV.MC.Entry.data.AB.forReview.xlsx")
  rm(BV.MC.Entry, BV.MC.Entry.data, BV.MC.Entry.data.AB, BV.MC.LINE, df1, df2, linked.line.peds, qualdatbyYear,
     to_search_in.line, to_search_with.line, shdSlk1519, qualday)
  invisible(gc(reset=T)) #cleans memory "garbage collector"
  #colnames(lines2020)[16] = "LINE"
  colnames(lines2020)[47] = "LINE"


  #qualdat<-read.xlsx('R:/Breeding/MT_TP/Models/Data/Shd_slk/tblSlkShdGDUs.xlsx',1, header=T)#all varieties to build the model
  #tblFlowerData_JL.xlsx
  #levels(qualdat$InbName)#LEvels before
  ######convert Industry names to Becks names
  industryNames = InbredNameLibrary()
  industryNames = industryNames[[2]]

  lines2020$LINE <- suppressWarnings(suppressMessages(plyr::revalue(as.character(lines2020$LINE), industryNames)))
  #industry name to inbred name conversion
  #levels(as.factor(qualdat$InbName)) #Levels After
  #levels(as.factor(qualdat$loc))
  #write.csv(qualdat,"qualdat.csv")
  #qualdat <- qualdat[-c(3430,3428,3429,3427),] #may need to change these rows if qualdat was sorted prior; removed 2116 datapoints (not true data?)
  qual.count<-data.table(lines2020)#convert to a data.table for easier processing
  #z<-stat.desc(lines2020) # get some stats on dataset
  counts <- qual.count[, .(rowCount = .N), by = LINE]; colnames(counts)=c("LINE","Observations")  #counts observations per InbName
  #counts.loc<-qual.count[, .(rowCount = .N), by = c("InbName","loc","yr")] #counts observations per InbName

  #qualdat.singles <- counts %>% dplyr::filter(Observations!=1);colnames(qualdat.singles) = c("LINE", "Observations") #filter only single values for model buliding
  #head(counts);tail(counts) #count the number of observations per InbName


  ######Check to ensure data imported correctly
  #str(lines2020)
  #head(lines2020)
  #tail(lines2020)

  ######Examine distribution of of all the data first, will do more in depth distributions later
  ######Remove observations that are not true data...
  attach(lines2020) # attached dataset so I dont need to address the dataset in the functions
  hist(Shd_50 , col="gold") #distrubiton on all values
  hist(Slk_50, col="green") #distrubtion on all values
  boxplot(Shd_50~Level, xlab="Trial", ylab="Degrees obs_shd", main="Degrees obs_shd by Trial", col="pink")
  boxplot(Slk_50~Level, xlab="Trial", ylab="Degrees obsSLK", main="Degrees obsSLK by Trial", col="pink")
  boxplot(Shd_50~Yr, xlab="yr", ylab="Degrees obs_shd", main="Degrees obs_shd by yr", col="pink")
  boxplot(Slk_50~Yr, xlab="yr", ylab="Degrees obsSLK", main="Degrees obsSLK by yr", col="pink")
  boxplot(Shd_50~Loc, xlab="Location", ylab="Degrees obs_shd", main="Degrees obs_shd by Location", col="pink")
  boxplot(Slk_50~Loc, xlab="Location", ylab="Degrees obsSLK", main="Degrees obsSLK by Location", col="pink")

  ######Rename variables for ease of use
  #lines2020$GDU_slk50 = as.numeric(Slk_50)
  #lines2020$GDU_shd50 = as.numeric(Shd_50)
  lines2020$FIELD = as.factor(Loc)
  lines2020$YEAR = as.factor(Yr)
  lines2020$REP = as.factor(Rep)
  lines2020$EXP = as.factor(EBN)
  lines2020$LINE = as.factor(LINE)

  qualdat.mt <- lines2020 %>% dplyr::filter(Loc == "Marshalltown") #filter only marshalltown lines
  qualdat.olivia <- lines2020 %>% dplyr::filter(Loc == "Olivia") #filter only olivia lines
  qualdat.atlanta <- lines2020 %>% dplyr::filter(Loc == "Atlanta") #filter only olivia lines

  # require(agricolae)
  # aggregate(lines2020[,c("Slk_50","Shd_50")], by=list(FIELD=lines2020$FIELD),skewness)#calculate skewness for each effect

  #levelstodrop<-c("Alanta")#drop any location variables,
  #for(i in levelstodrop){qualdat <- droplevels(qualdat[grep( i,qualdat[ ,"loc"], invert=TRUE), ] )}

  detach(lines2020)
  ##############################
  linesYear = lines2020 %>% filter( between(as.numeric(as.character(YEAR)), l_year, h_year))
  #linesreploc = lines2020 %>% dplyr::filter(YEAR == Current)


  qualdat.df = linesYear #choose your dataset for the model
  #write.csv(qualdat.df,"PropandChoice.csv",na="")
  #write.csv(qualdat.df,"R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/SHD_SLK_ALLYears.csv", row.names=F)
  ######################################################################################################
  ######Calculate variance components
  ######Linear Model with random effects for variance components
  ######################################################################################################
  #str(lines2020)
  l<-length(qualdat.df); l
  names<-names(qualdat.df[ ,c(7,8,14,15,16,17,18,19,20,21,22,23,27,28,32,33,34,35,36,37,38,39,40,41,44)]); names
  classes<-sapply(qualdat.df[ ,c(7,8,14,15,16,17,18,19,20,21,22,23,27,28,32,33,34,35,36,37,38,39,40,41,44)],class); classes

  qualdat.order.index = order(qualdat.df$RecId)
  qualdat.df=qualdat.df[qualdat.order.index,]


  attach(qualdat.df)

  lexp=levels(EXP); lexp=data.frame(lexp); lexp$c = c(1:nrow(lexp))
  lfield=levels(FIELD); lfield=data.frame(lfield); lfield$c = c(1:nrow(lfield))
  lyear=levels(YEAR); lyear=data.frame(lyear); lyear$c = c(1:nrow(lyear))
  line=levels(LINE); line=data.frame(line); line$c = c(1:nrow(line))

  qualdat.df = left_join(qualdat.df, lexp,   by = c("EXP"="lexp"))
  qualdat.df = left_join(qualdat.df, lfield, by = c("FIELD"="lfield"))
  qualdat.df = left_join(qualdat.df, lyear,  by = c("YEAR"="lyear"))
  qualdat.df = left_join(qualdat.df, line,   by = c("LINE"="line"))
  colnames(qualdat.df)[52:55] = c("expn","fieldn","yearn","linen")
  qualdat.df$FieldBook = paste0(qualdat.df$fieldn,"_",qualdat.df$yearn,"_",qualdat.df$expn)

  FB=levels(as.factor(qualdat.df$FieldBook))

  detach(qualdat.df)
  qualdat.df = data.frame(qualdat.df)
  qualdat.df$REP = as.numeric(qualdat.df$REP)
  for(i in FB){
    x = subset(qualdat.df, FieldBook == i)
    xd = x[!duplicated(x$linen),]
    index.line = levels(as.factor(xd$linen))
    for(s in index.line){
      xl = subset(x, linen == s)
      xl$REP = c(1:nrow(xl))

      xls = levels(as.factor(xl$RecId))
      for(m in xls){
        xlsx = subset(xl, RecId == m)
        matchRec =xlsx$RecId
        matchREP = xlsx$REP

        qualdat.df <- within(qualdat.df, REP[ RecId ==matchRec] <- matchREP)

        rm(matchRec,matchREP)
      }
    }
  }


  ##############################
  ######use a for loop to do mulitple traits at once
  #names<-names(lines2020[c("GDU_slk50","GDU_shd50")]); names
  #classes<-sapply(lines2020[c("GDU_slk50","GDU_shd50")],class); classes
  cat("----------------------------Adjusting Inbred Data----------------------------", "\n")

  # name="SLK10"
  rm(BV.MC.Entry.data.AB2020, BV.MC.Inbred, lexp, lfield, line, lines2020, linked.peds, linked.peds.beck,
     linked.peds.save, lyear, match, newData, qualdat.atlanta, qualdat.mt, qualdat.olivia, to_search_in, to_search_with,
     x, xd, xl, xlsx)
  gc()
  #DS = 2020
  if(doAdjInbredData){
    sink(file=paste0(dpf,"SHDSLK_",season,"S.field.txt"),split=TRUE)
    pdf(file = paste0(dpf,"SHDSLK_",season,"S.field.pdf"), paper="special",width = 8.5, height = 11,
        family="Times", pointsize=11,bg="white",fg="black")
    for(name in names){
      #name="GDU_slk50"
      qualdat.df = data.frame(qualdat.df)
      SP_data.filter = cbind(qualdat.df, qualdat.df[,name])
      BV.l=length(SP_data.filter)
      colnames(SP_data.filter)[[BV.l]] = "feature"
      SP_data.filter=data.frame(SP_data.filter)
      SP_data.filter = transform(SP_data.filter,
                                 #male   = factor(male),
                                 #female = factor(female),
                                 feature = (feature),
                                 LINE = factor(LINE),
                                 FIELD=factor(FIELD),
                                 YEAR=factor(YEAR),
                                 EXP=factor(EXP),
                                 REP = factor(REP)
      )

      dim(SP_data.filter)
      #SP_data.filter = SP_data.filter %>% dplyr::filter(feature > 0)
      #dim(SP_data.filter);head(SP_data.filter)

      SP_data.filter.counts<-data.table(SP_data.filter)#convert to a data.table for easier processing
      counts <- SP_data.filter.counts[, .(rowCount = .N), by = LINE]; colnames(counts)=c("LINE","Observations")  #counts observations per InbName

      #######################################
      ######Graphs and Figures
      #######################################

      #head(SP_data.filter.gmean)
      #dim(SP_data.filter.gmean)
      #head(SP_data.filter)
      #dim(SP_data.filter)
      #str(BV.HSIdentical_lables)
      #str(BV_data.filter)
      #######################################
      ######Run the model
      #######################################
      #trait1 <- paste0(name,"~","FIELD") #Set the model
      #QUALDAT = lmer(trait1, qualdat.data, REML = T) #run the model
      #BV.ped<-SP_data.filter[,c(3,4,5)]
      #index <- which(duplicated(BV.ped$LINE))
      #BV.ped <- BV.ped[-index,]
      #ainv<-ainverse(pedigree=BV.ped)
      rm(df3,df3.gmean,df5,ear_height,gs_early,gs_late,pct_hoh,plot_wt,plt_height,qual.count,QUALDAT.SUM,rl_early_count,rl_percent,
         sl_cound,sl_percent,stand_cnt_early,stant_cnt_final,test_wt,Y_m,yield,BV,BV,
         BV.EC,BV.EC.filter,BV.EC.filter.check,BV.filter.Var,BV.MC,BV.MC.filter.check,BV.MC.filter,BV.ped,BV.Var,z,
         BV.check,index)
      invisible(gc(reset=T)) #cleans memory "garbage collector"
      cat(paste0("----------------------------",name,"----------------------------"), "\n")

      if(name == "Glume.Ring" || name == "Leaf.Color" || name == "Leaf.Texture" || name == "Brace.Root" || name == "Silk.Color" ||
         name == "Anther.Color" || name == "Glume.Color" ){


        SP_data.filter$feature = as.character(SP_data.filter$feature)
        if(name== "Glume.Ring" || name =="Leaf.Color" || name == "Leaf.Texture"){
          SP_data.filter[is.na(SP_data.filter)] <- " "
        }
        SP_data.filter.feature = SP_data.filter[,c("LINE","feature")]
        SP_data.filter.feature  = na.omit(SP_data.filter.feature )
        SP_data.filter.index = order(SP_data.filter.feature $feature)
        SP_data.filter.feature  = SP_data.filter.feature [SP_data.filter.index, ]
        BV.HSIdentical.model.predicted = SP_data.filter.feature  %>% distinct() %>% group_by(LINE) %>%
          mutate(mean=paste0(feature , collapse=" "))

        colnames(BV.HSIdentical.model.predicted)[2] = "g_mean"
        # BV.HSIdentical.model.predicted$std_err = ""
        BV.HSIdentical.model.predicted$mean.char = BV.HSIdentical.model.predicted$mean

        if(name == "Glume.Ring"){
          BV.HSIdentical.model.predicted$mean.char = gsub(BV.HSIdentical.model.predicted$mean.char, replacement = "", pattern = " ")
          BV.HSIdentical.model.predicted$mean.char = ifelse(BV.HSIdentical.model.predicted$mean.char == "AbsentPresent",
                                                            "Absent/Present",
                                                            BV.HSIdentical.model.predicted$mean.char)
          BV.HSIdentical.model.predicted$mean.char = ifelse(BV.HSIdentical.model.predicted$mean.char == "PresentAbsent",
                                                            "Absent/Present",
                                                            BV.HSIdentical.model.predicted$mean.char)

          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Absent", replacement = "Absent")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Present", replacement ="Present")


        }
        if(name == "Leaf.Color"){
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Dark Light Medium Very Dark", replacement = "Light, Dark, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Dark Light Very Dark", replacement = "Light, Dark, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Dark Medium Very Dark", replacement = "Dark, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Dark Very Dark", replacement = "Dark, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Light Medium Very Dark", replacement = "Light, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Light Very Dark", replacement = "Light, Very Dark")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Medium Very Dark", replacement = "Very Dark")
        }

        if(name == "Leaf.Texture"){
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Hairy, Wide", replacement ="Hairy, Medium Width, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Smooth Smooth, Medium Width", replacement ="Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal, wide Normal, wider Smooth, Medium Width Smooth, Wide", replacement ="Smooth, Medium Width, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal Smooth, Medium Width Smooth, wider", replacement ="Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal Smooth, Medium Width Smooth, Narrow", replacement ="Smooth, Narrow, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal Smooth, Medium Width", replacement ="Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal Smooth Smooth, Medium Width", replacement ="Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Normal Rough, Medium Width Smooth, Medium Width", replacement ="Rough, Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Narrow Normal", replacement ="Hairy, Narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Smooth Smooth, Medium Width Smooth, wide", replacement ="Hairy, Medium Width, Smooth, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Normal Smooth, Medium Width", replacement ="Hairy, Medium Width, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Normal Smooth", replacement ="Hairy, Medium Width, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Normal Normal, wide", replacement ="Hairy, Medium Width, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Normal", replacement ="Hairy, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Hairy, Wide Normal, wide Smooth, wider", replacement ="Hairy, Medium Width, Wide, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Hairy, Wide Normal Normal, wide", replacement ="Hairy, Medium Width, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Hairy, Wide Normal", replacement ="Hairy, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Hairy, Medium Width Hairy, Narrow Normal", replacement ="Hairy, Narrow, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width, Wide Normal, wide Smooth Smooth, wider Wide, smooth", replacement ="Hairy-Smooth, Wide, Normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width Hairy, Narrow Narrow smooth Normal Smooth", replacement ="Hairy-Smooth, Narrow-Medium Width, Normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width Hairy, Narrow Narrow smooth Normal Rough,  Rough, straight Smooth", replacement ="Hairy, Rough-Smooth, Narrow, Straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width, Wide Normal Rough Rough,  Smooth", replacement ="Hairy-Smooth, Medium Width, Wide, Rough")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width, Wide Normal, wide Rough,  Rough, wide Smooth Smooth, wider Wide, smooth", replacement ="Rough-Smooth, Medium Width, Wide, Normal, Hairy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width Smooth Smooth, wavy Smooth, wavy, wide Smooth, wide", replacement ="Hairy, Medium Width, Smooth, Wavy, Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal, wide Normal, Smooth, Medium Width Smooth, wavy", replacement ="Normal, Wide, Smooth, Wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" wavy Wavy", replacement ="Wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wavy Wavy, smooth", replacement ="Smooth, Wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Straight, smooth Straight, smooth, normal", replacement ="Smooth, Normal, Straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, straight, smooth", replacement ="Smooth, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, straight Smooth,wavy", replacement ="Smooth, Straight, Wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, narrow straight, smooth", replacement ="Smooth, narrow, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, Medium Width Smooth, wavy", replacement ="Smooth, medium width, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth,  Smooth, normal", replacement ="Smooth, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Wide, smooth", replacement ="Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Wavy, smooth", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth wavy Wavy smooth", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Straight, smooth", replacement ="Smooth, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wide, wide", replacement ="Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wide, wavy", replacement ="Smooth, wide, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wide Smooth, wide, wavy Wide, smooth", replacement ="Smooth, wide, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wide", replacement ="Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wavy Very smooth", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, wavy Smooth, wide Wide", replacement ="Smooth, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" smooth Smooth, wavy", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, Straight Smooth, wavy", replacement ="Smooth, straight, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" smooth Smooth,", replacement ="Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" smooth Smooth", replacement ="Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth normal Smooth, ", replacement ="Smooth, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wide, wavy Wide, Rough", replacement ="Rough, wide, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wide Wide, smooth", replacement ="Rough-Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wide Straight, Rough", replacement ="Rough, wide, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wide Smooth, wide", replacement ="Rough-Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wavy Wavy, smooth", replacement ="Rough, wavy, smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wavy Wavy, Rough", replacement ="Rough, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, rough, wavy, wide Rough, wavy Smooth Wide, smooth", replacement ="Rough-smooth, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, Rough, Rough Smooth", replacement ="Rough, smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, narrow Smooth, narrow", replacement ="Rough-smooth, narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, narrow Rough, wavy", replacement ="Rough, narrow, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, narrow Rough, normal", replacement ="Rough, narrow, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Wide, Rough", replacement ="Rough, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Straight, wide, Rough", replacement ="Rough, straight, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Straight, Rough", replacement ="Rough, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Sooth", replacement ="Rough, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Smooth", replacement ="Rough, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Rough", replacement ="Rough")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal, wide Normal, wider Smooth, Medium Width Smooth, wavy", replacement ="Smooth, wide, wavy, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal Rough Rough, Medium Width Smooth, Medium Width", replacement ="Rough-smooth, wide, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow,Rough, straight Rough, narrow, straight", replacement ="Rough, narrow, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, Straight, Rough Rough, narrow", replacement ="Rough, narrow, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width, Wide Normal Normal, wide", replacement ="Normal, wide, hairy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width, Wide", replacement ="Wide, hairy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width Smooth, Medium Width", replacement ="Wide, hairy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, normal, wavy Smooth, wavy Very smooth", replacement ="Smooth, normal, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth, Rough", replacement ="Smooth, Rough")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Wide, hairy Normal", replacement ="Wide, hairy, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Smooth straight", replacement ="Smooth, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Rough, Smooth", replacement ="Rough-Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Rough, RoughSmooth, wide", replacement ="Rough-Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Wide, rough Wide, smooth", replacement ="Wide, rough, smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium Width Hairy, Narrow", replacement ="Hairy, Medium Width, Narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium WidthRough, Smooth", replacement ="Hairy, Medium Width,Rough-Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, rough Smooth, wavy", replacement ="Rough-Smooth, wavy, narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, smooth Rough, narrow", replacement ="Smooth-Rough, narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, smooth Rough, normal", replacement ="Smooth-Rough, normal, narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, smooth Straight, smooth ", replacement ="Narrow, straight, smooth ")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, smoothRough, straight Smooth", replacement ="Narrow, smooth-rough, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, Straight Rough", replacement ="Narrow, straight, Rough")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, straight, rough Rough, wavy", replacement ="Narrow, straight, Rough, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow, straight,Rough, Smooth, ", replacement ="Narrow, straight, Rough, Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Narrow,Rough", replacement ="Narrow, rough")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal Smooth Smooth,  Smooth, Medium Width", replacement ="Normal, Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal, wide Normal, widerSmooth, medium width, wavy", replacement ="Normal, wide, smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal,Rough, Smooth, Smooth,  Wide, smooth", replacement ="Normal, Rough-Smooth,  Wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" NormalRough, Medium Width Smooth, Medium Width", replacement ="Normal, Rough-Smooth, Medium Width")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, rough, wavy, wide Rough, wavySmooth, wide", replacement =" Rough-Smooth, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, rough, wide Straight, Rough", replacement =" Rough, wide, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Rough, straight ", replacement =" Rough, straight ")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Smooth, narrow, straight", replacement =" Rough, straight, Smooth, narrow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" SmoothWavy smooth", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth,Wavy, smooth", replacement ="Smooth, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wide, wavy Wide, Rough", replacement ="Smooth-Rough, wide, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wide,  Wide, Rough", replacement ="Smooth-Rough, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wide Wide, smooth", replacement ="Smooth, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wide Wide, Rough", replacement ="Smooth-Rough, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wide Straight, Rough", replacement ="Smooth-Rough, wide, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth, wavy Wide, smooth", replacement ="Smooth, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Smooth Smooth", replacement ="Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough,Wavy, Rough", replacement ="Rough, Wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wide Smooth, normal", replacement ="Rough-Smooth, wide, normal")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wavy Wide, Rough", replacement ="Rough, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wavy Straight, smooth", replacement =" Rough-Smooth, wavy, straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, wavy Rough, wide", replacement ="Rough, wavy, wide")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="straight Straight", replacement ="straight")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Smooth, straight", replacement ="Rough-Smooth, straight")

          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Wide, smooth", replacement ="Rough, wide, smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Wavy, smooth", replacement =" Rough, wavy, smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough Wavy, Rough", replacement =" Rough, wavy")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Hairy, Medium WidthRough-Smooth", replacement ="Hairy, Medium Width, Rough-Smooth")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Normal,Rough-Smooth, Smooth,  Wide, smooth", replacement ="Normal, Rough-Smooth, Wide")
          # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Smooth, straight", replacement ="Rough-Smooth, straight")
          # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Smooth, straight", replacement ="Rough-Smooth, straight")
          # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern=" Rough, straight Smooth, straight", replacement ="Rough-Smooth, straight")
          #
          #
          #
        }




        if(name == "Brace.Root"){
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "1",
                                                          replacement = "Green")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "2",
                                                          replacement = "Striped Green")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "3",
                                                          replacement = "Striped Green and Purple")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "4",
                                                          replacement = "Purple")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "9",
                                                          replacement = "Seg")

          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Green Striped Green",replacement =  "Green, Striped Green")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Green Striped Green and Purple",replacement =  "Green, Striped Green and Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Green Striped Green and Purple Purple", replacement = "Green, Striped Green and Purple, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Green Striped Green Purple",replacement =  "Green, Striped Green, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Green Striped Green Striped Green and Purple", replacement = "Green, Striped Green, Striped Green and Purple")
          BV.HSIdentical.model.predicted$mean.char = gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Striped Green and Purple Purple", replacement = "Striped Green and Purple, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Striped Green Purple",replacement =  "Striped Green, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Striped Green Striped Green and Purple", replacement = "Striped Green, Striped Green and Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Striped Green Striped Green and Purple Purple" ,replacement = "Striped Green, Striped Green and Purple, Purple")


        }

        if(name == "Silk.Color" || name == "Anther.Color"){
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "1",
                                                          replacement = "Green")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "2",
                                                          replacement = "Yellow")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "3",
                                                          replacement = "Pink")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "4",
                                                          replacement = "Purple")
          BV.HSIdentical.model.predicted$mean.char = gsub(x = BV.HSIdentical.model.predicted$mean.char, pattern = "9",
                                                          replacement = "Seg")

          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Pink",replacement = "Green, Pink" )
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Pink Purple",replacement = "Green, Pink, Purple" )
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Purple",replacement =  "Green, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Yellow",replacement =  "Green, Yellow")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Yellow Pink",replacement =  "Green, Yellow, Pink")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Yellow Pink Purple",replacement =  "Green, Yellow, Pink, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Green Yellow Purple",replacement =  "Green, Yellow, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Pink Purple",replacement =  "Pink, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Yellow Pink",replacement =  "Yellow, Pink")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Yellow Pink Purple",replacement =  "Yellow, Pink, Purple")
          BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Yellow Purple",replacement =  "Yellow, Purple")

        }







        #BV.HSIdentical.model.predicted = gsub(1,"",BV.HSIdentical.model.predicted$mean)
        df8=left_join(BV.HSIdentical.model.predicted[,c(1,4,2)],counts,by="LINE"); colnames(df8)=c("LINE",paste0(name),paste0(name,"_std.error"), paste0(name,"_Observations"))
        df8 = df8[!duplicated(df8$LINE), ]

        assign(paste0(name), df8)

        rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
           SP_data.filter.gmean, SP_data.filter.feature, lines)
        gc()
        next

      }

      if(name == "Ear.Stalk" ||  name == "Stand.Count" || name == "Tillers" ||
         name == "Pollen.Duration..GDUs.10..to.90.."){
        SP_data.filter$feature =  as.numeric(SP_data.filter$feature)
        SP_data.filter.gmean = aggregate(SP_data.filter[,c("feature")], by=list(LINE = SP_data.filter$LINE),mean,na.rm=T); colnames(SP_data.filter.gmean) = c("LINE","g_mean"); SP_data.filter.gmean=data.frame(SP_data.filter.gmean)
        BV.HSIdentical.model.predicted = SP_data.filter.gmean
        BV.HSIdentical.model.predicted$mean = SP_data.filter.gmean$g_mean
        BV.HSIdentical.model.predicted$mean.coef = SP_data.filter.gmean$g_mean

        sigfigs.traits<-c("mean","g_mean")
        for(i in sigfigs.traits){BV.HSIdentical.model.predicted[,i]<-round(BV.HSIdentical.model.predicted[,i] ,0)}
        for(i in sigfigs.traits){BV.HSIdentical.model.predicted[,i]<-as.numeric(BV.HSIdentical.model.predicted[,i])}

      }

      if(name == "Tassel.Extension"){
        SP_data.filter$feature =  as.numeric(SP_data.filter$feature)

        QUALDAT = asreml(fixed = feature ~ YEAR + FIELD ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE ,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)
        BV.HSIdentical.model.predicted$predicted.value = round(BV.HSIdentical.model.predicted$predicted.value, 0)

        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "1",
                                                              replacement = "Short")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "2",
                                                              replacement = "Short-Medium")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "3",
                                                              replacement = "Medium")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "4",
                                                              replacement = "Medium-Long")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "5",
                                                              replacement = "Long")

        df8=left_join(BV.HSIdentical.model.predicted[,c(1:3)],counts,by="LINE"); colnames(df8)=c("LINE",paste0(name),paste0(name,"_std.error"), paste0(name,"_Observations"))
        df8$Tassel.Extension = gsub(x = df8$Tassel.Extension , pattern = "NaN",
                                    replacement = "")

        assign(paste0(name), df8)

        rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
           SP_data.filter.gmean, SP_data.filter.feature, lines)
        gc()

        next
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Medium-Long Long", replacement = "Medium-Long, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Medium Long", replacement = "Medium, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Medium Medium-Long", replacement =  "Medium, Medium-Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Medium Medium-Long Long", replacement = "Medium, Medium-Long, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Short-Medium Long", replacement = "Short-Medium, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short-Medium Medium", replacement = "Short-Medium, Medium")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short-Medium Medium-Long", replacement = "Short-Medium, Medium-Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Long", replacement = "Short, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium", replacement = "Short, Medium")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium-Long", replacement = "Short, Medium-Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium-Long Long", replacement = "Short, Medium-Long, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium Long", replacement = "Short, Medium, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium Medium-Long", replacement = "Short, Medium, Medium-Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Medium Medium-Long Long", replacement = "Short, Medium, Medium-Long, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Short-Medium", replacement = "Short, Short-Medium")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Short-Medium Long", replacement = "Short, Short-Medium, Long")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern="Short Short-Medium Medium", replacement = "Short, Short-Medium, Medium")
        #

      }

      if(name == "Shed.Rating" ){
        SP_data.filter$feature =  as.numeric(SP_data.filter$feature)

        QUALDAT = asreml(fixed = feature ~ YEAR ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE + FIELD*LINE ,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)
        BV.HSIdentical.model.predicted$predicted.value = round(BV.HSIdentical.model.predicted$predicted.value, 0)


        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "1",
                                                              replacement = "No Shed")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "2",
                                                              replacement = "No Shed-Normal Shed")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "3",
                                                              replacement = "Normal Shed")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "4",
                                                              replacement = "Normal Shed-Heavy Shed")
        BV.HSIdentical.model.predicted$predicted.value = gsub(x = BV.HSIdentical.model.predicted$predicted.value, pattern = "5",
                                                              replacement = "Heavy Shed")
        df8=left_join(BV.HSIdentical.model.predicted[,c(1:3)],counts,by="LINE"); colnames(df8)=c("LINE",paste0(name),paste0(name,"_std.error"), paste0(name,"_Observations"))
        df8$Shed.Rating = gsub(x = df8$Shed.Rating , pattern = "NaN",
                               replacement = "")

        assign(paste0(name), df8)

        rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
           SP_data.filter.gmean, SP_data.filter.feature, lines)
        gc()
        next
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed-Normal Shed Normal Shed",replacement ="No Shed-Normal Shed, Normal Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed-Normal Shed Normal Shed Heavy Shed",replacement ="No Shed-Normal Shed, Normal Shed, Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed-Normal Shed Normal Shed Normal Shed-Heavy Shed",replacement ="No Shed-Normal Shed, Normal Shed, Normal Shed-Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed Normal Shed",replacement ="No Shed, Normal Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed Normal Shed Heavy Shed",replacement ="No Shed, Normal Shed, Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed Normal Shed Normal Shed-Heavy Shed",replacement ="No Shed, Normal Shed, Normal Shed-Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Normal Shed-Heavy Shed Heavy Shed",replacement ="Normal Shed-Heavy Shed, Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Normal Shed Heavy Shed",replacement ="Normal Shed, Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Normal Shed Normal Shed-Heavy Shed",replacement ="Normal Shed, Normal Shed-Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "Normal Shed Normal Shed-Heavy Shed Heavy Shed",replacement = "Normal Shed, Normal Shed-Heavy Shed, Heavy Shed")
        # BV.HSIdentical.model.predicted$mean.char =gsub(BV.HSIdentical.model.predicted$mean.char, pattern= "No Shed-Normal Shed, Normal Shed, Normal Shed-Heavy Shed",replacement = "No Shed-Heavy Shed")
        #
      }

      if(name == "Slk_50" || name == "Shd_50"){
        QUALDAT = asreml(fixed = feature ~ YEAR + FIELD ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE ,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)

        #BV.HSIdentical.model.predicted.blupse = summary(QUALDAT, coef=TRUE)$coef.random
        #BV.HSIdentical.model.predicted.blupse = BV.HSIdentical.model.predicted.blupse[,c(2,3)]
        #BV.HSIdentical.model.predicted = cbind(BV.HSIdentical.model.predicted, BV.HSIdentical.model.predicted.blupse)

      }

      if(name == "NCLB" ){

        QUALDAT = asreml(fixed = feature ~ YEAR ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE + REP*LINE + REP*YEAR ,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)
        BV.HSIdentical.model.predicted$predicted.value = round(BV.HSIdentical.model.predicted$predicted.value, 0)

        BV.HSIdentical.model.predicted$predicted.value = ifelse(BV.HSIdentical.model.predicted$predicted.value == "1", "Good", BV.HSIdentical.model.predicted$predicted.value)
        BV.HSIdentical.model.predicted$predicted.value = ifelse(BV.HSIdentical.model.predicted$predicted.value == "2", "Good-Average", BV.HSIdentical.model.predicted$predicted.value)
        BV.HSIdentical.model.predicted$predicted.value = ifelse(BV.HSIdentical.model.predicted$predicted.value == "3", "Average", BV.HSIdentical.model.predicted$predicted.value)
        BV.HSIdentical.model.predicted$predicted.value = ifelse(BV.HSIdentical.model.predicted$predicted.value == "4", "Average-Bad", BV.HSIdentical.model.predicted$predicted.value)
        BV.HSIdentical.model.predicted$predicted.value = ifelse(BV.HSIdentical.model.predicted$predicted.value == "5", "Bad", BV.HSIdentical.model.predicted$predicted.value)
        #BV.HSIdentical.model.predicted.blupse = summary(QUALDAT, coef=TRUE)$coef.random
        #BV.HSIdentical.model.predicted.blupse = BV.HSIdentical.model.predicted.blupse[,c(2,3)]
        #BV.HSIdentical.model.predicted = cbind(BV.HSIdentical.model.predicted, BV.HSIdentical.model.predicted.blupse)

        attach(SP_data.filter)
        boxplot(feature~FIELD, xlab="FIELD", ylab=paste0(name), main=paste0(name," by FIELD"), col="pink")
        #boxplot(trait~EXP, xlab="EXP", ylab=paste0(name), main=paste0(name," by EXP"), col="pink")
        #boxplot(trait~LINE, xlab="LINE", ylab=paste0(name), main=paste0(name," by LINE"), col="pink")
        detach(SP_data.filter)

        SP_data.filter.gmean = aggregate(SP_data.filter[,c("feature")], by=list(LINE = SP_data.filter$LINE),mean,na.rm=T); colnames(SP_data.filter.gmean) = c("LINE","g_mean"); SP_data.filter.gmean=data.frame(SP_data.filter.gmean)


        #sigfigs to three digits
        df8=left_join(BV.HSIdentical.model.predicted[,1:3],counts,by="LINE");colnames(df8)=c("LINE",paste0(name),paste0(name,"_stderror"),paste0(name,"_Observations"))

        #df8$name = ifelse(df8[,name] == -0, 0 , df8[,name])
        #sigfigs.traits<-c(paste0(name))
        #for(i in sigfigs.traits){df8[,i]<-as.double(df8[,i])}


        #write.csv(df8, file= paste0("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/","ID", name,"_2020.csv"), row.names=F) ) #write BLUEs to spreadsheet
        assign(paste0(name), df8)

        rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
           SP_data.filter.gmean, SP_data.filter.feature, lines)
        gc()
        next
      }

      if(name == "GOSS" ){

        QUALDAT = asreml(fixed = feature ~ YEAR  ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE + REP,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)

        #BV.HSIdentical.model.predicted.blupse = summary(QUALDAT, coef=TRUE)$coef.random
        #BV.HSIdentical.model.predicted.blupse = BV.HSIdentical.model.predicted.blupse[,c(2,3)]
        #BV.HSIdentical.model.predicted = cbind(BV.HSIdentical.model.predicted, BV.HSIdentical.model.predicted.blupse)
        attach(SP_data.filter)
        boxplot(feature~FIELD, xlab="FIELD", ylab=paste0(name), main=paste0(name," by FIELD"), col="pink")
        #boxplot(trait~EXP, xlab="EXP", ylab=paste0(name), main=paste0(name," by EXP"), col="pink")
        #boxplot(trait~LINE, xlab="LINE", ylab=paste0(name), main=paste0(name," by LINE"), col="pink")
        detach(SP_data.filter)

        SP_data.filter.gmean = aggregate(SP_data.filter[,c("feature")], by=list(LINE = SP_data.filter$LINE),mean,na.rm=T); colnames(SP_data.filter.gmean) = c("LINE","g_mean"); SP_data.filter.gmean=data.frame(SP_data.filter.gmean)


        #sigfigs to three digits
        df8=left_join(BV.HSIdentical.model.predicted[,1:3],counts,by="LINE");colnames(df8)=c("LINE",paste0(name),paste0(name,"_stderror"),paste0(name,"_Observations"))
        df9 = left_join(df8,SP_data.filter.gmean,by="LINE")
        df8$GOSS = round(df9$g_mean,0)

        df8$GOSS = ifelse(df8$GOSS == 1,"Excellent",df8$GOSS)
        df8$GOSS = ifelse(df8$GOSS == 2,"Excellent-Average",df8$GOSS)
        df8$GOSS = ifelse(df8$GOSS == 3,"Average",df8$GOSS)
        df8$GOSS = ifelse(df8$GOSS == 4,"Average-Poor",df8$GOSS)
        df8$GOSS = ifelse(df8$GOSS == 5,"Poor",df8$GOSS)



        assign(paste0(name), df8)
        #hist(as.numeric(df5[4]), col="gold", main=paste0(name," Histogram"), xlab=paste0(name))
        # hist(df8[, 2], col="blue", main=paste0("adj Values For ", name), xlab=paste0("adj Values"))
        #hist(SP_data.filter[, name], main=paste0("GDU Values For ", name), col="brown", xlab=paste0("Raw"))

        rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
           SP_data.filter.gmean, SP_data.filter.feature, lines)
        gc()

        next
      }

      if(name == "Plt.Height" || name == "EarHt" ||  name == "Leaf.Angle" || name == "Tassel.Branches"){
        SP_data.filter$feature =  as.numeric(SP_data.filter$feature)

        QUALDAT = asreml(fixed = feature ~ YEAR ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE + FIELD*LINE,
                         residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT, classify = "LINE", aliased = F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)

        #BV.HSIdentical.model.predicted.blupse = summary(QUALDAT, coef=TRUE)$coef.random
        #BV.HSIdentical.model.predicted.blupse = BV.HSIdentical.model.predicted.blupse[,c(2,3)]
        #BV.HSIdentical.model.predicted = cbind(BV.HSIdentical.model.predicted, BV.HSIdentical.model.predicted.blupse)

        sigfigs.traits<-c("predicted.value")
        for(i in sigfigs.traits){BV.HSIdentical.model.predicted[,i]<-round(BV.HSIdentical.model.predicted[,i] ,0)}

      }

      if(name == "SLK10" || name == "SLK90" || name == "SHD10" || name == "SHD90"  ){
        QUALDAT = asreml(fixed = feature ~ FIELD ,
                         #random = ~ LINE + LINE*FIELD + REP*LINE,
                         random = ~ LINE ,
                         #residual = ~ idv(units),
                         data = SP_data.filter,

                         na.action=na.method(y="omit")
        )

        QUALDAT.SUM <- print(summary(QUALDAT))

        plot(QUALDAT)
        BV.HSIdentical.model.predicted<-predict(QUALDAT,classify = "LINE",  aliased=F)
        BV.HSIdentical.model.predicted = data.frame(BV.HSIdentical.model.predicted$pvals)

        #BV.HSIdentical.model.predicted.blupse = summary(QUALDAT, coef=TRUE)$coef.random
        #BV.HSIdentical.model.predicted.blupse = BV.HSIdentical.model.predicted.blupse[,c(2,3)]
        #BV.HSIdentical.model.predicted = cbind(BV.HSIdentical.model.predicted, BV.HSIdentical.model.predicted.blupse)

        sigfigs.traits<-c("predicted.value")
        for(i in sigfigs.traits){BV.HSIdentical.model.predicted[,i]<-round(BV.HSIdentical.model.predicted[,i] ,0)}
      }

      #######################################
      ######BLUPs
      #######################################

      attach(SP_data.filter)
      boxplot(feature~YEAR, xlab="YEAR", ylab=paste0(name), main=paste0(name," by YEAR"), col="pink")
      boxplot(feature~FIELD, xlab="FIELD", ylab=paste0(name), main=paste0(name," by FIELD"), col="red")
      #boxplot(trait~LINE, xlab="LINE", ylab=paste0(name), main=paste0(name," by LINE"), col="pink")
      detach(SP_data.filter)

      SP_data.filter.gmean = aggregate(SP_data.filter[,c("feature")], by=list(LINE = SP_data.filter$LINE),mean,na.rm=T); colnames(SP_data.filter.gmean) = c("LINE","g_mean"); SP_data.filter.gmean=data.frame(SP_data.filter.gmean)


      #sigfigs to three digits
      df8=left_join(BV.HSIdentical.model.predicted[,1:3],counts,by="LINE");colnames(df8)=c("LINE",paste0(name),paste0(name,"_stderror"),paste0(name,"_Observations"))

      #df8$name = ifelse(df8[,name] == -0, 0 , df8[,name])
      #sigfigs.traits<-c(paste0(name))
      #for(i in sigfigs.traits){df8[,i]<-as.double(df8[,i])}
      #df8[,2] = as.numeric(df8[,2])
      #write.csv(df8, file= paste0("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/","ID", name,"_2020.csv"), row.names=F) ) #write BLUEs to spreadsheet
      assign(paste0(name), df8)
      #hist(as.numeric(df5[4]), col="gold", main=paste0(name," Histogram"), xlab=paste0(name))
      hist(df8[, 2], col="blue", main=paste0("adj Values For ", name), xlab=paste0("adj Values"))
      hist(SP_data.filter[, "feature"], main=paste0("GDU Values For ", name), col="brown", xlab=paste0("Raw"))
      ## Compare BLUEs to line averages on a scatterplot
      #plot(df5[,2],df5[,3], main=paste0("adj Values By ", name," Plot"),xlab=paste0(name),ylab="adj Values")
      #xy<-qqmath(ranef(BV.HSIdentical))
      #print(xy)
      rm(BV.HSIdentical.model.predicted, counts, df8, df9, QUALDAT, QUALDAT.SUM, SP_data.filter, SP_data.filter.counts,
         SP_data.filter.gmean)
      gc()

    }

    sink()
    dev.off()

    rm(linesYear)
    gc()
    ######################################################################################################
    ######Submit report
    ######################################################################################################
    #GDU_shd50_Line_ML_BLUEs.1LOC=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_shd50LOC_ADJ.csv")
    #GDU_shd50_Line_ML_BLUEs.1YEAR=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_shd50YEAR_ADJ.csv")


    #GDU_slk50_Line_ML_BLUEs.1LOC=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_slk50LOC_ADJ.csv")
    #GDU_slk50_Line_ML_BLUEs.1YEAR=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_slk50YEAR_ADJ.csv")

    #################################################
    #shd_join = dplyr::left_join(GDU_shd50_Line_ML_BLUEs.1LOC,GDU_shd50_Line_ML_BLUEs.1YEAR,by="LINE")
    #silk_join = dplyr::left_join(GDU_slk50_Line_ML_BLUEs.1LOC, GDU_slk50_Line_ML_BLUEs.1YEAR, by = "LINE")

    #write.csv(shd_join,"R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/shd_Join.csv",row.names=F)

    #write.csv(silk_join,"R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/silk_Join.csv",row.names=F)

    #################################################
    #GDU_slk50_Line_ML_BLUEs.1=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_slk50YEAR_ADJ.csv")
    #GDU_shd50_Line_ML_BLUEs.1=read.csv("R:/Breeding/MT_TP/Models/Data/Shd_slk/2020/GDU_shd50YEAR_ADJ.csv")
    linesYear.line = data.frame(qualdat.df[!duplicated(qualdat.df$LINE), "LINE"])
    colnames(linesYear.line) = "LINE"
    rm(qualdat.df)
    gc()
    #Flowering_Catalog19s.1<-left_join(GDU_shd50_Line_ML_BLUEs.1, GDU_slk50_Line_ML_BLUEs.1, by = 'LINE')
    inbredDat = left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(
      left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(
        linesYear.line,
        Brace.Root, by = "LINE"),
        SLK10, by = "LINE"),
        SLK90, by = "LINE"),
        SHD10, by = "LINE"),
        SHD90 , by = "LINE"),
        Ear.Stalk, by = "LINE"),
        Plt.Height, by = "LINE"),
        EarHt, by = "LINE"),
        Tillers, by = "LINE"),
      Leaf.Angle, by = "LINE"),
      Shed.Rating, by = "LINE"),
      Stand.Count, by = "LINE"),
      Pollen.Duration..GDUs.10..to.90.., by = "LINE"),
      Silk.Color, by = "LINE"),
      Tassel.Extension, by = "LINE"),
      Tassel.Branches, by = "LINE"),
      Anther.Color , by = "LINE"),
      Slk_50 , by = "LINE"),
      Shd_50, by = "LINE"),
      Leaf.Texture , by = "LINE"),
      Leaf.Color , by = "LINE"),
      Glume.Ring , by = "LINE"),
      GOSS , by = "LINE"),
      NCLB, by = "LINE")
    inbredDat = inbredDat[!duplicated(inbredDat$LINE), ]
    #sigfigs to three digits
    #inbredDat$Shd_50_std.error<-signif(inbredDat$Shd_50_std.error ,2)
    #inbredDat$Slk_50_std.error<-signif(inbredDat$Slk_50_std.error ,2)
    #head(Flowering_Catalog19s.1);dim(Flowering_Catalog19s.1)
    inbredDat$Shd_50<-signif(inbredDat$Shd_50 ,3)
    inbredDat$Slk_50<-signif(inbredDat$Slk_50 ,3)
    inbredDatInbred = as.character(inbredDat$LINE)
    #inbredDat =  inbredDat %>% mutate_all(as.double)
    inbredDat$LINE = inbredDatInbred
    #remove unwanted varieties
    levelstodrop<-c("3MG2509", "3MG2510", "3MG3506","3MG3604", "3MG3608", "3MG4504", "3MG5504",  "3MG9502", "3MG9703",  "3MG9802",
                    "FR19",  "I5009/MBS3520", "I9005/I5009","I9005/MBS3520", "NUG003",  "NUG004", "NUG006", "NUG097", "NUG139",
                    "NUG922", "NUG923",   "SeagullSeventeen", "UI781",   "UI782", "UI783", "UI784", "UI785")

    for(i in levelstodrop){inbredDat <- droplevels(inbredDat[grep( i,inbredDat[ ,"LINE"], invert=TRUE), ] )}
    dim(inbredDat)
    names(inbredDat) <- gsub("\\.", "", names(inbredDat))
    cat("----------------------------Writing To File----------------------------", "\n")
    inbredDat[inbredDat=="#NUM!"] = NULL

   # write.xlsx(inbredDat, paste0(dpf,".xlsx") ,row.names=F, showNA=F)
    write.xlsx(inbredDat, paste0(dpf, season,"S.xlsx") ,row.names=F, overwrite=T)

    write.xlsx(inbredDat[,c(1,53:58)], paste0(dpf, "Flowering Catalog_",season,"S.xlsx") ,row.names=F, overwrite=T)

  }
  cat("----------------------------Done!----------------------------", "\n")

}


#FlowerCat()


