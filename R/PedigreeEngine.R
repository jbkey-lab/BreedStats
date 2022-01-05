############
#match and nest linked pedigress for export and review
############
#  folder="Test"
#  x="5_27_2021"
# # ws = paste0('/home/jacoblamkey/Downloads/Peds/YT_BV Yield Trial Master Catalog ',x, ".csv")
# # fdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder)
# # fdph = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/Hybrid")
# # wdp = paste0("/home/jacoblamkey/Downloads/Peds/",folder,"/")
# # vdp = '/home/jacoblamkey/Downloads/Peds/Variety.male.female.csv'
# #
#  ws = paste0('R:/Breeding/MT_TP/Models/Data/Department Data/YT_BV Yield Trial Master Catalog ',x, ".csv")
#  fdp = paste0("C:/Users/jake.lamkey/Desktop/")
#  fdph = paste0("R:/Breeding/MT_TP/Models/Breeding Values/",folder,"/Hybrid")
#  wdp = paste0("R:/Breeding/MT_TP/Models/Data/Department Data/",folder,"/")
#  vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'
# # #
# # ws = ws
# doHybridID=F
# doPedigreeChange =T
# doPedigreeToBecksChange=T
# doGCABV = F
# doWriteFinalPedigrees = F
# year= "2020"
#
# A = T
# B = F
# C = F
# Prop = T
# Choice = F
# D=F
# R=F
# X=F
# E=F
# Q=F
# V=F
# GEM=F
#
# s0=T
# s1=T
# s2=T
# s3=T
# s4=T
# s5=F
# doDNN=F
#
# seas0=21
# seas1=20
# seas2=19
# seas3=18
#  seas4=17
#  seas5=16
#  doYear=T
#
#
#  doYear = doYear
#  doHybridID = doHybridID
#  doPedigreeChange = doPedigreeChange
#  doPedigreeToBecksChange = doPedigreeToBecksChange
#  doGCABV = doGCABV
#  doWriteFinalPedigrees = doWriteFinalPedigrees
#  A = A
#  B = B
#  C = C
#  Prop = Prop
#  Choice = Choice
#  D=D
#  R=R
#  X=X
#  E=E
#  Q=Q
#  V=V
#  GEM=GEM
#  s0=s0
#  s1=s1
#  s2=s2
#  s3=s3
#  s4=s4
#  s5=s5
#  season0=seas0
#  season1=seas1
#  season2=seas2
#  season3=seas3
#  season4=seas4
#  season5=seas5
#  folder=folder
#  fdp=as.character(fdp)
#

#source("R:/Breeding/MT_TP/Models/R-Scripts/greplPeds.R")

pedigreeEngine = function(ws,
                          season0,
                          season1,
                          season2,
                          season3,
                          season4,
                          season5,
                          A ,
                          B ,
                          C ,
                          Prop ,
                          Choice ,
                          D,
                          R,
                          X,
                          E,
                          Q,
                          V,
                          GEM,
                          s0,
                          s1,
                          s2,
                          s3,
                          s4,
                          s5,
                          simulate,
                          InventoryPedigree,
                          ytData,
                          date,
                          doReduceNonCodes
){

  ## capture messages and errors to a file.
  #zz <- file("C:/Users/jake.lamkey/Documents/ADebugger.txt", open="wt")
  #sink(zz, type="message")

  #try(log("a"))


  #sink("C:/Users/jake.lamkey/Documents/ADebugger.txt")


  cat("B")
  if(simulate){
    data("bvdata")
  }
  if(ytData){
    BV.MC.Entry<-fread(paste0(ws))#all varieties to build the model
  }
  if(InventoryPedigree){
    BV.MC.Entry = InventoryPedigreeAdj(date=date)
  }
  colnames(BV.MC.Entry)
  dim(BV.MC.Entry)
  BV.MC.Entry<-data.frame(BV.MC.Entry)

  BV.MC.Entry<-data.table(BV.MC.Entry)
  cat("C")

  BV.MC.Inbred <- openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.xlsx"),1)
  BV.MC.Inbred$Pedigre_Backup = BV.MC.Inbred$PEDIGREE
  BV.MC.Inbred = BV.MC.Inbred[,c(1:3,22,4:21)]
  cat("D")


  #Clean the Entry list file
  colnames(BV.MC.Entry)[4] = "Entry.Book.Name"
  colnames(BV.MC.Entry)[13] = "Plot.Discarded"
  colnames(BV.MC.Entry)[14] = "Plot.Status"

  BV.MC.Entry.data = BV.MC.Entry[,-c(7,9,10,11,17,6,19,26,27,28,30,31,32,35,37,38,42,45,21,22,12)]

  BV.MC.Entry.data = data.table(BV.MC.Entry.data)
  dim(BV.MC.Entry.data)
  #rm(BV.MC.Entry)
  BV.MC.Entry.data$Plot.Discarded = as.character(BV.MC.Entry.data$Plot.Discarded)
  BV.MC.Entry.data$Plot.Status = as.character(BV.MC.Entry.data$Plot.Status)
  cat("E","\n")

  cat(A, B, C, Prop, Choice, D, R, X, E, Q, V, GEM, ws,"\n")
  cat(season0,season1,season2,season3,season4,season5,"\n")
  cat(s0,s1,s2,s3,s4,s5,"\n")

  BV.MC.Entry.data <- BV.MC.Entry.data %>% dplyr::filter(#Plot.Discarded != "Yes", Plot.Status != "3 - Bad",
    Pedigree != "FILL",
    Variety != "FILL",
    Pedigree != "placeholder",
    Variety != "placeholder",
    Entry.Book.Name != "Filler",
    Entry.Book.Name != "INBRED-GW_Prop"
  )
  dim(BV.MC.Entry.data)
  levelSelector = function(level="A"){
    if(s0){BV.MC.Entry.filterAs0 <- BV.MC.Entry.data %>% filter(paste0(season0,"S: Corn") == `Book.Season`) %>% filter(grepl(paste0(season0,paste0(level) ),Entry.Book.Name))}
    if(s0){BV.MC.Entry.filterAs0.1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season0,"S: Corn")) %>% filter(grepl(paste0(season0-10,level),Entry.Book.Name)) %>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s0){BV.MC.Entry.filterAs0.2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season0,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}


    if(s1){BV.MC.Entry.filterAs1 <- BV.MC.Entry.data %>% filter(paste0(season1,"S: Corn") == `Book.Season`) %>% filter(grepl(paste0(season1,paste0(level) ),Entry.Book.Name))}
    if(s1){BV.MC.Entry.filterAs1.1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season1,"S: Corn")) %>% filter(grepl(paste0(season1-10,level),Entry.Book.Name)) %>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s1){BV.MC.Entry.filterAs1.2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season1,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}


    if(s2){BV.MC.Entry.filterAs2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season2,"S: Corn")) %>% filter(grepl(paste0(season2,level),Entry.Book.Name))}
    if(s2){BV.MC.Entry.filterAs2.1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season2,"S: Corn")) %>%filter(grepl(paste0(season2-10,level),Entry.Book.Name))%>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s2){BV.MC.Entry.filterAs2.2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season2,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}


    if(s3){BV.MC.Entry.filterAs3 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season3,"S: Corn")) %>% filter(grepl(paste0(level),substr(Entry.Book.Name,1,1)))}
    if(s3){BV.MC.Entry.filterAs3.1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season3,"S: Corn")) %>%filter(grepl(paste0(season3-10,level),Entry.Book.Name))%>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s3){BV.MC.Entry.filterAs3.2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season3,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}


    if(s4){BV.MC.Entry.filterAs4 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season4,"S: Corn")) %>% filter(grepl(paste0(level),substr(Entry.Book.Name,1,1)))}
    if(s4){BV.MC.Entry.filterAs4.1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season4,"S: Corn")) %>% filter(grepl(paste0(season4-10,level),Entry.Book.Name))%>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s4){BV.MC.Entry.filterAs4.2 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season4,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}


    if(s5){BV.MC.Entry.filterAs5 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season5,"S: Corn")) %>% filter(grepl(paste0(level),substr(Entry.Book.Name,1,1))) %>%
      filter(Entry.Book.Name != "Agrivida - Nash", Entry.Book.Name != "Agrivida - Crehan", Entry.Book.Name != "AT Herb")}
    if(s5){BV.MC.Entry.filterAs5.1 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season5,"S: Corn")) %>% filter(grepl(paste0(season5-10,level),Entry.Book.Name))%>%
      filter(!grepl(paste0("Prop"),substr(Entry.Book.Name,1,4)))}
    if(s5){BV.MC.Entry.filterAs5.2 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season5,"S: Corn")) %>% filter(grepl(paste0("Group ",level),Entry.Book.Name))}
    #BV.MC.Entry.filterAs5.1=BV.MC.Entry.filterAs5.1[c(1:),]

    BV.MC.Entry.filterA = rbind(if(s0){BV.MC.Entry.filterAs0},
                                if(s0){BV.MC.Entry.filterAs0.1},
                                if(s0){BV.MC.Entry.filterAs0.2},
                                if(s1){BV.MC.Entry.filterAs1},
                                if(s1){BV.MC.Entry.filterAs1.1},
                                if(s1){BV.MC.Entry.filterAs1.2},
                                if(s2){BV.MC.Entry.filterAs2},
                                if(s2){BV.MC.Entry.filterAs2.1},
                                if(s2){BV.MC.Entry.filterAs2.2},
                                if(s3){BV.MC.Entry.filterAs3},
                                if(s3){BV.MC.Entry.filterAs3.1},
                                if(s3){BV.MC.Entry.filterAs3.2},
                                if(s4){BV.MC.Entry.filterAs4},
                                if(s4){BV.MC.Entry.filterAs4.1},
                                if(s4){BV.MC.Entry.filterAs4.2},
                                if(s5){BV.MC.Entry.filterAs5},
                                if(s5){BV.MC.Entry.filterAs5.1},
                                if(s5){BV.MC.Entry.filterAs5.2}
    )
    BV.MC.Entry.filter = BV.MC.Entry.filterA[!duplicated(BV.MC.Entry.filterA$RecId),]
    rm(BV.MC.Entry.filterAs1,BV.MC.Entry.filterAs1.1,BV.MC.Entry.filterAs1.2,BV.MC.Entry.filterAs2,BV.MC.Entry.filterAs2.1,
       BV.MC.Entry.filterAs2.2,BV.MC.Entry.filterAs3,BV.MC.Entry.filterAs3.1,BV.MC.Entry.filterAs3.2,BV.MC.Entry.filterAs4,
       BV.MC.Entry.filterAs4.1,BV.MC.Entry.filterAs4.2,BV.MC.Entry.filterAs5,BV.MC.Entry.filterAs5.1,BV.MC.Entry.filterAs5.2,
       BV.MC.Entry.filterA,BV.MC.Entry.filterAs0,BV.MC.Entry.filterAs0.1,BV.MC.Entry.filterAs0.2)
    invisible(gc())
    #dim(BV.MC.Entry.filterA)
    return(BV.MC.Entry.filter)
  }

  BV.MC.Entry.filterA = levelSelector(level="A")
  dim(BV.MC.Entry.filterA)

  BV.MC.Entry.filterB = levelSelector(level="B")
  dim(BV.MC.Entry.filterB)

  BV.MC.Entry.filterC = levelSelector(level="C")
  dim(BV.MC.Entry.filterC)

  BV.MC.Entry.filterE= levelSelector(level="E")
  dim(BV.MC.Entry.filterE)

  BV.MC.Entry.filterR = levelSelector(level="R")
  dim(BV.MC.Entry.filterR)

  BV.MC.Entry.filterX = levelSelector(level="X")
  dim(BV.MC.Entry.filterX)

  BV.MC.Entry.filterD = levelSelector(level="D")
  dim(BV.MC.Entry.filterD)

  BV.MC.Entry.filterQ = levelSelector(level="Q")
  dim(BV.MC.Entry.filterQ)

  BV.MC.Entry.filterV = levelSelector(level="V")
  dim(BV.MC.Entry.filterV)

  BV.MC.Entry.filterGEM = levelSelector(level="GEM")
  dim(BV.MC.Entry.filterGEM)
  cat("F")
  ## reset message sink and close the file connection
  #sink()



  pcSelector = function(commericalType = "Choice", altCommericalType = "CHOICE"){

    if(s0){BV.MC.Entry.filterProps0 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season0,"S: Corn")) %>% filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp0 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season0,"S: Corn")) %>%  filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp00 = rbind(BV.MC.Entry.filterProps0,
                                     BV.MC.Entry.filterProp0)
    }

    if(s1){BV.MC.Entry.filterProps1 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season1,"S: Corn")) %>% filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp1 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season1,"S: Corn")) %>%  filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp10 = rbind(BV.MC.Entry.filterProps1,
                                     BV.MC.Entry.filterProp1)
    }

    if(s2){BV.MC.Entry.filterProps2 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season2,"S: Corn")) %>%  filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp2 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season2,"S: Corn")) %>%  filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp20 = rbind(BV.MC.Entry.filterProps2,
                                     BV.MC.Entry.filterProp2)
    }

    if(s3){BV.MC.Entry.filterProps3 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season3,"S: Corn")) %>% filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp3 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season3,"S: Corn")) %>%  filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp30 = rbind(BV.MC.Entry.filterProps3,
                                     BV.MC.Entry.filterProp3)
    }

    if(s4){BV.MC.Entry.filterProps4 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season4,"S: Corn")) %>% filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp4 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season4,"S: Corn")) %>%filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp40 = rbind(BV.MC.Entry.filterProps4,
                                     BV.MC.Entry.filterProp4)
    }

    if(s5){BV.MC.Entry.filterProps5 <- BV.MC.Entry.data %>% filter(`Book.Season`== paste0(season5,"S: Corn")) %>%filter(grepl(commericalType, Entry.Book.Name))
    BV.MC.Entry.filterProp5 <- BV.MC.Entry.data %>%filter(`Book.Season`== paste0(season5,"S: Corn")) %>% filter(grepl(altCommericalType, Entry.Book.Name))

    BV.MC.Entry.filterProp50 = rbind(BV.MC.Entry.filterProps5,
                                     BV.MC.Entry.filterProp5)
    }

    BV.MC.Entry.filter = rbind(if(s0){BV.MC.Entry.filterProp00},
                               if(s1){BV.MC.Entry.filterProp10},
                               if(s2){BV.MC.Entry.filterProp20},
                               if(s3){BV.MC.Entry.filterProp30},
                               if(s4){BV.MC.Entry.filterProp40},
                               if(s5){BV.MC.Entry.filterProp50}


    )
    BV.MC.Entry.filter= BV.MC.Entry.filter[!duplicated(BV.MC.Entry.filter$RecId), ]
    rm(BV.MC.Entry.filterProp20,BV.MC.Entry.filterProp30,BV.MC.Entry.filterProp40,BV.MC.Entry.filterProp50,
       BV.MC.Entry.filterProps0, BV.MC.Entry.filterProp0, BV.MC.Entry.filterProps1, BV.MC.Entry.filterProp1,
       BV.MC.Entry.filterProps2, BV.MC.Entry.filterProp2,BV.MC.Entry.filterProps3, BV.MC.Entry.filterProp3,
       BV.MC.Entry.filterProps4, BV.MC.Entry.filterProp4,BV.MC.Entry.filterProps5, BV.MC.Entry.filterProp5)

    invisible(gc())
    return(BV.MC.Entry.filter)
  }

  BV.MC.Entry.filterChoice = pcSelector(commericalType = "Choice", altCommericalType = "CHOICE")
  dim(BV.MC.Entry.filterChoice)
  BV.MC.Entry.filterProp = pcSelector(commericalType = "Prop", altCommericalType = "PET")
  dim(BV.MC.Entry.filterProp)

  cat("G")
  BV.MC.Entry.data.AB = rbind(if(A){BV.MC.Entry.filterA},
                              if(B){ BV.MC.Entry.filterB},
                              if(C){BV.MC.Entry.filterC},
                              if(Choice){BV.MC.Entry.filterChoice},
                              if(Prop){BV.MC.Entry.filterProp},
                              if(D){BV.MC.Entry.filterD},
                              if(E){BV.MC.Entry.filterE},
                              if(R){BV.MC.Entry.filterR},
                              if(X){BV.MC.Entry.filterX},
                              if(Q){BV.MC.Entry.filterQ},
                              if(V){BV.MC.Entry.filterV},
                              if(GEM){BV.MC.Entry.filterGEM}
  )

  rm(BV.MC.Entry.filterA,BV.MC.Entry.filterB,BV.MC.Entry.filterC,BV.MC.Entry.filterChoice,BV.MC.Entry.filterProp,
     BV.MC.Entry.filterD,BV.MC.Entry.filterE,BV.MC.Entry.filterR,BV.MC.Entry.filterX,BV.MC.Entry.filterQ,BV.MC.Entry.filterV,
     BV.MC.Entry.filterGEM
  )
  invisible(gc())
  dim(BV.MC.Entry.data.AB)
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$RecId), ]
  dim(BV.MC.Entry.data.AB)

  testanti = anti_join(BV.MC.Entry.data, BV.MC.Entry.data.AB, by="RecId")
  dim(testanti)
  PD= BV.MC.Entry.data.AB[,c(1,7,8)]
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[, -c(7,8,12)]#12
  #BV.MC.Entry.data.AB=BV.MC.Entry.data.AB[,c(1,2,4:26,3)]
  rm(BV.MC.Entry.data)
  colnames(BV.MC.Entry.data.AB)
  dim(BV.MC.Entry.data.AB)
  BV.MC.Entry.data.AB$`Male.Pedigree` = as.character(BV.MC.Entry.data.AB$`Male.Pedigree`)
  BV.MC.Entry.data.AB$`Female.Pedigree` = as.character(BV.MC.Entry.data.AB$`Female.Pedigree`)

  BV.MC.Entry.data.AB$`Male.Pedigree`[as.character(BV.MC.Entry.data.AB$`Male.Pedigree`) == "-"] = NA
  BV.MC.Entry.data.AB$`Female.Pedigree`[as.character(BV.MC.Entry.data.AB$`Female.Pedigree`) == "-"] = NA

  #BV.MC.Entry.data.AB.dash = BV.MC.Entry.data.AB %>% filter(`Female.Pedigree` == "-")
  #Add adjusted pedigrees to data file
  #BV.MC<-read.csv(paste0('R:/Breeding/MT_TP/Models/Data/Department Data/Yield Trial Master CatalogAll_1125.csv'))#all varieties to build the model
  #BV.Var<-read.csv('R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.csv')

  #seperate Hybrid.ID to obtain line id for male and Female.Pedigrees
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Hybrid.ID`,sep="[ + ][ + ]",
                                                                c("unique_female_id.numeric","unique_male_id.numeric"),
                                                                extra="merge",
                                                                remove=F)
  #BV.MC.Entry.data.AB$unique_female_id.numeric <- gsub("\\+", "", as.character(BV.MC.Entry.data.AB$unique_female_id.numeric))
  #BV.MC.Entry.data.AB$unique_female_id.numeric = as.character(  BV.MC.Entry.data.AB$unique_female_id.numeric)

  BV.MC.Entry.data.AB$unique_female_id=round(as.numeric(BV.MC.Entry.data.AB$unique_female_id.numeric), digits=8)
  BV.MC.Entry.data.AB$unique_male_id=round(as.numeric(BV.MC.Entry.data.AB$unique_male_id.numeric), digits=8)

  #BV.MC.Entry.data.AB$unique_female_id = format((BV.MC.Entry.data.AB[,29]), nsmall=8, digits=9, na.encode	= FALSE)
  #BV.MC.Entry.data.AB$unique_male_id = format((BV.MC.Entry.data.AB[,30]), nsmall=8, digits=9, na.encode = FALSE)

  BV.MC.Entry.data.AB$unique_female_id = as.character(rep("needsdata",nrow(BV.MC.Entry.data.AB)))
  BV.MC.Entry.data.AB$unique_male_id = as.character(rep("needsdata",nrow(BV.MC.Entry.data.AB)))
#
#   BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id < 0,
#                                                 "needsdata",BV.MC.Entry.data.AB$unique_female_id)
#   BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id < 0,
#                                               "needsdata",BV.MC.Entry.data.AB$unique_male_id)
#

  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="needsdata",
                                              BV.MC.Entry.data.AB$`Male.Pedigree`,
                                              BV.MC.Entry.data.AB$unique_male_id)
  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="needsdata",
                                                BV.MC.Entry.data.AB$`Female.Pedigree`,
                                                BV.MC.Entry.data.AB$unique_female_id)

  #merge the male or Female.Pedigree for NA values of unique columns
  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                BV.MC.Entry.data.AB$`Female.Pedigree`,
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                BV.MC.Entry.data.AB$`Female.Pedigree`,
                                                BV.MC.Entry.data.AB$unique_female_id)

  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$`Male.Pedigree`),
                                              BV.MC.Entry.data.AB$unique_male_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$`Male.Pedigree`),
                                              BV.MC.Entry.data.AB$unique_male_id)

  #get check with pedigree
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Pedigree`,sep="\\/",
                                                                c("unique_femalecheck_id","unique_malecheck_id"),
                                                                #extra="merge",
                                                                remove=F)
  BV.MC.Entry.data.AB$unique_femalecheck_id[is.na(BV.MC.Entry.data.AB$unique_malecheck_id)] = NA

  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Pedigree`,sep="[ + ][ + ]",
                                                                c("unique_femalepedigree_id","unique_malepedigree_id"),
                                                                #extra="merge",
                                                                remove=F)
  BV.MC.Entry.data.AB$unique_femalepedigree_id[is.na(BV.MC.Entry.data.AB$unique_malepedigree_id)] = NA

  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Pedigree`,sep="[  + ][  + ][  + ][  + ]",
                                                                c("unique_female2pedigree_id","unique_male2pedigree_id"),
                                                                #extra="merge",
                                                                remove=F)
  BV.MC.Entry.data.AB$unique_female2pedigree_id[is.na(BV.MC.Entry.data.AB$unique_male2pedigree_id)] = NA

  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                BV.MC.Entry.data.AB$unique_femalepedigree_id,
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                BV.MC.Entry.data.AB$unique_femalepedigree_id,
                                                BV.MC.Entry.data.AB$unique_female_id)

  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$unique_malepedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$unique_malepedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                BV.MC.Entry.data.AB$unique_female2pedigree_id,
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                BV.MC.Entry.data.AB$unique_female2pedigree_id,
                                                BV.MC.Entry.data.AB$unique_female_id)

  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$unique_male2pedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$unique_male2pedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  ##########################################

  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Ped.Column`,sep="[/]",
                                                                c("unique_femalepedcol_id","unique_malepedcol_id"),
                                                                #extra="merge",
                                                                remove=F)
  BV.MC.Entry.data.AB$unique_femalepedcol_id[is.na(BV.MC.Entry.data.AB$unique_malepedcol_id)] = NA

  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                BV.MC.Entry.data.AB$unique_femalepedcol_id,
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                BV.MC.Entry.data.AB$unique_femalepedcol_id,
                                                BV.MC.Entry.data.AB$unique_female_id)

  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$unique_malepedcol_id),
                                              BV.MC.Entry.data.AB$unique_male_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$unique_malepedcol_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  ##############################################

  BV.MC.Entry.data.AB.checkfilter = BV.MC.Entry.data.AB %>% tidyr::separate(`Pedigree`,sep="[/]",
                                                                            c("unique_femalecheck_id","unique_malecheck_id"),
                                                                            #extra="merge",
                                                                            remove=F) %>% filter(unique_female_id=="")

  #gets variaty checks without pedigree
  BV.MC.Entry.data.AB.checkfilter.variety = BV.MC.Entry.data.AB.checkfilter %>% filter(is.na(unique_malecheck_id))
  BV.MC.Entry.data.AB.checkfilter.variety.duplciate=BV.MC.Entry.data.AB.checkfilter.variety[!duplicated(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree),]
  #BV.MC.Entry.data.AB.checkfilter.variety.duplciate$Pedigree#shows variety checks that need a pedigree
  BV.MC.Entry.data.AB.save = BV.MC.Entry.data.AB
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB.save
  #attached a male and Female.Pedigree to missing variety without pedigree
  vdp = 'R:/Breeding/MT_TP/Models/Data/Department Data/Variety.male.female.xlsx'

  BV.MC.Entry.data.AB.NAcountMale = BV.MC.Entry.data.AB %>% filter(is.na(unique_male_id))
  BV.MC.Entry.data.AB.NAcountFemale = BV.MC.Entry.data.AB %>% filter(is.na(unique_female_id))

  dim(BV.MC.Entry.data.AB.NAcountMale)
  dim(BV.MC.Entry.data.AB.NAcountFemale)

  Variety.checknames = openxlsx::read.xlsx(vdp,1)
  #Variety.checknames=data.frame(Variety=c("5994V2P","6557V2P","6374V2P","DKC54-38RIB","5507AM","5955wx","6015wx","DKC45-65RIB",
  #                                        "P9188AMX","DKC50-08RIB"),
  ##                              Female.checkVariety=c("T5508","R3584","F9898","DKC54-38RIB-Female","H4S","T1741","R2613","DKC45-65RIB-Female","P9188AMX-Female","DKC50-08RIB-Female"),
  #                              Male.checkvariety=c("R2558","F1232","R2846","DKC54-38RIB-Male","8SY","F5451","R2281","DKC45-65RIB-Male","P9188AMX-Male","DKC50-08RIB-Male"))
  #merge this dataframe with the others then add the checks with pedigrees to the unique column
  #this should be it for pedigrees
  colnames(Variety.checknames)[c(2,3,4)] = c("Female.checkVariety","Male.checkVariety","")
  Variety.checknames$Female.checkVariety = as.character(Variety.checknames$Female.checkVariety)
  Variety.checknames$Male.checkVariety = as.character(Variety.checknames$Male.checkVariety)
  Variety.checknames$Variety = as.character(Variety.checknames$Variety)
  BV.MC.Entry.data.AB$Pedigree = as.character(BV.MC.Entry.data.AB$Pedigree)
  BV.MC.Entry.data.AB$Variety = as.character(BV.MC.Entry.data.AB$Variety)

  BV.MC.Entry.data.AB$unique_female_id = as.character(BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = as.character(BV.MC.Entry.data.AB$unique_male_id)


  BV.MC.Entry.data.AB = left_join(BV.MC.Entry.data.AB,Variety.checknames,by= c("Pedigree"="Variety"))

  #remove spaces in all mathcing columns
  BV.MC.Entry.data.AB$unique_female_id <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$unique_male_id)

  #merge the variety checks in to unique column
  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                as.character(BV.MC.Entry.data.AB$Female.checkVariety),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$Male.checkVariety),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                as.character(BV.MC.Entry.data.AB$Female.checkVariety),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$Male.checkVariety),
                                              BV.MC.Entry.data.AB$unique_male_id)
  #merge the variety with pedigree into unique column
  ##############################################3
  colnames(Variety.checknames)[c(2,3,4)] = c("Female.check2Variety","Male.check2variety","")

  BV.MC.Entry.data.AB = left_join(BV.MC.Entry.data.AB,Variety.checknames,by= c("Variety"="Variety"))

  #remove spaces in all mathcing columns
  BV.MC.Entry.data.AB$unique_female_id <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id <- gsub("[[:space:]]", "", BV.MC.Entry.data.AB$unique_male_id)

  #merge the variety checks in to unique column
  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                as.character(BV.MC.Entry.data.AB$Female.check2Variety),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$Male.check2variety),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                as.character(BV.MC.Entry.data.AB$Female.check2Variety),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$Male.check2variety),
                                              BV.MC.Entry.data.AB$unique_male_id)

  ########################################################
  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                as.character(BV.MC.Entry.data.AB$unique_femalecheck_id),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$unique_malecheck_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                as.character(BV.MC.Entry.data.AB$unique_femalecheck_id),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$unique_malecheck_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[ !which(BV.MC.Entry.data.AB$Variety == "" & BV.MC.Entry.data.AB$Pedigree == "" & BV.MC.Entry.data.AB$`Hybrid.ID` == ""), ]

  #BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% filter(Entry.Book.Name != "INBRED-GW_Prop", Entry.Book.Name != "B163-RAGT-95-100")
  ###################################################
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB %>% tidyr::separate(`Variety`,sep="[/]",
                                                                c("unique_female3pedigree_id","unique_male3pedigree_id"),
                                                                #extra="merge",
                                                                remove=F)
  BV.MC.Entry.data.AB$unique_female3pedigree_id[is.na(BV.MC.Entry.data.AB$unique_male3pedigree_id)] = NA

  BV.MC.Entry.data.AB$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_female_id),
                                                as.character(BV.MC.Entry.data.AB$unique_female3pedigree_id),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id),
                                              as.character(BV.MC.Entry.data.AB$unique_male3pedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = ifelse(BV.MC.Entry.data.AB$unique_female_id=="",
                                                as.character(BV.MC.Entry.data.AB$unique_female3pedigree_id),
                                                BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = ifelse(BV.MC.Entry.data.AB$unique_male_id=="",
                                              as.character(BV.MC.Entry.data.AB$unique_male3pedigree_id),
                                              BV.MC.Entry.data.AB$unique_male_id)


  ###########################################333



  BV.MC.Entry.data.AB.checkfilter.variety = BV.MC.Entry.data.AB %>% filter(is.na(unique_male_id))
  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id=="",
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id=="",
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id),
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id),
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id[BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id == ".Male"]= NA
  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id[BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id == ".Female"]= NA

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id=="",
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id=="",
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id),
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id),
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  # ind <- match(BV.MC.Entry.data.AB.checkfilter.variety$RecId, BV.MC.Entry.data.AB$RecId)
  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = as.character(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id)
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = as.character(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id)

  BV.MC.Entry.data.AB$unique_female_id = as.character(BV.MC.Entry.data.AB$unique_female_id)
  BV.MC.Entry.data.AB$unique_male_id = as.character(BV.MC.Entry.data.AB$unique_male_id)

  # BV.MC.Entry.data.AB=setDT(BV.MC.Entry.data.AB)
  # BV.MC.Entry.data.AB.checkfilter.variety=setDT(BV.MC.Entry.data.AB.checkfilter.variety)
  BV.MC.Entry.data.AB =  BV.MC.Entry.data.AB %>%
    anti_join(BV.MC.Entry.data.AB.checkfilter.variety, by = "RecId") %>%
    bind_rows(BV.MC.Entry.data.AB.checkfilter.variety) %>%
    arrange(RecId)

  #BV.MC.Entry.data.AB[ind, 39:40] <- BV.MC.Entry.data.AB.checkfilter.variety[39:40]

  BV.MC.Entry.data.AB.checkfilter.variety = BV.MC.Entry.data.AB %>% filter(is.na(unique_male_id))


  BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped=BV.MC.Entry.data.AB.checkfilter.variety[!duplicated(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree),]
  BV.MC.Entry.data.AB.checkfilter.variety.duplciate=BV.MC.Entry.data.AB.checkfilter.variety[!duplicated(BV.MC.Entry.data.AB.checkfilter.variety$Variety),]

  pedigreeToAdd=BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped[,c("Pedigree","Entry.Book.Name","Female.Pedigree","Male.Pedigree","Hybrid.ID")]#shows variety checks that need a pedigree
  varietyToAdd=BV.MC.Entry.data.AB.checkfilter.variety.duplciate[,c("Variety","Entry.Book.Name","Female.Pedigree","Male.Pedigree","Hybrid.ID")]#shows variety checks that need a pedigree

  dim(pedigreeToAdd); dim(varietyToAdd)

  ####################################################Female
  BV.MC.Entry.data.AB.checkfilter.variety = BV.MC.Entry.data.AB %>% filter(is.na(unique_female_id))

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id=="",
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id=="",
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id),
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id),
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Variety,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id[BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id == ".Male"]= NA
  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id[BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id == ".Female"]= NA

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id=="",
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id=="",
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id),
                                                                    paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Female"),
                                                                    BV.MC.Entry.data.AB.checkfilter.variety$unique_female_id      )
  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id = ifelse(is.na(BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id),
                                                                  paste0(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree,".Male"),
                                                                  BV.MC.Entry.data.AB.checkfilter.variety$unique_male_id      )

  # ind <- match(BV.MC.Entry.data.AB.checkfilter.variety$RecId, BV.MC.Entry.data.AB$RecId)

  # BV.MC.Entry.data.AB=setDT(BV.MC.Entry.data.AB)
  # BV.MC.Entry.data.AB.checkfilter.variety=setDT(BV.MC.Entry.data.AB.checkfilter.variety)
  # BV.MC.Entry.data.AB =  BV.MC.Entry.data.AB %>%
  #   anti_join(BV.MC.Entry.data.AB.checkfilter.variety, by = "RecId") %>%
  #   bind_rows(BV.MC.Entry.data.AB.checkfilter.variety) %>%
  #   arrange(RecId)

  #BV.MC.Entry.data.AB[ind, 39:40] <- BV.MC.Entry.data.AB.checkfilter.variety[39:40]

  BV.MC.Entry.data.AB.checkfilter.variety = BV.MC.Entry.data.AB %>% filter(is.na(unique_male_id))


  BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped=BV.MC.Entry.data.AB.checkfilter.variety[!duplicated(BV.MC.Entry.data.AB.checkfilter.variety$Pedigree),]
  BV.MC.Entry.data.AB.checkfilter.variety.duplciate=BV.MC.Entry.data.AB.checkfilter.variety[!duplicated(BV.MC.Entry.data.AB.checkfilter.variety$Variety),]

  pedigreeToAdd=BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped[,c("Pedigree","Entry.Book.Name","Female.Pedigree","Male.Pedigree","Hybrid.ID")]#shows variety checks that need a pedigree
  varietyToAdd=BV.MC.Entry.data.AB.checkfilter.variety.duplciate[,c("Variety","Entry.Book.Name","Female.Pedigree","Male.Pedigree","Hybrid.ID")]#shows variety checks that need a pedigree

  dim(pedigreeToAdd); dim(varietyToAdd)
  #if(nrow(varietyToAdd)>0){
  #write.csv(pedigreeToAdd,paste0(fdp,"pedigreeToAdd.csv"),row.names=F,na="")
  #write.csv(varietyToAdd,paste0(fdp,"varietyToAdd.csv"),row.names=F,na="")

  #}
  #BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[!duplicated(BV.MC.Entry.data.AB$Pedigree), ]

  rm(
    "BV.MC.Entry.data",
    "Variety.checknames"  ,
    "varietyToAdd" ,
    "pedigreeToAdd",
    "testanti",
    "BV.MC.Entry.data.AB.save"

    #"BV.MC.Entry"

  )
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  # write.table(x=BV.MC.Entry.data.AB.clean, file=paste0(wdp,"/","BV.MC.Entry.data.",
  #                                                      if(A){print("A")},
  #                                                      if(B){print("B")},
  #                                                      if(C){print("C")},
  #                                                      if(Prop){print("Prop")},
  #                                                      if(Choice){print("Choice")},
  #                                                      if(D){print("D")},
  #                                                      if(E){print("E")},
  #                                                      if(Q){print("Q")},
  #                                                      if(R){print("R")},
  #                                                      if(V){print("V")},
  #                                                      if(X){print("X")}
  #                                                      ,".txt"))
  #
  # cat("H")

  #source("R:/Breeding/MT_TP/Models/R-Scripts/pedAdjustment.R")
  newData = PedAdjust( data = BV.MC.Entry.data.AB,doReduceNonCodes=doReduceNonCodes  )

  nrow(newData)

  newDataRmDups = newData[!duplicated(newData$match),]
  nrow(newDataRmDups)

  newData$changed = ifelse(newData$pedigree != newData$match, T, F)
  newData.changed = newData %>% filter(changed==T)

  nrow(newData.changed)

  cat("I")
  BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[,-c(8,9,11,12,13,14,15,16,18,19,23,24,41,42,43,44,45,46)]


  ##########################################
  #IMPORT REVIEWED MALE AND Female.PedigreeS
  ##########################################
  #BV.MC<-read.xlsx(paste0(wdp,'linked.male.female.nestedpeds.update.xlsx'),1,na.strings ="")#all varieties to build the model

  linked.peds = openxlsx::read.xlsx(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/linked.peds.updated.xlsx"), 1 )
  BV.MC = linked.peds

  # dim(linked.peds)
  # generation = read.xlsx("P:/Temp/Inventory Master Catalog_4_16_2021_1_45 PM.xlsx", 1)
  # generation = generation[!duplicated(generation$Pedigree),]
  #
  # colnames(generation)
  # linked.peds = left_join(linked.peds, generation, by=c("pedigree"="Pedigree"))
  #
  # generationMatch = read.xlsx("P:/Temp/Inventory Master Catalog_4_16_2021_1_45 PM.xlsx", 1)
  # generationMatch = generationMatch[!duplicated(generationMatch$Pedigree),]
  #
  # colnames(generationMatch)
  # linked.peds = left_join(linked.peds, generationMatch, by=c("match"="Pedigree"))
  # dim(linked.peds)
  # write.xlsx(linked.peds,"C:/Users/jake.lamkey/Desktop/linked.male.female.nestedpeds.updated.xlsx")
  #
  # BV.MC.Entry.data.AB = read.table(paste0(wdp,"/","BV.MC.Entry.data.",
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
  #
  cat("J")

  #BV.MC.Male = BV.MC %>% filter(Gender == "Male")
  #BV.MC.Female = BV.MC %>% filter(Gender == "FEMALE")
  #BV.MC.Female$unique_female_id <- gsub("[[:space:]]", "", BV.MC.Female$unique_female_id)
  #BV.MC.Male$unique_male_id <- gsub("[[:space:]]", "", BV.MC.Male$unique_male_id)

  #BV.MC.Male = BV.MC.Male[,c(1,2)];colnames(BV.MC.Male)=c("unique_male_id","match1")
  #BV.MC.Female = BV.MC.Female[,c(1,2)];colnames(BV.MC.Female)=c("unique_female_id","match1")

  #write.csv(BV.MC.Entry.data.AB,"")
  ######create male and female merge reduced and line_id list to us

  #BV.MC.Male.reduced <- BV.MC.Male[,c("unique_male_id.1","Reduced_Final_male_pedigree")]
  #BV.MC.Male.Line_ID <- BV.MC.Male[,c("unique_male_id.1","Line_ID_Final_Male_Pedigree")]

  #BV.MC.Female.reduced <- BV.MC.Female[,c("unique_female_id.1","Reduced_Final_Female_Pedigree")]
  #BV.MC.Female.Line_ID <- BV.MC.Female[,c("unique_female_id.1","Line_ID_Final_Female_Pedigree")]
  # rm(Variety.checknames,BV.MC.Entry.data.AB.checkfilter.variety.duplciate,BV.MC.Entry.data.AB.checkfilter.variety,
  #    BV.MC.Entry.data.AB.checkfilter,BV.MC.Female.Line_ID,BV.MC.Female.Line_ID.entry,BV.MC.Female.reduced,BV.MC.Female.reduced.entry,
  #    BV.MC.Male.Line_ID,BV.MC.Male.Line_ID.entry,BV.MC.Male.reduced,BV.MC.Male.reduced.entry,
  #    to_search_in.female,to_search_in.male,to_search_with.female,to_search_with.male,linked.female.peds,linked.male.peds,BV.MC.Entry)
  # rm(BV.MC.Entry.filterA, BV.MC.Entry.filterB)
  # invisible(gc(reset=T)) #cleans memory "garbage collector"
  #
  #BV.MC.Entry.data.AB$unique_male_id.x = ifelse(is.na(BV.MC.Entry.data.AB$unique_male_id.x),
  #                                            as.character(BV.MC.Entry.data.AB$Pedigree),
  #                                            BV.MC.Entry.data.AB$unique_male_id)
  #BV.MC.Entry.data.AB = BV.MC.Entry.data.AB[ ,c(1:6,7,10,17,25:38,39:40)]
  BV.MC.Entry.data.AB <- left_join(BV.MC.Entry.data.AB, BV.MC[,c(1,3)], by=c("unique_male_id"="uniqued_id"));dim(BV.MC.Entry.data.AB)
  #BV.MC.Male.Line_ID.entry <- left_join(BV.MC.Male.reduced.entry, BV.MC.Male.Line_ID, by=c("unique_male_id"="unique_male_id.1"),keep=T);dim(BV.MC.Male.Line_ID.entry)

  BV.MC.Entry.data.AB <- left_join(BV.MC.Entry.data.AB, BV.MC[,c(1,3)], by=c("unique_female_id"="uniqued_id"));dim(BV.MC.Entry.data.AB)
  #BV.MC.Female.Line_ID.entry <- left_join(BV.MC.Female.reduced.entry, BV.MC.Female.Line_ID, by=c("unique_female_id"="unique_female_id.1"),keep=T);dim(BV.MC.Female.Line_ID.entry)


  BV.MC.Entry.data.AB.review.x = BV.MC.Entry.data.AB %>% filter(match.x == "")
  BV.MC.Entry.data.AB.review.y = BV.MC.Entry.data.AB %>% filter(match.y == "")
  #cat(BV.MC.Entry.data.AB.review.x)
  #cat(BV.MC.Entry.data.AB.review.y)

  BV.MC.Entry.data.AB.review.xna = BV.MC.Entry.data.AB %>% filter(is.na(match.x ))
  BV.MC.Entry.data.AB.review.yna = BV.MC.Entry.data.AB %>% filter(is.na(match.y ))
  cat("Number of missing male lines is ",nrow(BV.MC.Entry.data.AB.review.xna) + nrow(BV.MC.Entry.data.AB.review.x),".\n")
  cat("Number of missing female lines is ",nrow(BV.MC.Entry.data.AB.review.yna) + nrow(BV.MC.Entry.data.AB.review.y),".\n")

  # BV.MC.Entry.data.AB.forReview = BV.MC.Entry.data.AB[,-c(3,5,6,32,33,14,15,8,9)]
  # BV.MC.Entry.data.AB.forReview.female = BV.MC.Entry.data.AB.forReview[is.na(BV.MC.Entry.data.AB.forReview$match1.y.y.y.y.y.y),]
  # BV.MC.Entry.data.AB.forReview.female=BV.MC.Entry.data.AB.forReview.female[!duplicated(BV.MC.Entry.data.AB.forReview.female$Pedigree),]
  # BV.MC.Entry.data.AB.forReview.male = BV.MC.Entry.data.AB.forReview[is.na(BV.MC.Entry.data.AB.forReview$match1.x.x.x.x.x.x),]
  # BV.MC.Entry.data.AB.forReview.male=BV.MC.Entry.data.AB.forReview.male[!duplicated(BV.MC.Entry.data.AB.forReview.male$Pedigree),]
  #write.csv(BV.MC.Entry.data.AB.forReview,"R:/Breeding/MT_TP/Models/Data/Department Data/2020/BV.MC.Entry.data.AB.forReview.csv")

  #wb<-createWorkbook(type="xlsx")
  #CellStyle(wb, dataFormat=NULL, alignment=NULL,
  #          border=NULL, fill=NULL, font=NULL)
  #Male <- createSheet(wb, sheetName = "Male")
  #Female <- createSheet(wb, sheetName = "Female")
  #addDataFrame(data.frame(BV.MC.Entry.data.AB.forReview.female),Female, startRow=1, startColumn=1,row.names=F)
  #addDataFrame(data.frame(BV.MC.Entry.data.AB.forReview.male),Male, startRow=1, startColumn=1,row.names=F)
  #saveWorkbook(wb, "R:/Breeding/MT_TP/Models/Data/Department Data/2020/BV.MC.Entry.data.AB.forReview.xlsx")
  #rm(BV.MC,linked.peds)
  invisible(gc(reset=T)) #cleans memory "garbage collector"

  ########################################################################################################################
  #############################################REVIEW FINAL SHEET BEFORE CONTINUING#######################################
  ########################################################################################################################
  #BV.MC.Entry.data.AB.DONE = read.xlsx("R:/Breeding/MT_TP/Models/Data/Department Data/2020/BV.MC.Entry.data.AB.forReview.xlsx")
  BV.MC.Entry.data.AB.DONE = BV.MC.Entry.data.AB[,-c(27,28,9:13)]
  colnames(BV.MC.Entry.data.AB.DONE)[c(22,23)] = c("unique_male_ID","unique_female_ID")
  ######convert Industry names to Becks names

  named<-names(BV.MC.Entry.data.AB.DONE[,c(22,23)]); named
  BV.MC.Entry.data.AB.DONE=data.frame(BV.MC.Entry.data.AB.DONE)
  industryNames = InbredNameLibrary()
  industryNames = industryNames[[2]]

  for(i in named){
    BV.MC.Entry.data.AB.DONE[,i] <- suppressWarnings(suppressMessages(plyr::revalue(as.character(BV.MC.Entry.data.AB.DONE[,i]), industryNames )))  #industry name to inbred name conversion

  }


  BV.MC.Entry.data.AB.DONE$unique_pedigree <- paste0(BV.MC.Entry.data.AB.DONE$unique_female_ID," + ",BV.MC.Entry.data.AB.DONE$unique_male_ID)

  #### ##############################################################################################################
  ##################################################  DATA VISUAL AND ASReml!!!    #################################
  ##################################################################################################################
  cat("K")

  #Graphs of data

  attach(BV.MC.Entry.data.AB.DONE) # attached dataset so I dont need to address the dataset in the functions
  #hist(`# Seed`, col="gold") #distrubiton on all values
  #hist( Yield, col="green") #distrubtion on all values
  #hist(`Y/M`, col="gold") #distrubiton on all values
  #hist(`Test WT`, col="green") #distrubtion on all values
  #hist(`StandCnt (UAV)`, col="gold") #distrubiton on all values
  #hist(`StandCnt (Final)`, col="green") #distrubtion on all values
  #hist(`SL Count`, col="gold") #distrubiton on all values
  #hist(`SL %`, col="green") #distrubtion on all values
  #hist(`RL Count`, col="gold") #distrubiton on all values
  #hist(`RL %`, col="green") #distrubtion on all values
  ##hist(`Plt Height`, col="gold") #distrubiton on all values
  #hist(`PCT HOH`, col="green") #distrubtion on all values
  #hist(`GS Late`, col="gold") #distrubiton on all values
  #hist(`EarHt`, col="green") #distrubtion on all values
  ######Rename variables for ease of use
  #BV.HSIdentical$GDU_slk50 = as.numeric(yld)
  #BV.HSIdentical$GDU_shd50 = as.numeric(obs_shd)
  BV.MC.Entry.data.AB.DONE$YEAR = gsub(x=`Book.Season`,pattern="S: Corn","")
  BV.MC.Entry.data.AB.DONE$YEAR = as.numeric(BV.MC.Entry.data.AB.DONE$YEAR)
  BV.MC.Entry.data.AB.DONE$YEAR = 2000+BV.MC.Entry.data.AB.DONE$YEAR
  #BV.MC.Entry.data.AB.DONE$YEAR = as.factor(YEAR)
  BV.MC.Entry.data.AB.DONE$LINE = as.factor(unique_pedigree)
  BV.MC.Entry.data.AB.DONE$FIELD = as.factor(`Book.Name`)
  BV.MC.Entry.data.AB.DONE$EXP = as.factor(`Entry.Book.Name`)
  #BV.HSIdentical$EBN = as.factor(`entry_book_name`)
  #BV.MC.Entry.data.AB.DONE$REP = as.factor(`User Rep`)
  #BV.MC.Entry.data.AB.DONE$CHECK = as.factor(Check)
  #BV.MC.Entry.data.AB.DONE$CHECK.RM = as.factor(`RecId`)
  #BV.MC.Entry.data.AB.DONE$InvRecId = as.factor(`Inv.RecId`)
  #BV.MC.Entry.data.AB.DONE$RecId = as.factor(RecId)
  #BV.MC.Entry.data.AB.DONE$VARIETY = as.factor(Variety)
  BV.MC.Entry.data.AB.DONE$MALE = as.factor(unique_male_ID)
  BV.MC.Entry.data.AB.DONE$FEMALE = as.factor(unique_female_ID)
  #BV.HSIdentical.mt <- BV.HSIdentical %>% filter(loc == "Marshalltown") #filter only marshalltown lines
  #BV.HSIdentical.olivia <- BV.HSIdentical %>% filter(loc == "Olivia") #filter only olivia lines
  head(BV.MC.Entry.data.AB.DONE)
  dim(BV.MC.Entry.data.AB.DONE)
  #head(BV.HSIdentical.mt)
  #head(BV.HSIdentical.olivia)
  detach(BV.MC.Entry.data.AB.DONE)

  #BV.MC.Entry.data.AB.DONE = data.table(BV.MC.Entry.data.AB.DONE)
  #colnames(BV.MC.Entry.data.AB.DONE)[c(17,19)] = c("RL.Per","SL.Per")
  #BV.MC.Entry.data.AB.DONE.field.exp = aggregate(BV.MC.Entry.data.AB.DONE[,c( "EarHt","Plt.Height","RL.Per","RL.Count","SL.Per",
  #                                                         "SL.Count","StandCnt..Final.","StandCnt..UAV.","Test.WT","Y.M" ,
  #                                                         "Yield")],by=list(LINE = BV.MC.Entry.data.AB.DONE$LINE,
  #                                                                           FIELD = BV.MC.Entry.data.AB.DONE$FIELD,
  #                                                                           EXP = BV.MC.Entry.data.AB.DONE$EXP,
  #                                                                           MALE = BV.MC.Entry.data.AB.DONE$MALE,
  #                                                                           FEMALE = BV.MC.Entry.data.AB.DONE$FEMALE), mean,trim=0.1, na.rm=T); dim(BV.MC.Entry.data.AB.DONE.field.exp); BV.MC.Entry.data.AB.DONE.field.exp=data.frame(BV.MC.Entry.data.AB.DONE.field.exp)
  #
  BV.MC.Entry.data.AB.DONE = left_join(BV.MC.Entry.data.AB.DONE, PD, by="RecId")

  BV.HSIdentical.df = data.frame(BV.MC.Entry.data.AB.DONE) #choose your dataset for the model
  # write.table(BV.HSIdentical.df,paste0(fdp,
  #                                      # if(A){print("A")},
  #                                      # if(B){print("B")},
  #                                      # if(C){print("C")},
  #                                      # if(Prop){print("Prop")},
  #                                      # if(Choice){print("Choice")},
  #                                      # if(D){print("D")},
  #                                      # if(E){print("E")},
  #                                      # if(Q){print("Q")},
  #                                      # if(R){print("R")},
  #                                      # if(V){print("V")},
  #                                      # "."
  #                                      # if(s0){print("S0")},
  #                                      # if(s1){print("S1")},
  #                                      # if(s2){print("S2")},
  #                                      # if(s3){print("S3")},
  #                                      # if(s4){print("S4")},
  #                                      # if(s5){print("S5")},
  #                                      #
  #                                      "BV.diallel.readyforASReml.csv"))


  print("Writing final dataset for ASReml")
  #destfile20 = paste0(fdp,"/BV.diallel.readyforASReml.csv")
  #if(doWriteFinalPedigrees){

  #write.table(BV.HSIdentical.df,paste0(fdp,"/BV.diallel.readyforASReml.csv"),na="",row.names=F)
  #}

  #proc.time() - ptm

  rm(BV.MC.Entry.data.AB, BV.MC.Entry.data.AB.checkfilter, BV.MC.Entry.data.AB.checkfilter.variety, BV.MC.Entry.data.AB.checkfilter.variety.duplciate,
     BV.MC.Entry.data.AB.checkfilter.variety.duplciate.ped, BV.MC.Entry.data.AB.clean, BV.MC.Entry.data.AB.DONE, BV.MC,BV.MC.Inbred,newData,newData.changed,
     newDataRmDups,PD, BV.MC.Entry.data.AB.NAcountFemale,BV.MC.Entry.data.AB.NAcountMale,linked.peds, BV.MC.Entry.data.AB.review.x,
     BV.MC.Entry.data.AB.review.y,  BV.MC.Entry.data.AB.review.xna, BV.MC.Entry.data.AB.review.yna)
  invisible(gc())
  # sink(type="message")
  # close(zz)
  return(data.frame(BV.HSIdentical.df))
}



