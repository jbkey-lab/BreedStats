
InventoryPedigreeAdj = function(date){

  BV.Entry = data.table::fread(paste0("P:/Temp/PedAdjust ",date,".csv"))
  colnames(BV.Entry)[8] = "Inv.RecId"

  BV.Entry = data.frame(BV.Entry)
  #BV.Entry = data.table(BV.Entry)
  colnames(BV.Entry)[4] = "Check"

  BV.Entry$LocID = ""
  BV.Entry$Range = ""
  BV.Entry$Row = ""
  BV.Entry$RecId = ""
  BV.Entry$Plot.Discarded = ""
  BV.Entry$Plot.Status = ""
  BV.Entry$Treatments.Applied = ""
  BV.Entry$X..Seed = ""
  BV.Entry$Date.Harvested = ""
  BV.Entry$Date.Planted = ""
  BV.Entry$EarHt = ""
  BV.Entry$GS.Late = ""
  BV.Entry$GS.Rating = ""
  BV.Entry$PCT.HOH = ""
  BV.Entry$Plot.Length = ""
  BV.Entry$PLOT.WT = ""
  BV.Entry$Plt.Height = ""
  BV.Entry$RL.. = ""
  BV.Entry$RL.Count = ""
  BV.Entry$SL.. = ""
  BV.Entry$SL.Count = ""
  BV.Entry$StandCnt..Early. = ""
  BV.Entry$StandCnt..Final. = ""
  BV.Entry$StandCnt..UAV. = ""
  BV.Entry$Test.WT = ""
  BV.Entry$Y.M = ""
  BV.Entry$Yield = ""


  BV.col.names = c(
    "RecId"  ,            "LocID" ,    "Book.Season"  ,
    "Entry.Book.Name"   , "Book.Name"          ,"Check" ,
    "RepNo"              ,"User.Rep"           ,"Range"  ,
    "Row"                ,"Entry.."            ,"Inv.RecId" ,
    "Plot.Discarded"     ,"Plot.Status"        ,"Variety"     ,
    "Pedigree"           ,"Source.ID"          ,"Ped.Column" ,
    "Comments"           ,"Comments.Breeder"   ,"Comments.Product" ,
    "Comments.Testing"   ,"Male.Pedigree"      ,"Female.Pedigree"  ,
    "Hybrid.ID"          ,"Treatments.Applied" ,"Trait.Package" ,
    "Line.ID"            ,"X..Seed"          ,  "Check.RM"   ,
    "Date.Harvested"     ,"Date.Planted"   ,    "EarHt"       ,
    "GS.Late"            ,"GS.Rating"     ,     "PCT.HOH"     ,
    "Plot.Length"        ,"PLOT.WT"       ,     "Plt.Height" ,
    "RL.."               ,"RL.Count"     ,      "RM"         ,
    "SL.."               ,"SL.Count"      ,     "StandCnt..Early."  ,
    "StandCnt..Final."   ,"StandCnt..UAV." ,    "Test.WT"    ,
    "Y.M"                ,"Yield")



  BV.Entry=BV.Entry[,BV.col.names]
  BV.Entry$Entry.Book.Name = paste0("TP21A",  BV.Entry$Entry.Book.Name)

  BV.Entry$Variety = as.character(BV.Entry$Variety)
  BV.Entry$Comments.Breeder = as.character(BV.Entry$Comments.Breeder)
  BV.Entry$Ped.Column = as.character(BV.Entry$Ped.Column)
  BV.Entry$User.Rep = as.character(BV.Entry$User.Rep)


  #BV.Entry$RecId = c(99900001: (nrow(BV.Entry)+99900000))
  BV.Entry$RecId = BV.Entry$Inv.RecId
  BV.Entry[ is.na(BV.Entry)] <- ""

  str(BV.Entry)

  rm(BV.col.names, date)
  gc()
  return(BV.Entry)
}




#BV.MC.Entry = InventoryPedigreeAdj(date="12_22_2021")











