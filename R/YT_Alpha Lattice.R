######################################################################################################
######Rscript for Alpha Lattice Designs
######################################################################################################
#rm(list = ls()) #remove environment vairalbes
#invisible(gc(reset = T)) #cleans memory "garbage collector"
#memory.limit(size = 8071)
#setwd("R:/Breeding/MT_TP/Models/QTL")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################
#library("dplyr")
#library("DiGGer")
#library("agricolae")

######################################################################################################
####output and input files

fdp = "C:/Users/jake.lamkey/Documents/"

#load function to serpintine data becuase prism loads like this. This wont change the order it will only load correctly into prism to match this output.
serpentine <- function(x, columnwise=F){
  if (columnwise) {
    odd <- x[, seq(1, by=2, length.out = ncol(x)/2)] # odd x
    rev_even <- x[, seq(from = 2, 
                        by=2, 
                        length.out = (ifelse((ncol(x)%%2 != 0), 
                                             ((ncol(x)/2)-1), 
                                             (ncol(x)/2))))][seq(dim(x)[1],1),] # or, even[rev(1:nrow(x)),] # reversed even x
    alternate_cbind <-  cbind(odd, rev_even)[, order(c(seq(ncol(odd)), 
                                                       seq(ncol(rev_even))))]
    return(alternate_cbind)}
  else {
    odd <- x[seq(1, by=2, length.out = nrow(x)/2),] # odd x
    rev_even <- x[seq(from = 2, by=2, length.out = (ifelse((nrow(x)%%2 != 0), 
                                                           ((nrow(x)/2)-1), 
                                                           (nrow(x)/2)))), ][, seq(dim(x)[2],1)] # or, even[, rev(1:ncol(x))] # reversed even x
    alternate_rbind <-  rbind(odd, rev_even)[order(c(seq(nrow(odd)), 
                                                     seq(nrow(rev_even)))), ]
    return(alternate_rbind)
  }
}
serpentine_sorter <- function(x, ncols, byrow = F, columnwise = T){
  
  # Arguments
  # x   vector which will be re-ordered
  # ncols   An integer specifying number of columns in design matrix where x belongs to
  # byrow   Logical: is the design matrix order filled column-level wise(_first_) and row-level wise(_second_)?
  #         Note that here, the name of argument is somewhat misleading. Look [sorting](./sorting.R) example
  #         to learn how sorting is done with multiple column specifications.
  # columnwise  Logical: Should the serpentine generation be done column wise or row wise?
  
  as.vector(t(serpentine(matrix(rev(x), ncol = ncols, 
                                byrow = byrow)[, ncols:1],
                         columnwise = columnwise)))
}

serpentineOdd <- function(x, columnwise=F){
  if (columnwise) {
    odd <- x[, seq(1, by=2, length.out = ncol(x)/2)] # odd x
    rev_even <- x[, seq(from = 2, 
                        by=2, 
                        length.out = (ifelse((ncol(x)%%2 != 0), 
                                             ((ncol(x)/2)-1), 
                                             (ncol(x)/2))))][seq(dim(x)[1],1),] # or, even[rev(1:nrow(x)),] # reversed even x
    alternate_cbind <-  cbind(odd, rev_even)[, order(c(seq(ncol(odd)), 
                                                       seq(ncol(rev_even))))]
    return(alternate_cbind)}
  else {
    odd <- x[seq(1, by=2, length.out = nrow(x)/2),] # odd x
    rev_even <- x[seq(from = 2, by=2, length.out = (ifelse((nrow(x)%%2 != 0), 
                                                           ((nrow(x)/2)-1), 
                                                           (nrow(x)/2)))), ][, seq(dim(x)[2],1)] # or, even[, rev(1:ncol(x))] # reversed even x
    alternate_rbind <-  rbind(odd, rev_even)[order(c(seq(nrow(odd)), 
                                                     seq(nrow(rev_even)))), ]
    return(alternate_rbind)
  }
}
serpentine_sorterOdd <- function(x, ncols, byrow = F, columnwise = T){
  
  # Arguments
  # x   vector which will be re-ordered
  # ncols   An integer specifying number of columns in design matrix where x belongs to
  # byrow   Logical: is the design matrix order filled column-level wise(_first_) and row-level wise(_second_)?
  #         Note that here, the name of argument is somewhat misleading. Look [sorting](./sorting.R) example
  #         to learn how sorting is done with multiple column specifications.
  # columnwise  Logical: Should the serpentine generation be done column wise or row wise?
  
  as.vector(t(serpentineOdd(matrix(x, ncol = ncols, 
                                byrow = byrow)[, ncols:1],
                         columnwise = columnwise)))
}
#```
#Alpha-lattice design
#Patterson and Williams orginal design
#Where reps = 2 or 3, 
#```{r}

#trt<-1:48
#t <- length(trt)
# size block k
#k<-4
# Blocks s
#s<-t/k

# replications r
#r=3
#outdesign<- design.alpha(trt, k, r, serie=2)
#outdesign$book

#```

###################################################
#Function Spatial IB - REP with one rep per location
###################################################
#These are balanced lattice designs and are resolvable (incomplete blocks grouped together that are complete) blocks which is equivalent to an alpha-lattice design. This uses an optimial design to find the highest efficency and balanced over mulitple reps.
#```{r}

oneRepIBDSpatial = function(numOfReps, numOfEntry, ebn, numOfChecks){
  
  numOfReps = numOfReps  #reps of locations
  numOfEntry = numOfEntry #entry number * reps per location
  expDimRow = numOfEntry / 12 #num of rows with replicated design
  numOfRows = numOfReps * expDimRow
  numOfRows
  numOfChecks = numOfChecks
  numOfCheckMinEntry = numOfEntry - numOfChecks #subtrace checks from entries
  initialDesign = matrix()
  repIsOnePerLoc <- ibDiGGer(numberOfTreatments = numOfEntry,
                             rowsInDesign = numOfRows, columnsInDesign = 12, # 56 * 12 / 48 = number of reps of each treat
                             rowsInRep = expDimRow, columnsInRep = 12, # diminsions entry list to put into field
                             rowsInBlock = 1, columnsInBlock = 12, # same diminsions as number of columns in design
                             maxInterchanges = 1000000,
                             searchIntensity = 100,
                             #targetAValue = 0.07470644,
                             #rngSeeds = c(11301, 29798)
  )
  #pr122 <- run(repIsOnePerLoc)
  desPlot(getDesign(repIsOnePerLoc), trts=1:numOfCheckMinEntry, new=TRUE, label=TRUE,col='tan')
  #desPlot(getDesign(repIsOnePerLoc), trts=numOfCheckMinEntry+1, new=FALSE, label=TRUE, col=5)
  #desPlot(getDesign(repIsOnePerLoc), trts=numOfCheckMinEntry+2, new=FALSE, label=TRUE, col=6)
  desPlot(getDesign(repIsOnePerLoc), trts=(numOfCheckMinEntry+1):numOfEntry, new=FALSE, label=TRUE, col=7,
          bdef=cbind(expDimRow,12), bcol=3, bwd=2)
  
  iswapOneReps <- getDesign(repIsOnePerLoc)
  iswapOneReps[iswapOneReps > numOfChecks] <- numOfChecks+1
  corRepIsOnePerLoc <- corDiGGer(
    numberOfTreatments=numOfEntry, 
    rowsInDesign=numOfRows, 
    columnsInDesign=12,
    rowsInReplicate = expDimRow,
    columnsInReplicate = 12,
    #treatRepPerRep= rep(c(1,1,1,1), c(numOfCheckMinEntry,1,1,1)),
    treatRep = rep(c(rep(1,numOfChecks+1)), c(numOfCheckMinEntry,rep(1,numOfChecks))),
    treatGroup = rep(c((numOfChecks+1):1), c(numOfCheckMinEntry,rep(1,numOfChecks))),
    searchIntensity = 100,
    maxInterchanges = 1000000,
    blockSeq = list(c(expDimRow, 12), c(12, 1), c(1, 1)),
    spatial = F,
    #targetAValue = 0.07470644,
    #independentBlocks = list(c(expDimRow,12)),
    initialDesign = getDesign(repIsOnePerLoc),
    initialSwap = iswapOneReps, rngState = repIsOnePerLoc$.rng)
  
  desPlot(getDesign(corRepIsOnePerLoc), trts = 1:numOfCheckMinEntry, new=TRUE, label=TRUE,col='tan')
  #desPlot(getDesign(corRepIsOnePerLoc), trts = numOfCheckMinEntry+5, new=FALSE, label=TRUE, col=5)
  #desPlot(getDesign(corRepIsOnePerLoc), trts = numOfCheckMinEntry+2, new=FALSE, label=TRUE, col=6)
  desPlot(getDesign(corRepIsOnePerLoc), trts = (numOfCheckMinEntry+1):numOfEntry, new=FALSE, label=TRUE, col=7,bdef=cbind(expDimRow,12), bcol=3, bwd=2)
  
  onePerLoc = repIsOnePerLoc$dlist
  
  onePerLocSpatial = corRepIsOnePerLoc$dlist
  onePerLocSpatial$`Entry Book Name` = ebn
  
  onePerLocSpatial = onePerLocSpatial[order(onePerLocSpatial$ROW),]
  onePerLocSpatial = onePerLocSpatial[order(onePerLocSpatial$REP),]
  colnames(onePerLocSpatial)[c(3,7,6)] = c("Entry #", "Plot #", "User Rep")
  
  onePerLocSpatial$`PlotOrig` = rep(1:numOfEntry, numOfReps)
  
  onePerLocSpatialSerp = serpentine_sorter(onePerLocSpatial[,c(5)],ncols=12, byrow=T,columnwise=F)
  
  onePerLocSpatialSerpOdd = serpentine_sorterOdd(onePerLocSpatial[,c(5)],ncols=12, byrow=T,columnwise=F)
  
  #onePerLocSpatialSerpEvenRepRemoved = onePerLocSpatialSerp %>% filter()
  
  
  
  onePerLocSpatialMod = onePerLocSpatial
  onePerLocSpatialMod$RANGE = onePerLocSpatialSerp
  onePerLocSpatialMod$RANGEODD = onePerLocSpatialSerpOdd
  
  if((expDimRow %% 2)==0){
    print(paste0(expDimRow,"is Even"))
  }else{
    print(paste0(expDimRow,"is Odd"))
    for(i in c(2,4,6,8,10,12)){
      onePerLocSpatialModEvenRepRemoved = onePerLocSpatialMod %>% filter(`User Rep` == i)
      onePerLocSpatialModEvenRepRemoved = onePerLocSpatialModEvenRepRemoved[,"RANGEODD"]
      #onePerLocSpatialModEvenRepRemoved = data.frame(onePerLocSpatialModEvenRepRemoved)
      #colnames(onePerLocSpatialModEvenRepRemoved) = "Range"
      
      onePerLocSpatialMod$RANGE[ onePerLocSpatialMod$'User Rep' == i] <- onePerLocSpatialModEvenRepRemoved
      
      
      
      rm(onePerLocSpatialModEvenRepRemoved)
      gc()
    }
    
  }
  
  
  
  onePerLocSpatialRev <- onePerLocSpatial %>%
    group_by(`User Rep`) %>% 
    mutate(ROW = setNames(rev(unique(ROW)),
                          unique(ROW))[as.character(ROW)]) %>%
    ungroup
  
  onePerLocSpatialMod$ROW = onePerLocSpatialRev$ROW
  onePerLocSpatialMod$`Plot #` = rep(1:numOfEntry, numOfReps)
  #onePerLocSpatialMod = onePerLocSpatialMod[,c(3,4,5,6,7,10)]
  #onePerLocSpatial = onePerLocSpatial[,c(3,4,5,6,11)]
  onePerLocSpatialDone = left_join(onePerLocSpatial, onePerLocSpatialMod, by=c("RANGE","ROW"))
  #onePerLocSpatialDone = onePerLocSpatialDone[,c(1,4,8,9)]
  onePerLocSpatialDone = onePerLocSpatialDone[,c("Entry #.x","User Rep.x","Plot #.y","Entry Book Name.x")]
  
  colnames(onePerLocSpatialDone)[c(1,2,3,4)] = c("Entry #","User Rep","Plot #","Entry Book Name")
  #onePerLocSpatialDone$`User Rep` = gsub(x=onePerLocSpatialDone$`User Rep`, pattern="1", replacement="5")
  #onePerLocSpatialDone$`User Rep` = gsub(x=onePerLocSpatialDone$`User Rep`, pattern="2", replacement="6")
  
  #onePerLocSpatialDone = serp
  write.csv(onePerLocSpatialDone, paste0(fdp,ebn,"_AL.csv"),row.names=F,na="")
  
  return(list(onePerLocSpatial = onePerLocSpatialDone, onePerLoc = onePerLoc ))
  
}


#```
###################################################
#Function Spatial IB - REP with two reps per location, not prism ready
###################################################
#```{r}
twoRepIBDSpatial = function(numOfReps, numOfEntry){
  
  numOfReps = 11  #reps of locations
  numOfEntry = 18
  numOfEntry = numOfEntry * 2 #entry number * reps per location
  expDimRow = numOfEntry / 12 #num of rows with replicated design
  numOfRows = numOfReps * expDimRow
  numOfRows
  numOfCheckMinEntry = numOfEntry - 6
  numOfCheckMinEntry.rep = numOfCheckMinEntry/2
  
  repIsTwoPerLoc <- ibDiGGer(numberOfTreatments = numOfEntry,
                             # rowsInDesign * columnsInDesign / numberOfTreatments = number of reps of each treat
                             rowsInDesign = numOfRows, columnsInDesign = 12, 
                             # 11 reps
                             rowsInRep = expDimRow, columnsInRep = 12, 
                             # diminsions entry list to put into field
                             rowsInBlock = 1, columnsInBlock = 12, 
                             # same diminsions as number of columns in design
                             treatGroup = rep(c(1,2,3,3,4,4,5,5), c(numOfCheckMinEntry.rep,numOfCheckMinEntry.rep,1,1,1,1,1,1)),
                             maxInterchanges = 10000000,
                             #targetAValue = 0.387097,
                             searchIntensity =100,
                             #rngSeeds = c(11301, 29798)
  )
  
  desPlot(getDesign(repIsTwoPerLoc), trts=1:numOfCheckMinEntry.rep, new=TRUE, label=FALSE,col='lightcyan1')
  desPlot(getDesign(repIsTwoPerLoc), trts=c((numOfCheckMinEntry.rep+1): numOfCheckMinEntry), 
          new=FALSE, label=FALSE, col='lightgreen')
  desPlot(getDesign(repIsTwoPerLoc), trts=c(numOfCheckMinEntry+1, numOfCheckMinEntry+2), 
          new=FALSE, label=TRUE, col=5)
  desPlot(getDesign(repIsTwoPerLoc), trts=c(numOfCheckMinEntry+3, numOfCheckMinEntry+4), 
          new=FALSE, label=TRUE, col=5)
  desPlot(getDesign(repIsTwoPerLoc), trts=c(numOfCheckMinEntry+5, numOfCheckMinEntry+6), 
          new=FALSE, label=TRUE, col=6,
          bdef=cbind(expDimRow,12), bcol=4, bwd=4)
  
  
  iswaptwoReps <- getDesign(repIsTwoPerLoc)
  iswaptwoReps[iswaptwoReps > 7] <- 8
  corRepIsTwoPerLoc <- corDiGGer(
    numberOfTreatments=numOfEntry, 
    rowsInDesign=numOfRows, 
    columnsInDesign=12,
    rowsInReplicate = expDimRow,
    columnsInReplicate = 12,
    treatRep = rep(c(1,1,1,1,1,1,1,1),
                   c(numOfCheckMinEntry.rep,numOfCheckMinEntry.rep,1,1,1,1,1,1)),
    treatGroup = rep(c(1,2,3,3,4,4,5,5), c(numOfCheckMinEntry.rep,numOfCheckMinEntry.rep,1,1,1,1,1,1)),
    #searchIntensity = 100,
    #maxInterchanges = 1000000,
    blockSeq = list(c(expDimRow,12),c(numOfReps,1)),
    #independentBlocks = list(c(expDimRow,6)),
    initialDesign = getDesign(repIsTwoPerLoc),
    initialSwap = iswaptwoReps, rngState = repIsTwoPerLoc$.rng)
  
  desPlot(getDesign(corRepIsTwoPerLoc), trts= c(1:numOfCheckMinEntry.rep), 
          new=TRUE, label=FALSE ,col='lightcyan1')
  desPlot(getDesign(corRepIsTwoPerLoc), trts= c((numOfCheckMinEntry.rep+1): numOfCheckMinEntry), 
          new=FALSE, label=FALSE, col='lightgreen')
  
  desPlot(getDesign(corRepIsTwoPerLoc), trts= c(numOfCheckMinEntry+1, numOfCheckMinEntry+2), 
          new=FALSE, label=TRUE, col=2)
  desPlot(getDesign(corRepIsTwoPerLoc), trts= c(numOfCheckMinEntry+3, numOfCheckMinEntry+4), 
          new=FALSE, label=TRUE, col=4)
  desPlot(getDesign(corRepIsTwoPerLoc), trts= c(numOfCheckMinEntry+5, numOfCheckMinEntry+6), 
          new=FALSE, label=TRUE, col=6,
          bdef=cbind(expDimRow,12), bcol=4, bwd=4)
  
  
  repIsTwoPerLoc.plot = repIsTwoPerLoc$dlist
  repIsTwoPerLoc.group = repIsTwoPerLoc$treatment
  
  twoPerLoc = left_join(repIsTwoPerLoc.plot, repIsTwoPerLoc.group, by = c("ID"="ID"))
  
  twoPerLocSpatial.plot = corRepIsTwoPerLoc$dlist
  twoPerLocSpatial.group = corRepIsTwoPerLoc$treatment
  
  twoPerLocSPatial = left_join(twoPerLocSpatial.plot, twoPerLocSpatial.group, by = c("ID"="ID"))
  
  return(list(twoPerLocSPatial=twoPerLocSPatial , twoPerLoc=twoPerLoc))
}

#```



