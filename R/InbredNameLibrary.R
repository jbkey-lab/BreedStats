#levels(qualdat$InbName)#LEvels before
######convert Industry names to Becks names

InbredNameLibrary=function(){
  patterns=c("\\/",	           
    "\\%",	                     
    "\\:",	            
    "\\*",	             
    "\\."           
  )
  industryNames=c()
  return(list(patterns,industryNames))

}



#industry name to inbred name conversion
#levels(as.factor(qualdat$InbName)) #Levels After
#levels(as.factor(qualdat$loc))
