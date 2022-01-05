# BV.MC.Inbred <- read.csv(paste0("R:/Breeding/MT_TP/Models/Data/Department Data/NEW LINE CODES.csv"))
# BV.MC.Inbred$Pedigree_Backup = BV.MC.Inbred$PEDIGREE
# BV.MC.Inbred = BV.MC.Inbred[,c(1:3,22,4:21)]
# BV.MC.Inbred$PEDIGREE = as.character(BV.MC.Inbred$PEDIGREE)
# BV.MC.Inbred$Pedigree_Backup = as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred$Pedigree_Backup1 = as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred$Pedigree_Backup2= as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred$Pedigree_Backup3 = as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred$Pedigree_Backup4 = as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred$Pedigree_Backup5 = as.character(BV.MC.Inbred$Pedigree_Backup)
# BV.MC.Inbred = BV.MC.Inbred[,c(1:4,22:27,5:21)]

#BV.MC.Inbred$Pedigre_Backup = as.character(BV.MC.Inbred$Pedigre_Backup)
#BV.MC.Inbred$PEDIGREE = as.character(BV.MC.Inbred$PEDIGREE)

#data = BV.MC.Inbred
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


pedigreeReduce = function(data, Codes){
  if(Codes){
    data = data
    data$match = gsub(data$PEDIGREE, pattern = "HX1", replacement = "Hx1")
    
    female.inbred = "([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})"
    male.inbred = "([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})"
    
    generation = "(?:-[[:alnum:]]+){3}"
    generation1 = "(?:-[[:alnum:]]+){1}"
    generation2 = "(?:-[[:alnum:]]+){2}"
    generation3 = "(?:-[[:alnum:]]+){0,6}"
    generation4 = "(?:-[[:alnum:]]+){4}"
    
    addgen = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6}"
    addgen2 = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    staraddgen = "[\\(]{0,6}[?:\\*[[:alnum:]]+]{0,20}[\\)]{0,6}"
    decaddgen = "[\\(]{0,6}[?:\\.[[:alnum:]]+]{0,20}[\\)]{0,6}"
    decaddgen.1 = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    conv1 = paste0("^",female.inbred, "/", male.inbred, generation)
    
    conv1.1 = paste0("^",female.inbred, "/", male.inbred, generation2,"$")
    
    #BDA015/BDA032//BHH069)-B-093-1-1	
    conv3 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen, generation)
    
    conv3.1 = paste0("^",female.inbred, "/{1,2}", male.inbred, "[\\)]{0,6}",generation3,"[\\)]{0,6}/{1,2}", addgen,"/",addgen, generation2)
    
    
    conv2 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen)
    
    
    #I10516/PHJ33//PHN82/I10516)-B-040-3-1-1-02	
    conv4 = paste0("^",female.inbred, "/", male.inbred, "//", addgen, "/", addgen, generation)
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP021)-B-01	
    conv4.1 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation2)
    
    #LH195*2/DK2FACC)-B-B-B-0081-01-1-B	
    conv5 = paste0("^",staraddgen, "/", male.inbred, generation)
    conv6 = paste0("^",female.inbred, "/", male.inbred, "//", addgen, "/", addgen)
    conv7 = paste0("^",female.inbred, "x", male.inbred, generation)
    
    
    #PHAW6/PHN82//PHAW6/PHJ33)-007-03-2///P02/R03)-B-057-1-2)-B-002	
    conv8 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen, generation3,"[\\)]{0,6}",generation2 )
    #(BJH074/I11063)/(PHJ40/I7016)-10/I7016)-B-B-69-1-2-B))-B-B-001	
    conv9 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation3,"[\\)]{0,6}",generation2 )
    #((5020/MM501D)/I10516-39.4)/I10516-1-1-1	
    conv10 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation3,"[\\)]{0,6}",generation2 )
    
    #(I9005/DJ7)-B-040-1-1)/(I9005/ICI740)-B-084-3-3)//(I9005/ICI740)-B-084-3-1-1)	
    conv3.2 = paste0("^",female.inbred, "/{0,3}", male.inbred,generation3, "[\\)]{0,6}/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen, generation2 )
    #(I9005/DJ7)-B-040-1-1)/(I9005/ICI740)-B-084-3-3))-B	
    conv3.3 = paste0("^",female.inbred, "/{0,3}", male.inbred,generation3, "[\\)]{0,6}/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}",generation2 )
    
    #((065125/054245)/RS710-39.09)/065125-1-4-4	
    conv11 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, decaddgen,"/{0,3}", decaddgen.1,generation )
    #((I5009/I7016.076-2-1-1-2)/R03)/(I5009/I7016.088-2-1)	
    conv11.1 = paste0("^",female.inbred, "/{0,3}", decaddgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen,"/{0,3}", decaddgen,"/{0,3}", decaddgen.1,generation2,"\\)" )
    
    
    #(046358/CI6621)/054530-B-45-1-1		
    conv19 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation2)
    
    #(PHP02/054530)/046358-41	
    conv19.1 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation2)
    
    
    conv19.2 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation)
    
    conv19.3 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen, generation2)
    
    #(046358/MM501D)/(046358/I10516)-01-4	
    conv20 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", female.inbred, "/{1,2}", male.inbred, generation3)
    
    
    # str_detect(p, pattern = conv1)
    # str_match(pattern = conv1, p)
    # str_detect(data[i,2], pattern = conv19.3)
    # str_match(pattern = conv19.3, data[i,2])
    # 
    data$Proccessed = ""
    
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        #if(!str_detect(data[i,3], pattern = conv1.1)){
        if(!str_detect(data[i,3], pattern = conv3)){
          if(!str_detect(data[i,3], pattern = conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          
                          
                          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
                            
                            if(str_detect(data[i,3], pattern = conv1)){
                              data[i,24] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern = conv1, data[i,3])
                              data[i,7] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}} }}}}}}}}}}
    
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv3)){
          if(!str_detect(data[i,3], pattern = conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(!grepl(data[i,3], pattern ="\\:")){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            
                            
                            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==1){
                              
                              replacement = str_match(pattern = conv1, data[i,3])
                              data[i,24] = "Proccessed" # add a period for using wildcards
                              
                              if(!is.na(replacement)){
                                
                                if(str_detect(data[i,3], pattern = fixed(replacement[1]))){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  data[i,8] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}} }}}}}}}}}}}#}
    
    
    
    #BDA015/BDA032//BHH069)-B-093-1-1	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          #if(str_detect(data[i,3], pattern =conv2)){
          if(!str_detect(data[i,3], pattern =conv3.1)){
            if(str_detect(data[i,3], pattern =conv3)){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          #if(!str_detect(data[i,3], pattern = conv3.2)){
                          #if(!str_detect(data[i,3], pattern = conv3.3)){
                          if(!grepl(data[i,3], pattern = "\\*")){
                            
                            
                            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==2 ){
                              
                              if(str_detect(data[i,3], pattern = conv3)){
                                data[i,24] = "Proccessed" # add a period for using wildcards
                                
                                replacement = str_match(pattern = conv3, data[i,3])
                                data[i,9] = replacement[1] # add a period for using wildcards
                                data[i,3] = replacement[1] # add a period for using wildcards
                                
                              }} }}}}}}}}}}}}
    
    #(I11063/FF6224)-B-B)/(I11063/FF6788)-14-03	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          #if(str_detect(data[i,3], pattern =conv2)){
          if(str_detect(data[i,3], pattern =conv3.1)){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!str_detect(data[i,3], pattern ="\\(GA21")){
                    #if(str_detect(data[i,3], pattern = conv1.1)){
                    if(!grepl(data[i,3], pattern ="\\:")){
                      if(!str_detect(data[i,3], pattern ="\\.")){
                        if(!str_detect(data[i,3], pattern = conv3.2)){
                          if(!str_detect(data[i,3], pattern = conv3.3)){
                            if(!grepl(data[i,3], pattern = "\\*")){
                              
                              
                              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3 ){
                                
                                if(str_detect(data[i,3], pattern = conv3.1)){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern = conv3.1, data[i,3])
                                  data[i,9] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }} }}}}}}}}}}}}}
    
    
    #I10516/PHJ33//PHN82/I10516)-B-040-3-1-1-02	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(str_detect(data[i,3], pattern = conv4)){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!str_detect(data[i,3], pattern =conv4.1)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!str_detect(data[i,3], pattern = conv8)){
                          if(!grepl(data[i,3], pattern ="\\:")){
                            if(!str_detect(data[i,3], pattern = conv9)){
                              if(!str_detect(data[i,3], pattern ="\\.")){
                                
                                
                                #if(!str_detect(data[i,3], pattern =conv3)){
                                
                                if(str_detect(data[i,3], pattern = conv4)){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern = conv4, data[i,3])
                                  data[i,11] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                } } }  }}}}}}}}}}}}
    
    # str_match(pattern = conv4.1, data.changed[i,2])
    # str_detect(data.changed[i,2], pattern =conv4.1)
    # 
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP043)-B-18	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        #if(str_detect(data[i,3], pattern = conv4)){
        if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
          if(!str_detect(data[i,3], pattern =conv7)){
            if(str_detect(data[i,3], pattern =conv4.1)){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!str_detect(data[i,3], pattern ="\\(GA21")){
                    #if(!str_detect(data[i,3], pattern =conv3)){
                    #if(!str_detect(data[i,3], pattern = conv1.1)){
                    if(!grepl(data[i,3], pattern ="\\:")){
                      if(!str_detect(data[i,3], pattern = conv9)){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          
                          
                          
                          if(str_detect(data[i,3], pattern = conv4.1)){
                            data[i,24] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern = conv4.1, data[i,3])
                            data[i,11] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          } } }  }}}}}}}}}
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP043)-B-18	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          if(!lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!str_detect(data[i,3], pattern =conv4.1)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern =conv3)){
                      if(str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern = conv9)){
                            if(!str_detect(data[i,3], pattern ="\\.")){
                              
                              
                              if(str_detect(data[i,3], pattern = conv1.1)){
                                data[i,24] = "Proccessed" # add a period for using wildcards
                                
                                replacement = str_match(pattern = conv1.1, data[i,3])
                                data[i,10] = replacement[1] # add a period for using wildcards
                                data[i,3] = replacement[1] # add a period for using wildcards
                                
                              } } }  }}}}}}}}}}}
    
    #LH195*2/DK2FACC)-B-B-B-0081-01-1-B	
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(str_detect(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==1){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            
                            
                            if(str_detect(data[i,3], pattern = conv5)){
                              data[i,24] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern=conv5, data[i,3])
                              data[i,12] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}}  }}}}}}}}}}
    
    
    #((065125/054245)/RS710-39.09)/065125-1-4-4	
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 3){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(str_detect(data[i,3], pattern ="\\.")){
                            if(str_detect(data[i,3], pattern = conv11)){
                              if(!str_detect(data[i,3], pattern = conv11.1)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv11)){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv11, data[i,3])
                                  data[i,13] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}
    #I9011-B/LH200//FX8091)-B-B-048	
    #(046358/CI6621)/054530-B-45
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19)){
                            if(!str_detect(data[i,3], pattern = conv11.1)){
                              if(!str_detect(data[i,3], pattern = conv6)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv19)){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv19, data[i,3])
                                  data[i,19] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}#}
    
    #(046358/CI6621)/054530-B
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19.1)){
                            if(!str_detect(data[i,3], pattern = conv19.2)){
                              
                              if(!str_detect(data[i,3], pattern = conv11.1)){
                                if(!str_detect(data[i,3], pattern = conv6)){
                                  
                                  
                                  if(str_detect(data[i,3], pattern = conv19.1)){
                                    data[i,24] = "Proccessed" # add a period for using wildcards
                                    
                                    replacement = str_match(pattern=conv19.1, data[i,3])
                                    data[i,19] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }}}  }}}}}}}}}}}}}#}
    
    #I9011-B/LH200//FX8091)-B-B-048
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19.2)){
                            if(!str_detect(data[i,3], pattern = conv19.1)){
                              
                              if(!str_detect(data[i,3], pattern = conv11.1)){
                                if(!str_detect(data[i,3], pattern = conv6)){
                                  
                                  
                                  if(str_detect(data[i,3], pattern = conv19.2)){
                                    data[i,24] = "Proccessed" # add a period for using wildcards
                                    
                                    replacement = str_match(pattern=conv19.2, data[i,3])
                                    data[i,19] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }}}  }}}}}}}}}}}}}
    
    #I9011-B/LH200//FX8091)-B-B-048
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19.3)){
                            if(!str_detect(data[i,3], pattern = conv19.1)){
                              if(!str_detect(data[i,3], pattern = conv19.2)){
                                if(!str_detect(data[i,3], pattern = conv19)){
                                  
                                  if(!str_detect(data[i,3], pattern = conv11.1)){
                                    if(!str_detect(data[i,3], pattern = conv6)){
                                      
                                      
                                      if(str_detect(data[i,3], pattern = conv19.3)){
                                        data[i,24] = "Proccessed" # add a period for using wildcards
                                        
                                        replacement = str_match(pattern=conv19.3, data[i,3])
                                        data[i,19] = replacement[1] # add a period for using wildcards
                                        data[i,3] = replacement[1] # add a period for using wildcards
                                        
                                      }}}  }}}}}}}}}}}}}}}
    
    #(056460/CI6621)/(I10516/CI6621)-B-10
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 3){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv20)){
                            if(!str_detect(data[i,3], pattern = conv11.1)){
                              if(!str_detect(data[i,3], pattern = conv6)){
                                #if(!str_detect(data[i,3], pattern = conv5)){
                                #if(!str_detect(data[i,3], pattern = conv3)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv20)){
                                  data[i,24] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv20, data[i,3])
                                  data[i,20] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}#}
    
    
    
    data$Matched = ifelse(data$PEDIGREE != data$match, T, F)
    # # # 
    dataF = data %>% filter(Matched== F) %>% filter(Proccessed != "Proccessed") %>% 
      filter(!grepl(".Male",PEDIGREE)) %>% filter(!grepl("\\:",PEDIGREE)) %>% filter(!grepl(".Female",PEDIGREE))
    
    
    # # # 
    dataPros = data %>% filter(Proccessed == "Proccessed")
    nrow(dataPros)
    nrow(dataPros) / nrow(data)
    
    
    
    dataSave = data
    
    
    
    
    data = dataSave
    ##################################
    #DH 
    
    #  data = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/data.txt" )
    # dataF = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataF.txt" )
    #  dataPros = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataPros.txt" )
    
    generation = "(?:-[[:alnum:]]+){3}"
    generation2 = "(?:-[[:alnum:]]+){2}"
    generation3 = "(?:\\.[[:alnum:]]+){0,6}"
    generation4 = "(?:-[[:alnum:]]+){4}"
    generation1 = "(?:-[[:alnum:]]+){1}"
    
    
    nondigitDH = "([?-][?B][\\.?]DHB[?-][[:alnum:]]{0,4}|[?-][?B][\\.?]DH[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DH-B[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DHB[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DH[?-][[:alnum:]]{0,4})"
    digitDH = "([?-]B[\\.?]DHB[[:digit:]]{0,4}|[?-]?B[[:digit:]]{0,4}[\\.?]DH[[:digit:]]{0,4}|[?-]?[\\.?]DH[[:digit:]]{0,4}-B|[?-]?[\\.?]DH[[:digit:]]{0,4})"
    nondigitDH1 = "([?-][?B]DHB[?-][[:alnum:]]{0,4}|[?-][?B]DH[?-][[:alnum:]]{0,4}|[?-]?DH-B[?-][[:alnum:]]{0,4}|[?-]?DHB[?-][[:alnum:]]{0,4}|[?-]?DH[?-][[:alnum:]]{0,4})"
    digitDH1 = "([?-]BDHB[[:digit:]]{0,4}|[?-]?B[[:digit:]]{0,4}DH[[:digit:]]{0,4}|[?-]?DH[[:digit:]]{0,4}-B|[?-]?DH[[:digit:]]{0,4})"
    
    DH1 = paste0("(", nondigitDH1, "|", digitDH1, ")")
    
    DH = paste0("(", nondigitDH, "|", digitDH, ")")
    
    decaddgen = "[\\(]{0,6}[?:\\.[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    dhString = paste0("[\\(]{0,6}[[:alnum:]]?(?:-[[:alnum:]]+){0,20}", DH,"[\\)]{0,6}")
    
    male.inbred = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    female.inbred = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    addgen = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    
    #(046358/912)/PHP02.DH-031-1	
    convDH12 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, DH)
    
    #(046358/LH185)/(046358/I10516).DH-005-1	
    convDH13 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, "/{0,3}", addgen, DH)
    
    ##((I5009/I7016.DH078-2-1)/R03)/(I5009/I7016.088-2-1)	
    ##convDH14 = paste0("^",male.inbred,"/{0,3}", female.inbred, DH,generation3,"\\){0,3}/{0,3}",generation3,"/{0,3}",generation3,"/{0,3}" ,generation3)# , "\\.",generation2  )#, generation3, "[\\)]{0,6}") #, generation2)
    
    #(054530/I10516)/I10517-B86.DH-023-B	
    convDH14 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}",addgen, DH,generation1)
    
    #(065125/LH119)/065125)-B-B.DH051-B	
    convDH15 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, generation1, DH)
    
    #(AW6/R03)/(PHN82 x I8515.DH042))-B-001
    convDH16 = paste0("^",male.inbred, "/{0,3}", female.inbred, "/{0,3}", addgen,DH, "\\){0,3}", generation2)
    
    #(I10540/I9570.DH103-1-B)/N46)-B.DHB025	
    convDH17 = paste0("^",male.inbred, "/{0,3}", female.inbred,DH,generation2,"\\){0,3}/{0,3}", addgen, digitDH)
    
    #(GEMS-0227/I9005)-B.DHB-007	
    convDH18 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH)
    convDH18.1 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH1)
    
    
    convDH30 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH, generation)
    convDH30.1 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH1, generation)
    #  str_detect(p, pattern = convDH30)
    #  str_match(pattern = convDH30, p)
    # # 
    # #(I5009/I7016.088-2-1)/(I8044/I9070.DH54-2-2)	  15
    
    #(I5009/I7016.DH088-2-1-1-1)/(I5009/I7016.088-2-1)	15
    
    #(I10540/I9570.DH103-1-B)/N46)-B.DHB025	17
    
    #(I5009/I7016.DH104)/I11063.DH017-1	16
    
    #(I8025/I5009.14-1)/I11054)-B.DHB-02	16
    
    #(I8025/I5009.14-B)/(I8025/I5009.DH008-B))-B-011	15 16
    
    #(I8025/I5009.14-B)/(I8025/I5009.DH008-B))-B.DHB-26	15 16
    
    #(I8025/I5009.DH008-1)/I11063)-B-023	16
    
    #(I9005/DJ7)-B-040-1-1)/(LFX6244/I9005.DH032-1-01-2))-B	15
    
    #(LFX6244/I9005.DH022-1-01-1)/(LFX6244/I9005.DH032-1-01-2))-B	15
    
    #(LH212/I9570A.DH020)-1)/LH185)-B.DHB-020	16
    
    #(PHK35 x PHG86.DH127-1)/BAJ808//P38.DHB44	
    
    #(PHV78/LH212)-B-B-049-1)/I12510.DHB20	
    
    #2369/I11012//I11063)-B.DHB46	15
    
    #BAJ808/HB9//P38.DHB031	15
    
    #BHF027/(BHF041/(PHK35/I7016.DH027-1-1-3)))-B-B-028	15
    
    #BJH074/(PHK29/I7016.DH039-1-1))-B	16
    
    #DSR046358/I10516//CI6621.DH-173)-01	15
    
    #I10001/BJH031)/DSR065125)-B.DH-B-060	16 17
    
    #I10509//I9543/I9560.DH031)-1-1-1	15
    
    #I10516/(DSR046358/3IIH6)-B.DHB-057-2))-B-B-004	 17 16
    
    #I10516-B/LH82//CI6621.DH-9)-01	15
    
    #I10516/LH212.DHB-002//BUR024)-B-023	15
    
    #I11503/I10516)/JC6794)-B.DH-B-21	17 16
    
    #I12003/(BHF041/(PHK35/I7016.DH027-1-1-3)))-B.DH063-1	15
    
    #I12028//I8025/I5009.DH008-2-2.DHB-60	15
    
    #I8025/I5009.DH008-1-1)/I11041-2-6)-B-B-009	16
    
    #I8514/I9524.06-2)/I10516.DH-6-01	16
    
    #I9542/I8510)-B.DH02-2)/LH185)-B-B-015	16
    
    #ID5754/I10516-4-3-B)/BQS042)-B.DH-B-019	17 16
    
    #LH198/I11063.DH077-1)-2-2)/BHF027)-B.DH-063-1
    
    #PHN82/I9505.DH072-1)/I10516.DH-19-01	16
    
    #PHN82/I9543.DH140-1-1)/I10516)-B-B-066	 16
    
    #PHR03/LH185//BUR070)-B.DHB075	15
    
    #(I5009/I7016.DH104)/I11063.DH002-2-B	16
    
    #PHN82/I9543.DH140-1-1)/I10516)-B-B-090 16
    
    
    # str_detect(dataF[i,2], pattern = convDH18.1)
    # str_match(pattern = convDH18.1, dataF[i,2])
    # BAA046/BHH069)-B.DHB-087-1
    
    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) >= 1){
              #if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) > 1){
              
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\:")){
                  #if(str_detect(data[i,3], pattern ="\\.")){
                  #if(!str_detect(data[i,3], pattern =convDH13)){
                  #if(!str_detect(data[i,3], pattern =convDH14)){
                  #if(!str_detect(data[i,3], pattern =convDH12)){
                  #if(!str_detect(data[i,3], pattern =convDH15)){
                  #if(!str_detect(data[i,3], pattern =convDH16)){
                  if(!str_detect(data[i,3], pattern =convDH17)){
                    if(str_detect(data[i,3], pattern =convDH18)){
                      
                      
                      if(str_detect(data[i,3], pattern = convDH18)){
                        data[i,24] = "Proccessed" # add a period for using wildcards
                        
                        replacement = str_match(pattern=convDH18, data[i,3])
                        data[i,18] = replacement[1] # add a period for using wildcards
                        data[i,3] = replacement[1] # add a period for using wildcards
                        
                      }}}}} }}}}}#} #} }}
    
    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(str_detect(data[i,3], pattern ="\\DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) >= 1){
              #if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) > 1){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\:")){
                  #if(str_detect(data[i,3], pattern ="\\.")){
                  if(!str_detect(data[i,3], pattern ="GEMS")){
                    #if(!str_detect(data[i,3], pattern =convDH13)){
                    # if(!str_detect(data[i,3], pattern =convDH14)){
                    #if(!str_detect(data[i,3], pattern =convDH12)){
                    # if(!str_detect(data[i,3], pattern =convDH15)){
                    if(!str_detect(data[i,3], pattern =convDH16)){
                      if(!str_detect(data[i,3], pattern =convDH17)){
                        if(str_detect(data[i,3], pattern =convDH18.1)){
                          
                          
                          if(str_detect(data[i,3], pattern = convDH18.1)){
                            data[i,24] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH18.1, data[i,3])
                            data[i,18] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}} }}}}} }}#} }}}
    
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) == 0){
            if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) == 0){
              
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\:")){
                  #if(str_detect(data[i,3], pattern ="\\.")){
                  #if(!str_detect(data[i,3], pattern =convDH13)){
                  #if(!str_detect(data[i,3], pattern =convDH14)){
                  #if(!str_detect(data[i,3], pattern =convDH12)){
                  #if(!str_detect(data[i,3], pattern =convDH15)){
                  #if(!str_detect(data[i,3], pattern =convDH16)){
                  if(!str_detect(data[i,3], pattern =convDH17)){
                    if(str_detect(data[i,3], pattern =convDH30)){
                      
                      
                      if(str_detect(data[i,3], pattern = convDH30)){
                        data[i,24] = "Proccessed" # add a period for using wildcards
                        
                        replacement = str_match(pattern=convDH30, data[i,3])
                        data[i,18] = replacement[1] # add a period for using wildcards
                        data[i,3] = replacement[1] # add a period for using wildcards
                        
                      }}}}} }}}}}  #}}
    
    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(str_detect(data[i,3], pattern ="\\DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) == 0){
              if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) == 0){
                
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    #if(str_detect(data[i,3], pattern ="\\.")){
                    if(!str_detect(data[i,3], pattern ="GEMS")){
                      #if(!str_detect(data[i,3], pattern =convDH13)){
                      # if(!str_detect(data[i,3], pattern =convDH14)){
                      #if(!str_detect(data[i,3], pattern =convDH12)){
                      # if(!str_detect(data[i,3], pattern =convDH15)){
                      if(!str_detect(data[i,3], pattern =convDH16)){
                        if(!str_detect(data[i,3], pattern =convDH17)){
                          if(str_detect(data[i,3], pattern =convDH30.1)){
                            
                            
                            if(str_detect(data[i,3], pattern = convDH30.1)){
                              data[i,24] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern=convDH30.1, data[i,3])
                              data[i,18] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}}}} }}}}} }}}#} }}}
    
    #(046358/912)/PHP02.DH-031
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                if(str_detect(data[i,3], pattern ="\\.")){
                  if(!str_detect(data[i,3], pattern ="GEMS")){
                    
                    #if(!str_detect(data[i,3], pattern =convDH13)){
                    if(!str_detect(data[i,3], pattern =convDH14)){
                      if(str_detect(data[i,3], pattern =convDH12)){
                        if(!str_detect(data[i,3], pattern =convDH16)){
                          
                          
                          if(str_detect(data[i,3], pattern = convDH12)){
                            data[i,24] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH12, data[i,3])
                            data[i,14] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}} }}}}} }}
    
    
    
    #(046358/LH185)/(046358/I10516).DH-005
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 3){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!str_detect(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(str_detect(data[i,3], pattern ="\\.")){
                      if(str_detect(data[i,3], pattern =convDH13)){
                        if(!str_detect(data[i,3], pattern ="GEMS")){
                          
                          #if(!str_detect(data[i,3], pattern =convDH14)){
                          #if(!str_detect(data[i,3], pattern =convDH12)){
                          #if(!str_detect(data[i,3], pattern =convDH15)){
                          
                          
                          
                          if(str_detect(data[i,3], pattern = convDH13)){
                            data[i,24] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH13, data[i,3])
                            data[i,15] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}}} }}}}}}#}}
    
    
    #(054530/I10516)/I10517-B86.DH-023-B	
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!str_detect(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(grepl(data[i,3], pattern ="\\.")){
                      #if(!str_detect(data[i,3], pattern =convDH13)){
                      if(str_detect(data[i,3], pattern =convDH14)){
                        #if(!str_detect(data[i,3], pattern =convDH12)){
                        if(!str_detect(data[i,3], pattern =convDH17)){
                          if(!str_detect(data[i,3], pattern ="GEMS")){
                            
                            
                            
                            if(str_detect(data[i,3], pattern = convDH14)){
                              data[i,24] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern=convDH14, data[i,3])
                              data[i,16] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}}}}} }}}}}}}#}
    
    
    #(065125/LH119)/065125)-B-B.DH051-B	
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(grepl(data[i,3], pattern ="\\.")){
                      #if(!str_detect(data[i,3], pattern =convDH13)){
                      #if(!str_detect(data[i,3], pattern =convDH14)){
                      #if(!str_detect(data[i,3], pattern =convDH12)){
                      if(str_detect(data[i,3], pattern =convDH15)){
                        if(!grepl(data[i,3], pattern ="GEMS")){
                          
                          
                          if(str_detect(data[i,3], pattern = convDH15)){
                            data[i,24] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH15, data[i,3])
                            data[i,17] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}}} }}}}}}#}}
    
    
    data$Matched = ifelse(data$PEDIGREE != data$match, T, F)
    # # # 
    dataF = data %>% filter(Matched== F) %>% filter(Proccessed != "Proccessed") %>% 
      filter(!grepl(".Male",PEDIGREE)) %>% filter(!grepl("\\:",PEDIGREE)) %>% filter(!grepl(".Female",PEDIGREE))
    
    
    # # # 
    dataPros = data %>% filter(Proccessed == "Proccessed")
    nrow(dataPros)
    print(nrow(dataPros) / nrow(data))
    
    return(data.frame(data))
    
  }
  else{
    
    data = data
    data$match = gsub(data$match, pattern = "HX1", replacement = "Hx1")
    
    female.inbred = "([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})"
    male.inbred = "([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})"
    
    generation = "(?:-[[:alnum:]]+){3}"
    generation1 = "(?:-[[:alnum:]]+){1}"
    generation2 = "(?:-[[:alnum:]]+){2}"
    generation3 = "(?:-[[:alnum:]]+){0,6}"
    generation4 = "(?:-[[:alnum:]]+){4}"
    
    addgen = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6}"
    addgen2 = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    staraddgen = "[\\(]{0,6}[?:\\*[[:alnum:]]+]{0,20}[\\)]{0,6}"
    decaddgen = "[\\(]{0,6}[?:\\.[[:alnum:]]+]{0,20}[\\)]{0,6}"
    decaddgen.1 = "[\\(]{0,6}[?:-[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    conv1 = paste0("^",female.inbred, "/", male.inbred, generation)
    
    conv1.1 = paste0("^",female.inbred, "/", male.inbred, generation2,"$")
    
    #BDA015/BDA032//BHH069)-B-093-1-1	
    conv3 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen, generation)
    
    conv3.1 = paste0("^",female.inbred, "/{1,2}", male.inbred, "[\\)]{0,6}",generation3,"[\\)]{0,6}/{1,2}", addgen,"/",addgen, generation2)
    
    
    conv2 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen)
    
    
    #I10516/PHJ33//PHN82/I10516)-B-040-3-1-1-02	
    conv4 = paste0("^",female.inbred, "/", male.inbred, "//", addgen, "/", addgen, generation)
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP021)-B-01	
    conv4.1 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation2)
    
    #LH195*2/DK2FACC)-B-B-B-0081-01-1-B	
    conv5 = paste0("^",staraddgen, "/", male.inbred, generation)
    conv6 = paste0("^",female.inbred, "/", male.inbred, "//", addgen, "/", addgen)
    conv7 = paste0("^",female.inbred, "x", male.inbred, generation)
    
    
    #PHAW6/PHN82//PHAW6/PHJ33)-007-03-2///P02/R03)-B-057-1-2)-B-002	
    conv8 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen, generation3,"[\\)]{0,6}",generation2 )
    #(BJH074/I11063)/(PHJ40/I7016)-10/I7016)-B-B-69-1-2-B))-B-B-001	
    conv9 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation3,"[\\)]{0,6}",generation2 )
    #((5020/MM501D)/I10516-39.4)/I10516-1-1-1	
    conv10 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen, generation3,"[\\)]{0,6}",generation2 )
    
    #(I9005/DJ7)-B-040-1-1)/(I9005/ICI740)-B-084-3-3)//(I9005/ICI740)-B-084-3-1-1)	
    conv3.2 = paste0("^",female.inbred, "/{0,3}", male.inbred,generation3, "[\\)]{0,6}/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen, generation2 )
    #(I9005/DJ7)-B-040-1-1)/(I9005/ICI740)-B-084-3-3))-B	
    conv3.3 = paste0("^",female.inbred, "/{0,3}", male.inbred,generation3, "[\\)]{0,6}/{0,3}", addgen, "/{0,3}", addgen, generation3, "[\\)]{0,6}/{0,3}",generation2 )
    
    #((065125/054245)/RS710-39.09)/065125-1-4-4	
    conv11 = paste0("^",female.inbred, "/{0,3}", male.inbred, "/{0,3}", addgen, decaddgen,"/{0,3}", decaddgen.1,generation )
    #((I5009/I7016.076-2-1-1-2)/R03)/(I5009/I7016.088-2-1)	
    conv11.1 = paste0("^",female.inbred, "/{0,3}", decaddgen, generation3, "[\\)]{0,6}/{0,3}", addgen,"/{0,3}",addgen,"/{0,3}", decaddgen,"/{0,3}", decaddgen.1,generation2,"\\)" )
    
    
    #(046358/CI6621)/054530-B-45-1-1		
    conv19 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation2)
    
    #(PHP02/054530)/046358-41	
    conv19.1 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation2)
    
    
    conv19.2 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen2, generation)
    
    conv19.3 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", addgen, generation2)
    
    #(046358/MM501D)/(046358/I10516)-01-4	
    conv20 = paste0("^",female.inbred, "/{1,2}", male.inbred, "/{1,2}", female.inbred, "/{1,2}", male.inbred, generation3)
    
  
    # str_detect(p, pattern = conv1)
    # str_match(pattern = conv1, p)
    # str_detect(data[i,2], pattern = conv19.3)
    # str_match(pattern = conv19.3, data[i,2])
    # 
    
    
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        #if(!str_detect(data[i,3], pattern = conv1.1)){
          if(!str_detect(data[i,3], pattern = conv3)){
            if(!str_detect(data[i,3], pattern = conv4)){
              if(!grepl(data[i,3], pattern ="\\*")){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            
                            
                            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
                              
                              if(str_detect(data[i,3], pattern = conv1)){
                                data[i,23] = "Proccessed" # add a period for using wildcards
                                
                                replacement = str_match(pattern = conv1, data[i,3])
                                data[i,7] = replacement[1] # add a period for using wildcards
                                data[i,3] = replacement[1] # add a period for using wildcards
                                
                              }}} }}}}}}}}}}
    
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv3)){
          if(!str_detect(data[i,3], pattern = conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(!grepl(data[i,3], pattern ="\\:")){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                          if(!grepl(data[i,3], pattern ="\\:")){
                            if(!str_detect(data[i,3], pattern ="\\.")){
                              
                              
                              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==1){
                                
                                replacement = str_match(pattern = conv1, data[i,3])
                                data[i,23] = "Proccessed" # add a period for using wildcards
                                
                                if(!is.na(replacement)){
                                  
                                  if(str_detect(data[i,3], pattern = fixed(replacement[1]))){
                                    data[i,23] = "Proccessed" # add a period for using wildcards
                                    
                                    data[i,8] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }}}} }}}}}}}}}}}#}
  
    
    
    #BDA015/BDA032//BHH069)-B-093-1-1	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          #if(str_detect(data[i,3], pattern =conv2)){
          if(!str_detect(data[i,3], pattern =conv3.1)){
            if(str_detect(data[i,3], pattern =conv3)){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          #if(!str_detect(data[i,3], pattern = conv3.2)){
                            #if(!str_detect(data[i,3], pattern = conv3.3)){
                              if(!grepl(data[i,3], pattern = "\\*")){
                                
                                
                                if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==2 ){
                                  
                                  if(str_detect(data[i,3], pattern = conv3)){
                                    data[i,23] = "Proccessed" # add a period for using wildcards
                                    
                                    replacement = str_match(pattern = conv3, data[i,3])
                                    data[i,9] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }} }}}}}}}}}}}}
    
    #(I11063/FF6224)-B-B)/(I11063/FF6788)-14-03	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          #if(str_detect(data[i,3], pattern =conv2)){
          if(str_detect(data[i,3], pattern =conv3.1)){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!str_detect(data[i,3], pattern ="\\(GA21")){
                    #if(str_detect(data[i,3], pattern = conv1.1)){
                    if(!grepl(data[i,3], pattern ="\\:")){
                      if(!str_detect(data[i,3], pattern ="\\.")){
                        if(!str_detect(data[i,3], pattern = conv3.2)){
                          if(!str_detect(data[i,3], pattern = conv3.3)){
                            if(!grepl(data[i,3], pattern = "\\*")){
                              
                              
                              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3 ){
                                
                                if(str_detect(data[i,3], pattern = conv3.1)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern = conv3.1, data[i,3])
                                  data[i,9] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }} }}}}}}}}}}}}}
    
    
    #I10516/PHJ33//PHN82/I10516)-B-040-3-1-1-02	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(str_detect(data[i,3], pattern = conv4)){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!str_detect(data[i,3], pattern =conv4.1)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!str_detect(data[i,3], pattern = conv8)){
                          if(!grepl(data[i,3], pattern ="\\:")){
                            if(!str_detect(data[i,3], pattern = conv9)){
                              if(!str_detect(data[i,3], pattern ="\\.")){
                                
                                
                                #if(!str_detect(data[i,3], pattern =conv3)){
                                
                                if(str_detect(data[i,3], pattern = conv4)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern = conv4, data[i,3])
                                  data[i,11] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                } } }  }}}}}}}}}}}}
    
    # str_match(pattern = conv4.1, data.changed[i,2])
    # str_detect(data.changed[i,2], pattern =conv4.1)
    # 
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP043)-B-18	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        #if(str_detect(data[i,3], pattern = conv4)){
        if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
          if(!str_detect(data[i,3], pattern =conv7)){
            if(str_detect(data[i,3], pattern =conv4.1)){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!str_detect(data[i,3], pattern ="\\(GA21")){
                    #if(!str_detect(data[i,3], pattern =conv3)){
                    #if(!str_detect(data[i,3], pattern = conv1.1)){
                    if(!grepl(data[i,3], pattern ="\\:")){
                      if(!str_detect(data[i,3], pattern = conv9)){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          
                          
                          
                          if(str_detect(data[i,3], pattern = conv4.1)){
                            data[i,23] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern = conv4.1, data[i,3])
                            data[i,11] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          } } }  }}}}}}}}}
    
    #(PHAW6/PHN82//PHAW6/PHJ33)-B-039-1)/BRP043)-B-18	
    for(i in 1:nrow(data)) {
      if(!str_detect(data[i,3], pattern = "\\.DH")){
        if(!str_detect(data[i,3], pattern = conv4)){
          if(!lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >=3){
            if(!str_detect(data[i,3], pattern =conv7)){
              if(!str_detect(data[i,3], pattern =conv4.1)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern =conv3)){
                      if(str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern = conv9)){
                            if(!str_detect(data[i,3], pattern ="\\.")){
                              
                              
                              if(str_detect(data[i,3], pattern = conv1.1)){
                                data[i,23] = "Proccessed" # add a period for using wildcards
                                
                                replacement = str_match(pattern = conv1.1, data[i,3])
                                data[i,10] = replacement[1] # add a period for using wildcards
                                data[i,3] = replacement[1] # add a period for using wildcards
                                
                              } } }  }}}}}}}}}}}
    
    #LH195*2/DK2FACC)-B-B-B-0081-01-1-B	
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(str_detect(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==1){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            
                            
                            if(str_detect(data[i,3], pattern = conv5)){
                              data[i,23] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern=conv5, data[i,3])
                              data[i,12] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}}  }}}}}}}}}}
    
    
    #((065125/054245)/RS710-39.09)/065125-1-4-4	
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 3){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(str_detect(data[i,3], pattern ="\\.")){
                            if(str_detect(data[i,3], pattern = conv11)){
                              if(!str_detect(data[i,3], pattern = conv11.1)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv11)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv11, data[i,3])
                                  data[i,13] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}
    #I9011-B/LH200//FX8091)-B-B-048	
    #(046358/CI6621)/054530-B-45
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            if(str_detect(data[i,3], pattern = conv19)){
                              if(!str_detect(data[i,3], pattern = conv11.1)){
                                if(!str_detect(data[i,3], pattern = conv6)){

                                
                                if(str_detect(data[i,3], pattern = conv19)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv19, data[i,3])
                                  data[i,19] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}#}
    
    #(046358/CI6621)/054530-B
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19.1)){
                            if(!str_detect(data[i,3], pattern = conv19.2)){
                              
                            if(!str_detect(data[i,3], pattern = conv11.1)){
                              if(!str_detect(data[i,3], pattern = conv6)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv19.1)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv19.1, data[i,3])
                                  data[i,19] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}}#}
    
    #I9011-B/LH200//FX8091)-B-B-048
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv19.2)){
                            if(!str_detect(data[i,3], pattern = conv19.1)){
                              
                            if(!str_detect(data[i,3], pattern = conv11.1)){
                              if(!str_detect(data[i,3], pattern = conv6)){
                                
                                
                                if(str_detect(data[i,3], pattern = conv19.2)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv19.2, data[i,3])
                                  data[i,19] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}}
      
      #I9011-B/LH200//FX8091)-B-B-048
      for(i in 1:nrow(data)) {
        if(!grepl(data[i,3], pattern ="\\.DH")){
          #if(!str_detect(data[i,3], pattern =conv2)){
          #if(!str_detect(data[i,3], pattern =conv3)){
          if(!str_detect(data[i,3], pattern =conv4)){
            if(!grepl(data[i,3], pattern ="\\*")){
              if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 2){
                if(!str_detect(data[i,3], pattern =conv7)){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!grepl(data[i,3], pattern ="\\(wx")){
                      if(!str_detect(data[i,3], pattern ="\\(GA21")){
                        #if(!str_detect(data[i,3], pattern = conv1.1)){
                        if(!grepl(data[i,3], pattern ="\\:")){
                          if(!str_detect(data[i,3], pattern ="\\.")){
                            if(str_detect(data[i,3], pattern = conv19.3)){
                              if(!str_detect(data[i,3], pattern = conv19.1)){
                                if(!str_detect(data[i,3], pattern = conv19.2)){
                                  if(!str_detect(data[i,3], pattern = conv19)){
                                    
                                if(!str_detect(data[i,3], pattern = conv11.1)){
                                  if(!str_detect(data[i,3], pattern = conv6)){
                                    
                                    
                                    if(str_detect(data[i,3], pattern = conv19.3)){
                                      data[i,23] = "Proccessed" # add a period for using wildcards
                                      
                                      replacement = str_match(pattern=conv19.3, data[i,3])
                                      data[i,19] = replacement[1] # add a period for using wildcards
                                      data[i,3] = replacement[1] # add a period for using wildcards
                                      
                                    }}}  }}}}}}}}}}}}}}}
    
    #(056460/CI6621)/(I10516/CI6621)-B-10
    for(i in 1:nrow(data)) {
      if(!grepl(data[i,3], pattern ="\\.DH")){
        #if(!str_detect(data[i,3], pattern =conv2)){
        #if(!str_detect(data[i,3], pattern =conv3)){
        if(!str_detect(data[i,3], pattern =conv4)){
          if(!grepl(data[i,3], pattern ="\\*")){
            if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) >= 3){
              if(!str_detect(data[i,3], pattern =conv7)){
                if(!grepl(data[i,3], pattern ="\\(wx")){
                  if(!grepl(data[i,3], pattern ="\\(wx")){
                    if(!str_detect(data[i,3], pattern ="\\(GA21")){
                      #if(!str_detect(data[i,3], pattern = conv1.1)){
                      if(!grepl(data[i,3], pattern ="\\:")){
                        if(!str_detect(data[i,3], pattern ="\\.")){
                          if(str_detect(data[i,3], pattern = conv20)){
                            if(!str_detect(data[i,3], pattern = conv11.1)){
                              if(!str_detect(data[i,3], pattern = conv6)){
                                #if(!str_detect(data[i,3], pattern = conv5)){
                                  #if(!str_detect(data[i,3], pattern = conv3)){

                                
                                if(str_detect(data[i,3], pattern = conv20)){
                                  data[i,23] = "Proccessed" # add a period for using wildcards
                                  
                                  replacement = str_match(pattern=conv20, data[i,3])
                                  data[i,20] = replacement[1] # add a period for using wildcards
                                  data[i,3] = replacement[1] # add a period for using wildcards
                                  
                                }}}  }}}}}}}}}}}}#}
    
    
    
    data$Matched = ifelse(data$pedigree != data$match, T, F)
    # # # 
    dataF = data %>% filter(Matched== F) %>% filter(proccessed != "Proccessed") %>% 
      filter(!grepl(".Male",pedigree)) %>% filter(!grepl("\\:",pedigree)) %>% filter(!grepl(".Female",pedigree))
    
    
    # # # 
    dataPros = data %>% filter(proccessed == "Proccessed")
    nrow(dataPros)
    nrow(dataPros) / nrow(data)
    
    
    
    dataSave = data
    
    
    
    
    data = dataSave
    ##################################
    #DH 
    
  #  data = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/data.txt" )
   # dataF = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataF.txt" )
  #  dataPros = read.table("C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataPros.txt" )
    
    generation = "(?:-[[:alnum:]]+){3}"
    generation2 = "(?:-[[:alnum:]]+){2}"
    generation3 = "(?:\\.[[:alnum:]]+){0,6}"
    generation4 = "(?:-[[:alnum:]]+){4}"
    generation1 = "(?:-[[:alnum:]]+){1}"
    
    
    nondigitDH = "([?-][?B][\\.?]DHB[?-][[:alnum:]]{0,4}|[?-][?B][\\.?]DH[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DH-B[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DHB[?-][[:alnum:]]{0,4}|[?-]?[\\.?]DH[?-][[:alnum:]]{0,4})"
    digitDH = "([?-]B[\\.?]DHB[[:digit:]]{0,4}|[?-]?B[[:digit:]]{0,4}[\\.?]DH[[:digit:]]{0,4}|[?-]?[\\.?]DH[[:digit:]]{0,4}-B|[?-]?[\\.?]DH[[:digit:]]{0,4})"
    nondigitDH1 = "([?-][?B]DHB[?-][[:alnum:]]{0,4}|[?-][?B]DH[?-][[:alnum:]]{0,4}|[?-]?DH-B[?-][[:alnum:]]{0,4}|[?-]?DHB[?-][[:alnum:]]{0,4}|[?-]?DH[?-][[:alnum:]]{0,4})"
    digitDH1 = "([?-]BDHB[[:digit:]]{0,4}|[?-]?B[[:digit:]]{0,4}DH[[:digit:]]{0,4}|[?-]?DH[[:digit:]]{0,4}-B|[?-]?DH[[:digit:]]{0,4})"
    
    DH1 = paste0("(", nondigitDH1, "|", digitDH1, ")")
    
    DH = paste0("(", nondigitDH, "|", digitDH, ")")
    
    decaddgen = "[\\(]{0,6}[?:\\.[[:alnum:]]+]{0,6}[\\)]{0,6}"
    
    dhString = paste0("[\\(]{0,6}[[:alnum:]]?(?:-[[:alnum:]]+){0,20}", DH,"[\\)]{0,6}")
    
    male.inbred = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    female.inbred = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    addgen = paste0("(([\\(]{0,6}[?:-[[:alnum:]]+]{0,20}[\\)]{0,6})|", dhString,")")
    
    #(046358/912)/PHP02.DH-031-1	
    convDH12 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, DH)
    
    #(046358/LH185)/(046358/I10516).DH-005-1	
    convDH13 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, "/{0,3}", addgen, DH)
    
    ##((I5009/I7016.DH078-2-1)/R03)/(I5009/I7016.088-2-1)	
    ##convDH14 = paste0("^",male.inbred,"/{0,3}", female.inbred, DH,generation3,"\\){0,3}/{0,3}",generation3,"/{0,3}",generation3,"/{0,3}" ,generation3)# , "\\.",generation2  )#, generation3, "[\\)]{0,6}") #, generation2)
    
    #(054530/I10516)/I10517-B86.DH-023-B	
    convDH14 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}",addgen, DH,generation1)
    
    #(065125/LH119)/065125)-B-B.DH051-B	
    convDH15 = paste0("^",male.inbred,"/{0,3}", female.inbred,"/{0,3}", addgen, generation1, DH)
    
    #(AW6/R03)/(PHN82 x I8515.DH042))-B-001
    convDH16 = paste0("^",male.inbred, "/{0,3}", female.inbred, "/{0,3}", addgen,DH, "\\){0,3}", generation2)
    
    #(I10540/I9570.DH103-1-B)/N46)-B.DHB025	
    convDH17 = paste0("^",male.inbred, "/{0,3}", female.inbred,DH,generation2,"\\){0,3}/{0,3}", addgen, digitDH)
    
    #(GEMS-0227/I9005)-B.DHB-007	
    convDH18 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH)
    convDH18.1 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH1)
    
    
    convDH30 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH, generation)
    convDH30.1 = paste0("^",male.inbred, "/{0,3}", female.inbred,  DH1, generation)
    #  str_detect(p, pattern = convDH30)
    #  str_match(pattern = convDH30, p)
    # # 
    # #(I5009/I7016.088-2-1)/(I8044/I9070.DH54-2-2)	  15
    
    #(I5009/I7016.DH088-2-1-1-1)/(I5009/I7016.088-2-1)	15
    
    #(I10540/I9570.DH103-1-B)/N46)-B.DHB025	17
    
    #(I5009/I7016.DH104)/I11063.DH017-1	16
    
    #(I8025/I5009.14-1)/I11054)-B.DHB-02	16
    
    #(I8025/I5009.14-B)/(I8025/I5009.DH008-B))-B-011	15 16
    
    #(I8025/I5009.14-B)/(I8025/I5009.DH008-B))-B.DHB-26	15 16
    
    #(I8025/I5009.DH008-1)/I11063)-B-023	16
    
    #(I9005/DJ7)-B-040-1-1)/(LFX6244/I9005.DH032-1-01-2))-B	15
    
    #(LFX6244/I9005.DH022-1-01-1)/(LFX6244/I9005.DH032-1-01-2))-B	15
    
    #(LH212/I9570A.DH020)-1)/LH185)-B.DHB-020	16
    
    #(PHK35 x PHG86.DH127-1)/BAJ808//P38.DHB44	
    
    #(PHV78/LH212)-B-B-049-1)/I12510.DHB20	
    
    #2369/I11012//I11063)-B.DHB46	15
    
    #BAJ808/HB9//P38.DHB031	15
    
    #BHF027/(BHF041/(PHK35/I7016.DH027-1-1-3)))-B-B-028	15
    
    #BJH074/(PHK29/I7016.DH039-1-1))-B	16
    
    #DSR046358/I10516//CI6621.DH-173)-01	15
    
    #I10001/BJH031)/DSR065125)-B.DH-B-060	16 17
    
    #I10509//I9543/I9560.DH031)-1-1-1	15
    
    #I10516/(DSR046358/3IIH6)-B.DHB-057-2))-B-B-004	 17 16
    
    #I10516-B/LH82//CI6621.DH-9)-01	15
    
    #I10516/LH212.DHB-002//BUR024)-B-023	15
    
    #I11503/I10516)/JC6794)-B.DH-B-21	17 16
    
    #I12003/(BHF041/(PHK35/I7016.DH027-1-1-3)))-B.DH063-1	15
    
    #I12028//I8025/I5009.DH008-2-2.DHB-60	15
    
    #I8025/I5009.DH008-1-1)/I11041-2-6)-B-B-009	16
    
    #I8514/I9524.06-2)/I10516.DH-6-01	16
    
    #I9542/I8510)-B.DH02-2)/LH185)-B-B-015	16
    
    #ID5754/I10516-4-3-B)/BQS042)-B.DH-B-019	17 16
    
    #LH198/I11063.DH077-1)-2-2)/BHF027)-B.DH-063-1
    
    #PHN82/I9505.DH072-1)/I10516.DH-19-01	16
    
    #PHN82/I9543.DH140-1-1)/I10516)-B-B-066	 16
    
    #PHR03/LH185//BUR070)-B.DHB075	15
    
    #(I5009/I7016.DH104)/I11063.DH002-2-B	16
    
    #PHN82/I9543.DH140-1-1)/I10516)-B-B-090 16
    
    
    # str_detect(dataF[i,2], pattern = convDH18.1)
    # str_match(pattern = convDH18.1, dataF[i,2])
    # BAA046/BHH069)-B.DHB-087-1

    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) >= 1){
              #if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) > 1){
                
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                #if(str_detect(data[i,3], pattern ="\\.")){
                  #if(!str_detect(data[i,3], pattern =convDH13)){
                    #if(!str_detect(data[i,3], pattern =convDH14)){
                      #if(!str_detect(data[i,3], pattern =convDH12)){
                        #if(!str_detect(data[i,3], pattern =convDH15)){
                          #if(!str_detect(data[i,3], pattern =convDH16)){
                            if(!str_detect(data[i,3], pattern =convDH17)){
                              if(str_detect(data[i,3], pattern =convDH18)){

                                  
                                  if(str_detect(data[i,3], pattern = convDH18)){
                                    data[i,23] = "Proccessed" # add a period for using wildcards
                                    
                                    replacement = str_match(pattern=convDH18, data[i,3])
                                    data[i,18] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }}}}} }}}}}#} #} }}
    
    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(str_detect(data[i,3], pattern ="\\DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) >= 1){
              #if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) > 1){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                #if(str_detect(data[i,3], pattern ="\\.")){
                if(!str_detect(data[i,3], pattern ="GEMS")){
                  #if(!str_detect(data[i,3], pattern =convDH13)){
                   # if(!str_detect(data[i,3], pattern =convDH14)){
                      #if(!str_detect(data[i,3], pattern =convDH12)){
                       # if(!str_detect(data[i,3], pattern =convDH15)){
                          if(!str_detect(data[i,3], pattern =convDH16)){
                            if(!str_detect(data[i,3], pattern =convDH17)){
                                if(str_detect(data[i,3], pattern =convDH18.1)){
                                  
                                  
                                  if(str_detect(data[i,3], pattern = convDH18.1)){
                                    data[i,23] = "Proccessed" # add a period for using wildcards
                                    
                                    replacement = str_match(pattern=convDH18.1, data[i,3])
                                    data[i,18] = replacement[1] # add a period for using wildcards
                                    data[i,3] = replacement[1] # add a period for using wildcards
                                    
                                  }}}}} }}}}} }}#} }}}
    
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) == 0){
            if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) == 0){
              
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                #if(str_detect(data[i,3], pattern ="\\.")){
                #if(!str_detect(data[i,3], pattern =convDH13)){
                #if(!str_detect(data[i,3], pattern =convDH14)){
                #if(!str_detect(data[i,3], pattern =convDH12)){
                #if(!str_detect(data[i,3], pattern =convDH15)){
                #if(!str_detect(data[i,3], pattern =convDH16)){
                if(!str_detect(data[i,3], pattern =convDH17)){
                  if(str_detect(data[i,3], pattern =convDH30)){
                    
                    
                    if(str_detect(data[i,3], pattern = convDH30)){
                      data[i,23] = "Proccessed" # add a period for using wildcards
                      
                      replacement = str_match(pattern=convDH30, data[i,3])
                      data[i,18] = replacement[1] # add a period for using wildcards
                      data[i,3] = replacement[1] # add a period for using wildcards
                      
                    }}}}} }}}}}  #}}
    
    #(GEMS-0227/I9005)-B.DHB-063
    for(i in 1:nrow(data)) {
      if(str_detect(data[i,3], pattern ="\\DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 1){
            if(lengths(regmatches(data[i,3], gregexpr("\\)", data[i,3]))) == 0){
              if(lengths(regmatches(data[i,3], gregexpr("\\(", data[i,3]))) == 0){
                
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                #if(str_detect(data[i,3], pattern ="\\.")){
                if(!str_detect(data[i,3], pattern ="GEMS")){
                  #if(!str_detect(data[i,3], pattern =convDH13)){
                  # if(!str_detect(data[i,3], pattern =convDH14)){
                  #if(!str_detect(data[i,3], pattern =convDH12)){
                  # if(!str_detect(data[i,3], pattern =convDH15)){
                  if(!str_detect(data[i,3], pattern =convDH16)){
                    if(!str_detect(data[i,3], pattern =convDH17)){
                      if(str_detect(data[i,3], pattern =convDH30.1)){
                        
                        
                        if(str_detect(data[i,3], pattern = convDH30.1)){
                          data[i,23] = "Proccessed" # add a period for using wildcards
                          
                          replacement = str_match(pattern=convDH30.1, data[i,3])
                          data[i,18] = replacement[1] # add a period for using wildcards
                          data[i,3] = replacement[1] # add a period for using wildcards
                          
                        }}}}} }}}}} }}}#} }}}
    
    #(046358/912)/PHP02.DH-031
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern = "\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\:")){
                if(str_detect(data[i,3], pattern ="\\.")){
                  if(!str_detect(data[i,3], pattern ="GEMS")){
                    
                    #if(!str_detect(data[i,3], pattern =convDH13)){
                    if(!str_detect(data[i,3], pattern =convDH14)){
                      if(str_detect(data[i,3], pattern =convDH12)){
                        if(!str_detect(data[i,3], pattern =convDH16)){
                          
                          
                          if(str_detect(data[i,3], pattern = convDH12)){
                            data[i,23] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH12, data[i,3])
                            data[i,14] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}} }}}}} }}
    
    
    
    #(046358/LH185)/(046358/I10516).DH-005
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 3){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!str_detect(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(str_detect(data[i,3], pattern ="\\.")){
                      if(str_detect(data[i,3], pattern =convDH13)){
                        if(!str_detect(data[i,3], pattern ="GEMS")){
                          
                          #if(!str_detect(data[i,3], pattern =convDH14)){
                          #if(!str_detect(data[i,3], pattern =convDH12)){
                          #if(!str_detect(data[i,3], pattern =convDH15)){
                          
                          
                          
                          if(str_detect(data[i,3], pattern = convDH13)){
                            data[i,23] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH13, data[i,3])
                            data[i,15] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}}} }}}}}}#}}
    
    
    #(054530/I10516)/I10517-B86.DH-023-B	
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!str_detect(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(grepl(data[i,3], pattern ="\\.")){
                      #if(!str_detect(data[i,3], pattern =convDH13)){
                      if(str_detect(data[i,3], pattern =convDH14)){
                        #if(!str_detect(data[i,3], pattern =convDH12)){
                        if(!str_detect(data[i,3], pattern =convDH17)){
                          if(!str_detect(data[i,3], pattern ="GEMS")){
                            
                            
                            
                            if(str_detect(data[i,3], pattern = convDH14)){
                              data[i,23] = "Proccessed" # add a period for using wildcards
                              
                              replacement = str_match(pattern=convDH14, data[i,3])
                              data[i,16] = replacement[1] # add a period for using wildcards
                              data[i,3] = replacement[1] # add a period for using wildcards
                              
                            }}}}}} }}}}}}}#}
    
    
    #(065125/LH119)/065125)-B-B.DH051-B	
    for(i in 1:nrow(data)) {
      if(grepl(data[i,3], pattern ="\\.DH")){
        if(!grepl(data[i,3], pattern ="\\*")){
          if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) == 2){
            if(!grepl(data[i,3], pattern ="\\(wx")){
              if(!grepl(data[i,3], pattern ="\\(wx")){
                if(!grepl(data[i,3], pattern ="\\(GA21")){
                  if(!grepl(data[i,3], pattern ="\\:")){
                    if(grepl(data[i,3], pattern ="\\.")){
                      #if(!str_detect(data[i,3], pattern =convDH13)){
                      #if(!str_detect(data[i,3], pattern =convDH14)){
                      #if(!str_detect(data[i,3], pattern =convDH12)){
                      if(str_detect(data[i,3], pattern =convDH15)){
                        if(!grepl(data[i,3], pattern ="GEMS")){
                          
                          
                          if(str_detect(data[i,3], pattern = convDH15)){
                            data[i,23] = "Proccessed" # add a period for using wildcards
                            
                            replacement = str_match(pattern=convDH15, data[i,3])
                            data[i,17] = replacement[1] # add a period for using wildcards
                            data[i,3] = replacement[1] # add a period for using wildcards
                            
                          }}}}}} }}}}}}#}}
    
    
    data$Matched = ifelse(data$pedigree != data$match, T, F)
    # # # 
    dataF = data %>% filter(Matched== F) %>% filter(proccessed != "Proccessed") %>% 
      filter(!grepl(".Male",pedigree)) %>% filter(!grepl("\\:",pedigree)) %>% filter(!grepl(".Female",pedigree))
    
    
    # # # 
    dataPros = data %>% filter(proccessed == "Proccessed")
    nrow(dataPros)
    print(nrow(dataPros) / nrow(data))
    
    #write.table(dataPros, "C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataPros.txt")
    #write.table(dataF, "C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataF.txt")
    #write.table(data, "C:/Users/jake.lamkey/Desktop/5-Year GCA Results/data.txt")
    
    #write.csv(dataF, "C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataF.csv")
    #write.csv(dataPros, "C:/Users/jake.lamkey/Desktop/5-Year GCA Results/dataPros.csv")
    
    # dhStringped.07 = paste0(digitDH,"?", nondigitDH,"?","[[:alnum:]]*(?:-[[:alnum:]]+){0}")
    # 
    # dhStringperiod = paste0("[[:alnum:]]*(?:-([[:alnum:]]+(\\.)?[[:alnum:]])+){0,5}(-)?",digitDH,"?(-)?", nondigitDH,"?","(?:-[[:alnum:]]+){0,5}")
    # 
    # patterns.04 = paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}",digitDH)
    # patterns.05 = paste0("[\\(]{0,4}",dhStringped,"/",dhString)
    # #((MM501D/CI6621)/I10516-52.2)/I10516-3-1-1-2-B
    # patterns.06 = paste0("[\\(]{0,4}[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringped,"[\\)]{0,4}/",dhStringped)
    # #(046358/LH185)/(046358/I10516).DH-007
    # patterns.07 = paste0("[\\(]{0,4}",dhStringped,"/", dhStringped,"[\\)]{0,4}/[\\(]{0,4}", dhStringped,"/", dhStringped,"[\\)]{0,4}", dhStringped.07)
    # 
    # bcString = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,4}(-)?",digitDH,"?(-)?", nondigitDH,"?")
    # patterns30=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringperiod,"[\\)]{0,4}/",bcString,"(?:-[[:alnum:]]+){2}")
    # patterns.27=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}",dhStringped,"([[:alnum:]]+){1}")
    # patterns.26=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}","(?:-[[:alnum:]]+){3}")
    # patterns.25=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringperiod,"[\\)]{0,4}/",bcString,"(?:-[[:alnum:]]+){2}")
    # # #patterns.30=paste0("^[\\(]{0,1}[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,1}/",dhStringperiod,"\\)/",bcString,"(?:-[[:alnum:]]+){2}")
    # patterns.08 = paste0("\\(",dhStringped,"/", dhStringped,"/", dhStringped,"\\)","[[:alnum:]]*(?:-[[:alnum:]]+){2}")
    # dhStringped3 = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,2}",digitDH,"?", nondigitDH,"?","(?:-[[:alnum:]]+){0,1}")
    # patterns.20=paste0("^\\(",dhStringped,":",dhStringped,"\\)", dhStringped3)
    # dhStringped = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,20}",digitDH,"?", nondigitDH,"?","(?:-[[:alnum:]]+){0,3}")
    # 
    # dhString = paste0("[[:alnum:]]?(?:-[[:alnum:]]+){0,20}", digitDH, "?", nondigitDH, "?")
    # 
    # patterns.4way =paste0("^[\\(]{0,6}",dhStringped,"/",dhStringped,"[\\)]{0,6}/[\\(]{0,6}",dhStringped,"/",dhStringped,"[\\)]{0,6}","(?:-[[:alnum:]]+){4}")
    # 
    #nondigitDH ="((?-)?B\\.DHB|(?-)?B\\.DH|(?-)?\\.DH-B|(?-)?\\.DHB|(?-)?\\.DH)" 
    
    #BDA015/BDA032//BHH069)-B-093-1-1	?error
    # for(i in 1:nrow(data)) {
    #   if(!str_detect(data[i,3], pattern = "\\.DH")){
    #     if(!str_detect(data[i,3], pattern = conv4)){
    #       #if(str_detect(data[i,3], pattern =conv3)){
    #       if(lengths(regmatches(data[i,3], gregexpr("/", data[i,3]))) ==2 ){
    #         
    #         replacement = str_match(pattern = conv3, data[i,3])
    #         
    #         if(!is.na(replacement)){
    #           if(str_detect(data[i,3], pattern = fixed(replacement[1]))){
    #             data[i,10] = replacement[1] # add a period for using wildcards
    #             data[i,3] = replacement[1] # add a period for using wildcards
    #             
    #           }} }}}}
    
    
    return(data.frame(data))
  }
}
# 
# data$Matched = ifelse(data$pedigree != data$match, T, F)
# # # 
# dataF = data %>% filter(Matched== T)
# 

# #DH material######################################################
######B.DH min of either F4 or NewDH 

##################################
#DH
# 
# #I9005/LH198)-B.DH045-1-2	#F4
# #digitDH = "(((-)?B\\.DHB[0-9]+)|((-)?B\\.DH[0-9]+)|((-)?\\.DH-B[0-9]+)|((-)?\\.DHB[0-9]+)|((-)?\\.DH[0-9]+))"
# #nondigitDH ="((-)?B\\.DHB[^[0-9]]+|(-)?B\\.DH[^[0-9]]+|(-)?\\.DH-B[^[0-9]]+|(-)?\\.DHB[^[0-9]]+|(-)?\\.DH[^[0-9]]+)" 
# dhString = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,5}",digitDH,"?", nondigitDH,"?")
# dhStringped = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,5}",digitDH,"?", nondigitDH,"?","(?:-[[:alnum:]]+){0,5}")
# dhStringped.07 = paste0(digitDH,"?", nondigitDH,"?","[[:alnum:]]*(?:-[[:alnum:]]+){0}")
# 
# dhStringperiod = paste0("[[:alnum:]]*(?:-([[:alnum:]]+(\\.)?[[:alnum:]])+){0,5}(-)?",digitDH,"?(-)?", nondigitDH,"?","(?:-[[:alnum:]]+){0,5}")
# 
# patterns.04 = paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}",digitDH)
# patterns.05 = paste0("[\\(]{0,4}",dhStringped,"/",dhString)
# #((MM501D/CI6621)/I10516-52.2)/I10516-3-1-1-2-B
# patterns.06 = paste0("[\\(]{0,4}[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringped,"[\\)]{0,4}/",dhStringped)
# #(046358/LH185)/(046358/I10516).DH-007
# patterns.07 = paste0("[\\(]{0,4}",dhStringped,"/", dhStringped,"[\\)]{0,4}/[\\(]{0,4}", dhStringped,"/", dhStringped,"[\\)]{0,4}", dhStringped.07)
# 
# bcString = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,4}(-)?",digitDH,"?(-)?", nondigitDH,"?")
# patterns30=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringperiod,"[\\)]{0,4}/",bcString,"(?:-[[:alnum:]]+){2}")
# patterns.27=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}",dhStringped,"([[:alnum:]]+){1}")
# patterns.26=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}","(?:-[[:alnum:]]+){3}")
# patterns.25=paste0("[\\(]{0,4}",dhStringped,"/",dhStringped,"[\\)]{0,4}/",dhStringperiod,"[\\)]{0,4}/",bcString,"(?:-[[:alnum:]]+){2}")
# # #patterns.30=paste0("^[\\(]{0,1}[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,1}/",dhStringperiod,"\\)/",bcString,"(?:-[[:alnum:]]+){2}")
# patterns.08 = paste0("\\(",dhStringped,"/", dhStringped,"/", dhStringped,"\\)","[[:alnum:]]*(?:-[[:alnum:]]+){2}")
# dhStringped3 = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,2}",digitDH,"?", nondigitDH,"?","(?:-[[:alnum:]]+){0,1}")
# patterns.20=paste0("^\\(",dhStringped,":",dhStringped,"\\)", dhStringped3)


# #CI6621/054530)-B.DH089	
# #046358/054530)-B.DH-009	
# #(GEMS-0227/I9005)-B.DHB-007
# 
# for(i in 1:nrow(data)) {
#   if(grepl(data[i,3], pattern ="\\.DH")){
#     if(str_detect(data[i,3], pattern = patterns.04)){
#       if(!str_detect(data[i,3], pattern = patterns.06)){
#         if(!str_detect(data[i,3], pattern = patterns.07)){
#           
#           replacement = str_match(pattern = patterns.04, data[i,3])
#           data[i,3] = replacement[1] # add a period for using wildcards
#         }} } }}
# 
# #1.3432321e-01	
# #LH198/I10001.DH02-3
# #CI6621/054530)-B.DH089-1-B-B-B
# for(i in 1:nrow(data)) {
#   if(grepl(data[i,3], pattern ="\\.DH")){
#     if(str_detect(data[i,3], pattern = patterns.05)){
#       if(!str_detect(data[i,3], pattern = patterns.04)){
#         if(!str_detect(data[i,3], pattern = patterns.06)){
#           if(!str_detect(data[i,3], pattern = patterns.07)){
#             
#             replacement = str_match(pattern = patterns.05, data[i,3])
#             data[i,3] = replacement[1] # add a period for using wildcards
#           }}  }}}}
# 
# #(046358/LH185)/(046358/I10516).DH-029
# for(i in 1:nrow(data)) {
#   if(str_detect(data[i,3], pattern = "\\.DH")){
#     if(!str_detect(data[i,3], pattern = patterns.05)){
#       if(!str_detect(data[i,3], pattern = patterns.04)){
#         if(!str_detect(data[i,3], pattern = patterns.06)){
#           if(str_detect(data[i,3], pattern = patterns.07)){
#             
#             replacement = str_match(pattern = patterns.07, data[i,3])
#             data[i,3] = replacement[1] # add a period for using wildcards
#           }}  }}}}
# 
# #(GEMS-0227/I9005)-B.DHB-015-1	
# #(I10516/JC6794)/I10516.DH-093-1-B	
# #(ID5754/3IIH6)/DSR046358).DH-B-070-B	
# #(I11063/I12003)/I12003.DH132-1-B-B	
# #(LH185/CJ7008)/(KDL6289/LH185)).DHB-06	add in as pattern to filter
# #046358/054530)-B.DH-009-1-B	add in as pattern to filter
# #24AFD-B04/40AQA-B03.DH-013)-B	add in as pattern to filter
# #patterns.25=paste0("^[\\(]{0,1}[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,1}/",dhStringperiod,"\\)/",bcString,"(?:-[[:alnum:]]+){2}")
# 
# # for(i in 1:nrow(data)) {
# #   if(!str_detect(data[i,3], pattern = "\\.DH")){
# #     if(!str_detect(data[i,3], pattern =patterns30)){
# #       if(!str_detect(data[i,3], pattern = patterns.25)){
# #         if(!str_detect(data[i,3], pattern = patterns.26)){
# #           if(!str_detect(data[i,3], pattern = patterns.05)){
# #             if(!str_detect(data[i,3], pattern = patterns.04)){
# #               if(!str_detect(data[i,3], pattern = patterns.06)){
# #                 if(str_detect(data[i,3], pattern = patterns.26)){
# #                   
# #                 replacement = str_match(pattern = patterns.26, data[i,3])
# #                 data[i,3] = replacement[1] # add a period for using wildcards
# #               }}  }}}}}}}
# 
# 
# 
# 
# 
# #Back-Cross BC2S3###########################################################
# #((MM501D/CI6621)/I10516-57.1)/I10516-4-1-1 #BC2S3
# 
# for(i in 1:nrow(data)) {
#   if(!str_detect(data[i,3], pattern =patterns30)){
#     if(str_detect(data[i,3], pattern = patterns.25)){
#       if(!str_detect(data[i,3], pattern = patterns.05)){
#         if(!str_detect(data[i,3], pattern = patterns.04)){
#           if(!str_detect(data[i,3], pattern = patterns.06)){
#             if(!str_detect(data[i,3], pattern = patterns.07)){
#               
#               replacement = str_match(pattern = patterns.25, data[i,3])
#               data[i,3] = replacement[1] # add a period for using wildcards
#             }}  }}}}}
# 
# 
# #Four-way Cross#######################################################
# 
# 
# #(046358/LH185)/(046358/CI6621)-B-24-1 #F4
# 
# for(i in 1:nrow(data)) {
#   if(!str_detect(data[i,3], pattern = "\\.DH")){
#     if(!str_detect(data[i,3], pattern =patterns30)){
#       if(!str_detect(data[i,3], pattern = patterns.25)){
#         if(str_detect(data[i,3], pattern = patterns.26)){
#           if(!str_detect(data[i,3], pattern = patterns.05)){
#             if(!str_detect(data[i,3], pattern = patterns.04)){
#               if(!str_detect(data[i,3], pattern = patterns.06)){
#                 
#                 replacement = str_match(pattern = patterns.26, data[i,3])
#                 data[i,3] = replacement[1] # add a period for using wildcards
#               }}  }}}}}}
# 
# #(046358/LH185)/(046358/I10516).DH-014 #DH1
# 
# for(i in 1:nrow(data)) {
#   if(str_detect(data[i,3], pattern = "\\.DH")){
#     if(!str_detect(data[i,3], pattern =patterns30)){
#       if(!str_detect(data[i,3], pattern = patterns.25)){
#         if(str_detect(data[i,3], pattern = patterns.27)){
#           if(!str_detect(data[i,3], pattern = patterns.05)){
#             if(!str_detect(data[i,3], pattern = patterns.04)){
#               if(!str_detect(data[i,3], pattern = patterns.06)){
#                 
#                 replacement = str_match(pattern = patterns.27, data[i,3])
#                 data[i,3] = replacement[1] # add a period for using wildcards
#               }}  }}}}}}
# 
# 
# #three-way Cross#######################################################
# patterns.08 = paste0("\\(",dhStringped,"/", dhStringped,"/", dhStringped,"\\)","[[:alnum:]]*(?:-[[:alnum:]]+){2}")
# 
# #(2FACC/999165/065125)-06-5
# for(i in 1:nrow(data)) {
#   if(!str_detect(data[i,3], pattern = "\\.DH")){
#     if(!str_detect(data[i,3], pattern =patterns30)){
#       if(!str_detect(data[i,3], pattern = patterns.25)){
#         if(!str_detect(data[i,3], pattern = patterns.27)){
#           if(!str_detect(data[i,3], pattern = patterns.05)){
#             if(!str_detect(data[i,3], pattern = patterns.04)){
#               if(!str_detect(data[i,3], pattern = patterns.06)){
#                 if(str_detect(data[i,3], pattern = patterns.08)){
#                   
#                   replacement = str_match(pattern = patterns.08, data[i,3])
#                   data[i,3] = replacement[1] # add a period for using wildcards
#                 }}  }}}}}}}
# 
# #: Cross#######################################################
# 
# dhStringped3 = paste0("[[:alnum:]]*(?:-[[:alnum:]]+){0,2}",digitDH,"?", nondigitDH,"?","(?:-[[:alnum:]]+){0,1}")
# patterns.20=paste0("^\\(",dhStringped,":",dhStringped,"\\)", dhStringped3)
# #(BR52051:S172641)-B-018-B #F4
# 
# for(i in 1:nrow(data)) {
#   if(str_detect(data[i,3], pattern = ":")){
#     #if(str_detect(data[i,3], pattern =digitDH)){
#     if(str_detect(data[i,3], pattern = patterns.20)){
#       replacement = str_match(pattern = patterns.20, data[i,3])
#       data[i,3] = replacement[1] # add a period for using wildcards
#     }}  }
# 
# 






































# # for(i in 1:nrow(data)) {
# #   if(!grepl(data[i,3], pattern ="\\.DH")){
# #     if(str_detect(data[i,3], pattern = conv2)){
# #       if(!str_detect(data[i,3], pattern =conv1)){
# #         if(!str_detect(data[i,3], pattern =conv3)){
# #           if(!str_detect(data[i,3], pattern =conv4)){
# #             if(!str_detect(data[i,3], pattern =conv5)){
# #               if(!str_detect(data[i,3], pattern =conv6)){
# #                 if(!str_detect(data[i,3], pattern =conv7)){
# #                   if(!str_detect(data[i,3], pattern =conv8)){
# #                     if(!str_detect(data[i,3], pattern =conv9)){
# #                       if(!str_detect(data[i,3], pattern =conv10)){
# #                         if(!str_detect(data[i,3], pattern =conv11)){
# #       replacement = str_match(pattern= conv2,data[i,3])
# #       data[i,3] = replacement
# #     }}}}}}}}}}}}}
# # 
# # for(i in 1:nrow(data)) {
# #   if(!grepl(data[i,3], pattern ="\\.DH")){
# #     if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv1)){
# #         if(!str_detect(data[i,3], pattern =conv4)){
# #           if(!str_detect(data[i,3], pattern =conv5)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv7)){
# #                 if(!str_detect(data[i,3], pattern =conv8)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv10)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #     # add a period for using wildcards
# #     if(str_detect(data[i,3], pattern = conv3)){
# #       replacement = str_match(pattern= conv3,data[i,3])
# #       data[i,3] = replacement# add a period for using wildcards
# #     }}}}}}}}}}}}}
# # 
# # for(i in 1:nrow(data)) {
# #   if(!grepl(data[i,3], pattern ="\\.DH")){
# #     if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv3)){
# #         if(!str_detect(data[i,3], pattern =conv1)){
# #           if(!str_detect(data[i,3], pattern =conv5)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv7)){
# #                 if(!str_detect(data[i,3], pattern =conv8)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv10)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #     if(str_detect(data[i,3], pattern =  conv4)){
# #       replacement = str_match(pattern= conv4,data[i,3])
# #       data[i,3] = replacement # add a period for using wildcards
# #     }}}}}}}}}}}}}
# # 
# # for(i in 1:nrow(data)) {
# #   if(!str_detect(data[i,3], pattern =patterns.4way)){
# #     if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv3)){
# #         if(!str_detect(data[i,3], pattern =conv4)){
# #           if(!str_detect(data[i,3], pattern =conv1)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv7)){
# #                 if(!str_detect(data[i,3], pattern =conv8)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv10)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #     
# #     if(!grepl(data[i,3], pattern ="\\.DH")){
# #       if(str_detect(data[i,3], pattern =  conv5)){
# #         replacement = str_match(pattern= conv5,data[i,3])
# #         data[i,3] = replacement # add a period for using wildcards
# #       }}}}}}}}}}}}}}
# 
# #Three-way crosses######################################################
# ########
# patterns.4way=paste0("^[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,1}/[\\(]{0,1}",dhStringped,"/",dhStringped,"[\\)]{0,1}","(?:-addgen){4}")
# 
# #(I11063/I12003)/065125-35-5 F4
# #(065125/054245)/I10001-29-5-1-1-1-3-01	
# #(I11063/I12003)/065125-35-5-1-1-B	
# 
# 
# for(i in 1:nrow(data)) {
#   if(!str_detect(data[i,3], pattern =patterns.4way)){
#     if(!str_detect(data[i,3], pattern =conv2)){
#       if(!str_detect(data[i,3], pattern =conv3)){
#         if(!str_detect(data[i,3], pattern =conv4)){
#           if(!str_detect(data[i,3], pattern =conv5)){
#             if(!str_detect(data[i,3], pattern =conv1)){
#               if(!str_detect(data[i,3], pattern =conv7)){
#                 if(!str_detect(data[i,3], pattern =conv8)){
#                   if(!str_detect(data[i,3], pattern =conv9)){
#                     if(!str_detect(data[i,3], pattern =conv10)){
#                       if(!str_detect(data[i,3], pattern =conv11)){
#                         if(!grepl(data[i,3], pattern ="\\.DH")){
#                           if(str_detect(data[i,3], pattern = conv6)){
#                             replacement = str_match(pattern=conv6, data[i,3])
#                             data[i,3] = replacement # add a period for using wildcards
#                           }}}}}}}}}}}}}}
# 
# #((PHV78/PHG47)/3IIH6)-B-002-1
# # for(i in 1:nrow(data)) {
# #   if(!str_detect(data[i,3], pattern =patterns.4way)){
# #     if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv3)){
# #         if(!str_detect(data[i,3], pattern =conv4)){
# #           if(!str_detect(data[i,3], pattern =conv5)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv1)){
# #                 if(!str_detect(data[i,3], pattern =conv8)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv10)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #     if(!grepl(data[i,3], pattern ="\\.DH")){
# #       if(str_detect(data[i,3], pattern = conv7)){
# #         replacement = str_match(pattern=conv7, data[i,3])
# #         data[i,3] = replacement # add a period for using wildcards
# #       }}}}}}}}}}}}}}
# 
# # #DSR065125/(065125/I11063))-B-084-B	
# # for(i in 1:nrow(data)) {
# #   if(!grepl(data[i,3], pattern ="\\.DH")){
# #     if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv3)){
# #         if(!str_detect(data[i,3], pattern =conv4)){
# #           if(!str_detect(data[i,3], pattern =conv5)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv7)){
# #                 if(!str_detect(data[i,3], pattern =conv1)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv10)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #     if(str_detect(data[i,3], pattern = conv8)){
# #       replacement = str_match(pattern=conv8,data[i,3])
# #       data[i,3] = replacement # add a period for using wildcards
# #     }}}}}}}}}}}}}
# # 
# # #FF6224/BJH104-B-B)/I12003)-B-149-B
# # for(i in 1:nrow(data)) {
# #   if(!grepl(data[i,3], pattern ="\\.DH")){
# #     if(str_detect(data[i,3], pattern = conv9)){
# #       if(!str_detect(data[i,3], pattern =conv2)){
# #         if(!str_detect(data[i,3], pattern =conv3)){
# #           if(!str_detect(data[i,3], pattern =conv4)){
# #             if(!str_detect(data[i,3], pattern =conv5)){
# #               if(!str_detect(data[i,3], pattern =conv6)){
# #                 if(!str_detect(data[i,3], pattern =conv7)){
# #                   if(!str_detect(data[i,3], pattern =conv8)){
# #                     if(!str_detect(data[i,3], pattern =conv1)){
# #                       if(!str_detect(data[i,3], pattern =conv10)){
# #                         if(!str_detect(data[i,3], pattern =conv11)){
# #       replacement = str_match(pattern=conv9,data[i,3])
# #       data[i,3] = replacement # add a period for using wildcards
# #     }}}}}}}}}}}}}
# # 
# # #(65125/I10001)/I12003)-B-B-32-1-1-B	
# # for(i in 1:nrow(data)) {
# #   if(!str_detect(data[i,3], pattern =patterns.4way)){
# #     
# #     if(!grepl(data[i,3], pattern ="\\.DH")){     
# #       if(!str_detect(data[i,3], pattern =conv2)){
# #       if(!str_detect(data[i,3], pattern =conv3)){
# #         if(!str_detect(data[i,3], pattern =conv4)){
# #           if(!str_detect(data[i,3], pattern =conv5)){
# #             if(!str_detect(data[i,3], pattern =conv6)){
# #               if(!str_detect(data[i,3], pattern =conv7)){
# #                 if(!str_detect(data[i,3], pattern =conv8)){
# #                   if(!str_detect(data[i,3], pattern =conv9)){
# #                     if(!str_detect(data[i,3], pattern =conv1)){
# #                       if(!str_detect(data[i,3], pattern =conv11)){
# #       if(str_detect(data[i,3], pattern = conv10)){
# #         replacement = str_match(pattern= conv10,data[i,3])
# #         data[i,3] = replacement # add a period for using wildcards
# #       }}}}}}}}}}}}}}
# # 
# #BDA015/BDA032//BHH069)-B-093-1-1	
# for(i in 1:nrow(data)) {
#   if(!grepl(data[i,3], pattern ="\\.DH")){
#     if(!str_detect(data[i,3], pattern =conv2)){
#       if(!str_detect(data[i,3], pattern =conv3)){
#         if(!str_detect(data[i,3], pattern =conv4)){
#           if(!str_detect(data[i,3], pattern =conv5)){
#             if(!str_detect(data[i,3], pattern =conv6)){
#               if(!str_detect(data[i,3], pattern =conv7)){
#                 if(!str_detect(data[i,3], pattern =conv8)){
#                   if(!str_detect(data[i,3], pattern =conv9)){
#                     if(!str_detect(data[i,3], pattern =conv10)){
#                       if(!str_detect(data[i,3], pattern =conv1)){
#                         if(str_detect(data[i,3], pattern = conv11)){
#                           replacement = str_match(pattern=conv11, data[i,3])
#                           data[i,3] = replacement # add a period for using wildcards
#                         }}}  }}}}}}}}}}
# 
# 
# for(i in 1:nrow(data)) {
#   if(!str_detect(data[i,3], pattern =patterns.4way)){
#     if(!grepl(data[i,3], pattern ="\\.DH")){
#       if(!str_detect(data[i,3], pattern =conv4)){
#         if(!str_detect(data[i,3], pattern =conv3)){
#           #if(!str_detect(data[i,3], pattern =conv5)){
#           
#           if(str_detect(data[i,3], pattern = conv2)){
#             replacement = str_match(pattern=conv2, data[i,3])
#             data[i,3] = replacement # add a period for using wildcards
#           }}} }}}#}
# conv2 = paste0(female.inbred,extend.inbred,"/",male.inbred,generation)

#conv3 = paste0(female.inbred,"/",addgen,extend.inbred,generation)

#conv4 = paste0(female.inbred,"/",addgen,generation)

# conv5 = paste0(female.inbred,extend.inbred,"/",addgen,extend.inbred,generation)


#conv2 = paste0(female.inbred,"/",male.inbred,"/",addgen,generation2)

#((PHV78/PHG47)/3IIH6)-B-002-1
#conv7 =  paste0(female.inbred,"/",addgen,extend.inbred,"/",addgen,generation)

#DSR065125/(065125/I11063))-B-084-B	
# conv8 =  paste0(female.inbred,extend.inbred,"/",female.inbred,extend.inbred,"/",addgen,extend.inbred,generation)

#FF6224/BJH104-B-B)/I12003)-B-149-B
#conv9 = paste0(female.inbred,extend.inbred,"/",addgen,extend.inbred,"/",addgen,extend.inbred,generation)

#(65125/I10001)/I12003)-B-B-32-1-1-B	
#conv10 = paste0(addgen,extend.inbred,"/",addgen,extend.inbred,"/",addgen, extend.inbred, generation)
