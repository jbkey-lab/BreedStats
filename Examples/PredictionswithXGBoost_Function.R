# library(devtools)
# install_github("jbkey730/BreedStats")


library(BreedStats)
library(dplyr)



datasets=xgblinearBV(
  sdp = "C:/Users/jake.lamkey/Documents/",
  fdp= "C:/Users/jake.lamkey/Documents/",
  s0=T,
  s1 =T,
  s2 =F,
  s3 =F,
  s4 =F,
  s5 =F,
  seas0 = 21,
  seas1 = 20,
  seas2 = "",
  seas3 = "",
  seas4 = "",
  seas5 = "",
  season = "22S",
  inbred = "BRS313",
  male =   data.frame(male=c('40QHQ-E07',
                             'BAA039',
                             'BAA441',
                             'BAC020',
                             'BHB075',
                             'BHJ471',
                             'BQS025',
                             'BQS986',
                             'BRQ064',
                             'BRR553',
                             'BSR273',
                             'BSU151',
                             'BUR032',
                             'GP718',
                             'BAA419',
                             'BFA143',
                             'BRS312',
                             'BSQ941',
                             'BSR095',
                             'BSS009',
                             'BHI306',
                             '12AZD81',
                             '24ILP-E02',
                             '40APQ-F03',
                             '40PAI-C02',
                             '81AMA-H02',
                             'R18026',
                             'RQQ041'
  )),
  genotype=F,
  seed=30,
  nthread=8

)
















