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
  season = "21S",
  inbred = "BRS313",
  male =   data.frame(male=c('BSQ033',	'GP734GTCBLL', "BFA143", "BQS025", "BQS986",
                             'BRQ529', 'GP718',	'BSR095',
                             'BRP251', 'BUR070',	'BRS312',
                             'BAC020','BSU151', 'GP6823Hx1',	'I10516',	'W8039RPGJZ',
                             'GP717', 'BAA441',	'GP738Hx1',	'BHH069',
                             'TR4949', "BRS312", "BRS314", 'I12003',	'BSQ941',
                             "BAA419","BHB075","BHJ471","GP702",
                             "40QHQ-E07", "BQS941","BRS313","BSS009","GP738","BRR553"
  )),
  genotype=F,
  seed=30,
  nthread=8

)
















