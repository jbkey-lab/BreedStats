# library(devtools)
# install_github("jbkey730/BreedStats")


library(BreedStats)



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
  inbred = "BRS312",
  rounds = 3000,
  eta = 1,
  lambda = 0.0003,
  alpha = 0.0003,
  male =   data.frame(male=c('BSQ033',	'GP734GTCBLL',	'BEX905',	'R08072HT',
                    'BRQ529',	'8D2',	'SGI193',	'8SY',
                    'GP718',	'FC2',	'BSR095',	'BRU059',
                    'BRQ291',	'BRP251',	'TR6254RR2',	'BSQ033-PWRA',
                    'BUR070',	'BRS312',	'8SY-AM',	'GP717Hx1',
                    'BAC020',	'TPCJ6605',	'BSU151',	'SGI193-V2P',
                    'BRQ064',	'GP6823Hx1',	'I10516',	'W8039RPGJZ',
                    'FB6455',	'BSU311',	'GP717',	'BSQ002',
                    'BAA441',	'GP738Hx1',	'BHH069',
                    '84Z',	'TR4949',	'GP695Hx1',	'BSU313',
                    'BHA493',	'R2846-NS6408DGV2P',	'I12003',	'R2846',
                    'BSR273',	'BSQ941',	'BUR032',	'PRW-AM',
                    'GP718Hx1',	'24AED-D02',"BAA419","BAA411","BHB075","BHJ471","GP702",
                    "40QHQ-E07", "BQS941","BRS313","BSS009","GP738","BRR553",
                   "BCA509","F8994","T1874","BUR011", "BQR334",
                    "R6076","BRQ041","BBH030","F9898","85E","LFX7508","FC2YHR"
  ))
)


