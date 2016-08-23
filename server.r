library(shiny)
library(shinydashboard)
library(plotly)
library(scales)
library(data.table)
library(dplyr)

setwd("C:/Users/Veronica/Desktop/ISS Final")
isshist = read.csv("issforhistogram2.csv", stringsAsFactors=FALSE)
isslevels = read.csv("issforcritlevels2.csv", stringsAsFactors=FALSE)
riskflip = read.csv("riskflip.csv", stringsAsFactors=FALSE)
riskfeed = read.csv("riskfeed.csv", stringsAsFactors=FALSE)
sorted = read.csv ("MostandLeastCrit_Sorted.csv")
comments = read.csv("finishedcomments2.csv", stringsAsFactors=FALSE, allowEscapes=TRUE)
triplefins = read.csv("triplefinancials.csv", stringsAsFactors=FALSE)
compiled = read.csv("compiledH.csv", stringsAsFactors=FALSE)
apematch = read.csv("ape-iss-match.csv", stringsAsFactors=FALSE)

custom = list(
  "100: Installation Management" = "ISS100",
  "102: Administration and Civil Law" = "ISS102",
  "103: Criminal Law and Discipline" = "ISS103",
  "104: Client Services" = "ISS104",
  "105: Claims" = "ISS105",
  "106: Religious Support" = "ISS106",
  "107: Public Affairs" = "ISS107",
  "109: Equal Employment Opportunity (EEO)" = "ISS109",
  "111: Internal Review" = "ISS111",
  "112: Installation Safety/Occupational Health" = "ISS112",
  "113: Administrative Management" = "ISS113",
  "115: Program Budget" = "ISS115",
  "116: Support Agreement (MOU/MOA) Mgmt" = "ISS116",
  "118: Installation TDA Management" = "ISS118",
  "121: Management Analysis" = "ISS121",
  "200: UEPH/SEBQ/UOQ Management" = "ISS200",
  "201: Family Housing Management"  = "ISS201",
  "202: Army Lodging Management" = "ISS202",
  "250: Substance Abuse" = "ISS250",
  "251: Army Community Service" = "ISS251",
  "252: Child, Youth, and School Services" = "ISS252",
  "253: Sports, Recreation, and Libraries" = "ISS253",
  "254: Business Operations" = "ISS254",
  "300: Clothing and Equipment" = "ISS300",
  "301: Retail Supply" = "ISS301",
  "302: Asset Management" = "ISS302",
  "303: Military Clothing and Sales" = "ISS303",
  "304: Laundry and Dry-Cleaning Services" = "ISS304",
  "305: Food Services" = "ISS305",
  "306: Material Support Maintenance" = "ISS306",
  "307: Transportation Services - NTVs" = "ISS307",
  "308: Transportation Services - Personal Property" = "ISS308",
  "309: Supply and Storage" = "ISS309",
  "400: Facilities Engineering Services Mgmt" = "ISS400",
  "401: Fire and Emergency Response Service" = "ISS401",
  "402: Custodial Services" = "ISS402",
  "403: Refuse Removal" = "ISS403",
  "404: Maintenance - Grounds" = "ISS404",
  "405: Master Planning" = "ISS405",
  "406: Real Estate/Real Property  Administration" = "ISS406",
  "408: Snow, Ice, and Sand Removal" = "ISS408",
  "411: Facilities Maintenance - Vertical" = "ISS411",
  "414: Facilities Maintenance - Army Family Housing" = "ISS414",
  "420: Facilities Maintenance - Horizontal" = "ISS420",
  "500: Electrical Services" = "ISS500",
  "501: Heating/Cooling services" = "ISS501",
  "502: Water Services" = "ISS502",
  "503: Waste Water Services" = "ISS503",
  "504: Other Utility Services" = "ISS504",
  "505: Compliance Services" = "ISS505",
  "506: Conservation Services" = "ISS506",
  "507: Pollution Prevention Services" = "ISS507",
  "510: Pest Management" = "ISS510",
  "600: Physical Security" = "ISS600",
  "601: Law Enforcement Services" = "ISS601",
  "602: Anti-Terrorism Services" = "ISS602",
  "603: Installation Security Program Mgmt Support" = "ISS603",
  "604: Emergency Management" = "ISS604",
  "700: Automation" = "ISS700",
  "701: Communication" = "ISS701",
  "702: Multimedia/Visual Information" = "ISS702",
  "703: Information Assurance"= "ISS703",
  "800: Military Personnel Services" = "ISS800",
  "803: Continuing Education Services" = "ISS803",
  "900: Airfield Operations" = "ISS900",
  "901: Mobilization and Deployment Support" = "ISS901",
  "902: Command and Control" = "ISS902",
  "903: Training Land Sustainment" = "ISS903",
  "904: Range Operations" = "ISS904",
  "905: Training Support Center" = "ISS905",
  "906: Battle Command Training Center" = "ISS906")

netcom = list(
  "701: Communication Systems and System Support" = "ISS701",
  "703: Information Assurance" = "ISS703",
  "700: Automation" = "ISS700")

amc = list(
  "300: Clothing and Equipment" = "ISS300",
  "301: Retail Supply" = "ISS301",
  "302:  Asset Management" = "ISS302",
  "303:	Military Clothing and Sales" = "ISS303",
  "304:	Laundry and Dry-Cleaning Services" = "ISS304",
  "305:	Food Services" = "ISS304",
  "306:	Material Support Maintenance" = "ISS306",
  "307:	Transportation Services - NTVs" = "ISS307",
  "308:	Transportation Services - Personal Property" = "ISS308",
  "309:	Supply and Storage" = "ISS309")

imcom = list(
  "100: Installation Management" = "ISS100",
  "102: Administration and Civil Law" = "ISS102",
  "103: Criminal Law and Discipline" = "ISS103",
  "104: Client Services" = "ISS104",
  "105: Claims" = "ISS105",
  "106: Religious Support" = "ISS106",
  "107: Public Affairs" = "ISS107",
  "109: Equal Employment Opportunity (EEO)" = "ISS109",
  "111: Internal Review" = "ISS111",
  "112: Installation Safety/Occupational Health" = "ISS112",
  "115: Program Budget" = "ISS115",
  "116: Support Agreement (MOU/MOA) Mgmt" = "ISS116",
  "118: Installation TDA Management" = "ISS118",
  "121: Management Analysis" = "ISS121",
  "113: Administrative Management" = "ISS113",
  "250: Substance Abuse" = "ISS250",
  "800: Military Personnel Services" = "ISS800",
  "803: Continuing Education Services" = "ISS803",
  "202: Army Lodging Management" = "ISS202",
  "251: Army Community Service" = "ISS251",
  "252: Child, Youth, and School Services" = "ISS252",
  "253: Sports, Recreation, and Libraries" = "ISS253",
  "254: Business Operations" = "ISS254",
  "201: Family Housing Management"  = "ISS201",
  "414: Facilities Maintenance - Army Family Housing" = "ISS414",
  "411: Facilities Maintenance - Vertical" = "ISS411",
  "420: Facilities Maintenance - Horizontal" = "ISS420",
  "200: UEPH/SEBQ/UOQ Management" = "ISS200",
  "400: Facilities Engineering Services Mgmt" = "ISS400",
  "402: Custodial Services" = "ISS402",
  "403: Refuse Removal" = "ISS403",
  "404: Maintenance - Grounds" = "ISS404",
  "405: Master Planning" = "ISS405",
  "406: Real Estate/Real Property  Administration" = "ISS406",
  "408: Snow, Ice, and Sand Removal" = "ISS408",
  "500: Electrical Services" = "ISS500",
  "501: Heating/Cooling services" = "ISS501",
  "502: Water Services" = "ISS502",
  "503: Waste Water Services" = "ISS503",
  "504: Other Utility Services" = "ISS504",
  "505: Compliance Services" = "ISS505",
  "506: Conservation Services" = "ISS506",
  "507: Pollution Prevention Services" = "ISS507",
  "510: Pest Management" = "ISS510",
  "401: Fire and Emergency Response Service" = "ISS401",
  "600: Physical Security" = "ISS600",
  "601: Law Enforcement Services" = "ISS601",
  "602: Anti-Terrorism Services" = "ISS602",
  "603: Installation Security Program Mgmt Support" = "ISS603",
  "604: Emergency Management" = "ISS604",
  "702: Multimedia/Visual Information" = "ISS702",
  "900: Airfield Operations" = "ISS900",
  "901: Mobilization and Deployment Support" = "ISS901",
  "902: Command and Control" = "ISS902",
  "903: Training Land Sustainment" = "ISS903",
  "904: Range Operations" = "ISS904",
  "905: Training Support Center" = "ISS905",
  "906: Battle Command Training Center" = "ISS906")

high = list(
  '800: Military Personnel Services' = "ISS800",
  '252: Child, Youth, and School Services' = "ISS252",
  '500: Electrical Services' = "ISS500",
  '501: Heating/Cooling Services' = "ISS501",
  '502: Water Services' = "ISS502",
  '503: Waste Water Services' = "ISS503",
  '504: Other Utility Services' = "ISS504",
  '401: Fire and Emergency Response Service' = "ISS401",
  '600: Physical Security' = "ISS600",
  '601: Law Enforcement Services' = "ISS601",
  '603: Installation Security Program Mgmt Support' = "ISS603",
  '604: Emergency Management' = "ISS604",
  '900: Airfield Operations' = "ISS900",
  '901: Mobilization and Deployment Support' = "ISS901",
  '701: Communication Systems and System Support' = "ISS701",
  '703: Information Assurance' = "ISS703",
  '300: Clothing and Equipment' = "ISS300",
  '306: Material Support Maintenance' = "ISS306")

modhigh = list(
  "902:  Command and Control" = "ISS902",
  "904:	Range Operations" = "ISS904",
  "602:	Anti-Terrorism Services" = "ISS602",
  "903:	Training Land Sustainment" = "ISS903",
  "305:	Food Services" = "ISS305",
  "201:	Family Housing Management" = "ISS201",
  "700:	Automation" = "ISS700",
  "309:	Supply and Storage" = "ISS309",
  "103:	Criminal Law and Discipline" = "ISS103",
  "905:	Training Support Center" = "ISS905",
  "302:	Asset Management" = "ISS302",
  "100:	Installation Management" = "ISS100",
  "301:	Retail Supply" = "ISS301",
  "906:	Battle Command Training Center (MTC)" = "ISS906",
  "414:	Facilities Maintenance - Army Family Housing" = "ISS414",
  "115:	Program Budget" = "ISS115",
  "251:	Army Community Service" = "ISS251",
  "308:	Transportation Services - Personal Property" = "ISS308")

mod = list(
  "102:  Administration and Civil Law" = "ISS102",
  "420:	Facilities Maintenance - Horizontal" = "ISS420",
  "250:	Substance Abuse" = "ISS250",
  "411:	Facilities Maintenance - Vertical" = "ISS411",
  "307:	Transportation Services - NTVs" = "ISS307",
  "303:	Military Clothing and Sales" = "ISS303",
  "200:	UEPH/SEBQ/UOQ Management" = "ISS200",
  "403:	Refuse Removal" = "ISS403",
  "803:	Continuing Education Services" = "ISS803",
  "400:	Facilities Engineering Services Mgmt" = "ISS400",
  "121:	Management Analysis" = "ISS121",
  "113:	Administrative Management" = "ISS113",
  "202:	Army Lodging Management" = "ISS202",
  "253:	Sports, Recreation, and Libraries" = "ISS253",
  "505:	Compliance Services" = "ISS505",
  "406:	Real Estate/Real Property Administration" = "ISS406",
  "105:	Claims" = "ISS105",
  "405:	Master Planning" = "ISS405",
  "112:	Installation Safety and Occupational Health" = "ISS112",
  "116:	Support Agreement (MOU/MOA) Mgmt" = "ISS116",
  "118:	Installation TDA Management" = "ISS118",
  "104:	Client Services" = "ISS104")

low = list(
  "510:  Pest Management" = "ISS510",
  "111:	Internal Review" = "ISS111",
  "106:	Religious Support" = "ISS106",
  "408:	Snow, Ice, and Sand Removal" = "ISS408",
  "109:	Equal Employment Opportunity (EEO)" = "ISS409",
  "507:	Pollution Prevention Services" = "ISS507",
  "402:	Custodial Services" = "ISS402",
  "304:	Laundry and Dry-Cleaning Services" = "ISS304",
  "506:	Conservation Services" = "ISS506",
  "404:	Maintenance - Grounds" = "ISS404",
  "702:	Multimedia/Visual Information Processes" = "ISS702",
  "254:	Business Operations" = "ISS254",
  "107:	Public Affairs" = "ISS107")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ##METHODOLOGY PAGE
  output$TotalFunds <- renderValueBox({
    valueBox(
      paste("$76,349,596,000"), "Total ISS Funding Included in Analysis (FY10-FY21)",  icon = icon("fa fa-usd"),
      color = "blue"
    )
  })
  
  output$TotalReqs <- renderValueBox({
    valueBox(
      paste("$85,921,161,000"), "Total ISS Requirement Included in Analysis (FY10-FY21)",  icon = icon("fa fa-usd"),
      color = "maroon"
    )
  })

  
  output$TotalDefs <- renderValueBox({
    valueBox(
      paste("$-9,550,970,000"), "Total ISS Deficit Calculated in Analysis (FY10-FY21)",  icon = icon("fa fa-usd"),
      color = "aqua"
    )
  })
  
  #############################################################################################
  ###############################RISK PAGE#######################################
  #############################################################################################
  #SELECT ALL SERVICES
  output$riskscatter <- renderPlotly({
    
    dataRS <- switch(input$selectRS, 
                   "Y2010" = riskfeed$Y2010,
                   "Y2011" = riskfeed$Y2011,
                   "Y2012" = riskfeed$Y2012,
                   "Y2013" = riskfeed$Y2013,
                   "Y2014" = riskfeed$Y2014,
                   "Y2015" = riskfeed$Y2015,
                   "Y2016" = riskfeed$Y2016,
                   "Y2017" = riskfeed$Y2017,
                   "Y2018" = riskfeed$Y2018,
                   "Y2019" = riskfeed$Y2019,
                   "Y2020" = riskfeed$Y2020,
                   "Y2021" = riskfeed$Y2021,)
    
    defRS <- switch(input$selectRS, 
                     "Y2010" = riskfeed$Def10,
                     "Y2011" = riskfeed$Def11,
                     "Y2012" = riskfeed$Def12,
                     "Y2013" = riskfeed$Def13,
                     "Y2014" = riskfeed$Def14,
                     "Y2015" = riskfeed$Def15,
                     "Y2016" = riskfeed$Def16,
                     "Y2017" = riskfeed$Def17,
                     "Y2018" = riskfeed$Def18,
                     "Y2019" = riskfeed$Def19,
                     "Y2020" = riskfeed$Def20,
                     "Y2021" = riskfeed$Def21,)
    
    sizeRS <- switch(input$selectRS, 
                    "Y2010" = riskfeed$Size10,
                    "Y2011" = riskfeed$Size11,
                    "Y2012" = riskfeed$Size12,
                    "Y2013" = riskfeed$Size13,
                    "Y2014" = riskfeed$Size14,
                    "Y2015" = riskfeed$Size15,
                    "Y2016" = riskfeed$Size16,
                    "Y2017" = riskfeed$Size17,
                    "Y2018" = riskfeed$Size18,
                    "Y2019" = riskfeed$Size19,
                    "Y2020" = riskfeed$Size20,
                    "Y2021" = riskfeed$Size21,)
    
    chosen2 <- switch(input$findISSrisk, 
                      "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                      "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                      "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                      "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                      "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                      "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                      "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                      "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                      "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                      "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                      "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                      "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                      "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                      "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                      "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                      "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                      "ISS307"=69, "ISS308"=70, "ISS309"=71)
    
    
    a <- list(
      x = riskfeed$AVG[chosen2],
      y = dataRS[chosen2],
      text = paste(input$findISSrisk),
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 5,
      ax = 20,
      ay = -40
    )
    
    datanames <- riskfeed$Name
    AVGs <- riskfeed$AVG
    df <- data.frame(datanames, AVGs, dataRS, defRS, sizeRS)

    overdataRS <- subset(df$dataRS, dataRS > 0)
    overdefRS <- subset(df$defRS, dataRS > 0)
    oversizeRS <- subset(df$sizeRS, dataRS > 0)
    overname <- subset(df$datanames, dataRS > 0)
    overAVG <- subset(df$AVGs, dataRS > 0)
    
    metdataRS <- subset(df$dataRS, dataRS == 0)
    metdefRS <- subset(df$defRS, dataRS == 0)
    metsizeRS <- subset(df$sizeRS, dataRS == 0)
    metname <- subset(df$datanames, dataRS == 0)
    metAVG <- subset(df$AVGs, dataRS == 0)
    
    underdataRS <- subset(df$dataRS, dataRS < 0)
    underdefRS <- subset(df$defRS, dataRS < 0)
    undersizeRS <- subset(df$sizeRS, dataRS < 0)
    undername <- subset(df$datanames, dataRS < 0)
    underAVG <- subset(df$AVGs, dataRS < 0)
    
    overdf <- data.frame(overname, overAVG, overdataRS, overdefRS, oversizeRS)
    underdf <- data.frame(undername, underAVG, underdataRS, underdefRS, undersizeRS)
    metdf <- data.frame(metname, metAVG, metdataRS, metdefRS, metsizeRS)
    
    together1 <- paste("Funding Delta: ", dollar(overdefRS))
    together2 <- paste("Funding Delta: ", dollar(metdefRS))
    together3 <- paste("Funding Delta: ", dollar(underdefRS))
    
    color1 <- ifelse(overAVG > 7.34, 'red', ifelse(overAVG > 6.27, 'darkorange', 
                        ifelse(overAVG > 5.20, 'gold', 'green')))
    
    color3 <- ifelse(underAVG > 7.34, 'red', ifelse(underAVG > 6.27, 'darkorange', 
                        ifelse(underAVG > 5.20, 'gold', 'green')))
    
    color2 <- ifelse(metAVG > 7.34, 'red', ifelse(metAVG > 6.27, 'darkorange', 
                                                    ifelse(metAVG > 5.20, 'gold', 'green')))
    
    pRS <- plot_ly(overdf, x=overAVG, y=overdataRS, 
                   text=paste(overname, together1, sep='<br>'),
                   name='Over-Funded',
                   type='scatter', mode="markers", marker=list(size=8*oversizeRS, color=color1, 
                                                               line=list(color='black'), opacity=0.4)) 
    pRS <- add_trace(metdf, x=metAVG, y=metdataRS, 
                     text=paste(metname, together2, sep='<br>'),
                     name='Funding Met',
                     type='scatter', mode="markers", marker=list(size=8*metsizeRS, color=color2, 
                                                                 line=list(color='black'), opacity=0.7)) 
    
    pRS <- add_trace(underdf, x=underAVG, y=underdataRS, 
                   text=paste(undername, together3, sep='<br>'),
                   name='Under-Funded',
                   type='scatter', mode="markers", marker=list(size=8*undersizeRS, color=color3, 
                                                               opacity=0.8)) 
    
    layout(pRS, title=paste('Risk for All Installation Services in', input$selectRS),
           yaxis=list(title="Ratio of Funding Delta/Requirement (%)"), 
           xaxis=list(title="Level of Impact to Readiness", autorange=T, autotick=T),
           annotations=a, showlegend=TRUE) 
  })
  
  
  #Mini Year Graph for Chosen Service
  output$minirisk <- renderPlotly({
  
  dataRS <- switch(input$selectRS, 
                   "Y2010" = riskfeed$Y2010, "Y2011" = riskfeed$Y2011,
                   "Y2012" = riskfeed$Y2012, "Y2013" = riskfeed$Y2013,
                   "Y2014" = riskfeed$Y2014, "Y2015" = riskfeed$Y2015,
                   "Y2016" = riskfeed$Y2016, "Y2017" = riskfeed$Y2017, 
                   "Y2018" = riskfeed$Y2018, "Y2019" = riskfeed$Y2019, 
                   "Y2020" = riskfeed$Y2020, "Y2021" = riskfeed$Y2021,)
  
  chosen3 <- switch(input$findISSrisk, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)
  
  years <- c("`10", "`11", "`12", "`13", "`14", 
            "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  ratiodata <- c(riskfeed$Y2010[chosen3], riskfeed$Y2011[chosen3],
                riskfeed$Y2012[chosen3], riskfeed$Y2013[chosen3],
                riskfeed$Y2014[chosen3], riskfeed$Y2015[chosen3],
                riskfeed$Y2016[chosen3], riskfeed$Y2017[chosen3],
                riskfeed$Y2018[chosen3], riskfeed$Y2019[chosen3],
                riskfeed$Y2020[chosen3], riskfeed$Y2021[chosen3])
  
  defdata <- c(riskfeed$Def10[chosen3], riskfeed$Def11[chosen3],
                 riskfeed$Def12[chosen3], riskfeed$Def13[chosen3],
                 riskfeed$Def14[chosen3], riskfeed$Def15[chosen3],
                 riskfeed$Def16[chosen3], riskfeed$Def17[chosen3],
                 riskfeed$Def18[chosen3], riskfeed$Def19[chosen3],
                 riskfeed$Def20[chosen3], riskfeed$Def21[chosen3])
  
  sizedata <- c(riskfeed$Size10[chosen3], riskfeed$Size11[chosen3],
               riskfeed$Size12[chosen3], riskfeed$Size13[chosen3],
               riskfeed$Size14[chosen3], riskfeed$Size15[chosen3],
               riskfeed$Size16[chosen3], riskfeed$Size17[chosen3],
               riskfeed$Size18[chosen3], riskfeed$Size19[chosen3],
               riskfeed$Size20[chosen3], riskfeed$Size21[chosen3])
  
  colordata <- ifelse(riskfeed$AVG[chosen3] > 7.34, 'red', 
                  ifelse(riskfeed$AVG[chosen3]> 6.27, 'darkorange', 
                  ifelse(riskfeed$AVG[chosen3] > 5.20, 'gold', 'green')))
  
  opacitydata <- ifelse(ratiodata > 0, 0.5, 0.8)
  info <- ifelse(ratiodata > 0, "OVERFUNDED", ifelse(ratiodata < 0, "UNDERFUNDED", "FUNDING MET"))
  together <- paste("Funding Delta: ", dollar(defdata))
  
  point <- list(x = input$selectRS, y = dataRS[chosen3],
    text = paste(input$findISSrisk), xref = "x", yref = "y",
    showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40)
  
  MRS <- plot_ly(riskfeed, x=colnames(riskfeed[3:14],),y=ratiodata, 
                 text=paste(Name[chosen3], sep='<br>',together, info),
                 type='scatter', mode="markers", marker=list(size=4*sizedata, 
                 color=colordata, opacity=opacitydata)) 
  
  layout(MRS, title=paste('Annual Risk for ', Name[chosen3]), 
         titlefont=list(size=12), 
         yaxis=list(title="Funding Delta/Requirement Ratio"), 
         xaxis=list(title="Year", tickmode="array", tickvals=colnames(riskfeed[3:14],), 
                    ticktext=years, tickfont=list(size=10), tickangle=0),
         annotations=point) 
})
  
  #COMPARE ISS
output$comparerisk <- renderPlotly({
  df <- riskflip[c(1:37),]
  df$dummy <- NA
  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  p <- plot_ly(df, type='scatter', mode="markers",y=dummy, x=years, 
               showlegend = FALSE)
  
  #####plot additional traces based on selections
  #capture selections
  selected <<- input$checkcomp

  if(length(selected) > 0){
    for(i in 1:length(selected)){
      
      p <- add_trace(p, y=eval(parse(text=selected[i])), x=years, mode="markers",
                     text=paste("Deficit: ", dollar(eval(parse(text=selected[i])))[14:25]),
                     marker=list(size=6*(eval(parse(text=selected[i])))[26:37], 
                                 color=ifelse(eval(parse(text=selected[i]))[13] > 7.34, 'red', 
                                          ifelse(eval(parse(text=selected[i]))[13]> 6.27, 'darkorange', 
                                             ifelse(eval(parse(text=selected[i]))[13] > 5.20, 'gold', 'green'))),
                                 opacity=ifelse(eval(parse(text=selected[i])) > 0, 0.5, 0.8)),
                     evaluate=TRUE, name=selected[i])
    }
  }
  
  layout(p, title = "Compare ISS Funding Deltas by Fiscal Years",
         yaxis=list(title="Funding Delta/Requirement Ratios"), 
         xaxis=list(title="Year", autorange=T, autotick=T, tickangle=0),
         legend=list(bordercolor="#FFFFFF"))
  
})

output$riskfilters <- renderUI({
  choice1 <- list("High Impact"="high", "Moderately High Impact"="modhigh",
                  "Moderate Impact"="mod", "Low Impact"="low")
  choice2 <- list("IMCOM", "NETCOM", "AMC")
  choice3 <- list("Custom")
  
  ifelse(input$riskradio1 == 2, choice <- choice1,
         ifelse(input$riskradio1 == 1, choice <- choice2, choice <- choice3))
  selectInput('riskchoice2', 'Make Selection', choice)
})

output$riskcheckbox <- renderUI({
  
  data <- switch(input$riskchoice2, 
                 "IMCOM" = imcom, "NETCOM" = netcom, "AMC" = amc, "Custom" = custom,
                 "high" = high, "modhigh" = modhigh, "mod" = mod, "low" = low,)
  
  checkboxGroupInput('checkcomp', 'Select Installation Service(s)', data)
})

#############################################################################################
###############################FINANCIAL DEFICITS PAGE#######################################
#############################################################################################

#INDIVIDUAL - FINANCIAL DEFICITS
output$ISSPlotF <- renderPlotly({
  
    dataF <- switch(input$selectF, 
                    "ISS100" = isslevels$ISS100[3:14],
                    "ISS102" = isslevels$ISS102[3:14],
                    "ISS103" = isslevels$ISS103[3:14],
                    "ISS104" = isslevels$ISS104[3:14],
                    "ISS105" = isslevels$ISS105[3:14],
                    "ISS106" = isslevels$ISS106[3:14],
                    "ISS107" = isslevels$ISS107[3:14],
                    "ISS109" = isslevels$ISS109[3:14],
                    "ISS111" = isslevels$ISS111[3:14],
                    "ISS112" = isslevels$ISS112[3:14],
                    "ISS115" = isslevels$ISS115[3:14],
                    "ISS116" = isslevels$ISS116[3:14],
                    "ISS118" = isslevels$ISS118[3:14],
                    "ISS121" = isslevels$ISS121[3:14],
                    "ISS113" = isslevels$ISS113[3:14],
                    "ISS250" = isslevels$ISS250[3:14],
                    "ISS800" = isslevels$ISS800[3:14],
                    "ISS803" = isslevels$ISS803[3:14],
                    "ISS202" = isslevels$ISS202[3:14],
                    "ISS251" = isslevels$ISS251[3:14],
                    "ISS252" = isslevels$ISS252[3:14],
                    "ISS253" = isslevels$ISS253[3:14],
                    "ISS254" = isslevels$ISS254[3:14],
                    "ISS201" = isslevels$ISS201[3:14],
                    "ISS414" = isslevels$ISS414[3:14],
                    "ISS411" = isslevels$ISS411[3:14],
                    "ISS420" = isslevels$ISS420[3:14],
                    
                    "ISS200" = isslevels$ISS200[3:14],
                    "ISS402" = isslevels$ISS402[3:14],
                    "ISS403" = isslevels$ISS403[3:14],
                    "ISS404" = isslevels$ISS404[3:14],
                    "ISS405" = isslevels$ISS405[3:14],
                    "ISS406" = isslevels$ISS406[3:14],
                    "ISS408" = isslevels$ISS408[3:14],
                    "ISS500" = isslevels$ISS500[3:14],
                    "ISS501" = isslevels$ISS501[3:14],
                    "ISS502" = isslevels$ISS502[3:14],
                    "ISS503" = isslevels$ISS503[3:14],
                    "ISS504" = isslevels$ISS504[3:14],
                    "ISS505" = isslevels$ISS505[3:14],
                    "ISS506" = isslevels$ISS506[3:14],
                    "ISS507" = isslevels$ISS507[3:14],
                    "ISS510" = isslevels$ISS510[3:14],
                    "ISS401" = isslevels$ISS401[3:14],
                    "ISS600" = isslevels$ISS600[3:14],
                    "ISS601" = isslevels$ISS601[3:14],
                    "ISS602" = isslevels$ISS602[3:14],
                    "ISS603" = isslevels$ISS603[3:14],
                    "ISS604" = isslevels$ISS604[3:14],
                    "ISS702" = isslevels$ISS702[3:14],
                    "ISS900" = isslevels$ISS900[3:14],
                    "ISS901" = isslevels$ISS901[3:14],
                    "ISS902" = isslevels$ISS902[3:14],
                    "ISS903" = isslevels$ISS903[3:14],
                    "ISS904" = isslevels$ISS904[3:14],
                    "ISS905" = isslevels$ISS905[3:14],
                    "ISS906" = isslevels$ISS906[3:14],
                    "ISS701" = isslevels$ISS701[3:14],
                    "ISS703" = isslevels$ISS703[3:14],
                    "ISS700" = isslevels$ISS700[3:14],
                    "ISS300" = isslevels$ISS300[3:14],
                    "ISS301" = isslevels$ISS301[3:14],
                    "ISS302" = isslevels$ISS302[3:14],
                    "ISS303" = isslevels$ISS303[3:14],
                    "ISS304" = isslevels$ISS304[3:14],
                    "ISS305" = isslevels$ISS305[3:14],
                    "ISS306" = isslevels$ISS306[3:14],
                    "ISS307" = isslevels$ISS307[3:14],
                    "ISS308" = isslevels$ISS308[3:14],
                    "ISS309" = isslevels$ISS309[3:14],)
    
    pF <- plot_ly(isslevels, x=isslevels$Button[3:14],y=dataF,
                 name=input$selectF,type="bar",marker=list(color = toRGB(c
                                                                         ("cyan3", "cyan3", "cyan3",
                                                                          "cyan3", "cyan3", "cyan3", "cyan3", 
                                                                          "violetred3", "violetred3", "violetred3", 
                                                                          "violetred3", "violetred3"))))
    
    def <- switch(input$minifinyears, 
                   "Y2010" = dataF[1], "Y2011" = dataF[2],
                   "Y2012" = dataF[3], "Y2013" = dataF[4],
                   "Y2014" = dataF[5], "Y2015" = dataF[6],
                   "Y2016" = dataF[7], "Y2017" = dataF[8], 
                   "Y2018" = dataF[9], "Y2019" = dataF[10], 
                   "Y2020" = dataF[11], "Y2021" = dataF[12],)
    
    year <- switch(input$minifinyears, 
                  "Y2010" = isslevels$Button[3], "Y2011" = isslevels$Button[4],
                  "Y2012" = isslevels$Button[5], "Y2013" = isslevels$Button[6],
                  "Y2014" = isslevels$Button[7], "Y2015" = isslevels$Button[8],
                  "Y2016" = isslevels$Button[9], "Y2017" = isslevels$Button[10], 
                  "Y2018" = isslevels$Button[11], "Y2019" = isslevels$Button[12], 
                  "Y2020" = isslevels$Button[13], "Y2021" = isslevels$Button[14],)
    
    point <- list(x = year, y = def,
                  text = paste(year), xref = "x", yref = "y",
                  showarrow = TRUE, arrowhead = 3, ax = 0, ay = -50)
    
    layout(pF, title=paste('FY10-FY21 Annual Funding Deltas for', input$selectF),
           yaxis=list(title="Deficit in $"), xaxis=list(title="Year", autorange=T, autotick=T),
           annotations=point)
  })
  
  output$Def16Box <- renderValueBox({
    dataF2 <- switch(input$selectF, 
                     "ISS100" = isslevels$ISS100[15],
                     "ISS102" = isslevels$ISS102[15],
                     "ISS103" = isslevels$ISS103[15],
                     "ISS104" = isslevels$ISS104[15],
                     "ISS105" = isslevels$ISS105[15],
                     "ISS106" = isslevels$ISS106[15],
                     "ISS107" = isslevels$ISS107[15],
                     "ISS109" = isslevels$ISS109[15],
                     "ISS111" = isslevels$ISS111[15],
                     "ISS112" = isslevels$ISS112[15],
                     "ISS115" = isslevels$ISS115[15],
                     "ISS116" = isslevels$ISS116[15],
                     "ISS118" = isslevels$ISS118[15],
                     "ISS121" = isslevels$ISS121[15],
                     "ISS113" = isslevels$ISS113[15],
                     "ISS250" = isslevels$ISS250[15],
                     "ISS800" = isslevels$ISS800[15],
                     "ISS803" = isslevels$ISS803[15],
                     "ISS202" = isslevels$ISS202[15],
                     "ISS251" = isslevels$ISS251[15],
                     "ISS252" = isslevels$ISS252[15],
                     "ISS253" = isslevels$ISS253[15],
                     "ISS254" = isslevels$ISS254[15],
                     "ISS201" = isslevels$ISS201[15],
                     "ISS414" = isslevels$ISS414[15],
                     "ISS411" = isslevels$ISS411[15],
                     "ISS420" = isslevels$ISS420[15],
                     
                     "ISS200" = isslevels$ISS200[15],
                     "ISS402" = isslevels$ISS402[15],
                     "ISS403" = isslevels$ISS403[15],
                     "ISS404" = isslevels$ISS404[15],
                     "ISS405" = isslevels$ISS405[15],
                     "ISS406" = isslevels$ISS406[15],
                     "ISS408" = isslevels$ISS408[15],
                     "ISS500" = isslevels$ISS500[15],
                     "ISS501" = isslevels$ISS501[15],
                     "ISS502" = isslevels$ISS502[15],
                     "ISS503" = isslevels$ISS503[15],
                     "ISS504" = isslevels$ISS504[15],
                     "ISS505" = isslevels$ISS505[15],
                     "ISS506" = isslevels$ISS506[15],
                     "ISS507" = isslevels$ISS507[15],
                     "ISS510" = isslevels$ISS510[15],
                     "ISS401" = isslevels$ISS401[15],
                     "ISS600" = isslevels$ISS600[15],
                     "ISS601" = isslevels$ISS601[15],
                     "ISS602" = isslevels$ISS602[15],
                     "ISS603" = isslevels$ISS603[15],
                     "ISS604" = isslevels$ISS604[15],
                     "ISS702" = isslevels$ISS702[15],
                     "ISS900" = isslevels$ISS900[15],
                     "ISS901" = isslevels$ISS901[15],
                     "ISS902" = isslevels$ISS902[15],
                     "ISS903" = isslevels$ISS903[15],
                     "ISS904" = isslevels$ISS904[15],
                     "ISS905" = isslevels$ISS905[15],
                     "ISS906" = isslevels$ISS906[15],
                     "ISS701" = isslevels$ISS701[15],
                     "ISS703" = isslevels$ISS703[15],
                     "ISS700" = isslevels$ISS700[15],
                     "ISS300" = isslevels$ISS300[15],
                     "ISS301" = isslevels$ISS301[15],
                     "ISS302" = isslevels$ISS302[15],
                     "ISS303" = isslevels$ISS303[15],
                     "ISS304" = isslevels$ISS304[15],
                     "ISS305" = isslevels$ISS305[15],
                     "ISS306" = isslevels$ISS306[15],
                     "ISS307" = isslevels$ISS307[15],
                     "ISS308" = isslevels$ISS308[15],
                     "ISS309" = isslevels$ISS309[15],)
    
    check2 <- ifelse(dataF2 < 0, "Cumulative Delta: Under-Funded (FY10-FY16)", 
                    ifelse(dataF2 > 0, "Cumulative Delta: Over-Funded (FY10-FY16)", 
                           "Cumulative Delta: Funding Met (FY10-FY16)")) 
    
    valueBox(
      paste(dollar(dataF2)), check2, icon = icon("fa fa-usd"),
      color = "teal"
    )
  })
  
  output$Pred21Box <- renderValueBox({
    dataF3 <- switch(input$selectF, 
                     "ISS100" = isslevels$ISS100[16],
                     "ISS102" = isslevels$ISS102[16],
                     "ISS103" = isslevels$ISS103[16],
                     "ISS104" = isslevels$ISS104[16],
                     "ISS105" = isslevels$ISS105[16],
                     "ISS106" = isslevels$ISS106[16],
                     "ISS107" = isslevels$ISS107[16],
                     "ISS109" = isslevels$ISS109[16],
                     "ISS111" = isslevels$ISS111[16],
                     "ISS112" = isslevels$ISS112[16],
                     "ISS115" = isslevels$ISS115[16],
                     "ISS116" = isslevels$ISS116[16],
                     "ISS118" = isslevels$ISS118[16],
                     "ISS121" = isslevels$ISS121[16],
                     "ISS113" = isslevels$ISS113[16],
                     "ISS250" = isslevels$ISS250[16],
                     "ISS800" = isslevels$ISS800[16],
                     "ISS803" = isslevels$ISS803[16],
                     "ISS202" = isslevels$ISS202[16],
                     "ISS251" = isslevels$ISS251[16],
                     "ISS252" = isslevels$ISS252[16],
                     "ISS253" = isslevels$ISS253[16],
                     "ISS254" = isslevels$ISS254[16],
                     "ISS201" = isslevels$ISS201[16],
                     "ISS414" = isslevels$ISS414[16],
                     "ISS411" = isslevels$ISS411[16],
                     "ISS420" = isslevels$ISS420[16],
                     
                     "ISS200" = isslevels$ISS200[16],
                     "ISS402" = isslevels$ISS402[16],
                     "ISS403" = isslevels$ISS403[16],
                     "ISS404" = isslevels$ISS404[16],
                     "ISS405" = isslevels$ISS405[16],
                     "ISS406" = isslevels$ISS406[16],
                     "ISS408" = isslevels$ISS408[16],
                     "ISS500" = isslevels$ISS500[16],
                     "ISS501" = isslevels$ISS501[16],
                     "ISS502" = isslevels$ISS502[16],
                     "ISS503" = isslevels$ISS503[16],
                     "ISS504" = isslevels$ISS504[16],
                     "ISS505" = isslevels$ISS505[16],
                     "ISS506" = isslevels$ISS506[16],
                     "ISS507" = isslevels$ISS507[16],
                     "ISS510" = isslevels$ISS510[16],
                     "ISS401" = isslevels$ISS401[16],
                     "ISS600" = isslevels$ISS600[16],
                     "ISS601" = isslevels$ISS601[16],
                     "ISS602" = isslevels$ISS602[16],
                     "ISS603" = isslevels$ISS603[16],
                     "ISS604" = isslevels$ISS604[16],
                     "ISS702" = isslevels$ISS702[16],
                     "ISS900" = isslevels$ISS900[16],
                     "ISS901" = isslevels$ISS901[16],
                     "ISS902" = isslevels$ISS902[16],
                     "ISS903" = isslevels$ISS903[16],
                     "ISS904" = isslevels$ISS904[16],
                     "ISS905" = isslevels$ISS905[16],
                     "ISS906" = isslevels$ISS906[16],
                     "ISS701" = isslevels$ISS701[16],
                     "ISS703" = isslevels$ISS703[16],
                     "ISS700" = isslevels$ISS700[16],
                     "ISS300" = isslevels$ISS300[16],
                     "ISS301" = isslevels$ISS301[16],
                     "ISS302" = isslevels$ISS302[16],
                     "ISS303" = isslevels$ISS303[16],
                     "ISS304" = isslevels$ISS304[16],
                     "ISS305" = isslevels$ISS305[16],
                     "ISS306" = isslevels$ISS306[16],
                     "ISS307" = isslevels$ISS307[16],
                     "ISS308" = isslevels$ISS308[16],
                     "ISS309" = isslevels$ISS309[16],)
    
    check <- ifelse(dataF3 < 0, "Predicted Cumulative Delta: Under-Funded (FY17-F21)", 
                    ifelse(dataF3 > 0, "Predicted Cumulative Delta: Over-Funded (FY17-F21)", 
                           "Predicted Cumulative Delta: Funding Met (FY17-FY21)")) 
    
    valueBox(
      paste(dollar(dataF3)), check, icon = icon("fa fa-usd"),
      color = "maroon"
    )
  })


output$minifins <- renderPlotly({

  funds <- switch(input$minifinyears, 
                   "Y2010" = triplefins$F10, "Y2011" = triplefins$F11,
                   "Y2012" = triplefins$F12, "Y2013" = triplefins$F13,
                   "Y2014" = triplefins$F14, "Y2015" = triplefins$F15,
                   "Y2016" = triplefins$F16, "Y2017" = triplefins$F17, 
                   "Y2018" = triplefins$F18, "Y2019" = triplefins$F19, 
                   "Y2020" = triplefins$F20, "Y2021" = triplefins$F21,)
  
  reqs <- switch(input$minifinyears, 
                  "Y2010" = triplefins$R10, "Y2011" = triplefins$R11,
                  "Y2012" = triplefins$R12, "Y2013" = triplefins$R13,
                  "Y2014" = triplefins$R14, "Y2015" = triplefins$R15,
                  "Y2016" = triplefins$R16, "Y2017" = triplefins$R17, 
                  "Y2018" = triplefins$R18, "Y2019" = triplefins$R19, 
                  "Y2020" = triplefins$R20, "Y2021" = triplefins$R21,)
  
  defs <- switch(input$minifinyears, 
                 "Y2010" = triplefins$D10, "Y2011" = triplefins$D11,
                 "Y2012" = triplefins$D12, "Y2013" = triplefins$D13,
                 "Y2014" = triplefins$D14, "Y2015" = triplefins$D15,
                 "Y2016" = triplefins$D16, "Y2017" = triplefins$D17, 
                 "Y2018" = triplefins$D18, "Y2019" = triplefins$D19, 
                 "Y2020" = triplefins$D20, "Y2021" = triplefins$D21,)
  
  service <- switch(input$selectF, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)

  p <- plot_ly(triplefins, x=1, y=dollar(funds[service]),
                name='Funding', type="bar",marker=list(color = toRGB("seagreen")))
  
  p <- add_trace(p, x=2, y=dollar(reqs[service]), 
                 name='Requirement', type='bar', marker=list(color=toRGB("tomato")))
  
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  colorchoice <- ifelse(input$minifinyears %in% future == TRUE, 'violetred3', 'cyan3')
  
  check <- ifelse(defs[service] < 0, "Under-funded by", 
                  ifelse(defs[service] > 0, "Over-funded by", "Funding Met:"))    
  
  
  p <- add_trace(p, x=3, y=dollar(defs[service]), 
                 name='Delta', type='bar', marker=list(color=toRGB(c(colorchoice))))
  
  defpoint <- list(x = 3, 
                   y = defs[service],
                   text = paste(check, '</br>', dollar(defs[service])), xref = "x", yref = "y",
                   showarrow = T, arrowhead = 3, ax = 20, ay = -40)
  
  layout(p, title=paste0(input$minifinyears, ' Financial Information for ', input$selectF),
         yaxis=list(title="Dollars"), xaxis=list(title=triplefins$Name[service], tickmode="array", 
                                                 tickvals=c(1, 2, 3), 
                                                 ticktext=c("Funding", "Requirement", "Delta"), 
                                                 tickfont=list(size=12), tickangle=0),
         annotations=defpoint, showlegend=F)
})

  output$yearlydef <- renderValueBox({

  defs <- switch(input$minifinyears, 
                    "Y2010" = triplefins$D10Rank, "Y2011" = triplefins$D11Rank,
                    "Y2012" = triplefins$D12Rank, "Y2013" = triplefins$D13Rank,
                    "Y2014" = triplefins$D14Rank, "Y2015" = triplefins$D15Rank,
                    "Y2016" = triplefins$D16Rank, "Y2017" = triplefins$D17Rank, 
                    "Y2018" = triplefins$D18Rank, "Y2019" = triplefins$D19Rank, 
                    "Y2020" = triplefins$D20Rank, "Y2021" = triplefins$D21Rank,)
  
  service <- switch(input$selectF, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)

  valueBox(paste(ordinal(defs[service])), paste("Largest Deficit out of 71 Services for ", input$minifinyears), 
           icon = icon("fa fa-exclamation-triangle"),color = "teal")
  })

  output$yearlyfund <- renderValueBox({
    
    funds <- switch(input$minifinyears, 
                    "Y2010" = triplefins$F10Rank, "Y2011" = triplefins$F11Rank,
                    "Y2012" = triplefins$F12Rank, "Y2013" = triplefins$F13Rank,
                    "Y2014" = triplefins$F14Rank, "Y2015" = triplefins$F15Rank,
                    "Y2016" = triplefins$F16Rank, "Y2017" = triplefins$F17Rank, 
                    "Y2018" = triplefins$F18Rank, "Y2019" = triplefins$F19Rank, 
                    "Y2020" = triplefins$F20Rank, "Y2021" = triplefins$F21Rank,)
    
    service <- switch(input$selectF, 
                       "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                       "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                       "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                       "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                       "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                       "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                       "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                       "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                       "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                       "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                       "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                       "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                       "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                       "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                       "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                       "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                       "ISS307"=69, "ISS308"=70, "ISS309"=71)

    valueBox(paste(ordinal(funds[service])), paste("Largest Funding out of 71 Services for ", input$minifinyears), 
             icon = icon("fa fa-dollar"),color = "olive")
  })

  output$yearlyreq <- renderValueBox({
    
    reqs <- switch(input$minifinyears, 
                     "Y2010" = triplefins$R10Rank, "Y2011" = triplefins$R11Rank,
                     "Y2012" = triplefins$R12Rank, "Y2013" = triplefins$R13Rank,
                     "Y2014" = triplefins$R14Rank, "Y2015" = triplefins$R15Rank,
                     "Y2016" = triplefins$R16Rank, "Y2017" = triplefins$R17Rank, 
                     "Y2018" = triplefins$R18Rank, "Y2019" = triplefins$R19Rank, 
                     "Y2020" = triplefins$R20Rank, "Y2021" = triplefins$R21Rank,)
    
    service <- switch(input$selectF, 
                       "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                       "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                       "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                       "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                       "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                       "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                       "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                       "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                       "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                       "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                       "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                       "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                       "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                       "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                       "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                       "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                       "ISS307"=69, "ISS308"=70, "ISS309"=71)
    
    valueBox(paste(ordinal(reqs[service])), paste("Largest Requirement out of 71 Services for ", input$minifinyears), 
             icon = icon("fa fa-dollar"),color = "red")
  })

#########################################################################  
output$frdfunds <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  ordfunds <- switch(input$frdyear, 
                  "Y2010" = triplefins[order(-triplefins$F10),], "Y2011" = triplefins[order(-triplefins$F11),],
                  "Y2012" = triplefins[order(-triplefins$F12),], "Y2013" = triplefins[order(-triplefins$F13),],
                  "Y2014" = triplefins[order(-triplefins$F14),], "Y2015" = triplefins[order(-triplefins$F15),],
                  "Y2016" = triplefins[order(-triplefins$F16),], "Y2017" = triplefins[order(-triplefins$F17),], 
                  "Y2018" = triplefins[order(-triplefins$F18),], "Y2019" = triplefins[order(-triplefins$F19),], 
                  "Y2020" = triplefins[order(-triplefins$F20),], "Y2021" = triplefins[order(-triplefins$F21),],)
  
  col <- switch(input$frdyear, 
                     "Y2010" = ordfunds$F10, "Y2011" = ordfunds$F11,
                     "Y2012" = ordfunds$F12, "Y2013" = ordfunds$F13,
                     "Y2014" = ordfunds$F14, "Y2015" = ordfunds$F15,
                     "Y2016" = ordfunds$F16, "Y2017" = ordfunds$F17,
                     "Y2018" = ordfunds$F18, "Y2019" = ordfunds$F19,
                     "Y2020" = ordfunds$F20, "Y2021" = ordfunds$F21,)

  p <- plot_ly(ordfunds, x=Name, y=col,
               name=ISS, type="bar",marker=list(color = toRGB("seagreen")))
  
  current <- ordfunds$Name[which(grepl(input$frdservice, ordfunds$ISS))]
  
  point2 <- list(x = current, 
                   y = col[which(grepl(input$frdservice, ordfunds$ISS))],
                   text = paste(ordfunds$ISS[which(grepl(input$frdservice, ordfunds$ISS))]), 
                   xref = "x", yref = "y",
                   showarrow = T, arrowhead = 3, ax = 20, ay = -40)
  
  layout(p, title=paste0(input$frdyear, ' Funding Received for Each ISS'),
         yaxis=list(title="Dollars"), 
     xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point2,
         showlegend=F)
})

output$frdreqs <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  ordreqs <- switch(input$frdyear, 
                     "Y2010" = triplefins[order(-triplefins$R10),], "Y2011" = triplefins[order(-triplefins$R11),],
                     "Y2012" = triplefins[order(-triplefins$R12),], "Y2013" = triplefins[order(-triplefins$R13),],
                     "Y2014" = triplefins[order(-triplefins$R14),], "Y2015" = triplefins[order(-triplefins$R15),],
                     "Y2016" = triplefins[order(-triplefins$R16),], "Y2017" = triplefins[order(-triplefins$R17),], 
                     "Y2018" = triplefins[order(-triplefins$R18),], "Y2019" = triplefins[order(-triplefins$R19),], 
                     "Y2020" = triplefins[order(-triplefins$R20),], "Y2021" = triplefins[order(-triplefins$R21),],)
  
  col <- switch(input$frdyear, 
                "Y2010" = ordreqs$R10, "Y2011" = ordreqs$R11,
                "Y2012" = ordreqs$R12, "Y2013" = ordreqs$R13,
                "Y2014" = ordreqs$R14, "Y2015" = ordreqs$R15,
                "Y2016" = ordreqs$R16, "Y2017" = ordreqs$R17,
                "Y2018" = ordreqs$R18, "Y2019" = ordreqs$R19,
                "Y2020" = ordreqs$R20, "Y2021" = ordreqs$R21,)
  p <- plot_ly(ordreqs, x=Name, y=col,
               name=ISS, type="bar",marker=list(color = toRGB("tomato")))
  
  current <- ordreqs$Name[which(grepl(input$frdservice, ordreqs$ISS))]
  
  point3 <- list(x = current, 
                 y = col[which(grepl(input$frdservice, ordreqs$ISS))],
                 text = paste(ordreqs$ISS[which(grepl(input$frdservice, ordreqs$ISS))]), 
                 xref = "x", yref = "y",
                 showarrow = T, arrowhead = 3, ax = 20, ay = -40)
  
  layout(p, title=paste0(input$frdyear, ' Requirements for Each ISS'),
         yaxis=list(title="Dollars"), 
         xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point3,
         showlegend=F)
})

output$frddefs <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  orddefs <- switch(input$frdyear, 
                    "Y2010" = triplefins[order(triplefins$D10),], "Y2011" = triplefins[order(triplefins$D11),],
                    "Y2012" = triplefins[order(triplefins$D12),], "Y2013" = triplefins[order(triplefins$D13),],
                    "Y2014" = triplefins[order(triplefins$D14),], "Y2015" = triplefins[order(triplefins$D15),],
                    "Y2016" = triplefins[order(triplefins$D16),], "Y2017" = triplefins[order(triplefins$D17),], 
                    "Y2018" = triplefins[order(triplefins$D18),], "Y2019" = triplefins[order(triplefins$D19),], 
                    "Y2020" = triplefins[order(triplefins$D20),], "Y2021" = triplefins[order(triplefins$D21),],)
  
  col <- switch(input$frdyear, 
                "Y2010" = orddefs$D10, "Y2011" = orddefs$D11,
                "Y2012" = orddefs$D12, "Y2013" = orddefs$D13,
                "Y2014" = orddefs$D14, "Y2015" = orddefs$D15,
                "Y2016" = orddefs$D16, "Y2017" = orddefs$D17,
                "Y2018" = orddefs$D18, "Y2019" = orddefs$D19,
                "Y2020" = orddefs$D20, "Y2021" = orddefs$D21,)
  
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  colorchoice <- ifelse(input$frdyear %in% future == TRUE, 'violetred3', 'cyan3')
  
  p <- plot_ly(orddefs, x=Name, y=col,
               name=ISS, type="bar",marker=list(color = toRGB(colorchoice)))
    
  current <- orddefs$Name[which(grepl(input$frdservice, orddefs$ISS))]
  
  point4 <- list(x = current, 
                 y = col[which(grepl(input$frdservice, orddefs$ISS))],
                 text = paste(orddefs$ISS[which(grepl(input$frdservice, orddefs$ISS))]), 
                 xref = "x", yref = "y",
                 showarrow = T, arrowhead = 3, ax = 20, ay = -40)
  
  layout(p, title=paste0(input$frdyear, ' Funding Deltas for Each ISS'),
         yaxis=list(title="Dollars"), 
         xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point4,
         showlegend=F)
})

output$minifrdfunds <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  service <- switch(input$frdservice, 
                 "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                 "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                 "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                 "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                 "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                 "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                 "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                 "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                 "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                 "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                 "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                 "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                 "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                 "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                 "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                 "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                 "ISS307"=69, "ISS308"=70, "ISS309"=71)
  
  ydata <-  c(triplefins$F10[service],triplefins$F11[service],triplefins$F12[service], 
                    triplefins$F13[service], triplefins$F14[service], triplefins$F15[service], 
                    triplefins$F16[service], triplefins$F17[service],triplefins$F18[service], 
                    triplefins$F19[service], triplefins$F20[service], triplefins$F21[service])

  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  p <- plot_ly(triplefins, x=years, y=ydata,
              name=triplefins$ISS[service], type="bar",
               marker=list(color = toRGB("seagreen")))
  
  chooseyear <- switch(input$frdyear, 
                       "Y2010" = 1, "Y2011" = 2,
                       "Y2012" = 3, "Y2013" = 4,
                       "Y2014" = 5, "Y2015" = 6,
                       "Y2016" = 7, "Y2017" = 8, 
                       "Y2018" = 9, "Y2019" = 10, 
                       "Y2020" = 11, "Y2021" = 12,)

  
  #point <- list(x = years[chooseyear], y = ydata[chooseyear],
         #       xref = "x", yref = "y",
          #      showarrow = TRUE, arrowhead = 3, ax = 0, ay = -30)
  
  layout(p, title=paste0('Funding Received for ', input$frdservice),
         yaxis=list(title="Dollars", titlefont=list(size=12)), 
         xaxis=list(title=paste0(triplefins$Name[service]),
                    tickmode="array", 
                    ticktext=years, 
                    tickfont=list(size=10), tickangle=0, titlefont=list(size=12))) #annotations=point)
})

output$minifrdreqs <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  service <- switch(input$frdservice, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)
  
  ydata <-  c(triplefins$R10[service],triplefins$R11[service],triplefins$R12[service], 
              triplefins$R13[service], triplefins$R14[service], triplefins$R15[service], 
              triplefins$R16[service], triplefins$R17[service],triplefins$R18[service], 
              triplefins$R19[service], triplefins$R20[service], triplefins$R21[service])
  
  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  p <- plot_ly(triplefins, x=years, y=ydata,
               name=triplefins$ISS[service], type="bar",
               marker=list(color = toRGB("tomato")))
  
  chooseyear <- switch(input$frdyear, 
                       "Y2010" = 1, "Y2011" = 2,
                       "Y2012" = 3, "Y2013" = 4,
                       "Y2014" = 5, "Y2015" = 6,
                       "Y2016" = 7, "Y2017" = 8, 
                       "Y2018" = 9, "Y2019" = 10, 
                       "Y2020" = 11, "Y2021" = 12,)
  
 # point <- list(x = years[chooseyear], y = ydata[chooseyear],
   #             xref = "x", yref = "y",
    #            showarrow = TRUE, arrowhead = 3, ax = 0, ay = -30)
  
  layout(p, title=paste0('Annual Requirement for ', input$frdservice),
         yaxis=list(title="Dollars", titlefont=list(size=12)), 
         xaxis=list(title=paste0(triplefins$Name[service]),
                    tickmode="array", 
                    ticktext=years, titlefont=list(size=12), 
                    tickfont=list(size=10), tickangle=0)) #annotations=point)
})

output$minifrddefs <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  service <- switch(input$frdservice, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)
  
  ydata <-  c(triplefins$D10[service],triplefins$D11[service],triplefins$D12[service], 
              triplefins$D13[service], triplefins$D14[service], triplefins$D15[service], 
              triplefins$D16[service], triplefins$D17[service],triplefins$D18[service], 
              triplefins$D19[service], triplefins$D20[service], triplefins$D21[service])
  
  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  p <- plot_ly(triplefins, x=years, y=ydata,
               name=triplefins$ISS[service], type="bar",
               marker=list(color = toRGB(c("cyan3", "cyan3", "cyan3",
                                          "cyan3", "cyan3", "cyan3", "cyan3", 
                                          "violetred3", "violetred3", "violetred3", 
                                          "violetred3", "violetred3"))))
  
  
  chooseyear <- switch(input$frdyear, 
                       "Y2010" = 1, "Y2011" = 2,
                       "Y2012" = 3, "Y2013" = 4,
                       "Y2014" = 5, "Y2015" = 6,
                       "Y2016" = 7, "Y2017" = 8, 
                       "Y2018" = 9, "Y2019" = 10, 
                       "Y2020" = 11, "Y2021" = 12,)
  
  ifelse(ydata[chooseyear] > 0, check <- -30, check <- 30)
  defcheck <- ifelse(ydata[chooseyear] > 0, "Over-Funded", 
         ifelse(ydata[chooseyear] < 0, "Under-Funded", "Met"))
  
  #point <- list(x = years[chooseyear], y = ydata[chooseyear],
         #       xref = "x", yref = "y", text=paste0(defcheck), 
           #     showarrow = TRUE, arrowhead = 3, ax = 0, ay = check)
  
  layout(p, title=paste0('Annual Funding Delta for ', input$frdservice),
         yaxis=list(title="Dollars", titlefont=list(size=12)), 
         xaxis=list(title=paste0(triplefins$Name[service]),
                    tickmode="array", 
                    ticktext=years, titlefont=list(size=12), 
                    tickfont=list(size=10), tickangle=0)) #annotations=point)
})



###################################


output$minifrdreqs2 <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  service <- switch(input$selectF, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)
  
  ydata <-  c(triplefins$R10[service],triplefins$R11[service],triplefins$R12[service], 
              triplefins$R13[service], triplefins$R14[service], triplefins$R15[service], 
              triplefins$R16[service], triplefins$R17[service],triplefins$R18[service], 
              triplefins$R19[service], triplefins$R20[service], triplefins$R21[service])
  
  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  p <- plot_ly(triplefins, x=years, y=ydata,
               name=triplefins$ISS[service], type="bar",
               marker=list(color = toRGB("tomato")))
  
  chooseyear <- switch(input$minifinyears, 
                       "Y2010" = 1, "Y2011" = 2,
                       "Y2012" = 3, "Y2013" = 4,
                       "Y2014" = 5, "Y2015" = 6,
                       "Y2016" = 7, "Y2017" = 8, 
                       "Y2018" = 9, "Y2019" = 10, 
                       "Y2020" = 11, "Y2021" = 12,)
  
#  point <- list(x = years[chooseyear], y = ydata[chooseyear],
   #             xref = "x", yref = "y",
   #             showarrow = TRUE, arrowhead = 3, ax = 0, ay = -25)
  
  layout(p, title=paste0('Annual Requirement for ', input$selectF), titlefont=list(size=13),
         yaxis=list(title="Dollars", titlefont=list(size=11)), 
         xaxis=list(title=paste0(triplefins$Name[service]), titlefont=list(size=11),
                    tickmode="array", 
                    ticktext=years, 
                    tickfont=list(size=10), tickangle=0)) #annotations=point)
})

output$minifrdfunds2 <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  service <- switch(input$selectF, 
                    "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                    "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                    "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                    "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                    "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                    "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                    "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                    "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                    "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                    "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                    "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                    "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                    "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                    "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                    "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                    "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                    "ISS307"=69, "ISS308"=70, "ISS309"=71)
  chooseyear <- switch(input$minifinyears, 
                  "Y2010" = 1, "Y2011" = 2,
                  "Y2012" = 3, "Y2013" = 4,
                  "Y2014" = 5, "Y2015" = 6,
                  "Y2016" = 7, "Y2017" = 8, 
                  "Y2018" = 9, "Y2019" = 10, 
                  "Y2020" = 11, "Y2021" = 12,)
  
  ydata <-  c(triplefins$F10[service],triplefins$F11[service],triplefins$F12[service], 
              triplefins$F13[service], triplefins$F14[service], triplefins$F15[service], 
              triplefins$F16[service], triplefins$F17[service],triplefins$F18[service], 
              triplefins$F19[service], triplefins$F20[service], triplefins$F21[service])
  
  years <- c("`10", "`11", "`12", "`13", "`14", 
             "`15", "`16", "`17", "`18", "`19", "`20", "`21")
  
  p <- plot_ly(triplefins, x=years, y=ydata,
               name=triplefins$ISS[service], type="bar",
               marker=list(color = toRGB("seagreen")))
  
  #point <- list(x = years[chooseyear], y = ydata[chooseyear],
      #          xref = "x", yref = "y",
       #         showarrow = TRUE, arrowhead = 3, ax = 0, ay = -25)
  
  layout(p, title=paste0('Annual Funding Received for ', input$selectF), titlefont=list(size=13),
         yaxis=list(title="Dollars", titlefont=list(size=11)), 
         xaxis=list(title=paste0(triplefins$Name[service]), titlefont=list(size=11),
                    tickmode="array", 
                    ticktext=years, 
                    tickfont=list(size=10), tickangle=0)) #annotations=point)
})


  #############################################################################################
    #COMPARISON - FINANCIAL DEFICITS
  output$ISSLineF <- renderPlotly({
    
    #process data to only what we want
    df <- isslevels[c(17:28),]
    #add dummy column
    df$dummy <- NA
    
    #plot dummy column - this is so you will always have something to start with
    p <- plot_ly(df, type='line', y=dummy, x=Button, showlegend = FALSE)
    
    #####plot additional traces based on selections
    #capture selections
    selected <<- input$checkboxF
    
    #only plot new stuff if something is selected
    #add trace for each selection - note the eval(parse(...)) thing and evaluate=TRUE to get this to work
    #also, set legend entries for new series
    if(length(selected) > 0){
      for(i in 1:length(selected)){
        p <- add_trace(p, y=eval(parse(text=selected[i])), x=Button, evaluate=TRUE, name=selected[i])
      }
    }

    #only change to your original was to remove some weird border from the legend
    layout(p, title = "Cumulative ISS Deficit",
           yaxis=list(title="Deficit in $"), xaxis=list(title="Year", autorange=T, autotick=T),
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$ISSLineF2 <- renderPlotly({
    
    #process data to only what we want
    df <- isslevels[c(3:16),]
    #add dummy column
    df$dummy <- NA
    
    #plot dummy column - this is so you will always have something to start with
    p <- plot_ly(df, type='line', y=dummy, x=Button, showlegend = FALSE)
    
    #####plot additional traces based on selections
    #capture selections
    selected <<- input$checkboxF
    
    #only plot new stuff if something is selected
    #add trace for each selection - note the eval(parse(...)) thing and evaluate=TRUE to get this to work
    #also, set legend entries for new series
    if(length(selected) > 0){
      for(i in 1:length(selected)){
        p <- add_trace(p, y=eval(parse(text=selected[i])), x=Button, evaluate=TRUE, name=selected[i])
      }
    }
    
    #only change to your original was to remove some weird border from the legend
    layout(p, title = "Annual ISS Deficit",
           yaxis=list(title="Deficit in $"), xaxis=list(title="Year", autorange=T, autotick=T),
           legend=list(bordercolor="#FFFFFF"))
    
  })

output$fund1filters <- renderUI({
  choice1 <- list("High Impact"="high", "Moderately High Impact"="modhigh",
                      "Moderate Impact"="mod", "Low Impact"="low")
  choice2 <- list("IMCOM", "NETCOM", "AMC")
  choice3 <- list("Custom")
  
  ifelse(input$fundradio1 == 2, choice <- choice1,
         ifelse(input$fundradio1 == 1, choice <- choice2, choice <- choice3))
  selectInput('fundchoice2', 'Make Selection', choice)
})

output$fund2checkbox <- renderUI({

  data <- switch(input$fundchoice2, 
                 "IMCOM" = imcom, "NETCOM" = netcom, "AMC" = amc, "Custom" = custom,
                  "high" = high, "modhigh" = modhigh, "mod" = mod, "low" = low,)
  
  checkboxGroupInput('checkboxF', 'Select Installation Service(s)', data)
})


#############################################################################################
###############################Readiness Impact LEVELS PAGE#######################################
#############################################################################################

  #Individual - Readiness Impact Levels

      
  output$ISSPlot <- renderPlotly({
  
    data <- switch(input$selectC, 
                   "ISS100" = isshist$ISS100,
                   "ISS102" = isshist$ISS102,
                   "ISS103" = isshist$ISS103,
                   "ISS104" = isshist$ISS104,
                   "ISS105" = isshist$ISS105,
                   "ISS106" = isshist$ISS106,
                   "ISS107" = isshist$ISS107,
                   "ISS109" = isshist$ISS109,
                   "ISS111" = isshist$ISS111,
                   "ISS112" = isshist$ISS112,
                   "ISS115" = isshist$ISS115,
                   "ISS116" = isshist$ISS116,
                   "ISS118" = isshist$ISS118,
                   "ISS121" = isshist$ISS121,
                   "ISS113" = isshist$ISS113,
                   "ISS250" = isshist$ISS250,
                   "ISS800" = isshist$ISS800,
                   "ISS803" = isshist$ISS803,
                   "ISS202" = isshist$ISS202,
                   "ISS251" = isshist$ISS251,
                   "ISS252" = isshist$ISS252,
                   "ISS253" = isshist$ISS253,
                   "ISS254" = isshist$ISS254,
                   "ISS201" = isshist$ISS201,
                   "ISS414" = isshist$ISS414,
                   "ISS411" = isshist$ISS411,
                   "ISS420" = isshist$ISS420,
                   "ISS200" = isshist$ISS200,
                   "ISS402" = isshist$ISS402,
                   "ISS403" = isshist$ISS403,
                   "ISS404" = isshist$ISS404,
                   "ISS405" = isshist$ISS405,
                   "ISS406" = isshist$ISS406,
                   "ISS408" = isshist$ISS408,
                   "ISS500" = isshist$ISS500,
                   "ISS501" = isshist$ISS501,
                   "ISS502" = isshist$ISS502,
                   "ISS503" = isshist$ISS503,
                   "ISS504" = isshist$ISS504,
                   "ISS505" = isshist$ISS505,
                   "ISS506" = isshist$ISS506,
                   "ISS507" = isshist$ISS507,
                   "ISS510" = isshist$ISS510,
                   "ISS401" = isshist$ISS401,
                   "ISS600" = isshist$ISS600,
                   "ISS601" = isshist$ISS601,
                   "ISS602" = isshist$ISS602,
                   "ISS603" = isshist$ISS603,
                   "ISS604" = isshist$ISS604,
                   "ISS702" = isshist$ISS702,
                   "ISS900" = isshist$ISS900,
                   "ISS901" = isshist$ISS901,
                   "ISS902" = isshist$ISS902,
                   "ISS903" = isshist$ISS903,
                   "ISS904" = isshist$ISS904,
                   "ISS905" = isshist$ISS905,
                   "ISS906" = isshist$ISS906,
                   "ISS701" = isshist$ISS701,
                   "ISS703" = isshist$ISS703,
                   "ISS700" = isshist$ISS700,
                   "ISS300" = isshist$ISS300,
                   "ISS301" = isshist$ISS301,
                   "ISS302" = isshist$ISS302,
                   "ISS303" = isshist$ISS303,
                   "ISS304" = isshist$ISS304,
                   "ISS305" = isshist$ISS305,
                   "ISS306" = isshist$ISS306,
                   "ISS307" = isshist$ISS307,
                   "ISS308" = isshist$ISS308,
                   "ISS309" = isshist$ISS309,)
    
    
    minx<- min(data)
    maxx<- max(data)

    p <- plot_ly(isshist, x=as.character(isshist$Score), y=data,
                 name=input$selectC,type="bar",marker=list(color = toRGB(c("red", "red", "orangered", "orange", "gold", "greenyellow",
                                                                          "olivedrab3", "springgreen4", "springgreen4"))))
    
    
    
    layout(p, title=paste("Distribution of ISDI Questionnaire Responses"), titlefont=list(size=16),
           yaxis=list(title="Frequency of Responses"), xaxis=list(title="ISDI Questionnaire Responses", 
                         titlefont=list(size=12), autorange=T, 
                         autotick=T, tickangle=0, showticklabels=TRUE, tickfont=list(size=10)))
  })
  
  output$HighCritBox <- renderValueBox({
    data2 <- switch(input$selectC, 
                   "ISS100" = isslevels$ISS100[1],
                   "ISS102" = isslevels$ISS102[1],
                   "ISS103" = isslevels$ISS103[1],
                   "ISS104" = isslevels$ISS104[1],
                   "ISS105" = isslevels$ISS105[1],
                   "ISS106" = isslevels$ISS106[1],
                   "ISS107" = isslevels$ISS107[1],
                   "ISS109" = isslevels$ISS109[1],
                   "ISS111" = isslevels$ISS111[1],
                   "ISS112" = isslevels$ISS112[1],
                   "ISS115" = isslevels$ISS115[1],
                   "ISS116" = isslevels$ISS116[1],
                   "ISS118" = isslevels$ISS118[1],
                   "ISS121" = isslevels$ISS121[1],
                   "ISS113" = isslevels$ISS113[1],
                   "ISS250" = isslevels$ISS250[1],
                   "ISS800" = isslevels$ISS800[1],
                   "ISS803" = isslevels$ISS803[1],
                   "ISS202" = isslevels$ISS202[1],
                   "ISS251" = isslevels$ISS251[1],
                   "ISS252" = isslevels$ISS252[1],
                   "ISS253" = isslevels$ISS253[1],
                   "ISS254" = isslevels$ISS254[1],
                   "ISS201" = isslevels$ISS201[1],
                   "ISS414" = isslevels$ISS414[1],
                   "ISS411" = isslevels$ISS411[1],
                   "ISS420" = isslevels$ISS420[1],
                   "ISS200" = isslevels$ISS200[1],
                   "ISS402" = isslevels$ISS402[1],
                   "ISS403" = isslevels$ISS403[1],
                   "ISS404" = isslevels$ISS404[1],
                   "ISS405" = isslevels$ISS405[1],
                   "ISS406" = isslevels$ISS406[1],
                   "ISS408" = isslevels$ISS408[1],
                   "ISS500" = isslevels$ISS500[1],
                   "ISS501" = isslevels$ISS501[1],
                   "ISS502" = isslevels$ISS502[1],
                   "ISS503" = isslevels$ISS503[1],
                   "ISS504" = isslevels$ISS504[1],
                   "ISS505" = isslevels$ISS505[1],
                   "ISS506" = isslevels$ISS506[1],
                   "ISS507" = isslevels$ISS507[1],
                   "ISS510" = isslevels$ISS510[1],
                   "ISS401" = isslevels$ISS401[1],
                   "ISS600" = isslevels$ISS600[1],
                   "ISS601" = isslevels$ISS601[1],
                   "ISS602" = isslevels$ISS602[1],
                   "ISS603" = isslevels$ISS603[1],
                   "ISS604" = isslevels$ISS604[1],
                   "ISS702" = isslevels$ISS702[1],
                   "ISS900" = isslevels$ISS900[1],
                   "ISS901" = isslevels$ISS901[1],
                   "ISS902" = isslevels$ISS902[1],
                   "ISS903" = isslevels$ISS903[1],
                   "ISS904" = isslevels$ISS904[1],
                   "ISS905" = isslevels$ISS905[1],
                   "ISS906" = isslevels$ISS906[1],
                   "ISS701" = isslevels$ISS701[1],
                   "ISS703" = isslevels$ISS703[1],
                   "ISS700" = isslevels$ISS700[1],
                   "ISS300" = isslevels$ISS300[1],
                   "ISS301" = isslevels$ISS301[1],
                   "ISS302" = isslevels$ISS302[1],
                   "ISS303" = isslevels$ISS303[1],
                   "ISS304" = isslevels$ISS304[1],
                   "ISS305" = isslevels$ISS305[1],
                   "ISS306" = isslevels$ISS306[1],
                   "ISS307" = isslevels$ISS307[1],
                   "ISS308" = isslevels$ISS308[1],
                   "ISS309" = isslevels$ISS309[1],)
    valueBox(
      paste(percent(data2)), "Percent of Responses with High Impact Ratings", icon = icon("fa fa-arrow-up"),
      color = "red"
    )
  })
  output$LowCritBox <- renderValueBox({
    data3 <- switch(input$selectC, 
                    "ISS100" = isslevels$ISS100[2],
                    "ISS102" = isslevels$ISS102[2],
                    "ISS103" = isslevels$ISS103[2],
                    "ISS104" = isslevels$ISS104[2],
                    "ISS105" = isslevels$ISS105[2],
                    "ISS106" = isslevels$ISS106[2],
                    "ISS107" = isslevels$ISS107[2],
                    "ISS109" = isslevels$ISS109[2],
                    "ISS111" = isslevels$ISS111[2],
                    "ISS112" = isslevels$ISS112[2],
                    "ISS115" = isslevels$ISS115[2],
                    "ISS116" = isslevels$ISS116[2],
                    "ISS118" = isslevels$ISS118[2],
                    "ISS121" = isslevels$ISS121[2],
                    "ISS113" = isslevels$ISS113[2],
                    "ISS250" = isslevels$ISS250[2],
                    "ISS800" = isslevels$ISS800[2],
                    "ISS803" = isslevels$ISS803[2],
                    "ISS202" = isslevels$ISS202[2],
                    "ISS251" = isslevels$ISS251[2],
                    "ISS252" = isslevels$ISS252[2],
                    "ISS253" = isslevels$ISS253[2],
                    "ISS254" = isslevels$ISS254[2],
                    "ISS201" = isslevels$ISS201[2],
                    "ISS414" = isslevels$ISS414[2],
                    "ISS411" = isslevels$ISS411[2],
                    "ISS420" = isslevels$ISS420[2],
                    
                    "ISS200" = isslevels$ISS200[2],
                    "ISS402" = isslevels$ISS402[2],
                    "ISS403" = isslevels$ISS403[2],
                    "ISS404" = isslevels$ISS404[2],
                    "ISS405" = isslevels$ISS405[2],
                    "ISS406" = isslevels$ISS406[2],
                    "ISS408" = isslevels$ISS408[2],
                    "ISS500" = isslevels$ISS500[2],
                    "ISS501" = isslevels$ISS501[2],
                    "ISS502" = isslevels$ISS502[2],
                    "ISS503" = isslevels$ISS503[2],
                    "ISS504" = isslevels$ISS504[2],
                    "ISS505" = isslevels$ISS505[2],
                    "ISS506" = isslevels$ISS506[2],
                    "ISS507" = isslevels$ISS507[2],
                    "ISS510" = isslevels$ISS510[2],
                    "ISS401" = isslevels$ISS401[2],
                    "ISS600" = isslevels$ISS600[2],
                    "ISS601" = isslevels$ISS601[2],
                    "ISS602" = isslevels$ISS602[2],
                    "ISS603" = isslevels$ISS603[2],
                    "ISS604" = isslevels$ISS604[2],
                    "ISS702" = isslevels$ISS702[2],
                    "ISS900" = isslevels$ISS900[2],
                    "ISS901" = isslevels$ISS901[2],
                    "ISS902" = isslevels$ISS902[2],
                    "ISS903" = isslevels$ISS903[2],
                    "ISS904" = isslevels$ISS904[2],
                    "ISS905" = isslevels$ISS905[2],
                    "ISS906" = isslevels$ISS906[2],
                    "ISS701" = isslevels$ISS701[2],
                    "ISS703" = isslevels$ISS703[2],
                    "ISS700" = isslevels$ISS700[2],
                    "ISS300" = isslevels$ISS300[2],
                    "ISS301" = isslevels$ISS301[2],
                    "ISS302" = isslevels$ISS302[2],
                    "ISS303" = isslevels$ISS303[2],
                    "ISS304" = isslevels$ISS304[2],
                    "ISS305" = isslevels$ISS305[2],
                    "ISS306" = isslevels$ISS306[2],
                    "ISS307" = isslevels$ISS307[2],
                    "ISS308" = isslevels$ISS308[2],
                    "ISS309" = isslevels$ISS309[2],)
    valueBox(
      paste(percent(data3)), "Percent of Responses with Low Impact Ratings", icon = icon("fa fa-arrow-down"),
      color = "green"
    )
  })
  
    output$tabcomment <- renderText({
      input$tabcomment
    })

    output$Current <- renderText({
      input$selectC
    })

  output$Current2 <- renderText({
    triplefins$Name[which(grepl(input$selectC, triplefins$ISS))]
  })

  output$issdescription <- renderText({
    triplefins$Description[which(grepl(input$selectC, triplefins$ISS))]
  })

  output$Positives <- renderUI({
    pos_data <- switch(input$selectC, 
                   "ISS100" = comments$ISS100P,
                   "ISS102" = comments$ISS102P,
                   "ISS103" = comments$ISS103P,
                   "ISS104" = comments$ISS104P,
                   "ISS105" = comments$ISS105P,
                   "ISS106" = comments$ISS106P,
                   "ISS107" = comments$ISS107P,
                   "ISS109" = comments$ISS109P,
                   "ISS111" = comments$ISS111P,
                   "ISS112" = comments$ISS112P,
                   "ISS115" = comments$ISS115P,
                   "ISS116" = comments$ISS116P,
                   "ISS118" = comments$ISS118P,
                   "ISS121" = comments$ISS121P,
                   "ISS113" = comments$ISS113P,
                   "ISS250" = comments$ISS250P,
                   "ISS800" = comments$ISS800P,
                   "ISS803" = comments$ISS803P,
                   "ISS202" = comments$ISS202P,
                   "ISS251" = comments$ISS251P,
                   "ISS252" = comments$ISS252P,
                   "ISS253" = comments$ISS253P,
                   "ISS254" = comments$ISS254P,
                   "ISS201" = comments$ISS201P,
                   "ISS414" = comments$ISS414P,
                   "ISS411" = comments$ISS411P,
                   "ISS420" = comments$ISS420P,
                   
                   "ISS200" = comments$ISS200P,
                   "ISS402" = comments$ISS402P,
                   "ISS403" = comments$ISS403P,
                   "ISS404" = comments$ISS404P,
                   "ISS405" = comments$ISS405P,
                   "ISS406" = comments$ISS406P,
                   "ISS408" = comments$ISS408P,
                   "ISS500" = comments$ISS500P,
                   "ISS501" = comments$ISS501P,
                   "ISS502" = comments$ISS502P,
                   "ISS503" = comments$ISS503P,
                   "ISS504" = comments$ISS504P,
                   "ISS505" = comments$ISS505P,
                   "ISS506" = comments$ISS506P,
                   "ISS507" = comments$ISS507P,
                   "ISS510" = comments$ISS510P,
                   "ISS401" = comments$ISS401P,
                   "ISS600" = comments$ISS600P,
                   "ISS601" = comments$ISS601P,
                   "ISS602" = comments$ISS602P,
                   "ISS603" = comments$ISS603P,
                   "ISS604" = comments$ISS604P,
                   "ISS702" = comments$ISS702P,
                   "ISS900" = comments$ISS900P,
                   "ISS901" = comments$ISS901P,
                   "ISS902" = comments$ISS902P,
                   "ISS903" = comments$ISS903P,
                   "ISS904" = comments$ISS904P,
                   "ISS905" = comments$ISS905P,
                   "ISS906" = comments$ISS906P,
                   "ISS701" = comments$ISS701P,
                   "ISS703" = comments$ISS703P,
                   "ISS700" = comments$ISS700P,
                   "ISS300" = comments$ISS300P,
                   "ISS301" = comments$ISS301P,
                   "ISS302" = comments$ISS302P,
                   "ISS303" = comments$ISS303P,
                   "ISS304" = comments$ISS304P,
                   "ISS305" = comments$ISS305P,
                   "ISS306" = comments$ISS306P,
                   "ISS307" = comments$ISS307P,
                   "ISS308" = comments$ISS308P,
                   "ISS309" = comments$ISS309P,)

        HTML(paste0('<br/>', pos_data, '<br/>'))
        
  })

  output$Negatives <- renderUI({
    neg_data <- switch(input$selectC, 
                       "ISS100" = comments$ISS100N,
                       "ISS102" = comments$ISS102N,
                       "ISS103" = comments$ISS103N,
                       "ISS104" = comments$ISS104N,
                       "ISS105" = comments$ISS105N,
                       "ISS106" = comments$ISS106N,
                       "ISS107" = comments$ISS107N,
                       "ISS109" = comments$ISS109N,
                       "ISS111" = comments$ISS111N,
                       "ISS112" = comments$ISS112N,
                       "ISS115" = comments$ISS115N,
                       "ISS116" = comments$ISS116N,
                       "ISS118" = comments$ISS118N,
                       "ISS121" = comments$ISS121N,
                       "ISS113" = comments$ISS113N,
                       "ISS250" = comments$ISS250N,
                       "ISS800" = comments$ISS800N,
                       "ISS803" = comments$ISS803N,
                       "ISS202" = comments$ISS202N,
                       "ISS251" = comments$ISS251N,
                       "ISS252" = comments$ISS252N,
                       "ISS253" = comments$ISS253N,
                       "ISS254" = comments$ISS254N,
                       "ISS201" = comments$ISS201N,
                       "ISS414" = comments$ISS414N,
                       "ISS411" = comments$ISS411N,
                       "ISS420" = comments$ISS420N,
                       
                       "ISS200" = comments$ISS200N,
                       "ISS402" = comments$ISS402N,
                       "ISS403" = comments$ISS403N,
                       "ISS404" = comments$ISS404N,
                       "ISS405" = comments$ISS405N,
                       "ISS406" = comments$ISS406N,
                       "ISS408" = comments$ISS408N,
                       "ISS500" = comments$ISS500N,
                       "ISS501" = comments$ISS501N,
                       "ISS502" = comments$ISS502N,
                       "ISS503" = comments$ISS503N,
                       "ISS504" = comments$ISS504N,
                       "ISS505" = comments$ISS505N,
                       "ISS506" = comments$ISS506N,
                       "ISS507" = comments$ISS507N,
                       "ISS510" = comments$ISS510N,
                       "ISS401" = comments$ISS401N,
                       "ISS600" = comments$ISS600N,
                       "ISS601" = comments$ISS601N,
                       "ISS602" = comments$ISS602N,
                       "ISS603" = comments$ISS603N,
                       "ISS604" = comments$ISS604N,
                       "ISS702" = comments$ISS702N,
                       "ISS900" = comments$ISS900N,
                       "ISS901" = comments$ISS901N,
                       "ISS902" = comments$ISS902N,
                       "ISS903" = comments$ISS903N,
                       "ISS904" = comments$ISS904N,
                       "ISS905" = comments$ISS905N,
                       "ISS906" = comments$ISS906N,
                       "ISS701" = comments$ISS701N,
                       "ISS703" = comments$ISS703N,
                       "ISS700" = comments$ISS700N,
                       "ISS300" = comments$ISS300N,
                       "ISS301" = comments$ISS301N,
                       "ISS302" = comments$ISS302N,
                       "ISS303" = comments$ISS303N,
                       "ISS304" = comments$ISS304N,
                       "ISS305" = comments$ISS305N,
                       "ISS306" = comments$ISS306N,
                       "ISS307" = comments$ISS307N,
                       "ISS308" = comments$ISS308N,
                       "ISS309" = comments$ISS309N,)
    HTML(paste0('<br/>', neg_data, '<br/>'))
  })

  #BUILDING THE GAUGE PLOT
output$issgauge <- flexdashboard::renderGauge({
  flexdashboard::gauge(average <- switch(input$selectC, 
                          "ISS100" = isslevels$ISS100[29],
                          "ISS102" = isslevels$ISS102[29],
                          "ISS103" = isslevels$ISS103[29],
                          "ISS104" = isslevels$ISS104[29],
                          "ISS105" = isslevels$ISS105[29],
                          "ISS106" = isslevels$ISS106[29],
                          "ISS107" = isslevels$ISS107[29],
                          "ISS109" = isslevels$ISS109[29],
                          "ISS111" = isslevels$ISS111[29],
                          "ISS112" = isslevels$ISS112[29],
                          "ISS115" = isslevels$ISS115[29],
                          "ISS116" = isslevels$ISS116[29],
                          "ISS118" = isslevels$ISS118[29],
                          "ISS121" = isslevels$ISS121[29],
                          "ISS113" = isslevels$ISS113[29],
                          "ISS250" = isslevels$ISS250[29],
                          "ISS800" = isslevels$ISS800[29],
                          "ISS803" = isslevels$ISS803[29],
                          "ISS202" = isslevels$ISS202[29],
                          "ISS251" = isslevels$ISS251[29],
                          "ISS252" = isslevels$ISS252[29],
                          "ISS253" = isslevels$ISS253[29],
                          "ISS254" = isslevels$ISS254[29],
                          "ISS201" = isslevels$ISS201[29],
                          "ISS414" = isslevels$ISS414[29],
                          "ISS411" = isslevels$ISS411[29],
                          "ISS420" = isslevels$ISS420[29],
                          
                          "ISS200" = isslevels$ISS200[29],
                          "ISS402" = isslevels$ISS402[29],
                          "ISS403" = isslevels$ISS403[29],
                          "ISS404" = isslevels$ISS404[29],
                          "ISS405" = isslevels$ISS405[29],
                          "ISS406" = isslevels$ISS406[29],
                          "ISS408" = isslevels$ISS408[29],
                          "ISS500" = isslevels$ISS500[29],
                          "ISS501" = isslevels$ISS501[29],
                          "ISS502" = isslevels$ISS502[29],
                          "ISS503" = isslevels$ISS503[29],
                          "ISS504" = isslevels$ISS504[29],
                          "ISS505" = isslevels$ISS505[29],
                          "ISS506" = isslevels$ISS506[29],
                          "ISS507" = isslevels$ISS507[29],
                          "ISS510" = isslevels$ISS510[29],
                          "ISS401" = isslevels$ISS401[29],
                          "ISS600" = isslevels$ISS600[29],
                          "ISS601" = isslevels$ISS601[29],
                          "ISS602" = isslevels$ISS602[29],
                          "ISS603" = isslevels$ISS603[29],
                          "ISS604" = isslevels$ISS604[29],
                          "ISS702" = isslevels$ISS702[29],
                          "ISS900" = isslevels$ISS900[29],
                          "ISS901" = isslevels$ISS901[29],
                          "ISS902" = isslevels$ISS902[29],
                          "ISS903" = isslevels$ISS903[29],
                          "ISS904" = isslevels$ISS904[29],
                          "ISS905" = isslevels$ISS905[29],
                          "ISS906" = isslevels$ISS906[29],
                          "ISS701" = isslevels$ISS701[29],
                          "ISS703" = isslevels$ISS703[29],
                          "ISS700" = isslevels$ISS700[29],
                          "ISS300" = isslevels$ISS300[29],
                          "ISS301" = isslevels$ISS301[29],
                          "ISS302" = isslevels$ISS302[29],
                          "ISS303" = isslevels$ISS303[29],
                          "ISS304" = isslevels$ISS304[29],
                          "ISS305" = isslevels$ISS305[29],
                          "ISS306" = isslevels$ISS306[29],
                          "ISS307" = isslevels$ISS307[29],
                          "ISS308" = isslevels$ISS308[29],
                          "ISS309" = isslevels$ISS309[29],),
        min = 4.13, max = 8.53, 
        flexdashboard::gaugeSectors(
    success = c(4.13, 5.2), warning = c(5.2, 6.27), danger = c(7.34, 8.53), 
    colors <- c(ifelse(average < 5.2, "#009900", ifelse(average < 6.27, "#FFFF00",
               ifelse(average < 7.34, "#F09307", "#F00707"))))
  ))
  })


##########################################################################
  ## Static Readiness Impact listings
  output$highimpact <- renderPlotly ({
    
    df <- data.frame(triplefins)
    imp <- subset(df, df$Impact == 'High Impact')
    imp2 <- imp[order(-imp$AVG),]

    choice <- input$impactchoice
    
    ifelse(choice %in% imp2$ISS == TRUE, 
           point <- list(x = imp2$ISS[which(grepl(choice, imp2$ISS))], 
                      y = imp2$AVG[which(grepl(choice, imp2$ISS))],
                      text = paste(input$impactchoice), xref = "x", yref = "y",
                      showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40),
                      point <- NA)
   
    s <- plot_ly(imp2, type='bar', x=ISS, y=AVG, 
                 showlegend = FALSE, error_y = list(array=stdevs, thickness=0.75),
                 name=imp2$ISS, text=paste0(Name), marker=list(color=toRGB('red')))

    layout(s, title='High Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,11), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point)
    
  })

  output$modhighimpact <- renderPlotly ({
    df <- data.frame(triplefins)
    imp <- subset(df, df$Impact == 'Moderately High Impact')
    imp3 <- imp[order(-imp$AVG),]
    choice <- input$impactchoice

    s <- plot_ly(imp3, type='bar', x=ISS, y=AVG, 
                 showlegend = FALSE, error_y = list(array=stdevs, thickness=0.75),
                 name=imp3$ISS, text=paste0(Name), marker=list(color=toRGB('orange')))    
    
    ifelse(choice %in% imp3$ISS == TRUE, 
           point <- list(x = imp3$ISS[which(grepl(choice, imp3$ISS))], 
                         y = imp3$AVG[which(grepl(choice, imp3$ISS))],
                         text = paste(input$impactchoice), xref = "x", yref = "y",
                         showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40),
           point <- NA)
    
    layout(s, title='Moderately High Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,11), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point,
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$modimpact <- renderPlotly ({
    df <- data.frame(triplefins)
    imp <- subset(df, df$Impact == 'Moderate Impact')
    imp4 <- imp[order(-imp$AVG),]
    choice <- input$impactchoice
    
    s <- plot_ly(imp4, type='bar', x=ISS, y=AVG, 
                 showlegend = FALSE, error_y = list(array=stdevs, thickness=0.75),
                 name=imp4$ISS, text=paste0(Name), marker=list(color=toRGB('gold')))
    
    
    ifelse(choice %in% imp4$ISS == TRUE, 
           point <- list(x = imp4$ISS[which(grepl(choice, imp4$ISS))], 
                         y = imp4$AVG[which(grepl(choice, imp4$ISS))],
                         text = paste(input$impactchoice), xref = "x", yref = "y",
                         showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40),
           point <- NA)
    
    layout(s, title='Moderate Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,11), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point,
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$lowimpact <- renderPlotly ({
    df <- data.frame(triplefins)
    imp <- subset(df, df$Impact == 'Low Impact')
    imp5 <- imp[order(-imp$AVG),]
    choice <- input$impactchoice
    
    s <- plot_ly(imp5, type='bar', x=ISS, y=AVG, 
                 showlegend = FALSE, error_y = list(array=stdevs, thickness=0.75),
                 name=imp5$ISS, text=paste0(Name), marker=list(color=toRGB('forestgreen')))
    
    
    ifelse(choice %in% imp5$ISS == TRUE, 
           point <- list(x = imp5$ISS[which(grepl(choice, imp5$ISS))], 
                         y = imp5$AVG[which(grepl(choice, imp5$ISS))],
                         text = paste(input$impactchoice), xref = "x", yref = "y",
                         showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40),
           point <- NA)
    
    layout(s, title='Low Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,11), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE), annotations=point,
           legend=list(bordercolor="#FFFFFF"))
    
  })
  
  output$selectedimpact <- renderText({
    choice <- switch(input$impactchoice, 
                      "ISS100" = 1, "ISS102" = 2, "ISS103" = 3, "ISS104" = 4,
                      "ISS105" = 5, "ISS106" = 6, "ISS107" = 7, "ISS109" = 8,
                      "ISS111" = 9, "ISS112" = 10, "ISS115" = 11, "ISS116" = 12,
                      "ISS118" = 13, "ISS121" = 14, "ISS113" = 15, "ISS250" = 16,
                      "ISS800" = 17, "ISS803" = 18, "ISS202" = 19, "ISS251" = 20,
                      "ISS252" = 21, "ISS253" = 22,  "ISS254" = 23, "ISS201" = 24,
                      "ISS414" = 25, "ISS411" = 26, "ISS420" = 27, "ISS200" = 28,
                      "ISS400" = 29, "ISS402" = 30, "ISS403" = 31, "ISS404" = 32,
                      "ISS405" = 33, "ISS406" = 34, "ISS408"= 35, "ISS500" = 36,
                      "ISS501" = 37, "ISS502" = 38, "ISS503" = 39, "ISS504" = 40,
                      "ISS505" = 41, "ISS506" = 42, "ISS507" = 43, "ISS510" = 44, 
                      "ISS401" = 45, "ISS600" = 46, "ISS601" = 47, "ISS602"=48,
                      "ISS603"=49, "ISS604"=50, "ISS702"=51, "ISS900"=52, "ISS901"=53,
                      "ISS902"=54, "ISS903"=55, "ISS904"=56, "ISS905"=57, "ISS906"=58,
                      "ISS701"=59, "ISS703"=60, "ISS700"=61, "ISS300"=62, "ISS301"=63,
                      "ISS302"=64, "ISS303"=65, "ISS304"=66, "ISS305"=67, "ISS306"=68,
                      "ISS307"=69, "ISS308"=70, "ISS309"=71)
    df <- data.frame(triplefins)
    paste0(input$impactchoice, ' : ', df$Impact[choice], ', ', df$AVG[choice])
  })


#########################################################################
  output$MostandLeast <- renderPlotly ({
    
  #add dummy column
  df2 <- sorted[c(2:72),]
  df2$dummy2 <- NA
  
  #x axis
  xdataML <- switch(input$radioML, 
                    "Critical to Readiness"  = sorted$ISSCrit,
                    "Non-Critical to Readiness" = sorted$ISSNonCrit,)
  
  #plot dummy column - this is so you will always have something to start with
  s <- plot_ly(df2, type='bar', y=dummy2, x=xdataML, showlegend = FALSE)
  
  #y axis
  ydataML <- switch(input$radioML, 
                "Critical to Readiness" = sorted$CritHigh,
                 "Non-Critical to Readiness" = sorted$NoncritLow,)
  
  serviceML <- switch(input$radioML,
                      "Critical to Readiness" = sorted$CritNames,
                       "Non-Critical to Readiness"= sorted$NonCritNames)
  namesML <- switch(input$radioML,
                     "Critical to Readiness" = "Critical to Readiness",
                     "Non-Critical to Readiness" = "Non-Critical to Readiness",)

  color1 <- ifelse(input$radioML == 'Critical to Readiness', 'red2', 'seagreen4')
  s <- add_trace(s, x=xdataML, y=ydataML, name=namesML, text=paste0(serviceML), type="bar",
                 marker=list(color=toRGB(color1)))
  
  ydataML2 <- switch(input$radioML, 
               "Critical to Readiness"  = sorted$CritLow,
               "Non-Critical to Readiness" = sorted$NoncritHigh,)

  namesML2 <- switch(input$radioML,
                  "Non-Critical to Readiness" = "Critical to Readiness",
                  "Critical to Readiness" = "Non-Critical to Readiness",)

  color2 <- ifelse(input$radioML == 'Critical to Readiness', 'seagreen4', 'red2')
  s <- add_trace(s, x=xdataML, y=ydataML2, name=namesML2, text=paste0(serviceML), type="bar",
                 marker=list(color=toRGB(color2)))


  layout(s, yaxis=list(title="Percentage of (Non-)Critical Responses (%)"), xaxis=list(title="Installation Services", showticklabels=FALSE),
       legend=list(bordercolor="#FFFFFF"))
  })

#############################################################################################
##################################KEY INSIGHTS PAGE##########################################
#############################################################################################

output$keytable4 <- renderDataTable({
  #selected <- input$keyyear
  
  newriskdata <- data.frame(riskfeed[1], riskfeed[3:27])
  high18 <- subset(newriskdata, AVG > 7.34)
  high18 <- high18[order(-high18$AVG),]
  high18[,-1] <-round(high18[,-1],3)
  
  high18[1:13]

})



output$minikey <- renderPlotly({
  
  newriskdata <- data.frame(riskfeed[1:27])
  high18 <- subset(newriskdata, AVG > 7.34)
  high18 <- high18[order(high18$AVG),]
  
  defs <- switch(input$keychoice2, 
                 "Y2010" = high18$Def10, "Y2011" = high18$Def11,
                 "Y2012" = high18$Def12, "Y2013" = high18$Def13,
                 "Y2014" = high18$Def14, "Y2015" = high18$Def15,
                 "Y2016" = high18$Def16, "Y2017" = high18$Def17, 
                 "Y2018" = high18$Def18, "Y2019" = high18$Def19, 
                 "Y2020" = high18$Def20, "Y2021" = high18$Def21,)
  
  drs <- switch(input$keychoice2, 
                "Y2010" = high18$Y2010, "Y2011" = high18$Y2011,
                "Y2012" = high18$Y2012, "Y2013" = high18$Y2013,
                "Y2014" = high18$Y2014, "Y2015" = high18$Y2015,
                "Y2016" = high18$Y2016, "Y2017" = high18$Y2017, 
                "Y2018" = high18$Y2018, "Y2019" = high18$Y2019, 
                "Y2020" = high18$Y2020, "Y2021" = high18$Y2021,)
  
  seldata <- switch(input$keyDR, 
                    "DR" = drs,
                    "delta" = defs,)

  seltext <- switch(input$keyDR, 
                    "DR" = "Delta/Requirement Ratios for Highest Impact Services",
                    "delta" = "Funding Deltas for Highest Impact Services",)
  
  selylabels <- switch(input$keyDR, 
                    "DR" = "Delta/Requirement Ratios (%)",
                    "delta" = "Dollars",)
  colorchoice <- ifelse(seldata > 0, '#F99C9C', '#F52B2B')
  key <- plot_ly(newriskdata, x=high18$Name,y=seldata, name=high18$Name,
                 text=paste(high18$Name, paste("Impact Score: ", high18$AVG), sep='<br>'),
                 type='bar', marker=list(color=toRGB(colorchoice)))
  
  layout(key, title=paste(seltext), 
         titlefont=list(size=12), 
        yaxis=list(title=selylabels, titlefont=list(size=11)), 
        xaxis=list(title="Installation Services (Increasing by Impact Score)", 
                   titlefont=list(size=11),
                   showticklabels=FALSE))
})


output$keyfilters <- renderUI({
  year <- list(
    "2010" = "Y2010",
    "2011" = "Y2011",
    "2012" = "Y2012",
    "2013" = "Y2013",
    "2014" = "Y2014",
    "2015" = "Y2015",
    "2016" = "Y2016", 
    "2017" = "Y2017",
    "2018" = "Y2018",
    "2019" = "Y2019",
    "2020" = "Y2020",
    "2021" = "Y2021")

  selectInput('keychoice2', 'Make Selection', year)
})

output$comunderfunding <- renderPlotly({
  
  triplefins <- data.frame(triplefins)
  orddefs <- switch(input$underyear, 
                    "Y2010" = triplefins[order(triplefins$D10),], "Y2011" = triplefins[order(triplefins$D11),],
                    "Y2012" = triplefins[order(triplefins$D12),], "Y2013" = triplefins[order(triplefins$D13),],
                    "Y2014" = triplefins[order(triplefins$D14),], "Y2015" = triplefins[order(triplefins$D15),],
                    "Y2016" = triplefins[order(triplefins$D16),], "Y2017" = triplefins[order(triplefins$D17),], 
                    "Y2018" = triplefins[order(triplefins$D18),], "Y2019" = triplefins[order(triplefins$D19),], 
                    "Y2020" = triplefins[order(triplefins$D20),], "Y2021" = triplefins[order(triplefins$D21),],)
  
  col <- switch(input$underyear, 
                "Y2010" = orddefs$D10, "Y2011" = orddefs$D11,
                "Y2012" = orddefs$D12, "Y2013" = orddefs$D13,
                "Y2014" = orddefs$D14, "Y2015" = orddefs$D15,
                "Y2016" = orddefs$D16, "Y2017" = orddefs$D17,
                "Y2018" = orddefs$D18, "Y2019" = orddefs$D19,
                "Y2020" = orddefs$D20, "Y2021" = orddefs$D21,)
  
  orddefs2 <- subset(orddefs, col < 0)
  
  col2 <- switch(input$underyear, 
                "Y2010" = orddefs2$D10, "Y2011" = orddefs2$D11,
                "Y2012" = orddefs2$D12, "Y2013" = orddefs2$D13,
                "Y2014" = orddefs2$D14, "Y2015" = orddefs2$D15,
                "Y2016" = orddefs2$D16, "Y2017" = orddefs2$D17,
                "Y2018" = orddefs2$D18, "Y2019" = orddefs2$D19,
                "Y2020" = orddefs2$D20, "Y2021" = orddefs2$D21,)
  
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  colorchoice <- ifelse(orddefs2$AVG > 7.34, 'red', ifelse(orddefs2$AVG > 6.27, 'darkorange', 
                                                 ifelse(orddefs2$AVG > 5.20, 'gold', 'forestgreen')))  
  p <- plot_ly(orddefs2, x=Name, y=col2,
               name=ISS, type="bar",marker=list(color = toRGB(colorchoice)))
  
  
  layout(p, title=paste0(input$underyear, ' Under-Funded Services'),
         yaxis=list(title="Dollars"), 
         xaxis=list(title="Installation Services", showticklabels=FALSE), 
         showlegend=F)
  
})

output$comoverfunding <- renderPlotly({
  triplefins <- data.frame(triplefins)
  orddefs <- switch(input$overyear, 
                    "Y2010" = triplefins[order(triplefins$D10),], "Y2011" = triplefins[order(triplefins$D11),],
                    "Y2012" = triplefins[order(triplefins$D12),], "Y2013" = triplefins[order(triplefins$D13),],
                    "Y2014" = triplefins[order(triplefins$D14),], "Y2015" = triplefins[order(triplefins$D15),],
                    "Y2016" = triplefins[order(triplefins$D16),], "Y2017" = triplefins[order(triplefins$D17),], 
                    "Y2018" = triplefins[order(triplefins$D18),], "Y2019" = triplefins[order(triplefins$D19),], 
                    "Y2020" = triplefins[order(triplefins$D20),], "Y2021" = triplefins[order(triplefins$D21),],)
  
  col <- switch(input$overyear, 
                "Y2010" = orddefs$D10, "Y2011" = orddefs$D11,
                "Y2012" = orddefs$D12, "Y2013" = orddefs$D13,
                "Y2014" = orddefs$D14, "Y2015" = orddefs$D15,
                "Y2016" = orddefs$D16, "Y2017" = orddefs$D17,
                "Y2018" = orddefs$D18, "Y2019" = orddefs$D19,
                "Y2020" = orddefs$D20, "Y2021" = orddefs$D21,)
  
  orddefs3 <- subset(orddefs, col > 0)
  
  col3 <- switch(input$underyear, 
                 "Y2010" = orddefs3$D10, "Y2011" = orddefs3$D11,
                 "Y2012" = orddefs3$D12, "Y2013" = orddefs3$D13,
                 "Y2014" = orddefs3$D14, "Y2015" = orddefs3$D15,
                 "Y2016" = orddefs3$D16, "Y2017" = orddefs3$D17,
                 "Y2018" = orddefs3$D18, "Y2019" = orddefs3$D19,
                 "Y2020" = orddefs3$D20, "Y2021" = orddefs3$D21,)
    
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  colorchoice <- ifelse(orddefs3$AVG > 7.34, 'red', ifelse(orddefs3$AVG > 6.27, 'darkorange', 
                                                           ifelse(orddefs3$AVG > 5.20, 'gold', 'forestgreen')))  
  p <- plot_ly(orddefs3, x=Name, y=col3,
               name=ISS, type="bar",marker=list(color = toRGB(colorchoice)))
  
  layout(p, title=paste0(input$overyear, ' Over-Funded Services'),
         yaxis=list(title="Dollars"), 
         xaxis=list(title="Installation Services", showticklabels=FALSE), 
         showlegend=F)
  
})

####################################################
output$rawdatafilters <- renderUI({
  choice1 <- list("High Impact"="high", "Moderately High Impact"="modhigh",
                  "Moderate Impact"="mod", "Low Impact"="low")
  choice2 <- list("IMCOM", "NETCOM", "AMC")
  
  ifelse(input$rawdatasort == "command", choice <- choice2, choice <- choice1)
  selectInput('rawdata2', 'Make Selection', choice)
})

output$deltatable <- renderDataTable({
  
  datalabel <- switch(input$rawdata2, 
                 "IMCOM" = "IMCOM", "NETCOM" = "NETCOM", "AMC" = "AMC", 
                 "high" = 'High Impact', "modhigh" = 'Moderately High Impact', 
                 "mod" = 'Moderate Impact', "low" = 'Low Impact',)

  ydata <-  data.frame(triplefins$ISS, triplefins$Name, triplefins$D10, triplefins$D11,triplefins$D12, 
              triplefins$D13, triplefins$D14, triplefins$D15, 
              triplefins$D16, triplefins$D17,triplefins$D18, 
              triplefins$D19, triplefins$D20, triplefins$D21, triplefins$Impact,
              triplefins$Command)
  
  commandlist <- list("IMCOM", "NETCOM", "AMC")
  ifelse(datalabel %in% commandlist == TRUE, 
         chosendata <- subset(ydata, ydata$triplefins.Command == datalabel), 
         chosendata <- subset(ydata, ydata$triplefins.Impact == datalabel))
                              
  chosen2data <- subset(chosendata, select = -c(triplefins.Impact,
                                                triplefins.Command, triplefins.ISS))
  setnames(chosen2data, old=c("triplefins.Name", "triplefins.D10",  "triplefins.D11",
                              "triplefins.D12",  "triplefins.D13", "triplefins.D14",
                              "triplefins.D15",  "triplefins.D16",  "triplefins.D17", 
                              "triplefins.D18", "triplefins.D19",  "triplefins.D20",  
                              "triplefins.D21"), new=c("ISS", "D10", "D11", "D12",
                                                       "D13", "D14", "D15", "D16", "D17",
                                                       "D18", "D19", "D20", "D21"))
  
  chosen2data

})

output$fundtable <- renderDataTable({

  datalabel <- switch(input$rawdata2, 
                      "IMCOM" = "IMCOM", "NETCOM" = "NETCOM", "AMC" = "AMC", 
                      "high" = 'High Impact', "modhigh" = 'Moderately High Impact', 
                      "mod" = 'Moderate Impact', "low" = 'Low Impact',)
  
  ydata <-  data.frame(triplefins$ISS, triplefins$Name, triplefins$F10, triplefins$F11,triplefins$F12, 
                       triplefins$F13, triplefins$F14, triplefins$F15, 
                       triplefins$F16, triplefins$F17,triplefins$F18, 
                       triplefins$F19, triplefins$F20, triplefins$F21, triplefins$Impact,
                       triplefins$Command)
  
  commandlist <- list("IMCOM", "NETCOM", "AMC")
  ifelse(datalabel %in% commandlist == TRUE, 
         chosendata <- subset(ydata, ydata$triplefins.Command == datalabel), 
         chosendata <- subset(ydata, ydata$triplefins.Impact == datalabel))
  
  chosen2data <- subset(chosendata, select = -c(triplefins.Impact,
                                                triplefins.Command, triplefins.ISS))
  setnames(chosen2data, old=c("triplefins.Name", "triplefins.F10",  "triplefins.F11",
                              "triplefins.F12",  "triplefins.F13", "triplefins.F14",
                              "triplefins.F15",  "triplefins.F16",  "triplefins.F17", 
                              "triplefins.F18", "triplefins.F19",  "triplefins.F20",  
                              "triplefins.F21"), new=c("ISS", "F10", "F11", "F12",
                                                       "F13", "F14", "F15", "F16", "F17",
                                                       "F18", "F19", "F20", "F21"))
  
  chosen2data
  
})

output$reqtable <- renderDataTable({
  
  datalabel <- switch(input$rawdata2, 
                      "IMCOM" = "IMCOM", "NETCOM" = "NETCOM", "AMC" = "AMC", 
                      "high" = 'High Impact', "modhigh" = 'Moderately High Impact', 
                      "mod" = 'Moderate Impact', "low" = 'Low Impact',)
  
  ydata <-  data.frame(triplefins$ISS, triplefins$Name, triplefins$R10, triplefins$R11,triplefins$R12, 
                       triplefins$R13, triplefins$R14, triplefins$R15, 
                       triplefins$R16, triplefins$R17,triplefins$R18, 
                       triplefins$R19, triplefins$R20, triplefins$R21, triplefins$Impact,
                       triplefins$Command)
  
  commandlist <- list("IMCOM", "NETCOM", "AMC")
  ifelse(datalabel %in% commandlist == TRUE, 
         chosendata <- subset(ydata, ydata$triplefins.Command == datalabel), 
         chosendata <- subset(ydata, ydata$triplefins.Impact == datalabel))
  
  chosen2data <- subset(chosendata, select = -c(triplefins.Impact,
                                                triplefins.Command, triplefins.ISS))
  
  setnames(chosen2data, old=c("triplefins.Name", "triplefins.R10",  "triplefins.R11",
                              "triplefins.R12",  "triplefins.R13", "triplefins.R14",
                              "triplefins.R15",  "triplefins.R16",  "triplefins.R17", 
                              "triplefins.R18", "triplefins.R19",  "triplefins.R20",  
                              "triplefins.R21"), new=c("ISS", "R10", "R11", "R12",
                                                       "R13", "R14", "R15", "R16", "R17",
                                                       "R18", "R19", "R20", "R21"))
  
  chosen2data
  
})

output$DRtable <- renderDataTable({
  
  datalabel <- switch(input$rawdata2, 
                      "IMCOM" = "IMCOM", "NETCOM" = "NETCOM", "AMC" = "AMC", 
                      "high" = 'High Impact', "modhigh" = 'Moderately High Impact', 
                      "mod" = 'Moderate Impact', "low" = 'Low Impact',)
  
  ydata <-  data.frame(triplefins$ISS, triplefins$Name, triplefins$Y2010, triplefins$Y2011,triplefins$Y2012, 
                       triplefins$Y2013, triplefins$Y2014, triplefins$Y2015, 
                       triplefins$Y2016, triplefins$Y2017,triplefins$Y2018, 
                       triplefins$Y2019, triplefins$Y2020, triplefins$Y2021, triplefins$Impact,
                       triplefins$Command)
  
  commandlist <- list("IMCOM", "NETCOM", "AMC")
  ifelse(datalabel %in% commandlist == TRUE, 
         chosendata <- subset(ydata, ydata$triplefins.Command == datalabel), 
         chosendata <- subset(ydata, ydata$triplefins.Impact == datalabel))
  
  chosen2data <- subset(chosendata, select = -c(triplefins.Impact,
                                                triplefins.Command, triplefins.ISS))
  
  setnames(chosen2data, old=c("triplefins.Name", "triplefins.Y2010",  "triplefins.Y2011",
                              "triplefins.Y2012",  "triplefins.Y2013", "triplefins.Y2014",
                              "triplefins.Y2015",  "triplefins.Y2016",  "triplefins.Y2017", 
                              "triplefins.Y2018", "triplefins.Y2019",  "triplefins.Y2020",  
                              "triplefins.Y2021"), new=c("ISS", "D/R10", "D/R11", "D/R12",
                                                       "D/R13", "D/R14", "D/R15", "D/R16", "D/R17",
                                                       "D/R18", "D/R19", "D/R20", "D/R21"))
  
  chosen2data[,-1] <-round(chosen2data[,-1],3)
  chosen2data
  
})

output$downloadData <- downloadHandler(

  filename = function() {
    paste("IRA-Financial-Data", "csv", sep = ".")
  },

  content = function(file) {
    sep <- ","
    
    # Write to a file specified by the 'file' argument
    write.table(triplefins[1:38], file, sep = sep,
                row.names = FALSE)
  }
)

####################################################################################################################################
###################################################DEMOGRAPHICS PAGE################################################################
####################################################################################################################################
output$demofortfilters <- renderUI({
  choice1 <- list("High Impact"="high", "Moderately High Impact"="modhigh",
                  "Moderate Impact"="mod", "Low Impact"="low")
  choice2 <- list("IMCOM", "NETCOM", "AMC")
  
  ifelse(input$demosort == "command", choice <- choice2, 
         ifelse(input$demosort == "readiness", choice <- choice1, choice <- "All Services"))
  selectInput('demodata2', 'Make ISS Selection', choice)
})

output$compareforts <- renderPlotly({
  triplefins <- triplefins[order(-triplefins$AVG),]
  datalabel <- switch(input$demodata2, 
                      "IMCOM" = "IMCOM", "NETCOM" = "NETCOM", "AMC" = "AMC", 
                      "high" = 'High Impact', "modhigh" = 'Moderately High Impact', 
                      "mod" = 'Moderate Impact', "low" = 'Low Impact',
                      "All Services" = "All Services")
  
  ydata <-  data.frame(triplefins$ISS, triplefins$Name, triplefins$AVG, triplefins$AVGBragg,
                       triplefins$AVGCampbell, triplefins$AVGEustis, triplefins$AVGHood, triplefins$Impact,
                       triplefins$Command)
  
  commandlist <- list("IMCOM", "NETCOM", "AMC")
  readylist <- list("High Impact", "Moderately High Impact", "Moderate Impact", "Low Impact")
  
  ifelse(datalabel %in% commandlist == TRUE, 
         chosendata <- subset(ydata, ydata$triplefins.Command == datalabel), 
         ifelse(datalabel %in% readylist == TRUE, chosendata <- subset(ydata, ydata$triplefins.Impact == datalabel),
                chosendata <- ydata))
  
  df <- subset(chosendata, select = -c(triplefins.Impact, triplefins.Command))
  
  current <- df$triplefins.Name[which(grepl(input$fortservice, df$triplefins.ISS))]

  p <- plot_ly(df, y=triplefins.AVGBragg, x=triplefins.Name, name="Fort Bragg")
  p <- add_trace(p, y=triplefins.AVGCampbell, x=triplefins.Name, name="Fort Campbell")
  p <- add_trace(p, y=triplefins.AVGEustis, x=triplefins.Name, name="Fort Eustis")
  p <- add_trace(p, y=triplefins.AVGHood, x=triplefins.Name, name="Fort Hood")
  p <- add_trace(p, y=triplefins.AVG, x=triplefins.Name, name="Overall Average")

  ifelse(((current %in% df$triplefins.Name == TRUE) && (!length(current) == FALSE)),
         point2 <- list(x = current, 
                        y = max(df$triplefins.AVGBragg[which(grepl(input$fortservice, df$triplefins.ISS))],
                                df$triplefins.AVGCampbell[which(grepl(input$fortservice, df$triplefins.ISS))],
                                df$triplefins.AVGHood[which(grepl(input$fortservice, df$triplefins.ISS))],
                                df$triplefins.AVGEustis[which(grepl(input$fortservice, df$triplefins.ISS))]),
                        text = paste(df$triplefins.ISS[which(grepl(input$fortservice, df$triplefins.ISS))]), 
                        xref = "x", yref = "y",
                        showarrow = T, arrowhead = 3, ax = 20, ay = -40), 
         point2 <- NA)
  
  layout(p, title='Compare ISS Average Impact Ratings Across Forts',
         yaxis=list(title="Average Readiness Impact Score"), 
         xaxis=list(title="Installation Services (Ordered by Overall Average Impact Ratings)", showticklabels=FALSE),
         legend=list(bordercolor="#FFFFFF"), annotations=point2)
})

output$fortservice2 <- renderUI({
  
  data <- switch(input$demodata2, 
                 "IMCOM" = imcom, "NETCOM" = netcom, "AMC" = amc, "All Services" = custom,
                 "high" = high, "modhigh" = modhigh, "mod" = mod, "low" = low,)
  
  selectInput('fortservice', 'Select Installation Service', data)
})


output$demographs <- renderPlotly({
  demo <- input$demchoices
  data <- switch(demo, 
                 "Rank" = compiled$Rank, "Gender" = compiled$Gender, "Age" = compiled$Age, 
                 "Living Quarters  (On/Off-Post)" = compiled$PQ, "Race" = compiled$Race, "Rank Level" = compiled$Rtrend,
                 "Number of Dependents" = compiled$Depends, "Enlisted vs. Officers" = compiled$EO,)
  
  count <- as.data.frame(table(data))
  comp = data.frame(data, compiled[15:85])
  df <- aggregate(comp[2:72], list(demographic = comp$data), 
            mean, na.rm=TRUE, na.action = na.pass)
  df$counts <- count$Freq
  ranks = c("WO1", "2LT", "1LT", "CPT", "MAJ", "SPC", "CPL", "SGT", "SSG", "SFC", "1SG", "MSG")
  races = c("White", "Black", "Hispanic", "Native American", "Asian", "Pacific Islander", "Unknown")
  
  ifelse(demo=="Rank", 
         df$demographic <- gdata::reorder.factor(df$demographic, new.order=ranks),
         ifelse(demo=="Race", 
                df$demographic <- gdata::reorder.factor(df$demographic, new.order=races),
                df <- df))
  
  df<- df %>% arrange(demographic)
  service <- switch(input$demservice, 
                    "ISS100" = df$ISS100,
                    "ISS102" = df$ISS102,
                    "ISS103" = df$ISS103,
                    "ISS104" = df$ISS104,
                    "ISS105" = df$ISS105,
                    "ISS106" = df$ISS106,
                    "ISS107" = df$ISS107,
                    "ISS109" = df$ISS109,
                    "ISS111" = df$ISS111,
                    "ISS112" = df$ISS112,
                    "ISS115" = df$ISS115,
                    "ISS116" = df$ISS116,
                    "ISS118" = df$ISS118,
                    "ISS121" = df$ISS121,
                    "ISS113" = df$ISS113,
                    "ISS250" = df$ISS250,
                    "ISS800" = df$ISS800,
                    "ISS803" = df$ISS803,
                    "ISS202" = df$ISS202,
                    "ISS251" = df$ISS251,
                    "ISS252" = df$ISS252,
                    "ISS253" = df$ISS253,
                    "ISS254" = df$ISS254,
                    "ISS201" = df$ISS201,
                    "ISS414" = df$ISS414,
                    "ISS411" = df$ISS411,
                    "ISS420" = df$ISS420,
                    "ISS200" = df$ISS200,
                    "ISS402" = df$ISS402,
                    "ISS403" = df$ISS403,
                    "ISS404" = df$ISS404,
                    "ISS405" = df$ISS405,
                    "ISS406" = df$ISS406,
                    "ISS408" = df$ISS408,
                    "ISS500" = df$ISS500,
                    "ISS501" = df$ISS501,
                    "ISS502" = df$ISS502,
                    "ISS503" = df$ISS503,
                    "ISS504" = df$ISS504,
                    "ISS505" = df$ISS505,
                    "ISS506" = df$ISS506,
                    "ISS507" = df$ISS507,
                    "ISS510" = df$ISS510,
                    "ISS401" = df$ISS401,
                    "ISS600" = df$ISS600,
                    "ISS601" = df$ISS601,
                    "ISS602" = df$ISS602,
                    "ISS603" = df$ISS603,
                    "ISS604" = df$ISS604,
                    "ISS702" = df$ISS702,
                    "ISS900" = df$ISS900,
                    "ISS901" = df$ISS901,
                    "ISS902" = df$ISS902,
                    "ISS903" = df$ISS903,
                    "ISS904" = df$ISS904,
                    "ISS905" = df$ISS905,
                    "ISS906" = df$ISS906,
                    "ISS701" = df$ISS701,
                    "ISS703" = df$ISS703,
                    "ISS700" = df$ISS700,
                    "ISS300" = df$ISS300,
                    "ISS301" = df$ISS301,
                    "ISS302" = df$ISS302,
                    "ISS303" = df$ISS303,
                    "ISS304" = df$ISS304,
                    "ISS305" = df$ISS305,
                    "ISS306" = df$ISS306,
                    "ISS307" = df$ISS307,
                    "ISS308" = df$ISS308,
                    "ISS309" = df$ISS309,)
  
  deminfo <- paste("Number of Respondents: ", df$counts)
  avginfo <- paste("Impact Score: ", service)
  
  colordata <- ifelse(service > 7.34, 'red', ifelse(service > 6.27, 'darkorange', 
                                                 ifelse(service > 5.20, 'gold', 'green')))
  sizeup <- switch(demo, 
                 "Rank" = 3, "Gender" = 1, "Age" = 3, 
                 "Living Quarters  (On/Off-Post)" = 1, "Race" = 1.5, "Rank Level" = 2,
                 "Number of Dependents" = 2, "Enlisted vs. Officers" = 1,)
  
  p <- plot_ly(df, x=demographic, y=service, 
                 text=paste(avginfo, deminfo, sep='<br>'),
                 type='scatter', mode="markers", 
               marker=list(size=sizeup*counts, color=colordata, 
                      line=list(color='black'))) 
  
  layout(p, title=paste('Demographic Distribution For', demo), 
         yaxis=list(title="Impact to Readiness Score"), 
         xaxis=list(title=demo, tickfont=list(size=10), tickangle=0))
})

##################################METHODOLOGY######################################
output$downloadapedata <- downloadHandler(
  
  filename = function() {
    paste("IRA-APE-ISS-Match", "csv", sep = ".")
  },
  
  content = function(file) {
    sep <- ","
    
    # Write to a file specified by the 'file' argument
    write.table(apematch, file, sep = sep,
                row.names = FALSE)
  }
)

output$downloadqdata <- downloadHandler(
  
  filename = function() {
    paste("IRA-Questionnaire-Data", "csv", sep = ".")
  },
  
  content = function(file) {
    sep <- ","
    
    # Write to a file specified by the 'file' argument
    write.table(compiled, file, sep = sep,
                row.names = FALSE)
  }
)


})


