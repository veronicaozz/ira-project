library(shiny)
library(plotly)
library(scales)

#setwd("C:/Users/veronica.m.osborn/Desktop/Veronica - ISS Services Project")
isshist = read.csv("issforhistogram.csv", stringsAsFactors=FALSE)
isslevels = read.csv("issforcritlevels2.csv", stringsAsFactors=FALSE)
riskflip = read.csv("riskflip.csv", stringsAsFactors=FALSE)
riskfeed = read.csv("riskfeed.csv", stringsAsFactors=FALSE)
sorted = read.csv ("MostandLeastCrit_Sorted.csv")
comments = read.csv("soldiercomments.csv", stringsAsFactors=FALSE)
impactdata = read.csv("impactlevels.csv", stringsAsFactors=FALSE)
triplefins = read.csv("triplefinancials.csv", stringsAsFactors=FALSE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ##HOME PAGE
  output$TotalFunds <- renderValueBox({
    valueBox(
      paste("$70,377,028,000"), "Total ISS Funding Included in Analysis",  icon = icon("fa fa-usd"),
      color = "light-blue"
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

    color <- ifelse(riskfeed$AVG > 7.34, 'red', ifelse(riskfeed$AVG > 6.27, 'darkorange', 
              ifelse(riskfeed$AVG > 5.20, 'gold', 'green')))
    
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
    
    pRS <- plot_ly(riskfeed, x=AVG,y=dataRS, 
                   text=paste(Name,sep='<br>',"Deficit: ", dollar(defRS)),
                   type='scatter', mode="markers", marker=list(size=8*sizeRS, color=color)) 

    layout(pRS, yaxis=list(title="Ratio of Deficit/Requirement (%)"), 
           xaxis=list(title="Readiness Impact Rating", autorange=T, autotick=T),
           annotations=a) 
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
  
  point <- list(x = input$selectRS, y = dataRS[chosen3],
    text = paste(input$findISSrisk), xref = "x", yref = "y",
    showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40)
  
  MRS <- plot_ly(riskfeed, x=colnames(riskfeed[3:14],),y=ratiodata, 
                 text=paste(Name[chosen3], sep='<br>',"Deficit: ", dollar(defdata)),
                 type='scatter', mode="markers", marker=list(size=4*sizedata, 
                 color=colordata)) 
  
  layout(MRS, title=Name[chosen3], titlefont=list(size=12), 
         yaxis=list(title="Deficit/Requirement Ratio"), 
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
                     marker=list(size=8*(eval(parse(text=selected[i])))[26:37], 
                                 color=ifelse(eval(parse(text=selected[i]))[13] > 7.34, 'red', 
                                          ifelse(eval(parse(text=selected[i]))[13]> 6.27, 'darkorange', 
                                             ifelse(eval(parse(text=selected[i]))[13] > 5.20, 'gold', 'green')))),
                     evaluate=TRUE, name=selected[i])
    }
  }
  
  layout(p, title = "Compare ISS Financial Deficits by Fiscal Years",
         yaxis=list(title="Deficit/Requirement Ratios"), 
         xaxis=list(title="Year", autorange=T, autotick=T, tickangle=0),
         legend=list(bordercolor="#FFFFFF"))
  
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
                  showarrow = TRUE, arrowhead = 5, ax = 20, ay = -40)
    
    layout(pF, title=paste('FY10-FY21 Annual Deficits for', input$selectF),
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
    valueBox(
      paste(dollar(dataF2)), "Total Deficit (FY10-FY16)", icon = icon("fa fa-usd"),
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
    
    valueBox(
      paste(dollar(dataF3)), "Predicted Deficit (FY17-F21)", icon = icon("fa fa-usd"),
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

  p <- plot_ly(triplefins, x=triplefins$Name[service], y=dollar(funds[service]),
                name='Funding', type="bar",marker=list(color = toRGB("olivedrab3")))
  
  p <- add_trace(p, x=triplefins$Name[service], y=dollar(reqs[service]), 
                 name='Requirement', type='bar', marker=list(color=toRGB("indianred")))
  
  future <- list("Y2017", "Y2018", "Y2019", "Y2020", "Y2021")
  
  colorchoice <- ifelse(input$minifinyears %in% future == TRUE, 'violetred3', 'cyan3')
  
  p <- add_trace(p, x=triplefins$Name[service], y=dollar(defs[service]), 
                 name='Deficit', type='bar', marker=list(color=toRGB(c(colorchoice))))
  
  layout(p, title=paste0(input$minifinyears, ' Financial Information for ', input$selectF),
         yaxis=list(title="Dollars"), xaxis=list(title="Installation Service", autorange=T, autotick=T))
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


#############################################################################################
###############################CRITICALITY LEVELS PAGE#######################################
#############################################################################################

  #Individual - Criticality Levels

    #output$value <- renderText({
       # paste("You have selected", input$selectC)
      #})
      
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
    
    p <- plot_ly(isshist, x=as.character(isshist$Score),y=data,
                 name=input$selectC,type="bar",marker=list(color = toRGB(c
                                                                        ("red", "red", "red", 
                                                                         "orangered", "orangered", "orangered", 
                                                                         "orange", "orange", "orange", "orange", 
                                                                         "sandybrown", "sandybrown", "sandybrown", "sandybrown", "sandybrown", 
                                                                         "gold", "gold", "gold", "gold", 
                                                                         "greenyellow", "greenyellow", "greenyellow", 
                                                                         "green3", "green3", "green3"))
                 ))
    
    layout(p, title= "Distribution of Questionnaire Responses",
           yaxis=list(title="Frequency of Responses"), xaxis=list(title="Questionnaire Responses", autorange=T, 
                         autotick=T, tickangle=0, tickfont=list(size=10)))
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
      paste(percent(data2)), "Responses in Critical Zone", icon = icon("fa fa-arrow-up"),
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
      paste(percent(data3)), "Responses in Non-Critical Zone", icon = icon("fa fa-arrow-down"),
      color = "green"
    )
  })
  
    output$tabcomment <- renderText({
      input$tabcomment
    })

    output$Current <- renderText({
      input$selectC
    })

  output$Positives <- renderText({
    pos_data <- switch(input$selectC, 
                   "ISS100" = comments$ISS100[1],
                   "ISS102" = comments$ISS102[1],
                   "ISS103" = comments$ISS103[1],
                   "ISS104" = comments$ISS104[1],
                   "ISS105" = comments$ISS105[1],
                   "ISS106" = comments$ISS106[1],
                   "ISS107" = comments$ISS107[1],
                   "ISS109" = comments$ISS109[1],
                   "ISS111" = comments$ISS111[1],
                   "ISS112" = comments$ISS112[1],
                   "ISS115" = comments$ISS115[1],
                   "ISS116" = comments$ISS116[1],
                   "ISS118" = comments$ISS118[1],
                   "ISS121" = comments$ISS121[1],
                   "ISS113" = comments$ISS113[1],
                   "ISS250" = comments$ISS250[1],
                   "ISS800" = comments$ISS800[1],
                   "ISS803" = comments$ISS803[1],
                   "ISS202" = comments$ISS202[1],
                   "ISS251" = comments$ISS251[1],
                   "ISS252" = comments$ISS252[1],
                   "ISS253" = comments$ISS253[1],
                   "ISS254" = comments$ISS254[1],
                   "ISS201" = comments$ISS201[1],
                   "ISS414" = comments$ISS414[1],
                   "ISS411" = comments$ISS411[1],
                   "ISS420" = comments$ISS420[1],
                   
                   "ISS200" = comments$ISS200[1],
                   "ISS402" = comments$ISS402[1],
                   "ISS403" = comments$ISS403[1],
                   "ISS404" = comments$ISS404[1],
                   "ISS405" = comments$ISS405[1],
                   "ISS406" = comments$ISS406[1],
                   "ISS408" = comments$ISS408[1],
                   "ISS500" = comments$ISS500[1],
                   "ISS501" = comments$ISS501[1],
                   "ISS502" = comments$ISS502[1],
                   "ISS503" = comments$ISS503[1],
                   "ISS504" = comments$ISS504[1],
                   "ISS505" = comments$ISS505[1],
                   "ISS506" = comments$ISS506[1],
                   "ISS507" = comments$ISS507[1],
                   "ISS510" = comments$ISS510[1],
                   "ISS401" = comments$ISS401[1],
                   "ISS600" = comments$ISS600[1],
                   "ISS601" = comments$ISS601[1],
                   "ISS602" = comments$ISS602[1],
                   "ISS603" = comments$ISS603[1],
                   "ISS604" = comments$ISS604[1],
                   "ISS702" = comments$ISS702[1],
                   "ISS900" = comments$ISS900[1],
                   "ISS901" = comments$ISS901[1],
                   "ISS902" = comments$ISS902[1],
                   "ISS903" = comments$ISS903[1],
                   "ISS904" = comments$ISS904[1],
                   "ISS905" = comments$ISS905[1],
                   "ISS906" = comments$ISS906[1],
                   "ISS701" = comments$ISS701[1],
                   "ISS703" = comments$ISS703[1],
                   "ISS700" = comments$ISS700[1],
                   "ISS300" = comments$ISS300[1],
                   "ISS301" = comments$ISS301[1],
                   "ISS302" = comments$ISS302[1],
                   "ISS303" = comments$ISS303[1],
                   "ISS304" = comments$ISS304[1],
                   "ISS305" = comments$ISS305[1],
                   "ISS306" = comments$ISS306[1],
                   "ISS307" = comments$ISS307[1],
                   "ISS308" = comments$ISS308[1],
                   "ISS309" = comments$ISS309[1],)
    pos_data
  })

  output$Negatives <- renderText({
    neg_data <- switch(input$selectC, 
                       "ISS100" = comments$ISS100[2],
                       "ISS102" = comments$ISS102[2],
                       "ISS103" = comments$ISS103[2],
                       "ISS104" = comments$ISS104[2],
                       "ISS105" = comments$ISS105[2],
                       "ISS106" = comments$ISS106[2],
                       "ISS107" = comments$ISS107[2],
                       "ISS109" = comments$ISS109[2],
                       "ISS111" = comments$ISS111[2],
                       "ISS112" = comments$ISS112[2],
                       "ISS115" = comments$ISS115[2],
                       "ISS116" = comments$ISS116[2],
                       "ISS118" = comments$ISS118[2],
                       "ISS121" = comments$ISS121[2],
                       "ISS113" = comments$ISS113[2],
                       "ISS250" = comments$ISS250[2],
                       "ISS800" = comments$ISS800[2],
                       "ISS803" = comments$ISS803[2],
                       "ISS202" = comments$ISS202[2],
                       "ISS251" = comments$ISS251[2],
                       "ISS252" = comments$ISS252[2],
                       "ISS253" = comments$ISS253[2],
                       "ISS254" = comments$ISS254[2],
                       "ISS201" = comments$ISS201[2],
                       "ISS414" = comments$ISS414[2],
                       "ISS411" = comments$ISS411[2],
                       "ISS420" = comments$ISS420[2],
                       
                       "ISS200" = comments$ISS200[2],
                       "ISS402" = comments$ISS402[2],
                       "ISS403" = comments$ISS403[2],
                       "ISS404" = comments$ISS404[2],
                       "ISS405" = comments$ISS405[2],
                       "ISS406" = comments$ISS406[2],
                       "ISS408" = comments$ISS408[2],
                       "ISS500" = comments$ISS500[2],
                       "ISS501" = comments$ISS501[2],
                       "ISS502" = comments$ISS502[2],
                       "ISS503" = comments$ISS503[2],
                       "ISS504" = comments$ISS504[2],
                       "ISS505" = comments$ISS505[2],
                       "ISS506" = comments$ISS506[2],
                       "ISS507" = comments$ISS507[2],
                       "ISS510" = comments$ISS510[2],
                       "ISS401" = comments$ISS401[2],
                       "ISS600" = comments$ISS600[2],
                       "ISS601" = comments$ISS601[2],
                       "ISS602" = comments$ISS602[2],
                       "ISS603" = comments$ISS603[2],
                       "ISS604" = comments$ISS604[2],
                       "ISS702" = comments$ISS702[2],
                       "ISS900" = comments$ISS900[2],
                       "ISS901" = comments$ISS901[2],
                       "ISS902" = comments$ISS902[2],
                       "ISS903" = comments$ISS903[2],
                       "ISS904" = comments$ISS904[2],
                       "ISS905" = comments$ISS905[2],
                       "ISS906" = comments$ISS906[2],
                       "ISS701" = comments$ISS701[2],
                       "ISS703" = comments$ISS703[2],
                       "ISS700" = comments$ISS700[2],
                       "ISS300" = comments$ISS300[2],
                       "ISS301" = comments$ISS301[2],
                       "ISS302" = comments$ISS302[2],
                       "ISS303" = comments$ISS303[2],
                       "ISS304" = comments$ISS304[2],
                       "ISS305" = comments$ISS305[2],
                       "ISS306" = comments$ISS306[2],
                       "ISS307" = comments$ISS307[2],
                       "ISS308" = comments$ISS308[2],
                       "ISS309" = comments$ISS309[2],)
    neg_data
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
  ## Static Criticality listings
  output$highimpact <- renderPlotly ({
    
    df <- data.frame(impactdata)
    imp <- subset(df, df$Impact == 'High Impact')
    imp <- imp[order(-imp$AVG),]
    s <- plot_ly(imp, type='bar', x=service, y=AVG, showlegend = FALSE,
                 name=imp$service, text=paste0(Name), marker=list(color=toRGB('red')))

    layout(s, title='High Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,8.5), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE),
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$modhighimpact <- renderPlotly ({
    df <- data.frame(impactdata)
    imp <- subset(df, df$Impact == 'Moderately High Impact')
    imp <- imp[order(-imp$AVG),]
    s <- plot_ly(imp, type='bar', x=service, y=AVG, showlegend = FALSE,
                 name=imp$service, text=paste0(Name), marker=list(color=toRGB('orange')))    
    
    layout(s, title='Moderately High Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,8.5), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE),
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$modimpact <- renderPlotly ({
    df <- data.frame(impactdata)
    imp <- subset(df, df$Impact == 'Moderate Impact')
    imp <- imp[order(-imp$AVG),]
    s <- plot_ly(imp, type='bar', x=service, y=AVG, showlegend = FALSE,
                 name=imp$service, text=paste0(Name), marker=list(color=toRGB('gold')))
    
    layout(s, title='Moderate Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,8.5), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE),
           legend=list(bordercolor="#FFFFFF"))
    
  })

  output$lowimpact <- renderPlotly ({
    df <- data.frame(impactdata)
    imp <- subset(df, df$Impact == 'Low Impact')
    imp <- imp[order(-imp$AVG),]
    s <- plot_ly(imp, type='bar', x=service, y=AVG, showlegend = FALSE,
                 name=imp$service, text=paste0(Name), marker=list(color=toRGB('forestgreen')))
    
    layout(s, title='Low Level of Impact to Readiness',
           titlefont=list(size=12),
           yaxis=list(range=c(0,8.5), title="Average Readiness Impact Score"), 
           xaxis=list(title="Installation Services", showticklabels=FALSE),
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
    df <- data.frame(impactdata)
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
})
#############################################################################################