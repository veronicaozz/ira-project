library(shiny)
library(plotly)
library(ggplot2)
library(RColorBrewer)
#setwd("C:/Users/veronica.m.osborn/Desktop/Veronica - ISS Services Project")
isshist = read.csv("issforhistogram.csv")
isslevels = read.csv("issforcritlevels2.csv", stringsAsFactor=FALSE)
riskfeed = read.csv("riskfeed.csv")
sorted = read.csv ("MostandLeastCrit_Sorted.csv")
comments = read.csv("soldiercomments.csv", stringsAsFactors=FALSE, allowEscapes=TRUE)

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
    
    pRS <- plot_ly(riskfeed, x=AVG,y=dataRS, 
                   text=paste(Name,sep='<br>',"Deficit: ", defRS),
                   type='scatter', mode="markers", marker=list(size=9*sizeRS, color=color)) 
    
    layout(pRS, yaxis=list(title="Ratio of Deficit/Requirement (%)"), 
           xaxis=list(title="Average Criticality Score", autorange=T, autotick=T))
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
                    "ISS420" = isslevels$ISS420[3:14],)

    
    pF <- plot_ly(isslevels, x=isslevels$Button[3:14],y=dataF,
                 name=input$selectF,type="bar",marker=list(color = toRGB(c
                                                                         ("turquoise3", "turquoise3", "turquoise3",
                                                                          "turquoise3", "turquoise3", "turquoise3", "turquoise3", 
                                                                          "violetred3", "violetred3", "violetred3", 
                                                                          "violetred3", "violetred3")))
                 )
    
    layout(pF, yaxis=list(title="Deficit in $"), xaxis=list(title="Year", autorange=T, autotick=T))
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
                     "ISS420" = isslevels$ISS420[15],)
    valueBox(
      paste(dataF2), "Total Deficit (FY10-FY16)", icon = icon("fa fa-usd"),
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
                     "ISS420" = isslevels$ISS420[16],)
    
    valueBox(
      paste(dataF3), "Predicted Deficit (FY17-F21)", icon = icon("fa fa-usd"),
      color = "maroon"
    )
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
                   "ISS420" = isshist$ISS420,)
    
    
    minx<- min(data)
    maxx<- max(data)
    
    p <- plot_ly(isshist, x=isshist$Score,y=data,
                 name=input$selectC,type="bar",marker=list(color = toRGB(c
                                                                        ("red", "red", "red", 
                                                                         "orangered", "orangered", "orangered", 
                                                                         "orange", "orange", "orange", "orange", 
                                                                         "sandybrown", "sandybrown", "sandybrown", "sandybrown", "sandybrown", 
                                                                         "gold", "gold", "gold", "gold", 
                                                                         "greenyellow", "greenyellow", "greenyellow", 
                                                                         "green3", "green3", "green3"))
                 ))
    
    layout(p, yaxis=list(title="Frequency of Responses"), xaxis=list(title="Survey Responses", autorange=T, 
                         autotick=T))
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
                   "ISS420" = isslevels$ISS420[1],)
    valueBox(
      paste(data2), "Critical", icon = icon("fa fa-arrow-up"),
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
                    "ISS420" = isslevels$ISS420[2],)
    valueBox(
      paste(data3), "Non-Critical", icon = icon("fa fa-arrow-down"),
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
                   "ISS420" = comments$ISS420[1],)
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
                       "ISS420" = comments$ISS420[2],)
    neg_data
  })

##########################################################################
  ## Static Criticality listings
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