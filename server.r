library(shiny)
library(plotly)
library(ggplot2)
setwd("C:/Users/veronica.m.osborn/Desktop/Veronica - ISS Services Project")
isshist = read.csv("issforhistogram.csv")
isslevels = read.csv("issforcritlevels.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #############################################################################################
  ###############################FINANCIAL DEFICITS PAGE#######################################
  #############################################################################################
  
#INDIVIDUAL - FINANCIAL DEFICITS
  output$ISSPlotF <- renderPlotly({
    
    dataF <- switch(input$selectF, 
                   "ISS1" = isslevels$ISS1[3:14],
                   "ISS2" = isslevels$ISS2[3:14],
                   "ISS3" = isslevels$ISS3[3:14],
                   "ISS4" = isslevels$ISS4[3:14],
                   "ISS5" = isslevels$ISS5[3:14],
                   "ISS6" = isslevels$ISS6[3:14],
                   "ISS7" = isslevels$ISS7[3:14],
                   "ISS8" = isslevels$ISS8[3:14],
                   "ISS9" = isslevels$ISS9[3:14],
                   "ISS10" = isslevels$ISS10[3:14],
                   "ISS11" = isslevels$ISS11[3:14],
                   "ISS12" = isslevels$ISS12[3:14],
                   "ISS13" = isslevels$ISS13[3:14],
                   "ISS14" = isslevels$ISS14[3:14],
                   "ISS15" = isslevels$ISS15[3:14],
                   "ISS16" = isslevels$ISS16[3:14],
                   "ISS17" = isslevels$ISS17[3:14],
                   "ISS18" = isslevels$ISS18[3:14],
                   "ISS19" = isslevels$ISS19[3:14],
                   "ISS20" = isslevels$ISS20[3:14],
                   "ISS21" = isslevels$ISS21[3:14],
                   "ISS22" = isslevels$ISS22[3:14],
                   "ISS23" = isslevels$ISS23[3:14],
                   "ISS24" = isslevels$ISS24[3:14],
                   "ISS25" = isslevels$ISS25[3:14],
                   "ISS26" = isslevels$ISS26[3:14],
                   "ISS27" = isslevels$ISS27[3:14],)

    
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
                    "ISS1" = isslevels$ISS1[15],
                    "ISS2" = isslevels$ISS2[15],
                    "ISS3" = isslevels$ISS3[15],
                    "ISS4" = isslevels$ISS4[15],
                    "ISS5" = isslevels$ISS5[15],
                    "ISS6" = isslevels$ISS6[15],
                    "ISS7" = isslevels$ISS7[15],
                    "ISS8" = isslevels$ISS8[15],
                    "ISS9" = isslevels$ISS9[15],
                    "ISS10" = isslevels$ISS10[15],
                    "ISS11" = isslevels$ISS11[15],
                    "ISS12" = isslevels$ISS12[15],
                    "ISS13" = isslevels$ISS13[15],
                    "ISS14" = isslevels$ISS14[15],
                    "ISS15" = isslevels$ISS15[15],
                    "ISS16" = isslevels$ISS16[15],
                    "ISS17" = isslevels$ISS17[15],
                    "ISS18" = isslevels$ISS18[15],
                    "ISS19" = isslevels$ISS19[15],
                    "ISS20" = isslevels$ISS20[15],
                    "ISS21" = isslevels$ISS21[15],
                    "ISS22" = isslevels$ISS22[15],
                    "ISS23" = isslevels$ISS23[15],
                    "ISS24" = isslevels$ISS24[15],
                    "ISS25" = isslevels$ISS25[15],
                    "ISS26" = isslevels$ISS26[15],
                    "ISS27" = isslevels$ISS27[15],)
    valueBox(
      paste(dataF2), "Total Deficit (FY10-FY16)", icon = icon("fa fa-usd"),
      color = "teal"
    )
  })
  
  output$Pred21Box <- renderValueBox({
    dataF3 <- switch(input$selectF, 
                     "ISS1" = isslevels$ISS1[16],
                     "ISS2" = isslevels$ISS2[16],
                     "ISS3" = isslevels$ISS3[16],
                     "ISS4" = isslevels$ISS4[16],
                     "ISS5" = isslevels$ISS5[16],
                     "ISS6" = isslevels$ISS6[16],
                     "ISS7" = isslevels$ISS7[16],
                     "ISS8" = isslevels$ISS8[16],
                     "ISS9" = isslevels$ISS9[16],
                     "ISS10" = isslevels$ISS10[16],
                     "ISS11" = isslevels$ISS11[16],
                     "ISS12" = isslevels$ISS12[16],
                     "ISS13" = isslevels$ISS13[16],
                     "ISS14" = isslevels$ISS14[16],
                     "ISS15" = isslevels$ISS15[16],
                     "ISS16" = isslevels$ISS16[16],
                     "ISS17" = isslevels$ISS17[16],
                     "ISS18" = isslevels$ISS18[16],
                     "ISS19" = isslevels$ISS19[16],
                     "ISS20" = isslevels$ISS20[16],
                     "ISS21" = isslevels$ISS21[16],
                     "ISS22" = isslevels$ISS22[16],
                     "ISS23" = isslevels$ISS23[16],
                     "ISS24" = isslevels$ISS24[16],
                     "ISS25" = isslevels$ISS25[16],
                     "ISS26" = isslevels$ISS26[16],
                     "ISS27" = isslevels$ISS27[16],)
    
    valueBox(
      paste(dataF3), "Predicted Deficit (FY17-F21)", icon = icon("fa fa-usd"),
      color = "maroon"
    )
  })
  
  #############################################################################################
    #COMPARISON - FINANcIAL DEFICITS
  output$ISSLineF <- renderPlotly({
  
    dataLF <- switch(c(input$checkboxF), 
                    "ISS1" = isslevels$ISS1[17:28],
                    "ISS2" = isslevels$ISS2[17:28],
                    "ISS3" = isslevels$ISS3[17:28],
                    "ISS4" = isslevels$ISS4[17:28],
                    "ISS5" = isslevels$ISS5[17:28],
                    "ISS6" = isslevels$ISS6[17:28],)
      dataLF.mat <- subset(dataLF, subset=TRUE, select=input$checkboxF)
    print(dataLF.mat)
    pLF <- plot_ly(isslevels, x=isslevels$Button[17:28], y=dataLF.mat,
                   name=c(input$checkboxF), type="line")
       
    layout(pLF, yaxis=list(title="Deficit in $"), xaxis=list(title="Year", autorange=T, autotick=T))
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
                   "ISS1" = isshist$ISS1,
                   "ISS2" = isshist$ISS2,
                   "ISS3" = isshist$ISS3,
                   "ISS4" = isshist$ISS4,
                   "ISS5" = isshist$ISS5,
                   "ISS6" = isshist$ISS6,
                   "ISS7" = isshist$ISS7,
                   "ISS8" = isshist$ISS8,
                   "ISS9" = isshist$ISS9,
                   "ISS10" = isshist$ISS10,
                   "ISS11" = isshist$ISS11,
                   "ISS12" = isshist$ISS12,
                   "ISS13" = isshist$ISS13,
                   "ISS14" = isshist$ISS14,
                   "ISS15" = isshist$ISS15,
                   "ISS16" = isshist$ISS16,
                   "ISS17" = isshist$ISS17,
                   "ISS18" = isshist$ISS18,
                   "ISS19" = isshist$ISS19,
                   "ISS20" = isshist$ISS20,
                   "ISS21" = isshist$ISS21,
                   "ISS22" = isshist$ISS22,
                   "ISS23" = isshist$ISS23,
                   "ISS24" = isshist$ISS24,
                   "ISS25" = isshist$ISS25,
                   "ISS26" = isshist$ISS26,
                   "ISS27" = isshist$ISS27,)
    
    
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
                   "ISS1" = isslevels$ISS1[1],
                   "ISS2" = isslevels$ISS2[1],
                   "ISS3" = isslevels$ISS3[1],
                   "ISS4" = isslevels$ISS4[1],
                   "ISS5" = isslevels$ISS5[1],
                   "ISS6" = isslevels$ISS6[1],
                   "ISS7" = isslevels$ISS7[1],
                   "ISS8" = isslevels$ISS8[1],
                   "ISS9" = isslevels$ISS9[1],
                   "ISS10" = isslevels$ISS10[1],
                   "ISS11" = isslevels$ISS11[1],
                   "ISS12" = isslevels$ISS12[1],
                   "ISS13" = isslevels$ISS13[1],
                   "ISS14" = isslevels$ISS14[1],
                   "ISS15" = isslevels$ISS15[1],
                   "ISS16" = isslevels$ISS16[1],
                   "ISS17" = isslevels$ISS17[1],
                   "ISS18" = isslevels$ISS18[1],
                   "ISS19" = isslevels$ISS19[1],
                   "ISS20" = isslevels$ISS20[1],
                   "ISS21" = isslevels$ISS21[1],
                   "ISS22" = isslevels$ISS22[1],
                   "ISS23" = isslevels$ISS23[1],
                   "ISS24" = isslevels$ISS24[1],
                   "ISS25" = isslevels$ISS25[1],
                   "ISS26" = isslevels$ISS26[1],
                   "ISS27" = isslevels$ISS27[1],)
    valueBox(
      paste(data2), "Critical", icon = icon("fa fa-arrow-up"),
      color = "red"
    )
  })
  output$LowCritBox <- renderValueBox({
    data3 <- switch(input$selectC, 
                    "ISS1" = isslevels$ISS1[2],
                    "ISS2" = isslevels$ISS2[2],
                    "ISS3" = isslevels$ISS3[2],
                    "ISS4" = isslevels$ISS4[2],
                    "ISS5" = isslevels$ISS5[2],
                    "ISS6" = isslevels$ISS6[2],
                    "ISS7" = isslevels$ISS7[2],
                    "ISS8" = isslevels$ISS8[2],
                    "ISS9" = isslevels$ISS9[2],
                    "ISS10" = isslevels$ISS10[2],
                    "ISS11" = isslevels$ISS11[2],
                    "ISS12" = isslevels$ISS12[2],
                    "ISS13" = isslevels$ISS13[2],
                    "ISS14" = isslevels$ISS14[2],
                    "ISS15" = isslevels$ISS15[2],
                    "ISS16" = isslevels$ISS16[2],
                    "ISS17" = isslevels$ISS17[2],
                    "ISS18" = isslevels$ISS18[2],
                    "ISS19" = isslevels$ISS19[2],
                    "ISS20" = isslevels$ISS20[2],
                    "ISS21" = isslevels$ISS21[2],
                    "ISS22" = isslevels$ISS22[2],
                    "ISS23" = isslevels$ISS23[2],
                    "ISS24" = isslevels$ISS24[2],
                    "ISS25" = isslevels$ISS25[2],
                    "ISS26" = isslevels$ISS26[2],
                    "ISS27" = isslevels$ISS27[2],)
    valueBox(
      paste(data3), "Non-Critical", icon = icon("fa fa-arrow-down"),
      color = "green"
    )
  })
  
})

#############################################################################################