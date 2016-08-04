library(shiny)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)

setwd("C:/Users/veronica.m.osborn/Desktop/Veronica - ISS Services Project")
isshist = read.csv("issforhistogram.csv")
isslevels = read.csv("issforcritlevels2.csv")
riskfeed = read.csv("riskfeed.csv")
comments = read.csv("soldiercomments.csv", stringsAsFactors=FALSE, allowEscapes=FALSE)

header <- dashboardHeader(title = "Installation Readiness")
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("fa fa-home")),
      menuItem("Methodology", tabName = "methodology", icon = icon("fa fa-briefcase")),
      menuItem("Risk Assessment", tabName = "risk", icon = icon("fa fa-bomb")),
      menuItem("Financial Deficits", tabName = "deficits", icon = icon("fa fa-line-chart")),
      menuItem("ISS Criticality Levels", tabName = "criticality", icon = icon("fa fa-bar-chart")),
      menuItem("Demographic Insights", tabName = "demographics", icon = icon("fa fa-male")),
      menuItem("Raw Data", tabName = "data", icon = icon("fa fa-file-text")),
      menuItem("References", tabName = "references", icon = icon("fa fa-book"))
    ))
  
body <- dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: serif;
        font-weight: ;
        font-size: 18px;
      }
    '))),
    
    tabItems(
      # First tab content
#############################################################################################
  
      tabItem(tabName = "home",
              h2("Installation Readiness Analysis Project", align = "center"),
              fluidRow(column(6, offset=1, 
                              
                    box(h4("CSA #1 Priority", align='center'),
                    p('\"Our fundamental task is like not other - it is to win in
                      the unforgiving crucible of ground combat. We must ensure the Army remains                    
                      ready as the world\'s premier combat force. Readiness for ground combat is - and
                      will remain - the U.S. Army\'s No. 1 priority. Our most values assets, indeed, the
                      nation\'s most valued assets, are our Soldiers and our solemn commitment must 
                      always be to never send them into harm\'s way untrained, poorly led,
                      undermanned or with less than the best equipment we can provide. Readiness is
                      No. 1, and there is no other No. 1.\"'), h5(em("- General Milley, CSA"), align='center'),
                    solidHeader=TRUE, background='blue',width=NULL),
                              
                    box(h4("Background", align="center"),
                    p("The Principal Deputy Assistant Secretary of the Army for Installations, 
                      Energy & Environment (DASA-IE&E) has requested CAA assist them in prioritizing 
                      installations services critical to Army readiness to avoid potential underfunding 
                      of installations in future Program Objective Memorandums (POMs)."), 
                    solidHeader=TRUE, background='navy',width=NULL),
                    
                    box(h4("Purpose", align="center"),
                      p("Provide ASA-IE&E with an analytical overview of the direct and indirect 
                      linkages between installation and service programs and the U.S. Army's readiness 
                      posture, and show the potential impact of funding below requirements levels. 
                      The analysis will serve to assist future POM planning and funding."), 
                    solidHeader=TRUE, background='light-blue',width=NULL)),
                    
                    column(4, 
                           tabBox(
                             title = "Objectives",
                             # The id lets us use input$tabset1 on the server to find the current tab
                             id = "tabset1", width=NULL,
                             tabPanel("Criticality", h5(strong("With regards to ISS Criticality:"), align='center'),
                                      p("1. Determine installation and service program linkages to Readiness."), 
                                      p("2. Develop a proposed prioritization matrix to help articulate the 
                                      direct and indirect support to U.S. Army readiness."), 
                                      p("3. Develop methodology to measure the criticality of facilities on an installation."), 
                                      p("4. Transform qualitative commentary provided by respondents about most 
                                      critical/least critical services into constructive quantitative data to be used 
                                      as support for main insights.")),
                             tabPanel("Funding", 
                                      h5(strong("With regards to ISS Funding:"), align='center'),
                                      p("1. Analyze the Installation Program Evaluation Group (II PEG) installation and 
                                      service funding; comparing funding levels to requirements to assess short- and 
                                      long-term impact of deferred funding on Army readiness."),
                                      p("2. Perform CUFR analysis to determine Amy Program Elements (APEs) that are 
                                      under-funded based on Headquarters Department of the Army (HQDA) 
                                      critical requirements.")),
                             tabPanel("Risk", 
                                      h5(strong("Defining and Assessing Risk: "), align='center'),
                                      p("The end goal is to link service criticality level to service funding in order to develop insights 
                                      on what services are at the highest risk to mission readiness if left under-funded."),
                                      p("This can be achieved through a comparison of II PEG 
                                      funding/requirements to assess high/low priority installation services that 
                                      support unit readiness which have been under-funded in the past."),
                                      p("Analysis will provide data on where it is possible to divest (low priority services) and 
                                      which services require further investment (high priority services) during 
                                      future POM planning."))
                    ),
                    
                    valueBoxOutput("TotalFunds", width = NULL),
                   img(src="armyanalysis.png", height = 250, width = 250,
                       style="display: block; margin-left: auto; margin-right: auto;")
              ))),
      
#####################################################################################
      tabItem(tabName = "risk",
              navbarPage("Risk Assessment",
                tabPanel("All Services",
              h3("Risk Assessment by Fiscal Year"),
              fluidRow(column(5, fluidRow(column(width=5, box(selectInput
                          ("selectRS", label = h4("Select Fiscal Year"), 
                            choices = list(
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
                              "2021" = "Y2021"),
                            selected = "Y2010"), status='warning', width = NULL)),
                          column(7, box(selectInput("findISSrisk", label = h4("Find an ISS"), 
                                  choices = list(
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
                                    "420: Facilities Maintenance - Horizontal" = "ISS420"),
                                  selected = NULL), 
                              status='warning',width = NULL))),
                          box(plotlyOutput("minirisk", height=350), width=NULL),
                          box(h4(strong("Risk Analysis"), align="center"),
                              p("Risk is determined as a combination of a service\'s", strong(" criticality to
                                mission readiness"), "and the service\'s", strong("level of under-funding.")),
                              p("An ISS\'s (x,y) risk coordinates are determined by its average level of criticality, determined
                                from the ISDI questionnaire, and the ratio of the service\'s deficit to its funding requirement,
                                determined by the eProbe database by fiscal year."),
                              p("By representing both the size of the deficit (by means of bubble diameters)
                                and the ISS\'s deficit-to-requirement ratio, 
                               one can compare two services with an equal deficit ($1 million deficit) but with different HQDA
                               requirements: large requirement (small impact) vs. small requirement (large impact) to aid the 
                               POM planning process (i.e not all deficits are created equal)."),
                              background='yellow',width=NULL)),
        column(width = 7, box(plotlyOutput("riskscatter", height=650), width=NULL),
                     box(h5(strong("Highest and Lowest Risk ISSs"), align="center"),
                         p(strong("High risk "), "services lie in the bottom right quadrant on the plot",
                           em("(high criticality level with large deficit-to-requirement ratio)"), " while ", strong("low risk"), 
                           "services lie in the top left quadrant on the plot ", em("(low criticality level with small
                                  deficit-to-requirement ratio).")),status='warning', width=NULL))
    )),
                    tabPanel("Compare ISS")
  )),
  
  #############################################################################################
  #Content for the Methodology Tab
  tabItem(tabName = "methodology",
          h2("Methodology Content"),
          splitLayout(
            mainPanel(
              img(src="alphamatrix.png", height = 320, width = 458),
              img(src="scoreassignment.png", height = 320, width = 458)
               )
  )),
  
#############################################################################################
  #Content for the Financial Deficits Tab
  tabItem(tabName = "deficits",
          
          navbarPage("Financial Deficits",
                     tabPanel("Individual",
                  
          h3("Individual Assessment of ISS Financial Deficits"),
             fluidRow(column(width = 4, box(selectInput("selectF", label = h4("Select Installation Service"), 
                                      choices = list(
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
                                        "420: Facilities Maintenance - Horizontal" = "ISS420"),
                                    selected = "ISS100"), width = NULL), 
                             valueBoxOutput("Def16Box", width = NULL),
                            valueBoxOutput("Pred21Box", width = NULL)),
          column(width = 7, plotlyOutput("ISSPlotF")))),
          
          tabPanel("Compare", h3("Comparison of ISS Financial Deficits"),
                   fluidRow(column(width = 4, box(checkboxGroupInput("checkboxF", 
                         label = h4("Select Installation Service(s)"), 
                                  choices = list(
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
                                "420: Facilities Maintenance - Horizontal" = "ISS420"),
                             selected = "ISS100"), width = NULL)),
                             column(width=7, 
                                    box(plotlyOutput("ISSLineF"), collapsible=TRUE, status='primary', width=NULL),
                                    box(h5(strong("Note: "), "All colors have been arbitrarily chosen by the program.", align='center'), 
                                        background='blue', width=NULL),
                                    box(plotlyOutput("ISSLineF2"), collapsible=TRUE, status='primary', width=NULL))
                   )
  ))
  ),
  
  ############################################################################################# 
  #Content for the ISS Criticality Levels Tab
  tabItem(tabName = "criticality",
          
          navbarPage("Criticality Levels",
                     tabPanel("Individual",
          
          h3("Individual Assessment of ISS Criticality Levels"),
          fluidRow(column(width = 4, box(selectInput("selectC", label = h4("Select Installation Service"), 
                                   choices = list(
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
                                     "420: Facilities Maintenance - Horizontal" = "ISS420"),
                                   selected = "ISS100"), width = NULL, color="red"),
                       
          valueBoxOutput("HighCritBox", width = NULL),
          valueBoxOutput("LowCritBox", width = NULL),
          tabBox(
            title = textOutput("Current"),
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabcomment", width=NULL,
            tabPanel("Positive", 
                     h5(strong("Positive Soldier Commentary:"), align='center'),
                     h5(textOutput("Positives"))),
            tabPanel("Negative", 
                     h5(strong("Negative Soldier Commentary: "), align='center'),
                     h5(textOutput("Negatives")))

          )),
          column(width = 7, plotlyOutput("ISSPlot")))    
  ),
          tabPanel("Compare", h3("Comparison of ISS Criticality Levels"),
                   fluidRow(box(h4("Overall", align='center'), background='blue', width=NULL)),
                   fluidRow(box(
                     column(4, box(radioButtons("radioML", label = h4("Sorting Options"),
                                                               choices = list("Sort by Most Critical" = "Critical to Readiness", 
                                                                              "Sort by Least Critical" = "Non-Critical to Readiness"), 
                                                               selected = "Critical to Readiness"), width=NULL),
                            box(p("Another way to look at how the services can be ranked by criticality is by the percentage of 
                                  each service's responses which fell within the high criticality or low criticality ranges of the matrix."),
                                p("The ", strong("percentage of critical responses"), "refers to the number of responses that fell within
                                  the", strong("critical window"), "(i.e scores of 1,1 through 2,2) out of all responses for a particular service."),
                                p("The ", strong("percentage of non-critical responses"), "refers to the number of responses that fell within the ",
                                  strong("non-critical window"), "(i.e scores of 4,4 through 5,5) out of all responses for that service."),
                              background='light-blue', width=NULL
                              )),
                            column(8, plotlyOutput("MostandLeast")), status='primary', collapsible=TRUE, width=NULL))))
  
  ),

#############################################################################################
  #Content for the Demographic Insights Tab
  tabItem(tabName = "demographics",
          h2("Demographic Insights Content")
  ),
  #Content for the Raw Data Tab
  tabItem(tabName = "data",
          h2("Raw Data Content")
  ),
  #Content for the References Tab
  tabItem(tabName = "references",
          h2("References")
  )
 )
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)