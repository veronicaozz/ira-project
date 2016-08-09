library(shiny)
library(plotly)
library(shinydashboard)
library(scales)


setwd("C:/Users/veronica.m.osborn/Desktop/Veronica - ISS Services Project")
isshist = read.csv("issforhistogram.csv", stringsAsFactors=FALSE)
isslevels = read.csv("issforcritlevels2.csv")
riskflip = read.csv("riskflip.csv", stringsAsFactors=FALSE)
riskfeed = read.csv("riskfeed.csv", stringsAsFactors=FALSE)
comments = read.csv("soldiercomments.csv", stringsAsFactors=FALSE, allowEscapes=FALSE)
impactdata = read.csv("impactlevels.csv", stringsAsFactors=FALSE)
triplefins = read.csv("triplefinancials.csv", stringsAsFactors=FALSE)

header <- dashboardHeader(title = "Installation Readiness")

choicelist = list(
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
  "906: Battle Command Training Center" = "ISS906",
  "701: Communication" = "ISS701",
  "703: Information Assurance"= "ISS703",
  "700: Automation" = "ISS700",
  "300: Clothing and Equipment" = "ISS300",
  "301: Retail Supply" = "ISS301",
  "302: Asset Management" = "ISS302",
  "303: Military Clothing and Sales" = "ISS303",
  "304: Laundry and Dry-Cleaning Services" = "ISS304",
  "305: Food Services" = "ISS305",
  "306: Material Support Maintenance" = "ISS306",
  "307: Transportation Services - NTVs" = "ISS307",
  "308: Transportation Services - Personal Property" = "ISS308",
  "309: Supply and Storage" = "ISS309")

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
############################################################################################

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
                            choices = choicelist,
                                  selected = "ISS100"), 
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
                    tabPanel("Compare ISS",
                             h3(("Compare Risk Levels for ISSs"), align='center'),
                             box(fluidRow(h4(("Compare Multiple Installation Services"), align='center'), background='aqua', width=NULL),
                               fluidRow(column(5, box(h4(strong("ISS Comparison"), align='center'),
                                           p("Compare individual installation services and their associated
                                             levels of impact on readiness (criticality) and the size of both their deficit-to-requirement
                                             and annual deficit for each fiscal year from 2010-2021."), background='aqua', width=NULL),
                                          box(h4(("Make ISS Selection Below"), align='center'), background='aqua', width=NULL),
                                           box(checkboxGroupInput("checkcomp", 
                                            label = h5(strong("Select Installation Service(s)")), 
                                            choices = choicelist,
                                            selected = "ISS100"), width = NULL, collapsible=TRUE)),                                          
                                      column(7, box(plotlyOutput("comparerisk"), width=NULL))),
                                    collapsible=TRUE, status='info', width=NULL))
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
                  
          h3("Individual Assessment of ISS Financial Deficits", align='center'),
             fluidRow(column(6, 
                      fluidRow(column(6, box(selectInput("selectF", label = h4(strong("Select ISS")), 
                                      choices = choicelist,
                                    selected = "ISS100"), width = NULL, background='light-blue'),
                                    box(h4(strong("Analyzing Financial Data"), align='center'),
                                      p("Data was obtained from the eProbe database for Army Program Elements (APEs)
                                        associated with 65 out of 71 ISSs. The majority of APEs and their corresponding 
                                        ISSs were analyzed individually. Services with combined APE data
                                        (102-105, 115-116, 118, 253-254, 300, 302, 309, 901-902) were analyzed
                                        conservatively on a case-by-case basis."), 
                                      width=NULL, status='primary')),
                               
                             column(6, box(selectInput("minifinyears",
                                             label=h4(strong("Select Year"), align='center'),
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
                                             selected="Y2010"), width=NULL, background='light-blue'), 
                                    
                            valueBoxOutput("Def16Box", width = NULL),
                            valueBoxOutput("Pred21Box", width = NULL)))),  
                      column(6, box(plotlyOutput("ISSPlotF", height=350), width=NULL, status='primary'))),
          fluidRow(column(6, offset=1, box(plotlyOutput("minifins", height=350), width=NULL, status='primary')),
                          column(4, valueBoxOutput("yearlyfund", width=NULL),
                   valueBoxOutput("yearlyreq", width=NULL),
                   valueBoxOutput("yearlydef", width=NULL)))),
          
          
          
          
          tabPanel("Compare", h3("Comparison of ISS Financial Deficits"),
                   fluidRow(column(width = 4, box(checkboxGroupInput("checkboxF", 
                         label = h4("Select Installation Service(s)"), 
                                  choices = choicelist,
                             selected = "ISS100"), width = NULL, status='warning', collapsible=TRUE)),
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
          
          h3("Individual Assessment of ISS Criticality Levels", align='center'),
          fluidRow(column(width = 4, box(selectInput("selectC", label = h4("Select Installation Service"), 
                                   choices = choicelist,
                                   selected = "ISS100"), width = NULL, status="danger"),
          valueBoxOutput("HighCritBox", width = NULL),
          valueBoxOutput("LowCritBox", width = NULL),                
          img(src="impactscores.png", height = 330, width = 325, 
              style="display: block; margin-left: auto; margin-right: auto;")),
          column(width = 8, box(plotlyOutput("ISSPlot"), width=NULL, status='danger'), 
                 fluidRow(column(6, 
                          box(h4(strong("Level of Impact to Readiness"), align='center'),
                              h5("Average Score", align='center'),
                                 flexdashboard::gaugeOutput("issgauge", height=125), 
                              box(h5("Averages range from a minimum score of 4.13 and a maximum
                                score of 8.53 from the survey data.", align='center'), background='red', width=NULL),
                              width=NULL, status='danger')),
                 column(6, box(tabBox(
                          title = textOutput("Current"),
                          # The id lets us use input$tabset1 on the server to find the current tab
                          id = "tabcomment", width=NULL,
                          tabPanel("Positive", 
                                   h5(strong("Positive Soldier Commentary:"), align='center'),
                                   h5(textOutput("Positives"))),
                          tabPanel("Negative", 
                                   h5(strong("Negative Soldier Commentary: "), align='center'),
                                   h5(textOutput("Negatives")))), width=NULL, status='danger'))
          )))),
          
          tabPanel("Compare", 
                   h3("Comparison of ISS Criticality Levels", align='center'),
                   fluidRow(box(h4("By Level of Impact to Readiness", align='center'), background='teal', width=NULL)),
                     fluidRow(column(4, box(plotlyOutput("highimpact", height=350), status='danger', width=NULL), 
                              box(plotlyOutput("modimpact", height=350), status='warning',width=NULL)),
                              
                              column(4, box(plotlyOutput("modhighimpact", height=350),  status='warning', width=NULL), 
                              box(plotlyOutput("lowimpact", height=350),  status='success', width=NULL)),
                              
                              column(4, 
                              box(selectInput("impactchoice", label=h4("Find an ISS's Level of Impact to Readiness", align='center'), choice=choicelist,
                                                       selected="ISS100"), 
                                  h5(strong(textOutput("selectedimpact")), align='center'),
                                width=NULL, background='teal'),
                                     
                             box(h4(strong("Services with Highest Impact to Readiness"), align='center'),
                                p(em("There are 18 installation services which fall into the top 25% and
                                     have average criticality levels greater than 7.34.")),
                                 p('ISS 800: Military Personnel Services'),
                                 p('ISS 252: Child, Youth, and School Services'),
                                 p('ISS 500: Electrical Services'),
                                 p('ISS 501: Heating/Cooling Services'),
                                 p('ISS 502: Water Services'),
                                 p('ISS 503: Waste Water Services'),
                                 p('ISS 504: Other Utility Services'),
                                 p('ISS 401: Fire and Emergency Response Service'),
                                 p('ISS 600: Physical Security'),
                                 p('ISS 601: Law Enforcement Services'),
                                 p('ISS 603: Installation Security Program Mgmt Support'),
                                 p('ISS 604: Emergency Management'),
                                 p('ISS 900: Airfield Operations'),
                                 p('ISS 901: Mobilization and Deployment Support'),
                                 p('ISS 701: Communication Systems and System Support'),
                                 p('ISS 703: Information Assurance'),
                                 p('ISS 300: Clothing and Equipment'),
                                 p('ISS 306: Material Support Maintenance'), status='danger', 
                                 collapsible=TRUE, width=NULL),
                                     
                              box(h4(strong("Services with Lowest Impact to Readiness"),align='center'),
                                     p(em("There are 13 installation services which fall into the bottom 25% and
                                          have average criticality levels less than 5.2.")),
                                     p('ISS 106: Religious Support'),
                                     p('ISS 107: Public Affairs'),
                                     p('ISS 109: Equal Employment Opportunity (EEO)'),
                                     p('ISS 111: Internal Review'),
                                     p('ISS 254: Business Operations'),
                                     p('ISS 402: Custodial Services'),
                                     p('ISS 404: Maintenance - Grounds'),
                                     p('ISS 408: Snow, Ice, and Sand Removal'),
                                     p('ISS 506: Conservation Services'),
                                     p('ISS 507: Pollution Prevention Services'),
                                     p('ISS 510: Pest Management'),
                                     p('ISS 702: Multimedia/Visual Information Processes'),
                                     p('ISS 304: Laundry and Dry-Cleaning Services'), status='success',
                                     collapsible=TRUE, width=NULL))),

                   fluidRow(box(h4("By Percentage of Critical-Zone Ratings", align='center'), background='orange', width=NULL)),
                   fluidRow(box(
                     column(4, box(radioButtons("radioML", label = h4("Sorting Options"),
                                                               choices = list("Sort by Most Critical" = "Critical to Readiness", 
                                                                              "Sort by Least Critical" = "Non-Critical to Readiness"), 
                                                               selected = "Critical to Readiness"), width=NULL),
                            box(p("Another way to look at how the services can be ranked by criticality is by the percentage of 
                                  each service's responses which fell within the high criticality or low criticality zones of the matrix."),
                                p("The ", strong("percentage of critical responses"), "refers to the number of responses that fell within
                                  the", strong("critical window"), "(i.e scores of 1,1 through 2,2) out of all responses for a particular service."),
                                p("The ", strong("percentage of non-critical responses"), "refers to the number of responses that fell within the ",
                                  strong("non-critical window"), "(i.e scores of 4,4 through 5,5) out of all responses for that service."),
                              background='orange', width=NULL)),
                            column(8, plotlyOutput("MostandLeast")), status='warning', collapsible=TRUE, width=NULL))))
  
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