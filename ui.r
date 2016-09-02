library(shiny)
library(shinydashboard)
library(plotly)
library(scales)
library(data.table)
library(dplyr)
#Install the above packages, as well as 'gdata', 'flexdashboard'

#setwd("C:/Users/Veronica/Desktop/ISS Fake Final") #CHOOSE YOUR WORKING DIRECTORY
isshist = read.csv("issforhistogram2.csv", stringsAsFactors=FALSE)
isslevels = read.csv("issforcritlevels2.csv")
riskflip = read.csv("riskflip.csv", stringsAsFactors=FALSE)
riskfeed = read.csv("riskfeed.csv", stringsAsFactors=FALSE)
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

sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("fa fa-home")),
      menuItem("Methodology", tabName = "methodology", icon = icon("fa fa-briefcase")),
      menuItem("Readiness Impact Levels", tabName = "criticality", icon = icon("fa fa-bar-chart")),
      menuItem("Financial Deficits", tabName = "deficits", icon = icon("fa fa-line-chart")),
      menuItem("Risk Assessment", tabName = "risk", icon = icon("fa fa-bomb")),
      menuItem("Demographics", tabName = "demographics", icon = icon("fa fa-male")),
      menuItem("Raw Data", tabName = "data", icon = icon("fa fa-file-text"))
      #menuItem("References", tabName = "references", icon = icon("fa fa-book"))
      #menuItem("Key Insights", tabName = "insights", icon = icon("fa fa-key")),
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
############################################################################################
############################################################################################

      tabItem(tabName = "home",
              h2("Installation Readiness Analysis Project", align = "center"),
              fluidRow(column(6, offset=1, 
                              
                    box(h4(strong("CSA #1 Priority"), align='center'),
                    p('\"Our fundamental task is like not other - it is to win in
                      the unforgiving crucible of ground combat. We must ensure the Army remains                    
                      ready as the world\'s premier combat force. Readiness for ground combat is - and
                      will remain - the U.S. Army\'s No. 1 priority. Our most values assets, indeed, the
                      nation\'s most valued assets, are our Soldiers and our solemn commitment must 
                      always be to never send them into harm\'s way untrained, poorly led,
                      undermanned or with less than the best equipment we can provide. Readiness is
                      No. 1, and there is no other No. 1.\"'), h5(em("- General Milley, CSA"), align='center'),
                    solidHeader=TRUE, background='blue',width=NULL),
                              
                    box(h4(strong("Background"), align="center"),
                    p("The Principal Deputy Assistant Secretary of the Army for Installations, 
                      Energy & Environment (DASA-IE&E) has requested CAA assist them in prioritizing 
                      installations services critical to Army readiness to avoid potential underfunding 
                      of installations in future Program Objective Memorandums (POMs)."), 
                    solidHeader=TRUE, background='navy',width=NULL),
                    
                    box(h4(strong("Purpose"), align="center"),
                      p("Provide ASA-IE&E with an analytical overview of the direct and indirect 
                      linkages between installation and service programs and the U.S. Army's readiness 
                      posture, and show the potential impact of funding below requirements levels. 
                      The analysis will serve to assist future POM planning and funding."), 
                    solidHeader=TRUE, background='light-blue',width=NULL),
                    box(h4(strong("Project Lead:"), " MAJ Vincent Boncich", align='center'),
                        h5("App created by: Veronica Osborn, intern, and Claire Fisher, PMF.", align='center'), 
                        h5("Resource Analysis Division, Center for Army Analysis", align='center'),
                        h5(em("August 23, 2016"), align='center'), width=NULL, background='blue')),
                    
                    column(4, 
                           tabBox(
                             title = "Objectives",
                             # The id lets us use input$tabset1 on the server to find the current tab
                             id = "tabset1", width=NULL,
                             tabPanel("Impact on Readiness", h5(strong("With regards to ISS Readiness Impact Levels:"), align='center'),
                                      p("1. Determine installation and service program linkages to Readiness."), 
                                      p("2. Develop a proposed prioritization matrix to help articulate the 
                                      direct and indirect support to U.S. Army readiness."), 
                                      p("3. Develop methodology to measure the readiness impact levels of facilities on an installation."), 
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
                                      p("The end goal is to link service readiness impact level to service funding in order to develop insights 
                                      on what services are at the highest risk to mission readiness if left under-funded."),
                                      p("This can be achieved through a comparison of II PEG 
                                      funding/requirements to assess high/low priority installation services that 
                                      support unit readiness which have been under-funded in the past."),
                                      p("Analysis will provide data on where it is possible to divest (low priority services) and 
                                      which services require further investment (high priority services) during 
                                      future POM planning."))
                    ),
                   img(src="armyanalysis.png", height = 250, width = 250,
                       style="display: block; margin-left: auto; margin-right: auto;")
              ))),
      
############################################################################################
############################################################################################

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
                              p("Risk is determined as a combination of a service\'s", strong(" level of impact
                                mission readiness"), "and the service\'s", strong("level of under-funding.")),
                              p("An ISS\'s (x,y) risk coordinates are determined by its average readiness impact level, determined
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
                           span(em("(high readiness impact level with large deficit-to-requirement ratio)"), style='color:red'), " while ", strong("low risk"), 
                           "services lie in the top left quadrant on the plot ", span(em("(low readiness impact level with small
                                  deficit-to-requirement ratio)."), style = "color:green")), status='warning', width=NULL))
    )),
                    tabPanel("Compare ISS",
                             h3(("Compare Risk Levels for ISSs"), align='center'),
                             box(fluidRow(column(5, box(h4(strong("ISS Comparison"), align='center'),
                                           p("Compare individual installation services and their associated
                                             levels of impact on readiness and the size of both their deficit-to-requirement
                                             and annual deficit for each fiscal year from 2010-2021."), background='aqua', width=NULL),
                                          box(h4(("Make ISS Selection Below"), align='center'), background='aqua', width=NULL),
                                           box(radioButtons("riskradio1", label = h4("Filter Options"),
                                                             choices = list("By Command" = 1,
                                                                            "By Impact to Readiness" = 2,
                                                                            "Custom" = 3), 
                                                             selected = 1), 
                                                uiOutput("riskfilters"),
                                                box(uiOutput("riskcheckbox"), status='warning',
                                                    width=NULL, collapsible=TRUE), status='primary', width=NULL)),                                          
                                      column(7, box(plotlyOutput("comparerisk"), status='info', width=NULL))),
                                    collapsible=TRUE, status='info', width=NULL))
      )),

############################################################################################
############################################################################################

  tabItem(tabName = "methodology",
          navbarPage("Methodology",
                     tabPanel("Readiness Impact Levels",
          box(tabBox(
            title = "Readiness Impact Methodology",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabcomment", width=NULL,
            tabPanel("Questionnaire / Research Design",
                     fluidRow(column(5, h4("Data Collection", align='center'),
                                     box(
                       p("Data was collected upon traveling to four fort locations to collect 
                         data from U.S. Army senior leaders on how various installation 
                         services impact and enable unit readiness. The visited sites were Fort Bragg 
                         (127th Engineer Battalion), Fort Eustis (597th Transportation 
                         BDE), Fort Campbell (1-187 Infantry Battalion), and Fort Hood 
                         (2nd Armored Brigade Combat Team & 3rd Cavalry Regiment)."),
                       p("At each location, enlisted and non-enlisted officers were gathered in a 
                         classroom setting before introducing a questionnaire. Questionnaires 
                         (right) outlined a comprehensive list of
                         seventy-one individual Installation Service Standards (ISS) and 
                         required participating Army senior leaders to rank services on two criteria
                         measured on a 5-point scale."),
                       p("ISSs were ranked by:", strong("
                         Interruptibility"), ", which indicated how long an installation service 
                         could be interrupted without adverse readiness impacts, and ", 
                         strong("Replaceability"), ", which indicated how difficult it would be to replace 
                         or replicate an installation service with another provider from any source."),
                       background='light-blue', width=NULL),
                       box(p("Besides the rankings of ISSs,
                             demographic and unit-specific information about respondents was recorded via 
                             questionnaire response, including rank, race, gender, age, number of dependents, 
                             housing location, MOS, current staff position, and time in current position. 
                             All questionnaires were recorded anonymously and included written commentary
                            regarding soldiers' perception of installation services' impact on readiness."), width=NULL, background='light-blue'),
                       box(p("To minimize subjectivity in scoring and the 
                              resulting impact to the data quality, each questionnaire was 
                              prefaced with a cover sheet detailing the meanings of 
                              the 5-point scale with relation both to interruptibility 
                              and replaceability (bottom right)."), width=NULL, background='light-blue')),
                              column(7,
                                     img(src="servicestandards.png", height = 450, width = 650, 
                                            style="display: block; margin-left: auto; margin-right: auto;"),                                     
                                     img(src="isdiratings.png", height = 300, width = 450, 
                                         style="display: block; margin-left: auto; margin-right: auto;")))
                     
                     ),

            tabPanel("Score Calculations",
                     fluidRow(column(4, h4("Score Assignment", align='center'),
                                     box(p("Interruptibility and replaceability were given equal  
                                           weight in determining impact on readiness. Using the score matrix below, 
                                          Installation Service Dependency Index (ISDI) responses were converted to scores ranging from 2-10.
                                          The aforementioned scores were averaged across all 119 respondents to provide an overall ranking for
                                          the seventy-one ISSs and to determine each service\'s level of impact on readiness."),
                                         p("The ", strong("higher"), " an ISDI score,
                                          the ", strong("more of an impact to readiness"), " the service has. On
                                           the other hand, lower scores mean that the service has less of an impact
                                           on unit readiness."),
                                         width=NULL, background='yellow'),
                              h4("Levels of Impact to Readiness", align='center'),
                                     box(p("Ranges for the four levels of impact to readiness
                                           (high, moderately high, moderate, low) were calculated by breaking up the overall
                                          range of averages (from minimum score to maximum score) into four equal quadrants. 
                                           For exact range details, reference the ", strong ("\'Readiness Impact Levels\'"), " tab."),
                                         width=NULL, background='yellow'),
                              downloadButton('downloadqdata', 'Download Questionnaire Response Data')),
                      column(8, img(src="scoreassignment.png", height = 455, width = 700, 
                                            style="display: block; margin-left: auto; margin-right: auto;"))))), 
            width=NULL)),
          
          tabPanel("Financial Data Analysis",
             box(tabBox(
               title = "Financial Methodology",
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabcomment2", width=NULL,
               tabPanel("Acquiring Financial Data",
                        fluidRow(column(5, offset=1, h4("Data Collection", align='center'),
                        box(
                          p("Financial data was obtained from the Army PROBE database. 
                            In creating a unique ID for every entry of financial data from the
                            Installation Program Evaluation Group (II PEG),
                            a crosswalk was established between Army Program Elements (APE) in the II PEG and their 
                            corresponding ISS; this enabled linkages to past and future requirements/funding 
                            for 65 of the 71 ISS."),
                          p("ISSs 903-906, 803, and 308 could not be matched to APE
                            financial data. The 16 services with shared APEs (102-105, 115-116, 118, 253-254, 300, 
                            302, 309, 504, 512, 901-902) were analyzed conservatively by distributing requirements/funding 
                            across ISSs respectively."),
                          p(strong("Note:"), " Data within the PROBE database was not complete for all ISSs from FY10-FY21."),
                          background='olive', width=NULL),
                        fluidRow(column(6, offset=3,downloadButton('downloadapedata', 'Download APE/ISS Crosswalk Data')))),
                                 column(5, 
                                        valueBoxOutput("TotalFunds", width = NULL),
                                        valueBoxOutput("TotalReqs", width = NULL),
                                        valueBoxOutput("TotalDefs", width = NULL)
                                          ))),
               
               tabPanel("Funding Delta Calculations",
                        fluidRow(column(6, h4("Funding Deltas", align='center'),
                                        box(p(strong("Funding deltas"), "are the differences between actual funding received by ISSs
                                              (henceforth referred to as \'funding\') and required funding (henceforth referred
                                              to as \'requirement\')."),
                                            p("A negative funding delta, or deficit, means that the
                                              ISS is under-funded while a positive funding delta means that the ISS was over-funded
                                              within that fiscal year. Funding deltas of value 0 indicate that the requirement was met."),
                                            p("Because not all funding and requirements were provided for ISSs, assumptions
                                              regarding the true funding delta were made in analysis using the table at right
                                              for various F/R (funding-requirement) scenarios."),
                                            p("For a downloadable version of each ISS's calculated funding, requirement, and delta data for each fiscal
                                              year, reference the ", strong("\'Raw Data\' tab.")),
                                            width=NULL, background='olive')),
                                 
                        column(6, img(src="frscenarios.png", height = 349, width = 470, 
                                                         style="display: block; margin-left: auto; margin-right: auto;"))))), 
               width=NULL))
          
          )),
  
############################################################################################
############################################################################################

  tabItem(tabName = "deficits",
          
          navbarPage("Financial Deficits",
                     tabPanel("Individual",
                  
          h3("Individual Assessment of ISS Funding Deltas", align='center'),
             fluidRow(column(6, 
                      fluidRow(column(6, box(selectInput("selectF", label = h4(strong("Select ISS")), 
                                      choices = choicelist,
                                    selected = "ISS100"), width = NULL, background='light-blue'),
                                    box(selectInput("minifinyears",
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
                                                    selected="Y2010"), width=NULL, background='light-blue')),
                               
                             column(6, valueBoxOutput("Def16Box", width = NULL),
                            valueBoxOutput("Pred21Box", width = NULL)))),  
                      column(6, box(plotlyOutput("ISSPlotF", height=275), width=NULL, status='primary'))),
          fluidRow(column(4, 
                  valueBoxOutput("yearlyfund", width=NULL),
                   valueBoxOutput("yearlyreq", width=NULL),
                   valueBoxOutput("yearlydef", width=NULL)),
                  column(4, box(plotlyOutput("minifins", height=350), width=NULL, status='primary')),
                  column(4, fluidRow(box(plotlyOutput("minifrdfunds2", height=150), status='primary', width=NULL)),
                         fluidRow(box(plotlyOutput("minifrdreqs2", height=150), status='primary', width=NULL))))
          
          ),
          
          tabPanel("Deficit Comparison", h3("Comparison of ISS Funding Deltas"),
                   fluidRow(column(width = 4, box(radioButtons("fundradio1", label = h4("Filter Options"),
                                                choices = list("By Command" = 1,
                                                "By Impact to Readiness" = 2,
                                                "Custom" = 3), 
                                                              selected = 1), 
                                                  uiOutput("fund1filters"),
                                                box(uiOutput("fund2checkbox"), status='warning',
                                                    width=NULL, collapsible=TRUE), status='primary', width=NULL)),
                             column(width=7, 
                                    box(plotlyOutput("ISSLineF"), collapsible=TRUE, status='primary', width=NULL),
                                    box(h5(strong("Note: "), "All colors have been arbitrarily chosen by the program.", align='center'), 
                                        background='blue', width=NULL),
                                    box(plotlyOutput("ISSLineF2"), collapsible=TRUE, status='primary', width=NULL))
                   )
  ),
  
          tabPanel("Compare Funds/Reqs",
                   h3("Comparison of Funding and Requirements for All Services", align='center'),
                   fluidRow(column(3, 
                            box(selectInput("frdyear",
                                            label=h4("Select Year", align='center'),
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
                                            selected="Y2010"),background='navy',  width=NULL)),
                                  column(6, box(h4(strong("Where is the Money Going?"), align='center'),
                                       h5("An analysis of the yearly distribution of funds answers the questions: ", align='center'),
                                       h5(em("What services are currently the most funded?"), align='center'),
                                       h5(em("Do services receive similar levels of funding each year?"), align='center'),
                                       h5(em("Which services have the largest deficits (i.e. are most under-funded)?"), align='center'),
                                       h5("Further information regarding which APEs were included in the individual
                         installation service financial analysis is available under ", strong("\'Financial Methodology\'."), align='center'),
                                       width=NULL, background='purple')),
              column(3, box(selectInput("frdservice", label = h4("Select Installation Service"), 
                                          choices = choicelist,
                                          selected = "ISS100"), width = NULL, background='navy'))),
                    fluidRow(column(8, box(plotlyOutput("frdfunds", height=250), width=NULL, status='success', collapsible=TRUE),
                                   box(plotlyOutput("frdreqs", height=250), width=NULL, status='danger', collapsible=TRUE),
                                   box(plotlyOutput("frddefs", height=250), width=NULL, status='info', collapsible=TRUE)),
                            column(4, 
                                   box(plotlyOutput("minifrdfunds", height=250), width=NULL, status='success', collapsible=TRUE),
                                   box(plotlyOutput("minifrdreqs", height=250), width=NULL, status='danger', collapsible=TRUE),
                                   box(plotlyOutput("minifrddefs", height=250), width=NULL, status='info', collapsible=TRUE)
)))
  )),

############################################################################################
############################################################################################

  tabItem(tabName = "criticality",
          
          navbarPage("Readiness Impact Levels",
                     tabPanel("Individual",
          
          h3("Individual Assessment of ISS Readiness Impact Levels", align='center'),
          fluidRow(column(width = 4, box(selectInput("selectC", label = h4("Select Installation Service"), 
                                   choices = choicelist,
                                   selected = "ISS100"), width = NULL, status="danger"),
                          box(h4(strong(textOutput("Current2")), align='center'), 
                                 h5(em("Description of Service"), align='center'),
                                      p(textOutput("issdescription")),
                              width=NULL, status='danger'),                        
                          valueBoxOutput("HighCritBox", width = NULL),
                          valueBoxOutput("LowCritBox", width = NULL)),
          column(width = 8, 
                 fluidRow(column(7, box(plotlyOutput("ISSPlot", height=450), width=NULL, status='danger'),
                                 box(tabBox(
                                   title = textOutput("Current"),
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabcomment", width=NULL,
                                   tabPanel("Positive", 
                                            h5(strong("Positive Soldier Commentary:"), align='center'),
                                            h5(htmlOutput("Positives"))),
                                   tabPanel("Negative", 
                                            h5(strong("Negative Soldier Commentary: "), align='center'),
                                            h5(htmlOutput("Negatives")))), width=NULL, status='danger')),
                          column(5, box(h4(strong("Level of Impact to Readiness"), align='center'),
                                        h5("Average Score", align='center'),
                                        flexdashboard::gaugeOutput("issgauge", height=125), 
                                h5("Score in range [4.13, 5.20]: ", br(), span(strong("Low Impact to Readiness"), 
                                                                         style = "color:green"), align='center'),
                                h5("Score in range [5.21, 6.27]: ", br(), span(strong("Moderate Impact to Readiness"), 
                                                                         style = "color:gold"), align='center'),
                                h5("Score in range [6.28, 7.34]: ", br(), span(strong("Moderately High Impact to Readiness"), 
                                                                         style = "color:orange"), align='center'),
                                h5("Score in range [7.35, 8.53]: ", br(), span(strong("High Impact to Readiness"), 
                                                                         style = "color:red"), align='center'),
                                        box(h5("Averages range from a minimum score of 4.13 and a maximum
                                score of 8.53 from the survey data.", align='center'), background='red', width=NULL),
                                        width=NULL, status='danger'),
                                img(src="scorematrix.png", height = 305, width = 300, 
                                              style="display: block; margin-left: auto; margin-right: auto;")))))),
          
          tabPanel("Compare", 
                   h3("Comparison of ISS Readiness Impact Levels", align='center'),
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
                              box(p("The error bars on each of the graphs show the", strong(" overall variability "), "of the questionnaire responses,
                                    representing one standard deviation from the average ISDI score."), width=NULL, status='info'),
                                     
                             box(h4(strong("Services with Highest Impact to Readiness"), align='center'),
                                p(em("There are 18 installation services which fall into the top 25% and
                                     have average readiness impact levels greater than 7.34.")),
                                p('ISS 701: Communication Systems and System Support (8.42)'),
                                p('ISS 401: Fire and Emergency Response Service (8.36)'),
                                p('ISS 502: Water Services (8.08)'),
                                p('ISS 601: Law Enforcement Services (8.01)'),
                                p('ISS 600: Physical Security (8.00)'),
                                p('ISS 503: Waste Water Services (7.99)'),
                                p('ISS 703: Information Assurance (7.97)'),
                                p('ISS 604: Emergency Management (7.87)'),
                                p('ISS 901: Mobilization and Deployment Support (7.70)'),
                                p('ISS 800: Military Personnel Services (7.59)'),
                                p('ISS 500: Electrical Services (7.59)'),
                                p('ISS 300: Clothing and Equipment (7.58)'),
                                p('ISS 501: Heating/Cooling Services (7.55)'),
                                p('ISS 603: Installation Security Program Mgmt Support (7.55)'),
                                p('ISS 504: Other Utility Services (7.52)'),
                                p('ISS 252: Child, Youth, and School Services (7.48)'),
                                p('ISS 306: Material Support Maintenance (7.44)'), 
                                p('ISS 900: Airfield Operations (7.38)'),
                                status='danger', collapsible=TRUE, width=NULL),
       
                              box(h4(strong("Services with Lowest Impact to Readiness"),align='center'),
                                     p(em("There are 13 installation services which fall into the bottom 25% and
                                          have average readiness impact levels less than 5.2.")),
                                  p('ISS 510: Pest Management (5.19)'),
                                  p('ISS 111: Internal Review (5.01)'),
                                  p('ISS 106: Religious Support (4.91)'),
                                  p('ISS 408: Snow, Ice, and Sand Removal (4.81)'),
                                  p('ISS 109: Equal Employment Opportunity (EEO) (4.79)'),
                                  p('ISS 507: Pollution Prevention Services (4.75)'),
                                  p('ISS 402: Custodial Services (4.75)'),
                                  p('ISS 304: Laundry and Dry-Cleaning Services (4.61)'), 
                                  p('ISS 506: Conservation Services (4.43)'),
                                  p('ISS 404: Maintenance - Grounds (4.40)'),
                                  p('ISS 702: Multimedia/Visual Information Processes (4.38)'),
                                  p('ISS 254: Business Operations (4.38)'),
                                  p('ISS 107: Public Affairs (4.20)'), status='success',
                                     collapsible=TRUE, width=NULL))),

                   fluidRow(box(h4("By Percentage of Critical-Zone Ratings", align='center'), background='orange', width=NULL)),
                   fluidRow(box(
                     column(4, box(radioButtons("radioML", label = h4("Sorting Options"),
                                                               choices = list("Sort by Most Critical" = "Critical to Readiness", 
                                                                              "Sort by Least Critical" = "Non-Critical to Readiness"), 
                                                               selected = "Critical to Readiness"), width=NULL),
                            box(p("Another way to look at how the services can be ranked by readiness impact is by the percentage of 
                                  each service's responses which fell within the high readiness impact or low readiness impact zones of the matrix."),
                                p("The ", strong("percentage of critical responses"), "refers to the number of responses that fell within
                                  the", strong("critical window"), "(i.e scores of 1,1 through 2,2) out of all responses for a particular service."),
                                p("The ", strong("percentage of non-critical responses"), "refers to the number of responses that fell within the ",
                                  strong("non-critical window"), "(i.e scores of 4,4 through 5,5) out of all responses for that service."),
                              background='orange', width=NULL)),
                            column(8, plotlyOutput("MostandLeast")), status='warning', collapsible=TRUE, width=NULL))))
  
  ),

############################################################################################
############################################################################################

  tabItem(tabName = "insights",
          h3("Key Insights: Highest Impact Installation Services", align='center'),
          fluidRow(column(5, box(plotlyOutput("keyrisk"), width=NULL, status='danger')),
                   column(7, 
                    fluidRow(column(4, box(h4("INFO", align='center'), background='red', width=NULL)),
                      column(8, box(plotlyOutput("minikey", height=280), width=NULL, status='danger'),
                                    fluidRow(column(6, box(radioButtons("keyDR",
                                                     label=h4("Select Parameter", align='center'),
                                                     choices = list(
                                                       "Delta/Requirement Ratio" = "DR",
                                                       "Funding Delta" = "delta"),
                                                     selected="DR"), status='danger',  width=NULL)),
                                        column(6, box(uiOutput("keyfilters"), status='danger', width=NULL))))))),
          fluidRow(column(4, box(selectInput("underyear",
                                                       label=h4("Select Year (Under-Funding)", align='center'),
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
                                                       selected="Y2010"),width=NULL),
                          box(selectInput("overyear",
                                          label=h4("Select Year (Over-Funding)", align='center'),
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
                                          selected="Y2010"),width=NULL)),
                   column(4, box(plotlyOutput("comunderfunding"), width=NULL)),
                   column(4, box(plotlyOutput("comoverfunding"), width=NULL))
                   )
  ),

############################################################################################
############################################################################################

  tabItem(tabName = "demographics",
          navbarPage("Demographics",
                     tabPanel("Explore",
          fluidRow(column(4, h3("Explore Impact Ratings by Fort", align='center'),
                          box(radioButtons("demosort",
                                               label=h4("Filtering Options", align='center'),
                                               choices = list(
                                                 "All Services" = "all",
                                                 "By Command" = "command",
                                                 "By Impact to Readiness" = "readiness"),
                                               selected="all"),
                              uiOutput("demofortfilters"), width=NULL, background='orange'),
                          box(uiOutput("fortservice2"), width = NULL, background='orange')),
                   column(8, box(plotlyOutput("compareforts"), width=NULL, status='warning'))),
          fluidRow(column(4, h3("Explore Respondent Demographics", align='center'),
                          box(selectInput("demchoices", label = h5("Select Demographic"), 
                                          choices = list("Rank" = 'Rank',
                                                         "Gender" = 'Gender',
                                                         "Living Quarters  (On/Off-Post)" = 'Living Quarters  (On/Off-Post)',
                                                         "Race" = 'Race',
                                                         "Age" = 'Age',
                                                         "Enlisted vs. Officers" = 'Enlisted vs. Officers',
                                                         "Number of Dependents" = "Number of Dependents"),
                                          selected = "Rank"), width = NULL, background='light-blue'),
                          box(selectInput("demservice", label = h5("Select Installation Service"), 
                                          choices = choicelist,
                                          selected = "ISS100"), width = NULL, background='light-blue')),
                   column(8, box(plotlyOutput("demographs"), width=NULL, status='primary'))
                   )))
  ),

############################################################################################
############################################################################################

  tabItem(tabName = "data",
          navbarPage("Raw Data",
                     tabPanel("Financial Data",
          fluidRow(column(3, box(h4("Select Data", align='center'),
                            radioButtons("rawdatasort",
                            label=h4("Filtering Options", align='center'),
                            choices = list(
                              "By Command" = "command",
                              "By Impact to Readiness" = "readiness"),
                            selected="command"), status='success', width=NULL)),
                    column(2, box(uiOutput("rawdatafilters"), background='green', width=NULL)),
                   column(2, offset=5, downloadButton('downloadData', 'Download'))),
          fluidRow(box(tabsetPanel(
                   tabPanel('Funding Deltas',
                            dataTableOutput("deltatable")),
                   tabPanel('D/R Ratios',
                            dataTableOutput("DRtable")),
                   tabPanel('Funding',
                            dataTableOutput("fundtable")),
                   tabPanel('Requirements',
                            dataTableOutput("reqtable"))), width=NULL, status='success'))
  ))),

############################################################################################
############################################################################################

  tabItem(tabName = "references",
          h2("References")
  )
 )
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)
