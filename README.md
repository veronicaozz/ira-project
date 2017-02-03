# ira-project

###To Run App from GitHub:
Install all packages required for application if not previously installed.
```r
install.packages('gdata')
install.packages('flexdashboard')
install.packages('shiny')
install.packages('shinydashboard')
install.packages('plotly')
install.packages('scales')
install.packages('data.table')
install.packages('dplyr')
```
####Note 2/2/2017: 
Because of the updates to Plotly's 4.0 version R library, run this code under Plotly 3.6.0. to preserve functionality of all graphs and visualizations. 
```r
install.packages('devtools')
require(devtools)
install_version("plotly", version = "3.6.0", repos = "http://cran.us.r-project.org")
```
Updated server.R and ui.R files for shiny 1.0.0 (as of 1/12/2017)

In R Console do:
```r
library(shiny)
runGitHub( "ira-project", "veronicaozz") 
```

### Instructions for Running App Locally:
##### Requires: R, RStudio
1. Download zipped folder of .csv files, ui.R, server.R, and .png files.
    - compiledH.csv, finishedcomments2.csv, issforcritlevels2.csv, issforhistogram2.csv
    - riskfeed.csv, riskflip.csv, triplefinancials.csv, ape-iss-match.csv, MostandLeastCrit_Sorted.csv
2. Extract all files into a named folder (e.g. Desktop/ira-project).
3. Within named folder, place all .png files into folder named www (i.e. Desktop/ira-project/www).
4. Open ui.R and server.R files in RStudio.
5. Install all required packages if not previously installed.
  'gdata', 'flexdashboard', 'shiny', 'shinydashboard', 'plotly', 'scales', 'data.table', 'dplyr'
6. Within ui.R and server.R code, set working directory:
```r
setwd("C:/path/to/ira-project/")
```

Finally, Click 'Run App'.

####Please note that the financial data has been randomized to protect private ePROBE database queries.

More information regarding the project is available in the project's wiki, including research design, timeline, methodology, and  objectives.

### CREATOR COMMENTARY:
This was a first-attempt at an R Shiny application and created within a 5-week time-frame. There are definitely aspects of the app that I would have liked to change or add, including:
- an upload feature that allows the client to upload their own ePROBE data pulls (i.e. a pipeline) and have the app coded in a way that makes the process easily replicable for future fiscal years
- a tab purely for insights (such as isolated over-funding and under-funding for each fiscal year to identify where resources can be re-allocated)
- the number of .csv inputs could have been reduced to two .csv files (most of the financial data was managed and manipulated in Excel or Python; more of an effort could have been made to be more efficient with file uploads)

### Developed by: Veronica Osborn
