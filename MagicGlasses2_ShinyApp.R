#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# setup ####
mg2 = "./"

packages = c(
 'base' , 'CausalImpact' , 'cowplot' , 'data.tree' , 'dplyr' , 'DT' , 'dygraphs' , 
 'fable' , 'fable.prophet' , 'fabletools' , 'feasts' , 'forecast' , 'fpp3' , 
 'furrr' , 'future' , 'GGally' , 'ggExtra' , 'gtable' , 'HDInterval' , 'hrbrthemes' , 
 'htmltools' , 'httr' , 'igraph' , 'jsonlite' , 'lubridate' , 'magrittr' , 'mapview' , 
 'officer' , 'openxlsx' , 'pacman' , 'patchwork' , 'plotly' , 'progress' , 'progressr' , 
 'prophet' , 'purrr' , 'RColorBrewer' , 'readxl' , 'renv' , 'rlang' , 'rmarkdown' , 
 'rsconnect' , 'rvg' , 'scales' , 'sf' , 'shinybusy' , 'slider' , 'sugrrants' , 
 'tibbletime' , 'tictoc' , 'tidyfast' , 'tidytable' , 'tidyverse' , 'tsbox' , 'tsibble' , 
 'zoo'
)

pacman::p_load( packages , character.only = TRUE )

options("menu.graphics" = FALSE)
rstudioapi::writeRStudioPreference("console_max_lines", 2000L )


# library(shiny)
# 
# library(conflicted) # For diagnosing function name conflicts
# library(tidyverse)
# library( stringr )
# library(data.table) # for clean install, run from clean session: 
# # remove.packages("data.table")
# # install.packages("data.table", type = "source",
# # repos = "https://Rdatatable.gitlab.io/data.table")
# library( table.express )
# 
# library( flexdashboard )
# # library( bslib )
# library( shiny)
# library( shinyjs)
# library( shinyWidgets )
# # library( shinydashboard)
# library( shinythemes )
# library( shinyBS)
# library( shinyLP)
# library( shinyFiles )
# library( rstudioapi )
# library(  leaflet)
# library( progressr )
# library( RColorBrewer)
# library( plotly)
# library( cowplot )
# library( ggrepel )
# library( ragg )
# library( scales)
# library( lemon ) # for reposition_legend() 
# # if ( 'summarytools' %in% installed.packages() )  library( summarytools ) # invokes Xquartz on mac.
# # library( describer )
# 
# # library( tidyfast )
# # library( tidytable )
# # library(googleVis)
# 
# library( zoo )
# library( knitr)
# library( rlang)
# library( stringi)
# library( tidyselect)
# library( geojsonsf)
# library( geojsonio)
# library( jsonlite)
# library( httr)
# library( curl)
# library( assertthat)
# 
# library(futile.logger)
# # library(utils)
# library(DT)
# # library(textutils)
# library(readxl)
# library(openxlsx)
# # library(anytime)
# library(lubridate)
# library( tictoc )
# 
# library(ipc)
# library(future)
# library( future.apply )
# library(furrr)
# library( purrr )
# library(promises)
# 
# library( data.tree )
# library( igraph )
# 
# # library( knitrProgressBar)
# # library( progressr )
# library( progress )
# library(sf)
# # library( mapview )
# library( leaflet )
# library( leaflegend )
# library( crosstalk )
# # library(rmapshaper)
# 
# library( tsibble  )
# library( fable )
# library( fabletools )
# library( feasts )
# library( fable.prophet )
# # library( Metrics )
# library( forecast ) 


# older package that may not be able to install on some machines (e.g. Mac ARM)
# if ( 'fable.bsts' %in% installed.packages() ) library( fable.bsts )

# busy spinner
# library(shinybusy)

options(shiny.trace=FALSE)
options(shiny.reactlog=FALSE)
knitr::opts_chunk$set(echo = FALSE)

# add_busy_spinner(spin = "fading-circle", position = "bottom-right")

source( paste0( mg2 , "ingest_formula_data.R") ) 
source( paste0( mg2 , 'TS_Modeling_Functions.R') )
source( paste0( mg2 , "Summary_TS.R") ) # new version of summary_ts()
source( paste0( mg2 , 'TS_model_outlier_impute.R') ) # model_ts()
source( paste0( mg2 , 'Deviation_Expected_Functions.R') )
# source( 'theme_ppt.R')
# source( 'clean_ts.r' )
source( paste0( mg2 , 'DToptions.R') )
source( paste0( mg2 , 'model_ts2.R' ) )
source( paste0( mg2 , 'api_data.r' ) )
source( paste0( mg2 , 'dqa.r' ) )
source( paste0( mg2 , 'Cleaning.R' ) )
source( paste0( mg2 , 'prepareDataset.R' ) )
source( paste0( mg2 , 'cleaning_functions.R' ) )
source( paste0( mg2 , "data Functions.R" ) )
source( paste0( mg2 , "dqa_functions.R" ) )


# source( "key.mpe.distribution.R" )
# source( "pre_mable_fit.R")

# source( 'API.r' )

Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

Week_Year = function( x ){ tsibble::yearweek( x) }

options(dplyr.summarise.inform = FALSE)

useShinyjs()   # Set up shinyjs

# Modules ####

source(  paste0( mg2, 'directory_widget.R' ) )
source(  paste0( mg2, 'login_widget.R' ) )
source(  paste0( mg2, 'metadata_widget.R' ) )
source(  paste0( mg2, 'data_widget.r' ) )
source(  paste0( mg2, 'data_request_widget.R' ) )
source(  paste0( mg2, 'formula_widget.r' ) )
source(  paste0( mg2, 'dqa_widget.R' ) )
source(  paste0( mg2, 'reporting_widget_app.r' ) )
source(  paste0( mg2, 'cleaning_widget.r' ) )
source(  paste0( mg2, 'evaluation_widget.R' ) )

options(shiny.trace=FALSE)
options(shiny.reactlog=FALSE)

# add_busy_spinner(spin = "fading-circle", position = "bottom-right")

# Avoid naming conflicts for functions found in more than 1 package...
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("week", "lubridate")
conflict_prefer("runExample", "shiny")
conflict_prefer(":=", "rlang")
conflict_prefer( "validate", "jsonlite" )
conflicts_prefer( base::match )
conflicts_prefer( stats::mad )
conflicts_prefer( stats::sd )
conflicts_prefer( purrr::map )
conflicts_prefer( shiny::isolate )


# Define UI  ####
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "yeti"),
    # shinythemes::themeSelector(),
    
    setBackgroundColor( color = c( "#F5F5F5" ) ),

  
    # Application title
    titlePanel(h1("Magic Glasses 2" , style='background-color:#61A1FA;padding-left: 15px' )),
    
    navlistPanel( widths = c(1, 11) ,
    # tabsetPanel(type = "tabs",
                tabPanel( "Welcome" ,
                           br() ,    
                           h2('An epidemiological look at DHIS2 data') ,
                           br() , 
                           h4( "The goal is to make the analysis of routine data accessible, transparent, and repeatable." ) ,
                           br() , 
                           p( "The layout follows a research path using the pages (navigation at left) to ") ,
                           p( "- Understand which data are available " , span("(Metadata)" , style = "color:blue" ) ) ,
                           p( "- Request data", span( "(Data)" , style = "color:blue" ) ), 
                           p( "- Get overview of data Quality " , span( "(DQA)" , style = "color:blue" ) ) ,
                           p( "- Adjusting for reporting bias " , span( "(Reporting)" , style = "color:blue" ) ) ,
                           p( "- Scan for outliers " ,  span( "(Outliers)" , style = "color:blue" ) ), 
                           p( "- Evaluate trends and estimate intervention effectiveness ",
                           span( "(Evaluation)" , style = "color:blue" ) ), 
                           br() ,
                           p( "(Note: The layout of each page depends on your browser. 
                              You can adjust screen layout and text size by pressing ctrl- or ctrl+)" ),
                          p( "Ver ( December 11, 2023 )" )
                          ) ,
                          
                tabPanel( "Setup" ,
                      
                         fluidPage(
                           fluidRow(
                             column( 6 , directory_widget_ui( "directory1" ) ) ,
                             column( 6 , login_widget_ui( "login1" ) ) 
                             )
                           )
                         ),

                tabPanel("Metadata", metadata_widget_ui( "metadata1" ) ) ,

                tabPanel("Data",

                         fluidPage(
                           fluidRow(
                             column( 6 , 
                                     fluidRow( column( 11 , data_widget_ui( "data1" ) ) ),
                                     fluidRow( column( 11 , data_request_widget_ui( "data_request1" ) ))  
                                     )  ,
                             column( 6,  formula_widget_ui( "formula1" ) ) 
                             )
                           )
                         ),
                tabPanel("DQA", dqa_widget_ui( "dqa1" )) ,
                tabPanel("Reporting", reporting_widget_ui( "reporting1" )) ,
                tabPanel("Outliers", cleaning_widget_ui( "cleaning1" ) ) ,
                tabPanel("Evaluation", evaluation_widget_ui( "evaluation1" ) )
    )
    
)


# Define server  ####
server <- function(input, output, session ) {
    shinyjs::addClass(id = "menus", class = "navbar-right") 
  
    session$onSessionEnded( function() { stopApp()  })

    directory_widget_output = directory_widget_server( "directory1" )

    login_widget_output = login_widget_server( "login1" ,  directory_widget_output = directory_widget_output )

    metadata_widget_output =  metadata_widget_server( "metadata1" ,
                              login_widget_output  =  login_widget_output ,
                              directory_widget_output = directory_widget_output
      )

    data1_Widget_output =  data_widget_server( "data1" ,
                      directory_widget_output = directory_widget_output ,
                      metadata_widget_output = metadata_widget_output  ,
                      data_request_output = data_request_output
                      )

    data_request_output = data_request_widget_server( "data_request1" ,
                                  loginDetails  =  login_widget_output ,
                                  dataDirectory = directory_widget_output ,
                                  metadata_widget_output = metadata_widget_output ,
                                  data_widget_output = data1_Widget_output
      )

    formula1_Widget_output = formula_widget_server( "formula1" ,
                             metadata_widget_output = metadata_widget_output ,
                             data_Widget_output = data1_Widget_output ,
                             directory_widget_output = directory_widget_output)

    dqa_widget_output = dqa_widget_server( "dqa1" ,
                         directory_widget_output = directory_widget_output ,
                         metadata_widget_output = metadata_widget_output ,
                         data_widget_output = data1_Widget_output ,
                         reporting_widget_output = reporting_widget_output ,
                         cleaning_widget_output = cleaning_widget_output )

    reporting_widget_output =  reporting_widget_server( "reporting1" ,
                               dataDirectory = directory_widget_output ,
                               metadata_widget_output = metadata_widget_output ,
                               data_widget_output = data1_Widget_output ,
                               cleaning_widget_output = cleaning_widget_output )

    cleaning_widget_output =  cleaning_widget_server( "cleaning1" ,
                              directory_widget_output = directory_widget_output ,
                              metadata_widget_output = metadata_widget_output ,
                              data_widget_output = data1_Widget_output ,
                              reporting_widget_output = reporting_widget_output
      )

    evaluation_widget_output = evaluation_widget_server( "evaluation1" ,
                                directory_widget_output = directory_widget_output ,
                                metadata_widget_output = metadata_widget_output ,
                                data_widget_output = data1_Widget_output ,
                                reporting_widget_output = reporting_widget_output ,
                                cleaning_widget_output = cleaning_widget_output )

}

# Run the application 
shinyApp(ui = ui, server = server)
