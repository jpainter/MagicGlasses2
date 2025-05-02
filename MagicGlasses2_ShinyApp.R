#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button (above right).
#

# setup ####
mg2 = "./"

# Define the list of packages ####
packages = c(
 'base' , 
 # 'CausalImpact' , 
 'cowplot' , 'data.tree' , 'DT' , 'dygraphs' , 
 'tidyfast' ,  'tidyverse' , 'stringr' ,
 'tsibble' , 'feasts' , 'forecast' , 'fpp3' , 'tibbletime' ,
 'fable' , 'fabletools' , 'fable.prophet' , 'prophet' ,  "distributional" ,
 'table.express' , 'data.table' , 
 'furrr' , 'future' , 'GGally' , 'ggExtra' , 'gtable' , 'HDInterval' , 'hrbrthemes' , 
 'htmltools' , 'httr' , 'igraph' , 'jsonlite' , 'lubridate' , 'magrittr' , 'mapview' , 
 'officer' , 'openxlsx' , 'pacman' , 'patchwork' , 'plotly' , 'progress' , 'progressr' , 
 'prophet' , 'purrr' , 'RColorBrewer' , 'readxl' , 'renv' , 'rlang' , 'rmarkdown' , 
 'rsconnect' , 'rvg' , 'scales' , 'sf' , 
 'shiny' , "shinyjs" , 'shinybusy' , 'shinyWidgets' , 
 'shinythemes' , 'shinydashboard', 'shinyBS', 'shinyLP', 
 'shinyFiles' , 'shinyalert' , 
 'slider' , 'sugrrants' , 
 'leaflet' ,  'leaflegend' , 'ggrepel' ,
 'tibbletime' , 'tictoc' , 'tsbox' , 
 'zoo', 'conflicted', 'assertthat' , 'stringi' , 
 'digest'
)

# optional packages (from previous versions): forecast, Bolstad, bsts, fable.bsts 
# "gt",  "magick" , "kableExtra" , "tidyfst" ,  "gtsummary" , "flextable" ,
# "comparator" , "fuzzyjoin", "ggthemes",  "maps", "ggspatial", "leaflet.extras",
# "nngeo" , "overlapping" , "fitdistrplus","MLmetrics" ,"coin", "webshot" , "webshot2"
# 'tidytable' 

# cat( 'Recommended packages:' ,  packages )

installed = installed.packages()
if ( ! 'pacman' %in% installed ) install.packages( 'pacman' )
require( pacman )

# if ( ! 'BiocManager' %in% installed ) install.packages( 'BiocManager' )
# BiocManager::install(version = "3.20")

# Load packages dynamically using pacman
pacman::p_load( char = packages, install = TRUE )

x=conflict_scout()
save(x, file = 'conflicted' )

# Conflicts ####
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
conflicts_prefer(base::`%in%`)
conflicts_prefer(purrr::map_df)
conflicts_prefer(dplyr::pull)
conflicts_prefer(dplyr::between)
conflicts_prefer(dplyr::count)
conflicts_prefer(tidytable::map_lgl)
conflicts_prefer(fable.prophet::prophet)
conflicts_prefer(tsibble::index)
# conflicts_prefer(tidytable::ungroup)
conflicts_prefer( purrr::flatten )
# conflicts_prefer(testthat::is_null)
conflicts_prefer(purrr::compose)



# Options ####
options("menu.graphics" = FALSE)
rstudioapi::writeRStudioPreference("console_max_lines", 2000L )
options(future.globals.maxSize = 6 * 1024^3)

options(shiny.trace=FALSE)
options(shiny.reactlog=FALSE)
knitr::opts_chunk$set(echo = FALSE)

# Functions  ####
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
source( paste0( mg2 , 'api_data_function_revision.R') )  # revision October 2024
source( paste0( mg2 , 'dqa.r' ) )
source( paste0( mg2 , 'Cleaning.R' ) )
source( paste0( mg2 , 'prepareDataset.R' ) )
source( paste0( mg2 , 'cleaning_functions.R' ) )
source( paste0( mg2 , "data Functions.R" ) )
source( paste0( mg2 , "dqa_functions.R" ) )
source( paste0( mg2 , "MG2_Forecast_Functions.R" ) )


as.yearmonth = function( date.string , fmt = "%B%Y" ) zoo::as.yearmon( date.string , fmt) %>% yearmonth

knitr::opts_chunk$set(echo = FALSE , warning = FALSE )

get_date_part = function(x){
  x.parts = str_split( x , "_" , simplify = TRUE ) 
  date.part = x.parts[ length( x.parts ) ]
  date = str_split( date.part , fixed(".") , simplify = TRUE )[1] 
  return( ymd( date ))
}

get_file_date = function(x){
  file.info( x )$mtime
}

as.yearmonth = function( date.string , fmt = "%B%Y" ) zoo::as.yearmon( date.string , fmt) %>% yearmonth


knitr::opts_chunk$set(echo = FALSE , warning = FALSE )

if ( ! 'flextable' %in% installed ) install.packages( 'flextable' )
library( flextable ) 
         
# Flextable
set_flextable_defaults(font.size = 11)

# Function to have tables autofit to page
FitFlextableToPage <- function(ft, pgwidth = 6.5 ){
  # set as autofit to make width parameters adjustable
  
  ft_out <- ft %>% autofit()
  
  # set width as function of page width
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  
  return(ft_out)
}

intersect_length = function(x,y ) intersect( x, y) %>% length 

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
source(  paste0( mg2, 'evaluation_widget_2.R' ) )
source(  paste0( mg2, 'regions_widget.R' ) )

options(shiny.trace=FALSE)
options(shiny.reactlog=FALSE)

# add_busy_spinner(spin = "fading-circle", position = "bottom-right")



# Define UI  ####
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "yeti"),
    # shinythemes::themeSelector(),
    
    shinyWidgets::setBackgroundColor( color = c( "#F5F5F5" ) ),

  
    # Application title
    titlePanel(h1("Magic Glasses 2" , style='background-color:#61A1FA;padding-left: 15px' )),
    
    navlistPanel( widths = c(1, 11) , id = "tabs", 

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
                          p( "Ver ( December 5, 202 )" )
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
    
                tabPanel("Regions", regions_widget_ui( "regions1" ) ) ,

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
                tabPanel("Reporting", reporting_widget_ui( "reporting1" ), value = "reporting" ) ,
                tabPanel("Outliers", cleaning_widget_ui( "cleaning1" ) , value = "outliers" ) ,
                tabPanel("Evaluation", evaluation_widget_ui( "evaluation1" ) , value = "evaluation" )
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

    regions_widget_output =  regions_widget_server( "regions1" ,
                              directory_widget_output = directory_widget_output ,
                              metadata_widget_output = metadata_widget_output,
                              data_widget_output = data1_Widget_output
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
                                  data_widget_output = data1_Widget_output ,
                                  regions_widget_output = regions_widget_output
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

    reporting_trigger <- reactive({
      input$tabs == "reporting"
    })

    reporting_widget_output =  reporting_widget_server( "reporting1" ,
                               trigger = reporting_trigger ,
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
