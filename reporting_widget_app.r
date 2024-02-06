
reporting_widget_ui = function ( id ){
        ns <- NS(id)  
        # fillCol( height = 600, flex = c(NA ) , 
        

  tagList(
      
      shinybusy::add_busy_spinner(
        spin = "fading-circle" , # "self-building-square",
        position = 'bottom-right'
        # , margins = c(70, 1200)
      ) ,
      
    fillPage(       
    tabsetPanel( type = "tabs",
                 # add_busy_spinner(spin = "fading-circle", position = "bottom-right") ,
                 
                 
      tabPanel( "" ,
          sidebarLayout(
            
          sidebarPanel( width = 3 ,           

            tabsetPanel( type = "tabs",
            tabPanel( "Org Levels",  
                    

            h5( 'Filter Org Units (press update button to change display') ,
 
            inputPanel(
            
             
              selectInput( ns("level2"), label = "OrgUnit Level2" , 
                            choices = NULL, 
                            selected = NULL ,
                            multiple = TRUE ) ,
              
              selectInput( ns("level3"), label = "OrgUnit Level3" ,
                            choices = NULL,
                            selected = NULL ,
                            multiple = TRUE ) ,
              
              selectInput( ns("level4"), label = "OrgUnit Level4" ,
                            choices = NULL,
                            selected = NULL  ,
                            multiple = TRUE  ) ,
              
              selectInput( ns("level5"), label = "OrgUnit Level5" ,
                            choices = NULL,
                            selected = NULL  ,
                            multiple = TRUE  ) ,
            
              selectInput( ns("source") , label = "Original/Cleaned" , 
                          choices = c( 'Original', 'Cleaned' ) , 
                          selected = 'Original' ) ,
              
              selectInput( ns("split") , label = "Split Data By:" , 
                          choices = "None" , 
                          selected = "None" )
              
      
        ) , 
        
        actionButton( ns('update_reporting_org_levels') , label = "Update orgUnits ") ,
        
        
        
        h5( 'Filter display dates') ,
        
        inputPanel( 
          
          selectizeInput( ns("startDisplayMonth") , label = "begining", 
                          choices = NULL ,
                          selected = NULL ) ,
          
          selectizeInput( ns("endDisplayMonth"), label = "ending", 
                          choices = NULL , 
                          selected = NULL )
          
        ) ,
        
        
        h5( 'Aggregate Level' ) ,
        
        selectInput( ns("level") , label = 'Select data only at specific level ("leaf" is all facilities that enter data):' ,
                     choices = c( 'leaf'  ) ,
                     selected = NULL ) 
        
        )  ,
        
        tabPanel( "Reporting Consistency" , 

                  h5( 'Find *Champion* facilities  -- the ones that reported the most each year' ) ,
                  
                  br() , 
                  
                  checkboxInput( ns("mostReports") , label ='Stratify by consistently reporting facilties', value = TRUE ) ,
                  
                  inputPanel( 
                    

                    
                    selectizeInput( ns("startingMonth") , label = "Begining with", 
                                    choices = NULL ,
                                    selected = NULL ) ,
                    
                    selectizeInput( ns("endingMonth"), label = "Ending with", 
                                    choices = NULL , 
                                    selected = NULL ) ,
                  
                  
                  checkboxInput( ns("exclude_recent_month") , label ='Exclude most recent period? (e.g. current month if data expected to be incomplete)',
                                 value = TRUE  ) ,
                  
                  selectizeInput( ns("missing_reports") , label = "Number of missing reports allowed/yr" , 
                                  choices = 0:2 , 
                                  selected = 0 ) 
                  ) 
                  
              ) , 
      
       tabPanel( "dataElements and dataSets",  
        # inputPanel(
        
        h5( "Datasets (forms used to enter data)" ) , 
        
              checkboxInput( ns("dataset_merge"), 
                             label ='Merge data from all datasets (or choose datasets below)', value = FALSE ) ,
              
              checkboxInput( ns("dataset_merge_average") , 
                             label ='When merging, average values reported by same facility to mutliple datasets', value = FALSE ) ,
              
              selectInput( ns("dataSets") , 
                           label ='Merge selected datasets with selected dataElements/Categories', 
                           choices = NULL  ,
                           selected = 1 ,
                           width = "100%" ,
                           multiple = TRUE ,
                           selectize = TRUE ) ,
        
        h5( "Data elements" ) , 
              
               # checkboxInput( ns("all_categories") , 
               #                        label = 'Select all dataElements/Categories',
               #                        value = TRUE )  ,
        
        
                actionButton( ns('update_data_categories') , label = "Update") ,
        
        
                checkboxGroupInput( ns("data_categories") ,
                                   label = "DataElement/Category" ,
                           choices = NULL  ,
                           selected = 1 ,
                           width = "100%" ) ,
        
             h5( "By default, facility counted as reporting if the selected data was reported") ,
        
              checkboxInput( ns("count.any") , label ='Categorize facility as reporting if any data submitted, even when data not selected above', value = FALSE )
      

               # ) # end inputPanel 
        ) # end tabPanel
      ) # end tabset panel
      )  , # end sidebar panel 
      
      mainPanel(  width = 9, 
                  
        tabsetPanel( type = "tabs", 
        tabPanel( "Summary",  style = "height:90vh;" ,
      
                  # fluidPage(
                    
                    
                    fluidRow( style = "height:40vh;",
                              
                            column(6, 
                                   
                              # h5( 'Number of Facilties Reporting Each Period') ,
                              
                              ### Number of Facilties Reporting each Period (plot_reporting_by_month)
                              plotOutput( ns('plot_reporting_by_month') 
                                  #         , 
                                  # click = "plot2_click" ,
                                  # dblclick = "plot2_dblclick" ,
                                  # hover = "plot2_hover" ,
                                  # brush = "plot2_brush", 
                                  # height = "80%" )
                              ) ) ,
                              
                             column(6,  
                              # htmlOutput("x_value") ,
                              
                              # h5( 'Histogram of Periods Reported Each Year') ,
        
                              ### Histogram of Annual Number of Months Reported (plot_reports_in_a_year)
                                  # miniContentPanel(
         
                                            plotOutput( ns('plot_reports_in_a_year') 
                                                        ,
                                              # click = "plot1_click" ,
                                              # dblclick = "plot1_dblclick" ,
                                              # hover = "plot1_hover" ,
                                              # brush = "plot1_brush" , 
                                              # height = "80%") 
                                            
                                            # , scrollable = TRUE
                                            )
                            )
                            
                          # , h6( 'Red indicates the facilities that reported each month (*Champions*)' ) 
                    ) ,
                    
                  # h5( "Data Values: On Right, from facilities that reported each month (*Champions*); on left, from all others")  ,
                  
                  # fluidRow( style = "height:10vh;",
                  #           
                  #           column(6, h5( "Data Values from Inconsistenly Reporting Facilities" ) ) ,
                  #           
                  #           column(6, h5( "Data Values from *Champion* Facilities" ) ) 
                  #           
                  # ) , 

                  fluidRow( style = "height:40vh;"  ,
                          
                          column(12, 
                            plotOutput( ns('plot_values') 
                                #         ,
                                # hover = "plotreportingSelectedOUsValues_hover" ,
                                # brush = "plotreportingSelectedOUsValues_brush" , 
                                # height = "80%"
                                )
                          )
                          )
                  # )
        ) ,
        
        tabPanel( "Facilities", 
                  
                 # tableOutput( ns('orgUnitReportingTable')) 
                 
                 
                 # miniPage(
                 # gadgetTitleBar( "Shiny gadget example" ),
                 
                 # miniTabstripPanel(
                 tabsetPanel( type = "tabs" ,

                   tabPanel( "Chart" , style = "height:80vh;" ,
                   # miniTabPanel( "Visualize" , icon = icon("area-chart"),
                                 # miniContentPanel(
                                   plotOutput( ns("facility_chart") , height = "100%")
                                 # )
                   ),
                   tabPanel( "Map" , style = "height:90vh;" ,
                   # miniTabPanel( ns("Map") ,
                                 # icon = icon("map-o"),

                                 # miniContentPanel(padding = 0,
                                                  # leafletOutput( ns("map") , height = "100%")
                                 # ),
                                 # miniButtonBlock(
                                   # actionButton( ns("resetMap") , "Reset" )
                                 # )
                   
                       fluidRow( style = "height:80vh;",
                                 column(12, 
                                        leafletOutput( ns("facility_map") , height = "100%")  )
                                 )
                       ) ,
                   tabPanel( "Data" , style = "height:80vh;" ,
                   # miniTabPanel( ns("Data")  , icon = icon("table"),
                                 # miniContentPanel(
                                   DT::dataTableOutput( ns("facility_table") )
                                 # )
                   )
                 )
        )
        )
                  
                  
        ) # end main panel
) # end sidbar layout
) # end tagset
  )
))
}
        
reporting_widget_server <- function( id , 
                                     dataDirectory = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ,
                                     cleaning_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              # dataDirectory = dataDirectory,
              # metadata = metadata_widget_output ,
              # data.details = data_widget_output 
              ) {

    options(shiny.trace=FALSE)
    options(shiny.reactlog=FALSE)
    options( dplyr.summarise.inform = FALSE )
    
    # cat('\n**Starting Reporting Widget\n')
    
    data.folder = reactive({ dataDirectory$directory() })
    indicator = reactive({ data_widget_output$indicator() })
    formulas = reactive({ data_widget_output$formulas() })
    dataset.file = reactive({ data_widget_output$dataset.file() })
    dataset = reactive({ data_widget_output$dataset() })
    data1 = reactive({ data_widget_output$data1() })
    formula_elements = reactive({ data_widget_output$formula_elements() })
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    geoFeatures = reactive({ metadata_widget_output$geoFeatures() })
    data2 = reactive({ cleaning_widget_output$data2() })
    
    # see https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
    shift_legend3 <- function(p) {
          pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
            with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
      
          if( length(pnls) == 0 ) return(p)
      
          reposition_legend( p, "center", panel=names(pnls) )
          }

    dates = reactive({
        req( data1() )
        req( period() )
        cat('\n* reporting_widget dates:') 
        
         cat('\n -data1 class:' , class( data1()) ) 
        .period = period()
        
        if ( ! .period %in% names( data1() ) ){
        cat('\n -- dataset does not contain column:' , .period )
        cat( '\n - dataset columns are:' , names( data1() ) )
        return()
        }
        
        # data.table?
        dates = data1() %>% 
          ungroup %>%
          distinct( !! rlang::sym( .period ) ) %>%
          arrange(!! rlang::sym( .period ) ) %>%
          pull( !! rlang::sym( .period ) ) 
         
        cat('\n - min:' , min( dates ), ', max:' , max( dates )) 

        cat('\n - done') 
        return( dates )

        })

    # reporting_month selection
    observeEvent(  dates() , {
      cat('\n - reporting_widget observeEvent dates() update startingMonth-')
      dates = dates()
      updateSelectizeInput( session, 'startingMonth' ,
              choices =  dates  %>% as.character()  ,
              selected = min( dates, na.rm = TRUE ) %>% as.character() , 
              server = TRUE
      )
      
      updateSelectizeInput( session, 'startDisplayMonth' ,
                            choices =  dates  %>% as.character()  ,
                            selected = min( dates, na.rm = TRUE ) %>% as.character() , 
                            server = TRUE
      )
      
      cat('\n - reporting_widget observeEvent dates() update endingMonth-' ) 
      updateSelectizeInput( session, 'endingMonth' ,
              choices =  dates  %>% as.character() ,
              selected = ( max( dates , na.rm = TRUE ) - 1 ) %>% as.character() ,
              server = TRUE
      )
      
      updateSelectizeInput( session, 'endDisplayMonth' ,
                            choices =  dates  %>% as.character() ,
                            selected = max( dates , na.rm = TRUE ) %>% as.character() ,
                            server = TRUE
      )
      
      cat('-done\n')
      } )


  # Update data
  observe({
    
    req( dataSets() )
    cat( '\n* updating merge dataSets input' )
    cat( '\n - dataSets()', dataSets() )
    
    if ( any( nchar( dataSets() > 0 ) ) ){
      updateSelectInput( session, 'dataSets' ,
                         choices =  dataSets()
      )
    }
} )

  observeEvent( input$dataset_merge , {
    cat( '\n* updating merge dataSets to all' )
    req( dataSets() )
    cat( '\n - dataSets:'  , dataSets() )
    
    if( input$dataset_merge == TRUE ){
    if ( any( nchar( dataSets() > 0 ) ) ){ 
      updateSelectInput( session, 'merge' , 
                         choices =  dataSets() ,
                         selected = dataSets() )
    }
    } else {
      updateSelectInput( session, 'merge' , 
                         choices =  dataSets() ,
                         selected = NULL
      )
      cat( '\n - done' )
  }
} )

#   observEvent({
#     cat( '\n - updating data_choices' )
#     updateSelectInput( session, 'data_categories' ,
#                        choices =   unique( data1()$data ) ,
#                        selected = 1 )
#     cat( '\n - done' )
# } )
  
  selected_data_categories = reactiveValues( )
  
  observeEvent( input$update_data_categories , {
    
    cat('\n * update_data_categories')
    
    selected_data_categories$elements = input$data_categories
    
    cat( "\n - ", paste( selected_data_categories$elements, collapse = ", " ) )
  })
  
  selected_org_levels = reactiveValues( 
    level2 = NULL , level3 = NULL , level4 = NULL , level5 = NULL )
  
  observeEvent( input$update_reporting_org_levels , {
    
    cat('\n * update_reporting_org_levels')
    
    selected_org_levels$level2 = input$level2
    selected_org_levels$level3 = input$level3
    selected_org_levels$level4 = input$level4
    selected_org_levels$level5 = input$level5
    
  })

  observeEvent( 'data' %in% names( data1() ) , {
    req( data1()$data )
    cat( '\n* updating data_categories to all' )
    
    updateCheckboxGroupInput( session, 'data_categories' , 
                         choices =   unique( data1()$data ) ,
                         selected = unique( data1()$data ) ) 

    selected_data_categories$elements = unique( data1()$data )
    
    cat( '\n - done' )
  } )
  
  # update split
  observe({  updateSelectInput( session, 'split' , 
                              choices =  c('None', names( data1() )) ) } )
  
  dataSets = reactive({
    req( data1() )
    cat('\n* reporting_widget dataSets:')
    # cat('\n - data1 class:' , class( data1()) ) 
    
    if ( is_empty( data1() ) ){
      cat('\n - data1() is empty')
      return()
    }
    
    if ( ! 'dataSet' %in% names( data1() ) ){
      message( 'dataSet not in names( dataset) ')
      cat('\n names( data1()) :' , names(data1()) )
      return()
    }
    
    # x = setDT( data1() )[ !is.na( dataSet ) , dataSet , ] %>%
    # unique
    
    #testing
    # saveRDS( data1() , "data1.rds" )
    
    x = data1()[ !is.na( data1()$dataSet ) , ]$dataSet %>% unique
    
    cat('\n - there are' , length( x ) , 'dataSets')
    
    return( x )
})

  # Months and periods
  period = reactive({
      req( data1() )
    
      cat('\n* reporting_widget period():')
    
      period = dataPeriod( data1() )
 
      cat('\n - period: ' , period ) 
      return( period  )
    })
  
  most_recent_period = reactive({
      req( data1() )
      req( period() )
      cat( '\n* reporting_widget Looking for most recent' , period() )
      # cat('\n - data1 class:' , class( data1()) ) 
      
      # data.table?
      mrp = max( data1() %>%
                   pull( !! rlang::sym( period() ) ), na.rm = TRUE )
      # mrp = max( data1()[ , 'Month'] , na.rm = TRUE )
      # mrp = max( data1()$Month , na.rm = TRUE )
      cat( '\n - most recent', period(),  'is', mrp )

      if ( input$exclude_recent_month ){
        cat('\n - exclude most recent period')
        if ( period() == "Month" ) mrp = mrp - month(1)
        if ( period() == "Week" ) mrp = mrp - 1
      }

      cat( '\n - mrp:' , mrp )
      return( mrp )
    })
  
  # d Filter data1 to selected level ####
  d = reactive({

      req( data1() )
      req( period() )
      cat( '\n* reporting_widget d:')
    
      # Testing 
      # saveRDS( data1() , 'dataset.rds' )
      # cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )
      
      if ( nrow( data1() ) == 0 ){
        cat('\n - data1() has zero rows')
        return()
      }
      
      # NB: setting data = setDT( data1()) has side effect of changing data1() to data.table. 
      data = as.data.table( data1() )
      
      cat( '\n - data (d) converted to data.table' )
      
      .period = period()
      cat('\n - period is', .period )
      
      data = data[ , period := base::get( .period )  , ]
      # data = data1()  %>% mutate( period = !!rlang::sym( .period ))
      
      # cat( "\n - reporting_widget data class/cols:" ,class( data ) )
      # cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )
           
      if ( !is_empty( selected_org_levels$level2 ) ){
        cat(  '\n - filtering data by' , levelNames()[2] , "=" , selected_org_levels$level2 ) 
        
        # data = data %>% 
        #   filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
        
        # cat(  '\n - data was' , class(data) ) 
        
        data = setDT( data )[ base::get( levelNames()[2] )  %in%   selected_org_levels$level2 ,, ]
        
        # cat(  '\n - and now is' , class(data) ) 
        
        #print( paste( 'data filtered by level2 has' , nrow( data ), 'rows' ))
        # glimpse( data )
      }
  
      if ( !is_empty( selected_org_levels$level3 ) ){
      cat(  '\n - filtering data by' , levelNames()[3] , "=" , selected_org_levels$level3 ) 
        
      data = setDT( data )[ base::get( levelNames()[3] )  %in%   selected_org_levels$level3 ,, ]
      
      # data = data %>% 
      #   filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
      
      #print( paste( 'data filtered by level3 has' , nrow( data ), 'rows' ))
      # glimpse( data )
      }
  
      if ( !is_empty( selected_org_levels$level4 ) ){
          cat(  '\n - filtering data by' , levelNames()[4] , "=" , selected_org_levels$level4 )
        
          data = setDT( data )[ base::get( levelNames()[4] )  %in%   selected_org_levels$level4 ,, ]
          
          # data = data %>% 
          #   filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
          
          #print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
          # glimpse( data )
      }
        
      if ( !is_empty( selected_org_levels$level5 ) ){
          cat(  '\n - filtering data by' , levelNames()[5] , "=" , selected_org_levels$level5 ) 
        
          data = setDT( data )[ base::get( levelNames()[5] )  %in%   selected_org_levels$level5  ,, ]
        
          # data = data %>% 
          #   filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )
          
          #print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
          # glimpse( data )
        }
    
      cat( '\n - nrow( d ):' , nrow( data ))
    
      if ( input$level %in% 'leaf'){  
        
        # data = data %>% filter( effectiveLeaf == TRUE )
        data = setDT( data )[ effectiveLeaf == TRUE , , ]
        
      } else {
        
        # data = data %>% filter( levelName  %in% input$level  )
        level. = count( orgUnits() %>% as_tibble, level, levelName ) %>% 
          filter(levelName  %in% input$level  ) %>% pull( level )
        
        data = setDT( data )[ level  %in% level. , , ] 
      }
  
  # if ( input$exclude_recent_month ) data = data %>% 
  #   filter( !! rlang::sym( period() ) <= most_recent_period() )
  
    if ( input$source %in% 'Original' ){
      cat('\n - d() source is original')
      
      # data = data %>% mutate( dataCol = original )
      data = setDT( data )[ , dataCol := as.numeric( original ) , ] 
    }  
    
    if ( input$source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
      cat( '\n -' , paste('Cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      
      # data = data %>% 
      #   mutate( dataCol = ifelse( seasonal3, original, NA  ) )
      
      data = setDT( data )[ , dataCol := fifelse( seasonal3, original, NA_real_  ) , ]
      
      # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
      if ('mad15' %in% names( data )){
        # data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
        data = setDT( data )[, mad15 := fifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) , ] 
        
      }
      
      if ('mad10' %in% names( data )){ 
        # data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
        data = setDT( data )[, mad10 := fifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) , ] 
        
      }
      
      if ('mad5' %in% names( data )){ 
        # data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
        data = setDT( data )[, mad5 := fifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) , ] 
        
      }
      
      if ('seasonal5' %in% names( data )){ 
        # data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
        data = setDT( data )[, seasonal5 := fifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) , ] 
      }
      
      if ('seasonal3' %in% names( data )){ 
        # data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
        data = setDT( data )[, seasonal3 := fifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) , ] 
        
      }
      
      cat( '\n -' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
    
    # #print( 'd: max period ' ); #print( max( d$period ))
    
    cat( '\n - d: max period: ' , max( data %>% pull( period  ) , na.rm = TRUE )  ); 
    # #print( max( data$Month , na.rm = TRUE ))
    
    
    cat( '\n - end d():', nrow(data) , 'rows' )
    # cat( "\n - reporting_widget d() class/cols: \n -- " , class( data ) , "\n -- " , names( data ))
    
    # testing
    # saveRDS( data, 'reporting_widget_d.rds')
    
    return( data )
})
    
  #  Reports ####

  orgunit.reports = reactive({ 
      req( selected_data_categories$elements )
      req( most_recent_period() )
      req( period() )
      
      cat( '\n* reporting_widget orgunit.reports()' )
      
      mrm = most_recent_period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      cat('\n - orgunit.reports--data')
      data = d()
      
      #Testing
      # saveRDS( data, 'orgunits.reports.data.rds')
      
      if ( !input$count.any  ){
        
        # data =  data %>% filter( data %in% input$data_categories )
        data = setDT( data )[ data %in% selected_data_categories$elements , , ]
        
      }  
       
      cat('\n - orgunit.reports--period') 
      .period = period()
      
      #Testing
      # saveRDS( .period, '.period.rds')
    
      # cat('\n -orgunit.reports--o.r.')
      # o.r. =
      #   data %>% as_tibble() %>% ungroup %>%
      # 
      #   mutate(
      # 
      #   calendar_year = year( !! rlang::sym( .period )  )
      
      o.r. = setDT( data %>% as_tibble() %>% ungroup )[ , 
                                                        calendar_year := year( base::get( .period ) ) , ] %>%
      rename( year =  {{ year_var }} ) 
      
      cat('\n - orgunit.reports--o.r.(DT)')
      
      # Testing:
      # saveRDS( o.r. , "o.r..rds")
      
      o.r. = setDT( o.r. )[ ,  .(n_periods = uniqueN( base::get( .period ) ) ) , 
                       by = c( 'year' , 'orgUnit' ) ] %>%
        .[ , n_periods := factor( n_periods ) ] %>%
        .[ , year := factor( year ) ] %>%
        as_tibble() 
      
      # group_by( year , orgUnit ) %>%
      # summarise( n_periods = n_distinct( !! rlang::sym( period() )  ) 
      #            # , max_month = max( Month ) 
      #            ) 
      # mutate( n_periods = factor( n_periods ) ,
      #         year = factor( year ) )
      
      ##print( 'o.r:') ; # #print(head(o.r))
      cat( '\n - end orgunit.reports' )
      return(o.r.)
    })
    
  annual.reports = reactive({ 
      req( orgunit.reports() )
      #print( 'annual reports()' )
      
      or = orgunit.reports() 
      # #print( 'annual reports() or:' ); #print( names(or))
      
      ar = setDT(or)[, .( n = uniqueN( orgUnit )  ), 
                       by = c( 'year' , 'n_periods' ) ] %>%
        as_tibble() 
      # %>%
      #   group_by( year ,  n_periods ) %>%
      #   summarise( n = n() )
      
      #print( 'end annual reports' )
      return( ar )
    })
    
  orgunit.monthly.reports = reactive({ 
      req( selected_data_categories$elements )
      cat( '\n* reporting_widget orgunit.monthly.reports():' )
      
      # mrp = most_recent_period()
      .period = period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      data = d()
      cat( '\n - o.m.r data has' , nrow(data), 'rows' )
      
      if ( !input$count.any   ){
        data = data %>% filter( data %in% selected_data_categories$elements )
      } 
      
      o.m.r = 
        data %>% as_tibble() %>% ungroup %>%
    
        mutate(
               calendar_year = year( !! rlang::sym( .period )  )
         
      )  %>%
      rename( year =  {{ year_var }} ) 
      # %>%
      # mutate( year = factor( year ) )
    
      cat( '\n - end orgunit.monthly.reports():' , nrow( o.m.r ), 'rows')
      # print(head(o.m.r))
      return(o.m.r)
    })
    
  monthly.reports = reactive({ 
      req( orgunit.monthly.reports() )
      req( period() )
      #print( 'monthly reports():' )
      
      .period = period()
     
      o.m.r = orgunit.monthly.reports() 
      
      # #print('monthly.reports() o.m.r'); #print( names(o.m.r) )
      
      m.r = setDT(o.m.r)[, .( n = uniqueN( orgUnit ) ), 
                       by = c( "year" , .period  ) ]  %>%
        as_tibble()
      
      # group_by( year , !! rlang::sym( .period )   ) %>%
      # summarise( n = n_distinct( orgUnit ) )
      
      # #print('m.r') ; glimpse(m.r)
      #print( 'end monthly reports()' )
      # glimpse( m.r)
      return(m.r)
    })
    
  facilities = reactive({
      #print( 'facilities' )
      req( orgunit.reports() )
      
      f = orgunit.reports() %>%
      ungroup() 
      
      f = setDT(f)[, .( Total = uniqueN( orgUnit )), ] %>%
        as_tibble() %>%
        # summarise( Total = n_distinct( orgUnit ) ) %>%
        pull( Total)
      
      #print( 'end facilities' )
      return(f)
    })
  
  # Levels ####
  observe({  
    updateSelectInput( session, 'agg_level' , 
                              choices = levelNames() , 
                              selected = levelNames()[1] ) 
    
    updateSelectInput( session, 'level' , 
                              choices = c( 'leaf' , levelNames()  )
                       ) 
  } )

  # level 2
  observeEvent( data1()  , {  
    if( nrow( data1() ) > 0 && 'level' %in% names( data1() )){
            cat( '\n* reporting_widget updating level2' )
            updateSelectInput( session, 'level2' ,
                                choices =
                                  data1() %>%
                                    pull( !! rlang::sym( levelNames()[2]  ) ) %>%
                                    unique %>% str_sort(),
                                selected = NULL
                                )
    }
    } )

  # level 3
  observe({ #Event( data1()  , {  
    req( selected_org_levels$level2 )
    if( nrow( data1() ) > 0 && 'level' %in% names( data1() )){
              cat( '\n* reporting_widget updating level3' )
      
              ls = setDT( data1() )[ base::get( levelNames()[2] ) %in% selected_org_levels$level2 , 
                                     base::get( levelNames()[3]  ), 
              ] %>%
                unique %>% str_sort()
              
              cat( "\n - level3 update to:" , paste( ls , collapse = "" ) )
              
              updateSelectInput( session, 'level3' ,
                                 
                                choices = ls
                                
                                  # data1() %>%
                                  #   filter(
                                  #   !! rlang::sym( levelNames()[2] ) %in% input$level2 ) %>%
                                  #   pull( !! rlang::sym( levelNames()[3]  ) ) %>%
                                  #   unique %>% str_sort() ,
                                
                                , selected = NULL
              )

    }
    } )

  # level 4
  observe({ #Event( data1()  , {    
    req( input$level3 )          
    if( nrow( data1() ) > 0 && 'level' %in% names( data1() ) ){
              cat( '\n* reporting_widget updating level4' )
      
              ls = setDT( data1() )[ base::get( levelNames()[3] ) %in% input$level3 , 
                                     base::get( levelNames()[4]  ), 
              ] %>%
                unique %>% str_sort()
      
              updateSelectInput( session, 'level4' ,
                                choices = ls ,
                                  # data1() %>%
                                  #   filter(
                                  #   !! rlang::sym( levelNames()[3] ) %in% input$level3 ) %>%
                                  #   pull( !! rlang::sym( levelNames()[4]  ) ) %>%
                                  #           unique %>% str_sort(),
                                selected = NULL
                                )
    }
    
    } )
  
  level5 = reactive({
      req( input$level4 )
      req( levelNames() )
      req( data1() )
      cat('\n* reporting_widget level5:')
      # cat('\n - data1 class:' , class( data1()) ) 
      
      if( is_empty( data1() ) ) return( NA )
      if( is.na( levelNames()[5] ) ) return( NA ) 
      
      ls = setDT( data1() )[ base::get( levelNames()[4] ) %in% input$level4 , 
                             base::get( levelNames()[5]  ), 
      ] %>%
        unique %>% str_sort()
  
      return( ls )
      # data1() %>% 
      #     filter(
      #         !! rlang::sym( levelNames()[4] ) %in% 
      #                    input$level4 ) %>% 
      #     pull( !! rlang::sym( levelNames()[5]  ) ) %>% 
      #     unique %>% str_sort()  
  })
  
  observe({  
    
    if (  nrow( data1() ) > 0 && 'level' %in% names( data1() ) ){
      updateSelectInput( session, 'level5' ,
                                choices = level5(),
            selected = NULL 
  )
                                    
    } 
  })
  
  levelNames = reactive({ 
    req( orgUnits() )
    cat( '\n* reporting_widget levelNames():' )
    
    l = getLevelNames( orgUnits() )
    
    cat( '\n - ' , l )
    return(l)
})

  levels = reactive({ 
    req( orgUnits() )
    cat( '\n* reporting_widget levels():' )
    levels = 
      count( orgUnits() %>% as_tibble, level, levelName ) %>% 
      arrange( level ) 
    cat( '\n - end levels():' )
    return( levels )
})

  # reportingSelectedOUs ####

  selected <- reactiveValues( x  = NULL, panel = NULL , chart = NULL )
  
  observeEvent( input$plot1_brush  , {
    # glimpse( input$plot1_brush )
    selected$chart = 1
    
    selected$x = round( 
      seq( input$plot1_brush$xmin , input$plot1_brush$xmax , by = .5 )
    ) %>% unique 
    
    selected$panel = input$plot1_brush$panelvar1
        
    #print( "plot1 selected$x:" ) ;  #print( paste( selected$x , selected$panel ) )
    return( selected )
  })
  
  observeEvent( input$plot2_brush  , {
    # glimpse( input$plot1_brush )
    selected$chart = 2
    
    selected$x = round( 
      seq( input$plot2_brush$xmin , input$plot2_brush$xmax , by = .5 )
    ) %>% unique %>% as.integer()
    
    selected$panel = input$plot2_brush$panelvar1
        
    cat( "plot_2_selected$x:" , selected$x ) ; #print( selected$panel ) 
    return( selected )
  })
  
  reportingSelectedOUs <- reactive({
    #print( 'reportingSelectedOUs()' )
    req( input$endingMonth )
    req( input$startingMonth  )
    req( d() )
    req( period() )
    req( selected_data_categories$elements )
    
    cat( "\n* reporting_widget: reportingSelectedOUs" )
    
    if ( length( selected_data_categories$elements ) == 0 ){
      cat("\n - no data elements selected")
      return()
    }
    
    # Testing
    # d = d() 
    # endingMonth = input$endingMonth 
    # startingMonth = input$startingMonth 
    # .period = period() 
    # missing_reports = as.integer( input$missing_reports ) 
    # count.any = input$count.any
    # all_categories = input$all_categories 
    # data_categories = input$data_categories 
    # .cat = FALSE
    # save( d , endingMonth, startingMonth, .period, missing_reports, count.any,
    #       all_categories, data_categories, .cat, file = 'reportingSelectedOUs_data.rda')
    
    
    if ( input$mostReports & nrow( d() ) > 0 ){
       cat( "\n - determining most frequently reported facilities..." ,
            input$startingMonth  ,  input$endingMonth )
      
       # cat( "\n - selected_data_categories:" , 
       #      paste( selected_data_categories$elements , collapse = ", " ) 
       #      )
      
      sf = mostFrequentReportingOUs( d = d() ,
                       endingMonth = input$endingMonth ,
                       startingMonth = input$startingMonth ,
                       # period = period() , 
                       missing_reports = as.integer( input$missing_reports ) ,
                       count.any = input$count.any ,
                       # all_categories = input$all_categories ,
                       data_categories = selected_data_categories$elements ,
                       .cat = TRUE )

      cat( "\n - mostFrequentReportingOUs:", length(sf), 'orgUnits' ); toc()

    } else {
         sf = NULL
       }
    
        cat( "\n - end reportingSelectedOUs:" )
        
        # Testing
        # saveRDS( sf, 'reportingSelectedOUs.rds')
        
        return( sf )
      })
    
  x.annual = reactive({
    cat( '\n* reporting_widget x.annual()' )
   
    # x.a = orgunit.reports() %>% 
    #       filter( orgUnit %in% reportingSelectedOUs() )   # %>%  
          # group_by( year , n_periods ) %>%
          #   summarise( n =  n_distinct( orgUnit ) )
    
    # data.table speed up over dplyr
    x.a = setDT( orgunit.reports() )[ orgUnit %in% reportingSelectedOUs() , 
                                      .( n = uniqueN( orgUnit ) ), 
                                      by = c( 'year' , 'n_periods' ) ]  %>%
      as_tibble()
  
    #print('end x.annual:') ; toc();  # #print( x.a )
    cat( '\n - x.annual done' )
    return( x.a )
    
  })
  
  x.months = reactive({
    cat( '\n* reporting_widget x.months()' )
    # req( keeprows() )
    # tic()
    #print( 'x.months()' )
    
    .period = period()
    
    # x.m = orgunit.monthly.reports() %>% 
    #       filter( orgUnit %in% reportingSelectedOUs() ) 
    
    x.m = setDT( orgunit.monthly.reports() )[  orgUnit %in% reportingSelectedOUs(), 
                                               .( n = uniqueN( orgUnit ) ) , 
                                               by = c( 'year' , .period )   ]  %>%
      as_tibble()
    
    
    # %>%
    #       group_by( year , !! rlang::sym( period() )  ) %>%
    #       summarise( n = n_distinct( orgUnit ) )
    
    #print('end x.months:') ; toc() ; # glimpse( x.m )
    cat( '\n - x.months done' )
    return( x.m )
    
  })

  # plot_reporting_by_month ####

  plot2 = reactive({
    req( monthly.reports() )
    req( period() )
    
    cat('\n* reporting_widget plot2():')
    .period = period()
    cat('\n - period():', .period )
    
    # save data for testing ggplot options
    # cat('\n - saving plot2_data.rds')
    # saveRDS( monthly.reports(), 'monthly.reports.rds' )
    
    if ( length( monthly.reports()$year) > 0  ) {
    
    if ( .period == "Month" ){
              .breaks = 1:12
    } else {
              .breaks = seq(2, 53, 4)
    }
  
    cat('\n - plot2: ggplot( monthly.reports() ... ')
    g = ggplot( monthly.reports() %>% mutate( facilities = 'All' ), 
                aes( x =  !! rlang::sym( .period ) 
                     , y = n  
                     , group = facilities
                     , color = facilities 
                     ) ) +
      # geom_col() +
      geom_point( ) +
      geom_line( ) +
      geom_hline( yintercept = facilities() ) +
      facet_wrap( ~ year , scales = 'free_x') +
      # scale_x_discrete( .period 
      #                     , breaks = .breaks
      #                     # , labels  = as.character( .breaks )
      #                     )  +
      ylim( 0 , NA ) +
      scale_color_manual( values = c( 'All' = 'black' , 
                                      'Selected'= 'brown' ) ) +
      scale_fill_manual( values = c( 'All' = 'black' , 
                                      'Selected'= 'brown' ) ) 
  
    if (!is.null( reportingSelectedOUs() ) ){
      cat('\n - g + selected facilities ')
      g = g + 
        # geom_col(  data = x.months() %>% mutate( facilities = 'Selected' )  ) 
        geom_point( data = x.months() %>% mutate( facilities = 'Selected' ) ) +
        geom_line( data = x.months() %>% mutate( facilities = 'Selected' ) )
      }
    
    # return( shift_legend3(g) )
    return( g )
    }
    
    #print('end plot2')
    return( g )
  })
  
  output$plot_reporting_by_month <- renderPlot({  plot2()  } )
  
  verbatimTextOutput("info")

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str( input$plot1_click ),
      "dblclick: ", xy_str(input$plot1_dblclick),
      "hover: ", xy_str(input$plot1_hover),
      "brush: ", xy_range_str(input$plot1_brush)
    )
  })
  
  output$x_value <- renderText({
    
    if ( input$mostReports ){ 
      HTML("You've selected <code>" , comma( length( reportingSelectedOUs() ) ), "facilities" , 
           "</code>" )
    }
    
    if ( is.null( selected$x ) ) return("")
    
    else {
      lvls <- levels( annual.reports()$n_periods )
      panel = selected$panel
      
      name <- lvls[ round( selected$x ) ]
      HTML("You've selected <code>" , comma( length( reportingSelectedOUs() ) ) , 
           "</code>" ,
           "facilities that submitted data for <code>", 
           paste( name, collapse = "," ) , 
           "months during", panel , 
           "</code>" )
    }
  })

  # Histogram of Annual Number of Months Reported (plot_reports_in_a_year) ####
  
  plot1 = reactive({
      #print( 'plot1():' )
      req( annual.reports() )
      req( period() )
      .period = period()
      
      # save data for testing ggplot options
      # saveRDS( annual.reports() , 'plot1_data.rds' )
      
      
      if ( length( annual.reports()$year) > 0  ) {
        
      if ( .period == "Month" ){
                .breaks = 1:12
      } else {
                .breaks = seq(2, 53, 4)
      }
        
      cat('\n - plot1: ggplot( annual.reports() ... ')
      
      # Testing
      # saveRDS( annual.reports(), "annual.reports.rds" )
      # saveRDS( reportingSelectedOUs() , "reportingSelectedOUs.rds" )
      # saveRDS( facilities() , "facilities.rds" )
      # saveRDS( x.annual() , "x.annual.rds" )
      
      g = ggplot( annual.reports() , 
                  aes( x = n_periods , y = n ) ) +
        geom_col() + 
        scale_x_discrete( 'Number Months Reported' 
                          , breaks = .breaks   
                          , labels  =  .breaks ,
                          drop = FALSE 
                          )  +
        geom_hline( yintercept = facilities() ) +
        facet_wrap( ~ year , scales = 'free_x')
      
      if (!is.null( reportingSelectedOUs() ) ){
        g = g + geom_col( data = x.annual() , fill = 'brown', width = .9 ) 
      }
    
      #print( 'end plot1' )
      return(g)
      }
})

  output$plot_reports_in_a_year <- renderPlot({  plot1()  } )
  
  # selected_data. select ous and data element categories ####

  selected_data = reactive({
   #print( 'selected_data():')
   req( data1() )
   req( selected_data_categories$elements )
   
   cat("\n* reporting_widget selected_data(): " )
  
   cat("\n - levels " , 
       selected_org_levels$level2 , selected_org_levels$level3  ,selected_org_levels$level4  ,selected_org_levels$level5   )
   
   cat("\n - reporting_widget selected_data_categories(): " , 
       paste( selected_data_categories$elements , collapse = ", " )   )
    
   .cleanedData = cleanedData( 
                        data1() ,
                        .effectiveLeaf = TRUE ,
                        source = input$source ,
                        error =  NULL ,
                        algorithm = 'seasonal3' ,
                        .cat = TRUE )
   
   # testing
   
   # When there are no leaf orgUnits (e.g. National or District data only)
   if ( nrow( .cleanedData ) == 0 ){
     
     .cleanedData = cleanedData( 
                        data1() ,
                        .effectiveLeaf = FALSE ,
                        source = input$source ,
                        error =  NULL ,
                        algorithm = 'seasonal3' ,
                        .cat = TRUE )
     
     cat( "\n - .cleanedData with .effectiveLeaf FALSE, nrow = ", nrow( .cleanedData ))
   }
     
   selected_data =  selectedData( 
                        data = .cleanedData , # data1() ,
                        levelNames = levelNames() ,
                        data_categories = selected_data_categories$elements ,
                        # all_categories = input$all_categories ,
                        alwaysReporting = input$mostReports ,
                        reportingSelectedOUs = reportingSelectedOUs() ,
                        startingMonth = NULL , 
                        endingMonth = NULL ,
                         # source = 'Original' ,
                        level = 'leaf' , 
                        level2 = selected_org_levels$level2 ,
                        level3 = selected_org_levels$level3 ,
                        level4 = selected_org_levels$level4 ,
                        level5 = selected_org_levels$level5 ,
                        .cat = TRUE  )
   
   
      # data = data1() ,
      # levelNames = levelNames() ,
      # data_categories = selected_data_categories$elements ,
      # # all_categories = input$all_categories ,
      # alwaysReporting = input$mostReports ,
      # reportingSelectedOUs = reportingSelectedOUs() ,
      # source = input$source ,
      # level2 = input$level2 ,
      # level3 = input$level3 ,
      # level4 = input$level4 ,
      # level5 = input$level5 ,
      # .cat = TRUE )
  
   #  data = setDT( d() )[ , Selected := 'All', ]
   #  # data = d() %>% mutate( Selected = 'All' ) 
   #  
   #  
   #  # filter to selected category
   #  cat( '\n - selected_data filtered by' # , input$data_categories 
   #       )
   # 
   #  if ( !input$all_categories )  
   #    
   #    # data = data %>% filter( data %in% input$data_categories )
   #    
   #    data = setDT( data )[ data %in% input$data_categories ,, ]
   #  
   #  # Add var for selected ous
   #  cat( '\n - selected_data length( reportingSelectedOUs()): ' , length( reportingSelectedOUs())  )
   #  
   # if ( length( reportingSelectedOUs()) > 0 ){
   #   
   #   data = setDT( data )[ , Selected := fifelse( orgUnit %in% reportingSelectedOUs() , 
   #                                                    'Reporting Each Period',
   #                                                    'Inconsistent Reporting') ]
   #   # data  = data %>%
   #   #   mutate( Selected = ifelse( 
   #   #     orgUnit %in% reportingSelectedOUs() , 
   #   #     'Reporting Each Period', 
   #   #     'Inconsistent Reporting' )
   #   #   )
   # } 
   #  
   #  cat( '\n - end  selected_data()')  
   
   cat("\n - end selected_data()")
   
   # Testing 
   # saveRDS( selected_data , 'selected_data.rds')
   
  return( selected_data )
})

  group_by_cols = reactive({
    req( period() )
    req( levelNames() )
    
    cat("\n* reporting_widget group_by_cols():")
    
    # adms = backtick( levelNames() )
    adms = levelNames()
    cat( "\n - adms:" , adms )
    
    group_by_cols = groupByCols( 
                        period = period() , 
                        levelNames = levelNames() ,
                        agg_level = adms[ 1 ] ,
                        split = input$split )
    
    cat("\n - group_by_cols:", group_by_cols )
    
    return( group_by_cols )

})

  # data.total: merges data across multiple set ####
  data.total = reactive({
    req( selected_data() )
    # req( group_by_cols() )
    req( period() )
    req( input$startDisplayMonth )
    req( input$endDisplayMonth )
    
    cat( '\n* reporting_widget data.total()' )
    
    # Testing
    # saveRDS( selected_data()  , 'selected.data.rds')
    # saveRDS( group_by_cols()  , 'group_by_cols.rds')
    
    data.total = dataTotal(
        data = selected_data()   , 
        period = period() ,
        group_by_cols = group_by_cols() ,
        startMonth = input$startDisplayMonth ,
        endMonth = input$endDisplayMonth ,
        dataSets = input$dataSets ,
        mean.merge = input$dataset_merge_average ,
        .cat = TRUE )
    
    cat('\n - end data.total()')
    
    # Testing
    # saveRDS( data.total , 'data.total.rds')
    return( data.total )
    
  })
  
  num_facilities = reactive({
    req( data.total() )
    #print('num_facilities()')
    .d = data.total()
    l = length( unique( .d$Selected ) )
    #print( paste( 'number of Facilities', l ) )
    return( l )
  })
  
  num_datasets = reactive({
    req( data.total() )
    #print('num_datasets()')
    .d = data.total()
    l = length( unique( .d$dataSet ) )
    #print( paste( 'number of dataSets', l ) )
    return( l )
  })
  
  backtick <- function(x) paste0("`", x, "`")
  
  aggregateDataKey = reactive({
    req( num_facilities() )
    req( num_datasets() )
    
    cat('\n* reporting_widget aggregateDataKey():' )
    
    adms = backtick( levelNames() )
    
    hts = paste( "(" , adms[1] , ")" )
    
    # if >1 Facilities (ie. selected)
    if ( num_facilities() > 1 )  hts = paste( 
             'Selected *' , hts 
             )
    
    # if >1 dataset 
    if ( num_datasets() > 1 )  hts = paste( 
             'dataSet *' , hts
             )
    
    # # Cross by split
    if ( !input$split %in% 'None' ) hts =
      paste( backtick( input$split ) , '*' ,  hts )
    
    cat('\n - done:' , hts )
    return( hts )

  })
  
  aggregateselected_data = reactive({
    # req( data.hts() )
    req( data.total() )
    cat('\n* reporting_widget aggregateselected_data():' )
    
    # testing
    saveRDS( levelNames() , 'levelNames.rds')
    saveRDS( aggregateDataKey() , 'aggregateDataKey.rds')
    
    .d = data.total()
    cat('\n - data.total():' )
    
    # testing
    saveRDS( .d , 'data.total.rds' )
    
    if ( !is_tsibble( .d ) ){
      cat('\n - preparing data.total as tsibble')
      
      key.cols = setdiff( group_by_cols() , period() )
      
      cat('\n - key.cols:', key.cols )
          
      # testing
      # saveRDS( key.cols , 'key.cols.rds' )
      
      .d = .d %>% 
        as_tsibble( index = !! rlang::sym( period() )  ,
                    key =  all_of(  {{ key.cols }} ) )
    }  
    
    cat('\n - preparing aggregate_key')
    
    .d = .d %>%
      aggregate_key(  .spec = !!rlang::parse_expr( aggregateDataKey() ) ,
                      total = sum( total , na.rm = T )
                      ) 
    
    indexVar = index_var( .d )
    keyVars = key_vars( .d )
    
    # testing
    # saveRDS( .d , 'agg.d1.rds' )
    
    .d = .d %>%
      filter(
          ! is.na( !! rlang::sym( levelNames()[1] ) )
          , is_aggregated( !! rlang::sym( levelNames()[1] ) )
        ) %>%
      mutate( grouping_var = 'Total' )
       
      if ( num_datasets() > 1 ){
         #print( 'num_datasets()>1:') ;
         .d = .d %>% 
         filter( !is_aggregated( dataSet ) ) %>%
         mutate( dataSet = as.character( dataSet ) %>%
             str_remove_all( "<aggregated>" ) ,
             grouping_var = dataSet )
         #print( unique(.d$dataSet))
       }  
       
       if ( num_facilities() > 1 ){
         #print( 'num_facilities()>1:') ; 
         .d = .d %>% 
         filter( !is_aggregated( Selected )  ) %>%
         mutate( Selected = as.character( Selected ) %>%
             str_remove_all( "<aggregated>" )  )
         
         #print( unique(.d$Selected))
       }  
      
    # testing
    # saveRDS( .d , 'agg.d2.rds' )
    
      # if split, remove aggregate grouping
       if ( !input$split %in% 'None' ){
         #print( '!input split none') ; #print( input$split )
         .d = .d %>%
           filter( !is_aggregated( !! rlang::sym( input$split ) ) 
           ) %>%
           mutate( grouping_var = as.character( 
             !! rlang::sym( input$split ) )
           )
         #print( unique(.d$grouping_var) )
         # #print( glimpse( .d ))
         
       } 
    
    # testing
    # saveRDS( .d , 'agg.d3.rds' )
    
    # ensure output is tbl_ts
    if ( ! 'tbl_ts' %in% class( .d )  ){
      cat( '\n - convert .d to tsibble ')
      .d = .d  %>% as_tsibble( key = all_of( keyVars ) , index = indexVar  )
    }
    
      # testing
       # saveRDS( .d, 'aggregateselected_data.rds')
       
       cat('\n -  end aggregateselected_data()' )
       return( .d )
   
  })
  
  caption.text =  reactive({
      paste( 
        ifelse( reportingSelectedOUs() > 0 , 
             paste( comma( length( reportingSelectedOUs() ) ), 'facilities' ) ,
             "" ) ,
      ifelse( nchar( selected_org_levels$level2 ) > 0, paste( selected_org_levels$level2 , collapse = "+" ) ) ,
      ifelse( nchar( selected_org_levels$level3 ) > 0, paste(  "/" , selected_org_levels$level3 , collapse = "+" ) ) ,
      ifelse( nchar( selected_org_levels$level4 ) > 0, paste(  "/" , selected_org_levels$level4, collapse = "+"   ) ) ,
      ifelse( nchar( selected_org_levels$level5 ) > 0, paste(  "/" , selected_org_levels$level5, collapse = "+"   ) )
                           )
    })
  
  n_selected = reactive({
    req( selected_data() )
    
    cat("\n* n_selected(): ")
    
    # testing
    # saveRDS( reportingSelectedOUs(), "reportingSelectedOUs.rds") 
    # saveRDS( selected_data(),"selected_data.rds" ) 
    # saveRDS( data1(), "data1.rds" ) 
    # saveRDS( levelNames() , "levelNames.rds" )
    # saveRDS(  selected_data_categories$elements , "selected_data_categories.rds" )
    
    x = selected_data() %>% as_tibble %>% ungroup %>%
      distinct( Selected , orgUnit ) %>%
      group_by( Selected ) %>%
      summarise( n = n())
    
    # cat( x )
    return( x )

  })
  
  plotAgregateValue = reactive({
    
    req( aggregateselected_data() )
    req( input$split )
    cat('\n* reporting_widget plotAgregateValue():' )
  
    .d = aggregateselected_data()
    
    # testing
    saveRDS(.d, 'plot3_data.rds')
    
    
    data.text = paste( unique( selected_data()$data ) ,
                       collapse = " + " ) 
    cat("\n - data.text:" , data.text )
    
    # #print( 'data.text'); #print( data.text )
    
    .limits = c(0, NA)
    .period = period()
       
    #print('plotting aggregate data');
    
    cat('\n - fill_gaps' )
    .d = .d %>% 
      fill_gaps( .full = TRUE  )
    
    cat('\n - plot' )
    g = .d %>% 
      # autoplot( vars( total , grouping_var ) )
      ggplot( aes(x = !! rlang::sym( .period ) , y = total
               , group = grouping_var  
               , colour =  grouping_var
              ) )  +
        geom_line()
    
    # Line color for mulitple datasets
    if ( num_datasets() > 1 ){
      #print( 'unique dataSets'); #print( unique( .d$dataSet ) )
      dataSet_breaks = unique( .d$dataSet )
      datSet_labels =  unique( .d$dataSet )
      datSet_labels[ datSet_labels == "" ] = "Combined"
      
      g = g +
      scale_color_discrete( breaks = dataSet_breaks ,
                            labels =  datSet_labels ,
                            drop = TRUE ) +
        
      guides(color=guide_legend(title="dataSet"))
      
    } else {
       g = g + guides( color = "none" )
    }
    
    # Split data
    if ( !input$split %in% 'None' ){
      g = g + 
        
        guides(color=guide_legend(title= input$split )) 
    }
  
    facet_labeller = function( x ){
      
      y = n_selected() %>% filter( Selected %in% x ) %>% pull( n )
      
      paste0( x , " ( n= ", comma( y ) , " )")
      
    } 
    
    # facet when selected > 0
    # Testing
    cat("\n - length( reportingSelectedOUs() ):" , length( reportingSelectedOUs() ) )
    
    if ( length( reportingSelectedOUs() ) > 0 ) g =
                g + facet_wrap( vars( Selected ) ,
                    labeller = as_labeller( facet_labeller ) ,
                    # scales = 'free' , 
                    ncol = 3 ) 
    
    # Time scales
    if ( period() %in% 'Month' )  g = g + 
      scale_x_yearmonth( date_breaks = "1 year" )
    
    if ( period() %in% 'Week' )  g = g + 
      scale_x_yearweek( date_breaks = "1 year" )
    
    g = g +
      scale_y_continuous( label=comma, limits = .limits ) +
      labs( y = "" , x="" ,
            title = str_wrap( input$indicator , 200 ) ,
            subtitle = str_wrap( data.text , 200 ) 
            , caption =  str_wrap( caption.text() , 200 )
            ) +
      theme_minimal( )  + 
      theme( legend.position = "bottom",  
             strip.text = element_text(size = 20)  # facet label text size
             ) +
      guides( color = guide_legend( ncol=1 ,
                                    title="Dataset" ) )
      
    #print( ' end plotAgregateValue()' )
    cat('\n - done' )
    return( g )
  })
  
  output$plot_values <- renderPlot({  plotAgregateValue()  })
  outputOptions( output, "plot_values", suspendWhenHidden = TRUE )
  
# Champions Map and Table####
  
  # Avg Value by facility 
  avgValues = reactive({ 
    req( selected_data())  
    cat( "\n * avgValues:")
    
    # Testing 
    # saveRDS( selected_data() , 'selected_data.rds' )
    
    ## TODO: use DT to speed up...
    avgValues =
      selected_data()  %>% 
      group_by( orgUnit, Month ) %>% 
      summarise( dataCol = sum( dataCol , na.rm = TRUE )) %>%
      group_by( orgUnit ) %>%
      summarise( medianValue = median( dataCol , na.rm = TRUE )) 
    
    cat( "\n - done avgValues:")
    return( avgValues )
  })
  
  champion_facilities = reactive({
    req( avgValues()) 
    req( reportingSelectedOUs() )
    cat( "\n * champion_facilities:")
    
    ous = orgUnits()
    sou = reportingSelectedOUs()
    gf = geoFeatures()
    avgValues = avgValues()
    
    # testing 
    # saveRDS( gf , 'gf.rds' )
    # saveRDS( sou , 'sou.rds' )
    # saveRDS( avgValues , 'avgValues.rds' )
    saveRDS( ous , 'ous.rds')
    
    cat( "\n - quartile values:")
    quartileValues = quantile( avgValues$medianValue , probs = c( 0, 0.25, 0.5, 0.75, 1) )
 
    cat( "\n - champion column:")
    champion_facilities = ous %>%
      right_join( gf, by = join_by( id ) ) %>%
        # filter( st_geometry_type(.) == 'POINT') %>% 
        # filter( !st_is_empty(.) ) %>%
        mutate( 
          champion = ifelse( id %in% sou , "Consistent Reporting (Champion)", "Inconstent Reporting" )
        )
    
    cat( "\n - join avgvalues:")
    champion_facilities = champion_facilities %>%
      # filter( id %in% "a08881Oz98k" ) %>%
      left_join( avgValues , by = c( "id" = "orgUnit" ) ) %>%
      mutate( medianValueRange = cut( medianValue , breaks =  unique( quartileValues ) , ordered_result = TRUE ) ,
              medianValueRangeSize = medianValueRange %>% as.numeric() )
      
    cat( "\n - done facilities:")
    return( champion_facilities )
  })
  
  facility_chart = reactive({
    req( champion_facilities() )
    cat( "\n * facility_chart:")
    
    champion_facilities = champion_facilities() %>% st_drop_geometry()
    
    summary = champion_facilities %>%
      select( -parentGraph, groups ) %>%
      group_by( champion ) %>%
      summarise( n = n() , mean = mean( medianValue , na.rm = TRUE )) 
    
    annotation = paste0( "mean = ", round( summary$mean ), " (n=", scales::comma( summary$n ) , ")" )
    
    facility_chart = 
      champion_facilities %>%
      ggplot( aes( champion, medianValue  ) ) + 
      geom_boxplot() +
      stat_summary( fun = mean , geom = "point", shape = 5, size = 4 ) +
      geom_jitter( width = .35 ) +
      labs( x = "", y = "Median Value") +
      scale_y_log10() +
      annotate( 'text' , x = 1:2 , y = 1.5 , label =  annotation  ) +
      labs( title = "Comparison of Values Repoted by Consistent and Inconsistent Reporting Facilities" )

    cat( "\n - done facility_chart:")
    return( facility_chart )
  })
  
  base.map = reactive({
    
    cat( "\n * reporting_widget: base.map")
    gf = geoFeatures()

    cat( '\n - split geofeatures')
    split_geofeatures = base::split( gf , f = gf[['levelName']]  )
    
    levels = bind_rows(gf %>% st_drop_geometry()) %>% filter( !is.na( level)) %>% distinct( level, levelName ) 
    
    cat( '\n - levels:' , levels$levelName  , '\n')
    
    # reorder levels
    split_geofeatures = split_geofeatures[ levels$levelName ]
    
    # test for empty geometry
    not_all_empty_geo = map_lgl( split_geofeatures , ~!all(is.na(st_dimension(.x))) )
    
    n_levels = sum( not_all_empty_geo ) # 
    
    cat( paste('\n - geoFeatures split into' , n_levels , 'levels' ,
               paste( names( split_geofeatures ), collapse = ',' ), sep = " " ) , '\n')
    
    level.colors = RColorBrewer::brewer.pal(n_levels, 'Set2')
    names( level.colors ) = levels[ not_all_empty_geo, 'levelName' ]
    
    
    split_gf = split_geofeatures[ not_all_empty_geo ]
    
    
    admins = gf %>% filter( st_geometry_type(.) != 'POINT') %>% filter( !st_is_empty(.) )
    
    admin.levels = admins$levelName %>% unique 
    
    # pal <- colorNumeric( palette = "YlGnBu", domain = avgValues$medianValue  )
    cat( "\n - base.map")
    base.map =
      leaflet( options = leafletOptions( preferCanvas = TRUE ,  updateWhen = FALSE ) ) %>%
      addTiles(group = "OSM (default)") %>%
      
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters"
      ) %>%
      
      addProviderTiles(providers$Stadia.StamenToner, group = "Toner") %>%
      # addProviderTiles( providers$Stamen.TonerLite , group = "Toner Lite") %>%
      # addProviderTiles( "Stadia.Stamen.Terrain" , group = "Stamen.Terrain" ) %>%
      addProviderTiles( "Esri.WorldStreetMap", group = "Esri.WorldStreetMap" )  %>%
      addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
      addTiles( group = "No Background" , options = providerTileOptions( opacity = 0 ) ) %>%

      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", 
                       "Esri.WorldStreetMap" , "Esri.WorldImagery", "No Background" ) ,
        overlayGroups = c( admin.levels , "", "Facility" ) ,
        options = layersControlOptions( collapsed = TRUE )

      ) 
    
    
    for ( i in seq_along( admin.levels ) ){
        base.map = base.map %>%
          addPolygons( data = admins %>% filter( levelName == admin.levels[ i ] ) ,
                       group = admin.levels[ i ] ,
                       label = ~paste( name ,   
                                       ifelse( level < 3 , '' , 
                                               paste( 'in' ,  parentName ) )
                       ) ,
                       color = "black", 
                       weight = 1, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0 , fillColor = "lightblue" ,
                       highlightOptions = highlightOptions( color = "white", weight = 2,
                                                            bringToFront = TRUE)
          )  %>% hideGroup( admin.levels[ i ] )
      }
        

        
    # cat( "\n - addLayersControls")   
    # base.map = base.map 
          # Layers control
          # addLayersControl(
          #   baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Stamen.Terrain", 
          #                  "Esri.WorldStreetMap" , "Esri.WorldImagery", "No Background" ) ,
          #   overlayGroups = c( admin.levels , "Facility") ,
          #   options = layersControlOptions( collapsed = TRUE )
          # ) 

    return( base.map )
      
  })
  
  facility_map = reactive({
    
    cat( "\n * reporting_widget: facility map")
    gf = geoFeatures()
    facilities = champion_facilities()
    avgValues = avgValues()
    base.map = base.map()
    
    # Testing
    # cat( "\n - saving facility_map files for testing")
    # save( gf, facilities, avgValues, base.map , file = "facility_map.rda" )
    
    
    cat( "\n - admin.levels")
    admins = gf %>% filter( st_geometry_type(.) != 'POINT') %>% filter( !st_is_empty(.) )
    admin.levels = admins$levelName %>% unique 
    
    
    cat( '\n - factpal')
    
    factpal <- colorFactor( c("red4", "grey20")  , facilities$champion )
    
    cat( '\n - symbols')
    
    symbols <- makeSymbolsSize(
      # values = ifelse( is.na( facilities$medianValueRangeSize ), 0, facilities$medianValueRangeSize )  ,
      values = ifelse( is.na( facilities$medianValue ), 0, facilities$medianValue ) ,
      shape = 'circle' ,
      color = factpal( facilities$champion ) ,
      fillColor =  factpal( facilities$champion ) ,
      fillOpacity = .8,
      baseSize = .5
    )
    
    cat( '\n - add markers')
    
    gf.map = base.map  %>%

      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Stamen.Terrain", 
                       "Esri.WorldStreetMap" , "Esri.WorldImagery", "No Background" ) ,
        overlayGroups = c( admin.levels , "Reporting") ,
        options = layersControlOptions( collapsed = TRUE ) 
        ) %>%
        
      #   addCircleMarkers( data = facilities , group = "Facility" ,
      #     radius = ~ medianValueRangeSize  ,
      #     fillColor = ~ factpal( champion )  ,
      #     stroke = FALSE, fillOpacity = .9
      # )
      
      addMarkers(data = facilities ,
                 icon = symbols ,
                 group = "Reporting" ) 
    

  
      
    # size legend with library(  leaflegend )
    
    cat( '\n - add legend')
    
    gf.map = gf.map %>%
      
      addLegendSize(
            # values = ifelse( is.na( facilities$medianValueRangeSize ), 0, facilities$medianValueRangeSize )    ,
            values = ifelse( is.na( facilities$medianValue ), 0, facilities$medianValue ) , 
            color = 'black',
            fillColor = 'black' ,
            opacity = .5,
            title = 'Median Value',
            shape = 'circle',
            # orientation = 'horizontal',
            breaks = 4 ,
            baseSize = .5
            ) %>%
      
      addLegend("bottomright",
                values = facilities$champion ,
                pal = factpal ,
                title = 'Reporting Consistency' ,
                opacity = .5
  )
    
  #   options = popupOptions(closeButton = FALSE)
    
    cat( '\n - done')
    
    return( gf.map )
    
  })
  
  output$facility_chart <- renderPlot({ facility_chart() })
  
  output$facility_map <- renderLeaflet({ facility_map()  })
  
  output$facility_table <- DT::renderDataTable({ 
    champion_facilities() %>% st_drop_geometry() %>% 
      select( -parentGraph , - groups )
    })
  

# Return ####
  split = reactive({ input$split })
  startingMonth = reactive({ input$startingMonth })
  endingMonth = reactive({ input$endingMonth })
  missing_reports = reactive({ as.integer( input$missing_reports ) })

  return( 
    list(
      dates = dates ,
      # dataset = dataset , 
      d = d ,
      # data.hts = data.hts ,
      data.total = data.total , 
      aggregateselected_data = aggregateselected_data , 
      period = period ,
      group_by_cols = group_by_cols ,
      levelNames = levelNames ,
      split = split ,
      startingMonth = startingMonth ,
      endingMonth = endingMonth ,
      missing_reports = missing_reports  ,
      num_datasets = num_datasets ,
      num_facilities = num_facilities ,
      selected_data = selected_data ,
      caption.text = caption.text ,
      reportingSelectedOUs = reportingSelectedOUs 
    ))
} )
}


