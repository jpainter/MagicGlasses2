
reporting_widget_ui = function ( id ){
        ns <- NS(id)  
        # fillCol( height = 600, flex = c(NA ) , 
  tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,

    sidebarLayout(
      sidebarPanel(
          tabsetPanel( type = "tabs",
          tabPanel( "Monthly reporting",  
 
            inputPanel(
             selectInput( ns("level") , label = "Organization Level:" ,
                          choices = c( 'leaf'  ) ,
                          selected = NULL ) ,
             
            checkboxInput( ns("exclude_recent_month") , label ='Exclude most recent period?',
                       value = TRUE  ) ,
            
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
                          selected = "None" ) , 
              
              checkboxInput( ns("count.any") , label ='Count any categories', value = FALSE ) 
              
      
        ) ,
        
        h5( 'Filter to consistently reporting facilties') ,
        
        inputPanel( 
          
          checkboxInput( ns("mostReports") , label ='Find facilities reporting each month', value = TRUE ) ,
          
          selectizeInput( ns("startingMonth") , label = "Begining with", 
                          choices = NULL ,
                          selected = NULL ) ,
          
          selectizeInput( ns("endingMonth"), label = "Ending with", 
                          choices = NULL , 
                          selected = NULL )
          
          ) ,
        
        h5( 'Filter display dates') ,
        
        inputPanel( 
          
          selectizeInput( ns("startDisplayMonth") , label = "begining", 
                          choices = NULL ,
                          selected = NULL ) ,
          
          selectizeInput( ns("endDisplayMonth"), label = "ending", 
                          choices = NULL , 
                          selected = NULL )
          
        )
        )  
      
        , 
      
       tabPanel( "Choose dataElements, categories, and dataSets ",  
        # inputPanel(
        
              checkboxInput( ns("dataset_merge"), 
                             label ='Merge all datasets', value = FALSE ) ,
              
              checkboxInput( ns("dataset_merge_average") , 
                             label ='Average values when reported to mutliple datasets', value = FALSE ) ,
              
              selectInput( ns("merge") , 
                           label ='Merge selected datasets with selected dataElements/Categories', 
                           choices = NULL  ,
                           selected = 1 ,
                           width = "100%" ,
                           multiple = TRUE ,
                           selectize = TRUE ) ,
              
               checkboxInput( ns("all_categories") , 
                                      label = 'Select all dataElement/Categories',
                                      value = TRUE )  ,
      
               selectInput( ns("data_categories") , 
                                  label = "DataElement/Category" , 
                          choices = NULL  ,
                          selected = 1 ,
                          width = "100%" ,
                          multiple = TRUE ,
                          selectize = TRUE
                          ) 
      

               # ) # end inputPanel 
        ) # end tabPanel
      ) # end tabset panel
      ) , # end sidebar panel 
      
      mainPanel( 
        tabPanel( "Facilities Reporting",  style = "height:90vh;" ,
      
                  fluidPage( 
                  fluidRow( style = "height:40vh;",
                          column(6, 
                            ### Number of Facilties Reporting each Period (plot_reporting_by_month)
                            plotOutput( ns('plot_reporting_by_month') , 
                                click = "plot2_click" ,
                                dblclick = "plot2_dblclick" ,
                                hover = "plot2_hover" ,
                                brush = "plot2_brush" )
                            ) ,
                            
                           column(6,  
                            # htmlOutput("x_value") ,
      
                            ### Histogram of Annual Number of Months Reported (plot_reports_in_a_year)
                                miniContentPanel(
       
                                          plotOutput( ns('plot_reports_in_a_year') ,
                                            click = "plot1_click" ,
                                            dblclick = "plot1_dblclick" ,
                                            hover = "plot1_hover" ,
                                            brush = "plot1_brush" ) ,
                                          
                                          scrollable = TRUE
                                          )
                          ) 
                  )
        ,
                  fluidRow( style = "height:40vh;"  ,
                          
                          column(12, 
                            plotOutput( ns('plot_values') ,
                                hover = "plotSelectedOusValues_hover" ,
                                brush = "plotSelectedOusValues_brush"
                                )
                          )
                          )
                  )
        ) 
        ) # end main panel
) # end sidbar layout
) # end tagset
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
    dt1 = reactive({ data_widget_output$dt1() })
    formula_elements = reactive({ data_widget_output$formula_elements() })
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
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

    observeEvent( dt1() , {
      cat( "\n* observeEvent dt1. class:" , class( dt1() ))
    })

  # Update data
  observe({
    
    req( dataSets() )
    cat( '\n* updating merge dataSets input' )
    cat( '\n - dataSets()', dataSets() )
    
    if ( any( nchar( dataSets() > 0 ) ) ){
      updateSelectInput( session, 'merge' ,
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
      cat( '\n- done' )
  }
} )

#   observEvent({
#     cat( '\n- updating data_choices' )
#     updateSelectInput( session, 'data_categories' ,
#                        choices =   unique( data1()$data ) ,
#                        selected = 1 )
#     cat( '\n- done' )
# } )

  observeEvent( 'data' %in% names( data1() ) , {
    req( data1()$data )
    cat( '\n* updating data_categories to all' )
    
    if( input$all_categories == TRUE ){
      updateSelectInput( session, 'data_categories' , 
                         choices =   unique( data1()$data ) ,
                         selected = unique( data1()$data ) ) 
    } else {
      updateSelectInput( session, 'data_categories' , 
                         choices =   unique( data1()$data ) ) 
    }
    cat( '\n- done' )
  } )
  
  # update split
  observe({  updateSelectInput( session, 'split' , 
                              choices =  c('None', names( data1() )) ) } )
  
  dataSets = reactive({
    req( data1() )
    cat('\n* reporting_widget dataSets:')
    cat('\n - data1 class:' , class( data1()) ) 
    
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
    cat('\n - \n, ' , x )
    return( x )
})

  # Months and periods
  period = reactive({
      req( data1() )
      cat('\n* reporting_widget period():')
      cat('\n - data1 class:' , class( data1()) ) 
      
      weekly = any( map_lgl( data1() ,
                             ~any(class(.x) %in% 'yearweek'  )) )

      period = ifelse( weekly, "Week", "Month" )
      #print('end period()'); #print( period )
      return( period  )
    })
  
  most_recent_period = reactive({
      req( data1() )
      req( period() )
      cat( '\n* reporting_widget Looking for most recent' , period() )
      cat('\n - data1 class:' , class( data1()) ) 
      
      # data.table?
      mrp = max( data1() %>%
                   pull( !! rlang::sym( period() ) ), na.rm = TRUE )
      # mrp = max( data1()[ , 'Month'] , na.rm = TRUE )
      # mrp = max( data1()$Month , na.rm = TRUE )
      cat( '\nmost recent ', period(),  'is', mrp )

      if ( input$exclude_recent_month ){
        cat('\n - exclude most recent period')
        if ( period() == "Month" ) mrp = mrp - month(1)
        if ( period() == "Week" ) mrp = mrp - 1
      }

      cat( '\n- mrp:' , mrp )
      return( mrp )
    })
  
  d = reactive({

      req( data1() )
      req( period() )
      cat( '\n* reporting_widget d:')
    
      # Testing 
      # saveRDS( data1() , 'dataset.rds' )
      cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )
      
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
      
      cat( "\n - reporting_widget data class/cols:" ,class( data ) )
      cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )
           
      if ( !is_empty( input$level2 ) ){
        cat(  '\n - filtering data by' , levelNames()[2] , "=" , input$level2 ) 
        
        # data = data %>% 
        #   filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
        
        cat(  '\n - data was' , class(data) ) 
        
        data = setDT( data )[ base::get( levelNames()[2] )  %in%   input$level2 ,, ]
        
        cat(  '\n - and now is' , class(data) ) 
        
        #print( paste( 'data filtered by level2 has' , nrow( data ), 'rows' ))
        # glimpse( data )
      }
  
      if ( !is_empty( input$level3 ) ){
      cat(  '\n - filtering data by' , levelNames()[3] , "=" , input$level3 ) 
        
      data = setDT( data )[ base::get( levelNames()[3] )  %in%   input$level3 ,, ]
      
      # data = data %>% 
      #   filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
      
      #print( paste( 'data filtered by level3 has' , nrow( data ), 'rows' ))
      # glimpse( data )
      }
  
      if ( !is_empty( input$level4 ) ){
          cat(  '\n - filtering data by' , levelNames()[4] , "=" , input$level4 )
        
          data = setDT( data )[ base::get( levelNames()[4] )  %in%   input$level4 ,, ]
          
          # data = data %>% 
          #   filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
          
          #print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
          # glimpse( data )
      }
        
      if ( !is_empty( input$level5 ) ){
          cat(  '\n - filtering data by' , levelNames()[5] , "=" , input$level5 ) 
        
          data = setDT( data )[ base::get( levelNames()[5] )  %in%   input$level5  ,, ]
        
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
        data = setDT( data )[ levelName  %in% input$level , , ] 
      }
  
  # if ( input$exclude_recent_month ) data = data %>% 
  #   filter( !! rlang::sym( period() ) <= most_recent_period() )
  
    if ( input$source %in% 'Original' ){
      cat('\n - d() source is original')
      
      # data = data %>% mutate( dataCol = original )
      data = setDT( data )[ , dataCol := original , ] 
    }  
    
    if ( input$source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
      cat( '\n -' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      
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
      
      cat( '\n-' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
    
    # #print( 'd: max period ' ); #print( max( d$period ))
    
    cat( '\n - d: max period: ' , max( data %>% pull( period  ) , na.rm = TRUE )  ); 
    # #print( max( data$Month , na.rm = TRUE ))
    
    
    cat( '\n - end d():', nrow(data) , 'rows' )
    cat( "\n - reporting_widget d() class/cols: \n -- " , class( data ) , "\n -- " , names( data ))
    
    # testing
    # saveRDS( data, 'reporting_widget_d.rds')
    
    return( data )
})
    
  #  Reports ####

  orgunit.reports = reactive({ 
      req( input$data_categories )
      req( most_recent_period() )
      req( period() )
      
      cat( '\n* reporting_widget orgunit.reports()' )
      
      mrm = most_recent_period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      cat('\n - orgunit.reports--data')
      data = d()
      
      #Testing
      # saveRDS( data, 'orgunits.reports.data.rds')
      
      if ( !input$count.any & !input$all_categories ){
        # data =  data %>% filter( data %in% input$data_categories )
        data = setDT( data )[ data %in% input$data_categories , , ]
        
      }  
       
      cat('\n - orgunit.reports--period') 
      .period = period()
      
      #Testing
      # saveRDS( .period, '.period.rds')
    
      # cat('\n-orgunit.reports--o.r.')
      # o.r. =
      #   data %>% as_tibble() %>% ungroup %>%
      # 
      #   mutate(
      # 
      #   calendar_year = year( !! rlang::sym( .period )  )
      
      o.r. = setDT( data %>% as_tibble() %>% ungroup )[ , 
                                                        calendar_year := year( base::get( .period ) ) , ] %>%
      rename( year =  {{ year_var }} ) 
      
      cat('\n-orgunit.reports--o.r.(DT)')
      
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
      req( input$data_categories )
      cat( '\n* reporting_widget orgunit.monthly.reports():' )
      
      # mrp = most_recent_period()
      .period = period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      data = d()
      cat( '\n - o.m.r data has' , nrow(data), 'rows' )
      
      if ( !input$count.any & !input$all_categories   ) data = data %>% filter( data %in% input$data_categories )
      
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
    req( input$level2 )
    if( nrow( data1() ) > 0 && 'level' %in% names( data1() )){
              cat( '\n* reporting_widget updating level3' )
      
              ls = setDT( data1() )[ base::get( levelNames()[2] ) %in% input$level2 , 
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
      cat('\n - data1 class:' , class( data1()) ) 
      
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
    l = count( orgUnits() %>% as_tibble, level, levelName ) %>% 
      arrange( level ) %>% pull(levelName ) 
    l = l[ !is.na(l) ]
    cat( '\n- end levelNames():' )
    return(l)
})

  levels = reactive({ 
    req( orgUnits() )
    cat( '\n* reporting_widget levels():' )
    levels = 
      count( orgUnits() %>% as_tibble, level, levelName ) %>% 
      arrange( level ) 
    cat( '\n- end levels():' )
    return( levels )
})

  # selectedOus 

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
  
  selectedOUs <- reactive({
    #print( 'selectedOUS()' )
    tic()
    req( input$endingMonth )
    req( input$startingMonth  )
    
    if ( input$mostReports ){
       cat( "\ndetermining most frequently reported facilities..." ,
            input$startingMonth  ,  input$endingMonth )
      
      data = d()
      
      if ( !input$count.any & !input$all_categories  ){
          cat( '\ninput$all_categories:' , input$all_categories  )
          data = data %>% filter( data %in% input$data_categories )
       }
  
       if ( period() %in% 'Month' ){
         data = data %>% as_tibble %>%
         filter( 
           period >=  yearmonth( input$startingMonth )  ,
           period <=  yearmonth( input$endingMonth )  
                 )
       } 
      
       if ( period() %in% 'Week' ){
         cat( '\n - selectedOUS by Week')
         data = data %>% as_tibble %>%
         filter( 
           period >=  yearweek( input$startingMonth )  ,
           period <=  yearweek( input$endingMonth )  
                 )
       } 
      
       mr = data %>% 
         filter( !is.na( original  ) ) %>%
         distinct( !! rlang::sym( period() ) , orgUnit ) %>%
         group_by( orgUnit ) %>%
         summarise( n = n() ) %>%
         arrange( desc( n ))
       
       #print( "mr" ); #print( summary( mr$n ) )
  
       s = mr %>%
         filter( n == max( mr$n ) ) %>%
         pull( orgUnit ) %>% unique
       
       cat( "\n*** mostReports selectedOUs:", length(s), 'orgUnits' ); toc() 
       return( s )
       }
    
    if( is.null( selected$x ) ) return( NULL )
    
    if ( selected$chart == 1 ){
        cat('\n*** chart1 selected$x:' , selected$x )
        select_month =  as.numeric( orgunit.reports()$n_periods ) %in% selected$x
        cat('\nchart1 select_month:' , sum(select_month) ) 
        cat('\nchart1 orgunit.reports()$year:' , orgunit.reports()$year ) 
        cat('\nchart1 selected$panel:' , selected$panel ) 
        selectedRows = select_month &
           orgunit.reports()$year %in% selected$panel 
        s = orgunit.reports()[ selectedRows, ]$orgUnit %>% unique
        
  
      } else {
        cat('\n*** chart2 selected$x:' , selected$x )
        select_month = month( orgunit.monthly.reports() %>%
                                pull( !! rlang::sym( period() ) ) ) %in% selected$x
        selectedRows = select_month &
          year( orgunit.monthly.reports() %>%
                  pull( !! rlang::sym( period() ) ) ) %in% selected$panel
        s = orgunit.monthly.reports()[ selectedRows, ]$orgUnit %>% 
          unique
        
        
      }
  
        cat( "\n*** end selectedOUs:", length(s), 'orgUnits' ); toc()  # #print( selectedOUs )
        
        # Testing
        # saveRDS( s, 'selectedOUs.rds')
        
        return( s )
      })
    
  x.annual = reactive({
    cat( '\n* reporting_widget x.annual()' )
   
    # x.a = orgunit.reports() %>% 
    #       filter( orgUnit %in% selectedOUs() )   # %>%  
          # group_by( year , n_periods ) %>%
          #   summarise( n =  n_distinct( orgUnit ) )
    
    # data.table speed up over dplyr
    x.a = setDT( orgunit.reports() )[ orgUnit %in% selectedOUs() , 
                                      .( n = uniqueN( orgUnit ) ), 
                                      by = c( 'year' , 'n_periods' ) ]  %>%
      as_tibble()
  
    #print('end x.annual:') ; toc();  # #print( x.a )
    return( x.a )
    
  })
  
  x.months = reactive({
    # req( keeprows() )
    # tic()
    #print( 'x.months()' )
    
    .period = period()
    
    # x.m = orgunit.monthly.reports() %>% 
    #       filter( orgUnit %in% selectedOUs() ) 
    
    x.m = setDT( orgunit.monthly.reports() )[  orgUnit %in% selectedOUs(), 
                                               .( n = uniqueN( orgUnit ) ) , 
                                               by = c( 'year' , .period )   ]  %>%
      as_tibble()
    
    
    # %>%
    #       group_by( year , !! rlang::sym( period() )  ) %>%
    #       summarise( n = n_distinct( orgUnit ) )
    
    #print('end x.months:') ; toc() ; # glimpse( x.m )
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
    # cat('\n- saving plot2_data.rds')
    # saveRDS( monthly.reports(), 'monthly.reports.rds' )
    
    if ( length( monthly.reports()$year) > 0  ) {
    
    if ( .period == "Month" ){
              .breaks = 1:12
    } else {
              .breaks = seq(2, 53, 4)
    }
  
    cat('\n- plot2: ggplot( monthly.reports() ... ')
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
  
    if (!is.null( selectedOUs() ) ){
      cat('\n- g + selected facilities ')
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
  
  output$plot_reporting_by_month <- renderPlot({  plot2()  } , height = "auto")
  
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
      HTML("You've selected <code>" , comma( length( selectedOUs() ) ), "facilities" , 
           "</code>" )
    }
    
    if ( is.null( selected$x ) ) return("")
    
    else {
      lvls <- levels( annual.reports()$n_periods )
      panel = selected$panel
      
      name <- lvls[ round( selected$x ) ]
      HTML("You've selected <code>" , comma( length( selectedOUs() ) ) , 
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
    
  cat('\n- plot1: ggplot( annual.reports() ... ')
  
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
  
  if (!is.null( selectedOUs() ) ){
    g = g + geom_col( data = x.annual() , fill = 'brown' ) 
  }

  #print( 'end plot1' )
  return(g)
  }
})

  output$plot_reports_in_a_year <- renderPlot({  plot1()  } , height = "auto" )
  
  # Plot values ####

  plotData = reactive({
   #print( 'plotData():')
   req( d() )
   req( input$data_categories )
   cat("\n* reporting_widget plotData():")
  
    data = setDT( d() )[ , Selected := 'All', ]
    # data = d() %>% mutate( Selected = 'All' ) 
    
    
    # filter to selected category
    cat( '\n - plotData filtered by' , input$data_categories )

    if ( !input$all_categories )  
      
      # data = data %>% filter( data %in% input$data_categories )
      
      data = setDT( data )[ data %in% input$data_categories ,, ]
    
    # Add var for selected ous
    cat( '\n - plotData length( selectedOUs()): ' , length( selectedOUs())  )
    
   if ( length( selectedOUs()) > 0 ){
     
     data = setDT( data )[ , Selected := fifelse( orgUnit %in% selectedOUs() , 
                                                      'Reporting Each Period',
                                                      'Inconsistent Reporting') ]
     # data  = data %>%
     #   mutate( Selected = ifelse( 
     #     orgUnit %in% selectedOUs() , 
     #     'Reporting Each Period', 
     #     'Inconsistent Reporting' )
     #   )
   } 
    
    cat( "\n - plotData cols:" ,  names( data ) ) 
    cat( '\n- end  plotData()')  
    # TESTING
    # saveRDS( data , "plotData.rds" )
    
  return( data )
})

  group_by_cols = reactive({
    # req( input$split )
    cat("\n* reporting_widget group_by_cols():")
    
    .period = period()
    group_by_cols =  c(.period , 'orgUnit', 'Selected',
                        'dataSet' , 'data' ) 
    
    group_by_cols = c( group_by_cols, levelNames() )
  
    cat("\n- group_by_cols():", group_by_cols )
    
    if ( input$split != 'None' ) group_by_cols = c( group_by_cols , input$split )
    
    # if ( length( selectedOUs() > 0 ) ) 
      # group_by_cols = c( group_by_cols , 'Facilities' )
    
    # # If not merge when total, show separate datsets
    # if ( !input$merge & input$all_categories ) group_by_cols = c( group_by_cols , 'dataSet' )
    #   
    cat( "\n- end group_by_cols()" , unique( group_by_cols )  )
    return( unique( group_by_cols ) )

})

  data.total = reactive({
    req( plotData() )
    # req( group_by_cols() )
    req( period() )
    req( input$startDisplayMonth )
    req( input$endDisplayMonth )
    
    cat( '\n* reporting_widget data.total()' )
  
    
       .period = period()
       cat( '\n - .period:' , .period )
       # if ( input$merge & input$all_categories ){
      
      .group_by_cols =  group_by_cols()  
      cat( '\n - data.total .group_by_cols:'  )
      
      # Testing
      # saveRDS( .group_by_cols , 'group_by_cols.rds' )
  
      # Total categories by facilities and datasets
      data = plotData() 
      cat( "\n - starting with plotData().  class/cols:\n -- " , class( plotData() ) , "\n -- " , names( plotData() ) )
      
      # Testing
      # saveRDS( data , 'plotData.rds' )
      
      # Merge  datasets 
      # Set all dataSets to Combined and re-summaries taking mean
      
      mergeDatasets = input$merge %>% str_replace_all( fixed("\n"), "") 
      
      cat( '\n -  mergeDatasets:' , mergeDatasets  )
      
      # Testing
      # saveRDS( input$merge , 'merge.rds' )
      
      # cat( "\n - testing if nchar( mergeDatasets ) > 0", 
      #      any(!is.na( mergeDatasets )) & any(nchar( mergeDatasets ) > 0 ) 
      #      )
      
      if ( any(!is.na( mergeDatasets )) & any(nchar( mergeDatasets ) > 0 )  ){
        
        cat( "\n - combineSelectDatasets ")
        tic()
        
        # cat( "\n - datasets are:" , unique( data$dataSet ) )
        # cat( "\n - mergeDatasets are:" , mergeDatasets )
        
        data = 
                # setDT( data )[ , dataSet := dataSet %>% str_replace_all( fixed("\r\n"), "")  ] %>%
                # # mutate( dataSet = dataSet %>% str_replace_all( fixed("\r\n"), "") ) %>%
                # 
                # .[ ,  dataSet := dataSet %>% str_replace_all( fixed("\n"), "")   ] %>%
                # # mutate( dataSet = dataSet %>% str_replace_all( fixed("\n"), "") ) %>%
          
                setDT( data )[ any(  mergeDatasets %in% dataSet ) ,  dataSet :=  'Combined'  ] %>%
          
                as_tibble
                # mutate(
                #     dataSet = ifelse( any( dataSet %in% mergeDatasets ) ,
                #                       'Combined' ,
                #                       dataSet) ,
                #     # dataSet = 'Combined' ,
                #     data = 'Total'
                # )
        
        # Testing
        # saveRDS( data, "data.rds")
        
        toc()
        # data = setDT( data )[ , dataSet := dataSet %>% str_replace_all( fixed("\r\n"), "") , ]
        # 
        # data = setDT( data )[ , .(dataSet = fifelse( 
        #                             any( str_replace_all( dataSet, fixed("\n"), "") %in% mergeDatasets ) , 
        #                             'Combined' , 
        #                             dataSet ) ,
        #                           data = 'Total' ) , ]

        
      # if include, summarise precipitation
      tic()
      cat('\n -  summarising data by:' , .group_by_cols )
      
      if ( any( grepl( "avg_mm" , names( data ) ) ) ){
          
          cat( '\n - with avg_mm' )
          data = setDT( data ) %>%
                .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
                        avg_mm = mean( avg_mm , na.rm = TRUE  ) ) , 
                   by =  .group_by_cols ] 
          
        } else {
          
          data = setDT( data ) %>%
                .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
        }
        
      toc()
      
      }
      
      # Testing
      # saveRDS( combineSelectDatasets , 'combineSelectDatasets.rds' )
      
      # data.table sum/mean 
      mean.merge = input$dataset_merge_average 
        
      if ( mean.merge ) {
            cat( '\n -  merge data.table MEAN') 
            
          data = data %>%
                mutate( dataSet = 'Merged') %>%
                setDT() %>%
                # Mean of dataSets within orgUnit
                .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
  
            cat( '\n - Merge done' );  # glimpse( dataMerge )
        
      } 
      
      # Testing
      # saveRDS( dataMerge, 'dataMerge.rds' )
      # #print( dataMerge %>% duplicates %>% glimpse )
  
      key.cols = setdiff( group_by_cols() , .period ) 
      cat('\n - key.cols:' ,  key.cols )
      
      if ( .period %in% 'Month' ){
        cat( '\n -  .period %in% Month' )
      
      # data.total = data.total %>% 
      #   filter( 
      #     Month >=  yearmonth( input$startDisplayMonth )  ,
      #     Month <=  yearmonth( input$endDisplayMonth )  
      #   )
      
      data.total = setDT( data )[  which( Month >=  yearmonth( input$startDisplayMonth ) &
                                        Month <=  yearmonth( input$endDisplayMonth ) ) , ] 
      
    } 
    
    if ( .period %in% 'Week' ){
      cat( '\n -  .period %in% weeks' )
      
      # data.total = data.total %>% 
      #   filter( 
      #     Week >=  yearweek( input$startDisplayMonth )  ,
      #     Week <=  yearweek( input$endDisplayMonth )  
      #   )
      
      data.total = setDT( data )[ which( Week >=  yearweek( input$startDisplayMonth ) &
                                        Week <=  yearweek( input$endDisplayMonth ) ) , ] 
    } 
    
    
    cat( "\n - data.total cols:" , names( data.total ) )
    
    ## NB does data.total need to be a Tsibble?--it is slow.  
    
    data.total = data.total[ , total := replace_na( dataCol , 0)  ,] %>%
      
        # dataMerge %>% 
        # # fill_gaps( .full = TRUE  ) %>%
        
      # mutate( 
        #         total = replace_na( dataCol , 0) 
        #         )  %>% # for plotting, replace missing with zero 
        
        as_tibble()
    
        # as_tsibble( index = !! rlang::sym( .period )  , 
        #             key =  all_of(  {{ key.cols }} ) ) 

    cat( '\n - data.total class' , class( data.total ) ) 
    cat( '\n - data.total cols' , names( data.total ) ) 
    
    # Filter display dates
    # cat( '/n - data.total cols:', names( data.total ) )
    

    # testing:
    # saveRDS( data.total, 'data.total.rds')
    
    cat('\n- end data.total()')
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
  
  aggregatePlotData = reactive({
    # req( data.hts() )
    req( data.total() )
    cat('\n* reporting_widget aggregatePlotData():' )
    
    # testing
    # saveRDS( data.hts() , 'data.hts.rds' )
    # saveRDS( levelNames() , 'levelNames.rds')
    
    .d = data.total()
    
    if ( !is_tsibble( .d ) ){
      cat('\n - preparing data.total as tsibble')
      
      key.cols = setdiff( group_by_cols() , period() )
      
      cat('\n - key.cols:',key.cols )
      
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
    
    # ensure output is tbl_ts
    if ( ! 'tbl_ts' %in% class( .d )  ){
      cat( '\n - convert .d to tsibble ')
      .d = .d  %>% as_tsibble( key = all_of( keyVars ) , index = indexVar  )
    }
    
       
       cat('\n -  end aggregatePlotData()' )
       return( .d )
   
  })
  
  caption.text =  reactive({
      paste( 
        ifelse( selectedOUs() > 0 , 
             paste( comma( length( selectedOUs() ) ), 'facilities' ) ,
             "" ) ,
      ifelse( nchar( input$level2 ) > 0, paste( input$level2 , collapse = "+" ) ) ,
      ifelse( nchar( input$level3 ) > 0, paste(  "/" , input$level3 , collapse = "+" ) ) ,
      ifelse( nchar( input$level4 ) > 0, paste(  "/" , input$level4, collapse = "+"   ) ) ,
      ifelse( nchar( input$level5 ) > 0, paste(  "/" , input$level5, collapse = "+"   ) )
                           )
    })
  
  plotAgregateValue = reactive({
    
    req( aggregatePlotData() )
    req( input$split )
    cat('\n* reporting_widget plotAgregateValue():' )
  
    .d = aggregatePlotData()
    
    # testing
    # saveRDS(.d, 'plot3_data.rds')
    
    data.text = paste( unique( plotData()$data ),
                       collapse = " + " ) 
    
    # #print( 'data.text'); #print( data.text )
    
    .limits = c(0, NA)
    
       
    #print('plotting aggregate data');
    
  
    g = .d %>% 
      fill_gaps( .full = TRUE  ) %>%
      # autoplot( vars( total , grouping_var ) )
      ggplot( aes(x = !! rlang::sym( period() ) , y = total
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
  
    
    # facet when selected > 0
    if ( length( selectedOUs() ) > 0 ) g =
    g + facet_wrap( vars( Selected ) ,
                    scales = 'free' , ncol = 3 ) 
    
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
      theme( legend.position = "bottom" ) +
      guides( color = guide_legend( ncol=1 ,
                                    title="Dataset" ) )
      
    #print( ' end plotAgregateValue()' )
    cat('\n - done' )
    return( g )
  })
  
  output$plot_values <- renderPlot({  plotAgregateValue()  })
  outputOptions( output, "plot_values", suspendWhenHidden = TRUE )

# Return ####
  split = reactive({ input$split })
  startingMonth = reactive({ input$startingMonth })
  endingMonth = reactive({ input$endingMonth })

  return( 
    list(
      dates = dates ,
      # dataset = dataset , 
      d = d ,
      # data.hts = data.hts ,
      data.total = data.total , 
      aggregatePlotData = aggregatePlotData , 
      period = period ,
      group_by_cols = group_by_cols ,
      levelNames = levelNames ,
      split = split ,
      startingMonth = startingMonth ,
      endingMonth = endingMonth ,
      num_datasets = num_datasets ,
      num_facilities = num_facilities ,
      plotData = plotData ,
      caption.text = caption.text ,
      selectedOUs = selectedOUs 
    ))
} )
}


