reporting_widget_ui = function ( id ){
        ns <- NS(id)  
        # fillCol( height = 600, flex = c(NA ) , 
        

tabsetPanel( type = "tabs",
# add_busy_spinner(spin = "fading-circle", position = "bottom-right") ,

tabPanel( "Monthly reporting",             
  inputPanel(
  
  
  selectInput("level2", label = "OrgUnit Level2" , 
                choices = NULL, 
                selected = NULL ,
                multiple = TRUE ) ,
  
  selectInput("level3", label = "OrgUnit Level3" ,
                choices = NULL,
                selected = NULL ,
                multiple = TRUE ) ,
  
  selectInput("level4", label = "OrgUnit Level4" ,
                choices = NULL,
                selected = NULL  ,
                multiple = TRUE  ) ,
  
  selectInput("level5", label = "OrgUnit Level5" ,
                choices = NULL,
                selected = NULL  ,
                multiple = TRUE  ) ,

  
  selectInput("source", label = "Original/Cleaned" , 
              choices = c( 'Original', 'Cleaned' ) , 
              selected = 'Original' ) ,
  
  selectInput("split", label = "Split Data By:" , 
              choices = "None" , 
              selected = "None" ) , 
  
  checkboxInput( "count.any", label ='Count any categories', value = FALSE ) ,
  
  checkboxInput( "mostReports", label ='Most frequently reporting facilities', value = TRUE ) ,
  
  selectInput( "startingMonth", label = "begining with", 
               choices = NULL ,
               selected = NULL ) ,
  selectInput( "endingMonth", label = "ending with", 
               choices = NULL , 
               selected = NULL )
  
  )
  ) , 
 tabPanel( "Monthly reporting",  
  inputPanel(
  splitLayout( cellWidths = c("25%", "75%"), 
               checkboxInput( "all_categories", 
                              label = 'Select all dataElement/Categories',
                              value = FALSE )  ,
               div(id = "expr-container",
                
              selectInput("data_categories", 
                          label = "DataElement/Category" , 
                  choices = NULL  ,
                  selected = 1 ,
                  width = "100%" ,
                  multiple = TRUE ,
                  selectize = TRUE
                  ) 
            )
            ) ,

  
# div(id = "expr-container", 
  splitLayout( cellWidths = c("15%", "25%", "60%") ,
               
       checkboxInput( "dataset_merge", 
                     label ='Merge all datasets', value = FALSE ) ,
       
      checkboxInput( "dataset_merge_average", 
                     label ='Average values when reported to mutliple datasets', value = FALSE ) ,
 
        
       selectInput( "merge", 
                    label ='Merge selected datasets with selected dataElements/Categories', 
              choices = NULL  ,
              selected = 1 ,
              width = "100%" ,
              multiple = TRUE ,
              selectize = TRUE ) 
       ) 
  ) ),
  # tagList(

tabPanel( "Monthly reporting", 
                      ### Number of Facilties Reporting each Period (plot_reporting_by_month)
                      plotOutput( 'plot_reporting_by_month' , 
                          click = "plot2_click" ,
                          dblclick = "plot2_dblclick" ,
                          hover = "plot2_hover" ,
                          brush = "plot2_brush" ) ,
                      
                      htmlOutput("x_value") ,
                                
                      ### Histogram of Annual Number of Months Reported (plot_reports_in_a_year)
                      plotOutput( 'plot_reports_in_a_year' , 
                          click = "plot1_click" ,
                          dblclick = "plot1_dblclick" ,
                          hover = "plot1_hover" ,
                          brush = "plot1_brush" ) ,
                      
                      plotOutput( 'plot_values' ,
                          hover = "plotSelectedOusValues_hover" ,
                          brush = "plotSelectedOusValues_brush"
                          )
          ) 
) 
}
        
reporting_widget_server <- function( id , 
                                     dataDirectory = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ){
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
    
    cat('\n**Starting Reporting Widget\n')
    
    data.folder = reactive({ dataDirectory$directory() })
    indicator = reactive({ data_widget_output$indicator() })
    formulas = reactive({ data_widget_output$formulas() })
    dataset.file = reactive({ data_widget_output$dataset() })
    formula_elements = reactive({ data_widget_output$formula_elements() })
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })

    dataset = reactive({
      req( dataset.file() )
      req( data.folder() )
      cat('\n*Reading dataset file\n')
      file = paste0( data.folder() , dataset.file() )
      if ( !file.exists( file ) ) return()
      dataset = readRDS( file )
      cat( '\n-dataset read:' , dataset.file() , 'has' , nrow(dataset) , 'rows\n' )
      return( dataset )
    })

    dates = reactive({
        req( dataset() )
        req( period() )
        print('dates():');
        .period = period()
        
        if ( ! .period %in% names( dataset() ) ){
        cat('\n--dataset does not contain column:' , .period )
        cat( '\ndataset columns are:' , names( dataset() ) )
        return()
      }
        dates = dataset() %>% pull( !! rlang::sym( .period )) %>%
          unique

        # print( dates )
        print( max( dates ))
        print( 'end dates()')
        return( dates )

        })

    period = reactive({
      req( dataset() )
      print('period():')
      weekly = any( map_lgl( dataset() ,
                             ~any(class(.x) %in% 'yearweek'  )) )

      period = ifelse( weekly, "Week", "Month" )
      print('end period()'); print( period )
      return( period  )
    })

    # reporting_month_updates
    observeEvent(  dates() , {
      updateSelectInput( session, 'startingMonth' ,
              choices =  dates()  ,
              selected = min( dates(), na.rm = TRUE )
      )
      } )

    observeEvent(  dates() , {
      updateSelectInput( session, 'endingMonth' ,
              choices =  dates()  ,
              selected = max( dates(), na.rm = TRUE )
      )
      } )

    # Months and periods

    most_recent_period = reactive({
      req( period() )
      cat( '\nLooking for most recent ' , period() )
      mrp = max( dataset() %>%
                   pull( !! rlang::sym( period() ) ), na.rm = TRUE )
      # mrp = max( dataset()[ , 'Month'] , na.rm = TRUE )
      # mrp = max( dataset()$Month , na.rm = TRUE )
      cat( '\nmost recent ', period(),  'is', mrp )

      # if ( input$exclude_recent_month ){
      #   if ( period() == "Month" ) mrp = mrp - month(1)
      #   if ( period() == "Week" ) mrp = mrp - week(1)
      # }

      print( 'mrp:') ; print(mrp)
      return( mrp )
    })

    period = reactive({
      print('period():')
      req(dataset())

      weekly = any( map_lgl( dataset() ,
                             ~any(class(.x) %in% 'yearweek'  )) )

      period = ifelse( weekly, "Week", "Month" )
      print('end period()'); print( period )
      return( period  )
    })

    d = reactive({

      req( dataset() )
      req( period() )
      print( 'd:')
  
      .period = period()
      data = dataset()  %>% mutate( period = !!rlang::sym( .period ))
      
      
      if ( !is_empty( input$level2 ) ){
        print( paste( 'filtering data by' , levelNames()[2] , "=" , input$level2 ) )
        data = data %>% 
          filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
        
        print( paste( 'data filtered by level2 has' , nrow( data ), 'rows' ))
        # glimpse( data )
      }
  
      if ( !is_empty( input$level3 ) ){
      print( paste( 'filtering data by' , levelNames()[3] , "=" , input$level3 ) )
      data = data %>% 
        filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
      
      print( paste( 'data filtered by level3 has' , nrow( data ), 'rows' ))
      # glimpse( data )
      }
  
    if ( !is_empty( input$level4 ) ){
        print( paste( 'filtering data by' , levelNames()[4] , "=" , input$level4 ) )
        data = data %>% 
          filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
        
        print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
        # glimpse( data )
    }
      
    if ( !is_empty( input$level5 ) ){
        print( paste( 'filtering data by' , levelNames()[5] , "=" , input$level5 ) )
        data = data %>% 
          filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )
        
        print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
        # glimpse( data )
      }
  
    print( 'nrow( d )' ); print( nrow( data ))
    
    if ( input$level %in% 'leaf'){  
      data = data %>% filter( effectiveLeaf == TRUE )
    } else {
      data = data %>% filter( levelName  %in% input$level  )
    }
  
  # if ( input$exclude_recent_month ) data = data %>% 
  #   filter( !! rlang::sym( period() ) <= most_recent_period() )
  
    if ( input$source %in% 'Original' ){
      data = data %>% mutate( dataCol = original )
    }  
    
    if ( input$source %in% 'Cleaned' ){
      print( paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      data = data %>% mutate( dataCol = ifelse( seasonal3, original, NA  ) )
  
      print( paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
  
  # print( 'd: max period ' ); print( max( d$period ))
  print( 'd: max period ' ); 
  print( max( data %>% pull( period  ) , 
              na.rm = TRUE ))
  # print( max( data$Month , na.rm = TRUE ))
  
  # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
  if ('mad15' %in% names( data )) data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
  if ('mad10' %in% names( data )) data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
  if ('mad5' %in% names( data )) data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
  if ('seasonal5' %in% names( data )) data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
  if ('seasonal3' %in% names( data )) data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
    
  print( 'end d()' )
  return( data )
})
    
        #  Reports

    orgunit.reports = reactive({ 
      req( input$data_categories )
      req( most_recent_period() )
      req( period() )
      
      print( 'orgunit.reports()' )
      
      mrm = most_recent_period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      data = d()
      
      if ( !input$count.any & !input$all_categories )  data = 
        data %>% filter( data %in% input$data_categories )
        
      .period = period()
    
      o.r. = 
        data %>% as_tibble() %>% ungroup %>%
        
        mutate(
    
        calendar_year = year( !! rlang::sym( .period )  )
         
      ) %>%
      rename( year =  {{ year_var }} ) 
      
        
      o.r. = setDT(o.r.)[, .( n_periods = uniqueN( get( .period ) )), 
                       by = c( 'year' , 'orgUnit' ) ] %>%
        as_tibble() %>%
      # group_by( year , orgUnit ) %>%
      # summarise( n_periods = n_distinct( !! rlang::sym( period() )  ) 
      #            # , max_month = max( Month ) 
      #            ) 
      mutate( n_periods = factor( n_periods ) ,
              year = factor( year ) )
      
      #print( 'o.r:') ; # print(head(o.r))
      print( 'end orgunit.reports' )
      return(o.r.)
    })
    
    annual.reports = reactive({ 
      req( orgunit.reports() )
      print( 'annual reports()' )
      
      or = orgunit.reports() 
      # print( 'annual reports() or:' ); print( names(or))
      
      ar = setDT(or)[, .( n = .N ), 
                       by = c( 'year' , 'n_periods' ) ] %>%
        as_tibble()
      
      # group_by( year ,  n_periods ) %>%
      # summarise( n = n() )
      print( 'end annual reports' )
      return( ar )
    })
    
    orgunit.monthly.reports = reactive({ 
      req( input$data_categories )
      print( 'orgunit.monthly.reports():' )
      
      # mrp = most_recent_period()
      .period = period()
      
      year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )
      
      data = d()
      
      if ( !input$count.any & !input$all_categories   ) data = data %>% filter( data %in% input$data_categories )
      
      o.m.r = 
        data %>% as_tibble() %>% ungroup %>%
    
        mutate(
               calendar_year = year( !! rlang::sym( .period )  )
         
      )  %>%
      rename( year =  {{ year_var }} ) 
      # %>%
      # mutate( year = factor( year ) )
    
      print( 'end orgunit.monthly.reports()' )
      # print(head(o.m.r))
      return(o.m.r)
    })
    
    monthly.reports = reactive({ 
      req( orgunit.monthly.reports() )
      req( period() )
      print( 'monthly reports():' )
      
      .period = period()
     
      o.m.r = orgunit.monthly.reports() 
      
      # print('monthly.reports() o.m.r'); print( names(o.m.r) )
      
      m.r = setDT(o.m.r)[, .( n = uniqueN( orgUnit ) ), 
                       by = c( "year" , .period  ) ]  %>%
        as_tibble()
      
      # group_by( year , !! rlang::sym( .period )   ) %>%
      # summarise( n = n_distinct( orgUnit ) )
      
      # print('m.r') ; glimpse(m.r)
      print( 'end monthly reports()' )
      # glimpse( m.r)
      return(m.r)
    })
    
    facilities = reactive({
      print( 'facilities' )
      req( orgunit.reports() )
      
      f = orgunit.reports() %>%
      ungroup() 
      
      f = setDT(f)[, .( Total = uniqueN( orgUnit ))] %>%
        as_tibble() %>%
        # summarise( Total = n_distinct( orgUnit ) ) %>%
        pull( Total)
      
      print( 'end facilities' )
      return(f)
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
          
      print( "plot1 selected$x:" ) ;  print( paste( selected$x , selected$panel ) )
      return( selected )
    })
    
    observeEvent( input$plot2_brush  , {
      # glimpse( input$plot1_brush )
      selected$chart = 2
      
      selected$x = round( 
        seq( input$plot2_brush$xmin , input$plot2_brush$xmax , by = .5 )
      ) %>% unique %>% as.integer()
      
      selected$panel = input$plot2_brush$panelvar1
          
      print( "plot_2_selected$x:" ) ;  print( selected$x ) ; print( selected$panel ) 
      return( selected )
    })

    selectedOUs <- reactive({
      print( 'selectedOUS()' )
      tic()
      req( input$endingMonth )
      
      if ( input$mostReports ){
     print( "determining most frequently reported facilities..." ); 
     print(input$startingMonth  ) ; print( input$endingMonth )
    
    data = d()
    
    if ( !input$count.any & !input$all_categories  ){
        print( input$all_categories )
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
       data = data %>% as_tibble %>%
       filter( 
         period >=  yearweek( input$startingMonth )  ,
         period <=  yearweek( input$endingMonth )  
               )
     } 
    
     mr = data %>% 
       distinct( !! rlang::sym( period() ) , orgUnit ) %>%
       group_by( orgUnit ) %>%
       summarise( n = n() ) %>%
       arrange( desc( n ))
     
     print( "mr" ); print( summary( mr$n ) )

     s = mr %>%
       filter( n == max( mr$n ) ) %>%
       pull( orgUnit ) %>% unique
     
     print( "mostReports selectedOUs:" ); toc() ##print( selectedOUs )
     return( s )
     }
  
      if( is.null( selected$x ) ) return( NULL )
  
      if ( selected$chart == 1 ){
      select_month =  as.numeric( orgunit.reports()$n_periods ) %in% selected$x
      selectedRows = select_month &
         orgunit.reports()$year %in% selected$panel 
      s = orgunit.reports()[ selectedRows, ]$orgUnit %>% unique
      
      

    } else {
          select_month = month( orgunit.monthly.reports() %>%
                                  pull( !! rlang::sym( period() ) ) ) %in% selected$x
          selectedRows = select_month &
            year( orgunit.monthly.reports() %>%
                    pull( !! rlang::sym( period() ) ) ) %in% selected$panel
          s = orgunit.monthly.reports()[ selectedRows, ]$orgUnit %>% 
            unique
          }

      print( "end selectedOUs:" ); toc()  # print( selectedOUs )
      return( s )
    })
  
    x.annual = reactive({
      print( 'x.annual()' )
      tic()
      x.a = orgunit.reports() %>% 
            filter( orgUnit %in% selectedOUs() )   # %>%  
            # group_by( year , n_periods ) %>%
            #   summarise( n =  n_distinct( orgUnit ) )
      
      # data.table speed up over dplyr
      x.a = setDT(x.a)[, .( n = uniqueN( orgUnit )), 
                       by = c( 'year' , 'n_periods' ) ]  %>%
        as_tibble()
    
      print('end x.annual:') ; toc();  # print( x.a )
      return( x.a )
  
})

    x.months = reactive({
      # req( keeprows() )
      tic()
      print( 'x.months()' )
      
      .period = period()
      
      x.m = orgunit.monthly.reports() %>% 
            filter( orgUnit %in% selectedOUs() ) 
      
      x.m = setDT(x.m)[, .( n = uniqueN( orgUnit )), 
                       by = c( 'year' , .period )   ]  %>%
        as_tibble()
      
  
      # %>%
      #       group_by( year , !! rlang::sym( period() )  ) %>%
      #       summarise( n = n_distinct( orgUnit ) )
      
      print('end x.months:') ; toc() ; # glimpse( x.m )
      return( x.m )
  
})

    plotData = reactive({
   print( 'plotData():')
   req( d() )
   req( input$data_categories )
  
    data = d() %>%
      mutate( Selected = 'All' )
    
    # filter to selected category
    print( 'plotData filtered by' ); print( input$data_categories )

    if ( !input$all_categories )  
      data = data %>% filter( data %in% input$data_categories )
    
    # Add var for selected ous
    cat( '\n plotData length( selectedOUs()): ' , length( selectedOUs())  )
    
   if ( length( selectedOUs()) > 0 ) data  = data %>%
      mutate( Selected = ifelse( 
        orgUnit %in% selectedOUs() , 
        'Reporting Each Period', 
        'Inconsistent Reporting' )
      )
    
    cat( '\nend  plotData()')  ; # print( names( data )) 
    # TESTING
    saveRDS( data , "plotData.rds" )
  return( data )
})

    group_by_cols = reactive({
    # req( input$split )
    print( "group_by_cols():")
    group_by_cols =  c( period() , 'orgUnit', 'Selected',
                        'dataSet' , 'data' ) 
    
    group_by_cols = c( group_by_cols, levelNames() )
  
    
    if ( input$split != 'None' ) group_by_cols = c( group_by_cols , input$split )
    
    # if ( length( selectedOUs() > 0 ) ) 
      # group_by_cols = c( group_by_cols , 'Facilities' )
    
    # # If not merge when total, show separate datsets
    # if ( !input$merge & input$all_categories ) group_by_cols = c( group_by_cols , 'dataSet' )
    #   
    cat( "\nend group_by_cols()" , unique( group_by_cols )  )
    return( unique( group_by_cols ) )

})

    data.total = reactive({
  req( plotData() )
  # req( group_by_cols() )
  req( period() )
  cat( '\ndata.total():' )

  
     .period = period()
     cat( '\n.period:' , .period )
     # if ( input$merge & input$all_categories ){
    
    .group_by_cols =  group_by_cols()  
    cat( '\n# data.total .group_by_cols:' , .group_by_cols )

    # Total categories by facilities and datasets
    data = plotData() 
    # %>% 
    #   group_by(  !!! dataset_group_by_cols  ) 
    
    
    # testing exogenous regressors
    # if (input$covariates %in% c( 'ipti' , 'doses' ) ){
    #   data = setDT(data)[ , .( total = sum( dataCol , na.rm = TRUE  ) ,
    #                            doses = sum( doses, na.rm = TRUE ) ,
    #                            ipti = sum( ipti, na.rm = TRUE )
    #                            ) ,
    #                        by = .group_by_cols ] %>% 
    #   as_tibble()
    # } else {
    #       
    #   data = setDT(data)[ , .( total = sum( dataCol , na.rm = TRUE  ) ) ,
    #                        by = .group_by_cols ] %>% 
    #   as_tibble()
    # }
    
    # Merge  datasets 
    # Set all dataSets to Combined and re-summaries taking mean
    # print( 'data.total datasets' );  print( dataSets() )
    cat( '\ninput$merge ', input$merge )
    cat( '\ndata datsets ' , unique(data$dataSet) ) 
    
    mergeDatasets = input$merge %>% str_replace_all( fixed("\n"), "") 
    cat( '\n# mergeDatasets:' , mergeDatasets )
    saveRDS( input$merge , 'merge.rds' )
    
    if ( !is.null( mergeDatasets )  ){

     # convert ; separated list  to vector of dataSets
    # m = input$merge %>% str_replace_all( fixed("\n"), "") %>%
    #  map( . , ~str_split( .x , ";") ) %>% unlist %>% paste0(., collapse = "|") 
    
    combineSelectDatasets = data %>%
              mutate( dataSet = dataSet %>% str_replace_all( fixed("\r\n"), "") 
            ) %>%
              mutate(
                  dataSet = ifelse( 
                      dataSet %in% mergeDatasets , 'Combined' ,
                      dataSet) ,
                  data = 'Total'
              ) %>% 
              setDT() %>%
              .[ , .(dataCol = sum( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
    
    cat('\n*Combining dataSets %in% input$merge:' , mergeDatasets )
    saveRDS( combineSelectDatasets , 'combineSelectDatasets.rds' )
    # Save dataSets for testing/developmen

    } else { combineSelectDatasets = data }
    
    # if ( merge ){  # TODO: selecting a dataset returns no rows.  ???
    # 
    #   cat( '\nmerging datasets...' )

      # Merge datasets (TODO: somehow dropping Selected )
      # NB: remove data from group_by because categories may align with 
      # dataSet so not possible to combine data
      
 
      # data.table sum/mean 
      mean.merge = input$dataset_merge_average 
      
      if ( mean.merge ) {
          cat( '\n** merge data.table MEAN') 
          
          dataMerge = combineSelectDatasets %>%
              mutate( dataSet = 'Merged') %>%
              setDT() %>%
              # Mean of dataSets within orgUnit
              .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
              # Sum across all orgUnit
              # .[ , .(dataCol = sum( dataCol , na.rm = TRUE  )) ,by =  .(dataSet, Month )  ] %>%
              # as_tibble
              
          
        #   dataMerge = data %>%
        #      mutate(  data = 'Total' ) 
        #   
        #   # Sum data values within dataSet, then take mean when same orgUnit submits both datasets in same month
        # CombineDataWithinDataset = setDT( dataMerge )[ , .(dataCol = sum( dataCol , 
        #                                      na.rm = TRUE  )) ,
        #                    by =  .group_by_cols ] %>% 
        #       as_tibble() %>%
        #       mutate( dataSet = 'Combined' )
        #   
        #   dataMerge = setDT( CombineDataWithinDataset )[ , .(dataCol = mean( dataCol , 
        #                                      na.rm = TRUE  )) ,
        #                    by =  .group_by_cols ] %>% 
        #  as_tibble()
        #       
      # } else {
      #    cat( '\n** merge data.table SUM') 
      #     
      #    dataMerge = data %>%
      #        mutate( dataSet = 'Combined' , data = 'Total' ) 
      #    
      #    dataMerge = setDT( dataMerge )[ , .(dataCol = sum( dataCol , 
      #                                        na.rm = TRUE  )) ,
      #                      by = .group_by_cols  ] %>% 
      #   as_tibble()
      # }
      
      
      cat( '\ndataMerge done' );  # glimpse( dataMerge )
      
    #   dataNotMerge = data %>%
    #     mutate( dataSet = dataSet %>% 
    #               str_replace_all(fixed("\r"), "") ) %>%
    #     filter( ! dataSet %in% input$merge ) 
    #   
    #   # print( 'dataNotMerge' );  glimpse( dataNotMerge )
    #   
    #   if ( nrow( dataNotMerge ) > 0 & input$dataset_merge  ){
    #     
    #    data = bind_rows( dataMerge ,
    #                      dataNotMerge %>%
    #                        mutate( dataSet = 'Combined' )
    #                      )  
    # 
    #      data = setDT( data )[ , .(total = sum( total , na.rm = TRUE  ) ) ,
    #                   by = .group_by_cols ] %>%
    #        as_tibble()
    #     
    # 
    #    } else  {
    #     data  = bind_rows( dataMerge , dataNotMerge )
    #     print( 'data.total duplicates' )  
          # }
    } else {
        dataMerge = combineSelectDatasets
        # cat('\n glimpse( dataMerge )\n' ); print(glimpse( dataMerge ))
    }
    
    # saveRDS( dataMerge, 'dataMerge.rds' )
    # print( dataMerge %>% duplicates %>% glimpse )

  key.cols = setdiff( group_by_cols() , .period ) 
  
  data.total = 
      dataMerge %>% 
      as_tsibble( index = !! rlang::sym( .period )  , 
                  key =  all_of(  {{key.cols}} ) ) %>%
      # fill_gaps( .full = TRUE  ) %>%
      mutate( 
              total = replace_na( dataCol , 0) 
              )  # for plotting, replace missing with zero 
  
  print( 'data.total finalized' ); # print( toc())

  # test:
  # saveRDS( data.total, 'data.total.rds')
  
  return( data.total )
    

})
      
    num_facilities = reactive({
        req( data.total() )
        print('num_facilities()')
        .d = data.total()
        l = length( unique( .d$Selected ) )
        print( paste( 'number of Facilities', l ) )
        return( l )
      })
      
    num_datasets = reactive({
        req( data.total() )
        print('num_datasets()')
        .d = data.total()
        l = length( unique( .d$dataSet ) )
        print( paste( 'number of dataSets', l ) )
        return( l )
      })
      
    hts = reactive({   
        print( "hts():" )
      
        
        adms = backtick( levelNames() )
        
        if (input$hts){ 
          hts = paste( adms, collapse = "/" ) 
        } else {
          hts = paste( adms[1:as.integer(input$hts_level)] , 
                       collapse = "/" ) 
        }
        
        hts = paste( "(" , hts , ")" )
        
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
          paste( input$split , '*' ,  hts )
        # 
        # Cross by selected and split
        # if ( length( selectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
        #   paste( input$split ,  ' * Facilities * (', hts , ')' )
        
        print( "end hts():" ); print( hts )
      
        return( hts )
      })
      
    data.hts = reactive({
        req( data.total() )
      
        print( 'data.hts():' );   tic()
      
        .d = data.total()
        
        # testing exogenous vaiables
        if ( input$covariates %in% c('ipti' , 'doses' ) ){
          .d = .d %>%
          aggregate_key(  .spec = !!rlang::parse_expr( hts() ) ,
                          total = sum( total , na.rm = T ) ,
                          ipti = sum( !!rlang::parse_expr( 'ipti' ) , na.rm = T ) ,
                          doses = sum( !!rlang::parse_expr( 'doses' ) , na.rm = T )
                          ) 
        } else {
            .d = .d %>%
          aggregate_key(  .spec = !!rlang::parse_expr( hts() ) ,
                          total = sum( total , na.rm = T )
                          ) 
        }
        
        print( 'end data.hts():' ) ; toc()
          return(.d)
      })
      
    aggregatePlotData = reactive({
         req( data.hts() )
         print( 'aggregatePlotData():' )
        
        .d = data.hts() %>% 
            filter( 
              ! is.na( !! rlang::sym( levelNames()[1] ) )
              , is_aggregated( !! rlang::sym( levelNames()[1] ) )
            ) %>%
             mutate( 
               grouping_var = 'Total' )
           
           if ( num_datasets() > 1 ){
             print( 'num_datasets()>1:') ;
             .d = .d %>% 
             filter( !is_aggregated( dataSet ) ) %>%
             mutate( dataSet = as.character( dataSet ) %>%
                 str_remove_all( "<aggregated>" ) ,
                 grouping_var = dataSet )
             print( unique(.d$dataSet))
           }  
           
           if ( num_facilities() > 1 ){
             print( 'num_facilities()>1:') ; 
             .d = .d %>% 
             filter( !is_aggregated( Selected )  ) %>%
             mutate( Selected = as.character( Selected ) %>%
                 str_remove_all( "<aggregated>" )  )
             
             print( unique(.d$Selected))
           }  
              
          # if split, remove aggregate grouping
           if ( !input$split %in% 'None' ){
             print( '!input split none') ; print( input$split )
             .d = .d %>%
               filter( !is_aggregated( !! rlang::sym( input$split ) ) 
               ) %>%
               mutate( grouping_var = as.character( 
                 !! rlang::sym( input$split ) )
               )
             print( unique(.d$grouping_var) )
             # print( glimpse( .d ))
             
           } 
           
           print( 'end aggregatePlotData()' )
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
        print( 'plotAgregateValue():' )
      
        .d = aggregatePlotData()
        
        saveRDS(.d, 'plot3_data.rds')
        
        data.text = paste( unique( plotData()$data ),
                           collapse = " + " ) 
        
        # print( 'data.text'); print( data.text )
        
        .limits = c(0, NA)
        
           
        print('plotting aggregate data');
        
      
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
          print( 'unique dataSets'); print( unique( .d$dataSet ) )
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
          g = g + guides(color=guide_legend(title= input$split ))
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
          theme_minimal()  
          
        print( ' end plotAgregateValue()' )
      
        return( g )
      })
      
    output$plot_values <- renderPlot({  plotAgregateValue()  })
    
    

    return( list( 
        # dataset = reactive({ dataset() }) ,
        # directory = reactive({ data.folder() })
        ))
} )
}


