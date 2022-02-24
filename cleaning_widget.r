cleaning_widget_ui = function ( id ){
        ns <- NS(id)  
        
tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-left'
            # , margins = c(70, 1200)
          ) ,
          
        tabsetPanel( type = "tabs", 
                     
         tabPanel( "Outliers",  
                   
                sidebarLayout(
                    sidebarPanel( 
                      
                      radioButtons( ns("dataElement") , label = "DataElement_Category:" ,
                                     choices = c('') ,
                                     selected = NULL ) ,
                      
                      selectInput( ns("selectOrgType") , label = "Filter results" ,
                                   choices = c( 'Facilities only', 'Admin only' , 'All' ) ,
                                   selected = 'Facilities only' )
                    ) ,
                    
                    mainPanel( 
                      h5( 'Use the buttons to search for extreme values using deviation from the median, median absolute deviation (MAD), and then seasonally adjusted outliers') ,
                      actionButton( ns("determineExtremeValues") , 
                                    "Search for Extreme Values" , style='margin-top:25px' 
                      )   ,
                      
                      actionButton( ns("determineSeasonalOutliers") , 
                                    "Search for Seasonal Outliers" , style='margin-top:25px' 
                      )  ,
                      
                      tabsetPanel( type = "tabs",
                                
                             
        
                        tabPanel( "Outlier Summary",  
                                  textOutput( ns("outlierSummaryText")) ,
                                  tableOutput( ns("dqaTable") )  
                        ) ,
                        
                         tabPanel( "Mean Absolute Scaled Error",  
                                    fluidRow( style = "height:60vh;",
                                    plotOutput( ns("mase.summary") )
                                  )
                                  
                        ) ,
                       
                        tabPanel( "Inspect",  style = "height:70vh;" ,
                                fluidPage( 
                                  fluidRow( style = "height:10vh;",
                                    h5( 'Select orgUnit having error')     ,   
                                    selectInput( ns('madError'), 'Median Absolute Deviation' ,
                                                 choices = c('mad15', 'mad10', 'mad5', 'seasonal5', 'seasonal3' ) ) ,
                                    
                                    selectInput( ns('seasonalError'), 'Seasonally Adjusted error' ,
                                                 choices = c( 'seasonal5', 'seasonal3' ) ) ,
                                    
                                    selectInput( ns( 'flaggedOrgUnit') , 'Select orgUnit having this error' ,
                                                 choices = "" )
                                  ) ,
                                  
                                  fluidRow( style = "height:50vh;",
                                    plotOutput( ns("inspect") )
                                  ))
                              )
                        )
                      )
            
             ) 
        ) ,
        
        tabPanel( "Select Regions-Facilities" ,
                  
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
                                                  multiple = TRUE  ) 

                  ) , 
        tabPanel( "Dataset snapshot", tableOutput( ns("contents") ) ),
          
        tabPanel( "Summary (under construction)",
                        html("<div style='display:flex;'>") ,
                          htmlOutput( ns("profileSummary") ) ,
                        html("<div>") ,

              ) ,
    )
)
} # ui
        

cleaning_widget_server <- function( id , 
                                     directory_widget_output = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ,
                                     reporting_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {

  # Dependencies ####
    data.folder = reactive({ directory_widget_output$directory() })
    indicator = reactive({ data_widget_output$indicator() })
    formulas = reactive({ data_widget_output$formulas() })
    dataset.file = reactive({ data_widget_output$dataset.file() })
    dataset = reactive({ data_widget_output$dataset() })
    data1 = reactive({ data_widget_output$data1() })
    formula_elements = reactive({ data_widget_output$formula_elements() })
    dataElements = reactive({ metadata_widget_output$dataElements() })  
    categories = reactive({ metadata_widget_output$categories() })  
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    ousTree = reactive({ metadata_widget_output$ousTree() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    dates = reactive({ reporting_widget_output$dates() })
    data.hts = reactive({ reporting_widget_output$data.hts() })
    levelNames = reactive({ reporting_widget_output$levelNames() })
    period = reactive({ reporting_widget_output$period() })
    split = reactive({ reporting_widget_output$split() })
    startingMonth = reactive({ reporting_widget_output$startingMonth() })
    endingMonth = reactive({ reporting_widget_output$endingMonth() })
    num_datasets = reactive({ reporting_widget_output$num_datasets() })
    num_facilities = reactive({ reporting_widget_output$num_facilities() })
    plotData = reactive({ reporting_widget_output$plotData() })
    caption.text = reactive({ reporting_widget_output$caption.text() })
    # data = reactive({ reporting_widget_output$d() })
    # data.total = reactive({ reporting_widget_output$data.total() })
    
    
  # orgUnits ###
    
    # level 2
    observeEvent( levelNames()  , {  
        req( ousTree() )
        l2 = ousTree() %>% 
            pull( !! rlang::sym(levelNames()[2]) ) %>% 
            unique 
        
        cat( '\n* updating outliers level2' )
        updateSelectInput( session, 'level2' ,
                             choices = l2[!is.na(l2)]
                             , selected = NULL
                                    )
        } )

    # level 3
    observeEvent( input$level2 , {
      req( input$level2 )

      l3 = ousTree() %>%
        filter(
          !! rlang::sym( levelNames()[2] ) %in% input$level2
          ) %>%
        pull( !! rlang::sym( levelNames()[3]  )
              ) %>%
        unique %>% str_sort()

      cat( '\n* updating level3' )
      updateSelectInput( session, 'level3' ,
                                choices = l3[!is.na(l3)]
                                , selected = NULL
                                )
    } )

    # level 4
    observeEvent( input$level3 , {
      req( input$level3 )

      l4 = ousTree() %>%
        filter(
          !! rlang::sym( levelNames()[3] ) %in% input$level3
          ) %>%
        pull( !! rlang::sym( levelNames()[4]  )
              ) %>%
        unique %>% str_sort()

      cat( '\n* updating level4' )
      updateSelectInput( session, 'level4' ,
                                choices = l4[!is.na(l4)]
                                , selected = NULL
                                )
    } )
    
    # level 5
    observeEvent( input$level4 , {
      req( input$level4 )

      l5 = ousTree() %>%
        filter(
          !! rlang::sym( levelNames()[4] ) %in% input$level4
          ) %>%
        pull( !! rlang::sym( levelNames()[5]  )
              ) %>%
        unique %>% str_sort()

      cat( '\n* updating level5' )
      updateSelectInput( session, 'level5' ,
                                choices = l5[!is.na(l5)]
                                , selected = NULL
                                )
    } )
    
  # data names  ####
    observe({
      req( data1() )
      outlierData$df_data = data1()
      updateRadioButtons( session , "dataElement" , 
                       choices = outlierData$df_data$data %>% unique ) 
     
    })
  
  # outlierData ####
    
     outlierData <- reactiveValues( df_data = NULL ) 
 
  # scan for MAD outliers  ####
    scanForMAD = reactiveVal( FALSE )
    rerunMAD = reactiveVal( FALSE ) 
    
    # Option to rerun MAD
    rerunMadModal <- function() {
       ns <- NS(id) 
       modalDialog( title = "Seasonal outlier flags already present in data", 
                         easyClose = TRUE ,
                         size = 'm' ,
                         footer = tagList( modalButton( "Cancel" ),
                                           actionButton( ns("rerunMAD"), "Re-Run scan for MAD Outliers")
        )
                         )
   }
   
    observeEvent( input$determineExtremeValues  , {
      req( outlierData$df_data )
      if ( 'mad15' %in% names( outlierData$df_data ) ){
        showModal( rerunMadModal() ) 
        outlierData$df_data = data1.mad()
        
      } else {
        
          scanForMAD( TRUE )  
          cat('\n * determine extreme values button:' , scanForMAD() )
          outlierData$df_data = data1.mad()
            }
 } )
    
    observeEvent( input$rerunMAD , {
         req( data1() )
         
         scanForMAD( TRUE ) 
         rerunMAD( TRUE )
         cat('\n * rerun MAD outliers button:' , rerunMAD() )
         outlierData$df_data = data1.mad()
})

    data1.mad = reactive({
    req( outlierData$df_data )
    cat('\n* data1.mad' )
    
    # if data1 already has mad columns, return data1
    if ( !rerunMAD() & 'mad10' %in% names( outlierData$df_data ) ){
      cat('\n - mad cols already in data1' )
      return( outlierData$df_data )
    } 
 
    if ( scanForMAD() ){
      cat('\n* data1.mad search')
    
      d = data1()
      nrow1 = nrow( d )
      if ( nrow1 == 0 ){
        cat('\n - nrow1 = 0')
        return()
      } else { cat('\n - data1() has' , nrow1 , 'rows')}
      
      # remove duplicate rows because downloads may create duplicates
      u = d %>% as.data.table() %>% unique 
      nrow2 = nrow( u )
      cat('\n - There were', nrow1-nrow2, 'duplicates' )
    
     cat( '\n - Scanning for repetive key entry errors')
     key_entry_errors =
       count( as_tibble( d %>% 
                       filter( nchar(original)>3 , 
                              effectiveLeaf ) ) , 
             original ) %>% 
       arrange(-n) 
   
    # Default: values where the number happens at least 3 > than 
     # medianof the top 10 rows 
     key_entry_errors = key_entry_errors %>% 
       filter(  n > 3 * median( 
         key_entry_errors %>% filter( row_number()<11 )  %>%
           pull( n ) )
         ) %>% pull( original )
  
     # print( head( key_entry_errors ) )
     if ( is_empty( key_entry_errors )  ) key_entry_errors = NA
       
     
    cat( '\n - scanning for MAD outliers')
    .total = length( key_size( d ) )
  
    .threshold = 50

    withProgress(     message = "Searchng",
                        detail = "starting ...",
                        value = 0, {
      
    data1.mad = d %>%  
        group_by( orgUnit, data.id ) %>%
        mutate(
          .max = ifelse( 
            grepl("jour|day", data ) &
            grepl("out|rupture", data )   &
            effectiveLeaf
            , 31, NA  )  
          ) %>%
        mutate( 
            mad15 = extremely_mad( original , 
                                   deviation = 15 , 
                                   smallThreshold = .threshold ,
                                   key_entry_error = key_entry_errors ,
                                   maximum_allowed = .max , 
                                   logical = TRUE, .pb = NULL , 
                                   .progress = TRUE ,
                                   total = .total ) 
            , mad10 = extremely_mad( ifelse( mad15, original , NA ), 
                                     deviation = 10 , 
                                     smallThreshold = .threshold ,
                                     maximum_allowed = .max , 
                                     logical = TRUE ) 
            , mad5 = extremely_mad( ifelse( mad10, original , NA ), 
                                    deviation = 5 , 
                                    smallThreshold = .threshold * 2 ,
                                    maximum_allowed = .max , 
                                    logical = TRUE ) 
        )
      
      
    })
    
    scanForMAD( FALSE )
    rerunMAD( FALSE )
    
    showModal(
          modalDialog( title = "Finished scanning for extreme values", 
                       easyClose = TRUE ,
                       size = 'm' ,
                       footer = "(click anywhere to close dialog box)"
                       )
          )  
    
    # Save data for next time...
    cat('\n - saving data1.mad to replace dataset')
    saveRDS( data1.mad , paste0( data.folder(), dataset.file() ) )
    removeModal()
    
    } else( data1.mad = data1() ) 
    
    return( data1.mad )
    })
 
  # scan for Seaonal outliers  ####
    searchForSeasonalOutliers = reactiveVal( FALSE ) 
    rerunSeasonalOutliers = reactiveVal( FALSE ) 
    
    # Option to rerun seasonal outliers
    rerunSeasonalModal <- function() {
       ns <- NS(id) 
       modalDialog( title = "Seasonal outlier flags already present in data", 
                         easyClose = TRUE ,
                         size = 'm' ,
                         footer = tagList( modalButton( "Cancel" ),
                                           actionButton( ns("rerunSeasonal"), "Re-Run Seasonal Outliers")
        )
                         )
   }
 
    observeEvent( input$determineSeasonalOutliers  , {
      req( outlierData$df_data )
      if ( 'seasonal5' %in% names( outlierData$df_data ) ){
         showModal( rerunSeasonalModal() ) 
         # outlierData$df_data = data1.seasonal()
        
      } else {
        
          searchForSeasonalOutliers( TRUE )  
          cat('\n * determine seasonal outliers button:' , searchForSeasonalOutliers() )
          outlierData$df_data = data1.seasonal()
          
      }
 } )

    observeEvent( input$rerunSeasonal , {
         req( data1() )
         
         searchForSeasonalOutliers( TRUE ) 
         rerunSeasonalOutliers( TRUE )
         cat('\n * rerun seasonal outliers button:' , searchForSeasonalOutliers() )
         outlierData$df_data = data1.seasonal()
})
    
    data1.seasonal = reactive({
      req( outlierData$df_data )
      cat('\n* data1.seasonal')
      
      # if data1 already has seasonal columns, return data1
      if ( !rerunSeasonalOutliers() & 'seasonal3' %in% names( outlierData$df_data ) ){
        cat('\n - seasonal cols already in data1' )
        return( outlierData$df_data )
      } 
      
      if ( searchForSeasonalOutliers()  ){
      cat('\n* data1.seasonal search')
        
      # Stop if mad10 not in dataset
      if ( !'mad10' %in% names( data1() ) ){
            showModal(
              modalDialog( title = "Please search for extreme values first", 
                           easyClose = TRUE ,
                           size = 'm' ,
                           footer = "(click anywhere to close dialog box)"
                           )
              )
        searchForSeasonalOutliers( FALSE )
        return( outlierData$df_data )
      }
    
       d = outlierData$df_data
 
       cat( '\n - scanning for Seasonal outliers')
      .total = length( key_size( d ) )
       cat( '\n - .total' , .total )
  
      .threshold = 50

      withProgress(  message = "Seasonal Outliers",
                        detail = "starting ...",
                        value = 0, {
      
      data1.seasonal = d %>%  
        group_by( orgUnit, data.id ) %>%
        mutate(
          
          expected = unseasonal(  original , 
                                  smallThreshold = .threshold * 2  , 
                                  logical = FALSE , # Returns forecasted value
                                  .progress = TRUE ,
                                   total = .total 
                                  ) ,
          
          seasonal5 = unseasonal(  ifelse( mad10, original , NA) , 
                                  smallThreshold = .threshold * 2  , 
                                  deviation = 5 ,
                                  logical = TRUE ) ,
          
          seasonal3 = unseasonal(  ifelse( seasonal5, original , NA) , 
                                  smallThreshold = .threshold * 2 , 
                                  deviation = 3 ,
                                  logical = .total )  
      )
        })  
        
        showModal(
              modalDialog( title = "Finished scanning for seasonal values", 
                           easyClose = TRUE ,
                           size = 'm' ,
                           footer = "(click anywhere to close dialog box)"
                           )
              )
        
    cat('\n - saving data1.seasonal to replace dataset')
    cat('\n - names(data1.seasonal):', names(data1.seasonal) )
    
    saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
    removeModal()
    
    # Testing
    # saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
    searchForSeasonalOutliers( FALSE )
    rerunSeasonalOutliers( FALSE )
    
    return( data1.seasonal )
  }
    })
   
  # Summary ####
    # d.mase
    data1.summary = reactive({
        req( outlierData$df_data )
        cat( '\n* data1.summary')
        
        d = outlierData$df_data
        if (! 'expected' %in% names( d ) ){
          cat( '\n - "expected" column not found')
          return()
          }
          
        d.mase =  d %>%  as_tibble() %>%
            group_by( orgUnit, data.id ) %>%
            summarise( 
              mase = mase( actual = original, predicted = expected  ) ,
              MASE = 100 * mase  ,
              n = sum( !is.na( original )) 
            )
          
        return( d.mase )

  })
    
    output$mase.summary <- renderPlot({
      req( data1.summary() )
      cat( '\n* output$mase.summary')
      
      if ('MASE' %in% names( data1.summary() )){
        
        ggplot( data1.summary() , aes( x = MASE ) ) + 
        geom_histogram( binwidth = 1 )
      }
      
    })

    output$contents <- renderTable({
      cat('\n* contents')
      req( outlier.dataset() )
      head( outlier.dataset() , n = 100 )
    })
    
    output$profileSummary <- renderUI({ #describeData()
      cat('\n* profileSummary')
      out <- print( dfSummary( data1() ,
                         graph.magnif = 0.75),
               style = "grid" ,
               method = 'render',
               omit.headings = TRUE,
               bootstrap.css = FALSE)
      out
    })
  
# Outliers #####
    outlier.dataset = reactive({
      cat( '\n* outlier.dataset')
      req( outlierData$df_data )
      
      d = outlierData$df_data
        # if (!is_empty( data1.seasonal()  )){
        #   cat('\n - data1.seasonal' )
        #   d = data1.seasonal() 
        # } else {
        #   d = data1.mad()
        # }
        # } else if( !is_empty( data1.mad() )) {
        #   cat('\n - data1.mad' )
        #   d = data1.mad()
        # } else{ 
        #   cat('\n - data1' )
        #   d = data1() 
        # }
      if ( 'mad10' %in% names(d) ) cat('\n - data has mad10' )
      if ( 'seasonal3' %in% names(d) ) cat('\n - data has seasonal3' )
      
      if ( 'effectiveLeaf' %in% names( d ) && input$selectOrgType %in% 'Facilities only'){
        cat('\n - data has effectiveLeaf' )
        d = d %>% filter( effectiveLeaf )
      } else if ( input$selectOrgType %in% 'Admin only' ){
        d = d %>% filter( !effectiveLeaf )
      } 
      
      # Filter by region/level
      # level2
      if ( !is_empty( input$level2 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[2] , "=" , input$level2 ) 
        d = d %>% 
          filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
      }
 
      # level3
      if ( !is_empty( input$level3 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[3] , "=" , input$level3 ) 
        d = d %>% 
          filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
      }
 
      # level4
      if ( !is_empty( input$level4 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[4] , "=" , input$level4 ) 
        d = d %>% 
          filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
      }
 
      # level5
      if ( !is_empty( input$level5 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[5] , "=" , input$level5 ) 
        d = d %>% 
          filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )
      }
      
      cat( '\n - done')
      return( d )
      })

    outlier.summary.cols = reactive({
      req( outlierData$df_data )
      cat('\n* outlier.summary.cols():')
      
      d = outlierData$df_data
      
      if ( 'seasonal3' %in% names( d )){
        cols = c('mad15', 'mad10', 'mad5','seasonal5' , 'seasonal3')
      } else if( 'mad5' %in% names( d ) ){
        cols = c('mad15', 'mad10', 'mad5' )
      } else { 
        cat('\n - no outlier cols found')
        output$outlierSummaryText = renderText({ 'No outlier flags found. Please run the outlier detection algorithms'})
        return() }
      
      cat('\n - ', cols )
      output$outlierSummaryText = renderText({ ''})
       
      return( cols)
      
    })
    
    outlier.summary = reactive({
        req( outlier.dataset() )
        req( outlier.summary.cols() )
        req( input$dataElement )
        
        cat('\n* outlier.summary' )
        d = outlier.dataset()
        cols = c( 'data' , outlier.summary.cols() )
  
        cat('\n - totals' )
        total = d %>%  as_tibble() %>%
          filter( data %in% input$dataElement ) %>%
          group_by( data ) %>%
          summarise( Total = sum( original , na.rm = T ) ,
                     # monthlyN = n() ,
                     N = sum( !is.na( original )))
  
        
        cat('\n - summary' )
        os = d %>% as_tibble() %>% 
          filter( data %in% input$dataElement , !is.na( mad15 ) ) %>%
          group_by_at( cols ) %>%
          summarise( n = sum( !is.na( original )) , 
                     total = sum( original , na.rm = T ) ,
                     max = max( original , na.rm = T ) %>% comma()
                     ) %>%
          inner_join( total , by = c( "data" ) ) %>%
          mutate( 
                     `%N` = percent( n / N ) ,
                     `%Total` = percent( total / Total )
                     )   %>%
          ungroup %>%
          select( !! cols  , n ,  `%N` ,  max , `%Total` , -data ) 
        
        cat('\n - summary has' , nrow(os) , 'rows')
        return( os )
    })
    
    output$dqaTable = renderTable( outlier.summary() ) 
    
  ## Visualize cleaning (Inspect )  ####
  errorFlag = reactive({
    req( outlier.dataset()) 
    req( input$madError )
    req( input$dataElement )
    cat( '\n* errorFlag():')
    
    # print( head( data1() ) )
    d = outlier.dataset() 
     
    # testing
    # saveRDS( d , 'data1.rds')
    # MAD Error
    if ( input$madError %in% names( d ) ){
            flag = unique( as_tibble( d ) %>% 
                   filter( 
                     ( !! rlang::sym( input$madError )  == FALSE ),
                           data %in% input$dataElement ) %>% 
                   distinct( orgUnit , orgUnitName )  
                 )
            cat( '\n -  nrow errorFlag() :', nrow( flag ) )
            
    } else {  return() }

    return( flag )
  })
  
  observeEvent(  nrow( errorFlag() ) > 0 , {
    updateSelectInput( session, "flaggedOrgUnit" , 
                       choices = paste0( errorFlag()$orgUnitName )
    )
  })
  
  plot.single.data.series = reactive({
    req( outlier.dataset() )

    cat('\n* plot.single.data.series' )
    
    if ( nrow( errorFlag() ) == 0 ) return()
  
    inspectOrgUnitData = outlier.dataset() %>% as_tibble() %>%
      filter( orgUnitName %in% input$flaggedOrgUnit 
             ,  data %in% input$dataElement
              )
    cat('\n* inspectOrgUnitData points:' , nrow(inspectOrgUnitData) )
    
    g = inspectOrgUnitData %>%
        ggplot( aes( x = Month, y = original,  group = data ) ) +
        geom_line( alpha = .25 ) +
        geom_point( aes( color = !! rlang::sym( input$madError ) 
                         # , shape = seasonal3 
                         )) +
        labs( title = paste( unique( inspectOrgUnitData$orgUnitName ), collapse = ",") )
    
    return( g )
    
  })
  
  output$inspect = renderPlot({ plot.single.data.series() })
  
 
  # Return ####
  return( list(
    data2 = reactive({ outlierData$df_data })
  ))
  
})
}  


