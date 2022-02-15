cleaning_widget_ui = function ( id ){
        ns <- NS(id)  
        
tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-left'
            # , margins = c(70, 1200)
          ) ,


    tabsetPanel( type = "tabs", 
          tabPanel( "Dataset snapshot", tableOutput( ns("contents") ) ),
          
          tabPanel( "Summary (under construction",
                        html("<div style='display:flex;'>") ,
                          htmlOutput( ns("profileSummary") ) ,
                        html("<div>") ,

              ) ,
         tabPanel( "Outliers",  
                     
                    actionButton( ns("determineExtremeValues") , 
                      "Search for Extreme Values" , style='margin-top:25px' 
                      )   ,
                      
                      actionButton( ns("determineSeasonalOutliers") , 
                      "Search for Seasonal Outliers" , style='margin-top:25px' 
                      )  ,
                   
                sidebarLayout(
                    sidebarPanel( 
                      
                      radioButtons( ns("dataElement") , label = "DataElement_Category:" ,
                                     choices = c('') ,
                                     selected = NULL )
                    ) ,
                    mainPanel( 
                      tabsetPanel( type = "tabs",
        
                        tabPanel( "Summary",  tableOutput( ns("dqaTable") ) ) ,
                       
                        tabPanel( "Inspect",  style = "height:90vh;" ,
                                fluidPage( 
                                  fluidRow( style = "height:30vh;",
                                            
                                    selectInput( ns('error'), 'Select error type' ,
                                                 choices = c('mad15', 'mad10', 'mad5', 'seasonal5', 'seasonal3' ) ) ,
                                    
                                    selectInput( ns( 'flaggedOrgUnit') , 'Select orgUnit having this error' ,
                                                 choices = "" )
                                  ) ,
                                  
                                  fluidRow( style = "height:55vh;",
                                    plotOutput( ns("inspect") )
                                  ))
                              )
                        )
                      )
            
             ) 
        )
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
    
 # scan for outliers and flag ####
    searchForExtremeValues = reactiveVal( FALSE )
    
    observeEvent( input$determineExtremeValues  , {
      req( data1())
      if ( 'mad15' %in% names( data1() ) ){
         showModal(
                    modalDialog( title = "Outlier flags already present in data", 
                         easyClose = TRUE ,
                         size = 'm' ,
                         footer=NULL
                         )
         ) 
      } else {
        
          searchForExtremeValues( TRUE )  
          cat('\n * determine extreme values button:' , searchForExtremeValues() )
          a = data1.mad()
            }
 } )
    
    data1.mad = reactive({
    req( data1() )
    if ( searchForExtremeValues() ){
      cat('\n* data1.mad')
    
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
  
     print( head( key_entry_errors ) )
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
    
    showModal(
          modalDialog( title = "Finished scanning for extreme values", 
                       easyClose = TRUE ,
                       size = 'm' ,
                       footer=NULL
                       )
          )  
    
    # SAVE
    saveRDS( data1.mad , paste0( data.folder(), dataset.file() ) )
    
    return( data1.mad )
  }
    })
 
    searchForSeasonalOutliers = reactiveVal( FALSE )   
   
    observeEvent( input$determineSeasonalOutliers  , {
      req( data1())
      if ( 'seasonal5' %in% names( data1() ) ){
         showModal(
                    modalDialog( title = "Outlier flags already present in data", 
                         easyClose = TRUE ,
                         size = 'm' ,
                         footer=NULL
                         )
         ) 
      } else {
        
          searchForSeasonalOutliers( TRUE )  
          cat('\n * determine seasonal outliers button:' , searchForSeasonalOutliers() )
          b = data1.seasonal()
          
      }
 } )
    
    data1.seasonal = reactive({
   
      if ( searchForSeasonalOutliers() ){
      cat('\n* data1.seasonal')
    
       d = isolate( outlier.summary.data() )
 
       cat( '\n - scanning for Seasonal outliers')
      .total = length( key_size( d ) )
  
      .threshold = 50

     withProgress(  message = "Seasonal Outliers",
                        detail = "starting ...",
                        value = 0, {
      
      data1.seasonal = d %>%  
        group_by( orgUnit, data.id ) %>%
        mutate(
          
          seasonal5 = unseasonal(  ifelse( mad10, original , NA) , 
                                  smallThreshold = .threshold * 2  , 
                                  deviation = 5 ,
                                  logical = TRUE , 
                                  .pb = NULL ,
                                  total = .total ) ,
          seasonal3 = unseasonal(  ifelse( seasonal5, original , NA) , 
                                  smallThreshold = .threshold * 2 , 
                                  deviation = 3 ,
                                  logical = TRUE )
      )
        })  
        
        showModal(
              modalDialog( title = "Finished scanning for seasonal values", 
                           easyClose = TRUE ,
                           size = 'm' ,
                           footer=NULL
                           )
              )
        
    # SAVE
    saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
    return( data1.seasonal )
  }
    })
   
  # Summary ####
    
  output$contents <- renderTable({
    req(dataset())
    head( dataset() , n = 100 )
  })

  output$profileSummary <- renderUI({ #describeData()
    
    out <- print( dfSummary( dataset() ,
                       graph.magnif = 0.75),
             style = "grid" ,
             method = 'render',
             omit.headings = TRUE,
             bootstrap.css = FALSE)
    out
  })
  
  
  # Outliers tab  
  observeEvent( data1()  , {
    req( data1() )
    if( nrow( data1() ) > 0 && 'data' %in% names( data1() ) ){
      cat('\n-update dataElement-')
      updateRadioButtons( session, 'dataElement' ,
                       choices =  data1()  %>% pull( data ) %>% unique )
    }
    cat('-done\n')
  })
    
  outlier.summary.data = reactive({
    cat( '\n* outlier.summary.data')
    
      if (!is_empty( isolate( data1.seasonal() ) )){
        cat('\n - data.seasonal' )
        d = data1.seasonal() 
      } else if(!is_empty( data1.mad() )) {
        cat('\n - data.mad' )
        d = data1.mad()
      } else{ 
        cat('\n - dataset' )
        d = dataset() 
      }
    
    return( d )
    })

  
  outlier.summary.cols = reactive({
    req( outlier.summary.data() )
    cat('\n* outlier.summary.cols():')
    
    d = outlier.summary.data()
    
    if ( 'seasonal3' %in% names( d )){
      cols = c('mad15', 'mad10', 'mad5','seasonal5' , 'seasonal3')
    } else if( 'mad5' %in% names( d ) ){
      cols = c('mad15', 'mad10', 'mad5' )
    } else { 
      cat('\n - no outlier cols found')
      return() }
    
    cat('\n - ', cols )
    return( cols)
    
  })
  
  outlier.summary = reactive({
      req( outlier.summary.data() )
      req( outlier.summary.cols() )
      req( input$dataElement )
      
      cat('\n* outlier.summary' )
      d = outlier.summary.data()
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
        group_by_at( cols   ) %>%
        summarise( n = sum( !is.na( original )) , 
                   total = sum( original , na.rm = T ) ,
                   max = max( total , na.rm = T ) %>% comma()
                   ) %>%
        inner_join( total , by = c( "data" ) ) %>%
        mutate( 
                   `%N` = percent( n / N ) ,
                   `%Total` = percent( total / Total )
                   )   %>%
        ungroup %>%
        select( !! cols  , n , max , `%N` ,`%Total`  ) 
      
      cat('\n - summary has' , nrow(os) , 'rows')
      return( os )
  })
  
  output$dqaTable = renderTable( outlier.summary() ) 
  
  ## Visualize cleaning (Inspect )  ####
  errorFlag = reactive({
    req( data1.mad()) 
    req( input$error )
    req( input$dataElement )
    cat( '\n* errorFlag():')
    # print( head( data1() ) )
     d = data1()
    # testing
    saveRDS( d , 'data1.mad.rds')
    if ( input$error %in% names( d ) ){
            flag = unique( as_tibble( d ) %>% 
                   filter( !! rlang::sym( input$error )  == FALSE ,
                           data %in% input$dataElement ) %>% 
                   distinct( orgUnit , orgUnitName )  
                 )
            cat( '\n -  nrow errorFlag() :', nrow( flag ) )
            
    } else {  return() }

    return( flag )
  })
  
  observeEvent(  nrow( errorFlag() ) > 0 , {
    updateSelectInput( session, "flaggedOrgUnit" , 
                       choices = paste0( errorFlag()$orgUnit )
    )
  })
  
  plot.single.data.series = reactive({
    req( data1.mad() )

    cat('\n* plot.single.data.series' )
    
    if ( length( errorFlag() ) == 0 ) return()
  
    inspectOrgUnitData = data1.mad() %>% as_tibble() %>%
      filter( orgUnit %in% input$flaggedOrgUnit )
  
    g = inspectOrgUnitData %>%
        ggplot( aes( x = Month, y = original,  group = data ) ) +
        geom_line( alpha = .25 , aes( linetype = data ) ) +
        geom_point( aes( color = !! rlang::sym( input$error ) 
                         # , shape = seasonal3 
                         )) +
        labs( title = paste( unique( inspectOrgUnitData$orgUnitName ), collapse = ",") )
    
    return( g )
    
  })
  
  output$inspect = renderPlot({ plot.single.data.series() })
  
 
  # Return ####
  return()
  
})
}  


