cleaning_widget_ui = function ( id ){
        ns <- NS(id)  
        
tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-left'
            # , margins = c(70, 1200)
          ) ,


    tabsetPanel( type = "tabs", 
        tabPanel( "Prep" , 
            fluidRow(      
              column( 12 ,
              checkboxInput( ns('hasIntegerValues'), "SUM and COUNT converted from Character to Integer")  ,
              checkboxInput( ns('uniteDataElementCategoryCombo'), "Combine dataElement and categoryOptionCombo")  ,
              actionButton( ns("updateDataset") , label = "Update dataset") 
              ) ) ,
            
            tabsetPanel( type = "tabs",   
              
              tabPanel( "Table", tableOutput( ns("contents") ) ),
              tabPanel( "Summary",
                        html("<div style='display:flex;'>") ,
                          htmlOutput( ns("profileSummary") ) ,
                        html("<div>") ,

              )
            )
                  
        ) ,
        tabPanel( "Outliers",  
                     
                sidebarLayout(
                    sidebarPanel( 
                      
                      actionButton( ns("determineOutliers") , 
                      "Search for Outliers" , style='margin-top:25px' 
                      )   ,
                      
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
    
 # scan for outliers and flag 
    searchForExtremeValues = reactiveVal( FALSE )
    
    observeEvent( input$determineOutliers  , {
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
          a = data1.mad()
          cat('\n * determine outliers button:' , searchForExtremeValues() )
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
    
    # split into 1000 chunks and observe progress
    cat('\n - making splits')
    n_splits = 1000
    split_cut_numbers = cut_number( 1:nrow( d ), n_splits )
    # testing
    saveRDS( split_cut_numbers , 'split_cut_numbers.rds' )
    
    row_splits = base::split( 1:nrow( d ), split_cut_numbers  )
  
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
      # for stockout days: max = 31 
  
      # pb <- progress_bar$new( 
      #   format = ":current :percent  [:bar] :elapsedfull :eta ",
      #   total = .total, clear = FALSE, width= 50 )
  
      # setProgress( 
      #           detail = "Searching for extreme values with each orgUnit"  
      #           )
    withProgress(     message = "Requesting data",
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
    return( data1.mad )
  }
    })
    
  # Summary ####
    
  updated = reactiveVal( 0 )
 
  # dataset = reactive({ 
  #   req( dataset.file() ) # file name from data_widget (on Dictionary tab)
  #   cat('\n* cleaning_widget  dataset():')
  #   
  #   file = paste0( data.folder() , dataset.file() )
  #   cat('\n - ', file )
  #   
  #   if ( file_test("-f",  file) ){
  #     d = readRDS( file )
  #     cat('\n - dataset has' , nrow(d),  'rows')
  #     # updated( updated() + 1 )
  #     return( d )
  #   } else {
  #     cat('\n - dataset.file() not found')
  #   }
  #   })
    
  # Update dataset
  observeEvent( input$updateDataset ,{
    req( data1() )
    cat('\n* updating dataset')
    initialUpdated = updated()
    cat('\n - initial updated =' , initialUpdated )
    
    updated( updated() + 1 )
    })
    
  output$contents <- renderTable({
    req(dataset())
    head( dataset() , n = 100 )
  })
  
  describeData = reactive({
    req( dataset() )
    dat_descr <- describe_data( dataset() )
    describer( dat_descr )
    
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
  
  # Update hasIntegerValues
  # observeEvent( updated() , {
  #   req( dataset() )
  #   cat('\n* observe dataset and set hasIntegerValues' )
  #   if ( !all( c('SUM', 'COUNT')  %in% names( dataset() ) ) ){
  #     message( '\n - missing SUM and COUNT fields')
  #   } else {
  #     if ( all( 'integer' %in% c( class( dataset()$SUM ), class( dataset()$COUNT ) ) ) ){
  #     cat('\n - all are integer')
  #     updateCheckboxInput( session , 'hasIntegerValues', value = TRUE )
  #   } 
  #   }
  # })
  
  # Update uniteDataElementCategoryCombo
  # observeEvent( updated() , {
  #   req( dataset() )
  #   cat('\n* observe dataset and set uniteDataElementCategoryCombo')
  #   if ( ! 'data' %in% names( dataset() )  ){
  #     message( '\n - no data column')
  #   } else {
  #     if ( 'data' %in% names( dataset() ) ){
  #     cat('\n - has data column')
  #     updateCheckboxInput( session , 'uniteDataElementCategoryCombo', value = TRUE )
  #   } 
  #   }
  # })
  
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
    
  outlier.summary = reactive({
      req( data1.mad() )
      req( input$dataElement )
      
      cat('\n* outlier.summary' )
      cat('\n - data has' , nrow( data1.mad() ) , 'rows' )
      
      cols = c('data' , 'Month', 'mad15', 'mad10', 'mad5'
               # 'seasonal5' , 'seasonal3'
               )
      if ( !all( cols %in% names( data1.mad()) ) ){
        message('missing outlier columns')
        return()
      } 
      
      cat('\n - totals' )
      total = data1.mad() %>%  as_tibble() %>%
        filter( data %in% input$dataElement ) %>%
        group_by( data ) %>%
        summarise( Total = sum( original , na.rm = T ) ,
                   # monthlyN = n() ,
                   N = sum( !is.na( original )))

      
      cat('\n - summary' )
      os = data1.mad() %>% as_tibble() %>% 
        filter( data %in% input$dataElement , !is.na( mad15 ) ) %>%
        group_by( data , mad15, mad10, mad5
                  # , seasonal5 , seasonal3 
                  ) %>%
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
        select( mad15, mad10, mad5, 
                # seasonal5 , seasonal3, 
                n , max , `%N` ,`%Total`  ) 
      
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


