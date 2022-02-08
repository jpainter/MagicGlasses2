cleaning_widget_ui = function ( id ){
        ns <- NS(id)  
        
tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
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
    dataset.file = reactive({ data_widget_output$dataset() })
    formula_elements = reactive({ data_widget_output$formula_elements() })
    dataElements = reactive({ metadata_widget_output$dataElements() })  
    categories = reactive({ metadata_widget_output$categories() })  
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    dates = reactive({ reporting_widget_output$dates() })
    dataset = reactive({ reporting_widget_output$dataset() })
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
    data = reactive({ reporting_widget_output$d() })
    data.total = reactive({ reporting_widget_output$data.total() })
    
  
  # Summary ####
    
  updated = reactiveVal( 0 )
 
  dataset = reactive({ 
    req( dataset.file() ) # file name from data_widget (on Dictionary tab)
    cat('\n* cleaning_widget  dataset():')
    
    file = paste0( data.folder() , dataset.file() )
    cat('\n - ', file )
    
    if ( file_test("-f",  file) ){
      d = readRDS( file )
      cat('\n - dataset has' , nrow(d),  'rows')
      # updated( updated() + 1 )
      return( d )
    } else {
      cat('\n - dataset.file() not found')
    }
    })
    
  # Update dataset
  observeEvent( input$updateDataset ,{
    req( dataset() )
    cat('\n* updating dataset')
    initialUpdated = updated()
    cat('\n - initial updated =' , initialUpdated )
    
    # Integer values
    if ( !input$hasIntegerValues && all( c('SUM', 'COUNT')  %in% names( dataset() ) ) ){
      cat('\n - converting SUM and COUNT to integers')
      d = dataset() %>% 
        mutate( SUM = as.integer( SUM ) ,
                COUNT = as.integer( COUNT )
      )
    updated( updated() + 1 )
    }
    
    # data column
    if ( ! 'data'  %in% names( dataset() ) ){
      cat('\n - creating data column')
      d = dataset() %>% 
        rename( dataElement.id = dataElement , categoryOptionCombo.ids = categoryOptionCombo ) %>%
        left_join( formula_elements() %>% 
                     select( dataSet, periodType, zeroIsSignificant, 
                             dataElement.id, categoryOptionCombo.ids, dataElement, Categories ) , 
                   by = c("dataElement.id", "categoryOptionCombo.ids")
                   ) %>%
        # left_join( dataElements() , by = c("dataElement.id") ) %>%
        # left_join( categories() , by = c( "categoryOptionCombo.ids") ) %>%
        unite( 'data' , c(dataElement , Categories ) , sep = "_")
      
      updated( updated() + 1 )
    }
    
    cat('\n - final updated =' , updated() )
    if ( updated() > initialUpdated ){
          file = paste0( data.folder() , dataset.file() )
          file = str_replace( file, fixed('.rds') , '.RDS'  )
          cat('\n - saving dataset ')
          saveRDS( d, file )
    }

  cat('\n - end updateDataset')
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
  observeEvent( updated() , {
    req( dataset() )
    cat('\n* observe dataset and set hasIntegerValues' )
    if ( !all( c('SUM', 'COUNT')  %in% names( dataset() ) ) ){
      message( '\n - missing SUM and COUNT fields')
    } else {
      if ( all( 'integer' %in% c( class( dataset()$SUM ), class( dataset()$COUNT ) ) ) ){
      cat('\n - all are integer')
      updateCheckboxInput( session , 'hasIntegerValues', value = TRUE )
    } 
    }
  })
  
  # Update uniteDataElementCategoryCombo
  observeEvent( updated() , {
    req( dataset() )
    cat('\n* observe dataset and set uniteDataElementCategoryCombo')
    if ( ! 'data' %in% names( dataset() )  ){
      message( '\n - no data column')
    } else {
      if ( 'data' %in% names( dataset() ) ){
      cat('\n - has data column')
      updateCheckboxInput( session , 'uniteDataElementCategoryCombo', value = TRUE )
    } 
    }
  })
  
  # Outliers tab  
  observeEvent( dataset()  , {
    
    if( nrow( dataset() ) > 0 && 'data' %in% names(dataset() ) ){
      cat('\n-update dataElement-')
      updateRadioButtons( session, 'dataElement' ,
                       choices =  dataset()  %>% pull( data ) %>% unique )
    }
    cat('-done\n')
  })
    
  outlier.summary = reactive({
      req( dataset() )
      req( input$dataElement )
      
      cat('\n* outlier.summary' )
      cat('\n - data has' , nrow( dataset() ) , 'rows' )
      
      cols = c('data' , 'Month', 'mad15', 'mad10', 'mad5', 'seasonal5' , 'seasonal3')
      if ( !all( cols %in% names( dataset()) ) ){
        message('missing outlier columns')
        return()
      } 
      
      cat('\n - totals' )
      total = dataset() %>%  as_tibble() %>%
        filter( data %in% input$dataElement ) %>%
        group_by( data ) %>%
        summarise( Total = sum( original , na.rm = T ) ,
                   # monthlyN = n() ,
                   N = sum( !is.na( original )))

      
      cat('\n - summary' )
      os = dataset() %>% as_tibble() %>% 
        filter( data %in% input$dataElement ) %>%
        group_by( data , mad15, mad10, mad5, seasonal5 , seasonal3 ) %>%
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
        select( mad15, mad10, mad5, seasonal5 , seasonal3, n , max , `%N` ,`%Total`  )
      
      cat('\n - summary has' , nrow(os) , 'rows')
      return( os )
  })
  
  output$dqaTable = renderTable( outlier.summary() ) 
  
  ## Visualize cleaning (Inspect )  ####
  errorFlag = reactive({
    req( dataset()) 
    req( input$error )
    req( input$dataElement )
    cat( '\n* errorFlag():')
    # print( head( data() ) )
    if ( input$error %in% names( dataset() ) ){
            flag = unique( as_tibble( dataset() ) %>% 
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
    req( dataset() )

    cat('\n* plot.single.data.series' )
    
    if ( length( errorFlag() ) == 0 ) return()
  
    inspectOrgUnitData = dataset() %>% as_tibble() %>%
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


