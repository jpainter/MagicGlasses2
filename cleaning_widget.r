cleaning_widget_ui = function ( id ){
        ns <- NS(id)  
        
tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,

tabsetPanel( type = "tabs",

      tabPanel( "Summary",  
           selectInput( ns("dataElement") , label = "DataElement_Category:" ,
                             choices =NULL ,
                             selected = NULL ) , 
           tableOutput( ns("dqaTable") ) 
        ) ,
             
      tabPanel( "Inspect",  style = "height:90vh;" ,
                
        sidebarLayout(
            sidebarPanel(
            ) ,
            mainPanel( 
              fluidPage( 
                fluidRow( style = "height:80vh;",
                  plotOutput( ns("inspect") )
                ))
            )
          )
        )
    
     ) # tabsetPanel
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
    
  observe({
    cat('\n-update dataElement-') 
    updateSelectInput( session, 'dataElement' ,
                       choices =  data()  %>% pull(data) %>% unique 
    )
    cat('-done\n')
  })
    
  outlier.summary = reactive({
      req( data() )
      req( input$dataElement )
      
      cat('\n* outlier.summary' )
      cat('\n - data has' , nrow( data() ) , 'rows' )
      
      cols = c('data' , 'Month', 'mad15', 'mad10', 'mad5', 'seasonal5' , 'seasonal3')
      if ( !all( cols %in% names( data()) ) ){
        message('missing outlier columns')
        return()
      } 
      
      cat('\n - totals' )
      total = data() %>%  as_tibble() %>%
        filter( data %in% input$dataElement ) %>%
        group_by( data ) %>%
        summarise( Total = sum( original , na.rm = T ) ,
                   # monthlyN = n() ,
                   N = sum( !is.na( original )))

      
      cat('\n - summary' )
      os = data() %>% as_tibble() %>% 
        filter( data %in% input$dataElement ) %>%
        group_by( data , mad15, mad10, mad5, seasonal5 , seasonal3 ) %>%
        summarise( n = sum( !is.na( original )) , 
                   total = sum( original , na.rm = T ) 
                   ) %>%
        inner_join( total , by = c( "data" ) ) %>%
        summarise( 
                   `%N` = percent( n / N ) ,
                   `%Total` = percent( total / Total )
                   )        
      
      cat('\n - summary has' , nrow(os) , 'rows')
      return( os )
  })
  
  output$dqaTable = renderTable( outlier.summary() ) 
  
  ## Visualize cleaning (Inspect )  ####
  plot.single.data.series = reactive({
    req( data() )
    cat('\n* plot.single.data.series' )
    
    flag = unique( as_tibble( data() ) %>% 
                   filter( mad5, !seasonal5 ) %>% 
                   select( orgUnit, data ) 
                 )
  
    y = data() %>% as_tibble() %>%
      semi_join( flag[30,] , by = c("orgUnit", "data") ) 
  
    g = y %>%
        ggplot( aes( x = Month, y = original,  group = data ) ) +
        geom_line( alpha = .25 , aes( linetype = data ) ) +
        geom_point( aes( color = mad5 , shape = seasonal5 )) +
        labs( title = paste( unique(y$orgUnitName), collapse = ",") )
    
    return( g )
    
  })
  
  output$inspect = renderPlot({ plot.single.data.series() })
  
 
  # Return ####
  return()
  
})
}  


