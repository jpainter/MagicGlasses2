dqa_widget_ui = function ( id ){
        ns <- NS(id)  
        # fillCol( height = 600, flex = c(NA ) , 
        
    tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,
          
  fluidPage(
  
  titlePanel("Evolution of Data Quality"),
  tags$style(".row{height: 80vh;} "),
  navlistPanel(
    "Indicator", widths = c(2,10) , 
    
                tabPanel( "Reporting" , 
                
                          # fluidPage(
                          #   fluidRow(  style = "height:80vh;",
                                      plotOutput( ns("dqaReportingOutput") ) )
                            # ) ) 
  ,
                
                  tabPanel( "Outliers" ,
                
                          fluidPage(
                            fluidRow( style = "height:90vh;",
                                      plotOutput( ns("dqaNoErrorsOutput") ) )
                            ) ) ,
                
                  tabPanel( "Time-series Quality" ,
                
                          fluidPage(
                            fluidRow( style = "height:100vh;",
                                      plotOutput( ns("dqaMaseOutput") ) )
                            ) )
    
    
  )
)

)

}
        
dqa_widget_server <- function( id , 
                                     directory_widget_output = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ,
                                     reporting_widget_output = NULL ,
                                     cleaning_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {

    options(shiny.trace=FALSE)
    options(shiny.reactlog=FALSE)
    options( dplyr.summarise.inform = FALSE )
    
    # cat('\n**Starting Reporting Widget\n')
    
    data.folder = reactive({ directory_widget_output$directory() })
    indicator = reactive({ data_widget_output$indicator() })
    formulas = reactive({ data_widget_output$formulas() })
    dataset.file = reactive({ data_widget_output$dataset.file() })
    # dataset = reactive({ data_widget_output$data1() })
    
    data1 = reactive({ data_widget_output$data1() })
    
    aggregateselected_data = reactive({ reporting_widget_output$aggregateselected_data() })
    data.total = reactive({ reporting_widget_output$data.total() })
    selected_data = reactive({ reporting_widget_output$selected_data() })
    
    data2 = reactive({ cleaning_widget_output$data2() })
   
    formula_elements = reactive({ data_widget_output$formula_elements() })
    
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    
    dates = reactive({ reporting_widget_output$dates() })
    # dataset = reactive({ reporting_widget_output$data1() })
    # data.hts = reactive({ reporting_widget_output$data.hts() })
    levelNames = reactive({ reporting_widget_output$levelNames() })
    period = reactive({ reporting_widget_output$period() })
    group_by_cols = reactive({ reporting_widget_output$group_by_cols() })
    split = reactive({ reporting_widget_output$split() })
    startingMonth = reactive({ reporting_widget_output$startingMonth() })
    endingMonth = reactive({ reporting_widget_output$endingMonth() })
    missing_reports = reactive({ reporting_widget_output$missing_reports() })
    num_datasets = reactive({ reporting_widget_output$num_datasets() })
    num_facilities = reactive({ reporting_widget_output$num_facilities() })
    
    caption.text = reactive({ reporting_widget_output$caption.text() })
    
    aggregateselected_data = reactive({ reporting_widget_output$aggregateselected_data() })
    reportingSelectedOUs = reactive({ reporting_widget_output$reportingSelectedOUs() })

    

  plotDqaReporting = reactive({
        # req( input$components )
      cat('\n*  dqa_widget plotDqaReporting')

     dqa_data = data1()
     dqa_data %>% dqaPercentReporting() %>% dqa_reporting_plot()
})


  output$dqaReportingOutput <-  renderPlot({ plotDqaReporting()  })
  
  plotDqaNoError = reactive({
        # req( input$components )
    cat('\n*  dqa_widget plotDqaNoError')
      
    dqa_data = data1()
    dqa_data %>% monthly.outlier.summary() %>%
        yearly.outlier.summary() %>%
        dqa_outliers %>%
        yearly.outlier.summary_plot()
    
})
   
  output$dqaNoErrorsOutput <-  renderPlot({ plotDqaNoError()  })

  plotDqaMASE = reactive({
    cat('\n*  dqa_widget plotDqaMASE')
    dqa_data = data1()
    dqa_data %>% dqa_mase %>% dqa_mase_plot
  })
  
  output$dqaMaseOutput <- renderPlot({ plotDqaMASE() })
  
  # Return ####
  return( )
} )
}


