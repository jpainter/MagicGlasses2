dqa_widget_ui = function ( id ){
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
            # width = "25%" ,
            
            tabsetPanel(
              tabPanel( "Data" , 
                        inputPanel(
      
                selectInput( ns( "model" ), label = "Time-series model:" , 
                        choices = c( 
                                    # 'TSLM',
                                     'TSLM (trend)' , 'TSLM (trend+season)' , 
                                     'ETS' , 'ARIMA', 'SNAIVE' , 'NNETAR' ,
                                     # 'BSTS' , 
                                    'Prophet'
                                    
                                    # , 'TSLM (trend)'
                                    # , 'TSLM (trend+season)'
                                    ) , 
                        selected = 'ETS'  ) ,
  
                checkboxInput( ns( "autoModel" ) , label ='Automatic nmodel selection',
                               value = FALSE  ) 
                ) ) 
          ) ) ,
          
          mainPanel( width = 9 , 
               # width = "75%" ,
                # conditionalPanel( "input.plotly == 1" , ns = ns ,
                #     plotlyOutput( ns("plotlyOutput") , height = "100%" )
                #       ) ,
                
                # conditionalPanel( "input.plotly == 0" , ns = ns ,
                #     plotOutput( ns("plotOutput") , height = "600px" ,
                #              hover = "plot_hover"  )
                #  )
                
          tabsetPanel(
               
            
            tabPanel( "DQA" , 
                    
                    fluidPage(
                      fluidRow( style = "height:60vh;",
                                plotOutput( ns("plotOutput") ) )
                      ) ) 
                  ) 
) 
                 
             # plotOutput( ns( "plotOutput" ) , hover = "plot_hover"  )
          )
        )    


)))

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


    

  plotOutput = reactive({
        # req( input$components )
        cat('\n*  evaluation_widget plotTrendOutput')
        cat('\n - input$components:' , input$components)

      if ( input$components ){
          cat('\n - components')
          g = plotComponents()
      } else {
          cat('\n - plotTrends')
          g = plotTrends()
      }
      return( g )
})

  output$plotlyOutput <- renderPlotly({
      plotly::ggplotly( plotOutput() )  })

  output$plotOutput <-  renderPlot({ plotOutput()  })

  output$dynamic <- renderUI({
      req(input$plot_hover)
      verbatimTextOutput("vals")
  })

  output$vals <- renderPrint({
        hover <- input$plot_hover
        # print(str(hover)) # list
        y <- nearPoints( trend_Data() , input$plot_hover)[input$var_y]
        req(nrow(y) != 0)
        y
  })


  # Return ####
  return( )
} )
}


