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
      
                # selectInput( ns( "model" ), label = "Time-series model:" , 
                #         choices = c( 
                #                     # 'TSLM',
                #                      'TSLM (trend)' , 'TSLM (trend+season)' , 
                #                      'ETS' , 'ARIMA', 'SNAIVE' , 'NNETAR' ,
                #                      # 'BSTS' , 
                #                     'Prophet'
                #                     
                #                     # , 'TSLM (trend)'
                #                     # , 'TSLM (trend+season)'
                #                     ) , 
                #         selected = 'ETS'  ) ,
                # 
                # checkboxInput( ns( "autoModel" ) , label ='Automatic nmodel selection',
                #                value = FALSE  ) 
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
               
            
            tabPanel( "Reporting" , 
                    
                    fluidPage(
                      fluidRow( style = "height:60vh;",
                                plotOutput( ns("dqaReportingOutput") ) )
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


  dqaReporting = reactive({
    cat('\n*  dqa_widget dqaReporting function')
    
    dqa_data = data1() %>% as_tibble()
    
     # Testing
    # saveRDS( dqa_data , "dqa_data.rds" )
        
    year = dqa_years( dqa_data )
    
    cat('\n -  years:', paste( year, collapse = ","  ))
    
    n_frequently_reporting = dqa_reporting( dqa_data, missing_reports = 0 )
    n_facilities = nrow( data_ous( dqa_data = dqa_data) )
    pr = n_frequently_reporting / n_facilities
    
    cat('\n -  pr:', paste( pr, collapse = ","  ) )
    
    data = tibble( year, n_frequently_reporting , n_facilities, pr , label = percent(pr, 1.1) )

    print( data ) 
    return( data )
  })
    

  plotDqaReportingOutput = reactive({
        # req( input$components )
        cat('\n*  dqa_widget plotDqaReportingOutput')

        data = dqaReporting()
        
        n_facilities = max( data$n_facilities )
        # Testing
        # saveRDS( data , "plotDqaReportingOutput_data.rds" )
        
        g = ggplot( data = data , aes( x = as.character( Year ) , y = pr, label = label, group = 1  ) ) + 
          geom_line() +
          geom_text( vjust = -1 ) +
          ylim( 0, 1 ) +
          labs( x = "Year" , y = "Percent" , title = "Percent of facilities reporting all 12 months of the year",
                subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" ) 
                )
      return( g )
})

  # output$plotlyOutput <- renderPlotly({
  #     plotly::ggplotly( plotDqaReportingOutput() )  })

  output$dqaReportingOutput <-  renderPlot({ plotDqaReportingOutput()  })

  # output$dynamic <- renderUI({
  #     req(input$plot_hover)
  #     verbatimTextOutput("vals")
  # })
  # 
  # output$vals <- renderPrint({
  #       hover <- input$plot_hover
  #       # print(str(hover)) # list
  #       y <- nearPoints( trend_Data() , input$plot_hover)[input$var_y]
  #       req(nrow(y) != 0)
  #       y
  # })


  # Return ####
  return( )
} )
}


