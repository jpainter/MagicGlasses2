
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
                      
                      
                      h5( 'Use the buttons to search for extreme values using median absolute deviation (MAD), and then seasonally adjusted outliers') ,
                      
                      actionButton( ns("determineExtremeValues") , 
                                    "Search for Extreme Values" , style='margin-top:25px' 
                      )   ,
                      
                      # actionButton( ns("determineSeasonalOutliers") , 
                      #               "Search for Seasonal Outliers" , style='margin-top:25px' 
                      # )  ,
                      
                      inputPanel( 
                          
                          selectizeInput( ns("level2"), label = "OrgUnit Level2" , 
                                       choices = NULL, 
                                       selected = NULL ,
                                       multiple = TRUE ) ,
                          selectizeInput( ns("level3"), label = "OrgUnit Level3" ,
                                       choices = NULL,
                                       selected = NULL ,
                                       multiple = TRUE ) ,
                          selectizeInput( ns("level4"), label = "OrgUnit Level4" ,
                                       choices = NULL,
                                       selected = NULL  ,
                                       multiple = TRUE  ) ,
                          selectizeInput( ns("level5"), label = "OrgUnit Level5" ,
                                       choices = NULL,
                                       selected = NULL  ,
                                       multiple = TRUE  ) 
                        ) ,
                      
                      inputPanel(
                          selectizeInput( ns("startingMonth") , label = "Begining with", 
                            choices = NULL ,
                            selected = NULL ) ,
                          
                          selectizeInput( ns("endingMonth"), label = "Ending with", 
                            choices = NULL , 
                            selected = NULL )
                          
                          ) 
                       ,
                      
                      tabsetPanel( type = "tabs",
                                
                             
        
                        tabPanel( "Outlier Table",  
                                  textOutput( ns("outlierSummaryText")) ,
                                  tableOutput( ns("outlier.summary.table") )  
                        ) ,
                        
                        tabPanel( "Outlier Chart",  style = "height:60vh;" ,
                                fluidPage( 
                                  fluidRow( style = "height:50vh;",
                                    # h5( 'Select orgUnit having error')  
                                  # textOutput( ns("outlierSummaryText")) ,
                                  plotlyOutput( ns("outlier.summary.chart") )
                                  )
                                )
                        ) ,
                        
                        tabPanel( "Monthly Summary",  style = "height:60vh;" ,
                                fluidPage( 
                                  fluidRow( style = "height:50vh;",
                                    # h5( 'Select orgUnit having error')  
                                  # textOutput( ns("outlierSummaryText")) ,
                                  plotOutput( ns("monthly_summary_chart") )
                                  )
                                )
                        ) ,
                        
                         tabPanel( "Mean Absolute Scaled Error",  
                                    fluidRow( style = "height:50vh;",
                                    plotOutput( ns("mase.summary") )
                                  )
                                  
                        ) ,
                       
                        tabPanel( "Inspect",  style = "height:60vh;" ,
                                fluidPage( 
                                  fluidRow( style = "height:10vh;",
                                    # h5( 'Select orgUnit having error')     ,   
                                    
                                    selectInput( ns('Error'), 'Error Type' ,
                                                 choices = c('mad15', 'mad10', 'mad5', 'seasonal5', 'seasonal3' ) ) ,
                                    
                                    # selectInput( ns('seasonalError'), 'Seasonally Adjusted error' ,
                                    #              choices = c( 'seasonal5', 'seasonal3' ) ) ,
                                    
                                    selectizeInput( ns( 'flaggedOrgUnit') , 'Select orgUnit having this error' ,
                                                 choices = "" ) ,
                                    
                                    checkboxInput( ns('showAllData') , 'Show all data elements')
                                  ) ,
                                  
                                  fluidRow( style = "height:40vh;",
                                    plotlyOutput( ns("inspect") 
                                                # , hover = ns("plot_hover") , 
                                                # click = ns("plot_click") ) 
                                    # , uiOutput( ns("dynamic") 
                                    ) 
    
                                  ) 
                                  
                                  # , fluidRow( style = "height:5vh;",
                                  #          verbatimTextOutput( ns("info") ))
                                  
                                  )
                              )
                        )
                      )
            
             ) 
        ) ,
        
        tabPanel( "Select Regions-Facilities" ,
                  
              fluidPage(
                
                fluidRow(
                  
                  DTOutput( ns('ouErrorTable') ) 
                )
              )

                  ) , 
        tabPanel( "Data View", DTOutput( ns("contents") ) ),
          
        tabPanel( "Summary (under construction)",
                        html("<div style='display:flex;'>") ,
                          htmlOutput( ns("profileSummary") ) ,
                        html("<div>") ,

              ) 
    ) )
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
    # dataset = reactive({ data_widget_output$dataset() })
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
    
  # outlierData ####
    
    outlierData <- reactiveValues( df_data = NULL ) # use observeEvent levelNames to set to data1()
     
  # orgUnits ###
    
    # level 2
    observeEvent( levelNames()  , {  
        req( ousTree() )
        l2 = ousTree() %>% 
            pull( !! rlang::sym(levelNames()[2]) ) %>% 
            unique 
        
        cat( '\n* updating cleaning_widget level2' )
        updateSelectizeInput( session, 'level2' ,
                             choices = l2[!is.na(l2)]
                             , selected = NULL ,
                             server = TRUE 
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
      updateSelectizeInput( session, 'level3' ,
                                choices = l3[!is.na(l3)]
                                , selected = NULL ,
                            server = TRUE 
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
      updateSelectizeInput( session, 'level4' ,
                                choices = l4[!is.na(l4)]
                                , selected = NULL ,
                            server = TRUE 
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
      updateSelectizeInput( session, 'level5' ,
                                choices = l5[!is.na(l5)]
                                , selected = NULL ,
                         server = TRUE 
                                )
    } )
    
  # data names  ####
    observeEvent( data1() , {
      # req( data1() )
      cat('\n* cleaning_widget observe data1() class:' , class( data1() ) )
      
      # outlierData$df_data = data1()
      x = data1()
      
      # cat('\n - cleaning_widget observe outlierData$df_data class:' , class( outlierData$df_data ) )
      cat('\n - cleaning_widget observe outlierData$df_data class:' , class( x ) )
      
      updateRadioButtons( session , "dataElement" , 
                       # choices = c( 'All' , outlierData$df_data$data %>% unique ) ) 
                        choices = c( 'All' , x$data %>% unique ) ) 
     
    })
  
  # Dates
    observeEvent(  dates() , {
      cat('\n* cleaning_widget observeEvent dates()')
      cat('\n - observeEvent dates() update startingMonth-')
      dates = dates()
      updateSelectizeInput( session, 'startingMonth' ,
              choices =  dates  %>% as.character()  ,
              selected = min( dates, na.rm = TRUE ) %>% as.character() , 
              server = TRUE
      )
      
      
      cat('\n- cleaning_widget observeEvent dates() update endingMonth-' ) 
      updateSelectizeInput( session, 'endingMonth' ,
              choices =  dates  %>% as.character() ,
              selected = max( dates , na.rm = TRUE ) %>% as.character() ,
              server = TRUE
      )
      
      cat('\n  -done')
      } )
      
  # scan for MAD outliers  ####
    searchForMAD = reactiveVal( FALSE )
    scanForMAD = reactiveVal( FALSE )
    afterMAD = reactiveVal( FALSE )

    searchForSeasonalOutliers  = reactiveVal( FALSE ) 
    scanForSeasonal = reactiveVal( FALSE )
    afterSeasonal = reactiveVal( FALSE )
    
    # Option to rerun MAD
    rerunMadModal <- function() {
       ns <- NS(id) 
       modalDialog( title = "MAD flags already present in data", 
                         # easyClose = TRUE ,
                         size = 'm' ,
                         footer = tagList( modalButton( "Cancel" ),
                                           actionButton( ns("rerunMAD"), "Re-Run scan for MAD Outliers")
        )
                         )
    }
    
    observeEvent( input$rerunMAD , {
         req( data1() )
         
         searchForMAD( FALSE )
         searchForMAD( TRUE ) 
         cat('\n * rerun MAD outliers button:' , searchForMAD() )
         removeModal()
         # outlierData$df_data = data1.mad()
})
      
    observeEvent( input$determineExtremeValues  , {
      # req( outlierData$df_data )
      req( data1() )
      cat('\n* cleaning_widget observeEvent determineExtremeValues.  searchForMAD():' , searchForMAD() )
      
      #1. Scan for extreme values (MAD)
      x = data1()
      # cat( "\n - determineExtremeValues.  mad15 %in% names( outlierData$df_data ):" , 'mad15' %in% names( outlierData$df_data ))
      cat( "\n - determineExtremeValues.  mad15 %in% names( outlierData$df_data ):" , 'mad15' %in% names( x ))
      # if ( 'mad15' %in% names( outlierData$df_data ) ){
      if ( 'mad15' %in% names( x ) ){

        showModal( rerunMadModal() )
        cat('\n - reRun extreme values button:' , searchForMAD() )
        
      } else {
          searchForMAD( FALSE )
          searchForMAD( TRUE )
          cat('\n - determine extreme values button:' , searchForMAD() )
      }
      
      # cat( "\n - outlierData$df_data = data1.mad()")
      # outlierData$df_data = data1.mad()
      
 } )
    
    observeEvent( searchForMAD() , {
     # req( outlierData$df_data )

     cat( '\n* observeEvent searchForMAD: ' , searchForMAD())
     
     if ( searchForMAD() ){
       scanForMAD( TRUE ) 
       
       # cat('\n - cleaning_widget outlierData$df_data class:' , class( outlierData$df_data ) )
       cat('\n - cleaning_widget data1():' , class( data1() ) )
       
       # outlierData$df_data 
       x = data1.mad_seasonal()
       cat( "\n - cleaning_widget searchForMAD class(x):" , class(x) )
     }

     # cat( '\n - searchForMAD names(outlierData$df_data) ' , names(outlierData$df_data) )
     
      scanForMAD( FALSE )
      afterMAD( FALSE )
      afterMAD( TRUE )
      
 
   })
    
    data1.mad_seasonal = reactive({
      
          # req( outlierData$df_data )
          req( data1() )
        
          cat('\n* cleaning_widget data1.mad_seasonal' )
          cat('\n - scanForMAD:' , scanForMAD() )
          
          cat( '\n - class(outlierData$df_data):' , class( outlierData$df_data ))
          cat('\n - cleaning_widget data1():' , class( data1() ) )
          
          if ( scanForMAD() ){
            cat('\n - data1.mad search')
          
            # d = outlierData$df_data
            d = data1()
            cat( '\n - cleaning_widget data1.mad_seasonal class(d):' , class( d ))
            
            nrow1 = nrow( d )
            if ( nrow1 == 0 ){
              cat('\n - nrow1 = 0')
              return()
            } else { cat('\n - outlierData has' , nrow1 , 'rows')}
            
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
            
  
                data.mad = mad_outliers( d , .total = .total , 
                                .threshold = 50,
                                key_entry_errors = key_entry_errors  )
          })
          
         cat( '\n - scanning for Seasonal outliers')
         d = data.mad
        .total = length( key_size( d ) )
         cat( '\n - .total' , .total )
    
        withProgress(  message = "Seasonal Outliers",
                          detail = "starting ...",
                          value = 0, {
        
                data1.seasonal = seasonal_outliers( d , .total = .total , .threshold = 50)
          })  
          
          showModal(
                modalDialog( title = "Finished scanning for seasonal values; saving data", 
                             easyClose = TRUE ,
                             size = 'm' ,
                             footer = "(click anywhere to close dialog box)"
                             )
                )
          
      cat('\n - saving data1.seasonal to replace dataset')
      cat('\n - names(data1.seasonal):', names(data1.seasonal) )
      
      
      saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
      removeModal()
        
        } # end if scan for mad
      return( data1.seasonal  ) 
    })

    data1.mad = reactive({
        req( outlierData$df_data )
        cat('\n* data1.mad' )
        cat('\n - scanForMAD:' , scanForMAD() )

        if ( scanForMAD() ){
          cat('\n - data1.mad search')

          d = outlierData$df_data
          cat( '\n - class(d)' , class(d) )

          nrow1 = nrow( d )
          if ( nrow1 == 0 ){
            cat('\n - nrow1 = 0')
            return()
          } else { cat('\n - outlierData has' , nrow1 , 'rows')}

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


           data.mad = mad_outliers( d )
        })

        outlierData$df_data = data1.mad

        # showModal(
        #       modalDialog( title = "Finished scanning for extreme values",
        #                    easyClose = TRUE ,
        #                    size = 'm' ,
        #                    footer = "(click anywhere to close dialog box)"
        #                    )
        #       )

        # Save data for next time...
        cat('\n - saving data1.mad to replace dataset')
        saveRDS( data1.mad , paste0( data.folder(), dataset.file() ) )
        # removeModal()

        }


        return( data1.mad )
    })
    
    # Option to rerun seasonal outliers
    rerunSeasonalModal <- function() {
       ns <- NS(id) 
       modalDialog( title = "Seasonal outlier flags already present in data", 
                         # easyClose = TRUE ,
                         size = 'm' ,
                         footer = tagList( modalButton( "Cancel" ),
                                           actionButton( ns("rerunSeasonal"), "Re-Run Seasonal Outliers")
        )
                         )
   }
 
    observeEvent( input$rerunSeasonal , {
         req( outlierData$df_data )
         
         searchForSeasonalOutliers( TRUE ) 
         cat('\n * rerun seasonal outliers button:' , searchForSeasonalOutliers() )
         removeModal()

})
    
    observeEvent( afterMAD() , {
      req( outlierData$df_data )
      cat( "\n* observeEvent afterMAD():", afterMAD() )
      cat( '\n - afterMAD names(outlierData$df_data) ' , names(outlierData$df_data) )
      
      if ( afterMAD() ){
      #   # 2. Scan for seasonally adjusted outliers
      #   if ( 'seasonal5' %in% names( outlierData$df_data ) ){
      #      showModal( rerunSeasonalModal() )
      # 
      #   } else {

            # searchForSeasonalOutliers( FALSE )
            # searchForSeasonalOutliers( TRUE )
            # cat('\n - determine seasonal outliers button:' , searchForSeasonalOutliers() )

        # } 
      }
    })
 
    observeEvent( searchForSeasonalOutliers() , {
     req( outlierData$df_data )
     cat( '\n* observeEvent searchForSeasonalOutliers: ' , searchForSeasonalOutliers() )
     
     if ( searchForSeasonalOutliers() ){
       # scanForSeasonal( FALSE ) 
       # scanForSeasonal( TRUE ) 
       # cat( '\n - searchForSeasonalOutliers names(outlierData$df_data) ' , names(outlierData$df_data) )
       # outlierData$df_data = data1.seasonal()
     }
   })
         
    data1.seasonal = reactive({
      req( outlierData$df_data )
      cat('\n* data1.seasonal')
      
      # if data1 already has seasonal columns, return data1
      # if ( !rerunSeasonalOutliers() & 'seasonal3' %in% names( outlierData$df_data ) ){
      #   cat('\n - seasonal cols already in data1' )
      #   return( outlierData$df_data )
      # } 
      
      
      if ( scanForSeasonal()  ){
      cat('\n* data1.seasonal search')
      
      # outlierData$df_data = data1.mad() 
    
      # Stop if mad10 not in dataset
      
      cat('\n - names(outlierData$df_data)' , names(outlierData$df_data) )
      if ( !'mad10' %in% names( outlierData$df_data ) ){
            
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
 
       cat( '\n - scanning for Seasonal outliers')
       d = outlierData$df_data
      .total = length( key_size( d ) )
       cat( '\n - .total' , .total )
  
      .threshold = 50

      withProgress(  message = "Seasonal Outliers",
                        detail = "starting ...",
                        value = 0, {
      
              data1.seasonal = data1.seasonal( d )
        })  
        
        showModal(
              modalDialog( title = "Finished scanning for seasonal values; saving data", 
                           easyClose = TRUE ,
                           size = 'm' ,
                           footer = "Click anywhere to close dialog box.  To see results, go to Data page and refresh data selection."
                           )
              )
        
    cat('\n - saving data1.seasonal to replace dataset')
    cat('\n - names(data1.seasonal):', names(data1.seasonal) )
    
    
    saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
    removeModal()
    
    # outlierData$df_data = data1.seasonal 
    
    searchForSeasonalOutliers( FALSE )
    afterSeasonal( TRUE )
      }
      
    # return( outlierData$df_data )
    return( data1.seasonal )
    })
   
  # Summary ####
    
    # d.mase
    data1.summary = reactive({
        # req( outlierData$df_data )
        req( data1() )
        cat( '\n* cleaning_widget data1.summary')
        
        # d = outlierData$df_data
        d = data1()
        if (! 'expected' %in% names( d ) ){
          cat( '\n - "expected" column not found')
          return()
          }
        
        # data.table?  
        d.mase = setDT( d )[ 
          # year( Month ) == 2020
          ,
          .(   mase = mase( actual = original, predicted = expected  ) ,
               n = sum( !is.na( original ) ) ,
               expected = sum( expected, na.rm = TRUE )
          ) ,
          by = c(  'orgUnit', 'orgUnitName' , 'data.id' , 'data' ) ] %>%
          
          as_tibble() %>%
          group_by( orgUnit, orgUnitName , data.id , data )
        
        maxN = max( d.mase.ou$n.max )
        
        data1.summary = d.mase %>%
          mutate( 
            catMASE = case_when( 
              MASE == 0  ~ "0" ,
              MASE <= 50 ~ "0_50" ,
              MASE <= 100 ~ "50_100" ,
              MASE > 100 ~ "100+" ,
              is.nan( MASE ) ~ 'NaN'
            ) 
            , catN = case_when(
              n == maxN ~ "All" ,
              n < 50 ~ "0_50" ,
              n < 75 ~ "50_75" ,
              n < 100 ~ "75_100" ,
              is.na( n ) ~ 'None'
            )
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

    # output$contents <- renderTable({
    #   cat('\n* contents')
    #   req( outlier.dataset() )
    #   head( outlier.dataset() , n = 100 )
    # })
    
    output$contents <- DT::renderDT({
      
      cat('\n* contents')
      req( outlierData$df_data)
      
      DT::datatable(
        outlierData$df_data %>% select( -Month ) ,
        rownames = FALSE, 
        filter = 'top' ,
        options = DToptions_no_buttons()
      )
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
    
    outlier.summary = reactive({
      req( outlier.dataset() )
      
      data1 = outlier.dataset()
      
      # data.table? 
      d = data1 %>% 
        as_tibble() %>%
        group_by( Month , seasonal3 ) %>%
        summarise( original = sum( original , na.rm = T )  
                 ) %>%
        mutate( clean = seasonal3 %in% TRUE ) %>%
        group_by( Month , clean ) %>%
        summarise( original = sum( original , na.rm = T )  ) %>%
        pivot_wider( names_from = clean , values_from = original ) %>%
        mutate( Raw = `TRUE` + `FALSE` ) %>%
        rename( Clean = `TRUE` ) %>%
        select( - `FALSE`) %>%
        pivot_longer( cols = c(Clean, Raw) )
      
        
    })
    
    output$outlier.summary.chart <- renderPlotly({
      req( outlier.dataset()) 
      
      d = outlier.summary()

      cat('\n * outlier.summary.chart')
        
      g = d %>% 
          ggplot( aes( x = Month , y = value , group = name , color = name )) +
          scale_color_brewer() + 
          geom_line() +
          theme_minimal() 
        
        ggplotly( g )
          
    })
    
    output$monthly_summary_chart <- renderPlot({
      req( outlier.dataset() ) 
      
      df.ts = data1() 

      cat('\n * outlier.summary.chart')
        
      d =  monthly.outlier.summary( df.ts )
      g  = outlier.summary.chart( d )
      g 
      # ggplotly( g )
          
    })
    
  
  # Inspect Outliers #####
    # observeEvent( afterSeasonal() ,{
    #   req( outlierData$df_data )
    #   cat( '\n* observeEvent afterSeasonal')
    #   x = outlier.dataset()
    # })
    
    outlier.dataset = reactive({

      # req( outlierData$df_data )
      req( data1() )
      req( input$startingMonth )
      req( input$endingMonth )
      req( period() )

      cat( '\n* cleaning_widget outlier.dataset():')
      # if ( is.null( outlierData$df_data ) ){
      #   cat( '\n - is.null( outlierData$df_data )' )
      #   outlierData$df_data  = data1()
      # }

      # d = outlierData$df_data
      
      # filter date
      # d = d %>% 
      #   filter( 
      #     Month >= yearmonth( input$startingMonth ) , 
      #     Month <= yearmonth(input$endingMonth )
      #     )
      # d. = as.data.table( outlierData$df_data )
      d. = as.data.table( data1() )
   
      cat( '\n - period():' , period()) 
      cat( "\n - d. class/cols: \n -- ", class( d. ) , "\n -- ", names( d. ))
      
      # testing
      # saveRDS( d. , "d..rds" )
      
      if ( period() %in% 'Month' ){
        
          # d = d.[ which( 
          #   Month >= yearmonth( input$startingMonth ) & Month <= yearmonth(input$endingMonth ) ) ,] %>%
          #   as_tibble
          
          d = d. %>% filter( Month >= yearmonth( input$startingMonth ) & Month <= yearmonth(input$endingMonth ) ) %>%
            as_tibble
   
          cat('\n - period is month' )
      }
      
      if ( period() %in% 'Week' ){
        
          d = d.[ which( 
            Week >= yearweek( input$startingMonth ) & Week <= yearweek(input$endingMonth ) ) ,] %>%
            as_tibble
   
          cat('\n - period is week' )
      }
      

      if ( 'mad10' %in% names(d) ) cat('\n - data has mad10' )
      if ( 'seasonal3' %in% names(d) ) cat('\n - data has seasonal3' )

      if ( 'effectiveLeaf' %in% names( d ) && input$selectOrgType %in% 'Facilities only'){
        
        cat('\n - data has effectiveLeaf; facilities only' )
        
        d = setDT( d )[ effectiveLeaf == TRUE , ]
        # d = d %>% filter( effectiveLeaf )
        
        
      } else if ( input$selectOrgType %in% 'Admin only' ){
        cat('\n - Admin only' )
        d = setDT( d )[ effectiveLeaf != TRUE , ]
        # d = d %>% filter( effectiveLeaf )
      }

      # Filter by region/level
      # level2
      if ( !is_empty( input$level2 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[2] , "=" , input$level2 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
        d = setDT( d )[ base::get( levelNames()[2])  %in%   input$level2  ,, ]
      }

      # level3
      if ( !is_empty( input$level3 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[3] , "=" , input$level3 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
        d = setDT( d )[ base::get( levelNames()[3])  %in%   input$level3  ,, ]
      }

      # level4
      if ( !is_empty( input$level4 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[4] , "=" , input$level4 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
        d = setDT( d )[ base::get( levelNames()[4])  %in%   input$level4  ,, ]
      }

      # level5
      if ( !is_empty( input$level5 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[5] , "=" , input$level5 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )
        d = setDT( d )[ base::get( levelNames()[5])  %in%   input$level5  ,, ]
      }


      # filter dataElement
      if ( input$dataElement %in% 'All'){
        d = d %>% as_tibble()
      } else {
        d = setDT( d )[ data %in% input$dataElement , ] %>%  as_tibble()
        # %>%  filter( data %in% input$dataElement )
      }
      


      cat( '\n - done')
      return( d )
      })

    outlier.summary.cols = reactive({
      req( outlier.dataset() )
      cat('\n* outlier.summary.cols():')
      
      d = outlier.dataset()
      
      if ( 'seasonal3' %in% names( d )){
        cols = c('mad15', 'mad10', 'mad5','seasonal5' , 'seasonal3', 'expected')
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
    
    outlier.summary.table = reactive({
        req( outlier.dataset() )
        req( outlier.summary.cols() )
        req( input$dataElement )
        
        cat('\n* outlier.summary' )
        d = outlier.dataset()
        
        cols = outlier.summary.cols() 
        if ( 'expected' %in% cols ) cols = setdiff( cols, 'expected' )
  
        cat('\n - totals' )
        
        # data.table? 
        # total = d %>%
        #   summarise( Total = sum( original , na.rm = T ) ,
        #              # monthlyN = n() ,
        #              N = sum( !is.na( original )))
        
        #Testing
        # saveRDS( d, 'outlier.summary.table.d.rds')
  
        total = setDT( d )[ , .( Total = sum( original , na.rm = T ) ,
                                 # monthlyN = n() ,
                                 N = sum( !is.na( original )) ) ,  ]
        
        cat('\n - summary' )
        
        # data.table 
        os <- setDT( d )[ !is.na( mad15 ) ,
                          .( n = sum( !is.na( original ) ) ,
                             total = sum( original , na.rm = T )  ,
                             max = max( original , na.rm = T ) %>% comma() ) ,
                          cols] %>%
          as_tibble %>%
          arrange_at( cols ) %>%
          bind_cols( total  ) %>%
          mutate(
            `%N` = percent( n / N ) ,
            `%Total` = percent( total / Total ) ,
            n = comma( n ) ,
            total = comma( total )
          )   %>%
          select( !! cols  , n ,  `%N` ,  max , total , `%Total`  )
        
        # Tidy
        # os <- setDT( d )[ !is.na( mad15 ) , 
        #                  .( n = sum( !is.na( original ) ) , 
        #                     total = sum( original , na.rm = T )  ,
        #                     max = max( original , na.rm = T ) %>% comma() ) , 
        #                  cols] %>%
        #   as_tibble %>% 
        #   arrange_at( cols ) %>%
        #   bind_cols( total  ) %>%
        #   mutate(
        #              `%N` = percent( n / N ) ,
        #              `%Total` = percent( total / Total ) ,
        #              n = comma( n ) ,
        #              total = comma( total )
        #              )   %>%
        #   select( !! cols  , n ,  `%N` ,  max , total , `%Total`  ) 
          
        cat('\n - summary has' , nrow(os) , 'rows')
        return( os )
    })
    
    output$outlier.summary.table = renderTable( outlier.summary.table() ) 
    
  ## Visualize cleaning (Inspect )  ####
  errorFlag = reactive({
    req( outlier.dataset() ) 
    req( input$Error )
    req( input$dataElement )
    cat( '\n* errorFlag():')
    
    # print( head( data1() ) )
    d = outlier.dataset()
     
    # testing
    # saveRDS( d , 'outlier.dataset.rds')
    
    # MAD Error
    cat( "\n - input$Error:" , input$Error )
    if ( input$Error %in% names( d ) ){
      
            if ( ! 'All' %in% input$dataElement ) d = d %>% filter( data %in% input$dataElement )
            flag = unique( as_tibble( d ) %>% 
                   filter( 
                      !! rlang::sym( input$Error )  == FALSE 
                     ) %>% 
                   distinct( orgUnit , orgUnitName )  
                 )
            cat( '\n -  nrow errorFlag() :', nrow( flag ) )
            
    } else {  return() }

    return( flag )
  })
  
  observeEvent( errorFlag() , {
    
    if (  nrow( errorFlag() ) > 0  ){
      
      updateSelectizeInput( session, "flaggedOrgUnit" , 
                            choices = paste0( errorFlag()$orgUnitName ) ,
                            server = TRUE 
      )
    }

  })
  
  output$ouErrorTable = 
    DT::renderDT( DT::datatable(

      outlier.dataset() %>% 
        as_tibble() %>%
        select( data, period, orgUnitName, level, 
                original, !! rlang::syms( outlier.summary.cols() )
                ) %>%
        filter( 
          orgUnitName %in% errorFlag()$orgUnitName
                ,  data %in% input$dataElement
        )   ,
      
      rownames = FALSE, 
      filter = 'top' ,
      options = list(
        # bPaginate = FALSE, 
        scrollY = "60vh"  ,
        info = TRUE ,
        lengthMenu = list( c( -1, 1, 5, 10, 25, 100 ), 
                           list( 'All' , '1', '5' , '10', '25', '100') ) ,
        server = TRUE ),
      fillContainer = TRUE)
      # options = DToptions_no_buttons()
    )
  
  plot.single.data.series = reactive({
    req( outlier.dataset() )

    cat('\n* plot.single.data.series' )
    
    if ( nrow( errorFlag() ) == 0 ) return()
  
    inspectOrgUnitData = outlier.dataset() %>% 
      as_tibble() %>%
      filter( 
        orgUnitName %in% input$flaggedOrgUnit 
              )
    
    if ( ! ( input$showAllData ||  'All' %in% input$dataElement ) ){
      
      inspectOrgUnitData = inspectOrgUnitData %>%
        filter( 
          data %in% input$dataElement
        )
      
    } 
      
    cat('\n* inspectOrgUnitData points:' , nrow(inspectOrgUnitData) )
    
    g = inspectOrgUnitData %>%
        ggplot( aes( x = Month, y = original,  group = data ) ) +
        geom_line( alpha = .25 ) +
        geom_point( aes( color = !! rlang::sym( input$Error ) 
                         # , shape = seasonal3 
                         )) +
        labs( title = paste( unique( inspectOrgUnitData$orgUnitName ), collapse = ",") +
        theme_minimal()  )
    
    cat('\n -done' )
    return( g )
    
  })
  
  output$inspect = renderPlotly({ plot.single.data.series() })
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })

  output$vals <- renderPrint({
    hover <- input$plot_hover 
    # print(str(hover)) # list
    y <- nearPoints( outlier.dataset() , input$plot_hover )[ "original" ]
    req( nrow(y) != 0 )
    y
  })
  
  output$info <- renderPrint({
    req( input$plot_hover )
    x <- input$plot_hover$x
    y <- input$plot_hover$y
    # group = 
    cat("[", x, ", ", y, "]", sep = "")
  })
  
 
  # Return ####
  return( list(
    data2 = reactive({ outlier.dataset() }) 
  ))
  
})
}  



