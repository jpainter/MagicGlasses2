evaluation_widget_ui = function ( id ){
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
                tabPanel( "Models" , 
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
                  
                textInput( ns( 'model.formula' ) , 'Model Formula' ,
                  value =  "total ~ error() + trend() + season()" ) ,
            
            
                  textInput( ns( 'covariates' ), 'Model covariates' ,
                      value =  NULL ) ,
                  
                  checkboxInput( ns( "transform" ) , label ='Transform: box_cox(auto)',
                                 value = FALSE  ) ,
                    
                  checkboxInput( ns( "smooth" ) , label ='Show smoothed trend line (loess)',
                                 value = FALSE  ) ,
                  
         
                  checkboxInput( ns( "scale" ) , label ='Scale values (x-mean)/sd + 1)',
                                 value = FALSE  ) ,
                  
                  checkboxInput( ns( 'components' ), label = 'Visualize trend' ,
                              value = FALSE ) ,
                  
                  checkboxInput( ns( "forecast_ci" ) , label ='Prediction interval',
                                 value = FALSE  ) ,
                  
                  checkboxInput( ns( "bootstrap" ) , label ='Bootstrap estimate',
                                 value = FALSE  ) ,
                  
                  checkboxInput( ns( "autoModel" ) , label ='Automatic nmodel selection',
                                 value = FALSE  ) ,
                
                 checkboxInput( ns( "ensemble" ) , label ='Use ensemble models',
                                 value = FALSE  ) 
                  ) ) ,
                
                tabPanel( "Stratifications" ,
                checkboxInput( ns('hts'), label = "hts across full admin hierarchy", 
                       value = FALSE ) ,
         
                # selectInput( ns("hts_level") , label = "Aggregate only from level:" ,
                #       choices = 1:6 ,
                #       selected = 1 ) ,
              
                selectInput( ns( "agg_level") , label = "Aggregate to admin level:" , 
                  choices = NULL , 
                  selected = 1  ) ,
              
              selectInput( ns( "agg_method") , label = "Aggregate (regression) method:" , 
                  choices = c( "None", "Bottum up", "MINT(ols)" , "MINT(cov)") , 
                  selected = 1  ) ,
  
                checkboxInput( ns( "facet_admin" ) , label ="Facet by admin",
                               value = TRUE  ) ,
                
                checkboxInput( ns( "facet_split" ) , label ="Facet by split",
                               value = FALSE  ) ,
                
                checkboxInput( ns( "label" ) , label ='Show labels',
                               value = FALSE  ) ,
              
                checkboxInput( ns( "pe" ) , label ='Show mean percent difference from expected',
                               value = TRUE  ) ,
              
                checkboxInput( ns( "legend" ) , label ='Show legend',
                               value = FALSE  )
              ) ,
               
              tabPanel( "Reporting" , 
                h5("Choose whether or not to limit analysis to consistently reporting facilities.  
                   This will be based on the current choices in the Reporting page.  
                   The default is to filter to the consistently reporting facilities.") ,
                
                checkboxInput( ns( "selected" ) , label ='Selected facilities only',
                               value = TRUE  ) 
                ) ,
              
              tabPanel( "Outliers" ,
                        
               h5("Choose whether or not to remove potential outliers, as flagged in the Outliers page.  
                   The default is to omit values that were greater than the medican absolute deviation times 10 (MAD10)"
                  ) ,
               
               selectInput( ns( "error") , label = "Keep original data or remove data with following error flags:" , 
                  choices = c( "Original", "mad15", "mad10" , "seasonal5" , "seasonal3" ) , 
                  selected = "mad10"  ) 
                
                # checkboxInput( ns( "plotly" ) , label ='Plotly Chart',
                #                value = FALSE  ) 
                )
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
              inputPanel(  
                
                selectizeInput( ns("evaluation_month") , label = "Intervention Start", 
                      choices = NULL ,
                      selected = NULL ) ,
              
              # div(id = "expr-container",
                selectInput( ns("horizon") , label = "Number periods after intervention:" , 
                              choices = c(3,6,12,18,24,36) , 
                              selected = 12  ) ,
              
                actionButton( ns( "forecast" ) , "Forecast" ) ,
              
                checkboxInput( ns( "pre_evaluation") , label ='Pre-intervention model fit',
                                 value = FALSE  ) ,
                  
                  
                checkboxInput( ns( "evaluation" ), label ='Post-intervention evaluation',
                                 value = FALSE  ) 
                  
              ) ,
              
              tabsetPanel(
                   
                
              
                tabPanel( "ggPlot" ,
                          
                          fluidPage(
                            fluidRow( style = "height:60vh;",
                                      plotOutput( ns("plotOutput") ) ) ,
                          
                            fluidRow( tableOutput( ns( "forecastResult" ) ) )
                            )  
                          ) ,
                tabPanel("Plotly", plotlyOutput(  ns("plotlyOutput") ) ),
                tabPanel("Table", plotlyOutput( ns("tableOutput" )  ) ) 
              ) 
  ) 
                   
               # plotOutput( ns( "plotOutput" ) , hover = "plot_hover"  )
            )
          )    
  

)))

}
        
evaluation_widget_server <- function( id , 
                                     directory_widget_output = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ,
                                     reporting_widget_output = NULL ,
                                     cleaning_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {
      
    testing = FALSE

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


    # see https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
    shift_legend3 <- function(p) {
          pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
            with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))

          if( length(pnls) == 0 ) return(p)

          lemon::reposition_legend( p, "center", panel=names(pnls) )
    }

    # Dates

    dates = reactive({
      req( data1() )

      cat('\n* evaluation_widget dates():');
      .period = period()

      dates = data1() %>% pull( !! rlang::sym( .period )) %>%
        unique
      
      dates = dates[ order( dates ) ]

      # dates = setDT( data1() )[ , base::get( .period ) ] %>%
      #   unique

      # print( dates )
      # print( max( dates ))
      print( 'end dates()')
      return( dates )

    })

    observeEvent(
      dates() , {
      cat('\n* evaluation_widget update evaluation_month:');
      updateSelectInput( session, 'evaluation_month' ,
                         choices =  dates()  ,
                         selected = dates()[ round(length(dates())/2) ]
                         # ifelse( period() %in% 'Month' ,
                         #                  dates()[12],
                         #                  dates()[52] )
                         # length(  dates()  ) - 12  ,
                         # length(  dates()  ) - 12            )
      )
    } )


# Model ####
    # level names
    observeEvent(
      levelNames() ,{

      cat('\n* evaluation_widget update agg_level:');
      updateSelectInput( session, 'agg_level' ,

                              choices = levelNames() ,
                              selected = levelNames()[1] ) # 12 months before latest date
  } )

# Impact ####
    
      # Var_y
      observeEvent(
        data.total() , {
        cat('\n* evaluation_widget update var_y:');
        updateSelectInput( session, 'var_y' ,
                choices =  names( data.total() )
                )
        } )

      # evaluation date
      observeEvent(
        dates() , {
        cat('\n* evaluation_widget update evaluation_month:');
        updateSelectInput( session, 'evaluation_month' ,
                choices =  dates()  ,
                selected = dates()[ round(length(dates())/2) ]
                  # ifelse( period() %in% 'Month' ,
                  #                  dates()[12],
                  #                  dates()[52] )
                                   # length(  dates()  ) - 12  ,
                                   # length(  dates()  ) - 12            )
                                   )
        } )

      # Split plot
      observeEvent(
        split()  ,
        {
        cat('\n* evaluation_widget update split:');
        if ( !split() %in% 'None' ){

          print( "split():" ); print( split() )
          # print( "data.total():" ); # glimpse( data.total() )

          # splits = data.total() %>% pull( .data[[ split() ]] ) %>% unique

          splits = data.total()[, split()] %>% unique

          print( paste( 'splits: ', splits  ) )

          updateSelectInput( session, 'filter_data' , choices =  c( 'All', splits ) )

          updateSelectInput( session, 'filter_display' , choices =  c( 'All', splits ) )

        } else {

          updateSelectInput( session, 'filter_data' , choices =  c( 'All' ) )
          updateSelectInput( session, 'filter_display' , choices =  c( 'All' ) )
        }
      } )

      backtick <- function(x) paste0("`", x, "`")

      levelNames = reactive({
          req( orgUnits() )
          cat( '\n* evaluation_widget levelNames():' )
          l = count( orgUnits() %>% as_tibble, level, levelName ) %>%
            arrange( level ) %>% pull(levelName )

          # l = setDT( orgUnits )[ , .(n = uniqueN(order_no)), by = c("level", "levelName") ]

          l = l[ !is.na(l) ]
          cat( '\n - end levelNames():' )
          return(l)
  })

      levels = reactive({
          req( orgUnits() )
          cat( '\n* evaluation_widget levels():' )
          levels =
            count( orgUnits() %>% as_tibble, level, levelName ) %>%
            arrange( level )
          cat( '\n - end levels():' )
          return( levels )
    })

      sub_agg_level = reactive({
          req( levels() )
          cat('\n* evaluation_widget sub_agg_level:')
          x = levels() %>%
             mutate( parent = dplyr::lag( levelName ) ) %>%
             filter( parent == input$agg_level ) %>%
             pull( levelName )
          cat('\n - done:' , x )
          if ( is.na( x ) ) return(NULL)
          return( x )
        })

      MAPE = reactive({
        req( tsPreForecast() )
        cat('\n* evaluation_widget MAPE()')

        predicted = tsPreForecast() %>% as_tibble() %>% select(-total)
        actual =  trend_Data()
        d = predicted %>%
           inner_join( actual , by = period() )

        e = d %>% as_tibble() %>%
              # group_by( orgUnit , data  )  %>%
              summarise(
                mape = ifelse( mean( total , na.rm = T ) > 0 ,
                             mean( abs( total - .mean ) , na.rm = T ) /
                             mean( total , na.rm = T ) ,
                             NA )
                         )

        cat('\n* - ', e$mape )
        return( scales::percent( e$mape )  )

      })

      key.mape = reactive({
        req( tsPreForecast() )
        req( trend_Data() )

        cat('\n* evaluation_widget key.mape()')

        predicted = tsPreForecast() %>%
          rename( pred = .mean )

        actual =  trend_Data() %>%
          rename( actual = total )

        keyvars = key_vars( actual )
        cat('\n - keyvars' , keyvars )

        truth = predicted %>%
           inner_join( actual , by = c( period() , keyvars  ) )

        cat( '\n - truth'); #print( truth )

        mid_point = round( as.integer( input$horizon ) /2  )

        e = truth %>%
          group_by_key() %>%
          index_by( 1 ) %>%
          summarise(
                mape = ifelse( mean( pred , na.rm = T ) > 0 ,
                             mean( abs( actual - pred ) ,
                                   na.rm = T ) /
                             mean( pred , na.rm = T ) ,
                             NA ) ,
                !! rlang::sym( period() )  := nth( !! rlang::sym( period() )  , mid_point ) ,
                actual = ifelse( mape>=0 , max( actual, na.rm = TRUE ),
                                 min( actual, na.rm = TRUE  )
                                 #nth( actual , mid_point )
                ) ,
                just = ifelse( mape >= 0 , 2, -2 )
                ) %>%
        as_tibble() %>%
            mutate( !! input$agg_level :=
                      as.character( !! rlang::sym( input$agg_level  ) ) )

        if ( !split() %in% 'None' ){
           cat( '\n - key.mape grouping_var' , split() )
           e = e %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split() ) )
             )
     } else {
           e = e %>%
             mutate(  grouping_var = 'Total' )
         }

        # print( "end key.mape"); #glimpse(e )
        return( e )
      })

      MPE = reactive({
        req( tsForecast() )

        cat('\n* evaluation_widget MPE()')

        predicted = tsForecast() %>% as_tibble() %>% select(-total)
        actual =  trend_Data()

        d = predicted %>%
           inner_join( actual , by = period() )

        e = d %>% as_tibble() %>%
              # group_by( orgUnit , data  )  %>%
              summarise(
                mpe = ifelse( mean( .mean , na.rm = T ) > 0 ,
                             mean(  total - .mean  , na.rm = T ) /
                             mean( .mean , na.rm = T ) ,
                             NA )
                         )

        cat( "\n - ", e$mpe)
        return( scales::percent( e$mpe )  )

      })

      key.mpe = reactive({
        req( tsForecast() )
        req( trend_Data() )

        cat('\n* evaluation_widget key.mpe()')

        predicted = tsForecast() %>%
          rename( pred = .mean )

        actual =  trend_Data() %>%
          rename( actual = total )

        keyvars = key_vars( actual )
        cat('\n - keyvars' , keyvars )

        truth = predicted %>%
           inner_join( actual , by = c( period(), keyvars  ) )

        # print( 'truth'); #print( truth )

        mid_point = round( as.integer( input$horizon ) /2  )

        e = truth %>%
          group_by_key() %>%
          index_by( 1 ) %>%
          summarise(
                mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
                             mean( actual - pred  , na.rm = T ) /
                             mean( pred , na.rm = T ) ,
                             NA ) ,
                !! period()   := nth( !! rlang::sym( period() )  , mid_point ) ,
                 actual = ifelse( mpe>=0 , max( actual, na.rm = TRUE ),
                                 min( actual, na.rm = TRUE  )
                                 #nth( actual , mid_point )
                ) ,
                just = ifelse( mpe >= 0 , 1, -1 )
                ) %>%
        as_tibble()  %>%
            mutate( !! input$agg_level :=
                      as.character( !! rlang::sym( input$agg_level  ) ) )

        if ( !split() %in% 'None' ){
           cat( '\n - key.mape grouping_var' , split() )
           e = e %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split() ) )
             )
     } else {
           e = e %>%
             mutate(  grouping_var = 'Total' )
         }

        cat( "\n - mpe"  ); #glimpse(e )
        return( e )
      })

      pi_levels = reactive({
        # req( input$forecast_ci )
        cat( '/n* pi_levels:' , input$forecast_ci )
        if ( ! input$forecast_ci ) return( NULL )
        return( 90 )
      })

# Model forecasts ####

    evaluationParameters <- reactiveValues( Month  = NULL )

    model_formula = reactive({

      req( input$model.formula )
      cat( '\n* evaluation_widget model_formula' )

      formula.string = input$model.formula
      
      # if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  )'
      # 
      # f = as.formula(  formula.string )
      # 
      # 
      # if (input$model %in% 'ARIMA' ){
      #   cat("\n - input$model = ARIMA")
      # 
      #   # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
      #   #                         ' pdq() ' )
      # 
      #   formula.string = ' total ~  pdq() '
      #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
      # 
      #   if ( period() %in% "Month" ) formula.string = paste0( formula.string ,
      #                                                      '+ PDQ( period = "1 year" )'   )
      # 
      #   if ( period() %in% "Week" ) formula.string = paste0( formula.string ,
      #                                                      '+ PDQ( period = 52 )'   )
      # 
      # 
      #    if ( nchar( input$covariates ) > 0 ) formula.string =
      #        paste( formula.string , '+ xreg(' , input$covariates , ' ) '  )
      # 
      #    cat( '\n - ARIMA formula string:', formula.string )
      #    f = as.formula( formula.string )
      # }
      # 
      # if (input$model %in% 'BSTS' ){
      #   cat("\n - input$model = BSTS")
      # 
      #    f = as.formula( paste( 'total ~ season("year")' ) )
      # 
      #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season("year")'
      # 
      # }
      # 
      # if (input$model %in% 'ETS' ){
      #   cat("\n - input$model = ETS")
      #   
      #   f = as.formula( paste( 'total ~ season("year")' ) )
      #   
      #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
      #   
      # }

      cat( '\n - end model_formula:', formula.string )
      return( formula.string )

    })
    
    # Models formulas
    modelSpecs <- reactive({ 
      req( input$model ) 
      specs = list( input$model, input$transform ) 
      return( specs )
      })
    
    observeEvent( modelSpecs() , {
      
      if (input$model %in% 'TSLM (trend+season)' ){
        cat("\n - input$model = TSLM")
        
        formula.string  = "total ~ trend() + season()" 
        
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend() + season()'
        
      }
      
      if (input$model %in% 'TSLM (trend)' ){
        cat("\n - input$model = TSLM (trend)")
        
        formula.string  =  "total ~ trend()" 
        
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend()'
        
      }


      if (input$model %in% 'ARIMA' ){
        cat("\n - input$model = ARIMA")
        
        # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
        #                         ' pdq() ' )
        
        formula.string = ' total ~  pdq() '
        
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
        
        if ( period() %in% "Month" ) formula.string = paste0( formula.string ,
                                                              '+ PDQ( period = "1 year" )'   )
        
        if ( period() %in% "Week" ) formula.string = paste0( formula.string ,
                                                             '+ PDQ( period = 52 )'   )
        
        
        if ( nchar( input$covariates ) > 0 ) formula.string =
          paste( formula.string , '+ xreg(' , input$covariates , ' ) '  )
        
      }
      
      if (input$model %in% 'BSTS' ){
        cat("\n - input$model = BSTS")
        
        formula.string  =  'total ~ season("year")' 
        
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season()'
        
      }
      
      if (input$model %in% 'ETS' ){
        cat("\n - input$model = ETS")
        
        formula.string = 'total ~ error() + trend() + season()'  
    
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
        
      }
      
      if (input$model %in% 'NNETAR' ){
        cat("\n - input$model = NNETAR")
        
        formula.string = 'total'  
        
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  )'
        
      }
      
      if (input$model %in% 'SNAIVE' ){
        cat("\n - input$model = SNAIVE")
        
        formula.string = 'total ~ lag("year")'  
      
        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ lag("year")'
        
      }
      
      updateTextInput( session, "model.formula" , value = formula.string )
      
    })
    
    # observeEvent( input$forecast , { cat("\n * observed button push") })
    
    forecast = eventReactive( input$forecast , {
      
                     req( trend_Data() )
                     req( input$evaluation_month )
              
              cat("\n * modelingDataset")
              shinyalert( "Starting forecast", "fitting multiple models..." , type = 'info', timer = 2000)
                    
              trend_Data = trend_Data()
              evaluation_month  = yearmonth( input$evaluation_month )
              startMonth = min( trend_Data$Month , na.rm = T )
              endEvalMonth = max( trend_Data$Month, na.rm = T )
              numberTestMonths = 12 
              ensemble = input$ensemble 
              
               # # Testing
              cat( '\n - saving parameters')
              save(trend_Data, evaluation_month, startMonth, endEvalMonth, numberTestMonths, ensemble , 
                   file = 'model_output.rda' )
     
           
              modelingData = dataset( data = trend_Data ,
                                 startMonth = startMonth ,
                                 startEvalMonth = yearmonth( evaluation_month ) ,
                                 numberTestMonths = numberTestMonths ,
                                 endEvalMonth = endEvalMonth ,
                                 unadjusted = TRUE ,
                                 grouping = FALSE
                                 )



              train_data = modelingData$pre.intervention.train
              test_data = modelingData$pre.intervention.test

              cat("\n * test.forecasts")
              shinyalert( "Validating models", 
                          "generating forecasts for 12-months before intervention to compare with actual" , 
                          type = 'info', timer = 2000)

              test.forecasts = tsmodels(  train_data , test_data, n_forecasts = 20 ,
                                 .var = 'total' ,
                                 numberForecastMonths = numberTestMonths ,
                                 type = NA , # c('transform and covariate' ,'transform', 'covariate' )
                                 covariate = NULL ,
                                 ensemble = ensemble , msg = TRUE ,
                                 .set.seed = TRUE )

              cat("\n * validate.forecasts")
              shinyalert( "Validating models", "determining best fitting model", type = 'info', timer = 2000)

              validations = model_metrics( test.forecasts , test_data, .var = 'total' )

              model_selection = modelSelection( validations , type = 'synchronize'  )

              cat("\n * evaluating forecasts")
              shinyalert( "Creating counterfactual", "from best fitting model", type = 'info', timer = 2000)

              evaluation.forecasts = tsmodels(  train_data , test_data,
                                              n_forecasts = 20 ,
                                        .var = 'total' ,
                                        numberForecastMonths = numberTestMonths ,
                                        type = NA , # c('transform and covariate' ,'transform', 'covariate' )
                                        covariate = NULL ,
                                        ensemble = ensemble , msg = TRUE ,
                                        .set.seed = TRUE ) %>%
              semi_join( model_selection ,  by = c(  '.model' ))


              model_output = list( actual = modelingData$fable.data ,
                                   test.forecasts = test.forecasts ,
                                   validations = validations ,
                                   model_selection = model_selection , 
                                   predicted = evaluation.forecasts  )
              
              cat("\n * impactSummary")
              
              impactSummary = impactTable( actual = modelingData$fable.data , predicted = evaluation.forecasts , 
                           grouping = FALSE, .var = 'total' )
              
              # impactSummary[ 1 ,]

    
      return( impactSummary[ 1 ,] )
      # return( paste0('test forecast has ', nrow(test.forecasts ), 'models') )

     })

    # Use the output of eventReactive in the UI
   output$forecastResult <- renderTable({
    forecast()
  })
    
    # impact.tableaux( modelingData$fable.data , evaluation.forecasts )
    
    
############### deprecated?
    
    
    tsPreModel = reactive({

      req( trend_Data() )
      req( input$evaluation_month )
      req( model_formula() )
      
      trend_Data =  trend_Data()
      model_formula = model_formula()
      evaluation_month  = input$evaluation_month 
      period = period()
      model = input$model
      transform  = input$transform 
      

      if ( !input$pre_evaluation ) return( NULL )
      cat( '\n* evaluation_widget tsPreModel():' , as.character( model_formula ) )

      eval_month = input$evaluation_month
      if ( period() %in% "Month" ) time_period = yearmonth( eval_month  ) - 12
      if ( period() %in% "Week" ) time_period = yearweek( eval_month  ) - 52

      cat("\n - time_period:" , time_period )

      fit.data  = trend_Data %>% filter( Month < time_period )

      cat("\n - nrow(trend_Data()):" , nrow( trend_Data() )  )
      cat("\n - nrow(fit.data:" , nrow( fit.data )  )
      
      # Testing:
      # saveRDS( trend_Data() , 'trend_Data.rds' )
      # saveRDS( fit.data , 'fit.data.rds' )

      model.formula = model_formula
      if ( grepl( "~" ,  model.formula , fixed = TRUE ) ) model.formula = as.formula (model.formula )

      if (input$model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( model.formula  ) )

        cat( '\n - end tsPreModel() TSLM(trend):' )
        return( fit )
      }

      if ( model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( model.formula ) )

        cat( '\n - end tsPreModel() TSLM(trend + season):' )
        return( fit )
      }

      if ( model %in% 'ARIMA' ){
        fit = fit.data %>% model(
          arima = ARIMA( model.formula  )
          )
      # if ( input$reconcile ) fit = fit %>%
      #       reconcile(
      #         mint = min_trace(a, method = "mint_shrink")
      #         )

        cat( '\n - end tsPreModel(): arima fit' )
        # glimpse( fit )
        # testing model fit for forecasts

        # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

        return( fit )
      }
      
      
      if ( model %in% 'NNETAR' ){
        fit = fit.data %>%
          model(
            nnetar = NNETAR( total  )
          )
        
        if ( input$transform ) fit = fit.data %>% model( nnetar = NNETAR( fabletools::box_cox( total , lambda = .5  )  ) )
        
        
        cat( '\n - end tsModel():' )
        return( fit )
      }
      
      if ( model %in% 'BSTS' ){
        fit = fit.data %>%
          model(
            # b = BSTS( model_formula() )
            bsts = BSTS( model.formula )
            )

        cat( '\n - end tsPreModel() BSTS:' )
        return( fit )
      }

      if ( model %in% 'ETS' ){
        
        fit = fit.data %>% model( ets = ETS( !! model.formula ))
    
         # if ( input$reconcile ) fit = fit %>%
        #       reconcile(
        #         mint = min_trace(a, method = "mint_shrink")
        #         )
        cat( '\n - end tsModel():' )
        return( fit )
      }
      
      if ( model %in% 'SNAIVE' ){
        
        fit = fit.data %>% model( ets = SNAIVE(  model.formula ) )
        
        cat( '\n - end tsModel():' )
        return( fit )
      }

      if ( model %in% 'Prophet' ){


        if (  transform ){
          
          fit =  fit.data %>% model(
                prophet = prophet( fabletools::box_cox( total , lambda = .5  )  ~

                                        growth( type = 'linear',
                                                changepoint_range = 1 ,
                                                changepoint_prior_scale = 1 ,
                                                # capacity = 1e5 ,
                                                # floor = 0
                                                ) +
                                        season(period = 12,
                                               order = 4 ,
                                               type='multiplicative'),
                                   seed = TRUE )
            ) 
        } else {
          
          fit =  fit.data %>% model(
            prophet = prophet( total ~
                                 
                                 growth( type = 'linear',
                                         changepoint_range = 1 ,
                                         changepoint_prior_scale = 1 ,
                                         # capacity = 1e5 ,
                                         # floor = 0
                                 ) +
                                 season(period = 12,
                                        order = 4 ,
                                        type='multiplicative'),
                               seed = TRUE )
          )
          
          
        }

        cat( '\n - end tsPreModel() Prophet:' )
        return( fit )
      }


    })

    tsForecast = reactive({

      req( tsModel() )
      req( input$horizon )
      cat( '\n* evaluation_widget tsForecast()' )
      
      trend_Data =  trend_Data()
      model_formula = model_formula()
      evaluation_month  = input$evaluation_month 
      period = period()
      model = input$model
      transform  = input$transform 
      horizon = input$horizon
      bootstrap = input$bootstrap 
      Reps = input$Reps
      covariates = input$covariates
      tsModel = tsModel()
      
      eval_month = yearmonth( evaluation_month ) 
      time_period = seq( eval_month , eval_month + as.integer( horizon ) -1 , by = 1 ) 
      
      if ( period() %in% "Month" ) time_period = yearmonth( evaluation_month  ) # - month(1)
      if ( period() %in% "Week" ) time_period = yearweek( evaluation_month  )
      
      forecast.fit.data  = trend_Data %>%
        select( - total ) %>%
        # filter_index( as.character( time_period ) ~ . ,
        #             .preserve = TRUE ) %>%
        filter( Month %in% time_period )
      
      
      if ( input$bootstrap ){

        fcast = model %>%
          fabletools::forecast( h = as.integer( horizon ) ,
                    bootstrap = TRUE,
                    times = as.integer( Reps )
          )
      } else {

        # if ( nchar( covariates ) > 0 ){
        #   cat( '\n - covariates')
        #   
        #   fcast = tsModel %>% fabletools::forecast( new_data = forecast.fit.data )
        #   
        # } else {
        #   
        #   fcast = tsModel %>% fabletools::forecast(  h = as.integer( horizon ) )
        # }
        #   fcast = tsModel %>% fabletools::forecast(  h = as.integer( horizon ) )
      }

        # fcast = tsModel %>% fabletools::forecast( h=14 )
        fcast = tsModel %>% fabletools::forecast(  h = as.integer( horizon ) )
      #   fcast = tsModel %>% fabletools::forecast(  new_data = fd )
      # fcast = tsModel %>% fabletools::forecast( new_data = forecast.fit.data )
        
      # preserve tsibble key and index,
      indexVar = index_var( fcast )
      keyVars = key_vars( fcast )


      fcast = fcast %>%
          mutate( !! input$agg_level :=
                    as.character( !! rlang::sym( input$agg_level  ) ) )

      if ( !split() %in% 'None' ){
           cat( '\n - tsForecast grouping_var' , split() )
           fcast = fcast %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split() ) )
             )
      } else {
           fcast = fcast %>%
             mutate(  grouping_var = 'Total' )
      }

      # Ensure result is tstiblle
      # fcast = fcast %>%
      #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
      #        fill_gaps( .full = TRUE  )

      # Reconcile
      if ( input$agg_method %in% "None" ){
        if ( input$agg_method %in% 'Bottom up' ){
            fcast = fcast %>%
              reconcile( bu = bottom_up(base) )
        }
        if ( input$agg_method %in% 'MINT(ols)' ){
          fcast = fcast %>%
              reconcile( ols = min_trace(base, method = "ols") )
        }
        if ( input$agg_method %in% 'MINT(cov)' ){
          fcast = fcast %>%
              reconcile( mint = min_trace(base, method = "mint_cov") )
        }
      }

      # saveRDS( fcast , 'tsForecast.rds')
      cat( '\n - fcast end:' );  #glimpse( fcast )
      # print( names( fcast ) )
      return( fcast )
      })

    tsPreForecast = reactive({

      req( tsPreModel() )
      req( input$horizon )
      req( input$evaluation_month )

      eval_month = yearmonth( input$evaluation_month ) 
      time_period = seq( eval_month - 11  , eval_month , by = 1 )

      cat( '\n* evaluation_widget tsPreForecast' )
      # if ( input$covariates %in% "avg_mm"){

      
      cat( '\n - evaluation_widget test.data' )
      # testing
      # saveRDS( trend_Data() , 'trend_Data.rds')
      # saveRDS( time_period , 'time_period.rds')
      # saveRDS( input$horizon , 'horizon.rds')
      
  
      test.data  = trend_Data() %>%
        select( - total ) %>%
        filter( Month %in% time_period )
      

        # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
        #          bootstrap = FALSE , Reps = 1000 )


        if ( nchar( input$covariates ) > 0 ){
          cat( "\n - input$covariates:" , input$covariates )
          if ( period() %in% "Month" ) time_period = yearmonth( eval_month  ) # - month(1)
          if ( period() %in% "Week" ) time_period = yearweek( eval_month  )
 
          fcast = tsPreModel() %>% fabletools::forecast( new_data = test.data )

      } else {
        
        cat( '\n - evaluation_widget tsPreModel() %>% fabletools::forecast' )
        # Testing
        # saveRDS( tsPreModel() , 'tsPreModel.rds')
        # saveRDS( pi_levels() , 'pi_levels.rds' )

        if ( period() %in% 'Month' ) fcast = tsPreModel() %>% fabletools::forecast(h = "12 months" , level = pi_levels() ) 
        if ( period() %in% 'Week' ) fcast = tsPreModel() %>% fabletools::forecast(h = "52 weeks" , level = pi_levels() ) 

      }

      # preserve tsibble key and index,
      indexVar = index_var( fcast )
      keyVars = key_vars( fcast )

      cat( '\n - tsPreForecast done.  Adding agg_level' )

      fcast = fcast %>%
          mutate( !! input$agg_level :=
                    as.character( !! rlang::sym( input$agg_level  ) ) )

      cat( '\n - tsPreForecast grouping_var' , split() )
      if ( !split() %in% 'None' ){
           fcast = fcast %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split() ) )
             )

      } else {
           fcast = fcast %>%
             mutate(  grouping_var = 'Total' )
         }
      cat( '\n - tsPreForecast grouping_var values:' , unique(fcast$grouping_var) )

      # Ensure result is tsibble
      # fcast = fcast %>%
      #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
      #        fill_gaps( .full = TRUE  )

      cat( '\n - tsPreForecast done.' )
      # print( names( fcast ) )
      
      # Testing:
      # saveRDS( fcast , 'tsPreForecast.rds' )
      
      return( fcast )
      })

# Trend data ####
    
    trend_Data = reactive({
      
      cat( "\n * evaluation_widget trend_Data")
      
      cat( "\n missing_reports: " , missing_reports() )
      cat( "\n covariates: " , input$covariates )
      cat( "\n split: " , split()  )
      cat( "\n agg_level: " , input$agg_level  )
      
      error = NULL
      if ( ! input$error == "Original" ) error = input$error 
      cat( "\n error: " , error  )
      
      levelNames = orgUnitLevels()$levelName
      cat( "\n - levelNames" , levelNames )
      
      cat( "\n - mable.data" )
      
      selected_data = selected_data()
      startingMonth = startingMonth()
      endingMonth = endingMonth()
      split = split()
      agg_level = input$agg_level
      missing_reports = missing_reports()
  
      
      if ( testing ) save(selected_data, startingMonth, endingMonth, error , missing_reports ,
                          split , agg_level , levelNames, file = "trend_Data_inputs.rda")
      
      mable.data = mable_data(      
                                    ml.rtss.data = selected_data ,
                                    .orgUnit = FALSE , # group by orgunit
                                    .startingMonth = startingMonth ,
                                    .endingMonth = endingMonth ,
                                    .missing_reports = missing_reports ,
                                    selected.only = input$selected ,
                                    # alwaysReporting = input$selected , 
                                    # reportingSelectedOUs = reportingSelectedOUs() ,
                                    covariates =  input$covariates , 
                                    .split = split , 
                                    .error = error ,
                                    agg_level = agg_level ,
                                    levelNames = levelNames ,
                                    remove.aggregate = TRUE ,
                                    .cat = TRUE ,
                                    # Testing:
                                    testing = FALSE 
                                    )
      
      
      # # testing
      if ( testing ) saveRDS( mable.data, "mable.data.rds")
      return( mable.data )
      
    })
 

# Plot ####
  plotTrends = reactive({

          req( trend_Data() )

          cat( '\n* evaluation_widget plotTrends():' )

          .limits =
          if ( input$scale ){
            c( NA , NA ) } else {
              c( 0 , NA )
          }

          data.text = paste( unique( selected_data()$data ), collapse = " + " )
          
          cat( '\n - ploTrends trend_Data():');
          trend_Data = trend_Data()
          cat( '\n - ploTrends .d:'); #glimpse(.d)

          # if ( !input$filter_display %in% 'All' ) .d = .d %>%
          #         filter( .data[[ split() ]] %in%
          #                   input$filter_display )
          
          # Testing
          if (testing )  saveRDS( trend_Data , "trend_Data.rds")
          
          tic()

          .period = period()

      ## Main plot ####
          cat( "\n - main plot")
          
          
          g = trend_Data %>%
              filter( !is.na( total ) ) %>%
              autoplot( total ) +
              # ggplot( aes( x = !! rlang::sym( .period ), y = total
              # 
              #            , group =  as.character( !! rlang::sym( input$agg_level  ) )
              # 
              #            , color =  as.character( !! rlang::sym( input$agg_level  ) )
              #           ) )  +
              # geom_line() +
              theme_minimal()

          # Testing
          # save(.d, file = 'plot-trend-test-data.rda')


          cat( '\n - basic plot done' ); toc()

          if ( !input$legend ) g = g +
            theme(legend.position = "none")

          if ( input$label ){
            g = g + geom_label_repel(
                       data = trend_Data %>%
                         filter(
                           !! rlang::sym( .period ) == max( trend_Data %>% pull( .period ) ,
                                                            na.rm = T )
                           ) ,
                       aes( label = grouping_var ,
                            group = grouping_var )
                       )
          }

          # Determine number of agg levels available
          # If only one, do not facet (causes error, perhaps because of autoplot?)

          num_agg_levels = count( trend_Data %>% as_tibble ,
                                  !! rlang::sym( input$agg_level ) ) %>%
            nrow()

          # if ( input$agg_level != levelNames()[1] & input$facet_admin ){
          if ( num_agg_levels  > 1 & input$facet_admin ){
            cat( '\n -  admin facets' )

            if ( input$facet_split ){
              cat( '\n -  facet admin - split' )

                g = g +
                    facet_grid( rows = vars( as.character( !! rlang::sym( input$agg_level ) ) ) ,
                                cols = grouping_var   ,
                                   scales = "free_y" )
          } else {

            g = g +
            facet_wrap( vars( as.character( !! rlang::sym( input$agg_level ) ) ) ,
                           scales = "free_y" )
          }} else {

           if ( input$facet_split ){
            cat( '\n - facet_split' )
            g = g +
            facet_wrap( ~ grouping_var   ,
                           scales = "free_y" )
          }
            }

          # Time scale
          cat( '\n - Evaluation: setting x axis time scale', period() )
          if ( .period %in% 'Month') g = g + scale_x_yearmonth("", date_breaks = "1 year" )
          # Default for weeks seems ok - 6 months
          # if ( .period %in% 'Week') g = g + scale_x_yearweek("", date_breaks = "1 year" )

          g = g +
            scale_y_continuous( label = comma, limits = .limits ) +
            scale_color_discrete( drop = TRUE  ) +
            labs( y = "" , x="" ,
                  title = str_wrap( input$indicator , 200 ) ,
                  subtitle = str_wrap( data.text , 200 )
                  , caption =  str_wrap( caption.text() , 200 )
                  )
          cat( '\n - axis scales and labs done' )

          # Eval Date
          cat( '\n - evaluation date' , input$evaluation_month )
          if ( .period %in% 'Month' ) eval_date =   yearmonth( input$evaluation_month  )
          if ( .period %in% 'Week' ) eval_date =   yearweek( input$evaluation_month  )
          cat( '\n - eval_date:' , eval_date )

      ## Pre-Evaluation trend line #####
          if ( input$pre_evaluation ){
          cat( '\n - pre-evaluation line.  ' )
          cat( '\n - pi_levels:' , pi_levels() )

          cat( '\n - pre-evaluation date'  )
          if ( .period %in% 'Month' ) pre_eval_date =   yearmonth( input$evaluation_month  ) -12
          if ( .period %in% 'Week' ) pre_eval_date =   yearweek( input$evaluation_month  ) - 25
          cat( '\n - pre_eval_date:' , pre_eval_date )

          g = g +
            fabletools::autolayer( tsPreForecast()
                       , level = ifelse( input$forecast_ci , 89 , FALSE )
                       , color = 'black'
                       , linetype = 'dotted'  , linewidth = 2
                       ,  alpha = .75 ) +
            # geom_line( data = tsPreForecast(), aes(  y = .mean )
            #   # ,   color = 'light blue'
            #   , alpha = .75
            #   , linetype = 'dotted'  , size = 2
            # ) +
            # geom_vline( xintercept = as.Date( pre_eval_date ) ,
            #             color = 'brown' ,
            #             alpha = .25 ) +
            geom_vline( xintercept = as.Date( eval_date ) ,
                        color = 'black', alpha = 1 )

            if ( input$pe ) g = g +
              geom_label_repel( data =  key.mape() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual ,
                       label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
                       hjust = just ) ,
                       # force_pull = 0 ,
                       segment.colour = NA
                       )

          }

          cat( '\n - pre-evaluation line done' )


      ## Evaluation Trend Line ####
          if ( input$evaluation ){
            cat( '\n - evaluation line.  ')
            cat( '\n - evaluation line.  ' , 'pi_levels:' , pi_levels() )

           g = g +
            fabletools::autolayer( tsForecast()
                       # , level = 80  # pi_levels()
                       , color = 'black'
                       , level = ifelse( input$forecast_ci , 89 , FALSE )
                       , linetype = 'dashed', linewidth = 1
                       ,  alpha = .5 ) +

            geom_vline( xintercept = as.Date( eval_date ) ,
                        color = 'blue', alpha = 1 )

            # annotate( "text" ,
            #           x = as.Date( eval_date ) ,
            #           y = Inf ,
            #           hjust = 0 , vjust = 1 ,
            #           label = paste( "MPE:\n" )
            #           ) +

            if (input$pe) g = g +
              geom_label_repel( data =  key.mpe() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual ,
                       label = paste( "MPE:" , percent( mpe, accuracy = 1.0 ) ) ,
                       hjust = just
                       ) ,
                       # force_pull = 0 ,
                       segment.colour = NA
                       )
          }
          
      ## prediction Interval
          # if ( ){
          #   
          #   g = g + 
          #     geom_line( data = tsForecast() , aes( y = .mean )
          #              # ,   color = 'light blue'
          #              , alpha = .75
          #              , linetype = 'dotted'  , size = 2
          #   ) +
          # }

          cat( '\n - evaluation line done' )


      ## Smooth line #####
          if (input$smooth){
            cat( '\n - agg level', input$agg_level )
            .d. = .d %>%
              as_tibble %>%
              mutate( !! input$agg_level := as.character( !! rlang::sym( input$agg_level  ) ) )

            cat( '\n - smooth .d.') ; #glimpse(.d. )
            g = g +
            geom_smooth( data = .d. ,
                         alpha = .75 )

          }


      ## End ####
          cat( '\n - end plotTrends():' )

          # saveRDS( g, 'plotTrends.rds')
          return( g )
        })

  plotComponents = reactive({

      req( tsModel() )
      req( input$evaluation_month )
      cat( '\n* evaluation_widget plotComponenets():' )

      g = tsModel() %>% fabletools::components() %>% autoplot

      cat( '\n - end plotComponents():' )

      return( g )
})

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


