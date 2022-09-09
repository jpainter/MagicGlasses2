evaluation_widget_ui = function ( id ){
        ns <- NS(id)  
        # fillCol( height = 600, flex = c(NA ) , 
        
    tagList( 
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,
          
  tabsetPanel( type = "tabs",
  # add_busy_spinner(spin = "fading-circle", position = "bottom-right") ,

  tabPanel( 'Impact' ,
        sidebarLayout(
          sidebarPanel(
            # width = "25%" ,
            
            selectizeInput( ns("evaluation_month") , label = "Intervention Start", 
                    choices = NULL ,
                    selected = NULL ) ,
            
            # div(id = "expr-container",
              selectInput( ns("horizon") , label = "Number periods after intervention:" , 
                            choices = c(3,6,12,18,24,36) , 
                            selected = 12  ) ,

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
                             value = FALSE  ) ,
              
              checkboxInput( ns( "facet_split" ) , label ="Facet by split",
                             value = FALSE  ) ,
              
              checkboxInput( ns( "selected" ) , label ='Selected facilities only',
                             value = TRUE  ) ,
              
              checkboxInput( ns( "label" ) , label ='Show labels',
                             value = FALSE  ) ,
            
              checkboxInput( ns( "pe" ) , label ='Show mean percent difference from expected',
                             value = TRUE  ) ,
            
              checkboxInput( ns( "legend" ) , label ='Show legend',
                             value = FALSE  )
              
              # checkboxInput( ns( "plotly" ) , label ='Plotly Chart',
              #                value = FALSE  ) 
          ),
          
          mainPanel( 
               # width = "75%" ,
                # conditionalPanel( "input.plotly == 1" , ns = ns ,
                #     plotlyOutput( ns("plotlyOutput") , height = "100%" )
                #       ) ,
                
                # conditionalPanel( "input.plotly == 0" , ns = ns ,
                #     plotOutput( ns("plotOutput") , height = "600px" ,
                #              hover = "plot_hover"  )
                #  )
                
             tabsetPanel(
                  tabPanel( "Plot" ,
                    
                    fluidPage(
                      fluidRow( style = "height:60vh;",
                                plotOutput( ns("plotOutput") ) )
                      ) ) ,
                  tabPanel("Plotly", plotlyOutput(  ns("plotlyOutput") ) ),
                  tabPanel("Table", plotlyOutput( ns("tableOutput" )  ) )
                )
                 
             # plotOutput( ns( "plotOutput" ) , hover = "plot_hover"  )
          )
        )    

,
    
  tabPanel( "Model",  
    inputPanel(
      
      selectInput( ns( "model" ), label = "Time-series model:" , 
              choices = c( 
                          # 'TSLM',
                           'TSLM (trend)' , 'TSLM (trend+season)' , 
                           'ETS' , 'ARIMA', 
                           # 'BSTS' , 
                          'Prophet'
                          
                          # , 'TSLM (trend)'
                          # , 'TSLM (trend+season)'
                          ) , 
              selected = 'ARIMA'  ) ,

      textInput( ns( 'model.formula' ) , 'Model Formula' ,
          value =  'total ~ 1' ) ,

      textInput( ns( 'covariates' ), 'Model covariates' ,
          value =  NULL ) ,
      
      checkboxInput( ns( "transform" ) , label ='Transform: box_cox(lambda = .5  )',
                   value = FALSE  ) ,
      
    checkboxInput( ns( "smooth" ) , label ='Show smoothed trend line (loess)',
                   value = FALSE  ) ,
    
    checkboxInput( ns( "pre_evaluation") , label ='Pre-intervention model fit',
                   value = FALSE  ) ,
    
    
    checkboxInput( ns( "evaluation" ), label ='Post-intervention evaluation',
                   value = FALSE  ) ,
    
    checkboxInput( ns( "scale" ) , label ='Scale values (x-mean)/sd + 1)',
                   value = FALSE  ) ,
    
    checkboxInput( ns( 'components' ), label = 'Visualize trend' ,
                value = FALSE ) ,
    
    checkboxInput( ns( "forecast_ci" ) , label ='Prediction interval',
                   value = FALSE  ) ,
    
    checkboxInput( ns( "bootstrap" ) , label ='Bootstrap estimate',
                   value = FALSE  ) 
    
          )
)
))
)

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
    formula_elements = reactive({ data_widget_output$formula_elements() })
    
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    
    dates = reactive({ reporting_widget_output$dates() })
    # dataset = reactive({ reporting_widget_output$data1() })
    # data.hts = reactive({ reporting_widget_output$data.hts() })
    levelNames = reactive({ reporting_widget_output$levelNames() })
    period = reactive({ reporting_widget_output$period() })
    split = reactive({ reporting_widget_output$split() })
    startingMonth = reactive({ reporting_widget_output$startingMonth() })
    endingMonth = reactive({ reporting_widget_output$endingMonth() })
    num_datasets = reactive({ reporting_widget_output$num_datasets() })
    num_facilities = reactive({ reporting_widget_output$num_facilities() })
    plotData = reactive({ reporting_widget_output$plotData() })
    caption.text = reactive({ reporting_widget_output$caption.text() })
    data.total = reactive({ reporting_widget_output$data.total() })
    selectedOUs = reactive({ reporting_widget_output$selectedOUs() })


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

    observeEvent(
      levelNames() ,{

      cat('\n* evaluation_widget update agg_level:');
      updateSelectInput( session, 'agg_level' ,

                              choices = levelNames() ,
                              selected = levelNames()[1] ) # 12 months before latest date
  } )

    # observeEvent(
    #   levelNames() ,{
    #   req( data1() )
    #   cat('\n* evaluation_widget update level2l:');
    #   updateSelectInput( session, 'level2' ,
    #                               choices =
    #                                 # setDT( data1() )[ , base::get(levelNames()[2] )] %>%
    #                                 #   unique %>% str_sort() ,
    # 
    #                                 data1() %>%
    #                                   pull( !! rlang::sym( levelNames()[2]  ) ) %>%
    #                                   unique %>% str_sort(),
    # 
    #                               selected = NULL
    #                               )
    #   } )

    # observeEvent(
    #   levelNames() ,{
    #   req( data1() )
    #   cat('\n* evaluation_widget update level3:');
    #   updateSelectInput( session, 'level3' ,
    #                               choices =
    #                                 # setDT( data1() )[ base::get(levelNames()[2] ) %in% input$level2 ,
    #                                 #                   base::get( levelNames()[3] )] %>%
    #                                 #   unique %>% str_sort() ,
    # 
    #                                 data1() %>%
    #                                   filter(
    #                                   !! rlang::sym( levelNames()[2] ) %in% input$level2 ) %>%
    #                                   pull( !! rlang::sym( levelNames()[3]  ) ) %>%
    #                                   unique %>% str_sort() ,
    # 
    #                               selected = NULL
    #                               )
    #   } )

    # observeEvent(
    #   levelNames() ,{
    # 
    #   req( data1() )
    #   cat('\n* evaluation_widget update level4:');
    #   updateSelectInput( session, 'level4' ,
    #                               choices =
    #                                 # setDT( data1() )[ base::get(levelNames()[3] ) %in% input$level2 ,
    #                                 #                   base::get( levelNames()[4] )] %>%
    #                                 #   unique %>% str_sort() ,
    # 
    #                                 data1() %>%
    #                                   filter(
    #                                   !! rlang::sym( levelNames()[3] ) %in% input$level3 ) %>%
    #                                   pull( !! rlang::sym( levelNames()[4]  ) ) %>%
    #                                           unique %>% str_sort(),
    #                               selected = NULL
    #                               )
    #   } )

    # level5 = reactive({
    #     req( input$level4 )
    #     req( levelNames() )
    # 
    # 
    #     if( is.na( levelNames()[5] ) ) return( NA )
    # 
    #     cat('\n* evaluation_widget level5():');
    #     # setDT( data1() )[ base::get(levelNames()[4] ) %in% input$level2 ,
    #     #                                               base::get( levelNames()[5] )] %>%
    #     #                               unique %>% str_sort()
    # 
    #     data1() %>%
    #         filter(
    #             !! rlang::sym( levelNames()[4] ) %in%
    #                        input$level4 ) %>%
    #         pull( !! rlang::sym( levelNames()[5]  ) ) %>%
    #         unique %>% str_sort()
    # })

  #   observeEvent(
  #     level5() , {
  #     cat('\n* evaluation_widget update level5:');
  #     updateSelectInput( session, 'level5' ,
  #                                 choices = level5(),
  #             selected = NULL
  #   )
  # 
  # } )

# Impact ####
      observeEvent(
        trendData() , {
        cat('\n* evaluation_widget update var_y:');
        updateSelectInput( session, 'var_y' ,
                choices =  names( trendData() )
                )
        } )

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
        actual =  trendData()
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
        req( trendData() )

        cat('\n* evaluation_widget key.mape()')

        predicted = tsPreForecast() %>%
          rename( pred = .mean )

        actual =  trendData() %>%
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
        actual =  trendData()

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
        req( trendData() )

        cat('\n* evaluation_widget key.mpe()')

        predicted = tsForecast() %>%
          rename( pred = .mean )

        actual =  trendData() %>%
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


      # if (input$model %in% 'BSTS'){
      #   f = as.formula( 'total ~ intercept()' )
      #
      # }

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

         cat( '\n - ARIMA formula string:', formula.string )
         f = as.formula( formula.string )
      }

      if (input$model %in% 'BSTS' ){
        cat("\n - input$model = BSTS")

         f = as.formula( paste( 'total ~ season("year")' ) )

        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season("year")'

      }


      if ( any(input$model %in% c( 'TSLM', 'TSLM (trend+season)' ,'TSLM (trend)',
                                    'ETS',  'Prophet' ) ) ){
        cat("\n - input$model not ARIMA or BSTS' )")

        f = as.formula(  input$model.formula )

        formula.string = paste( 'total' )

        if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  )'

        # f = as.formula(  formula.string )

        }

      cat( '\n - end model_formula:', formula.string )
      return( f )

    })

    tsModel = reactive({
      req( trendData() )
      req( model_formula() )
      req( input$evaluation_month )

      if ( !input$evaluation ) return( NULL )
      cat( '\n* evaluation_widget tsModel():' )
      cat( '\n - ' , paste('available vars:',
                   paste( names(trendData()), collapse = ',')
                   )
      )

      # Dickey-Fuller test for stationary series
      # Null hypothese is non-stationary.
      # Evidence that series is stationary when p-v < .05
      # dickeyFuller = tseries::adf.test( trendData()$total )
      # print( dickeyFuller )

      # Filter data to period just before evaluation start
      print( input$evaluation_month )
      eval_month = input$evaluation_month
      if ( period() %in% "Month" ) time_period = yearmonth( eval_month  ) # - month(1)
      if ( period() %in% "Week" ) time_period = yearweek( eval_month  )

      fit.data  = trendData() %>%
        filter_index( ~ as.character( time_period ) ,
                      .preserve = TRUE )

      if (input$model %in% 'TSLM' ){
        fit = fit.data %>% model( l = TSLM( model_formula() ) )
        cat( '\n - end tsModel():' )
        return( fit )
        }

      if (input$model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend()  ) )

        if ( input$transform ) fit = fit.data %>% model( l = TSLM( fabletools::box_cox( total , lambda = .5  )  ~ trend()  ) )

        cat( '\n - end tsModel():' )
        return( fit )
      }

      if (input$model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend() + season() ) )

        if ( input$transform ) fit = fit.data %>% model( l = TSLM( fabletools::box_cox( total , lambda = .5  )  ~ trend() + season() ) )

        cat( '\n - end tsModel():' )
        return( fit )
      }

      if (input$model %in% 'ARIMA' ){
        fit = fit.data %>% model(
          arima = ARIMA( model_formula()  )
          )
      # if ( input$reconcile ) fit = fit %>%
      #       reconcile(
      #         mint = min_trace(a, method = "mint_shrink")
      #         )

        cat( '\n - end tsModel(): arima fit' )
        # glimpse( fit )
        # testing model fit for forecasts

        # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

        return( fit )
      }

      if (input$model %in% 'BSTS' ){
        fit = fit.data %>%
          model(
            # b = BSTS( model_formula() )
            bsts = BSTS( total ~  ar() + seasonal("1 year"))
            )

        if ( input$transform ) fit = fit.data %>% model( bsts = BSTS( fabletools::box_cox( total , lambda = .5  )  ~ ar() + seasonal("1 year") ) )


        cat( '\n - end tsModel():' )
        return( fit )
      }

      if (input$model %in% 'ETS' ){

        # if ( input$transform ){
        #   fit = fit.data %>% model( a = ETS( fabletools::box_cox( total , lambda = .5  ) )  )
        # } else {
          fit = fit.data %>% model( a = ETS( total )  )

          if ( input$transform ) fit = fit.data %>% model( a = ETS( fabletools::box_cox( total , lambda = .5  )  ) )


        # }


        cat( '\n - end ETS tsModel():' )

      # if ( input$reconcile ) fit = fit %>%
      #       reconcile(
      #         mint = min_trace(a, method = "mint_shrink")
      #         )
        cat( '\n - end tsModel():' )
        return( fit )
      }

      if (input$model %in% 'Prophet' ){
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


        if ( input$transform ) fit =  fit.data %>% model(
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

        cat( '\n - end tsModel():' )
        return( fit )
      }

    })

    tsPreModel = reactive({

      req( trendData() )
      req( input$evaluation_month )
      req( model_formula() )

      if ( !input$pre_evaluation ) return( NULL )
      cat( '\n* evaluation_widget tsPreModel():' , as.character( model_formula() ) )

      eval_month = input$evaluation_month
      if ( period() %in% "Month" ) time_period = yearmonth( eval_month  ) - 12
      if ( period() %in% "Week" ) time_period = yearweek( eval_month  ) - 52

      cat("\n - time_period:" , time_period )

      fit.data  = trendData() %>%
        filter_index( ~ as.character( time_period ) ,
                      .preserve = TRUE )

      cat("\n - nrow(trendData()):" , nrow( trendData() )  )
      cat("\n - nrow(fit.data:" , nrow( fit.data )  )
      # saveRDS( trendData() , 'trendData.rds' )
      saveRDS( fit.data , 'fit.data.rds' )


      if (input$model %in% 'TSLM' ){
        fit = fit.data %>% model( l = TSLM( model_formula() ) )
        # print( 'end tsPreModel() TSLM:' )
        cat( '\n - end tsPreModel() TSLM:' )
        return( fit )
        }

      if (input$model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend()  ) )

        if ( input$transform ) fit = fit.data %>% model( l = TSLM( fabletools::box_cox( total , lambda = .5  )  ~ trend()  ) )

        cat( '\n - end tsPreModel() TSLM(trend):' )
        return( fit )
      }

      if (input$model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend() + season() ) )

        if ( input$transform ) fit = fit.data %>% model( l = TSLM( fabletools::box_cox( total , lambda = .5  )  ~ trend() + season() ) )

        cat( '\n - end tsPreModel() TSLM(trend + season):' )
        return( fit )
      }

      if (input$model %in% 'ARIMA' ){
        fit = fit.data %>% model(
          arima = ARIMA( model_formula()  )
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

      if (input$model %in% 'BSTS' ){
        fit = fit.data %>%
          model(
            # b = BSTS( model_formula() )
            bsts = BSTS( total ~  ar() + seasonal("1 year"))
            )

        if ( input$transform ) fit = fit.data %>% model( bsts = BSTS( fabletools::box_cox( total , lambda = .5  )  ~ ar() + seasonal("1 year") ) )


        cat( '\n - end tsPreModel() BSTS:' )
        return( fit )
      }

      if (input$model %in% 'ETS' ){


          fit = fit.data %>% model( a = ETS( total )  )

          if ( input$transform ) fit = fit.data %>% model( a = ETS( fabletools::box_cox( total , lambda = .5  )  ) )


      # if ( input$reconcile ) fit = fit %>%
      #       reconcile(
      #         mint = min_trace(a, method = "mint_shrink")
      #         )
        cat( '\n - end tsPreModel() ETS:' )
        return( fit )
      }

      if (input$model %in% 'Prophet' ){
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


        if ( input$transform ) fit =  fit.data %>% model(
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

        cat( '\n - end tsPreModel() Prophet:' )
        return( fit )
      }


    })

    tsForecast = reactive({

      req( tsModel() )
      req( input$horizon )
      cat( '\n* evaluation_widget tsForecast()' )

      if ( input$bootstrap ){

        fcast = tsModel() %>%
          forecast( h = as.numeric( input$horizon ) ,
                    bootstrap = TRUE,
                    times = as.integer( input$Reps )
          )
      } else {
        fcast = tsModel() %>%
          forecast( h = as.numeric( input$horizon ) )
      }

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
      fcast = fcast %>%
             as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
             fill_gaps( .full = TRUE  )

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

      eval_month = input$evaluation_month
      time_period = yearmonth( eval_month  ) - 12

      cat( '\n* evaluation_widget tsPreForecast' )
      if ( input$covariates %in% "avg_mm"){

        test.data  = trendData() %>%
          filter_index( as.character( time_period ) ~ as.character( time_period + as.integer( input$horizon ) ) ,
                      .preserve = TRUE )

        fcast= getForecast( test_data = test.data , model = tsPreModel() ,
                 bootstrap = FALSE , Reps = 1000 )

        # if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
        # if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )

      } else {

        if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
        if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )

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
      fcast = fcast %>%
             as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
             fill_gaps( .full = TRUE  )

      cat( '\n - tsPreForecast done.' )
      print( names( fcast ) )
      # saveRDS( fcast , 'tsPreForecast.rds' )
      return( fcast )
      })

# Trend data ####

  hts = reactive({
    req( num_facilities() )
    req( num_datasets() )
    req( input$agg_level )
    req( levelNames() )
    cat("\n* evaluation_widget hts():" )

    adms = backtick( levelNames() )

    if (input$hts){
      hts = paste( adms, collapse = "/" )

    } else {

      cat( '\n - adms:',  adms )
      cat( '\n - input$agg_level:',  input$agg_level )

      hts_level = which( input$agg_level == levelNames()   )

      cat( '\n - hts_level:',  hts_level )

      hts = paste( adms[1:( hts_level + 1 )] ,
                   collapse = "/" )
    }

    hts = paste( "(" , hts , ")" )

    # if >1 Facilities (ie. selected)
    if ( num_facilities() > 1 )  hts = paste(
             'Selected *' , hts
             )

    # if >1 dataset
    if ( num_datasets() > 1 )  hts = paste(
             'dataSet *' , hts
             )

    # # Cross by split
    if ( !split() %in% 'None' ) hts =
      paste( backtick( split() ) , '*' ,  hts )
    #
    # Cross by selected and split
    # if ( length( selectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
    #   paste( input$split ,  ' * Facilities * (', hts , ')' )

    saveRDS( hts, 'hts.rds' )

    cat("\n - end hts():" , hts )

    return( hts )
  })

  data.hts = reactive({
    req( data.total() )
    req( hts() )
    tic()

    cat('\n* evaluation_widget data.hts():' )
    cat( "\n - data.total cols:" , paste( names( data.total() ) , collapse = "," ) )

    # Testing
    # saveRDS( data.total(), 'data.total.hts.rds' )

    data.hts = data.total()

     # if ( input$transform ) .d = .d %>% mutate( .total = fabletools::box_cox( total , lambda = .5  ) )

    if ( grepl( "avg_mm" , input$covariates ) & "avg_mm" %in% names( data.hts ) ){
      cat( "\n - grepl( 'avg_mm' , input$covariates ) & 'avg_mm' %in% names( data.hts )")
    # testing exogenous vaiables
    # if ( input$covariates %in% c('ipti' , 'doses' ) ){
      data.hts = data.hts %>%
      aggregate_key(  .spec = !!rlang::parse_expr( hts() ) ,
                      total = sum( total , na.rm = T ) ,
                      avg_mm = mean( !!rlang::parse_expr( 'avg_mm' ) , na.rm = T )
                      # ipti = sum( !!rlang::parse_expr( 'ipti' ) , na.rm = T ) ,
                      # doses = sum( !!rlang::parse_expr( 'doses' ) , na.rm = T )
                      )
    } else {
      
    data.hts = data.hts %>%
      aggregate_key(  .spec = !!rlang::parse_expr( hts() ) ,
                      total = sum( total , na.rm = T )
                      )
    }

    cat('\n- end data.hts():' ) ; toc()
    # saveRDS( .d, 'data.hts.rds' )

    return( data.hts )
  })

  trendData = reactive({
      # req( data.hts() )
      # req( aggregatePlotData() )
    cat( '\n* evaluation_widget: trendData(): ' )

    .d = data.hts()
    # cat( '\n - data.hts datasets:' , unique( .d$dataSet ) )

     if ( input$selected  & num_facilities() > 1 ){

      cat( '\n - input$selected TRUE' )

      .d = .d %>% filter(
        Selected ==  'Reporting Each Period' )

      if ( period() %in% 'Month' ){
        .d = .d %>% filter(
          Month >=  yearmonth( startingMonth() )   ,
          Month <= yearmonth( endingMonth() )  )
      }

      if ( period() %in% 'Week' ){
        .d = .d %>% filter(
          Week >=  yearweek( startingMonth() )   ,
          Week <= yearweek( endingMonth() )  )
      }

}

    cat( "\n - input$agg_level:", input$agg_level )

    sub_agg = sub_agg_level()
    cat( "\n- sub agg level" , sub_agg )

    .d = .d %>%
        filter(
          ! is_empty( !! rlang::sym( input$agg_level   ) ) ,
          ! is.na( !! rlang::sym( input$agg_level   ) ) ,
          # next line is good for level 0
          ! is_aggregated(  !! rlang::sym( input$agg_level   ) )
        )

    cat( '\n - !is_empty(sub_agg)' , sub_agg , !is_empty(sub_agg) )

    if ( !is_empty( sub_agg ) ){
      cat( '\n - filtering by sub_agg' )
      .d = .d %>% filter(
            is_aggregated( !! rlang::sym( sub_agg  ) )
      )
    }

       # preserve tsibble key and index,
       indexVar = index_var( .d )
       keyVars = key_vars( .d )

      .d = .d %>%
         mutate(
           grouping_var = 'Total' ) %>%
           # ensure tsibble before using fill_gaps
           as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
           fill_gaps( .full = TRUE  )


       cat( '\n - .d in trendData' ); # glimpse(.d)

       if ( num_datasets() > 1 ){
         .d = .d %>%
         filter( !is_aggregated( dataSet ) ) %>%
         mutate( dataSet = as.character( dataSet ) %>%
             str_remove_all( "<aggregated>" ) ,
             grouping_var = dataSet )

       }

       if ( num_facilities() > 1 ){
         .d = .d %>%
         filter( !is_aggregated( Selected )  ) %>%
         mutate( Selected = as.character( Selected ) %>%
             str_remove_all( "<aggregated>" )  )

         cat( '\n - Facilities:' ,  unique(.d$Selected) )
       }

      # if split, remove aggregate grouping
       if ( !split() %in% 'None' ){
         cat( '\n - input split:' , split() )
         .d = .d %>%
           filter( !is_aggregated( !! rlang::sym( split() ) )
           ) %>%
           mutate( grouping_var = as.character(
             !! rlang::sym( split() ) )
           )
         cat( '\n - .d  aggregated split' , unique(.d$grouping_var) )
         # print( glimpse( .d ))

       }

    cat( '\n - nrow(.d)' , nrow(.d))

      # if ( !split() %in% 'None' & !input$filter_data %in% 'All' ){
      #     print( 'filter_data is not null' )
      #     .d = .d %>%
      #       filter( .data[[ split() ]] %in% input$filter_data )
      # }

    if ( input$scale ) .d = .d %>%
        ungroup() %>%
        group_by( grouping_var ) %>%
        mutate(
          total = scale( total ) + 1
      )


    # ensure tsibble before using fill_gaps
    .d = .d %>% as_tsibble( key = all_of( keyVars ) , index = indexVar  )

      cat( '\n - end trend data():'); # print( glimpse( .d ) ); # print(.d)
      saveRDS( .d , 'trendData.rds' )

  return( .d )
})

# Plot ####
  plotTrends = reactive({

          # req( trendData() )
          # req( period() )
          # input$agg_level
          # req( split() )
          # req( input$evaluation_month )
          cat( '\n* evaluation_widget plotTrends():' )

          .limits =
          if ( input$scale ){
            c( NA , NA ) } else {
              c( 0 , NA )
          }

          data.text = paste( unique( plotData()$data ), collapse = " + " )
          
          cat( '\n - ploTrends trendData():');
          .d = trendData()
          cat( '\n - ploTrends .d:'); #glimpse(.d)

          # if ( !input$filter_display %in% 'All' ) .d = .d %>%
          #         filter( .data[[ split() ]] %in%
          #                   input$filter_display )

          tic()

          .period = period()

      ## Main plot ####
          g = .d %>%
          filter( !is.na( total ) ) %>%
          # autoplot( total ) +
          ggplot( aes( x = !! rlang::sym( .period ), y = total

                     , group =  grouping_var # as.character( !! rlang::sym( input$agg_level  ) )

                     , color =  grouping_var
                    ) )  +
          geom_line() +
          theme_minimal()

          # Testing
          # save(.d, file = 'plot-trend-test-data.rda')


          cat( '\n - basic plot done' ); toc()

          if ( !input$legend ) g = g +
            theme(legend.position = "none")

          if ( input$label ){
            g = g + geom_label_repel(
                       data = .d %>%
                         filter(
                           !! rlang::sym( .period ) == max( .d %>% pull( .period ) ,
                                                            na.rm = T )
                           ) ,
                       aes( label = grouping_var ,
                            group = grouping_var )
                       )
          }

          # Determine number of agg levels available
          # If only one, do not facet (causes error, perhaps because of autoplot?)

          num_agg_levels = count( .d %>% as_tibble ,
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
             forecast::autolayer( tsPreForecast()
                       # , level = c(80,90) # ci_levels()
                       , PI = TRUE
                       , color = 'black'
                       , linetype = 'dotted'  , size = 2
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
            forecast::autolayer( tsForecast()
                       , level = pi_levels()
                       , color = 'black'
                       , linetype = 'dashed', size = 1
                       ,  alpha = .5 ) +
            # geom_line( data = tsForecast() , aes( y = .mean )
            #   # ,   color = 'light blue'
            #   , alpha = .75
            #   , linetype = 'dotted'  , size = 2
            # ) +

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
        y <- nearPoints( trendData() , input$plot_hover)[input$var_y]
        req(nrow(y) != 0)
        y
  })


    # Return ####
  return( )
} )
}


