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

  tabPanel( "Model",  
    inputPanel(
      
      selectInput( ns( "model" ), label = "Time-series model:" , 
              choices = c('ETS' , 'ARIMA', 'BSTS' , 'Prophet', 
                          'TSLM', 'TSLM (trend)', 
                          'TSLM (trend+season)') , 
              selected = 'ETS'  ) ,

      textInput( ns( 'model.formula' ) , 'Model Formula' ,
          value =  'total ~ 1' ) ,

      textInput( ns( 'covariates' ), 'Model covariates' ,
          value =  NULL ) ,
      
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
    
    checkboxInput( ns( "forecast_ci" ) , label ='Confidence interval',
                   value = FALSE  ) 
    
          )
) ,
  
  tabPanel( 'Impact' ,
        sidebarLayout(
          sidebarPanel(
            
              selectInput( ns( "agg_level") , label = "Aggregate to" , 
                choices = NULL , 
                selected = 1  ) ,

              checkboxInput( ns( "facet_admin" ) , label ="Facet by admin",
                             value = FALSE  ) ,
              
              checkboxInput( ns( "facet_split" ) , label ="Facet by split",
                             value = FALSE  ) ,
              
              checkboxInput( ns( "selected" ) , label ='Selected facilities only',
                             value = TRUE  ) ,
              
              checkboxInput( ns( "legend" ) , label ='Show legend',
                             value = FALSE  ) ,
              
              checkboxInput( ns( "plotly" ) , label ='Plotly Chart',
                             value = FALSE  ) 
          ),
          
          mainPanel( 
            
             plotOutput( ns( "plotOutput" ) , hover = "plot_hover"  )
          )
        )    
        
        )

        ))

}
        
evaluation_widget_server <- function( id , 
                                     directory_widget_output = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ,
                                     reporting_widget_output = NULL ){
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
    data.total = reactive({ reporting_widget_output$data.total() })
    
    # see https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
    shift_legend3 <- function(p) {
          pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
            with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))

          if( length(pnls) == 0 ) return(p)

          lemon::reposition_legend( p, "center", panel=names(pnls) )
    }
    
    # Model ####
    
    observe({  updateSelectInput( session, 'agg_level' , 
                              choices = levelNames() , 
                              selected = levelNames()[1] ) # 12 months before latest date
  } )

    observe({  updateSelectInput( session, 'level2' ,
                                  choices = 
                                    dataset() %>% 
                                      pull( !! rlang::sym( levelNames()[2]  ) ) %>% 
                                      unique %>% str_sort(),
                                  selected = NULL 
                                  ) 
      } )
    
    observe({  updateSelectInput( session, 'level3' ,
                                  choices = 
                                    dataset() %>% 
                                      filter(
                                      !! rlang::sym( levelNames()[2] ) %in% input$level2 ) %>% 
                                      pull( !! rlang::sym( levelNames()[3]  ) ) %>% 
                                      unique %>% str_sort() ,
                                  selected = NULL 
                                  ) 
      } )
    
    observe({  updateSelectInput( session, 'level4' ,
                                  choices = 
                                    dataset() %>% 
                                      filter(
                                      !! rlang::sym( levelNames()[3] ) %in% input$level3 ) %>% 
                                      pull( !! rlang::sym( levelNames()[4]  ) ) %>% 
                                              unique %>% str_sort(),
                                  selected = NULL 
                                  ) 
      } )
    
    level5 = reactive({
        req( input$level4 )
        req( levelNames() )
        if( is.na( levelNames()[5] ) ) return( NA ) 
    
        dataset() %>% 
            filter(
                !! rlang::sym( levelNames()[4] ) %in% 
                           input$level4 ) %>% 
            pull( !! rlang::sym( levelNames()[5]  ) ) %>% 
            unique %>% str_sort()  
    })
    
    observe({  updateSelectInput( session, 'level5' ,
                                  choices = level5(),
              selected = NULL 
    )
                                  
  } )

# Impact ####
      observe({  
        updateSelectInput( session, 'var_y' ,
                choices =  names( trendData() ) 
                )
        } )
      
      observeEvent(  dates() , {  
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
          cat( '\n* levelNames():' )
          l = count( orgUnits() %>% as_tibble, level, levelName ) %>% 
            arrange( level ) %>% pull(levelName ) 
          l = l[ !is.na(l) ]
          cat( '\n- end levelNames():' )
          return(l)
  })
  
      levels = reactive({ 
          req( orgUnits() )
          cat( '\n* levels():' )
          levels = 
            count( orgUnits() %>% as_tibble, level, levelName ) %>% 
            arrange( level ) 
          cat( '\n- end levels():' )
          return( levels )
    })
    
      sub_agg_level = reactive({
          req( levels() )
          levels() %>%
             mutate( parent = dplyr::lag( levelName ) ) %>%
             filter( parent == input$agg_level ) %>%
             pull( levelName )
        })
        
      MAPE = reactive({
        req( tsPreForecast() ) 
        print('MAPE')
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
        print( "MAPE"); print( e$mape)
        return( scales::percent( e$mape )  )
        
      })
      
      key.mape = reactive({
        req( tsPreForecast() ) 
        req( trendData() ) 
        
        print('key.mape')
        
        predicted = tsPreForecast() %>% 
          rename( pred = .mean ) 
        
        actual =  trendData() %>% 
          rename( actual = total )
        
        keyvars = key_vars( actual )
        print('keyvars'); print( keyvars )
        
        truth = predicted %>% 
           inner_join( actual , by = c( period() , keyvars  ) )  
        
        print( 'truth'); #print( truth )
        
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
        
        print( "end key.mape"); #glimpse(e )
        return( e )
      })
      
      MPE = reactive({
        req( tsForecast() ) 
      
        print('MPE')
        
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
        
        print( "MPE"); print( e$mpe)
        return( scales::percent( e$mpe )  )
        
      })
      
      key.mpe = reactive({
        req( tsForecast() ) 
        req( trendData() ) 
        
        print('key.mpe')
        
        predicted = tsForecast() %>% 
          rename( pred = .mean ) 
        
        actual =  trendData() %>% 
          rename( actual = total )
        
        keyvars = key_vars( actual )
        print('keyvars'); print( keyvars )
        
        truth = predicted %>% 
           inner_join( actual , by = c( period(), keyvars  ) )  
        
        print( 'truth'); #print( truth )
        
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
        
        print( "mpe"); #glimpse(e )
        return( e )
      })
      
      ci_levels = reactive({
        if ( ! input$forecast_ci ) return( NULL )
        return( 90 )
      })

# Model forecasts ####
    
    evaluationParameters <- reactiveValues( Month  = NULL )
    
    model_formula = reactive({
      req( input$model.formula )
      print( 'model_formula' )
    
        
      # if (input$model %in% 'BSTS'){
      #   f = as.formula( 'total ~ intercept()' ) 
      #   
      # } 
      
      if (input$model %in% 'ARIMA' ){
        formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
                                ' pdq() ' ,  
                                '+ PDQ( period = 12 )' 
        )
         if ( nchar( input$covariates ) > 0 ) formula.string = 
             paste( formula.string , '+ xreg(' , input$covariates , ' ) ' )
         
         f = as.formula( formula.string)
      }
      
      if (input$model %in% 'BSTS' ){
         f = as.formula( paste( 'total ~ season("year")' 
                           )
         )
         }
                         
       if ( any(input$model %in% c( 'TSLM', 'ETS',  'Prophet' ) ) ){
          
        f = as.formula(  input$model.formula )
    
        }
    
      cat( '\n - end model_formula', f )
      return( f )
    })
    
    tsModel = reactive({
      req( trendData() )
      req( model_formula() )
      req( input$evaluation_month )
      
      if ( !input$evaluation ) return( NULL )
      print( 'tsModel():' )
      print( paste('available vars:', 
                   paste( names(trendData()), collapse = ',') 
                   )
      )
      
      # Filter data to period just before evaluation start
      print( input$evaluation_month )
      eval_month = input$evaluation_month
      time_period = yearmonth( eval_month  ) # - month(1)
      
      fit.data  = trendData() %>%
        filter_index( ~ as.character( time_period ) ,
                      .preserve = TRUE )
      
      if (input$model %in% 'TSLM' ){
        fit = fit.data %>% model( l = TSLM( model_formula() ) ) 
        print( 'end tsModel():' )
        return( fit )
        } 
      
      if (input$model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend()  ) )
        print( 'end tsModel():' )
        return( fit )
      } 
      
      if (input$model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend() + season() ) )
        print( 'end tsModel():' )
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
        print( 'end tsModel(): arima fit' )
        glimpse( fit )
        # testing model fit for forecasts
        if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )
        return( fit )
      } 
      
      if (input$model %in% 'BSTS' ){
        fit = fit.data %>% model( b = BSTS( model_formula() ) )
        print( 'end tsModel():' )
        return( fit )
      } 
      
      if (input$model %in% 'ETS' ){
        fit = fit.data %>% model( a = ETS( total )  ) 
        
        print('ETS model') ; #print( fit )
        
      # if ( input$reconcile ) fit = fit %>% 
      #       reconcile( 
      #         mint = min_trace(a, method = "mint_shrink") 
      #         )
        print( 'end tsModel():' )
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
                                        season(period=12, 
                                               order = 4 ,
                                               type='multiplicative'),
                                   seed = TRUE ,
                                   future.seed=TRUE )
            )
       
        print( 'end tsModel():' )
        return( fit )
      } 
      
    })
    
    tsPreModel = reactive({
      req( trendData() )
      req( input$evaluation_month )
    
      if ( !input$pre_evaluation ) return( NULL )
      print( 'tsPreModel():' )
    
      eval_month = input$evaluation_month 
      time_period = yearmonth( eval_month  ) - 12
      
      fit.data  = trendData() %>%
        filter_index( ~ as.character( time_period ) ,
                      .preserve = TRUE )
      
        if (input$model %in% 'TSLM' ){
        fit = fit.data %>% model( l = TSLM( model_formula() ) ) 
        print( 'end tsPreModel():' )
        return( fit )
        } 
      
      if (input$model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend()  ) )
        print( 'end tsPreModel():' )
        return( fit )
      } 
      
      if (input$model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( total ~ trend() + season() ) )
        print( 'end tsPreModel():' )
        return( fit )
      } 
      
      if (input$model %in% 'ARIMA' ){
        fit = fit.data %>% model( 
          arima = ARIMA( model_formula()  )
          )    
        print( 'end tsPreModel(): arima fit' )
        return( fit )
      } 
      
      if (input$model %in% 'BSTS' ){
        fit = fit.data %>% model( b = BSTS( model_formula() ) )
        print( 'end tsPreModel():' )
        return( fit )
      } 
      
      if (input$model %in% 'ETS' ){
        fit = fit.data %>% model( a = ETS( total )  ) 
        
        print('ETS model') ; #print( fit )
        
      # if ( input$reconcile ) fit = fit %>% 
      #       reconcile( 
      #         mint = min_trace(a, method = "mint_shrink") 
      #         )
        print( 'end tsPreModel():' )
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
                                        season(period=12, 
                                               order = 4 ,
                                               type='multiplicative'),
                                   seed = TRUE ,
                                   future.seed=TRUE )
            )
       
        print( 'end tsPreModel():' )
        return( fit )
      } 
      
    })
    
    tsForecast = reactive({ 
      
      req( tsModel() ) 
      req( input$horizon )
      print( 'tsForecast()' )
      
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
      
      fcast = fcast %>%
          mutate( !! input$agg_level := 
                    as.character( !! rlang::sym( input$agg_level  ) ) )
      
      print( 'fcast:' );  #glimpse( fcast )
      return( fcast )
      })
    
    tsPreForecast = reactive({ 
      
      req( tsPreModel() ) 
      req( input$horizon )
      
      fcast = tsPreModel() %>% forecast( h = 12 )
      
      fcast = fcast %>%
          mutate( !! input$agg_level := 
                    as.character( !! rlang::sym( input$agg_level  ) ) )
      
        
      # print( 'pre-fcast' ) ; print( fcast )
      return( fcast )
      })
    
# Trend data ####
    trendData = reactive({
      req( data.hts() )
      # req( aggregatePlotData() )
      cat( '\n* evaluation_widget: trendData(): ' )
      
      .d = data.hts()
      
      if ( input$selected  & num_facilities() > 1 ){ 
        
        cat( '\n- input$selected TRUE' )
      
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
            Week <= yearweek( endingMonth )  )
        }
  } 
    
      cat( "\n- input$agg_level:", input$agg_level )

      cat( "\n- sub_agg_level:" , sub_agg_level() )
       
      # .d = data.hts() %>% 
      #     filter( 
      #       ! is.na( !! rlang::sym( levelNames()[1] ) )
      #       , is_aggregated( !! rlang::sym( levelNames()[1] ) )
      #     ) 
      
      sub_agg = sub_agg_level() 
      cat( "\n- sub agg level filter" , sub_agg )
      
      .d = .d %>% 
          filter( 
            ! is.na( !! rlang::sym( input$agg_level   ) ) 
            # next line is good for level 0
            , ! is_aggregated(  !! rlang::sym( input$agg_level   ) )
          )
              
      cat( '\n- !is_empty(sub_agg)' , sub_agg )
      cat( '\n- ' , !is_empty(sub_agg)) 
      cat( '\n-',  class( sub_agg )) 
      if ( !is_empty(sub_agg) ){
        print( 'filtering by sub_agg' )
        .d = .d %>% filter( 
              is_aggregated( !! rlang::sym( sub_agg  ) )
        )
      }
        
        .d = .d %>%
           mutate( 
             grouping_var = 'Total' ) %>%
          fill_gaps( .full = TRUE  )
    
         
         cat( '\n- .d in trendData' ); # glimpse(.d)
         
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
    
           cat( '\n- Facilities:' ,  unique(.d$Selected) )
         }
            
        # if split, remove aggregate grouping
         if ( !split() %in% 'None' ){
           cat( '\n-input split:' , split() )
           .d = .d %>%
             filter( !is_aggregated( !! rlang::sym( split() ) ) 
             ) %>%
             mutate( grouping_var = as.character( 
               !! rlang::sym( split() ) )
             )
           cat( '\n- .d  aggregated split' , unique(.d$grouping_var) )
           # print( glimpse( .d ))
           
         } 
    
      cat( '\n- nrow(.d)' , nrow(.d))
         
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
      
      cat( '\n- end trend data():'); # print( glimpse( .d ) ); # print(.d)
      # saveRDS( .d , 'trendData.rds' )
  
  return( .d )
})
    
# Plot #### 
    plotTrends = reactive({
          
          req( trendData() )
          # req( split() )
          # req( input$evaluation_month )
          cat( '\n* plotTrends():' )
        
          .limits =
          if ( input$scale ){
            c( NA , NA ) } else {
              c( 0 , NA )
          }
         
          data.text = paste( unique( plotData()$data ), collapse = " + " )
          
          .d = trendData() 
          cat( '\n- ploTrends .d:'); #glimpse(.d)
          
          # if ( !input$filter_display %in% 'All' ) .d = .d %>% 
          #         filter( .data[[ split() ]] %in%
          #                   input$filter_display )
        
          tic() 
          g = .d %>%
          autoplot( total ) +
            # ggplot( aes(x = Month, y = total
            #          , group = grouping_var
            #          , color =  grouping_var
            #         ) )  +
            # geom_line() +
          theme_minimal() 
          save(.d, file = 'plot-trend-test-data.rda')
          # TESTING: 
          
          
          cat( '\n- basic plot done' ); toc()
          
          if ( !input$legend ) g = g + 
            theme(legend.position = "none")
          
          if ( !split() %in% 'None' ){ 
            g = g + geom_label_repel( 
                       data = .d %>% filter( Month == max( .d$Month , na.rm = T )) ,
                       aes( label = grouping_var , group = grouping_var )
                       )
          }
          
          # Determine number of agg levels available
          # If only one, do not facet (causes error, perhaps because of autoplot?)
          
          num_agg_levels = count( .d %>% as_tibble , 
                                  !! rlang::sym( input$agg_level ) ) %>%
            nrow()
        
          # if ( input$agg_level != levelNames()[1] & input$facet_admin ){
          if ( num_agg_levels  > 1 & input$facet_admin ){
            cat( '\n- facets' )
            g = g +
            facet_wrap( vars(!! rlang::sym( input$agg_level ) ) ,
                           scales = "free_y" )
          }
          
          if ( input$facet_split ){
            cat( '\n- facet_split' )
            g = g +
            facet_wrap( ~grouping_var   ,
                           scales = "free_y" )
          }
          
          g = g +
        
            scale_x_yearmonth("", date_breaks="1 year" ) +
            scale_y_continuous( label=comma, limits = .limits ) +
            scale_color_discrete( drop = TRUE  ) +
            labs( y = "" , x="" ,
                  title = str_wrap( input$indicator , 200 ) ,
                  subtitle = str_wrap( data.text , 200 ) 
                  , caption =  str_wrap( caption.text() , 200 )
                  ) 
          cat( '\n- axis scales and labs done' )
          
          #### Evaluation Trend Line
          if ( input$evaluation ){
            cat( '\n- evaluation line ' )
            eval_date =   yearmonth( input$evaluation_month  ) 
            
           g = g + 
            autolayer( tsForecast()
                       , level = ci_levels()
                       , color = 'black'
                       , linetype = 'dashed', size = 1
                       ,  alpha = .5 ) +
             
            # geom_line( data = tsForecast() %>%
            #   as_tibble %>%
            #   mutate( !! input$agg_level := as.character( !! rlang::sym( input$agg_level  ) ) ) ,
            #            aes( x = Month, y = .mean ) ,
            #            color = 'blue' , alpha = .5 ) 
            geom_vline( xintercept = as.Date( eval_date ) ,
                        color = 'black', alpha = 1 ) +
            # annotate( "text" ,
            #           x = as.Date( eval_date ) ,
            #           y = Inf ,
            #           hjust = 0 , vjust = 1 ,
            #           label = paste( "MPE:\n" )
            #           ) +
            geom_label_repel( data =  key.mpe() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual , 
                       label = paste( "MPE:" , percent( mpe, accuracy = 1.0 ) ) ,
                       hjust = just 
                       ) ,
                       # force_pull = 0 , 
                       segment.colour = NA
                       )
          }
          
          cat( '\n- evaluation line done' )
        
          ### Pre-Evaluation trend line
          if ( input$pre_evaluation ){
            
            cat( '\n- pre evaluation line ' )
            pre_eval_date = yearmonth( input$evaluation_month  ) - 12
                # month( as.integer( input$horizon ) )
              
            cat( '\n- pre_eval_date is' ); print( pre_eval_date )
            g = g + 
            autolayer( tsPreForecast()
                       , level = ci_levels()
                       , color = 'black' 
                       , linetype = 'dotted'  , size = 1
                       ,  alpha = .5 ) +
            # geom_line( data = tsPreForecast(), aes( y = .mean ) ,   color = 'light blue', alpha = 1 ) +
            geom_vline( xintercept = as.Date( pre_eval_date ) ,
                        color = 'brown' ,
                        alpha = .25 ) +
        
            geom_label_repel( data =  key.mape() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual , 
                       label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
                       hjust = just ) ,
                       # force_pull = 0 , 
                       segment.colour = NA 
                       )
        
          }
          
          cat( '\n- pre-evaluation line done' )
           
          if (input$smooth){
            cat( '\n- agg level', input$agg_level )
            .d. = .d %>% 
              as_tibble %>%
              mutate( !! input$agg_level := as.character( !! rlang::sym( input$agg_level  ) ) )
            
            cat( '\n- smooth .d.') ; #glimpse(.d. )
            g = g + 
            geom_smooth( data = .d. , 
                         alpha = .75 )
          
          } 
          
         
          cat( '\n- end plotTrends():' )
          
          saveRDS( g, 'plotTrends.rds')
          return( g )
        })
    
    plotComponents = reactive({
  
      req( tsModel() )
      req( input$evaluation_month )
      cat( '\n* plotComponenets():' )
    
      g = tsModel() %>% fabletools::components() %>% autoplot
      
      cat( '\n- end plotComponents():' )
      
      return( g )
})
    
    plotOutput = reactive({
        # req( input$components )
        cat('\n* plotTrendOutput')
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


