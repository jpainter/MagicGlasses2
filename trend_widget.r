

# Trend / Evaluation 
# ====================================

  checkboxInput( "smooth" , label ='Show smoothed trend line (loess)',
               value = FALSE  ) 

checkboxInput( "pre_evaluation" , label ='Pre-intervention model fit',
               value = FALSE  ) 


checkboxInput( "evaluation" , label ='Post-intervention evaluation',
               value = FALSE  ) 

checkboxInput( "scale" , label ='Scale values (x-mean)/sd + 1)',
               value = FALSE  )

checkboxInput('components', label = 'Visualize trend' ,
            value = FALSE )

checkboxInput( "forecast_ci" , label ='Confidence interval',
               value = FALSE  ) 

### Administrative Hierarchy


selectInput("agg_level", label = "Aggregate to" , 
              choices = NULL , 
              selected = 1  ) 


checkboxInput( "facet_admin" , label ="Facet by admin",
               value = FALSE  )

checkboxInput( "facet_split" , label ="Facet by split",
               value = FALSE  )

checkboxInput( "selected" , label ='Selected facilities only',
               value = TRUE  ) 


checkboxInput( "legend" , label ='Show legend',
               value = FALSE  ) 

checkboxInput( "plotly" , label ='Plotly Chart',
               value = FALSE  ) 


### Model Details

selectInput("model", label = "Time-series model:" , 
              choices = c('ETS' , 'ARIMA', 'BSTS' , 'Prophet', 
                          'TSLM', 'TSLM (trend)', 
                          'TSLM (trend+season)') , 
              selected = 'ETS'  ) 


textInput('model.formula', 'Model Formula' ,
          value =  'total ~ 1' )

textInput('covariates', 'Model covariates' ,
          value =  NULL )

plotOutput( "plot_trends" ,
                             hover = "plot_hover"  )

plotOutput( "plot_significance" )





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

admin_names = function( level = NULL ){
  admin_names = ous() %>% 
    as_tibble %>%
    filter( levelName %in% level ) %>%
    distinct( name , orgUnit ) %>%
    arrange( name )
  print( 'sub_admins'); print( admin_names )
  return( admin_names )
}

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
  input$split  , 
  
  { 
  if ( !input$split %in% 'None' ){
    
    print( "input$split:" ); print( input$split )
    # print( "data.total():" ); # glimpse( data.total() )
    
    # splits = data.total() %>% pull( .data[[ input$split ]] ) %>% unique
    
    splits = data.total()[, input$split] %>% unique
    
    print( paste( 'splits: ', splits  ) )

    updateSelectInput( session, 'filter_data' , choices =  c( 'All', splits ) )
    
    updateSelectInput( session, 'filter_display' , choices =  c( 'All', splits ) )
  
  } else {
    
    updateSelectInput( session, 'filter_data' , choices =  c( 'All' ) )
    updateSelectInput( session, 'filter_display' , choices =  c( 'All' ) )
  }
} )

backtick <- function(x) paste0("`", x, "`")

sub_agg_level = reactive({
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

trendData = reactive({
  req( data.hts() )
  # req( aggregatePlotData() )
  print( 'trendData(): ' )
  
  .d = data.hts()
  
  if (input$selected & num_facilities() > 1 ){
    .d = .d %>% filter( 
      Selected ==  'Reporting Each Period' )
    
    if ( period() %in% 'Month' ){ 
      .d = .d %>% filter( 
        Month >=  yearmonth( input$startingMonth )   ,
        Month <= yearmonth( input$endingMonth )  )
    }
      
    if ( period() %in% 'Week' ){ 
      .d = .d %>% filter( 
        Week >=  yearweek( input$startingMonth )   ,
        Week <= yearweek( input$endingMonth )  )
    }
  } 
    
  print( "input$agg_level:" ); print( input$agg_level )


  print( "sub_agg_level:" ); print( sub_agg_level() )
   
  # .d = data.hts() %>% 
  #     filter( 
  #       ! is.na( !! rlang::sym( levelNames()[1] ) )
  #       , is_aggregated( !! rlang::sym( levelNames()[1] ) )
  #     ) 
  
  sub_agg = sub_agg_level() 
  print( 'sub agg level filter' ) ; print( sub_agg )
  
  .d = .d %>% 
      filter( 
        ! is.na( !! rlang::sym( input$agg_level   ) ) 
        # next line is good for level 0
        , ! is_aggregated(  !! rlang::sym( input$agg_level   ) )
      )
  
  print( '!is_empty(sub_agg)' ); 
  print( sub_agg )
  print(!is_empty(sub_agg)); 
  print( class( sub_agg ));
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

     
     print( '.d in trendData' ); # glimpse(.d)
     
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

       print( 'Facilities:') ; print( unique(.d$Selected))
     }
        
    # if split, remove aggregate grouping
     if ( !input$split %in% 'None' ){
       print( 'input split:') ; print( input$split )
       .d = .d %>%
         filter( !is_aggregated( !! rlang::sym( input$split ) ) 
         ) %>%
         mutate( grouping_var = as.character( 
           !! rlang::sym( input$split ) )
         )
       print( '.d  aggregated split'); 
       print( unique(.d$grouping_var) )
       # print( glimpse( .d ))
       
     } 

  print( 'nrow(.d)'); print(nrow(.d))
     
    # if ( !input$split %in% 'None' & !input$filter_data %in% 'All' ){
    #     print( 'filter_data is not null' )
    #     .d = .d %>% 
    #       filter( .data[[ input$split ]] %in% input$filter_data )
    # }
  
  if ( input$scale ) .d = .d %>%
      ungroup() %>%
      group_by( grouping_var ) %>%
      mutate(
        total = scale( total ) + 1
    )
  
  print( 'end trend data():'); # print( glimpse( .d ) ); # print(.d)

  
  return( .d )
})

#  models_forecasts}
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

  print( 'end model_formula' ); print( f )
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

plotTrends = reactive({
  
  req( trendData() )
  req( input$split )
  # req( input$evaluation_month )
  print( 'plotTrends():' )

  .limits =
  if ( input$scale ){
    c( NA , NA ) } else {
      c( 0 , NA )
  }
 
  data.text = paste( unique( plotData()$data ), collapse = " + " )
  
  .d = trendData() 
  print( 'ploTrends .d:'); #glimpse(.d)
  
  # if ( !input$filter_display %in% 'All' ) .d = .d %>% 
  #         filter( .data[[ input$split ]] %in%
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
  
  
  print( 'basic plot done' ); toc()
  
  if ( !input$legend ) g = g + 
    theme(legend.position = "none")
  
  if ( !input$split %in% 'None' ){ 
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
    print( 'facets' )
    g = g +
    facet_wrap( vars(!! rlang::sym( input$agg_level ) ) ,
                   scales = "free_y" )
  }
  
  if ( input$facet_split ){
    print( 'facets' )
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
  print( 'axis scales and labs done' )
  
  #### Evaluation Trend Line
  if ( input$evaluation ){
    print( 'evaluation line ' )
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
  
  print( 'evaluation line done' )

  ### Pre-Evaluation trend line
  if ( input$pre_evaluation ){
    
    print( 'pre evaluation line ' )
    pre_eval_date = yearmonth( input$evaluation_month  ) - 12
        # month( as.integer( input$horizon ) )
      
    print( 'pre_eval_date is' ); print( pre_eval_date )
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
    # annotate( "text" ,
    #           x =  as.Date( pre_eval_date ) ,
    #           y = Inf ,
    #           hjust = 0 , vjust = 1 ,
    #           label = paste( "MAPE:"  )
    #           ) +
    geom_label_repel( data =  key.mape() ,
               aes(  x = !! rlang::sym( period() ) , y = actual , 
               label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
               hjust = just ) ,
               # force_pull = 0 , 
               segment.colour = NA 
               )

  }
  
  print( 'pre-evaluation line done' )
   
  if (input$smooth){
    print( 'agg level' ); print( input$agg_level )
    .d. = .d %>% 
      as_tibble %>%
      mutate( !! input$agg_level := as.character( !! rlang::sym( input$agg_level  ) ) )
    
    print( 'smooth .d.') ; #glimpse(.d. )
    g = g + 
    geom_smooth( data = .d. , 
                 alpha = .75 )
  
  } 
  
  # Confidence Interval
  # if ( input$forecast_ci & input$evaluation ){
  #   print( 'confidence intervals line ' )
  #   fbl = tsForecast() 
  #   print( 'glimpse fbl') ; # glimpse( fbl )
  #   ci = fbl %>% 
  #     mutate( hilo = hilo( total , 80 ) )
  #   ci$.lower = ci$hilo$lower 
  #   ci$.upper = ci$hilo$upper 
  #   print( 'glimpse ci') ; # glimpse( ci )
  #   
  #   print( 'summary lower') ; print( summary( ci$.lower ) )
  #   print( 'summary upper') ; print( summary( ci$.upper ) )
  #   
  #   g = g + 
  #     geom_ribbon(  data = ci , 
  #       color = 'blue' , alpha = .5 , 
  #       # fill = 'blue' , 
  #       inherit.aes = FALSE ,
  #       aes( x = Month, ymin = .lower, ymax = .upper )
  #            # ymin = .mean, 
  #            # ymax = .mean 
  #            # )
  #   )
  #   return( g )
  # } 
  #   
  print( 'end plotTrends():' )
  
  return( g )
})

plotComponenets = reactive({
  
  req( tsModel() )
  req( input$evaluation_month )
  print( 'plotComponenets():' )

  g = tsModel() %>% fabletools::components() %>% autoplot
  
  print( 'end plotComponents():' )
  
  return( g )
})

plotTrendOutput = reactive({
    if ( input$components ){
        plotComponenets()  
    } else {
        plotTrends()  
    }
})

output$plot_trends <-  renderPlot({ plotTrendOutput()  })
        
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

# significance_functions}

forecast_distribution = function( forecast, var ){
    if ( ! "fbl_ts" %in% class( forecast ) ) cat("missing fbl_ts")
    
    # get components 
    var = enquo( var )
    r = forecast %>% pull( {{var}} )
    dist <- map( r, "dist")
    trans <- map( r, "transform")

    # generate 
    gt = map( 1:length(r), ~{
    t = trans[[.x]]
    g = generate( dist[[.x]] ) %>% t 
    return(g)
    }  ) %>% unlist

    # differnece between forecast and actual
    t = map( forecast$value, "transform")[[1]]
    rd = forecast %>% 
        mutate(
        dist = map( {{var}}, "dist") ,
        # trans = map( value, "transform") ,
        .fore = map( dist, ~generate(.x) %>% t ) ,
) 
}

# Difference over each iteration, which is naturally wieghted
mpe_distribution = function( forecast_distribution , var ){
    
    var = enquo( var )
    
    index = tsibble::index( forecast_distribution )
    
    forecast_distribution %>%
    unnest( .fore ) %>% 
    group_by( {{ index }} ) %>%
    mutate( n = row_number() ,
            .fore = ifelse( is.na( {{var}}), NA, .fore ) 
            ) %>%
    group_by( n ) %>%
    summarise( 
        actual = sum( {{var}}, na.rm = T ) ,
        .fore = sum( .fore , na.rm = T) 
        ) %>%
    ungroup() %>%
    mutate( mpe = ( actual- .fore )/ .fore ,
            mape = abs( actual- .fore )/ .fore ) 
}

# function to get probability than impact, e, greater than a value
prb_change_lte = function( target_mpe , pe ){
   pe = enquo( pe )
   
   mpe %>%
     summarise(
       n = sum( !is.na( {{pe}} ) ) ,
       pe_lte_target = sum( {{pe}} <= target_mpe ) 
     ) %>%
     mutate( x = pe_lte_target / n ) %>%
     pull( x )
   
}

# plot error
mpe_plot = function( .data , var, rope_range  = NULL   ){
   var = enquo( var )
   
   me = .data %>% pull({{ var }})
   
   mean_me = mean( me , na.rm = T )
   
   cri = HDInterval::hdi( me , credMass = 0.9 ) %>% as.numeric()
   cri_label = paste( '90% credible interval:\n[', 
                      percent(cri[1], accuracy  = .1 ) , " - ", 
                      percent(cri[2] , accuracy  = .1 ) , "]" , sep = "" )
   
   mpe_rope = rope( me , range =  rope_range )
   mpe_rope_label = paste( 'Probabality in ROPE*\n [', 
                           percent( rope_range[1], accuracy = .1 ) , "-" , 
                           percent( rope_range[2] , accuracy = .1) , ']:', 
                           percent( mpe_rope$ROPE_Percentage , accuracy = .1 ) )
   
   # Title text...
   # if ( "mpe" %in%  var )
  
  g = 
    ggplot( data = .data , aes( x = {{ var }} )) +
    geom_histogram( aes(  y = ..count../sum(..count..) ) ,
                    fill = 'dark gray', color = 'gray', binwidth = .01 ) +
    theme_minimal() +
    scale_x_continuous( labels = percent_format(accuracy = 1) , 
                        breaks = seq(-1, 1, .1) ) +
    labs( title = 'Posterior Distribution of Error' ,
          # subtitle = {{ var }} ,
          y = 'Probability' 
          # x = {{ var }} ,
          # , caption = "*Region of Practical Equivalence"
          )

  # add mean
  g = g + 
    # geom_vline( aes( xintercept = mean(e) ) , linetype = 'dashed' ) +
    annotate( "segment" , x =  mean_me , xend =  mean_me , y = 0 , yend = .1 , 
              color = 'black' , linetype = 'dashed')  + 
    geom_text( aes( x = mean_me ,
                    y = .1  , 
                    label = paste('Mean:' , percent( mean_me , accuracy  = .1 )  ) ) )  +
    
    # cri
    annotate( "segment" , x = cri[1] , xend = cri[1] , y = 0 , yend = .5 , color = 'blue' )  + 
    annotate( "segment" , x = cri[2] , xend = cri[2] , y = 0 , yend = .5 , color = 'blue' )  +
    annotate( "text" , x = cri[1] , y = .4 , color = 'blue', label = cri_label , hjust = - 0.1 ) 
    
    # rope
  if ( !is.null( rope_range ) ) g = g +
    annotate( "segment" , 
              x = mpe_rope$ROPE_low , 
              xend = mpe_rope$ROPE_low , y = 0 , yend = .5 , color = 'brown' )  + 
    annotate( "segment" , 
              x = mpe_rope$ROPE_high , 
              xend = mpe_rope$ROPE_high , y = 0 , yend = .5 , color = 'brown' )  +
    annotate( "text" , x =  0  , y = .5 , color = 'brown', 
              label = mpe_rope_label , hjust = .5 , vjust = 1 )
  
  return( g )
  }

sigData = reactive({
  
  req( trendData() )
  req( tsForecast() )
  print( 'sigData():' )
  
  # print( names( trendData() ) ) ; 
  tsf = tsForecast()
  td = trendData()
  
  # testing: 
  # save(tsf, td,  file = 'tsftd.rda' )
  
  # glimpse( tsf ) 
  
  # fd = forecast_distribution( tsf , total )
  # glimpse( fd ) 
  
  cat( 'create forecast_distribution():')
  
    if ( ! "fbl_ts" %in% class( tsf ) ) cat("not a fbl_ts")
    
    # get components 
    r = tsf %>% pull( total )
    
    r.names = map( r, names ) %>% unlist %>% unique
    print( 'forecast_distribution() r.names:' ) ; print( r.names )
      
    if ( 'x' %in% r.names ){
        
       cat( 'forecast_distribution(): x')
      
        # dist <- map( r, "x")
        trans <- map( r, "transform") 

        # differnece between forecast and actual
        fd = tsf %>% 
          mutate(
            dist <- map( total , "x" ) 
            # trans = map( value, "transform") ,
            , .fore = map( dist, ~.x  )
          ) 
        
        print( 'end forecast_distribution(): x')

      }
      
    if ( 'dist' %in% r.names ){
      
      cat( 'forecast_distribution(): dist')
        
        dist <- map( r, "dist")
        trans <- map( r, "transform")
        # generate 
        gt = map( 1:length(r), ~{
          t = trans[[.x]]
          g = generate( dist[[.x]] ) %>% t 
          return(g)
        }  ) %>% unlist
        
        # differnece between forecast and actual
        t = map( forecast$value, "transform")[[1]]
        fd = forecast %>% 
          mutate(
            dist = map( {{var}}, "dist") ,
            # trans = map( value, "transform") ,
            .fore = map( dist, ~generate(.x) %>% t ) ,
          ) 
        
        cat( 'end forecast_distribution(): dist')

      }
  
  print( 'adding actual values' )
  fd$actual = trendData() %>% 
    rename( actual = total ) %>%
    semi_join( fd %>% as_tibble() ) %>% pull( actual )
  
  print( 'forecast distribution'); # glimpse( fd )
  
  print( 'end sigData():' )
  return( fd )
  
})


plotSig = reactive({
  
  print( 'plotSig():')
  
  req( sigData() )
  
  fd = sigData()
  
  cat('plotSig fd:' )
  
  mpe = mpe_distribution( fd , actual )

  cat('plotSig mpe:' )
  
  g =  mpe_plot( mpe , mpe )
  
  
  print( 'end plotSig():')
  return(g)
   
} )
  
 
output$plot_significance <- renderPlot({ plotSig()  }) 


