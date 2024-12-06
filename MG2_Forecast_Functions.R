

# Forecast functions ####

## Combination forecast function ####
combination_forecasts = function( primary.forecasts , only.models = NULL ){
  
  split_f <- primary.forecasts %>%
    group_by( .model ) %>%
    group_split()
  
  # Assign names to the list elements for easier access
  model.names <- unique( primary.forecasts$.model )
  names( split_f ) <- model.names
  
  # if not specifying a specific model, include all 
  # if ( is.null( only.models ) ){ 
  allf = primary.forecasts 
  # } else { 
  # allf = NULL }
  
  # if only.models one of the primary forecasts, select that one and quit
  if ( !is.null( only.models ) && all( only.models$.model %in% model.names ) ){
    allf = primary.forecasts %>% inner_join( only.models , by = join_by( Intervention, .model ))
    return( allf )
  }
  
  if ( is.null( only.models )){
    all_combinations <- unlist( lapply( seq_along( model.names ), function(x) {
      combn( model.names, x, paste, collapse = "")
    }))
    combo.model.names = setdiff( all_combinations  , model.names )
  } else { 
    all_combinations = only.models$.model %>% unique
    combo.model.names = setdiff( all_combinations  , model.names )
  }
  
  # list model names
  if ( length( combo.model.names ) == 0 ) combo.model.names = only.models$.model
  
  
  # Combine the forecasts
  for ( x in seq_along( combo.model.names ) ){
    
    # number of individual models in the combo model
    nmodels = nchar( combo.model.names[x] )
    
    # individual model names that will make up the combo model
    # indiv.model.names = unlist(strsplit( combo.model.names[x], split = "") )
    
    # Function to split the combination
    split_combination <- function(combination, original_values) {
      pattern <- paste( original_values, collapse = "|")
      strsplit( combination, split = paste0("(?<=", pattern, ")"), perl = TRUE)[[1]]
    }
    
    # Apply the function
    indiv.model.names <- split_combination(combo.model.names[x], model.names )
    
    
    # Inner join all columns
    join_vars = setdiff( c( index_var( primary.forecasts ), 
                            key_vars( primary.forecasts ) ) , ".model" )
    joined_fables <- reduce( split_f[ indiv.model.names ]  , 
                             left_join, by =join_vars )
    
    if ( ! 'older.x' %in% names( joined_fables ) ) {
      joined_fables$older.x = 0 
    }
    
    #all column names
    cols = colnames( joined_fables )
    
    #column names that begin with 'sample'
    sample.cols = cols[ grep( "sample" , cols ) ]
    
    cf <- joined_fables %>%
      mutate( 
        samples = pmap( 
          across(starts_with("sample")) , 
          ~ reduce(list(...), `+`) / nmodels
        ) ,
        older = older.x
      ) %>%
      mutate( .model = combo.model.names[x] ) %>%
      group_by( .model ) %>%
      mutate( .mean = map_dbl( samples, mean ) ,
              .sd = map_dbl( samples, sd ) ,
              var = dist_normal(mean = .mean, sd = .sd) ) %>%
      select( key_vars( primary.forecasts ), 
              index_var( primary.forecasts ) , var,  .mean, older , samples )
    
    dimnames( cf$var ) = 'var'
    
    if ( is.null( allf ) ){ 
      allf = cf }  else {
        allf = bind_rows( allf , cf ) 
      }
    
    
    
  }
  
  # filter to selected models
  if ( !is.null( only.models )){ 
    allf = allf %>% inner_join( only.models , 
                                by = join_by(Intervention, .model ) ) 
  }  
  
  return( allf )
}


best_fables_accuracy = function( fables_accuracy , metric = "MAPE" , top = 1000 ,
                                 grouping = FALSE , groups = 'Intervention' ){
  
  .metric = rlang::sym( metric )
  
  if ( grouping ){
    selectVars = c( groups, ".model" )
    groupByVars = groups 
  } else { 
    selectVars =  ".model"
    groupByVars = NULL
    }
  
  mean_model_metric = fables_accuracy %>%
    ungroup() %>% 
    group_by( .model ) %>%
    summarise( mean = mean( {{ .metric }} ))
  
  best_fables_accuracy = fables_accuracy %>%
    { if (!is.null(groupByVars)) group_by(., !!sym(groupByVars)) else . } %>%
    arrange( all_of( groupByVars ) , {{ .metric }} ) %>%
    mutate( 
      across( where(is.numeric), \(x) round(x, digits = 3) ),
      `Intervention Rank` = row_number() ,
      percent_diff = 100 * ( ( {{ .metric }} - first( {{ .metric }} ) ) / first( {{ .metric }} ) ) %>% 
        round( digits = 3 ) 
    )     %>%     
    filter(  `Intervention Rank` <= top ) %>%
    left_join( mean_model_metric , by = '.model')
  
  return(  best_fables_accuracy )
}

swape <- function(actual, forecast) {
  # Calculate SWAPE
  swape <- sum(2 * (forecast - actual) / sum(forecast + actual) )
  return(swape)
}

swape_pv = function( accuracy , .Intervention ,  .model1 , .model2 ){
  
  swape1 = accuracy %>% 
    filter( Intervention == .Intervention ,  .model == .model1 ) %>% 
    pull( swape )
  
  swape2 = accuracy %>% 
    filter( Intervention == .Intervention ,  .model == .model2 ) %>% 
    pull( swape ) 
  
  swape <- sum(2 * (forecast - actual) / sum(forecast + actual) )
  return(swape)
}

swape_sd = function( accuracy , .Intervention ,  .model1  ){
  
  swape1 = accuracy %>% 
    filter( Intervention == .Intervention ,  .model == .model1 ) %>% 
    pull( swape )
  
  return( sd( swape1) )
}

# swape_sd( pre.intervention.train.combo.accuracyOOS ,"Comparator" , "p1"  )

mean_swape = function( accuracy ,  .model1  ){
  
  accuracy %>%
    group_by( .model ) %>%
    summarise( mean = mean( swape ) 
    ) %>%
    arrange( mean ) %>%
    mutate( rank = row_number() )%>% 
    filter( .model == .model1 ) %>%
    pull( mean )
  
}


ranks = function( acccuracy ){
  best_fables_accuracy(
    acccuracy , metric = 'swape', top = 100 ) %>%
    group_by( Intervention ) %>%
    arrange( swape ) %>%
    mutate( rank = row_number())
}

mean_rank = function( accuracy ,  .model1  ){
  
  ranks( accuracy ) %>%
    group_by( .model ) %>%
    summarise( mean = mean( rank ) 
    ) %>%
    arrange( mean ) %>%
    mutate( rank = row_number() )%>% 
    filter( .model == .model1 ) %>%
    pull( rank )
  
}


differences = function( data , grouping = FALSE , groups = c("Comparator","Vaccinating") ){
  
  comparator = data %>% 
    filter( Intervention %in% groups[1] )  %>% pull( WPE )
  
  vaccinating = data %>% 
    filter( Intervention %in% groups[2])  %>% pull( WPE )
  
  difference = vaccinating - comparator
  
  return( difference )
  
}

differences_probabilty = function( differences , thresholds = c(0, -10, -20 ) ){
  
  # empirical CDF for probabilities < x
  probability = ecdf( differences )( thresholds )
  
  tibble( Threshold = thresholds , 
          `Probability less than` = probability )
  
}



## Facility characteristics functions ####

facility_characteristics = function( dataset, 
                                     vx = tibble( Month = NA, orgUnit = "NA", original = NA ) , 
                                     .selected =  c( TRUE, FALSE ), 
                                     errs = FALSE , 
                                     selected_facilities = consistently_reporting_facilities ){
  
  i = unique( dataset$Intervention )
  
  if ( any(.selected == FALSE ) ){
    data = dataset %>% filter( year( Month ) < 2023 ) %>%
      mutate( selected = orgUnit %in% selected_facilities )
  } else {
    data = dataset %>% filter( year( Month ) < 2023 ) %>%
      mutate( selected = TRUE )
  }
  
  combinations <- expand.grid( i  , .selected )
  
  if ( errs ){ 
    errors = map2_df( combinations$Var1, combinations$Var2,  
                      ~bind_cols( Intervention = .x , 
                                  selected = .y ,
                                  outlier.summary.tibble( 
                                    data = data %>% 
                                      filter( Intervention == .x , 
                                              selected == .y ) , 
                                    .print = FALSE ) %>% 
                                    filter( `Error Flag` %in% 'No Error Flags' ) 
                      ) 
    )
  }
  
  
  vx.summary = vx %>% as_tibble %>%
    filter( year( Month ) < 2023 ) %>%
    group_by( orgUnit ) %>%
    summarise( total.doses = sum( original , na.rm = TRUE )) 
  
  evaluation.period.cases = mcc0 %>% as_tibble() %>%
    filter( year( Month ) %in% 2020:2022, agegrp %in% "Under5" ) %>%
    group_by( orgUnit ) %>%
    summarise( under5 = sum( original, na.rm = TRUE ) )
  
  all.cases = mcc0 %>% as_tibble() %>%
    filter( year( Month ) %in% 2015:2022, agegrp %in% "Under5" ) %>%
    group_by( orgUnit ) %>%
    summarise( under5 = sum( original, na.rm = TRUE ) )
  
  
  d = mvip.facilities %>%
    st_drop_geometry() %>%
    mutate( selected = orgUnit %in% unique( dataset$orgUnit ) ,
            reporting = orgUnit %in% unique( mcc3$orgUnit ) 
    ) %>%
    # subset to facilities that report cases
    filter( reporting ) %>%
    left_join( all.cases, by = 'orgUnit' ) %>%
    left_join( vx.summary , by = 'orgUnit' ) %>% 
    group_by(  selected , Intervention ) %>%
    summarise( Facilities = n_distinct( orgUnit ) ,
               Under5 = sum( under5 , na.rm = TRUE ) %>% round(0)  ,
               Under5.p = paste0("(", percent( Under5 / sum( .$under5, na.rm = TRUE ) ) , ")" ), 
               vx.doses = sum( total.doses, na.rm = TRUE ) ,
               vx.doses.p =  paste0("(", percent( vx.doses / sum( .$total.doses, na.rm = TRUE ) ) , ")" ),
               PBO = sum( Vector_2021 %in% 'PBO', na.rm = TRUE )  ,
               percPBO = percent( PBO / Facilities  )
    ) 
  
  if ( errs ){ 
    d = d %>%
      left_join( errors %>% select( Intervention, selected, `%N`, `%Total Value` ), 
                 by = c( 'Intervention' , 'selected' )) }
  
  return(d)
  
}

ft_characteristics = function( d , errs = FALSE ){
  
  if ( errs ){
    ft = d %>% 
      select( - PBO ) %>%
      mutate(
        across( where(is.numeric), \(x) round(x, 1)) ,
        `Under5 Malaria` = paste( comma( Under5 ) , Under5.p ) ,
        `RTS,S Doses` = paste( comma( vx.doses ), vx.doses.p ) ,
        `%PBO-nets` = percPBO
      ) %>% 
      select( selected, Intervention, Facilities, `Under5 Malaria` , `RTS,S Doses`, `%PBO-nets`,
              `%N`, `%Total Value` ) %>%
      flextable
  } else {
    ft = d %>% 
      select( - PBO ) %>%
      mutate(
        across( where(is.numeric), \(x) round(x, 1)) ,
        `Under5 Malaria` = paste( comma( Under5 ) , Under5.p ) ,
        `RTS,S Doses` = paste( comma( vx.doses ), vx.doses.p ) ,
        `%PBO-nets` = percPBO
      ) %>% 
      select( selected, Intervention, Facilities, `Under5 Malaria` , 
              `RTS,S Doses`, `%PBO-nets` ) %>%
      flextable
    
  }
  
  
  # Add the header row spanning columns 4 and 5 as "Evaluation Period" and columns 8 and 9 as "Outliers"
  if (errs){
    ft <- add_header_row(
      ft,
      values = c("", "", "", "", "", "", "Pass Outlier Scan"),
      colwidths = c(1, 1, 1, 1, 1 , 1,  2)
    )
    
    # Define the border line style
    border_line <- officer::fp_border(color = "black", width = 2)
    
    # Apply the border line only under columns 4 and 5
    ft <- border(ft, part = "header", border.bottom = border_line, i = 1, j = 4:5)
    ft <- border(ft, part = "header", border.bottom = border_line, i = 1, j = 7:8)
  }
  
  
  
  # Display the flextable
  ft <- theme_vanilla(ft)
  
  ft %>% FitFlextableToPage()
  
}

facilities.reporting = function( .dataset ){
  
  reporting = 
    bind_rows( 
      .dataset %>% filter( Intervention %in% 'Vaccinating' ) %>%
        dqaNumberReporting() %>%
        mutate( Intervention = 'Vaccinating' ) ,
      .dataset %>% filter( Intervention %in% 'Comparator' ) %>% 
        dqaNumberReporting() %>%
        mutate( Intervention= 'Comparator' )
    ) 
  
  reporting %>%
    ggplot( aes( x = as.character( Year ) , 
                 y = mean_monthly, group = 1 , 
                 label = round( mean_monthly ) 
    ) )  + 
    geom_line(  linewidth = 1.25 ) +
    geom_text( vjust = 2 , size = 12 / 4  ) +
    labs( x = "Year" , 
          y = "Number of Facilities" 
          # title = "Number of facilities consistently reporting* confirmed cases each year" ,
          # subtitle = "*minimum 11 of 12 months" 
    )  + 
    scale_y_continuous( limits = c( 0, max( reporting$mean_monthly ) + 10 ) ) +
    theme_minimal( base_size = 12  ) +
    facet_grid( Intervention ~ .)
}

dataset.consisency = function( .dataset ){
  
  consistency = 
    bind_rows( 
      .dataset %>% filter( Intervention %in% 'Vaccinating' ) %>%
        dqaPercentReporting() %>%
        mutate( Intervention = 'Vaccinating' ) ,
      .dataset %>% filter( Intervention %in% 'Comparator' ) %>% 
        dqaPercentReporting() %>%
        mutate( Intervention= 'Comparator' )
    ) 
  
  consistency %>%
    ggplot( aes( x = as.character( Year ) , 
                 y = n_frequently_reporting, group = 1 , 
                 label = paste( n_frequently_reporting, "\n(" , label , ")" ) 
    ) )  + 
    geom_line(  linewidth = 1.25 ) +
    geom_text( vjust = 2 , size = 12 / 4  ) +
    labs( x = "Year" , 
          y = "Number of Facilities" 
          # title = "Number of facilities consistently reporting* confirmed cases each year" ,
          # subtitle = "*minimum 11 of 12 months" 
    )  + 
    scale_y_continuous( limits = c( 0, max( consistency$n_frequently_reporting ) + 10 ) ) +
    theme_minimal( base_size = 12  ) +
    facet_grid( Intervention ~ .)
}


### Impact Model Functions ####

#  Function to create time-period datasets
dataset = function( data = mcc5, startMonth = yearmonth( "Jan 2015" ) , 
                    startEvalMonth = yearmonth( "Jan 2020") ,
                    numberTestMonths = 12 , 
                    endEvalMonth = yearmonth( "Dec 2022") ,
                    unadjusted = FALSE ,
                    grouping = FALSE ,
                    groups = "agegrp"
){
  
  cat("\n * MG2_Forecast_Functions: dataset")
  
  
  
  if ( ! "tbl_ts" %in% class( data ) ){
    cat("\n * MG2_Forecast_Functions: dataset: !tbl_ts:" )
    data = tsibble( data, index = 'Month' ,
                    key = groups )
  }
  
  if ( grouping ){
    cat("\n * MG2_Forecast_Functions: dataset: grouping:", groups )
    groups = rlang::syms( groups )
    
    fable.data.age = data %>%
      filter( Month >= startMonth ) %>%
      group_by( {{ groups }} ) %>%
      summarise( total = sum( total , na.rm = T )) 
    
    # %>%
    #   pivot_wider( names_from = agegrp , values_from = total ) %>% 
    #   rename( younger = "Under5" , older = "Over4" )%>%
    #   mutate( log_younger = log( younger ) , log_older = log( older ))
    
  } else {
    
    fable.data = data %>%
      filter( Month >= startMonth , Month <= endEvalMonth ) %>%
      summarise( total = sum( total , na.rm = T )) 
  }
  
  
  pre.intervention = fable.data %>% 
    filter( Month < startEvalMonth )
  
  pre.intervention.train = fable.data %>% 
    filter( Month < startEvalMonth - numberTestMonths  )
  
  pre.intervention.test = fable.data %>% 
    filter( Month >= startEvalMonth - numberTestMonths & 
              Month < startEvalMonth )
  
  post.intervention = fable.data %>% 
    filter(  Month >= startEvalMonth & 
               Month <= endEvalMonth ) 
  
  # year 1
  post.intervention.yr1 = fable.data %>% 
    filter(  Month >= startEvalMonth & 
               Month < startEvalMonth + 12 ) 
  
  # year 2
  post.intervention.yr2 = fable.data %>% 
    filter(  Month >= startEvalMonth + 12 & 
               Month < startEvalMonth + 24 ) 
  
  # year 3
  post.intervention.yr3 = fable.data %>% 
    filter(  Month >= startEvalMonth + 24 & 
               Month < startEvalMonth + 36 )  
  
  
  return( list( fable.data= fable.data, 
                pre.intervention = pre.intervention, 
                pre.intervention.train = pre.intervention.train , 
                pre.intervention.test = pre.intervention.test ,
                post.intervention = post.intervention , 
                post.intervention.yr1 = post.intervention.yr1 , 
                post.intervention.yr2 = post.intervention.yr2 , 
                post.intervention.yr3 = post.intervention.yr3 ) )
  
}

# d = dataset( mcc5 )


## Function to create models ( OOS Training and Testing ) ####

tsmodels = function( train_data , test_data, n_forecasts = 2000 , 
                     .var = 'younger',
                     numberForecastMonths = 12 , 
                     type = NA , # c('transform and covariate' ,'transform', 'covariate' ) 
                     covariate = NULL , ensemble = TRUE , msg = TRUE ,
                     .set.seed = TRUE ){
  
  if ( !is.na( type ) && !type %in% c('transform and covariate' , 'transform', 'covariate' ) ){
    cat('\n Must select one of three types:', 
        c('transform and covariate' , 'transform', 'covariate' ) )
    return( NULL )
  }
  
  # set var for models.  Default is younger
  .var = rlang::sym( .var ) 
  train_data = train_data %>% mutate( var = {{ .var }} )
  test_data = test_data %>% mutate( var = {{ .var }}) %>% select( -{{.var}}, -var )
  
  if (! is.null( covariate) ) covariate = rlang::sym( covariate )
  
  tic()
  if (msg) cat('\n - tsmodels: Preparing primary models' )
  
  tic()
  
  if ( is.na( type ) ){
    pre.intervention.train.primary.models <- train_data %>%
      model(
        a = ARIMA( var )
        , e = ETS(  var )
        , n = NNETAR( var )
        , t = TSLM( var )

        , p1 = prophet( var ~ season("year", order = 1, type = "multiplicative")
                       )
        , p4 = prophet(  var ~ season("year", 4, type = "multiplicative")  )
        , p8 = prophet(  var ~ season("year", 8, type = "multiplicative")  )
      )
  }
  
  if ( type %in% 'transform and covariate' ){
    pre.intervention.train.primary.models <- train_data %>%
      model(
        a = ARIMA( log( var ) ~   log( {{covariate}} ) )
        , e = ETS(  log( var ) )
        , n = NNETAR(  log( var ) ~ {{covariate}}   ) 
        , t = TSLM( log( var )  ~ trend() + season() + {{covariate}}  ) 
        , p1 = prophet( log( var ) ~ {{covariate}} + 
                          season("year", 1, type = "multiplicative")  )
        , p4 = prophet(  log( var )  ~ {{covariate}} + 
                           season("year", 4, type = "multiplicative")  )
        , p8 = prophet(  log( var )  ~ {{covariate}} + 
                           season("year", 8, type = "multiplicative")  )
      )
  }
  
  if ( type %in% 'transform' ){
    pre.intervention.train.primary.models <- train_data %>%
      model(
        a = ARIMA( log( var )   )
        , e = ETS(  log( var ) )
        , n = NNETAR(  log( var )   ) 
        , t = TSLM( log( var )  ~ trend() + season()  ) 
        , p1 = prophet( log( var ) ~ 
                          season("year", 1, type = "multiplicative")  )
        , p4 = prophet(  log( var )  ~ 
                           season("year", 4, type = "multiplicative")  )
        , p8 = prophet(  log( var )  ~ 
                           season("year", 8, type = "multiplicative")  )
      )
  }
  
  if ( type %in% 'covariate' ){
    if (.set.seed) set.seed( 1432 )
    pre.intervention.train.primary.models <- train_data %>%
      model(
        a = ARIMA( var  ~  log( {{covariate}} ) )  
        , e = ETS(  var )
        , n = NNETAR(  var ~ {{covariate}}   ) 
        , t = TSLM( var ~ trend() + season() + {{covariate}}  ) 
        , p1 = prophet( var ~ {{covariate}} +
                          season("year", 1, type = "multiplicative")  )
        , p4 = prophet(  var  ~ {{covariate}} +
                           season("year", 4, type = "multiplicative")  )
        , p8 = prophet(  var ~ {{covariate}} +
                           season("year", 8, type = "multiplicative")  )
      )
  }
  
  t = toc(quiet = TRUE)
  
  if (msg) cat('\n - tsmodels: Primary models finished.', t$callback_msg, 
               '\n - tsmodels: Preparing primary forecasts...' )
  
  tic()
  if (.set.seed) set.seed( 1432 )
  
    pre.intervention.train.primary.forecasts = 
      suppressMessages(
        pre.intervention.train.primary.models %>% 
      forecast( h = numberForecastMonths # new_data = test_data ,
                , times = n_forecasts  ) %>% 
      mutate( samples = generate( var, n_forecasts  ) )
    )
  
  t = toc(quiet = TRUE)   
  
  
  if (msg) cat('\n - tsmodels:Primary forecasts finished', t$callback_msg, 
               '\n - tsmodels: Preparing ensemble forecasts...' )
  
  # Ensemble forecasts
  if ( ensemble ){
    tic() 
    if (.set.seed) set.seed( 1432 )
    pre.intervention.train.combo.forecasts = combination_forecasts(
      pre.intervention.train.primary.forecasts )   
    t = toc(quiet = TRUE)
    if (msg) cat('\n - tsmodels: Ensemble forecasts finished', t$callback_msg  )
    
    t = toc(quiet = TRUE)
    if (msg) cat('\n - tsmodels: Total time', t$callback_msg  ) 
    return( pre.intervention.train.combo.forecasts )
    
  } else {
    
    t = toc(quiet = TRUE)
    if (msg) cat('\n - tsmodels:Total time', t$callback_msg  ) 
    return( pre.intervention.train.primary.forecasts )
  }
  
  
  
}

# test.forecasts = tsmodels( d$pre.intervention.train , d$pre.intervention.test )


## Function to validate and select model ####


model_metrics = function( test.forecasts , test.data , msg = TRUE,
                          .var = 'younger' ,
                          grouping = FALSE ,
                          groups = "agegrp" ){
  
  if (msg) cat('\n * model_metrics: Calculating monthly SWAPE' ) 
  
  var = rlang::sym( .var )
  
  if ( grouping ){
    selectVars =  c( groups , .var, "Month" ) 
    byVars = c( groups ,  "Month" ) 
    group_by_cols1 = c( groups, ".model", "Month" ) 
    group_by_cols2 = c( groups, ".model", ".id" ) 
    group_by_cols3 = c( groups, ".model" ) 
  } else { 
    selectVars = c( .var, "Month" ) 
    byVars = c( "Month" ) 
    group_by_cols1 = c( ".model", "Month" ) 
    group_by_cols2 = c( ".model", ".id" ) 
    group_by_cols3 =  ".model" 
    } 
  
  
  # tic()

  pre.intervention.train.combo.accuracyOOS =
    test.forecasts %>%
    inner_join( test.data %>%
                  select( all_of( {{ selectVars }} ) ) %>%
                  rename( actual = {{ var }} ) ,
                by = byVars ) %>%
    unnest( samples ) %>%
    group_by( across(  group_by_cols1 ) ) %>%
    mutate(
      .id = row_number() ,
      ae = abs(actual - samples) 
    ) %>%
    group_by( across(  group_by_cols2  ) )  %>%
    summarise(
      # swape =  200 * sum( ae ) / ( abs(actual) + abs(samples) )
      swape = 200 * sum( ae ) / ( sum( abs(actual) ) + sum( abs(samples) ) ) ,
      .groups = "drop"
    )
  
  # t = toc( quiet = TRUE )
  # if (msg) cat( '\n ', t$callback_msg , '\nCalculating mean SWAPE') 
  
  pre.intervention.train.combo.accuracyOOS.all =
    pre.intervention.train.combo.accuracyOOS %>%
    group_by( across(  group_by_cols3  )   ) %>%
    summarise(
      swape = mean( swape ) ,
      .groups = "drop"
    )
  
  # t = toc( quiet = TRUE )
  # if (msg) cat('\n Total time' , t$callback_msg   ) 
  
  return( pre.intervention.train.combo.accuracyOOS.all )
  
}

# validations = model_metrics( test.forecasts , d$pre.intervention.test )


## Function to select best model ####


modelSelection = function( modelMetrics , 
                           type = c('synchronize', 'optimize' ) ,
                           table = TRUE ,
                           grouping = FALSE ,
                           groups = "agegrp" ){
  
  if ( grouping ){
    selectVars = c( groups, ".model" )
  } else { selectVars =  ".model" }
  
  ranks = best_fables_accuracy( fables_accuracy = modelMetrics , metric = 'swape'  ) 
  
  if ( table ){
    ranks %>%
      filter( `Intervention Rank` <= 10) %>%
      rename( `Intervention\nRank` =`Intervention Rank` ) %>%
      flextable() %>%
      FitFlextableToPage() %>%
      bg( i = ~ `Intervention\nRank` == 1 | percent_diff < 5 , bg = "#D3D3D3")  # Light grey background
  }
  
  if ( type %in% 'synchronize' ){
    modelSelection = ranks %>% 
      filter( mean == min( ranks$mean  , na.rm = TRUE ) )  %>%
      select( all_of( selectVars ) )
  }
  
  if ( type %in% 'optimize' ){
    modelSelection = ranks %>% 
      filter( row_number() == 1 )  %>%
      select( all_of( selectVars ) )
  }
  
  return( modelSelection )
}

# model_selection = modelSelection( validations , type = 'synchronize' )


## Plot actual vs predicted ####


plotActualPredicted = function(  actual, predicted , .var = "younger" , ...){
  
  var = rlang::sym( .var )
  
  group_by_vars = c( key_vars( predicted ) , 
                     index_var( predicted ) )
  
  fig.predicted.actual = 
    predicted %>% 
    unnest( samples ) %>%
    group_by( across( all_of( group_by_vars ) ) ) %>%
    mutate( .rep = row_number() ) %>%
    # filter( .rep <100 ) %>%
    as_tsibble( index = index_var( predicted ) , 
                key = c( key_vars( predicted ) , .rep ) ) %>%
    autoplot( samples, alpha = .01 )  +
    # geom_line( data = combo.forecasts.yr13 , aes(y = .mean) , color = "grey") +
    
    geom_line( data = actual , 
               aes( x = Month, y = {{ var }} ) ,  
               color = "black") +
    scale_x_yearmonth(breaks = date_breaks("1 year"),
                      labels = date_format("%b\n%Y")) +
    labs(
      # title = "Samples (colors) of forecast model (aen) and actual <5 confirmed malaria cases (black)",
      y = "Reported Confirmed Malaria Cases",
      x = "" # \nMonths following RTS,S implementation"
    ) +
    # facet_wrap( ~ Intervention, ncol = 1 ) +
    facet_grid( Intervention ~ . , scales = 'free' ) +
    # facet_grid( Intervention ~ .) +
    guides( color = "none" ) + 
    theme_minimal()
  
  return( fig.predicted.actual )
}



## Calculate impact within intervention ####

diff = function( actual ,  predicted , .var = 'younger' , grouping = TRUE  , ...){
  
  var = rlang::sym( .var )
  
  if ( grouping ){ 
      groups = c( Intervention , .model )
      selectVars = c( "Intervention", "Month", .var  )
  } else {
    groups = '.model'
    selectVars = c( "Month", .var  )
  }
  
  forecasts.difference = predicted %>% 
    inner_join( bind_rows(  actual )  %>% 
      select( all_of( selectVars ) ) %>%
      rename( actual = {{ .var }} ) , 
    by = setdiff( selectVars, .var ) ) %>%
    group_by( across( c( groups, "Month" ) )  ) %>%
    unnest( samples ) %>%
    group_by( across( c( groups, "Month" ) ) ) %>%
    # mutate( .rep = row_number() ) 
    mutate( 
      .rep = row_number( ) ,
      e = actual - samples  # negative when predicted>actual
    ) %>%
    group_by( across( base::union( groups, ".rep" ) ) )  %>%
    summarise( WPE = sum(  e  ) / sum( samples ) * 100  )  
  
  return( forecasts.difference )
} 

# interventionDifference = diff( evaluation.forecasts , d$post.intervention )


diff.summary = function( actual , predicted ,
                         .var = 'younger' , 
                         grouping = FALSE , groups = "Intervention" , ... ){
  
  if ( grouping ){ 
    groups = c( Intervention , .model )
  } else {
    groups = '.model'
  }
  
  diff( actual ,  predicted , .var, grouping = grouping , groups = groups ) %>%
    group_by( across( groups )) %>%
    summarise( 
      n = n() ,
      mean = mean( WPE ) ,
      sd = sd( WPE ) ,
      median = median( WPE ) ,
      hdi_lower = hdi( WPE )[1] ,
      hdi_upper = hdi( WPE )[2] 
    ) 
}


diffHistogram = function( actual , predicted, xlimits = c(NA, NA) , 
                          ... ){
  
  diffPredictedActual = diff( actual , predicted , ... )
  
  n_forecasts = max( diffPredictedActual$.rep )
  
  diffPredictedActual.summary = diff.summary( actual , predicted, ... )
  
  
  d = diffPredictedActual %>%
    inner_join( diffPredictedActual.summary , 
                by = join_by( Intervention, .model  ) ) %>%
    mutate( .model = paste0('tsmodel = ', .model )) 
  
  ggplot( ) +
    geom_histogram( data = d ,  aes(  WPE ) , color = 'white' , binwidth = 1 ) +
    geom_histogram( data = d %>% 
                      filter( round( WPE ) == round( median )) , 
                    aes(  WPE ) ,
                    binwidth = 1, fill = "blue", alpha = 0.5) +
    scale_x_continuous( limits = xlimits ) +
    facet_grid( Intervention ~ .model , scales = 'fixed' ) +
    labs( 
      # title = 'Distribution of estimated impact for Comparator and Vaccinating, Year 1 post-intervention',
      subtitle = paste0(
        'Each observation represents estimated difference from an individual forecast (n=',
        n_forecasts, ')' ) ,
      caption = 'Blue bar represents median value' ,
      x = "Weighted Percent Error (WPE)")
}


impact.reps = function( actual, predicted, grouping = TRUE ,... ){
  
  impact.reps =  diff( actual, predicted, grouping = grouping,  ... )  %>%  
    {
    if ( grouping == TRUE ) {
      differences(.) %>% 
      as_tibble() %>% rename( WPE = value )
    } else {
      .
    } }  
}

impact.summary = function( actual, predicted, grouping = FALSE ,
                           groups = c('Comparator', 'Vaccinating' ),
                           ...  ){
  
  impact.reps( actual, predicted, grouping = grouping, 
               # groups , 
               ... ) %>%
    summarise( 
      n = n() ,
      mean = mean( WPE ) ,
      sd = sd( WPE ) ,
      median = median( WPE ) ,
      hdi_lower = hdi( WPE )[1] ,
      hdi_upper = hdi( WPE )[2] 
    ) %>%
    mutate( across( where(is.numeric) , \(x) round(x , 2  ) ) ,
            Intervention = paste0( 'Impact (1-' ,
                                   length(unique(predicted$Month)) ,' months)' 
            ) , 
            .model = paste( groups[2],  "-" , groups[1] ) )
}


impactHistogram = function( actual, predicted, ... ){
  
  diffPredictedActual = diff( actual , predicted , ... )
  
  n_forecasts = max( diffPredictedActual$.rep )
  
  impact.reps( actual, predicted, ... ) %>%
    bind_cols( impact.summary( actual, predicted , ...) ) %>%
    ggplot( aes( WPE ) ) +
    geom_histogram( color = 'white' , fill = 'grey' , binwidth = 1 ) +
    geom_histogram(data = . %>% filter( WPE >= hdi_lower & WPE <= hdi_upper ) ,
                   binwidth = 1, fill = "black", alpha = 0.5) +
    geom_histogram(data = . %>% filter( round( WPE ) == round( median )) ,
                   binwidth = 1, fill = "blue", alpha = 0.5) +
    scale_x_continuous( limits = c(-40, 20 ),
                        breaks = seq(-40,20, 5 )) +
    labs( 
      # title = 'Distribution of estimated impact, during 36 months post-intervention',
      subtitle = paste0(
        'Each observation represents estimated difference from an individual forecast (n=' ,
        n_forecasts , ')' ),
      caption = 'Blue bar represents median value, dark bars represent highest density (95% of values)' ,
      x = "Weighted Percent Error (WPE)")
}


impactTable = function( actual, predicted , condense = FALSE, 
                        grouping = FALSE, groups = 'Intervention', 
                        .var = 'total' , ...){
  
  impactTable = 
    bind_rows( diff.summary(  actual , predicted , grouping = grouping, .var = .var ,  ...) ,
               impact.summary( actual, predicted, grouping = grouping , .var =.var , ... ) ) %>% 
    
    mutate( across( where(is.numeric ) , \(x) signif(x, 3 ))
    ) 
  
  if ( condense ){
    impactTable = impactTable %>% select( -n, -sd, - median )
  }
  
  return( impactTable )
}


impact.tableaux = function( actual, predicted , xlimits = c(NA, NA) , ...){
  
  
  plotActualPredicted( actual , predicted , ... ) %>% print 
  
  diffHistogram( actual , predicted , ... ) %>% print
  
  impactHistogram( actual , predicted , ... ) %>% print
  
  impactTable( actual , predicted , ...) %>%
    
    flextable() %>% 
    
    colformat_double( digits = 1) %>%
    
    colformat_double( j = 3, digits = 0) %>%
    
    FitFlextableToPage() %>%
    
    bg( i = ~ grepl( 'Impact', `Intervention` ), bg = "#D3D3D3")  # Light grey background
  
}





