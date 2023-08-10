# Functions for time-series analyses


Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

week_Year = function( x ){ tsibble::yearweek( x ) }

patch.plots = function( df , wrap_nchar = 35 , .period = Month ){
  .period = rlang::enquo( .period )
  plots = map( unique( df$data ) , ~ df %>% 
                 filter( data %in% .x ) %>%
                 ggplot( aes( x = !! .period , y = value , group = data ) ) +
                 geom_line( ) +
                 facet_grid( name  ~ data  , scales = 'free', 
                             labeller = labeller( data = label_wrap_gen( wrap_nchar ))
                 )
  )
  patch.plots = reduce( plots, `+`)
  return( patch.plots )
}

df.ts = function( df , period = "Month" , missing.value = NA ){
  
   .period = rlang::enquo( period )
  
   if (  period %in% 'Month' ){
     df = df %>% mutate( Month =  Month_Year( period ) )
   
     } else {
     
     df = df %>% mutate( Week =  week_Year( period ) )
   }
  
    ts = df %>%
    mutate( COUNT = as.numeric( COUNT ) ,
            SUM = as.numeric( SUM ) 
    ) %>% 
    unite( "data" , dataElement, Categories ) %>%
    pivot_longer( c( SUM , COUNT ) ) %>%
    as_tsibble( key = c(orgUnit, data, name ) , index = !! .period ) %>%
    mutate( raw = value ) %>% # preserve missing values
    # set NA to missing 
    fill_gaps( value = missing.value , .full = TRUE )
    
    return( ts )
  
}

df_pre_ts = function( df , period = "Month" , missing.value = NA  ){
  # if ( !is.null( pb )) pb()
  cat( "\n * df_pre_ts")
   
  if ( period %in% c( "Week", "Weekly") ) .period = "Week"
  if ( period %in% c( "Month", "Monthly") ) .period = "Month"

   .period = rlang::enquo( .period ) 
   
   # remove rows with no count (e.g.orgUnit = LEVEL-7 COUNT = NA)
   df = filter( df , !is.na( COUNT ) )
   
   cat( "\n - unite data and data.id")
         
   if (  period %in% 'Month'  ){
     
      if ( ! "data.table" %in% class( df ) ) df = data.table::as.data.table(df)
      df_pre_ts = df[ , ':=' ( Month = Month_Year( period ) ,
                       COUNT = as.integer( COUNT ) ,
                       SUM = as.numeric( SUM ) 
                       # , Categories = ifelse( is.na( Categories ), "", Categories )
                       ) ]  %>%
       as_tibble() %>%
       unite( "data" , dataElement, Categories , remove = FALSE , na.rm = TRUE ) %>%
       unite( "data.id" , dataElement.id, categoryOptionCombo.ids, remove = FALSE  , na.rm = TRUE ) 

       return( df_pre_ts )
     
     } else {
     
     # convert to data.table
    if ( ! "data.table" %in% class( df ) ) df = data.table::as.data.table(df)
    df_pre_ts = df[ , ':=' ( Week = yearweek( period ) ,
                       COUNT = as.integer( COUNT ) ,
                       SUM = as.numeric( SUM ) ,
                       Categories = ifelse( is.na( Categories ), "", Categories )
                       ) ] %>%
       as_tibble()%>%
       unite( "data" , dataElement, Categories , remove = FALSE ) %>%
       unite( "data.id" , dataElement.id, categoryOptionCombo.ids, remove = FALSE  ) 
       
     # testing
     # saveRDS( df_pre_ts, "df_pre_ts.rds")
     
     return( df_pre_ts )
}}

df_ts = function( df.pre.ts , period = "Month" , 
                  fill.gaps = FALSE , 
                  missing.value = NA ){
  
  cat('\n * TS_Modeling_Functions.R: df_ts')

  if ( period %in% c( "Week", "Weekly") ) .period = "Week"
  if ( period %in% c( "Month", "Monthly") ) .period = "Month"
  
  .period = rlang::enquo( period )
  
  cat( '\n - .period is' , period )
  
  # testing
  # saveRDS( df.pre.ts, "df.pre.ts.rds")
     
  cat( '\n - data.table distinct' )
  df.pre.ts = as.data.table( df.pre.ts )
  df.pre.ts = unique( df.pre.ts , by = c("orgUnit", "data.id", period ))
  
  cat( '\n - as_tsibble')
  ts = df.pre.ts %>%
    as_tsibble( key = c(orgUnit, data.id ) , 
                index = !! .period ,
                validate = FALSE ) 

  # set NA to missing 
  if ( fill.gaps ){
      cat( '\n - fill gaps' )  
      ts = ts %>% fill_gaps( value = missing.value , .full = TRUE )
    } 
  
  cat( '\n - done' )  
  return( ts )
    
    }

# outlier.df = function( ts , ou = NULL , pb= NULL ){
#   ## TODO: map_df( elements, ts %>% filter( data %in% .x ))
#   if (!is.null( pb )) pb$tick()$print()
#   if (!is.null( ou )){ d = ts %>% filter( orgUnit %in% ou ) }
#   
#   print(ou)
#   
#   elements = unique( d$data )
#   
#   to = map( elements , ~{
#     
#     e = d %>% filter( data %in% .x ) 
#     # if ( nrow(e) > 24 ){
#       decompose  = e %>%
#         model( arima = ARIMA( value  ~ 0 + 
#                  pdq(p = 0:2, d = 0:1, q = 0:2) + 
#                  PDQ(P = 0:2, D = 0:1, Q = 0:2, period = '1 year') 
#         )
#                ) %>%
#         augment() 
#         # print('dec')
#         anom = decompose %>%
#           anomalize( .resid , 
#                      method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
#                      alpha = 0.01 , max_anoms = 0.1 
#                      # , verbose = TRUE
#           )
#         
#         # print('anom')
#         topOutliers = anom %>% 
#           mutate( e = .resid / (.fitted + 1) )   %>% 
#           filter( anomaly %in% 'Yes' , value > 9 , e > 5) %>%
#           as_tibble()
#     # } else {
#       
#       # topOutliers = tibble( orgUnit = ou , data = .x)
#     # }
#     topOutliers 
#   })
#   
#   return( to  )
# }

with.without_outliers = function( ts, out){
  
  with = ts %>%
    filter( !data %in% 'NA_NA' ) %>%
    group_by( data , name ) %>%
    summarise( value = sum( value, na.rm = TRUE )) %>%
    mutate( name = recode( name,  'SUM'= 'Original Data'  ) )
  
  without =  ts %>%
    filter( !data %in% 'NA_NA' ) %>%
    anti_join( out ) %>%
    group_by( data , name ) %>%
    summarise( value = sum( value, na.rm = TRUE ) ) %>%
    mutate( name = recode( name,  'SUM'= 'Removed Outliers'  ) )
  
  bind_rows( with %>% as_tibble, 
             without %>% as_tibble %>% filter( !name %in% 'COUNT') ) 
  
}

mable_data = function(         
    ml.rtss.data = ml.rtss ,
    .orgUnit = TRUE , # group by orgunit
    .startingMonth = NULL ,
    .endingMonth = NULL , 
    .missing_reports = NULL ,
    selected.only = TRUE ,
    # .evaluation_month = NULL  ,
    .error = NULL , 
    covariates = NULL , 
    .split = NULL , 
    aggregate_by_split = TRUE ,
    levelNames = NULL , 
    hts = FALSE , 
    agg_level = NULL ,
    remove.aggregate = TRUE ,
    .cat = NULL , 
    testing = FALSE , ...
){
  
  if (.cat) cat("\n* TS_Modeling_Functions mable_data ")
  
  
  if ( testing ){
    saveRDS( ml.rtss.data , "ml.rtss.data.rds")
    save(.orgUnit, hts, remove.aggregate, .error, .startingMonth , .endingMonth, .missing_reports, agg_level, levelNames, .split , covariates ,
          file = "mable_data_parameters.rda" )
  } 
  
  d = ml.rtss.data %>% 
    error_factor %>% 
    # filter( Month >= ( yearmonth( startingMonth )   ) )  %>%
    cleanedData( . , 
                 error =  .error , .cat = .cat ) %>%
    
    selectedData(  startingMonth = .startingMonth ,
                   # data_categories = "agegrp" , 
                   endingMonth = .endingMonth ,
                   missing_reports = .missing_reports ,
                   .cat = .cat ) %>% as_tibble() 
  
  
    group_by_cols = groupByCols( period = dataPeriod( d ) , 
                               orgUnit = .orgUnit ,
                               hts = hts , 
                               agg_level = agg_level ,
                               levelNames = levelNames, 
                               split = .split , .cat = .cat )
  
   if (.cat) cat("\n - group_by_cols " , group_by_cols )
    
    num_facilities = mostFrequentReportingOUs( d ,
                                             startingMonth = .startingMonth ,
                                             endingMonth = .endingMonth ,
                                             missing_reports = .missing_reports , 
                                             .cat = .cat ) %>% length()
  
    .dataSets = unique( d$dataSet ) 
    num_datasets = length( .dataSets )
    
    data.total = dataTotal( data = d , 
                            group_by_cols = group_by_cols , 
                            dataSets = .dataSets, 
                            covariates = covariates , 
                            .cat = .cat  )
  
  if ( hts ){
    if (.cat) cat("\n - hts " )
    
    hts_formula = htsFormula( hts = hts , 
                              levelNames = levelNames ,  
                              agg_level = agg_level , 
                              all.levels = FALSE  , 
                              num_facilities =  num_facilities ,
                              num_datasets = num_datasets , 
                              split = split , .cat = .cat ) 
    
    data.agg.ts = htsData( data = data.total ,  
                           hts = hts , 
                           hts_formula = hts_formula , 
                           covariates = covariates , 
                           group_by_cols = group_by_cols , .cat = .cat )
  } else {
    if (.cat) cat("\n - aggData " )
    data.agg.ts = aggData( data.total = data.total ,  
                           covariates = covariates , 
                           group_by_cols = group_by_cols , .cat = .cat )
  }
  
  
  if (.cat) cat("\n - trend.data " )
    
    if (.cat) cat("\n - num_datasets: " , num_datasets )
    if (.cat) cat("\n - num_facilities: " , num_facilities )
    

  if (.cat) cat("\n - remove.aggregate: " , remove.aggregate )
  
  trend.data = trendData( .d = data.agg.ts , 
                          levelNames = levelNames , 
                          startingMonth = .startingMonth , 
                          endingMonth = .endingMonth ,
                          selected.only = selected.only  ,
                          num_facilities =  num_facilities ,
                          num_datasets = num_datasets , 
                          split = .split ,
                          agg_level = agg_level  , 
                          remove.aggregate = remove.aggregate , 
                          .cat = .cat  )
  
}


  fit_function = function( train.data , index = NULL , mixed.model = FALSE , covariates = NULL ){ 
    
    
        options(future.rng.onMisuse="ignore")
    
    
        if (is.null( index)){
          
          train.data. = train.data
        } else {
          train.data. = train.data %>%
                  semi_join( key_data( train.data )[ index ,], by = key_vars( train.data ) )
                  # filter( Cluster %in% unique_clusters[ .x ] ) %>%
                  # fill( all_of(covariates) , .direction = "down" ) 
        }
            
    # transformaion parameters
            x = train.data.[complete.cases( train.data.) ,]$total
            x. = x[ x>0 ]
            .lam = forecast::BoxCox.lambda( x. , method = "guerrero", lower = 0.1 )
  
            arima.formula = "total ~  pdq() + PDQ( period = '1 year' )"  
        
            arima.formula.tr = paste( "fabletools::box_cox( total, lambda = ", .lam, 
                                      ") ~  pdq() + PDQ( period = '1 year' ) " ) 
        
            arima.formula.cov.tr = paste(  "fabletools::box_cox( total, lambda = ", .lam ,
                                           " ) ~  pdq() + PDQ( period = '1 year' ) + xreg( " ,
                                      paste( covariates , collapse = " + " ) , ")" 
            ) 
            
            prophet.cov.tr = paste(  "fabletools::box_cox( total, lambda = ", .lam ,
                                      " ) ~ growth( 'linear' ,
                                           n_changepoints = 5 ) + 
                                           season( period = 12, order = 2,
                                              type = 'multiplicative')  + xreg( " ,
                                     paste( covariates , collapse = " + " ) , ")" 
            )
            
            prophet.cov.tr = paste(  "fabletools::box_cox( total, lambda = ", .lam ,
                                     " )  ~ growth('linear' ,
                                           n_changepoints = 5 ) + 
                                           season( period = 12, order = 2,
                                              type = 'multiplicative') + xreg( " ,
                                     paste( covariates , collapse = " + " ) , ")" 
            )
            
      # cat( "\n fitting with .lam", .lam )
      if ( !is.null( covariates) ){
        
        fit = train.data. %>% 
          model(
            # ets = ETS( total ),
            ets.tr = ETS( fabletools::box_cox( total , lambda = !! {{ .lam }}  ) ) 
            
            , arima.tr = ARIMA( as.formula( !! {{ arima.formula.tr }} ) ) 
            
            , arima.cov.tr = ARIMA(  as.formula( !! {{  arima.formula.cov.tr   }} ) )
            
            , prophet = prophet( as.formula( !! {{  prophet.tr   }} ) )
            
            , nnetar.tr = NNETAR( fabletools::box_cox( total , lambda = !! {{ .lam }}   ) )
            
            , nnetar.tr.cov = NNETAR( fabletools::box_cox( total , lambda = !! {{ .lam }}   )
                                      , lambda = .lam
                                      , xreg = c( .$any_stock_out , .$lag_avg_mm , .$llin )
            )
            , .safely = TRUE ) 
        
      } else {
        
        fit = train.data. %>% 
          model(
            # ets = ETS( total ),
            ets.tr = ETS( fabletools::box_cox( total , lambda = !! {{ .lam }}  ) ) 
            
            , arima.tr = ARIMA( as.formula( !! {{ arima.formula.tr }} ) ) 
            
            
            , prophet = prophet( as.formula( !! {{  prophet.cov.tr   }} )  )
            
            , nnetar.tr = NNETAR( fabletools::box_cox( total , lambda = !! {{ .lam }}   ) )
            
            
            , .safely = TRUE ) 
      }
            
    
      
        if ( mixed.model ) fit = fit %>%
  
                mutate( 
                      mixed = ifelse( is_null_model( ets.tr ) , 
                                      ( arima.tr + arima.cov.tr + prophet + nnetar.tr + nnetar.tr.cov ) / 5 , 
                                      (ets.tr + arima.tr + arima.cov.tr + prophet + nnetar.tr + nnetar.tr.cov ) / 6 )
                  ) 
              
        return( fit )
  }
  
   forecast_function = function( fit, key , 
                                 test.data ,
                                .times = 100 ,
                                .cat = FALSE ){
  
            fit. = fit %>% semi_join( key, 
                                       by = names( key  ) )
            
            # correct for missing values nnetar models using data 12-mnths prior
            for ( train_i in 1:nrow( fit )){
              
              if( "nnetar.tr" %in% names( fit. ) ){
                n1 = fit.$nnetar.tr[[train_i]]
                future.n1 = n1$fit$future
                box_cox_var = names(n1$data)[grep('box', names(n1$data) )]
                past.n1 = n1$data %>% pull( box_cox_var )
                future.n1 = future.n1 %>% forecast::tsclean()
                n1$fit$future  = future.n1
                past.n1 = past.n1 %>% forecast::tsclean()
                n1$data[ , box_cox_var ]  = past.n1
                fit.$nnetar.tr[[train_i]] = n1
              }
                
              if( "nnetar.tr.cov" %in% names( fit. ) ){
                n2 = fit.$nnetar.tr.cov[[train_i]]
                future.n2 = n2$fit$future
                box_cox_var = names(n2$data)[grep('box', names(n2$data) )]
                past.n2 = n2$data %>% pull( box_cox_var )
                future.n2  = future.n2 %>% forecast::tsclean()
                n2$fit$future  = future.n2
                past.n2 = past.n2 %>% forecast::tsclean()
                n2$data[ , box_cox_var ]   = past.n2
                fit.$nnetar.tr.cov[[train_i]] = n2
              }
            }
  
        test.data.  = test.data 
        
        if ( ".id" %in% names( fit. )){
          test.data.  = test.data  %>% 
           semi_join( key, 
                      by = names( key  ) ) %>%
           filter( Month < eval.start[i]  )  
        }
        
    
    # if any nnetar forecast is nan, subsequent forecasts throw error, so dont continue
    if ( .cat )  cat( "\n - check for nnetar nan")
    
    skip.nn = FALSE
        
    if ( ".id" %in% names( fit. ) ){
    
    ids = unique( test.data$.id )
    
    options( warn = -1 )
    
    for ( id in seq_along( ids ) ){
      # cat( "\n id - ", id , " ")
      test.data.i = test.data. %>% filter( .id %in% id ) 
      
      for ( r in seq_along( 1:nrow( test.data.i ) ) ){
        # cat( "\n r - ", r , " ")
        fc.nn = fit. %>% 
            filter( .id %in% id ) %>%
            select( key_vars( fit. ) ,  starts_with( 'nnetar' ) ) %>%
            forecast( new_data = test.data.i %>% slice( 1:r ) , times = 1  ) 
        
        # fc.pr = fit. %>% 
        #     filter( .id %in% id ) %>%
        #     select( key_vars( fit. ) ,  starts_with( 'proph' ) ) %>%
        #     forecast( new_data = test.data.i %>% slice( 1:r )  , times = 1  ) 
        
        if ( any( is.nan( fc.nn$.mean )) ){
          skip.nn = TRUE 
          # cat( "\n BREAK")
          break
        }
    } }
    }
        
    if ( .cat )  cat( "\n - skip.nn ", skip.nn )
    
    if ( skip.nn ){
      fc = fit. %>% 
          select( key_vars( fit. ) ,  ! starts_with( 'nnet' ) ) %>% 
          forecast( new_data = test.data. , times = .times  ) 
      
    } else {
      fc = fit. %>% forecast( new_data = test.data. , times = .times  ) 
    }    
    
    options( warn = 0 )
    
    if ( "id" %in% names( fit. ) ){
      fc = fc %>%
          group_by(.id, .model ) %>%
          mutate( h = row_number() ) %>%
          ungroup() %>%
          as_fable( response = "total", distribution = total )
    }
    
    return( fc )
  }

