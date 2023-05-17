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

outlier.df = function( ts , ou = NULL , pb= NULL ){
  ## TODO: map_df( elements, ts %>% filter( data %in% .x ))
  if (!is.null( pb )) pb$tick()$print()
  if (!is.null( ou )){ d = ts %>% filter( orgUnit %in% ou ) }
  
  print(ou)
  
  elements = unique( d$data )
  
  to = map( elements , ~{
    
    e = d %>% filter( data %in% .x ) 
    # if ( nrow(e) > 24 ){
      decompose  = e %>%
        model( arima = ARIMA( value  ~ 0 + 
                 pdq(p = 0:2, d = 0:1, q = 0:2) + 
                 PDQ(P = 0:2, D = 0:1, Q = 0:2, period = '1 year') 
        )
               ) %>%
        augment() 
        # print('dec')
        anom = decompose %>%
          anomalize( .resid , 
                     method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
                     alpha = 0.01 , max_anoms = 0.1 
                     # , verbose = TRUE
          )
        
        # print('anom')
        topOutliers = anom %>% 
          mutate( e = .resid / (.fitted + 1) )   %>% 
          filter( anomaly %in% 'Yes' , value > 9 , e > 5) %>%
          as_tibble()
    # } else {
      
      # topOutliers = tibble( orgUnit = ou , data = .x)
    # }
    topOutliers 
  })
  
  return( to  )
}

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