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
   
  # flag periods with W as weekly 
   weekly = any( grepl( "W", df$period[1:10]) )
   if (weekly ) period =  "Week" 
   
   .period = rlang::enquo( period )
  
   if (  period %in% 'Month' & !weekly ){
     df = df %>% mutate( Month =  Month_Year( period ) )
     df_pre_ts = df %>%
       mutate( COUNT = as.integer( COUNT ) ,
            SUM = as.numeric( SUM ) ,
            Categories = ifelse( is.na( Categories ), "", Categories )
            ) %>%
       unite( "data" , dataElement, Categories , remove = FALSE ) %>%
       unite( "data.id" , dataElement.id, categoryOptionCombo.ids, remove = FALSE  ) 
       
       # rename( raw = SUM ) %>%
       # pivot_longer( c( SUM , COUNT ) )
       # as_tsibble( key = c(orgUnit, data, name ) , index = !! .period ) %>%
       # mutate( value  = raw ) %>% # preserve missing values
       # set NA to missing
       # fill_gaps( value = missing.value , .full = TRUE )
    
       return( df_pre_ts )
     
     } else {
     
     # use Tsibble yearweek instead of week_year. results identical 
     # df = df %>% mutate( Week =  yearweek( period ) ,
       # COUNT = as.numeric( COUNT ) ,
       #      SUM = as.numeric( SUM )
       #      )
     # df = data.table::as.data.table( df )
     # convert to data.table
    if ( ! "data.table" %in% class( df ) ) df = data.table::as.data.table(df)
    df = df[ , ':=' ( Week = yearweek( period ) ,
                       COUNT = as.numeric( COUNT ) ,
                       SUM = as.numeric( SUM ) 
                       ) ] %>%
       as_tibble() %>%
       unite( "data" , dataElement, ifelse( is.na( Categories ) ,
                                            "", Categories ) 
       )
       # rename( raw = SUM ) %>%
       # pivot_longer( c( SUM , COUNT ) ) 
       # as_tsibble( key = c(orgUnit, data, name ) , index = !! .period ) %>%
       # # mutate( value  = raw ) %>% # preserve missing values
       # # set NA to missing
       # fill_gaps( value = missing.value , .full = TRUE )

     
     return( df )
}}

df_ts = function( df.pre.ts , period = "Month" , 
                  fill.gaps = FALSE , 
                  missing.value = NA ){
  
   # weekly = any( grepl( "W", df_pre_ts$period[1:10]) )
   # if (weekly ) period =  "Week" 
   # 
   # .period = rlang::enquo( period )
  
  ts = df.pre.ts %>%  
    as_tsibble( key = c(orgUnit, data.id ) , index = !! period ) 

    # set NA to missing 
    if ( fill.gaps ) ts = ts %>% fill_gaps( value = missing.value , .full = TRUE )

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

