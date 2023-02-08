# Cleaning.R
extremely_mad = function( x , # A vector of numeric values
                          deviation = 15, 
                          smallThreshold = 50 , 
                          key_entry_error = NA ,
                          over_max = NA ,
                          maximum_allowed = NA , 
                          logical = FALSE ,
                          .pb = NULL ,
                          .progress = FALSE ,
                          total = NA
                           ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
  
  if (.progress && !is.na( total )){
     # cat( "\n - setting up progress bar for extremely_mad()")
      setProgress( 
                detail = "Searching for extreme values within each orgUnit"  
                )
    incProgress( amount = 1 / total  ) 
  } 
  
  # Remove any value above maximum_allowed or key error
  # FALSE means that it fails the test.  TRUE is ok. 
  if ( ! all( is.na( over_max ) ) ){
     if ( all( !is.na( maximum_allowed ) ) ){
        over_max =  x > maximum_allowed 
      } else { over_max = NA }
  }
 
  # # Remove key entry errors
  # FALSE means that it fails the test.  TRUE is ok. 
  if ( !all(is.na( key_entry_error )) ) {
    key_error = ( x>=0 & x %in% key_entry_error )
  } else { key_error = NA }
  
  
  y = x
  y[  over_max | key_error  ] = NA
  
  if ( all( y <= smallThreshold | is.na( y ) )  ) {
      if ( logical ) return( rep( NA, length(y) )  ) 
      return( y )
  }
  
      # MAD
      medianVal = median( y , na.rm = TRUE )
      medianAbsDeviation = mad( y , na.rm = TRUE )
      # se <- function(x) sqrt( var(x , na.rm = TRUE )/ length( !is.na(x) ))
      # sdVal = sd( x , na.rm = TRUE )
      
      extreme = y > ( medianVal + deviation * medianAbsDeviation ) |
          y < ( medianVal - deviation * medianAbsDeviation )
      
      if ( logical ) return( extreme  ) # if extreme is TRUE, return FALSE (failed test)
      y[ !extreme ] = NA 
      return( y )
}

unseasonal = function( x ,  
                       smallThreshold = 100 , 
                       deviation = 3 ,
                       logical = FALSE ,
                       interpolate = FALSE , # only useful when logical is FALSE
                       .lambda = 1 , 
                       .pb = NULL ,
                       .progress = FALSE ,
                        total = NA ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
  
  if (.progress && !is.na( total )){
      setProgress( 
                detail = "Searching for seasonal adjusted outliers within each orgUnit"  
                )
    incProgress( amount = 1 / total  ) 
  } 

    # value = ifelse( !is.na(x), TRUE , NA ) 

    
    if (( sum( !is.na( x ) ) <= 1  ) | (all( x <= smallThreshold | is.na( x ) ) )){
      if ( logical ) return( rep( NA, length(x) )  ) 
      return( x )
    } 
                 
      x.ts = x %>% as.ts( . , frequency = 12 ) 
        
        x.forecast  = 
                
                tsclean( x.ts ,
                               replace.missing = interpolate ,
                               lambda = .lambda ) %>%
                as.integer()
        
        if ( !logical ) return( x.forecast )
        
        # Test for deviation from forecast
        MAD = mad( x , na.rm = TRUE )
        # standard_dev = sd( x, na.rm = TRUE )
            
        outlier = abs( ( x.forecast - x.ts ) / MAD ) >= deviation
            
        x.ts[ which( outlier ) ] =  NA
        # value[ outlier ] =  FALSE 
      
      if ( logical ) return( outlier ) # FALSE = outlier
      return( x.ts )
}


# MASE function borrowed from Metrics package, modified with sum na.rm = TRUE
abs_ae = function (actual, predicted){
  return(abs(actual - predicted))
  
}

mase = function (actual, predicted, step_size = 1 ){
    naive_start <- step_size + 1
    n <- as.numeric(length(actual))
    naive_end <- n - step_size
    sum_errors <- sum( abs_ae(actual, predicted), na.rm = TRUE )
    naive_errors <- sum(abs_ae(actual[naive_start:n], actual[1:naive_end]), na.rm = TRUE)
    
    # if ( (n * naive_errors/naive_end) > 0 |(n * naive_errors/naive_end) < 0 ){
      mase = sum_errors/(n * naive_errors/naive_end) 
    # } else { mase = NA }
    return( mase )
}

mad_outliers = function( d ,
                              .total = NULL , 
                              .threshold = 50,
                              key_entry_errors = NULL ){
  
  if ( is.null( key_entry_errors ) ){
    
    cat( '\n - mad_outliers: Scanning for repetive key entry errors')
    key_entry_errors =
      count( as_tibble( d %>%
                          filter( nchar(original) > 3 ,
                                  effectiveLeaf ) ) ,
             original ) %>%
      arrange(-n)
    
    # Default: values where the number happens at least 3 > than
    # median of the top 10 rows
    key_entry_errors = key_entry_errors %>%
      filter(  n > 3 * median(
        key_entry_errors %>% filter( row_number()<11 )  %>%
          pull( n ) )
      ) %>% pull( original )
    
    # print( head( key_entry_errors ) )
    if ( is_empty( key_entry_errors )  ) key_entry_errors = NA
  }
  
  
   m_o = d %>%  
            group_by( orgUnit, data.id ) %>%
            mutate(
              .max = ifelse( 
                grepl("stock", data, ignore.case = TRUE  ) &
                grepl("out|rupture", data , ignore.case = TRUE  )   &
                effectiveLeaf
                , 31, NA  )  ,

              ) %>%
            mutate( 
                 
                key_entry_error = ifelse( !is.na( key_entry_errors ) & !is.na( original ) , original %in% key_entry_errors , NA ) ,
              
                over_max = ifelse( !is.na( .max ) & !is.na( original ) , original > .max, NA ) ,
                
                mad15 = extremely_mad( ifelse( ( is.na( over_max) | !over_max ) & 
                                                 (is.na( key_entry_error ) | !key_entry_error) , 
                                                        original , NA ) , 
                                       deviation = 15 , 
                                       smallThreshold = .threshold ,
                                       key_entry_error = key_entry_error ,
                                       over_max = over_max ,
                                       maximum_allowed = .max , 
                                       logical = TRUE, 
                                       .progress = TRUE ,
                                       total = .total ) 
                
                , mad10 = extremely_mad( ifelse( !mad15 , original , NA ), 
                                         deviation = 10 , 
                                         smallThreshold = .threshold ,
                                         maximum_allowed = .max , 
                                         logical = TRUE ,
                                         .progress = FALSE ) 
                
                # , mad5 = extremely_mad( ifelse( !mad10 , original , NA ), 
                #                         deviation = 5 , 
                #                         smallThreshold = .threshold * 2 ,
                #                         maximum_allowed = .max , 
                #                         logical = TRUE ,
                #                         .progress = FALSE ) 
            )
   
   return( m_o )
}

seasonal_outliers = function( d ,
                              .total = NULL , 
                              .threshold = 50 ){
  
        data1.seasonal = d %>%  
        group_by( orgUnit, data.id ) %>%
        mutate(
          
          expected = unseasonal(  ifelse( ! mad10, original , NA) , 
                                  smallThreshold = .threshold * 2  , 
                                  logical = FALSE , # Returns forecasted value
                                  .progress = TRUE ,
                                   total = .total 
                                  ) ,
          
          seasonal5 = unseasonal(  ifelse( ! mad10, original , NA) , 
                                  smallThreshold = .threshold * 2  , 
                                  deviation = 5 ,
                                  logical = TRUE ) ,
          
          seasonal3 = unseasonal(  ifelse( ! seasonal5, original , NA) , 
                                  smallThreshold = .threshold * 2 , 
                                  deviation = 3 ,
                                  logical = .total )  
      )
        
        return( data1.seasonal )
}


outlier.summary = function(
    data = NULL ,
    cols = c( "key_entry_error", "over_max" , 'mad15', 'mad10', 
              # 'mad5',
              'seasonal5' , 'seasonal3',
              'expected') 
){
  warn<-options(warn=-1) # suppress divide by zero warning
  
  if ( 'expected' %in% cols ) cols = setdiff( cols, 'expected' )
  
  os.total = setDT( data )[ , .( Total = sum( original , na.rm = T ) ,
                                 N = sum( !is.na( original )) ) ]
  
  os <- setDT( data )[  ,
                        .( n = sum( !is.na( original ) ) ,
                           total = sum( original , na.rm = T )  ,
                           Max = max( original , na.rm = T )  ) ,
                        cols] %>%
    as_tibble 
  
  no.err.rows = map( os %>% select( !! cols ), ~  which( is.na( .x ) | .x == FALSE ) ) %>%
    Reduce( intersect ,  . )
  
  
  os.no.err = os[ no.err.rows , ] %>% 
    summarise( n =  sum( n, na.rm = T ) ,
               total = sum( total, na.rm = T ) ,
               Max = max( Max , na.rm =  T )) %>%
    mutate( err = "No Error Flags") 
  
  os.errs = 
    map_df( cols , ~ {
      r = os %>% filter( !! rlang::sym( .x ) == TRUE  ) 
      if ( nrow( r ) == 0 ) r = os[0,] %>% add_row()
      bind_cols( tibble( err = .x ) , r )
    }
    ) %>% 
    mutate( err = as_factor( err ) ) %>%
    group_by( err ) %>% 
    summarise( n =  sum( n, na.rm = T ) ,
               total = sum( total, na.rm = T ) ,
               Max = max( Max , na.rm =  T )
    ) 
  
  # select( !! cols  , n ,  `%N` ,  max , total , `%Total`  )
  
  rdt.outlier.summary = 
    bind_rows( os.errs , os.no.err )  %>%
    bind_cols( os.total  ) %>%
    mutate(
      `%N` = ifelse( n>0 , percent( n / N ) , -Inf ) ,
      `%Total` = ifelse( n>0 , percent( total / Total , accuracy = .01 ) , -Inf ) ,
      n = comma( n ) ,
      total = comma( total ),
      Max = comma( Max )
    )   %>%
    select( -N, -Total ) %>%
    rename( `Total Value` = total ,
            `Error Flag` = err ) %>%
    mutate_all( as.character ) 
  
  
  rdt.outlier.summary[ rdt.outlier.summary == -Inf] = "-"
  
  options(warn) # return to normal warnings 
  
  return(  rdt.outlier.summary )
}



          