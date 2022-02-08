# Cleaning.R
extremely_mad = function( x , 
                          deviation = 15, 
                          smallThreshold = 50 , 
                          key_entry_error = NA , 
                          maximum_allowed = NA , 
                          logical = FALSE ,
                          .pb = NULL
                           ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
  
  # Remove any value above maximum_allowed or key error
  if ( !is.na( maximum_allowed ) ){
    over_max = x > maximum_allowed 
  } else { over_max = !x>=0 }
  
  # Remove key entry errors
  if ( !all(is.na( key_entry_error )) ) {
    key_error =  x>=0 & x %in% key_entry_error$original 
  } else { key_error = !x>=0 }
  
  y = x
  y[ over_max | key_error ] = NA 
   
  if ( (all( y <= smallThreshold | is.na( y ) ) ) ) {
      if ( logical ) return( ! ( over_max | key_error ) )
      return( y )    
  }
  
      # pre clean extreme values
      medianVal = median( y , na.rm = TRUE )
      medianAbsDeviation = mad( y , na.rm = TRUE )
      # se <- function(x) sqrt( var(x , na.rm = TRUE )/ length( !is.na(x) ))
      # sdVal = sd( x , na.rm = TRUE )
      
      extreme = x > ( medianVal + deviation * medianAbsDeviation ) |
          x < ( medianVal - deviation * medianAbsDeviation )
      if ( logical ) return( !extreme & !( over_max | key_error ) ) # FALSE = outlier
      y[ extreme ] = NA 
      return( y )
}

unseasonal = function( x ,  
                       smallThreshold = 100 , 
                       deviation = 3 ,
                       logical = FALSE ,
                       interpolate = FALSE , # only useful when logical is FALSE
                       .lambda = 1 , 
                       .pb = NULL ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 

    value = ifelse( !is.na(x), TRUE , NA ) 
    
    if (( sum( !is.na( x ) ) <= 1  ) | (all( x <= smallThreshold | is.na( x ) ) )){
      if ( logical ) return( value ) 
      return( x )
    } 
                 
      x.ts = x %>% as.ts( . , frequency = 12 ) 
        
        x.forecast  = 
                
            forecast::tsclean( x.ts ,
                               replace.missing = TRUE ,
                               lambda = .lambda ) %>%
                as.integer()
            
        MAD = mad( x , na.rm = TRUE )
        # standard_dev = sd( x, na.rm = TRUE )
            
        outlier = abs( ( x.forecast - x.ts ) / MAD ) >= deviation
            
        x.ts[ which( outlier ) ] =  NA
        value[ outlier ] =  FALSE 
      
      if ( logical ) return( value ) # FALSE = outlier
      return( x.ts )
}


