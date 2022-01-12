# Clean TS data

clean_ts = function( x , 
                     tsibble_var = NULL , 
                     .lambda = .5 , 
                     .clean = 'tsclean' , #MAD, arima
                     interpolate = TRUE ,
                     predict = FALSE , 
                     pred_data = 6 , 
                     arima.list = FALSE , # if arima, returns fit, resid, and interp
                     MAD = 5 , 
                     ignoreSmall = TRUE , 
                     smallThreshold = 50 ,
                     .pb = NULL ,
                     timing = FALSE ,
                     ... ){ 
    # print('X'); print( x )
    # glimpse(x) 
    # print('MAD')
    
    
    if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
    } 
    
    if ( is_tsibble( x ) ){
      x. = x %>% pull( !!sym( tsibble_var ) )
    } else { x. = x }

    # If all values < 50, do not impute or look for outliers; process unreliable
    if ( ignoreSmall & (all( x. < smallThreshold | is.na( x. ) ) ) ) return( x )

   if ( .clean %in% 'MAD') {
    
     # If all values < 50, do not impute or look for outliers; process unreliable
      if ( ignoreSmall & (all( x. < smallThreshold | is.na( x. ) ) ) ) return( x )
      
      # pre clean extreme values
      medianVal = median( x. , na.rm = TRUE )
      medianAbsDeviation = mad( x. , na.rm = TRUE )
      # se <- function(x) sqrt( var(x , na.rm = TRUE )/ length( !is.na(x) ))
      # sdVal = sd( x , na.rm = TRUE )
      
      extreme = x. > ( medianVal + MAD * medianAbsDeviation ) |
          x. < ( medianVal - MAD * medianAbsDeviation )
      x.[ extreme ] = NA 
 
   }
  
    # convert to time-series
    x.ts = x. %>% as.ts( . , frequency = 12 ) 
    
    clean = x.
    
    # When lambda = .5, very fast. 2 sec.  No errors.
    ## lambda = 'auto' much slower (3 min) and yields errors
    # series with mostly low values have all outliers
    
    if ( .clean %in% 'tsclean' & ( sum( !is.na( x.ts ) ) > 2 ) ) {
                 
        # print( x.ts )
        
        if ( interpolate ){
            
            x.ts  = 
                
            forecast::tsclean( x.ts ,
                               replace.missing = interpolate ,
                               lambda = .lambda ) %>%
                as.integer()
            
            } else { 
                
                # returns index of outlier
             out = 
                # possibly(
                    forecast::tsoutliers( x.ts , lambda = .lambda )
                    # , otherwise = NA )
            
                if ( !all( is.na( out ) )  ){
    
                        # cat( paste( 'length of out is' , length( out$replacements ) ))
                        # cat( x.ts )
                        # cat( paste( 'original is' , x.ts[ out$index ]  ) )
                        # cat( paste( 'replacement is' , out$replacements  ))
    
                        x.ts[ out$index ] =  NA
                }
            }
                
                # print( x.ts )
                clean = x.ts
            # )
    }
    
    if (.clean %in% 'tso' & ( sum( !is.na( x.ts ) ) > 2 ) ) {
      
        out = tsoutliers::tso( x.ts , tsmethod = "auto.arima" ) 

        
        if ( !all( is.na( out ) )  ){
    
                        # cat( paste( 'length of out is' , length( out$replacements ) ))
                        # cat( x.ts )
                        # cat( paste( 'original is' , x.ts[ out$index ]  ) )
                        # cat( paste( 'replacement is' , out$replacements  ))
        
        clean = out$yadj
        }
    }
    
    # ARIMA
    if ( .clean %in% 'arima'  & ( sum( !is.na( x.ts ) ) > 24 ) 
         ){
      
      if (timing ) tic()
      
      if ( is_tsibble( x ) ){ 
        x.ts = x %>% select( -{{tsibble_var}} )
        x.ts$value = x %>% pull( tsibble_var ) 
        
      } else { x.ts = as_tsibble( x.ts ) }
      
      
      # This model not good if there is an outlier
        m = x.ts %>%
            model( arima = ARIMA( fabletools::box_cox( value , lambda = .5  ) ~
            # model( arima = ARIMA( log( value + 1 ) ~
            # model( arima = ARIMA( value ~
                                    pdq( 0:2, 0:1, 0:2 ) +
                       PDQ( 0:2, 0:1, 0:2 ,  period = 12 )  )
            )
        
      if (timing ) print( 'arima model') ;  toc()
        
      fit = NA; resid = NA ; innov = NA;  interp=NA ; pred = NA 
      
      if ( !all( is.na( m ) )  ){
        
        if (timing ) tic()
        
        m.ts = m %>% augment()
        fit = m.ts$.fitted  %>% as.integer()
        resid = m.ts$.resid
        innov = m.ts$.innov

        # extreme = extremely( resid, 10 )
        # clean[ extreme ] = NA 
     
        if (timing ) print( 'arima augment' ); toc() 
     
        if ( interpolate ){
          
          if (timing ) tic()
          
                if ( timing ) tic()
                m.i = interpolate( m , x.ts )
                interp = m.i$value %>% as.integer()
                if (timing ) toc()
                
                if ( arima.list ) return( list( fit, resid, interp ) )
                clean = interp 
                
          if (timing ) print( 'arima predict' ) ; toc() 
        }
        
        if ( predict ){
            
          if (timing )tic()
          
          if ( is.numeric( pred_data ) ){
              m.p = m %>% fabletools::forecast( h = pred_data )
              clean = m.p$.mean %>% as.integer()
            }
          
          if ( is_tsibble( pred_data ) ){
            m.p = m %>% fabletools::forecast( new_data = pred_data )
            
            if ( '.mean' %in% names( m.p ) ){
              
              clean = bind_rows( m.ts , 
                                 m.p %>% as_tsibble %>% 
                                   select(-.model, -value) %>% 
                                   rename( pred = .mean)
              )
            } else {
              clean = m.ts
            }
            
           
          }
            
          if (timing ) print( 'arima predict' ) ; toc() 
        }
        
      }
        
    } 
    
    # ETS
    if ( .clean %in% 'ets'  & ( sum( !is.na( x.ts ) ) > 24 ) 
         ){
      
      if (timing ) tic()
      
      if ( is_tsibble( x ) ){ 
        x.ts = x %>% select( -{{tsibble_var}} )
        x.ts$value = x %>% pull( tsibble_var ) 
        
      } else { x.ts = as_tsibble( x.ts ) }
      
      
      # This model not good if there is an outlier
        m = x.ts %>%
            model( ets = ETS( fabletools::box_cox( value , lambda = .5  ) )
            )
        
        if (timing ) print( 'ETS model' ); toc()
        
      fit = NA; resid = NA ; innov = NA;  interp=NA ; pred = NA 
      
      if ( !all( is.na( m ) )  ){
        
        if (timing ) tic()
        
        m.ts = m %>% augment()
        fit = m.ts$.fitted  %>% as.integer()
        resid = m.ts$.resid
        innov = m.ts$.innov

        # extreme = extremely( resid, 10 )
        # clean[ extreme ] = NA 
     
        if (timing ) print( 'ETS augment' ); toc()
     
        if ( interpolate ){
                if ( timing ) tic()
                m.i = interpolate( m , x.ts )
                interp = m.i$value %>% as.integer()
                if (timing ) toc()
                
                if ( arima.list ) return( list( fit, resid, interp ) )
                clean = interp 
        }
        
        if ( predict ){
            
          if (timing ) tic()
          
          if ( is.numeric( pred_data ) ){
              m.p = m %>% fabletools::forecast( h = pred_data )
              clean = m.p$.mean %>% as.integer()
            }
          
          if ( is_tsibble( pred_data ) ){
            m.p = m %>% fabletools::forecast( new_data = pred_data )
            
            if ( '.mean' %in% names( m.p ) ){
              
              clean = bind_rows( m.ts , 
                                 m.p %>% as_tsibble %>% 
                                   select(-.model, -value) %>% 
                                   rename( pred = .mean)
              )
            } else {
              clean = m.ts
            }
            
           
          }
          
          if (timing ) print(  'ETS predict' ); toc()   
            
        }
        
      }
        
    } 
    
  # PROPHET
  if ( .clean %in% 'prophet'  
       # & ( sum( !is.na( x.ts ) ) > 24 ) 
  ){
    
    if (timing ) tic()
    
    if ( is_tsibble( x ) ){ 
      x.ts = x %>% select( -{{tsibble_var}} )
      x.ts$value = x %>% pull( tsibble_var ) 
      
    } else { x.ts = as_tsibble( x.ts ) }
    
    clean = x.ts
   
    if ( sum( !is.na( x.ts ) ) > 24 ){
      m = x.ts %>%
        model( prophet = prophet( box_cox( value , lambda = .5  ) ~ 
                                    season(period=12, 
                                           order = 4 ,
                                           type='multiplicative') ,
                                 seed = TRUE 
               ))

    if (timing ) print( 'Prophet model' ); toc()
    
    fit = NA; resid = NA ; innov = NA;  interp=NA ; pred = NA 
    
    # if ( !all( is.na( m ) )  ){
      
      if (timing ) tic()
      
      m.ts = m %>% augment()
      fit = m.ts$.fitted  %>% as.integer()
      resid = m.ts$.resid
      innov = m.ts$.innov
      
      clean = m.ts 
      # extreme = extremely( resid, 10 )
      # clean[ extreme ] = NA 
      
      if (timing ) print( 'prophet augment' ); toc()
      
      if ( interpolate ){
        if ( timing ) tic()
        m.i = interpolate( m , x.ts )
        interp = m.i$value %>% as.integer()
        if (timing ) toc()
        
        if ( arima.list ) return( list( fit, resid, interp ) )
        clean = interp 
      }
      
      if ( predict ){
        
        if (timing ) tic()
        
        if ( is.numeric( pred_data ) ){
          m.p = m %>% fabletools::forecast( h = pred_data )
          clean = m.p$.mean %>% as.integer()
        }
        
        if ( is_tsibble( pred_data ) ){
          m.p = m %>% fabletools::forecast( new_data = pred_data )
          
          if ( '.mean' %in% names( m.p ) ){
            
            clean = bind_rows( m.ts , 
                               m.p %>% as_tsibble %>% 
                                 select(-.model, -value) %>% 
                                 rename( pred = .mean)
            )
          } else {
            clean = m.ts
          }
          
          
        }
        
        if (timing ) print(  'prophet predict' ); toc()   
        
      }
      
    }
      # if ( arima.list ) return( list( fit = fit, resid = resid , 
      #                                 innov = innov, interp = interp, 
      #                                 pred = pred  ) )
    
  } 
          
    return( clean )
}

extremely = function( x, MAD = 5 ){
  x = abs(x)
  median = median( x , na.rm = TRUE )
  medianAbsDeviation = mad( x , na.rm = TRUE )
  extreme = x > ( median + MAD * medianAbsDeviation ) |
    x < ( median - MAD * medianAbsDeviation )
  return( extreme )
}
