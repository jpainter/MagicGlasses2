

model_ts = function( x , 
                     tsibble_var = NULL , 
                     .lambda = 1 , 
                     .model = 'arima' , #MAD, arima
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
    
    if ( .model %in% 'arima'){
        m =  m = x  %>%
            model( arima = ARIMA( fabletools::box_cox( value , lambda = 1  ) ~
                                      # model( arima = ARIMA( log( value + 1 ) ~
                                      # model( arima = ARIMA( value ~
                                      pdq( 0:2, 0:1, 0:2 ) +
                                      PDQ( 0:2, 0:1, 0:2 ,  period = 12 )  )
            )
    }
    
    return( m )
}