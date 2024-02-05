# Cleaning.R

    outlier.dataset = function( data1, startingMonth , endingMonth  ){

      # req( outlierData$df_data )
      req( data1() )
      req( input$startingMonth )
      req( input$endingMonth )
      req( period() )

      cat( '\n* cleaning_widget outlier.dataset():')
      # if ( is.null( outlierData$df_data ) ){
      #   cat( '\n - is.null( outlierData$df_data )' )
      #   outlierData$df_data  = data1()
      # }

      # d = outlierData$df_data
      
      # filter date
      # d = d %>% 
      #   filter( 
      #     Month >= yearmonth( input$startingMonth ) , 
      #     Month <= yearmonth(input$endingMonth )
      #     )
      # d. = as.data.table( outlierData$df_data )
      
      d. = as.data.table( data1() )
   
      cat( '\n - period():' , period() ) 
      # cat( "\n - d. class/cols: \n -- ", class( d. ) , "\n -- ", names( d. ))
      
      # testing
      # saveRDS( d. , "d..rds" )
      
      if ( period() %in% 'Month' ){
        
          # d = d.[ which( 
          #   Month >= yearmonth( input$startingMonth ) & Month <= yearmonth(input$endingMonth ) ) ,] %>%
          #   as_tibble
          
          d = d. %>% filter( Month >= yearmonth( input$startingMonth ) & Month <= yearmonth(input$endingMonth ) ) %>%
            as_tibble
   
          cat('\n - period is month' )
      }
      
      if ( period() %in% 'Week' ){
        
          d = d.[ which( 
            Week >= yearweek( input$startingMonth ) & Week <= yearweek(input$endingMonth ) ) ,] %>%
            as_tibble
   
          cat('\n - period is week' )
      }
      

      if ( 'mad10' %in% names(d) ) cat('\n - data has mad10' )
      if ( 'seasonal3' %in% names(d) ) cat('\n - data has seasonal3' )

      if ( 'effectiveLeaf' %in% names( d ) && input$selectOrgType %in% 'Facilities only'){
        
        cat('\n - data has effectiveLeaf; facilities only' )
        
        d = setDT( d )[ effectiveLeaf == TRUE , ]
        # d = d %>% filter( effectiveLeaf )
        
        
      } else if ( input$selectOrgType %in% 'Admin only' ){
        cat('\n - Admin only' )
        d = setDT( d )[ effectiveLeaf != TRUE , ]
        # d = d %>% filter( effectiveLeaf )
      }

      # Filter by region/level
      # level2
      if ( !is_empty( input$level2 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[2] , "=" , input$level2 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )
        d = setDT( d )[ base::get( levelNames()[2])  %in%   input$level2  ,, ]
      }

      # level3
      if ( !is_empty( input$level3 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[3] , "=" , input$level3 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )
        d = setDT( d )[ base::get( levelNames()[3])  %in%   input$level3  ,, ]
      }

      # level4
      if ( !is_empty( input$level4 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[4] , "=" , input$level4 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )
        d = setDT( d )[ base::get( levelNames()[4])  %in%   input$level4  ,, ]
      }

      # level5
      if ( !is_empty( input$level5 ) ){
        cat(  '\n - filtering outlier data by' , levelNames()[5] , "=" , input$level5 )
        # d = d %>%
        #   filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )
        d = setDT( d )[ base::get( levelNames()[5])  %in%   input$level5  ,, ]
      }


      # filter dataElement
      if ( input$dataElement %in% 'All'){
        d = d %>% as_tibble()
      } else {
        d = setDT( d )[ data %in% input$dataElement , ] %>%  as_tibble()
        # %>%  filter( data %in% input$dataElement )
      }
      


      cat( '\n - done')
      return( d )
      }

    
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
        
        # WHO
        outlier.sd = abs( (x.ts - mean( x.ts , na.rm = T) ) / sd( x.ts , na.rm = T) ) >= deviation
        
        # MAD/JP
        outlier.mad = abs( (x.ts - median( x.ts , na.rm = T) ) / mad( x.ts , na.rm = T) ) >= deviation
        
        # MASE/ROb Hyndeman
        outlier.mase= abs( (x.ts - median( x.ts , na.rm = T) ) / mad( x.ts , na.rm = T) ) >= deviation
            
        x.ts[ which( outlier ) ] =  NA
        # value[ outlier ] =  FALSE 
      
      if ( logical ) return( outlier ) # FALSE = outlier
      return( x.ts )
}

fitValue = function( x ,  
                       smallThreshold = 100 , 
                       deviation = 3 ,
                       logical = FALSE ,
                       interpolate = FALSE , # only useful when logical is FALSE
                       .lambda = 1 , 
                       .pb = NULL ,
                       .progress = FALSE ,
                        total = NA,
                     model = 'prophet'   ){
  
   if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
                 
      cat( "\n - x is ", class(x) )
      
      .lam = forecast::BoxCox.lambda( as.numeric(x$original) , method = "guerrero", lower = 0.1 )
      
      cat( "\n - .lam is ", .lam )
        
      x.fitted  = x %>% 
        mutate( original = as.numeric( original ) ) %>%
        model( p = prophet( box_cox( original , lambda = 1  ) ) )  %>% 
        fitted %>% 
        pull( .fitted )
        
      
      # if ( logical ) return( outlier ) # FALSE = outlier
      return( x.fitted )
}


# MASE function borrowed from Metrics package, modified with sum na.rm = TRUE
abs_ae = function (actual, predicted){
  return(abs(actual - predicted))
  
}

mase = function (actual, predicted, step_size = 1 ){
  
    if ( all( is.na( predicted ) ) ) return( as.numeric( NA ) )
  
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

d.mase = function( d , selectedOUs ){
  cat( "\n * Cleaning.R d.mase ")
  
        # data.table?  
        d.mase = setDT( d )[ 
          # year( Month ) == 2020
          ,
          .(   MASE = mase( actual = original, predicted = expected  ) ,
               n = sum( !is.na( original ) ) ,
               expected = sum( expected, na.rm = TRUE ) ,
               actual = sum( original, na.rm = TRUE ) ,
               Selected = fifelse( orgUnit %in% selectedOUs , 
                                                      'Reporting Consistently',
                                                      'Inconsistent Reporting')
          ) ,
          by = c(  'orgUnit', 'orgUnitName' , 'data.id' , 'data' ) ] %>%
          
          as_tibble() %>%
          group_by( orgUnit, orgUnitName , data.id , data ) 
        
        maxN = max( d.mase$n )
        
        # Testing
        # saveRDS( data1(), "data1.rds")
        # saveRDS( d.mase , "d.mase.rds" )

        # maxN = max( d.mase.ou$n.max )
        
        d.mase = d.mase %>%
          mutate( 
            catMASE = case_when(
              MASE == 0  ~ "0" ,
              MASE <= .25 ~ "0_25" ,
              MASE <= .50 ~ "25_50" ,
              MASE <= 1.00 ~ "50_100" ,
              MASE > 1.00 ~ "100+" ,
              is.na( MASE ) ~ 'NA'
            ) 
            # , catN = case_when(
            #   # n == maxN ~ "Reporting = 100%" ,
            #   orgUnit %in% selectedOUS() ~ "Reporting Consistently" ,
            #   TRUE ~ "Inconsistent Reporting"
            # )
          )
        
        
        return( d.mase )
  
}

mase.summary = function( d.mase ){
  cat( "\n * Cleaning.R mase.summary ")
  d.mase[ !mase.zero , ] %>% 
          # na.omit() %>%
          group_by( Selected , orgUnit) %>%
          summarise( MASE = median( MASE , na.rm = T ) ) %>%
          group_by( Selected ) %>%
          summarise( mean.mase = mean( MASE  , na.rm = T ) , 
                     sd.mase = sd( MASE  , na.rm = T  ),
                     facilities = n())
}

mase.summary.around.cutpoint = function( d.mase , mase.cutpoint = 0.4 ){
  
  cat( "\n * Cleaning.R mase.breaks ")
  
  mase.zero = d.mase$MASE == 0L
          
  d.mase[ !mase.zero , ] %>% 
          na.omit() %>%
          group_by( Selected , orgUnit) %>%
          summarise( MASE = median( MASE , na.rm = T ) ) %>%
          # inner_join( mase.summary , by = "Selected" ) %>%
          mutate( model.error = ifelse( MASE > mase.cutpoint , 'High' , 'Low')) %>%
          group_by( Selected, model.error ) %>%
          summarise( mean.mase = mean( MASE  , na.rm = T ) ,
                     facilities = n() )
        
}

mase.summary.plot = function(d.mase , mase.cutpoint = 0.4 ){
      cat( "\n * Cleaning.R mase.summary.plot ")
  
      mase.zero = d.mase$MASE == 0L
      
      mase.summary = mase.summary.around.cutpoint( d.mase, mase.cutpoint )
      
      g1 = ggplot( na.omit(d.mase[ !mase.zero  , ]) , aes( x = MASE ) ) + 

            geom_histogram( binwidth = .025, fill = 'white', color = 'black' ) +
            geom_vline( 
                        # data = na.omit( mase.summary ), 
                        # aes( xintercept = mean.mase ),
                        xintercept = mase.cutpoint , 
                        color = 'dark blue') +
            geom_text( data = mase.summary %>% filter( model.error == "High") , 
                       aes( x = mean.mase , y = Inf , 
                                                 label = paste( facilities, 'facilities\nmean =',
                                                                percent( mean.mase , accuracy = 1))  ,
                                                 hjust = 0 , vjust = 1.25 ))  + 
              geom_text( data = mase.summary %>% filter( model.error == "Low")  , 
                         aes( x = mean.mase , y = Inf , 
                                                 label = paste( facilities, 'facilities\nmean =',
                                                                percent( mean.mase , accuracy = 1)) ,
                                                 hjust = 0 , vjust = 1.25 ))  + 
            facet_wrap( ~ Selected ) +
            labs( titles = "Mean absolute scale error (MASE)" ,
                  y = 'Number of Facilities' , caption = 'Note: MASE calculated among reported values; it does not account for missing values')
           
        g1        
        
        # g2 = ggplot( na.omit(d.mase[ !mase.zero  , ]) , aes( x = MASE ) ) + 
        # geom_boxplot() +
        # facet_wrap( ~ Selected ) 
        # 
        # patchwork::plot_layout( g1 /g2 )
        
}

mad_outliers = function( d ,
                              .total = NULL , 
                              .threshold = 50,
                              key_entry_errors = NULL,
                              progress = TRUE){
  
  if ( is.null( key_entry_errors ) ){
    
    # cat( '\n - mad_outliers: Scanning for repetive key entry errors')
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
                                       .progress = progress ,
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
                              .threshold = 50,
                              progress = FALSE ){
  
        data1.seasonal = d %>%  
        group_by( orgUnit, data.id ) %>%
        mutate(
          
          expected = unseasonal(  ifelse( ! mad10, original , NA) , 
                                  smallThreshold = .threshold * 2  , 
                                  logical = FALSE , # Returns forecasted value
                                  .progress = progress ,
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


outlier.summary.tibble = function(
    data = NULL ,
    cols = c( "key_entry_error", "over_max" , 'mad15', 'mad10', 
              # 'mad5',
              'seasonal5' , 'seasonal3',
              'expected') ,
    .print = TRUE 
){
  
  if ( .print ) cat( '\n * Cleaning.R outlier.summary.tibble ')
  
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
      `%N` = ifelse( n>0 , percent( n / N , accuracy = .01 ) , -Inf ) ,
      `%Total Value` = ifelse( n>0 , percent( total / Total , accuracy = .01 ) , -Inf ) ,
      N = comma( n ) ,
      `Total Value` = comma( total ),
      `Largest Value` = comma( Max )
    )   %>%
    select( err,  N, `%N` , `Total Value`, `%Total Value` , `Largest Value` ) %>%
    rename( 
            `Error Flag` = err ) %>%
    mutate_all( as.character ) 
  
  
  rdt.outlier.summary[ rdt.outlier.summary == -Inf] = "-"
  
  options(warn) # return to normal warnings 
  
  return(  rdt.outlier.summary )
}



          