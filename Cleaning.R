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

# mad_outliers <- function(x, threshold = 3.5, eps = 1e-6) {
#           med <- median(x, na.rm = TRUE)
#           mad_val <- mad(x, constant = 1, na.rm = TRUE)
#           mad_val <- ifelse(mad_val == 0, eps, mad_val)
#           mod_z <- 0.6745 * (x - med) / mad_val
#           abs(mod_z) > threshold
# }
# 
# iqr_outliers <- function(x) {
#   q1 <- quantile(x, 0.25, na.rm = TRUE)
#   q3 <- quantile(x, 0.75, na.rm = TRUE)
#   iqr <- q3 - q1
#   lower <- q1 - 1.5 * iqr
#   upper <- q3 + 1.5 * iqr
#   x < lower | x > upper
# }
# 
# tail_probs <- function(x, mean = 0, sd = 1) {
#   upper_tail <- pnorm(mean + x * sd, mean, sd, lower.tail = FALSE)
#   lower_tail <- pnorm(mean - x * sd, mean, sd, lower.tail = TRUE)
#   both_tails <- upper_tail + lower_tail
#   tibble(
#     x = x,
#     `P(X > mean + x*sd)` = upper_tail,
#     `P(X < mean - x*sd)` = lower_tail,
#     `P(|X - mean| > x*sd)` = both_tails
#   )
# }
# 
# quantile_outliers <- function(x, lower = 0.05, upper = 0.95) {
#   lo <- quantile(x, lower, na.rm = TRUE)
#   hi <- quantile(x, upper, na.rm = TRUE)
#   x < lo | x > hi
# }

# robust_z_trimmed <- function(x, trim = 0.01) {
#   m <- mean(x, trim = trim, na.rm = TRUE)
#   s <- sd(x[x > quantile(x, trim, na.rm = TRUE) & 
#               x < quantile(x, 1 - trim, na.rm = TRUE)], 
#           na.rm = TRUE)
#   z <- (x - m) / s
#   z
# }
# 
# detect_outliers_trimmed <- function(x, threshold = 3, trim = 0.01) {
#   abs( robust_z_trimmed(x, trim) ) > threshold
# }

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
  # TRUE means that it fails the test.  FALSE is ok. 
  # if ( ! all( is.na( over_max ) ) ){
  #    if ( all( !is.na( maximum_allowed ) ) ){
  #       over_max =  x > maximum_allowed 
  #     } else { over_max = NA }
  # }
  # 
  # # Remove key entry errors
  # TRUE means that it fails the test.  FALSE is ok. 
  # if ( !all( is.na( key_entry_error )) ) {
  #   key_error = ( x>=0 & x %in% key_entry_error )
  # } else { key_error = NA }
  
  
  y = x
  # y[  over_max | key_error  ] = NA
  
  if ( !is.null( smallThreshold ) && all( y <= smallThreshold | is.na( y ) )  ) {
      if ( logical ) return( rep( NA, length(y) )  )
      return( y )
  }
  
      # MAD
      medianVal = median( y , na.rm = TRUE )
      medianAbsDeviation = mad( y , na.rm = TRUE )
    
      if ( medianAbsDeviation < .01 * medianVal ){
        # When values are small, many values may equal median and then the mad is zero, 
        # in those cases, use trimmed sd times 0.6745 
        # The constant  makes MAD comparable to SD under the normal distribution (since MAD ≈ 0.6745 × SD).
        
        medianAbsDeviation = 0.6745 * sd(x[x > quantile(x, .01, na.rm = TRUE) & 
              x < quantile(x, 1 - .01, na.rm = TRUE) ], 
          na.rm = TRUE)
  
      }
      
      extreme = y > ( medianVal + deviation * ( medianAbsDeviation ) ) |
          y < ( medianVal - deviation * ( medianAbsDeviation ) )
 
      if ( logical ) return( extreme  ) 
      y[ !extreme ] = NA 
      return( y )
}

unseasonal = function( x ,  
                       smallThreshold = 100 , 
                       deviation = 3 ,
                       logical = FALSE ,
                       interpolate = FALSE , # only useful when logical is FALSE
                       .lambda =  .5 , 
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

    # skip if all NA
    if ( all( is.na( x ))) {
      if ( logical ) return( rep( NA, length(x) )  )
      return( x )
    }
  
    # skip if all small                                
    if ( !is.null( smallThreshold ) && (all( x <= smallThreshold | is.na( x ) ) )){
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
        # outlier.sd = abs( (x.ts - mean( x.ts , na.rm = T) ) / sd( x.ts , na.rm = T) ) >= deviation
        
        # MAD/JP
        # outlier.mad = abs( (x.ts - median( x.ts , na.rm = T) ) / mad( x.ts , na.rm = T) ) >= deviation
        
        # MASE/ROb Hyndeman
        # outlier.mase= abs( (x.ts - median( x.ts , na.rm = T) ) / mad( x.ts , na.rm = T) ) >= deviation
            
        x.ts[ which( outlier ) ] =  NA
        # value[ outlier ] =  FALSE 
      
      if ( logical ) return( outlier ) # FALSE = outlier
        
      return( x.ts )
}

detect_unseasonal_series <- function( df, 
                                   value_col = original, 
                                   index_col = Month,
                                   deviation = 3,
                                   method = "sd" , # or "mad"
                                   .lambda =  .5 ) {

  # print( cat( "\n - detect_unseasonal_series - ") )
  
  value_col <- enquo( value_col )
  index_col <- enquo( index_col )
  outlier_name <- sym(paste0( "seasonal" , deviation))

  missing = is.na(df[[ rlang::as_name( value_col )]])
  
  # print( cat( "\n ", sum(missing) , nrow( df ) , "values"))
  
  # Check for enough non-missing data
  if ( nrow(df) < 24 || sum( !missing ) < 12) {
    # df$outlier <- NA
    # print( cat("\n - no outlier: not enough values") )
    return(df)
  }
  
    # Fill missing months
    df. <- df %>%  fill_gaps(.full = TRUE)
    
    # recalculate missing
    missing. = is.na(df.[[ rlang::as_name( value_col )]])
 
    # Impute missing values using forecast::na.interp (must convert to ts object temporarily)
    df. <- df. %>%
    mutate( 
      
      not_mad = {
      x <- ts( not_mad , frequency = 12)
      as.numeric(forecast::na.interp( x , lambda = .5)) #sqr root transform keeps values positive
      
      # not_mad = {
      # x <- ts( not_mad, frequency = 12 )
      # as.numeric(forecast::na.interp(x, lambda = .5 ))
    })


  
  
  # Apply STL decomposition using feasts
  tryCatch({
    # print( cat("\n - sum", sum( df.$not_mad, na.rm = T)))
    # saveRDS( df. , "df..rds")
    
    stl_components <- df. %>%
      model(stl = STL( not_mad ~ season(window = "periodic"))) %>%
      fabletools::components()

    residuals <- stl_components$remainder

    # print( cat("\n - residuals", sum( residuals, na.rm = T)))
    
    if ( method == "mad" ) scale_val <- mad( residuals, na.rm = TRUE )
    if ( method == "sd" )  scale_val = sd( residuals, na.rm = TRUE )
    threshold_val <- deviation * scale_val

    # Mark outliers in original (non-gap-filled) data
    df. <- df. %>%
      mutate( !!outlier_name := abs(residuals) > threshold_val )

    # df. = df.[ !missing. , ]

    # print( cat("\n - outlier") )
    return(df.)
    
  }, error = function(e) {
    df <- df %>%
      mutate(!!outlier_name := NA) 
    # print( cat("\n - no outlier") )
    return( df )
  })

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
                                                      'Champion',
                                                      'Non-Champion')
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
                              # .total = NULL , 
                              .threshold = 50,
                              key_entry_errors = NULL,
                              progress = TRUE){
  .total = n_keys( d )
  
  if ( is.null( key_entry_errors ) ){
    
    # cat( '\n - mad_outliers: Scanning for repetive key entry errors')
    # starting with values having at least 4 numbers
    large_values =
      count( as_tibble( d %>%
                          filter( nchar(original) > 3 ,
                                  effectiveLeaf ) ) %>% ungroup ,
             original ) %>%
      arrange(-n)
    
    # find values where the number happens at least 3 > than
    # median of the top 10 rows
    key_entry_errors = large_values %>%
      filter(  n > 3 * median(
        large_values %>% filter( row_number()<11 )  %>%
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
                 
                key_entry_error = ifelse( !is.na( key_entry_errors ) & !is.na( original ) , 
                                          original %in% key_entry_errors , NA ) ,
              
                over_max = ifelse( !is.na( .max ) & !is.na( original ) , original > .max, NA ) ,
                
                AllSmall = ifelse(  !is.null( .threshold) &&
                                      ( all( is.na( original ) |  original <= .threshold ) ) ,
                                    TRUE , FALSE ) ,
                
                not_key_or_over_under = ifelse( ( is.na( over_max) | !over_max ) &
                                                 ( is.na( key_entry_error ) | !key_entry_error ) &
                                                  ! AllSmall  ,
                                                        original , NA )  ,
                
                , mad15 = extremely_mad( not_key_or_over_under ,
                                       deviation = 15 ,
                                       smallThreshold = .threshold ,
                                       key_entry_error = key_entry_error ,
                                       over_max = over_max ,
                                       maximum_allowed = .max ,
                                       logical = TRUE,
                                       .progress = progress ,
                                       total = .total )

                , mad10 = extremely_mad( not_key_or_over_under ,
                                         deviation = 10 ,
                                         smallThreshold = .threshold ,
                                         maximum_allowed = .max ,
                                         logical = TRUE ,
                                         .progress = FALSE )

                , mad5 = extremely_mad( not_key_or_over_under ,
                                         deviation = 5 ,
                                         smallThreshold = .threshold ,
                                         maximum_allowed = .max ,
                                         logical = TRUE ,
                                         .progress = FALSE )
            )
   
   return( m_o )
}

seasonal_outliers = function( d ,
                              .total = NULL , 
                              .threshold = 50,
                              mad = "mad10",
                              tests = c( 'seasonal3' , 'seasonal5') ,
                              progress = FALSE ){
  
        mad = rlang::sym( mad )
  
        if ( progress ) { 
          # handlers( global = TRUE )  # Enable console progress bar
          
          handlers(handler_progress(format = "  [:bar] :current/:total (:percent) Elapsed: :elapsed ETA: :eta", 
                          clear = FALSE, 
                          width = 60))
          
          # progressr::handlers("debug")
          
          # Extract key and index names from parent
          key_cols <- key_vars(d)
          index_col <- index_var(d)
    
          with_progress({
            p <- progressor( along = 1:n_keys(d)  )
  
            data1.seasonal = d %>%  
              group_by_key( .drop = TRUE ) %>%
              group_modify( 
                
                function( .x, .y ){ 
                  
                  # advance handler unless it has shutdown.
                  if (exists( "p" , mode = "function")) try( p(), silent = TRUE)
                
                  # Explicitly add the key columns from .y back into .x
                  .x <- bind_cols(.y[rep(1, nrow(.x)), ], .x) %>% 
                    select( key_vars(d), index_var(d) , original, {{mad}}) 
              
                  # Reconstruct tsibble (after key columns are added)
                  .x <- as_tsibble(.x, key = key_vars(d), index = index_var(d), validate = FALSE)

                  
                  .x = mutate( 
                    .x , 
                    
                    not_mad = ifelse( ! {{ mad }}, original , NA) 
                    
                    # , expected = unseasonal(  not_mad ,
                    #                         smallThreshold = .threshold   ,
                    #                         logical = FALSE , # Returns forecasted value
                    #                         .progress = progress ,
                    #                          total = .total
                    #                         )
                    
                    , seasonal5 = if ( 'seasonal5' %in% tests ) unseasonal(  not_mad  ,
                                            smallThreshold = .threshold   ,
                                            deviation = 5 ,
                                            .pb = NULL ,
                                            .progress = FALSE ,
                                            logical = TRUE )

                    , seasonal3 = if ( 'seasonal3' %in% tests )  unseasonal(  not_mad ,
                                            smallThreshold = .threshold  ,
                                            deviation = 3 ,
                                            .pb = NULL ,
                                            .progress = FALSE ,
                                            logical = TRUE )
                    
                    # ,ts.seasonal =  tsoutliers( not_mad, lambda = .lambda )
                    )
                  
                  .x = as_tibble( .x ) %>% ungroup %>% select( -key_vars(d) )
                  
                  return( .x )
                  } )
          }) 
          
          
        } else {
          # No progress bar
          data1.seasonal <- d %>%
            group_by_key() %>%
            group_modify(~ {
              .x <- mutate(
                .x,
                
                not_mad = ifelse( ! {{ mad }}, original , NA) ,
                
                seasonal5 = unseasonal(
                  not_mad,
                  smallThreshold = .threshold,
                  deviation = 5,
                  logical = TRUE
                ),
                
                seasonal3 = unseasonal(
                  not_mad,
                  smallThreshold = .threshold,
                  deviation = 3,
                  logical = TRUE
                )
              )
        
            return(.x)
              })
  }
        
    return( data1.seasonal )
}


detect_seasonal_outliers <- function( data, 
                                value_col = not_mad , 
                                exclude = mad5 ,
                                deviation = 3, 
                                method = "mad" , #or "sd"
                                threshold = 50){

  value_col <- enquo( value_col )
  exclude <- enquo( exclude )
  key_cols <- key_vars( data )
  index_col <- index_var( data )

  n_series <- n_keys( data ) 
  # pb <- progress::progress_bar$new( total = n_series, format = "[:bar] :current/:total :percent")
  handlers(handler_progress(format = "  [:bar] :current/:total (:percent) Elapsed: :elapsed ETA: :eta", 
                          clear = FALSE, 
                          width = 60))
  
  with_progress({
            p <- progressor( along = 1:n_series )
  
            result = 
                data %>%
                group_by_key(.drop = TRUE) %>%
                
                group_modify( function(.x, .y) {
                  
                  p()
                  
                  .x = mutate( .x,

                    not_mad = ifelse( ! {{exclude}} , original , NA)
                  )
                  
                  if ( all( .x[[ rlang::as_name( value_col )]] < threshold, na.rm = TRUE) ) {
                    # .x$outlier <- FALSE
                    # print( cat("\n - all small") )
                    return(.x)
                  }
                  
                  .x <- bind_cols(.y[rep(1, nrow(.x)), ], .x)  
                  
                  .x <- as_tsibble(.x, key = key_cols, index = index_col, validate = FALSE)
            
                  # print(cat( "\n - detect_unseasonal_series" ))
                  # .x %>% select( -orgUnit, -data.id  ) 
                  
                  detect_unseasonal_series(.x,
                                           value_col = not_mad ,
                                           deviation = deviation,
                                           method = method ) %>%
                    as_tibble %>% ungroup() %>%
                   select( -orgUnit, -data.id  )
    })
  })
  
  result = as_tsibble( result %>% filter( value ), !!key_cols, !!index_col ) 
  return( result )
}


outlier.summary.tibble = function(
    data = NULL ,
    cols = c( 'AllSmall' ,
              "key_entry_error", "over_max" , 
              'mad15', 'mad10', 'mad5',
              'seasonal5' , 'seasonal3' ) ,
    .print = FALSE 
){
  
  if ( .print ) cat( '\n * Cleaning.R outlier.summary.tibble ')
  
  warn<-options(warn=-1) # suppress divide by zero warning
  
  if ( 'expected' %in% cols ) cols = setdiff( cols, 'expected' )
  
  if ( ! 'mad5' %in% names( data ) ) cols = setdiff( cols, 'mad5' )
  
  if ( ! 'AllSmall' %in% names( data ) ) cols = setdiff( cols, 'AllSmall' )
  
  os.total = as.data.table( data )[  ,
                                     .( Total = sum( original , na.rm = T ) ,
                                        N = sum( !is.na( original )) ) ]
  
  if ( 'AllSmall' %in% names(data)){
    os.notSmall = as.data.table( data )[ AllSmall == FALSE  ,
                                     .( Total.notSmall = sum( original , na.rm = T ) ,
                                        N.notSmall = sum( !is.na( original )) ) ]
  } else {
    os.notSmall = as.data.table( data )[  ,
                                         .( Total.notSmall = sum( original , na.rm = T ) ,
                                            N.notSmall = sum( !is.na( original )) ) ]
  }
  
  os <- as.data.table( data )[  ,
                        .( n = sum( !is.na( original ) ) ,
                           total = sum( original , na.rm = T )  ,
                           Min = min( original , na.rm = T )  ,
                           Median = median( original , na.rm = T ) ,
                           Max = max( original , na.rm = T )
                           ) ,
                        cols] %>%
    as_tibble 
  
  if ( 'AllSmall' %in% names(data)){
    no.err.rows = map( os %>% select( !! cols , -AllSmall ), ~  which( is.na( .x ) | .x == FALSE ) ) %>%
      Reduce( intersect ,  . )
  } else {
    no.err.rows = map( os %>% select( !! cols ), ~  which( is.na( .x ) | .x == FALSE ) ) %>%
      Reduce( intersect ,  . )
  }
    
  os.no.err = os[ no.err.rows , ] %>% 
    summarise( n =  sum( n, na.rm = T ) ,
               total = sum( total, na.rm = T ) ,
               Min = min( Min , na.rm =  T ) ,
               Median = max( Median , na.rm =  T ) ,
               Max = max( Max , na.rm =  T )
               ) %>%
    mutate( err = "No Error Flags") 
  
  os.errs = 
    map_df(  cols   , ~ {
      r = os %>% filter( !! rlang::sym( .x ) == TRUE  ) 
      if ( nrow( r ) == 0 ) r = os[0,] %>% add_row()
      bind_cols( tibble( err = .x ) , r )
    }
    ) %>% 
    mutate( err = as_factor( err ) ) %>%
    group_by( err ) %>% 
    summarise( n =  sum( n, na.rm = T ) ,
               total = sum( total, na.rm = T ) ,
               Min = min( Min , na.rm =  T ) ,
               Median = median( Median , na.rm =  T ) ,
               Max = max( Max , na.rm =  T )
    ) 
  
    # os.small = os.errs %>% filter( err == "AllSmall" ) 
  
  # select( !! cols  , n ,  `%N` ,  max , total , `%Total`  )
  
  rdt.outlier.summary = 
    bind_rows( os.errs , os.no.err )  %>%
    bind_cols( os.total , os.notSmall  ) %>%
    mutate(
      `%N` = ifelse( n>0 , percent( n /
                                      # ifelse( grepl( 'AllSmall', err ) , N , N.notSmall ) , 
                                      N ,
                                    accuracy = .01 ) , -Inf ) ,
      `%Total Value` = ifelse( n>0 , percent( total / 
                                                # ifelse( grepl( 'AllSmall', err ) , Total , Total.notSmall ) ,
                                                Total , 
                                              accuracy = .01 ) , -Inf ) ,
      N = comma( n ) ,
      `Total Value` = comma( total ),
      `Smallest Value` = comma( Min ) ,
      `Median Value` = comma( Median ) ,
      `Largest Value` = comma( Max )
    )   %>%
    select( err,  N, `%N` , `Total Value`, `%Total Value` , 
            `Smallest Value` , 
            `Median Value` ,
            `Largest Value` ) %>%
    rename( 
            `Error Flag` = err ) %>%
    mutate_all( as.character ) 
  
  
  rdt.outlier.summary[ rdt.outlier.summary == -Inf] = "-"
  
  options(warn) # return to normal warnings 
  
  return(  rdt.outlier.summary )
}

outlier.summary.flextable = function(     
    data = NULL ,
    cols = c( 'AllSmall' ,
              "key_entry_error", "over_max" , 
              'mad15', 'mad10', 'mad5',
              'seasonal5' , 'seasonal3' ) ,
    .print = FALSE ){
  
  t = outlier.summary.tibble( data, cols, .print )
  
  if ( .print ) cat( '\n * Cleaning.R outlier.summary.flextable ')
  
  t.allSmall = t %>% filter( `Error Flag` == 'AllSmall') %>% mutate( table = "table1")
  t.other = t %>% filter( ! `Error Flag` == 'AllSmall') %>% mutate( table = "table2")
  
  if ( t.allSmall$N == 0 ) t = t.other
  if ( t.allSmall$N > 0 )t <- bind_rows( t.allSmall , t.other )
  
  flextable( t ) %>%
    theme_vanilla() %>%
    set_header_labels( table = "") %>%
    border( i = which( t$table == "table2")[1], 
           border.top = officer::fp_border(color = "black", width = 1)) %>%
    border( i = nrow(t) , 
           border.top = officer::fp_border(color = "black", width = 1)) %>%
    delete_columns( "table" )

}



          