

# Weighted Median from package gamboostLSS

weighted.median = function (x, w = 1, na.rm = FALSE) 
  {
    if (length(w) == 1) 
        w <- rep(w, length(x))
    x <- x[w != 0]
    w <- w[w != 0]
    if (na.rm) {
        keep <- !is.na(x) & !is.na(w)
        x <- x[keep]
        w <- w[keep]
    }
    else {
        if (any(is.na(x)) | any(is.na(w))) 
            return(NA)
    }
    ind <- order(x)
    x <- x[ind]
    w <- w[ind]
    ind1 <- min(which(cumsum(w)/sum(w) > 0.5))
    ind2 <- ifelse(ind1 == 1, 1, max(which(cumsum(w)/sum(w) <= 
        0.5)))
    if (sum(w)%%1 == 0 && sum(w)%%2 == 0) 
        return(mean(c(x[ind1], x[ind2])))
    return(max(c(x[ind1], x[ind2])))
}
# Harmonic and geometric mean functions 

harmonic_mean = function( x ){
    x = ifelse( is.nan(x) | x %in% Inf , NA, x )
    x= ifelse( x < .00001 , .00001 , x )
    1 / mean( 1/x, na.rm = TRUE )
}

sdhm <- function(x) sqrt((mean(1/x, na.rm = TRUE ))^(-4)*var(1/x, na.rm = TRUE)/length(x) )

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

gm_sd = function(x, na.rm = FALSE, ...)
{ exp(sd(log(x, ...), na.rm = na.rm, ...)) }


outside_mean_sd = function( x , threshold = 3 ){
  # Returns logical if value outside mean +/- 3SD
  mean = mean( x, na.rm = TRUE )
  sd = sd( x, na.rm = TRUE )
  scale = abs(x - mean ) / sd 
  outlier = scale >= threshold 
  outlier = ifelse( is.na( outlier ) , FALSE , outlier ) 
  return( outlier )
}

# weighted mean
weighted.var.se <- function(x, w, na.rm = FALSE)
#  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w, na.rm=na.rm )
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}


ts_model = function( ts , # ts = datasets.  May or may not have nested dataset, ts
                     pb = NULL , 
                     missing.value = NA ,
                     min.months = 24 ,
                     augment = FALSE ,
                     covariates = NULL  ){
  ## TODO: map_df( elements, ts %>% filter( data %in% .x ))
  if ( !is.null( pb ) ) pb$tick()
 
  # if needed, unnest
  if ( 'ts' %in% names( ts ) ){
    
    ts = ts %>% unnest( ts )
  } 
  
  non_missing_groups  = ts %>% as_tibble() %>% 
    group_by( var, orgUnit ) %>% 
    summarise( m = all( is.na( value )) ,
               n = n() ,
               .groups = 'drop' ) %>%
         filter( m == FALSE , n >= min.months)
  
  ts = semi_join( ts , non_missing_groups , 
                  by = c('var', 'orgUnit') ) 
  
  if ( nrow( ts ) < min.months ) return()
  
  if ( ! "tbl_ts" %in% class( ts ) ){
      ts = ts %>%
      as_tsibble( key = c( var, orgUnit ) , index = Month ) 
  }
  
  ts = ts %>% 
    fill_gaps() %>%
    mutate( observed = value 
            # , anomaly =  map_lgl( raw, ~outside_mean_sd(.x) ) 
            # , value = ifelse( anomaly , NA, value )
    ) 
   
    if ( !is.null( covariates ) ){
      ts = ts %>% select( orgUnit, var, Month, value , observed , {{covariates}})
    } else {
      ts = ts %>%  select( orgUnit, var, Month, value , observed )
    }

  defaultW <- getOption("warn") ; options(warn = -1) # turn off warnings when model will not fit
  
  y = "log( value + 1 )"
  pdq = 'pdq(0:2,0:1,0:2) + PDQ(0:2,0:1,0:2)'
  
  if ( !is.null( covariates ) ){
      arima.formula = as.formula(
        paste( y, "~" ,
              paste( paste( covariates, collapse="+" ) , pdq , sep = "+" )
              )
          )
    } else {
      arima.formula = as.formula( paste( y, "~" , pdq ) )
    }
  
  decompose  = ts %>%
      model( 
        # arima = ARIMA( log( value + 1)  ) 
        # arima = ARIMA( log( value + 1)  ~ 0 +
        #          pdq( p = 0, d = 0, q = 0 ) +
        #          PDQ(P = 0:1, D = 0:1, Q = 0:1 , period = '1 year')
        # )
       arima = ARIMA( 
         # box_cox( value , lambda = .5 )
         # log( value + 1) ~
         # ~  covariates + 
           # pdq(0:2,0:1,0:2) + PDQ(0:2,0:1,0:2)
         arima.formula 
                        ) 
      )
  
  # if ( TRUE ){
  #   decompose  = decompose %>% 
  #     inner_join( ts.nest, by = c("data", "orgUnit", "name") )
  # }
  
  options(warn = defaultW) # turn warnings back on
    
  if ( augment ) return( list( decompose , 
                               ts %>% select(-value) %>%
                                 left_join( decompose %>% augment ,
                                                 by = c('orgUnit', 'var', 'Month' ) )
                               )
  )
 
  return( decompose )
             

}


# tidytable version of ts_model (inspired by blog, but unfinished 
# https://www.brodrigues.co/blog/2020-09-05-tidytable/ )
ts_model. = function( ts , # ts = datasets.  May or may not have nested dataset, ts
                     pb = NULL , 
                     missing.value = NA ,
                     min.months = 24 
                     ){
  ## TODO: map_df( elements, ts %>% filter( data %in% .x ))
  if ( !is.null( pb ) ) pb$tick()
 
  # if needed, unnest
  if ( 'ts' %in% names( ts ) ){
    
    ts = ts %>% unnest( ts )
  } 
  
  non_missing_groups  = ts %>% as_tidytable() %>% 
    # group_by( data, orgUnit ) %>% 
    summarise_across.( m = all( is.na( value )) ,
               n = n() ,
               .by = c( data, orgUnit ) ) %>%
         filter.( m == FALSE , n >= min.months)
  
  ts = semi_join.( ts , non_missing_groups , 
                  by = c('var', 'orgUnit') )
  
  if ( nrow( ts ) < min.months ) return()
  
  if ( ! "tbl_ts" %in% class( ts ) ){
      ts = ts %>%
      as_tsibble( key = c( data, orgUnit ) , index = Month ) 
  }
  
  ts = ts %>% 
    fill_gaps() %>%
    mutate( observed = value 
            # , anomaly =  map_lgl( raw, ~outside_mean_sd(.x) ) 
            # , value = ifelse( anomaly , NA, value )
    ) %>%
    select( orgUnit, data, Month, value , observed )
  
  # ts.nest = ts %>% 
  #   select( -value ) %>% 
  #   as_tibble() %>%
  #   nest( ts = c( Month , raw, anomaly ) )
  

  defaultW <- getOption("warn") ; options(warn = -1) # turn off warnings when model will not fit
  
  decompose  = ts %>%
      model( 
        # arima = ARIMA( log( value + 1)  ) ,
        # arima = ARIMA( log( value + 1)  ~ 0 +
        #          pdq( p = 0, d = 0, q = 0 ) +
        #          PDQ(P = 0:1, D = 0:1, Q = 0:1 , period = '1 year')
       arima = box_cox( value , lambda = .5 ) ~ 
                          pdq(0:2,0:1,0:2) + PDQ(0:2,0:1,0:2) 
                       
        ) 
  
  # if ( TRUE ){
  #   decompose  = decompose %>% 
  #     inner_join( ts.nest, by = c("data", "orgUnit", "name") )
  # }
  
  options(warn = defaultW) # turn warnings back on
    
    
 
  return( decompose )
             

}


# anomaly_search <- function( x , y , alpha = .05 ,  
#                               seasonal = TRUE , 
#                               log = FALSE , 
#                               min.months = 24 ,
#                               hampel = FALSE ,
#                               hampel.mad = 3, 
#                               hampel.halfWidth = 5 ,
#                               
#                               pb = NULL ) {
#   
#   options(dplyr.summarise.inform = FALSE)
# 
#   if ( !is.null( pb ) ) pb$tick()
# 
#   data.unnest <- x %>% unnest( data )
# 
#   # if ( !"Mdate" %in% colnames( data.unnest ) ) return()
# 
#   data =
#       data.unnest %>% as_tibble() %>%
#       filter( !is.na( {{y}} )) %>%
#       mutate( 
#         Mdate = as_date( Month )  ) %>%
#       ungroup()  
#   
#   if ( log ){
#     data = data %>%
#       mutate( 
#         {{y}} := log( {{y}} )
#         )
#   }
# 
#   # if (nrow( data ) < 30 ) return()
#   a =
#     try(
#   # anomalize::decompose_stl( data, {{y}} , frequency = "12 months")
#     if ( seasonal & nrow( data ) > min.months  ){ 
#       data %>% 
#         time_decompose( {{y}} , message = FALSE, frequency = 12 ) %>%
#         mutate( orgUnit = x$orgUnit , var = x$var ) %>%
#         select( orgUnit, var, everything() ) %>%
#         anomalize( remainder , alpha = alpha ) %>%
#         rename(  l1 = ends_with( 'l1' ) ,
#                  l2 = ends_with( 'l2' ) ) %>%
#         mutate(
#           anomaly1 = ifelse( anomaly %in% 'Yes' , TRUE , FALSE ) 
#           ) %>%
#         mutate( Month = yearmonth( Mdate ) ,
#                 anomaly1 = ifelse( anomaly %in% 'Yes' , TRUE , FALSE ) ) %>%
#         select( -Mdate )
#     } else {
#       
#       if ( hampel ){
#         
#         data %>% 
#           group_by( orgUnit, var ) %>%
#           summarise( 
#              m = slide_dbl( {{y}} , ~median(.x) , 
#                             .before = hampel.halfWidth , .after = hampel.halfWidth ) ,
#              md = slide_dbl(  {{y}} , ~mad(.x) , 
#                               .before = hampel.halfWidth , .after = hampel.halfWidth ) ,
#              l1 = m -  hampel.mad * md   ,
#              l2 = m + hampel.mad * md ,
#              observed = slide_dbl( {{y}} , ~.x , .before = 0, .after = 0) 
#           ) %>% 
#               ungroup %>% 
#               mutate( 
#                  anomaly1 = observed < l1 | observed > l2
#               ) %>%
#         # select(  - m , -md ) %>%
#         mutate( Month = data$Month)
#         
#         # data %>%
#         #   mutate( 
#         #     l1 = median( {{y}} ) - hampel.mad * mad({{y}} )  ,
#         #     l2 = median( {{y}} ) + hampel.mad * mad({{y}} )  ,
#         #     observed = {{y}} ,
#         #     anomaly1 = ( observed < l1 | observed > l2 ) 
#         #         )  %>%
#         # select( -Mdate )
#         
#       } else { 
#         
#         data %>% 
#         anomalize( {{y}} , alpha = alpha ) %>%
#         mutate(
#           anomaly1 = ifelse( anomaly %in% 'Yes' , TRUE , FALSE ) 
#           ) %>%
#         rename( observed = {{y}} , 
#                 l1 = ends_with( 'l1' ) ,
#                 l2 = ends_with( 'l2' ) 
#                 ) %>%
#         select( -Mdate )
#       }
#     }
#     
#   , silent = TRUE
#   )
# 
#   if ( ! is_tibble( a ) ) return()
# 
#   a %>% mutate( orgUnit = x$orgUnit , var = x$var ) %>%
#     select( orgUnit, var, everything() )
#   
#   if ( log ){
#     a = a %>%
#       mutate_if( is_double , exp )
#   }
#    return( a )
# }
  


### Testing #####
# model partial dataset, n = 50
# ous = unique( hosp.nest$orgUnit )
# 
# tic()
#   fms = ts_model( ts =  hosp.nest %>% 
#                     filter( name %in% 'SUM' , 
#                             orgUnit %in% ous[7] ) ,
#                   pb = NULL )
# toc()


# fms
# 
# # estimated total time
# # ~20 sec for 50 ou
# ((length(ous)/50)*20/60)
# 
# # model partial dataset, n = 50, with parallel processing
# tic()
#   fm = future_map( list(1:10,11:20,21:30,31:40,41:50) ,
#     ~model.ts( ts =  hosp.nest %>% filter( name %in% 'SUM' ) , 
#                ou = ous[.x] , 
#                pb = NULL ) , 
#     .progress = TRUE ) 
#   fms = reduce( fm , bind_rows )
# toc() #8.2 sec
# fms
# 
# # estimated total time
# # ~1.3 sec for 50 ou
# ((length(ous)/50)*9.2/60) # ~6.65 min
# 
# # model whole dataset, n = 50, with parallel processing
# chunks <- function(x,n) split(x, ceiling(seq_along(x)/n))
# chunk_ous = chunks( 1:length(ous), 10 )
# tic()
#   fm = future_map( chunk_ous ,
#     ~model.ts( ts =  hosp.nest %>% filter( name %in% 'SUM' ) , 
#                ou = ous[.x] , 
#                pb = NULL ) , 
#     .progress = TRUE ) 
#   fms = reduce( fm , bind_rows )
# toc() 
# fms

# Error in rowSums(.x[, 1:6]) : ou = ous[chunk_ous[32][[1]][27:28] ]
