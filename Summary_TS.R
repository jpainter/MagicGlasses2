
summary_ts = function( dts , 
                       annual = TRUE # option for total and annual
                       ){
  
    options(dplyr.summarise.inform = FALSE)
  
    d = dts %>%
      unnest( cols = c(ts) ) %>%
      as_tsibble( key = c( data, orgUnit ) , index = Month ) %>%
      group_by_key() 
 
      if ( annual ){  
        d = d %>% index_by( Year = ~ year(.) ) 
      } else {
        d = d %>%  as_tibble() %>% group_by(data, orgUnit ) 
      }
      
      
      totals = d %>%
        mutate( APE = abs(.resid) / value  ) %>%
        summarise( 
          n = sum( !is.na( raw ) ) ,
          n_miss = sum( is.na( raw ) ) ,
          total_raw = sum( raw , na.rm = TRUE ) ,
          total_value = sum( value , na.rm = TRUE ) ,
          total_outlier = sum( outlier , na.rm = TRUE ) ,
          percent_outlier = total_outlier /  total_raw ,
          total_interp = sum( interpolate , na.rm = TRUE ) ,
          percent_interpolate =  total_interp / total_value ,
          MhAPE = harmonic_mean( APE ) ,
          SDhAPE = gm_mean( APE ) ,
          MgAPE = harmonic_mean( APE ) ,
          SDgAPE = gm_sd( APE ) ,
          MAPE = mean( APE , na.rm = TRUE )
        ) %>%
      ungroup %>%
      as_tibble()
    
    # View( totals )
    
    return( totals )
}


### Testing ####
# dts = fms[.x , ] 
# dts = fms[ chunk_ous[26][[1]],  ]
# 
# # add summary ( fms_summary )
#   n = nrow( fms )
#   tic()
#   fm_s = future_map( chunk_ous , ~summary_ts( dts = fms[.x , ] ) , .progress = TRUE )
#   fms_summary = fm_s %>% reduce( ., bind_rows )
#   toc()
#   cat( paste( 'fms_summary completed ' ) )
