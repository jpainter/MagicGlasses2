# cleaning_functions.R

monthly.outlier.summary = function( df.ts ){
   cat( "\n * monthly.outlier.summary" )
  
  # NOTE: 'All' refers to all data; 'algorithm' refers to the method used to flag and error; 'combined' refers to any error flag 
    
    cat( "\n - errors_by_data_and_algorithm" )
    errors_by_data_and_algorithm = 
        df.ts %>% as_tibble() %>%
        ungroup %>%
        filter( effectiveLeaf ) %>%
        select( Month ,  data.id , data ,value,  mad15, mad10, seasonal5, seasonal3 , original ) %>%
        mutate( Combined = pmax( mad15, mad10,seasonal5, seasonal3 , na.rm = T ) %>% as.logical() ) %>%
        pivot_longer( cols = c(  mad15, mad10, seasonal5, seasonal3 , Combined ) ,
                      names_to = "Alg" , values_to = "val"  ) %>%
        filter( val == TRUE 
                # ,  ( value & Alg %in% "seasonal3" & is.na( val ) )  
                ) %>%
        group_by( Month , data, Alg ) %>% 
        summarise( e = sum( original ) ,
                   n.e = n() )
  
  cat( "\n - data.totals_by_data" ) 
  data.totals_by_data = 
    df.ts %>%  as_tibble() %>%
    ungroup %>%
    filter( effectiveLeaf ) %>%
    group_by( Month , data ) %>% 
    summarise( t = sum( original ) ,
               n.t = n() )

  cat( "\n - errors_by_data_and_algorithm_with_totals_by_data" )
  total.errors_by_data_and_algorithm = 
    data.totals_by_data %>% as_tibble() %>%
    left_join( errors_by_data_and_algorithm , by = c("data", "Month") ) 
  
  
  # calculate for all data 
  cat( "\n- all data errors")
  errors_for_all_data_by_algorithm = 
    errors_by_data_and_algorithm %>%
    group_by( Month , Alg ) %>% 
    summarise_at( vars( e, n.e ) , sum , na.rm = TRUE )  %>%
    mutate( data = 'All' ) 
  
  cat( "\n - data.totals_all" ) 
  data.totals_all = 
    data.totals_by_data %>%
    group_by( Month ) %>%
    summarise_at( vars( t, n.t ) , sum , na.rm = TRUE )  %>%
    mutate( data = 'All' ) 
 
  
  cat( "\n - errors_by_data_and_algorithm_with_totals_by_data" )
  total.errors_for_all_data_by_algorithm = 
    data.totals_all %>% 
    left_join( errors_for_all_data_by_algorithm , by = c("data", "Month") ) 
  
   
  cat( "\n - complete dataset: grand.data.error.totals" )
  grand.data.error.totals = 
    total.errors_by_data_and_algorithm %>% 
    filter( !is.na( Alg ) ) %>%
    bind_rows(  
      total.errors_for_all_data_by_algorithm )  %>%
    mutate( pe =  e / t  ,
            pn =  n.e / n.t ,
            year = year( Month ) ,
            month = month( Month )
            ) %>% 
    as_tsibble( index = Month , key = c( data , Alg ))
  
  return( grand.data.error.totals )
}

yearly.outlier.summary = function( grand.data.error.totals ){
  
   cat( "\n - yearly.outlier.summary" ) 
   yearly.summary = grand.data.error.totals %>% 
     as_tibble %>%
     filter( Alg %in% 'Combined' ) %>%
     group_by( year , data ) %>%
     summarise_at( vars( e, n.e, t, n.t ) , sum  , na.rm = TRUE )  %>%
     mutate( pe =  e / t  ,
             pn =  n.e / n.t 
            ) 
   
   return( yearly.summary )
}

outlier.summary.chart = function( grand.data.error.totals  ){
  
  g =   grand.data.error.totals %>%
    ggplot( ) +
    geom_line ( aes( x = as.factor( month ), 
                     y = pe, color = Alg , group = Alg )
                ) +
    # scale_y_log10() +
    facet_grid( year ~ data  , labeller = label_wrap_gen() ) +
    geom_text( data = yearly.outlier.summary( grand.data.error.totals ) ,
                aes( x = 8, y = .65 , 
                     label =  paste( '%Err(n) =', percent( pn , accuracy = 0.1 ) , '\n' ,
                                     '%Err(value) =', percent( pe , accuracy = 1 ) 
                                     )
                     ) , 
               size = 3 
                ) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    labs( 
      title = 'Potential errors (outliers) among reported values ' ,
      subtitle = 'by year, data, and error detection algorithm' ,
      x = 'Month' , y = "Percent\n" ,
      caption = '%Err(n) = percent of values flagged as potential errors \n 
          %Err(value) = percent of total value flagged as potential errors' )
  
  return( g )
}

# TEST ####
# library( tidyverse )
# library( scales )
# library( lubridate )
# library(tsibble)
# 
# data.directory = "~/Library/CloudStorage/OneDrive-CDC/_Malaria/Projects/HMIS/Formulas/DRC/"
# data.directory = "~/Library/CloudStorage/OneDrive-CDC/_Malaria/Projects/HMIS/Formulas/Zimbabwe/"
# data.directory = "../HMIS/Formulas/Malawi/"
# 
# df.ts <- readRDS( paste0( data.directory ,
#                          #"cas confirme_All-levels_5yrs_2022-08-02.rds"
#                           "confirmed cases ipd, opd, and chw_All-levels_9yrs_2023-07-24.rds"
#                          )
#                  ) %>%
#   filter( COUNT > 0 )
# 
# d = monthly.outlier.summary( df.ts )
# outlier.summary.chart( d )
