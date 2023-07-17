# cleaning_functions.R

monthly.outlier.summary = function( df.ts ){
   cat( "\n * monthly.outlier.summary" )
  
    cat( "\n - errors" )
    errors = 
        df.ts %>%
        ungroup %>%
        filter( effectiveLeaf ) %>%
        select( Month ,  data.id , data ,value,  mad15, mad10, seasonal5, seasonal3 , original ) %>%
        mutate( Combined = pmin( mad15, mad10,seasonal5, seasonal3 , na.rm = T ) %>% as.logical() ) %>%
        pivot_longer( cols = c(  mad15, mad10, seasonal5, seasonal3 , Combined ) ,
                      names_to = "Alg" , values_to = "val"  ) %>%
        filter( val == FALSE 
                # ,  ( value & Alg %in% "seasonal3" & is.na( val ) )  
                ) %>%
        group_by( data, Alg ) %>% 
        summarise( e = sum( original ) ,
                   n.e = n() )
  
  cat( "\n - data.totals" )
  data.totals = 
    df.ts %>%
    ungroup %>%
    filter( effectiveLeaf ) %>%
    group_by( data ) %>% 
    summarise( t = sum( original ) ,
               n.t = n() )

  cat( "\n - data.error.totals" )
  data.error.totals = 
    data.totals %>% as_tibble() %>%
    left_join( errors , by = c("data", "Month") ) 
    
  cat( "\n - grand.totals" )
  grand.totals = data.error.totals %>% 
    group_by( Month , Alg ) %>% 
    summarise_at( vars( e, n.e, t, n.t ) , sum , na.rm = TRUE )  %>%
    mutate( data = 'All' ) %>%
    filter( !is.na( Alg ) )

  cat( "\n - grand.data.error.totals" ) 
  grand.data.error.totals = 
    data.error.totals %>% 
    bind_rows(  
      grand.totals %>% as_tibble() )  %>%
    filter( !is.na( Alg ) ) %>%
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
# 
# df.ts <- readRDS( paste0( data.directory ,
#                          "cas confirme_All-levels_5yrs_2022-08-02.rds"
#                          # "Confirmed Cases_All-levels_10yrs_2022-06-23.rds"
#                          )
#                  ) %>%
#   filter( COUNT > 0 )
# 
# d = monthly.outlier.summary( df.ts )
# outlier.summary.chart( d )
