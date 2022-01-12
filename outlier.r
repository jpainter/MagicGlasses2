
# Outlier function

outlier = function( 
    ts , 
    name = "outlier" ,
    .MAD ,
    ...
    ){
    
        df =  
            ts %>% 
            group_by( orgUnit ) %>%
            
            mutate( across( all_of( dataCols ) , 
                            possibly(  ~clean_ts( . , 
                                                  interpolate = FALSE 
                                                  , .clean = "" ,
                                                  MAD = .MAD ) , # median absolute deviation > 5 
                                       otherwise = NA ) 
            ) 
            ) %>% 
            ungroup() %>%
            
            pivot_longer( {{ dataCols }} , 
                          values_to = name ) %>% 
            select( -starts_with('Count') ) %>%
        
            inner_join( 
            ts %>% 
                pivot_longer( {{ dataCols }}  , 
                              values_to = 'original' ) %>% 
                select( -starts_with('Count') ) ) %>%
            
            mutate(
                is.outlier = is.na( !! sym( name ) ) & !is.na(original)
            ) %>% 
            as_tibble()
        
}

# TEST : 
# extreme = outlier( ts = dTs, name ='extreme', dataCols = dataCols,
#                    MAD= 15) %>%
#     count( is.outlier ) %>%
#             mutate( percent = percent( n / sum(n) )
#             ) 


# test = 
#     map_df( c(3,5,10) ,
#         ~ outlier( ts = dTs, name ='extreme', 
#                      dataCols = dataCols, 
#                      .MAD = .x ) %>%
#             count( is.outlier ) %>%
#             mutate( percent = percent( n / sum(n) ) ,
#                     MAD = .x 
#             )
#     )
# test

