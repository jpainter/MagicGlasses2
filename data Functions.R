
# period = function( data , .cat = FALSE ){
#       if ( .cat ) cat('\n* period: ')
#       
#       weekly = any( map_lgl( data ,
#                              ~any(class(.x) %in% 'yearweek'  )) )
# 
#       period = ifelse( weekly, "Week", "Month" )
#       if ( .cat ) cat( period )
#       return( period  )
#     }

as.yearmonth = function( date.string , fmt = "%B%Y" ){
  
  if ( is_yearmonth( date.string )) return( date.string )
    
  if ( !is.na( zoo::as.yearmon( date.string ) )  ) return( zoo::as.yearmon( date.string ) %>% yearmonth )
  if ( !is.na( lubridate::ym( date.string )  ) ) return( lubridate::ym( date.string ) %>% yearmonth )


}

getLevelNames = function( orgUnits, .cat = FALSE ){ 

    if ( 'sf' %in% class( orgUnits ) ) orgUnits = orgUnits %>% st_drop_geometry()
    if ( .cat ) cat( '\n* levelNames:' )
    l = count( orgUnits %>% as_tibble, level, levelName ) %>% 
      filter( !is.na( level ) ) %>%
      arrange( level ) %>% pull(levelName ) %>% unique
    l = l[ !is.na(l) ]
    if ( .cat ) cat( '\n - end levelNames:', paste(l, collapse = ", " )  )
    return(l)
}

# getLevelNames = function( orgUnits = NULL , .cat = FALSE  ){
#  
#     if ( .cat ) cat( '\n* reporting_widget levelNames():' )
#     
#     if ( is.null( orgUnits ) ){
#       cat( "\n - orgUnits missing")
#       return()
#     }
#   
#     LevelNames = count( orgUnits %>% as_tibble, level, levelName ) %>% 
#       arrange( level ) %>% pull(levelName ) 
#     
#     LevelNames = LevelNames[ !is.na( LevelNames ) ]
#     
#     if ( .cat ) cat( '\n - :' , LevelNames  )
#     return( LevelNames )
# }


error_factor = function(x){
  x %>% 
  mutate( 
    error = case_when( 
      ( !key_entry_error&!over_max&!mad15&!mad10&!seasonal5&!seasonal3 )~ 'none' ,
      seasonal3 ~ 'seasonal3' ,
      seasonal5 ~ 'seasonal5' ,
      mad10 ~ 'mad10' ,
      mad15 ~  'mad15' ,
      over_max ~ 'over_max' ,
      key_entry_error ~ 'key_entry_error' ,
      TRUE ~ 'none'
      ) %>%
      factor( c('All' , 'key_entry_error', 'over_max' , 'mad15', 'mad10', 'seasonal5', 'seasonal3', 'none' ) , ordered = TRUE )
      )
}

# cleanedData equivalent to MG2 reactive function d()
cleanedData = function( d , 
                        .effectiveLeaf = TRUE , 
                        source = 'Original' , 
                        error =  NULL ,
                        algorithm = 'seasonal3' ,
                        .cat = FALSE ){

      if ( .cat ) cat( '\n* data Functions.R cleanedData:')
    
      .period = dataPeriod( d )
      if ( .cat ) cat('\n - period is', .period )
      
      if ( .cat ) cat( '\n - filtering by effectiveLeaf' , .effectiveLeaf )
      d = d %>% filter( effectiveLeaf == .effectiveLeaf )

      if ( nrow( d ) == 0 ){
        if ( .cat ) cat('\n - d1 has zero rows')
        return( d )
      } 

      # TODO for speed -- use d.table ....
      # Don't know why...
      # d = d1  %>% mutate( period = !!rlang::sym( .period ) )
      
    # if ( is_null( error ) &  source %in% 'Original' ){
    if ( is_null( error ) || error == "Original" ){
      if ( .cat ) cat('\n- source is original')
      d = d %>% mutate( dataCol = original )
    }  
      
    # if ( is_null( error ) & ( source %in% 'Cleaned' ) & ( algorithm %in% names(d) ) ){
    # if ( ! is_null( error ) ){
    #   algorithm = error
      
      # if ( .cat ) cat( '\n-' , paste('cleaning removes', sum( d$value , na.rm = T ) - sum( d$seasonal3 , na.rm = T )  , 'd points' ) )
      
    #   d = d %>% 
    #     mutate( dataCol = ifelse( !! rlang::sym( algorithm ) , NA, original  ) )
    #   
    #   # Modify variables used for cleaning d so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means d is ok
    #   if ('mad15' %in% names( d )) d = d %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
    #   if ('mad10' %in% names( d )) d = d %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
    #   if ('mad5' %in% names( d )) d = d %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
    #   if ('seasonal5' %in% names( d )) d = d %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
    #   if ('seasonal3' %in% names( d )) d = d %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
    #   
    #   if ( .cat ) cat( '\n-' , paste('cleaning changes total by', sum( d$original , na.rm = T ) - sum( d$dataCol , na.rm = T )) )
    # }  
    
    if ( !is_null( error )  & 'error' %in% names( d ) ){
      if ( .cat ) cat("\n - error level:" , error )
      error.levels = levels( d$error ) 
      error.factor.value = which( error == error.levels )
      d = d %>% 
        mutate( dataCol = ifelse( as.numeric( error ) >  error.factor.value , 
                                  original, NA ) )
    }
      
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( d ))
    # if ( .cat ) cat( '\n - sum(!is.na(d$dataCol)):' , sum(!is.na(d$dataCol)) )
    removed = sum( is.na(d$dataCol) )  
    if ( .cat ) cat( '\n - error set this many values to NA:' , removed , "(", percent(removed/nrow(d)), ")")
    
    # Remove rows where d not translated correctly.  d is 'NA_'
    if ( .cat ) cat( '\n - removing rows where d is NA_: ', sum( d$data %in% "NA_" ) , 'rows')
    
    # TODO speed up with d.table
    d = d %>% filter( ! data %in% 'NA_') 
    
    # if ( .cat ) cat( '\n - nrow( d ):' , nrow( d ))
    
    return( d )
}

mostFrequentReportingOUs <- function( 
                          data ,  
                          endingMonth = NULL , 
                          startingMonth = NULL ,
                          period = NULL ,
                          missing_reports = 0 ,
                          count.any = TRUE , 
                          # all_categories = TRUE , 
                          data_categories = NULL ,
                         .cat = FALSE ){
  
    if ( .cat ) cat( '\n* data Functions.R mostFrequentReportingOUs' )
    
    if ( is.null( period ) ) period = dataPeriod( data )
    if ( .cat ) cat( '\n - period is:', period )
    
    if ( is.null( startingMonth ) ) startingMonth = min( data$period , na.rm = TRUE )
    if ( is.null( endingMonth ) ) endingMonth = max( data$period , na.rm = TRUE )

    if ( period %in% 'Month' ){
      if ( !'yearmonth' %in% class( startingMonth) ) startingMonth = as.yearmonth( startingMonth )
      if ( !'yearmonth' %in% class( endingMonth) ) endingMonth = as.yearmonth( endingMonth )

    }
    
    if ( period %in% 'Week' ){
      if ( !'yearweek' %in% class( startingMonth) ) startingMonth = yearweek( startingMonth  )
      if ( !'yearweek' %in% class( endingMonth) ) endingMonth = yearweek( endingMonth  )

    }
    
    # TODO for speed -- use data.table ....
    
    if ( !count.any   ){
          if ( .cat ) cat( '\n - not count.any'  )
          data = data %>% filter( data %in% data_categories )
       }
  
    if ( period %in% 'Month' ){
         if ( .cat ) cat( '\n - reportingSelectedOUs by Month, between ', startingMonth, endingMonth )
         data = data %>% as_tibble %>%
         filter( 
           Month >=   startingMonth   ,
           Month <=   endingMonth 
                 ) 
       } 
      
    if ( period %in% 'Week' ){
         if ( .cat ) cat( '\n - reportingSelectedOUs by Week')
         data = data %>% as_tibble %>%
         filter( 
           Week >=  yearweek( startingMonth  )  ,
           Week <=  yearweek( endingMonth )  
                 ) 
       } 
    
  # mr = data %>%
       #   filter( !is.na( original  ) ) %>%
       #   distinct( !! rlang::sym( period() ) , orgUnit ) %>%
       #   group_by( orgUnit ) %>%
       #   summarise( n = n() ) %>%
       #   arrange( desc( n ))
       #
       # #print( "mr" ); #print( summary( mr$n ) )
       #
       # s = mr %>%
       #   filter( n == max( mr$n ) ) %>%
       #   pull( orgUnit ) %>% unique

    # mr = data %>% 
    #      filter( !is.na( original  ) ) %>%
    #      distinct( !! rlang::sym( .period ) , orgUnit ) %>%
    #      group_by( orgUnit ) %>%
    #      summarise( n = n() ) %>%
    #      arrange( desc( n ))
    
    if ( .cat ) cat( '\n - mr')
    mr = data %>%
        filter( !is.na( original  ) ,   ) %>%
        group_by( year = year( !! rlang::sym( period )  ) ) %>%
        distinct( !! rlang::sym( period ) , orgUnit ) %>%
        group_by( year , orgUnit ) %>%
        summarise( report_periods = n() )
    
       
       #print( "mr" ); #print( summary( mr$n ) )
    max_years =  n_distinct( mr$year , na.rm = FALSE ) 
    if ( .cat ) cat( '\n - max_years', max_years )
    
    
    periods_per_year = data %>% 
        distinct( !! rlang::sym( period )  ) %>%
        mutate( year = year( !! rlang::sym( period )  )) %>%
        count( year ) %>%
        rename( max = n )
    if ( .cat ) cat( '\n - periods per year' )
    
    s = mr %>%
      inner_join( periods_per_year , by = "year" ) %>%
      ungroup %>%
      group_by( orgUnit ) %>%
      summarise(  
          years = n() ,
          consistent = all( report_periods >= ( max - missing_reports ) ) ,
          champion =  ( years == max_years ) & consistent
        ) %>%
         filter( champion ) %>%
         pull( orgUnit ) %>% unique
       
    if ( .cat ) cat( "\n - number reportingSelectedOUs:", length(s), 'orgUnits' ) 
    return( s )
}  

  
  
# group_by_cols = function( data = NULL , levelNames = NULL, 
#                           split = NULL, 
#                           merge = TRUE ,
#                           .cat = FALSE ){
#     # req( input$split )
#     if ( .cat ) cat("\n* group_by_cols():")
#   
#    .period = period( data )
#     
#     group_by_cols =  c(.period , 'orgUnit', 'data' ) 
#     
#     if ( !merge ) group_by_cols = c( group_by_cols, 'dataSet' )
#     
#     if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
#    
#     group_by_cols = c( group_by_cols, levelNames )
#   
#     if ( .cat ) cat("\n - group_by_cols():", group_by_cols )
#     
#     if ( !is.null( split ) ) group_by_cols = c( group_by_cols , split )
#     
#     # if ( length( reportingSelectedOUs() > 0 ) ) 
#       # group_by_cols = c( group_by_cols , 'Facilities' )
#     
#     # # If not merge when total, show separate datsets
#     # if ( !input$merge & input$all_categories ) group_by_cols = c( group_by_cols , 'dataSet' )
#     #   
#     if ( .cat ) cat( "\n- end group_by_cols()" , unique( group_by_cols )  )
#     return( unique( group_by_cols ) )
# 
# }


# data.total = function( data ,  
#                       period = "Month" ,
#                       .group_by_cols = NULL ,
#                       dataSet = NULL , 
#                       merge = FALSE ,
#                       dataset_merge_average = FALSE ,
#                       startDisplayMonth = NULL  , 
#                       endDisplayMonth = NULL,
#                       .cat = FALSE 
#                        ){
#     if ( .cat ) cat( '\n* data.total():' )
#   
#     .period = dataPeriod( data )
#   
#     if ( .cat ) cat( '\n - period:' , .period )
#     
#     .dates = data %>% pull( !!rlang::sym( .period )  )
#     if ( is.null( startDisplayMonth ) )  startDisplayMonth = min( .dates , na.rm = TRUE  )
#     if ( is.null( endDisplayMonth ) ) endDisplayMonth = max( .dates , na.rm = TRUE   )
#     
#     if ( is.null( .group_by_cols )) .group_by_cols = groupByCols( data )
#     if ( .cat ) cat( '\n - data.total .group_by_cols:'  , .group_by_cols )
#   
#     # Total categories by facilities and datasets
#     # data = plotData
# 
#       # Merge  datasets 
#       # Set all dataSets to Combined and re-summaries taking mean
#       # #print( 'data.total datasets' );  #print( dataSets() )
#       if ( .cat ) cat( '\n - merge ', merge )
#       if ( .cat ) cat( '\n - data datsets ' , unique( dataSet) ) 
#       
#       mergeDatasets = merge %>% str_replace_all( fixed("\n"), "") 
#       if ( .cat ) cat( '\n - mergeDatasets:' , mergeDatasets )
#       
#      
#       if ( merge  ){
#   
#       combineSelectDatasets = data %>%
#                 mutate( dataSet = dataSet %>% str_replace_all( fixed("\r\n"), "") 
#               ) %>%
#                 mutate(
#                     dataSet = ifelse( 
#                         str_replace_all(dataSet, fixed("\n"), "") %in% 
#                           mergeDatasets , 'Combined' , dataSet) ,
#                     data = 'Total'
#                 ) %>% 
#                 setDT() %>%
#                 .[ , .(dataCol = sum( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
#       
#       if ( .cat ) cat('\n - Combining dataSets %in% input$merge:' , mergeDatasets )
#       
# 
#       } else { combineSelectDatasets = data }
#       
#       # Testing
#       # saveRDS( combineSelectDatasets , 'combineSelectDatasets.rds' )
#       
#       # data.table sum/mean 
#       
#       if ( dataset_merge_average ) {
#            if ( .cat )  cat( '\n** merge data.table MEAN') 
#             
#             dataMerge = combineSelectDatasets %>%
#                 mutate( dataSet = 'Merged') %>%
#                 setDT() %>%
#                 # Mean of dataSets within orgUnit
#                 .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
#   
#             if ( .cat ) cat( '\ndataMerge done' );  # glimpse( dataMerge )
#         
#       } else {
#           dataMerge = combineSelectDatasets
#           # cat('\n glimpse( dataMerge )\n' ); #print(glimpse( dataMerge ))
#       }
#       
#       # Testing
#       # saveRDS( dataMerge, 'dataMerge.rds' )
#       # #print( dataMerge %>% duplicates %>% glimpse )
#   
#     key.cols = setdiff( .group_by_cols , .period ) 
#     if ( .cat ) cat('\n - key.cols:' ,  key.cols )
#     
#     data.total = 
#         dataMerge %>% 
#         # fill_gaps( .full = TRUE  ) %>%
#         mutate( 
#                 total = replace_na( dataCol , 0) 
#                 )  %>% # for plotting, replace missing with zero 
#         as_tsibble( index = !! rlang::sym( .period )  , 
#                     key =  all_of(  {{ key.cols }} ) ) 
# 
#     if ( .cat ) cat( '\n - data.total class' , class( data.total ) ) 
#     if ( .cat ) cat( '\n - data.total cols' , names( data.total ) ) 
#     
#     # Filter display dates
#     # cat( '/n - data.total cols:' , names( data.total ) )
#     
#     if ( .period %in% 'Month' ){
#       if ( .cat ) cat( '\n -  .period %in% Month' )
#       data.total = data.total %>% 
#         filter( 
#           Month >=  yearmonth( startDisplayMonth )  ,
#           Month <=  yearmonth( endDisplayMonth )  
#         )
#     } 
#     
#     if ( .period %in% 'Week' ){
#       if ( .cat ) cat( '\n -  .period %in% weeks' )
#       data.total = data.total %>% 
#         filter( 
#           Week >=  yearweek( startDisplayMonth )  ,
#           Week <=  yearweek( endDisplayMonth )  
#         )
#     } 
#     
#   
#     # test:
#     # saveRDS( data.total, 'data.total.rds')
#     
#     if ( .cat ) cat('\n- end data.total()')
#     return( data.total )
#       
#   
# }

backtick <- function(x) paste0("`", x, "`")

# determine the period used in dataset (e.g. Monthly or Weekly )
dataPeriod =   function( data1 , .cat = FALSE  ){
  
      if( .cat ) cat('\n* reporting_widget period():')
      
      search_for_weekly = any( map_lgl( data1 ,
                             ~any( class(.x) %in% 'yearweek'  )) )

      period = ifelse( search_for_weekly, "Week", "Month" )
      
      if ( .cat ) cat('\n - dataPeriod is ', period )
      
      return( period  )
    }


groupByCols = function( 
                        selected = TRUE, 
                        dataset = TRUE ,
                        orgUnit = TRUE , 
                        data = FALSE  , 
                        period = NULL , 
                        hts = FALSE , 
                        agg_level = NULL , 
                        levelNames = NULL ,
                        split = NULL , 
                        .cat = FALSE  ){
  
    if ( .cat ) cat('\n* data Functions.R groupByCols :')
  
    group_by_cols = period 
    
    if ( selected ) group_by_cols = c( group_by_cols, 'Selected' ) 
    
    if ( orgUnit ) group_by_cols = c( group_by_cols, 'orgUnit' ) 
    
    if ( dataset ) group_by_cols = c( group_by_cols, 'dataSet' ) 
    
    if ( data ) group_by_cols = c( group_by_cols, 'data' ) 
    
    if ( .cat ) cat( "\n - agg_level" , agg_level )
    
    if ( ! is_empty( agg_level ) ) group_by_cols = base::union( group_by_cols, agg_level ) 
    
    if ( hts && ! is.null( levelNames )  ) group_by_cols = base::union( group_by_cols, levelNames )
  
   
    if (  !is.null( split ) && any( split != 'None' ) ) group_by_cols = base::union( group_by_cols , split )
    
    if ( .cat ) cat( "\n - end group_by_cols()" , unique( group_by_cols )  )
    
    return( unique( group_by_cols ) )

}


# selectedData equivalent to MG2 reactive function plotData()
selectedData = function( data1 ,  
                         levelNames = NULL ,
                         # all_categories = TRUE , 
                         data_categories = NULL ,
                         alwaysReporting = TRUE , 
                         missing_reports = 0 ,
                         reportingSelectedOUs = NULL ,
                         startingMonth = NULL , 
                         endingMonth = NULL ,
                         period = NULL ,
                         # source = 'Original' ,
                         level = 'leaf' , 
                         level2 = NULL ,
                         level3 = NULL ,
                         level4 = NULL , 
                         level5 = NULL , 
                         .cat = FALSE ,
                         ... ){

   if ( .cat ) cat( "\n* data Functions.R selectedData:" )
  
   if ( nrow( data1 ) == 0 ){
        cat('\n - data1() has zero rows')
        return()
   }
  
  if ( .cat ) cat( '\n - nrow( d ):' , nrow( data1 ))

  if ( is.null( period ) ) period = dataPeriod( data1 )
  if ( .cat ) cat( '\n - period is:', period )
  
   # if ( is_null( startingMonth )) startingMonth = yearmonth( min( data1$period   , na.rm = T ) , format = "%B%Y" )
   if ( is.null( startingMonth ) ) startingMonth = min( data1$period , na.rm = TRUE )
   # if ( is_null( endingMonth )) endingMonth = yearmonth( max( data1$period   , na.rm = T ) , format = "%B%Y" )
   if ( is.null( endingMonth ) ) endingMonth = max( data1$period , na.rm = TRUE )
   if ( is_null( data_categories ) ) data_categories = unique( data1$data )
   if ( is_null( levelNames ) ) levelNames = unique( data1$data )

      # NB: setting data = setDT( data1()) has side effect of changing data1() to data.table. 
      data = as.data.table( data1  )
      
      if ( .cat ) cat( '\n - data (d) converted to data.table' )
      
      # period = dataPeriod( data1 )
      # if (.cat ) cat('\n - period is', period )
      # 
      # data = data[ , period := base::get( period )  , ]

      if ( !is_empty( level2 ) & !is_empty( levelNames ) ){ data = data[ base::get( levelNames[2] )  %in%  level2 ,, ] }
  
      if ( !is_empty( level3 ) & !is_empty( levelNames ) ){ data = data[ base::get( levelNames[3] )  %in%   level3 ,, ] }
  
      if ( !is_empty( level4 ) & !is_empty( levelNames ) ){ data = data[ base::get( levelNames[4] )  %in%   level4 ,, ] }
        
      if ( !is_empty( level5 ) & !is_empty( levelNames ) ){ data = data[ base::get( levelNames[5] )  %in%   level5  ,, ]  }
    
      if ( level %in% 'leaf' ){  
        
        if ( .cat ) cat( '\n - leaf level data only' )
        data = data[ effectiveLeaf == TRUE , , ]
        
      } else {
        
        if ( .cat ) cat( '\n - levelname', levelName )
        level. = count( orgUnits %>% as_tibble, level, levelName ) %>% 
          filter(levelName  %in% input$level  ) %>% pull( level )
        
        data = data[ level  %in% level. , , ] 
      }
      
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
  
    # if ( source %in% 'Original' ){
    #   if ( .cat ) cat('\n - d() source is original')
    #   
    #   data = data[ , dataCol := as.numeric( original ) , ] 
    # }  
    #   
    # if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
    # 
    # if ( source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
    #   if ( .cat ) cat( '\n -' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
    #   
    #   data = setDT( data )[ , dataCol := fifelse( seasonal3, original, NA_real_  ) , ]
    #   
    #   # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
    #   if ('mad15' %in% names( data )){
    #     # data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
    #     data = setDT( data )[, mad15 := fifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) , ] 
    #     
    #   }
    #   
    #   if ('mad10' %in% names( data )){ 
    #     # data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
    #     data = setDT( data )[, mad10 := fifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) , ] 
    #     
    #   }
    #   
    #   if ('mad5' %in% names( data )){ 
    #     # data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
    #     data = setDT( data )[, mad5 := fifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) , ] 
    #     
    #   }
    #   
    #   if ('seasonal5' %in% names( data )){ 
    #     # data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
    #     data = setDT( data )[, seasonal5 := fifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) , ] 
    #   }
    #   
    #   if ('seasonal3' %in% names( data )){ 
    #     # data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
    #     data = setDT( data )[, seasonal3 := fifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) , ] 
    #     
    #   }
    #   
    #   if ( .cat ) cat( '\n -' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    # }  
    # 
    # if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
      
    # filter to selected category
    

    if ( any( !is.null( data_categories ) ) ){
      if ( .cat ) cat( '\n - selectedData filtered by' , paste( data_categories , collapse = "\n - ") )
      data = data %>% filter( data %in% data_categories )
      if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
    }
   
    # Consistent reporting
    if ( alwaysReporting ){
     
     if ( .cat ) cat( '\n - alwaysReporting' )
     
     # reportingSelectedOUs = NULL 
     if ( is_empty( reportingSelectedOUs ) & nrow( data ) > 0 ){
       
        if ( .cat ) cat( "\n - finding most frequently reporting OUs")
         
        reportingSelectedOUs  = mostFrequentReportingOUs( data ,
                                             # all_categories = all_categories , 
                                             data_categories = data_categories ,
                                             startingMonth = startingMonth , 
                                             endingMonth = endingMonth ,
                                             missing_reports = missing_reports 
                                             
                                             )
     }  
   
        # Add var for selected ous
        if ( .cat ){
          cat( '\n - selectedData length( reportingSelectedOUs()): ' , length( reportingSelectedOUs ) )
          cat( '\n - starting - ending: ' , startingMonth,  endingMonth ) 
        
        } 
        data = setDT(data)[ , Selected := fifelse( orgUnit %in% reportingSelectedOUs, 
                                                          'Reporting Each Period',
                                                          'Inconsistent Reporting') ] %>%
          as_tibble(.) 
        
        if ( period %in% "Month" ) data = data %>%
          filter( Month  >= as.yearmonth( startingMonth ) )
      
        if ( period %in% "Week" ) data = data %>%
          filter( Week  >= yearweek( startingMonth ) )
      
    # data = data %>% filter( Selected %in% 'Reporting Each Period' )
    } else {
         data = setDT(data)[ , Selected := "All", ] %>% as_tibble(.)
   }
   
    if ( .cat ) cat( '\n - end  selectedData()' )  ; # #print( names( data )) 
    # TESTING
    # saveRDS( data , "plotData.rds" )
    
  return( data )
}
 

# merge datasets 
dataTotal = function(
    data = NULL , 
    group_by_cols = NULL ,
    period = "Month" , 
    startMonth = NULL ,
    endMonth = NULL ,
    dataSets = NULL ,
    sum.data = TRUE , 
    mean.merge = FALSE ,
    covariates = NULL , 
    .cat = TRUE ){
    
    if ( .cat ) cat( '\n* data Functions.R dataTotal()' )
  
    if ( is.null( period ) ) period = dataPeriod( data )
      
    if ( !is.null( dataSets ) && ( any( nchar( dataSets ) > 0 )) ){
      
      if ( .cat ) cat( "\n - combineSelectDatasets ")
      
      mergeDatasets = dataSets %>% str_replace_all( fixed("\n"), "") 
      
      if ( any(!is.na( mergeDatasets )) & any(nchar( mergeDatasets ) > 0 )  ){
        
               data = 

                setDT( data )[ any(  mergeDatasets %in% dataSet ) ,  dataSet :=  'Combined'  ] %>%
          
                as_tibble
      }
      
    }
    
    if ( is.null( group_by_cols )) group_by_cols = groupByCols( period = period  )
  
    # if ( any( grepl( "avg_mm" , names( data ) ) ) ){
    #       
    #       if ( .cat )  cat( '\n - with avg_mm' )
    #     
    #       data = setDT( data ) %>%
    #             .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
    #                     avg_mm = mean( avg_mm , na.rm = TRUE  ) ) , 
    #                by =  group_by_cols ] 
    #       
    #     } else {
    #       if ( .cat )   cat( "\n - creating dataCol")
    #       
    #       data = setDT( data ) %>%
    #             .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )) , by =  group_by_cols ] 
    #     }
    
    if (.cat) cat("\n - summarising covariates: ", covariates )
 
  
    summary_cols = NULL
    if ( ! any( is.null( covariates  ) ) &&  any( nchar( covariates) >0 ) ) summary_cols = c( summary_cols, covariates ) %>% unique 
    if (.cat) cat( "\n - summary_cols: ", summary_cols )
    if (.cat)cat( "\n - summarise group by: ", group_by_cols )

    if ( ! any(is.null( summary_cols )) &&  any( nchar( summary_cols  )>0 ) ){
      if (.cat)cat( "\n - and all_of : ", summary_cols )

      group_by_cols = str_remove( group_by_cols , fixed("`") )
      
      data = data %>%
        group_by_at( vars( {{  group_by_cols  }} )  )  %>%
        summarise(  
          across( dataCol , \(x) sum(x, na.rm = TRUE ) ) ,
          across( all_of( summary_cols ) , \(x) mean( as.numeric(x), na.rm = TRUE ) )  
        )
      
    } else { 
     
      data = data %>%
        group_by_at( vars( {{ group_by_cols }} ) )  %>%
        summarise(  
          across( dataCol , \(x) sum(x, na.rm = TRUE ) )   
        )
    }
    
    # data = setDT( data ) %>%
    #   .[ ,  lapply( .SD, sum  ) ,  
    #      by =  group_by_cols ]

    # if ( !any(is.null( covariates )) && any( grepl( "avg_mm", covariates ) ) && ! any( grepl( "rdt_so", covariates ) )) {
    #   
    #   if (.cat) cat("\n - avg_mm")
    #   
    #   data = setDT( data ) %>%
    #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
    #             avg_mm = mean( avg_mm , na.rm = TRUE  )  ) ,
    #        by =  group_by_cols ]
    # }  
    # 
    # if ( !any(is.null( covariates )) && any( grepl( "rdt_so", covariates ) ) && ! any( grepl( "avg_mm", covariates ) )) {
    #   
    #   if (.cat) cat("\n - rdt_so")
    #   
    #   data = setDT( data ) %>%
    #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
    #             rdt_so = mean( rdt_so, na.rm = T ) ) ,
    #        by =  group_by_cols ]
    # }  
    # 
    # if ( !any(is.null( covariates )) && any( grepl( "avg_mm", covariates ) ) && any( grepl( "rdt_so", covariates ) ) ) {
    #   
    #   if (.cat) cat("\n - avg_mm and rdt_so")
    #   
    #   data = setDT( data ) %>%
    #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
    #             avg_mm = mean( avg_mm , na.rm = TRUE  ) ,
    #             rdt_so = mean( rdt_so, na.rm = T )  ) ,
    #        by =  group_by_cols ]
    # }
    # 
    # if ( any(is.null( covariates )) ) {
    #   
    #   if (.cat) cat("\n - no covariates "
    #                 )
    #   data = setDT( data ) %>%
    #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )  ) ,
    #        by =  group_by_cols ]
    # }
    #   
        
    if ( mean.merge ) {
          if ( .cat )   cat( '\n -  merge data.table MEAN') 
            
          data = data %>%
                mutate( dataSet = 'Merged') %>%
                setDT() %>%
                # Mean of dataSets within orgUnit
                .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  group_by_cols ] 
  
         if ( .cat )    cat( '\n - Merge done' );  # glimpse( dataMerge )
        
      } 

      key.cols = setdiff( group_by_cols , period ) 
      
      if ( .cat )    cat( '\n - key.cols' , key.cols );
      
      
      if ( period %in% 'Month' & !is.null( startMonth )  & !is.null( endMonth )  ){

        if ( .cat )    cat( '\n - period' , period , startMonth , endMonth )
        
        data = setDT( data )[  which( Month >=  yearmonth( startMonth ) &
                                        Month <=  yearmonth( endMonth ) ) , ]

    }

    if ( period %in% 'Week' & !is.null( startMonth )  & !is.null( endMonth )){
      
      if ( .cat )    cat( '\n - period' , period , startMonth , endMonth )

      data = setDT( data )[ which( Week >=  yearweek( startMonth ) &
                                        Week <=  yearweek( endMonth ) ) , ]
    }
 
    ## NB does data.total need to be a Tsibble?--it is slow.  
    
    data.total = setDT( data )[ , total := replace_na( dataCol , 0)  ,] %>%
      as_tibble()
    
    # if (.cat ) cat( "\n - data.total columns:" , paste( names( data.total ) , collapse = ", "))

    if ( .cat ) cat('\n - end dataTotal()')
    
    # saveRDS( data.total , "data.total.rds")
    
    return( data.total )
    
  }
  
htsFormula = function( 
                hts = TRUE ,
                levelNames = NULL , 
                agg_level = NULL ,
                all.levels = FALSE , 
                num_facilities = NULL ,
                num_datasets = NULL ,
                split = 'None' ,
                .cat = FALSE ){   
 
    if ( .cat ) cat("\n* htsFormula():" )

    htsFormula = ""
    
    if ( hts ){
      if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
     
      adms = backtick( levelNames )
      
      if ( all.levels ){ 
        htsFormula = paste( adms, collapse = "/" ) 
        
      } else {
        
        if ( !is.null( agg_level ) ){
          
          if ( .cat ) cat( '\n - adms:',  adms )
          if ( .cat ) cat( '\n - input$agg_level:',  agg_level )
          
          hts_level = which( agg_level == levelNames   )
        
          if ( .cat ) cat( '\n - hts_level:',  hts_level )
        
          # hts = paste( adms[1:( hts_level + 1 )] ,
          #            collapse = "/" )
          
          htsFormula = paste( adms[1: hts_level ] ,
                     collapse = "/" )
          
          htsFormula = paste( "(" , hts , ")" )
        }
      }
    }
    
    # if >1 Facilities (ie. selected)
    if ( num_facilities > 1 )  htsFormula = paste(
             'Selected *' , htsFormula
             )

    # if >1 dataset
    if ( num_datasets > 1 )  htsFormula = paste(
             'dataSet *' , htsFormula
             )

    # # Cross by split
    if ( any( !split %in% 'None' ) ) htsFormula =
      paste( paste( backtick( split ) ,  collapse = " * " ) ,
             '*' ,  htsFormula )
    # 
    # Cross by selected and split
    # if ( length( reportingSelectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
    #   paste( input$split ,  ' * Facilities * (', hts , ')' )
    
    # remove a trailing * ...
    htsFormula = str_trim( htsFormula ) 
    
    if ( str_ends( htsFormula , fixed("*") ) ){
      hts.length = nchar( htsFormula  )
      htsFormula = substr( htsFormula , 1, hts.length - 1 )
    }
    
    if ( .cat ) cat("\n - end htsFormula():" , htsFormula )
  
    return( htsFormula )
  }
  
htsData = function( data = NULL , 
                    hts = TRUE ,
                    hts_formula = NULL, 
                    covariates = "" , 
                    group_by_cols = NULL ,
                    .cat = FALSE , 
                    timing = FALSE , ... ){

    if ( .cat ) cat('\n* htsData:' )
  
    if ( timing ) tic() 
  
    if ( is.null( data ) ){
      if ( .cat ) cat('\n - end htsData(): data is missing '  ) 
      return( data )
      } 
  
    if ( is.null( hts_formula ) ){
      if ( .cat ) cat('\n - end htsData(): hts_formula NULL '  ) 
      return( data )
    } 
  
    if ( is.null( group_by_cols )) group_by_cols = groupByCols( period = dataPeriod( data )  )
    
    if ( !is_tsibble( data ) ){
      
      if ( .cat ) cat('\n - preparing data.total as tsibble')
    
      period = dataPeriod( data )
      
      key.cols = setdiff( group_by_cols , period )
  
      data = data %>% 
        as_tsibble( index = !! rlang::sym( period )  ,
                  key =  all_of(  {{ key.cols }} ) )
    }
    
    # Testing
    # saveRDS( data.total(), 'data.total.hts.rds' )
  
    # exogenous variables
    
     if ( .cat) cat( "\n - covariates:",  covariates  )
     if ( .cat) cat( "\n - names(data):",  paste( names(data) , collapse = ", ")  )
     
     # if ( !any(is.null( covariates ))  ){
       
         # xreg.var = covariates 
          
    # Use admin hts
    if ( hts ){
      if (.cat) cat( "\n - hierarchical aggregations ")
    
          if ( any( grepl( "avg_mm", covariates ) ) && ! any( grepl( "rdt_so", covariates ) ) ) data = data %>%
              aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                              total = sum( total , na.rm = T ) ,
                              avg_mm = mean( !!rlang::parse_expr( 'avg_mm' ) , na.rm = T )
                              # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
              )
          
          if ( any( grepl( "rdt_so", covariates )) && ! any ( grepl( "avg_mm", covariates ) ) ) data = data %>%
              aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                              total = sum( total , na.rm = T ) ,
                              rdt_so = mean( !!rlang::parse_expr( 'rdt_so' ) , na.rm = T )
                              # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
              )
          
          if ( any( grepl( "avg_mm", covariates ) ) && any( grepl( "rdt_so", covariates ) ) ) data = data %>%
              aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                              total = sum( total , na.rm = T ) ,
                              avg_mm = mean( !!rlang::parse_expr( 'avg_mm' ) , na.rm = T ) ,
                              rdt_so = mean( !!rlang::parse_expr( 'rdt_so' ) , na.rm = T ) 
                              # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
              )
          
          
        if ( !any( grepl( "avg_mm", covariates ) ) && !any( grepl( "rdt_so", covariates ) ) )  data = data %>%
            aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                          total = sum( total , na.rm = T )
                          )
    }
      
    if ( .cat ) cat('\n - end htsData(): ' , paste( names( data) , collapse = ", ") ) 
  
    if ( timing ) cat( toc()$callback_msg )

    return( data )
  }

    
aggData = function( data.total = NULL , 
                    covariates = NULL , 
                    group_by_cols = NULL ,
                    .cat = FALSE , 
                    timing = FALSE , ... ){
  
  if ( .cat ) cat('\n* aggData:' )
  
  if ( .cat ) cat('\n - covariates:' , covariates)
  if ( .cat ) cat('\n - group_by_cols: ' , group_by_cols )
  # Testing
  # saveRDS( data.total , "data.total.rds")
  
  if ( timing ) tic() 
  
  if ( is.null( data.total ) ){
    if ( .cat ) cat('\n - end htsData(): data is missing '  ) 
    return( data.total )
  } 
  
  if ( is.null( group_by_cols )) group_by_cols = groupByCols( period = dataPeriod( data )  )
  # if ( "orgUnit" %in% group_by_cols ) group_by_cols = setdiff( group_by_cols , "orgUnit" )
  
  # Testing
  # saveRDS( data.total(), 'data.total.hts.rds' )
  
  # exogenous variables
  
  # if ( !any(is.null( covariates ))  ){
  
  # xreg.var = covariates 
  
  # Use admin hts
     if (.cat) cat( "\n - aggregations ")
  
    summary_cols = NULL
    if ( ! any( is.null( covariates  ) )) summary_cols = c( summary_cols, covariates )
    if (.cat) cat( "\n - summary_cols: ", summary_cols )
    
    if ( any( nchar( summary_cols  ) > 1 ) ){
      data = data.total %>%
          group_by_at( vars( {{ group_by_cols }} ) )  %>%
          summarise(  
            across( total , \(x) sum(x, na.rm = TRUE ) ) 
            , across( all_of( summary_cols ) , \(x) mean(x, na.rm = TRUE ) )  
          )
    } else {
      if (.cat) cat( "\n - no summary_cols")
      data = data.total %>%
          group_by_at( vars( {{ group_by_cols }} ) )  %>%
          summarise(  
            across( total , \(x) sum(x, na.rm = TRUE ) )  
          )
    }
    
  if ( timing ) cat( toc()$callback_msg )
    
  if ( !is_tsibble( data ) ){
      
      if ( .cat ) cat('\n - preparing data.total as tsibble')
      
      period = dataPeriod( data )
      
      key.cols = setdiff( group_by_cols , period )
      
      data = data %>% 
        as_tsibble( index = !! rlang::sym( period )  ,
                    key =  all_of(  {{ key.cols }} ) )
    }
  
  return( data )
}


trendData = function( .d = data.hts , 
                      reportingSelectedOUs = NULL , 
                      period = "Month" ,
                      startingMonth = NULL ,
                      endingMonth = NULL , 
                      selected.only = TRUE ,
                      num_facilities = NULL , 
                      num_datasets = NULL , 
                      levelNames = NULL , 
                      agg_level = NULL ,
                      split = 'None' ,
                      covariates = NULL , 
                      remove.aggregate = TRUE , 
                      scale = FALSE ,
                      .cat = FALSE ){

      if ( .cat ) cat( '\n* data Functions.R: trendData(): ' )
  
      # Testing
      # saveRDS( .d, "trendData.d.rds")

      if (  is.null( period ) ) period = dataPeriod( .d )
      
      .d. = .d
      
      if ( selected.only  & num_facilities > 1 ){
    
          if ( .cat ) cat( '\n - Show Selected (mostFrequeltylReporting) only' )
    
          .d. = .d %>% filter(
            Selected ==  'Reporting Each Period' )
          
          # make tibbles of covariate values to add back , if needed
          # Selected.d = .d. %>% 
          #   as_tibble() %>% 
          #   select_at( c( setdiff( key_vars( .d )  , "Selected"  ), "Month" , covariates ) ) %>%
          #   as_tsibble( key =  setdiff( key_vars( .d )  , "Selected"  ) , index = "Month" )
          # 
          # notSelected.d = .d %>% anti_join( .d. , by =c( key_vars( .d ) , "Month" ) ) %>%
          #   as_tibble() %>% 
          #   select_at( c( setdiff( key_vars( .d )  , "Selected"  ), "Month" , covariates ) ) %>%
          #   as_tsibble( key =  setdiff( key_vars( .d )  , "Selected"  ) , index = "Month" )
          
       if ( ! is.null( covariates )) covariate.data = .d %>% as_tibble() %>%
            # filter(  Selected ==  'Reporting Each Period' ) %>%
            group_by_at(  c( setdiff( key_vars( .d )  , c("Selected", "agegrp")  ), "Month" ) ) %>%
            summarise_at( covariates , median, na.rm = TRUE )
          
      } 
        
      
      if ( period %in% 'Month' & !is.null( startingMonth )  & !is.null( endingMonth ) ){
        
            if ( ! "yearmonth" %in% class( startingMonth ) ) startingMonth = as.yearmonth( startingMonth ) 
            if ( ! "yearmonth" %in% class( endingMonth ) ) endingMonth = as.yearmonth( endingMonth ) 
              
            .d. = .d. %>% filter(
              Month >=   startingMonth    ,
              Month <=  endingMonth    )
          }
    
      if ( period %in% 'Week'  & !is.null( startingMonth )  & !is.null( endingMonth ) ){
            .d. = .d. %>% filter(
              Week >=  yearweek( startingMonth )   ,
              Week <= yearweek( endingMonth )  )
          }
    
  
      if ( !is.null( agg_level ) ){
        
        if ( .cat ) cat( "\n - input$agg_level:", agg_level )
    
        sub_agg = levelNames[ which( agg_level == levelNames ) + 1 ] 
        if ( .cat ) cat( "\n - sub agg level" , sub_agg )
        
        .d. = .d. %>% 
            filter( 
              ! is_empty( !! rlang::sym( agg_level   ) ) ,
              ! is.na( !! rlang::sym( agg_level   ) ) 
              # next line is good for level 0
              # ,  ! is_aggregated(  !! rlang::sym( agg_level   ) )
            )
            
      }     
 
      # if ( !is_empty( sub_agg ) ){
      #   if ( .cat) cat( '\n - filtering by sub_agg' )
      #   .d = .d %>% filter(
      #         is_aggregated( !! rlang::sym( sub_agg  ) )
      #   )
      # }
      
         # preserve tsibble key and index,
         indexVar = index_var( .d. )
         keyVars = key_vars( .d. )
        
        .d. = .d. %>%
           mutate( 
             grouping_var = 'Total' ) %>%
             # ensure tsibble before using fill_gaps
             as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
             fill_gaps( .full = TRUE  )
        
        # once gaps are filled with NA, need to add back values of covariates
        if ( ! is.null( covariates )){
          .d. = .d. %>% 
          select_at(  setdiff( names( .d. ) , covariates  ) ) %>% 
          left_join( covariate.data,  by = c( setdiff( names( covariate.data), covariates ) ) ) 
        
        }
        
        # glimpse( .d.. )
        # .d..  %>% filter( Cluster %in% "Chiyendausiku", Month == march ) %>% View()
        
         if ( .cat ) cat( '\n - .d in trendData' ) # glimpse(.d)
         
        # Testing:
        # saveRDS(.d ,  "trendData.d.rds")
      
         # num_datasets = length( unique( .d$dataSet ))
         if ( num_datasets > 1 ){
           
           if ( .cat ) cat( "\n - num_datasets:", num_datasets )
             
           .d. = .d. %>%
           filter( !is_aggregated( dataSet ) ) %>%
           mutate( dataSet = as.character( dataSet ) %>%
               str_remove_all( "<aggregated>" ) ,
               grouping_var = dataSet )

         }
    
         if ( num_facilities > 1 ){
           
           if ( .cat ) cat( "\n - num_facilities:", num_facilities )
           
           .d. = .d. %>%
           filter( !is_aggregated( Selected )  ) %>%
           mutate( Selected = as.character( Selected ) %>%
               str_remove_all( "<aggregated>" )  )

           if ( .cat ) cat( '\n- Facilities:' ,  unique(.d$Selected) )
         }
            
        # if split, remove aggregate grouping
        if ( .cat ) cat( '\n - split:', split ) 
         
        if ( ! 'None' %in% split  && length( split ) == 1 ) {
           
           if ( .cat ) cat( '\n - input split:' , split )
           
           .d. = .d. %>%
             filter( !is_aggregated( !! rlang::sym( split ) ) 
             ) %>%
             mutate( grouping_var = as.character( 
               !! rlang::sym( split ) )
             )
           
           if ( remove.aggregate ) .d = .d %>%
             filter( !is_aggregated( !! rlang::sym( split ) ) 
             ) 
           
         } 
    
      # if ( .cat ) cat( '\n- nrow(.d)' , nrow(.d))
         
        # if ( !split() %in% 'None' & !input$filter_data %in% 'All' ){
        #     print( 'filter_data is not null' )
        #     .d = .d %>% 
        #       filter( .data[[ split() ]] %in% input$filter_data )
        # }
      
      if ( scale ){
         if ( .cat ) cat( '\n - scale:' )
        
        .d. = .d. %>%
          ungroup() %>%
          group_by( grouping_var ) %>%
          mutate(
            total = scale( total ) + 1
        ) }
      
      
      # ensure tsibble before using fill_gaps
      if ( .cat ) cat( '\n - as_tsibble:')
        
      .d. = .d. %>% as_tsibble( key = all_of(keyVars) , index = indexVar  ) 
      
      if ( .cat ) cat( '\n - end trend data():'); # print( glimpse( .d ) ); # print(.d)
      
      # Testing
      # saveRDS( .d , 'trendData.rds' )
  
  return( .d. )
}


model_formula = function( model = "ARIMA" , 
                          modelSpecs = NULL , transform = FALSE , period = "Month" ,
                          covariates = NULL , .cat = FALSE ){
      
      if ( .cat ) cat("\n* data Functions.R model_formula:")
  
      if ( model %in% 'TSLM (trend+season)' ){
        if ( .cat ) cat("\n - model = TSLM")
        
        formula.string  = "total ~ trend() + season()" 
        
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend() + season()'
        
      }
      
      if ( model %in% 'TSLM (trend)' ){
        if ( .cat ) cat("\n - model = TSLM (trend)")
        
        formula.string  =  "total ~ trend()" 
        
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend()'
        
      }

      if ( model %in% 'ARIMA' ){
        if ( .cat ) cat("\n - model = ARIMA")
        
        # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
        #                         ' pdq() ' )
        
        formula.string = ' total ~  pdq() '
        
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
        
        if ( period %in% "Month" ) formula.string = paste0( formula.string ,
                                                              '+ PDQ( period = "1 year" )'   )
        
        if ( period %in% "Week" ) formula.string = paste0( formula.string ,
                                                             '+ PDQ( period = 52 )'   )
        
        
        if ( !is_empty( covariates ) && any ( nchar( covariates ) > 0 ) ) formula.string =
          paste( formula.string , '+ xreg(' , paste( covariates , collapse = " + " ) , ' ) '  )
        
      }
      
      if ( model %in% 'BSTS' ){
        if ( .cat ) cat("\n - model = BSTS")
        
        formula.string  =  'total ~ season("year")' 
        
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season()'
        
      }
      
      if ( model %in% 'ETS' ){
        if ( .cat ) cat("\n - model = ETS")
        
        formula.string = 'total ~ error() + trend() + season()'  
    
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
        
      }
      
      if (model %in% 'NNETAR' ){
        if ( .cat ) cat("\n - model = NNETAR")
        
        formula.string = 'total'  
        
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  )'
        
      }
      
      if (model %in% 'SNAIVE' ){
        if ( .cat ) cat("\n - model = SNAIVE")
        
        formula.string = 'total ~ lag("year")'  
      
        if ( transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ lag("year")'
        
      }
      
      if (.cat) cat("\n - formula:" , formula.string )
      return( formula.string )
    }

tsPreModel = function( trend.data ,  
                       evaluation_month , 
                       model = "ARIMA" , 
                       formula.string = NULL , 
                       period = "Month", 
                       .cat = TRUE , ...){

      if ( .cat ) cat( '\n* data Function.R tsPreModel():' , model )

      if ( period %in% "Month" ) time_period = as.yearmonth( evaluation_month  ) - 12
      if ( period %in% "Week" ) time_period = as.yearweek( evaluation_month ) - 52

      if ( .cat ) cat("\n - evaluation begins:" , as.character( evaluation_month ) )
      if ( .cat ) cat("\n - training data ends:" , as.character( time_period ) )

      # fit.data  = trend.data %>%
      #   filter_index( ~ as.character( time_period ) ,
      #                 .preserve = TRUE )

      # if ( .cat ) cat("\n - nrow(trendData()):" , nrow( trend.data )  )
      # if ( .cat ) cat("\n - nrow(fit.data:" , nrow( fit.data )  )
      
      fit.data  = trend.data %>%
          filter_index( ~ as.character( time_period ) ,
                  .preserve = TRUE )
  
      if ( .cat ) cat("\n - fit.data ends:" , as.character( max( fit.data$Month ) ) )

  
      # Testing:
      # saveRDS( trendData() , 'trendData.rds' )
      # saveRDS( fit.data , 'fit.data.rds' )

      if ( is.null( formula.string ) ) formula.string  = model_formula( model , ... )
      if ( .cat ) cat( "\n - formula.string:" , formula.string )
      
      if ( grepl( "~" ,  formula.string , fixed = TRUE ) ) formula.string = as.formula (formula.string )

      if ( model %in% 'TSLM (trend)' ){
        fit = fit.data %>% model( l = TSLM( formula.string  ) )

        if ( .cat ) cat( '\n - end tsPreModel() TSLM(trend):' )
        return( fit )
      }

      if ( model %in% 'TSLM (trend+season)' ){
        fit = fit.data %>% model( l = TSLM( formula.string ) )

        if ( .cat ) cat( '\n - end tsPreModel() TSLM(trend + season):' )
        return( fit )
      }

      if ( model %in% 'ARIMA' ){
        # if ( .cat ) cat( '\n - fit.data names:' , paste( names(fit.data), collapse = ', ') )
   
        fit = fit.data %>% model(
          arima = ARIMA( formula.string  )
          )
      # if ( input$reconcile ) fit = fit %>%
      #       reconcile(
      #         mint = min_trace(a, method = "mint_shrink")
      #         )

        if ( .cat ) cat( '\n - end tsPreModel(): arima fit' )
        # glimpse( fit )
        # testing model fit for forecasts

        # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

        return( fit )
      }
      
      if ( model %in% 'NNETAR' ){
        fit = fit.data %>%
          model(
            nnetar = NNETAR( total  )
          )
        
        if ( transform ) fit = fit.data %>% model( nnetar = NNETAR( fabletools::box_cox( total , lambda = .5  )  ) )
        
        
        if ( .cat ) cat( '\n - end tsModel():' )
        return( fit )
      }
      
      if ( model %in% 'BSTS' ){
        fit = fit.data %>%
          model(
            # b = BSTS( model_formula() )
            bsts = BSTS( formula.string )
            )

        if ( .cat ) cat( '\n - end tsPreModel() BSTS:' )
        return( fit )
      }

      if ( model %in% 'ETS' ){
        
        fit = fit.data %>% model( ets = ETS( !! formula.string ))
    
         # if ( input$reconcile ) fit = fit %>%
        #       reconcile(
        #         mint = min_trace(a, method = "mint_shrink")
        #         )
        if ( .cat ) cat( '\n - end tsModel():' )
        return( fit )
      }
      
      if ( model %in% 'SNAIVE' ){
        
        fit = fit.data %>% model( ets = SNAIVE(  formula.string ) )
        
        if ( .cat ) cat( '\n - end tsModel():' )
        return( fit )
      }

      if ( model %in% 'Prophet' ){


        if ( transform ){
          
          fit =  fit.data %>% model(
                prophet = prophet( fabletools::box_cox( total , lambda = .5  )  ~

                                        growth( type = 'linear',
                                                changepoint_range = 1 ,
                                                changepoint_prior_scale = 1 ,
                                                # capacity = 1e5 ,
                                                # floor = 0
                                                ) +
                                        season(period = 12,
                                               order = 4 ,
                                               type='multiplicative'),
                                   seed = TRUE )
            ) 
        } else {
          
          fit =  fit.data %>% model(
            prophet = prophet( total ~
                                 
                                 growth( type = 'linear',
                                         changepoint_range = 1 ,
                                         changepoint_prior_scale = 1 ,
                                         # capacity = 1e5 ,
                                         # floor = 0
                                 ) +
                                 season(period = 12,
                                        order = 4 ,
                                        type='multiplicative'),
                               seed = TRUE )
          )
          
          
        }

        if ( .cat ) cat( '\n - end tsPreModel() Prophet:' )
        return( fit )
      }


    }

tsPreForecast = function( trend.data , preModel , 
                          horizon = 12 , 
                              evaluation_month = NULL  , 
                              period = "Month" , covariates = NULL ,
                              split = NULL , agg_level = NULL ,
                          prob = FALSE , 
                          pi_levels = .89 ,
                          .cat = FALSE ){

      if ( .cat ) cat( '\n* data Functions.R tsPreForecast' )


      if ( period %in% "Month" ){
        time_start = as.yearmonth( evaluation_month ) - horizon + 1
        time_end =  as.yearmonth( evaluation_month ) 
      }
  
      if ( period %in% "Week" ){ 
        time_period = yearweek( evaluation_month  ) 
        horizon = 52 }
  
      if ( .cat ) cat( "\n - evaluation period:", as.character(time_start) , "-" , as.character(time_end) )
  

      test.data  = trend.data %>%
          select( - total ) %>%
          filter( Month >= time_start  ,  Month <= time_end )
      
          # filter_index(  format(time_start,  format = "%Y-%m")  ~  format(time_end,  format = "%Y-%m")  ,
          #             .preserve = TRUE )
      
      if ( .cat ) cat( "\n - test.data period:", as.character(min( test.data$Month)) , "-" , as.character(max( test.data$Month)) )

        # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
        #          bootstrap = FALSE , Reps = 1000 )

        # if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
        # if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )
        
      if ( prob ){
            
        fcast = preModel %>%  fabletools::forecast( new_data = test.data   , level = pi_levels ) 

      } else { 
        
        fcast = preModel %>% fabletools::forecast( new_data = test.data ) 
        }

      # preserve tsibble key and index,
      indexVar = index_var( fcast )
      keyVars = key_vars( fcast )

      if ( ! is.null( agg_level ) ){
      if ( .cat ) cat( '\n - tsPreForecast done.  Adding agg_level' )

        fcast = fcast %>%
            mutate( !! agg_level :=
                      as.character( !! rlang::sym( agg_level  ) ) )
        
      }

      if ( .cat ) cat( '\n - tsPreForecast grouping_var' , split )
      if ( any( !split %in% 'None' ) && length( split ) == 1 ){
           
        fcast = fcast %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split ) )
             )

      } else {
           fcast = fcast %>%
             mutate(  grouping_var = 'Total' )
         }
      if ( .cat ) cat( '\n - tsPreForecast grouping_var values:' , unique(fcast$grouping_var) )

      # Ensure result is tsibble
      # fcast = fcast %>%
      #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
      #        fill_gaps( .full = TRUE  )

      if ( .cat ) cat( '\n - tsPreForecast done.' )
      # print( names( fcast ) )
      
      # Testing:
      # saveRDS( fcast , 'tsPreForecast.rds' )
      
      return( fcast )
}


MAPE = function( preForecast , trend.data , period = "Month" , var = ".mean" , .cat = FALSE ){
       
        if( .cat ) cat('\n* evaluation_widget MAPE()')

        predicted = preForecast %>% as_tibble() %>% select(-total)
        actual =  trend.data
        
        cols = intersect( names( predicted) , names(actual))
        
        d = predicted %>%
           inner_join( actual , by = cols )

        e = d %>% as_tibble() %>%
              # group_by( orgUnit , data  )  %>%
              summarise(
                mape = ifelse( mean( total , na.rm = T ) > 0 ,
                             mean( abs( total - {{ var }} ) , na.rm = T ) /
                             mean( total , na.rm = T ) ,
                             NA )
                         )

        if (.cat ) cat('\n* - ', e$mape )
        
        return( scales::percent( e$mape )  )

      }


key.mape = function( preForecast , trend.data , period = "Month" , 
                     split = 'None' , agg_level = NULL , 
                     horizon = 12, .cat = FALSE  ){
 
        if (.cat) cat('\n* data Functions.R  key.mape()')

        predicted = preForecast %>%
          rename( pred = .mean )

        actual =  trend.data %>%
          rename( actual = total )

        keyvars = key_vars( actual )
        if (.cat)  cat('\n - keyvars' , keyvars )

        truth = predicted %>%
          # select( -dataCol, -grouping_var ) %>%
           inner_join( actual , by = c( period , keyvars  ) )

        if (.cat)  cat( '\n - truth'); #print( truth )

        mid_point = round( as.integer( horizon ) /2  )

            # summarise 
    if ( 'orgUnit' %in% keyvars ){ 
      
      group_no_orgUnit = setdiff( keyvars, 'orgUnit' )
      
      truth = truth %>% as_tibble() %>%
        group_by_at( group_no_orgUnit ) 
      
    } else { 
        truth = truth %>%
          group_by_key() %>%
          index_by( 1 ) 
    }
        
        e = truth %>%
          summarise(
                mape = ifelse( mean( pred , na.rm = T ) > 0 ,
                             mean( abs( actual - pred ) ,
                                   na.rm = T ) /
                             mean( pred , na.rm = T ) ,
                             NA ) ,
                !! rlang::sym( period ) := nth( !! rlang::sym( period )  , mid_point ) ,
                actual = ifelse( mape>=0 , max( actual, na.rm = TRUE ),
                                 min( actual, na.rm = TRUE  )
                                 #nth( actual , mid_point )
                ) ,
                just = ifelse( mape >= 0 , 2, -2 )
                ) %>%
        as_tibble() 
        
        if (! is.null( agg_level ) ) e = e %>% 
            mutate( !! agg_level :=
                      as.character( !! rlang::sym( agg_level  ) ) )

        if (  any( !split %in% 'None' ) && length( split ) == 1 ){
           if (.cat)  cat( '\n - key.mape grouping_var' , split )
           e = e %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split ) )
             )
     } else {
           e = e %>%
             mutate(  grouping_var = 'Total' )
     }
        
        # print( "end key.mape"); #glimpse(e )
        return( e )
}

pre_impact_fit = function(
    ml.data = ml.data , 
    startingMonth = "Jan 2015" ,
    endingMonth = NULL ,
    selected.only = FALSE ,
    missing_reports = 0 , 
    split = NULL ,
    evaluation_month = "Dec 2018" ,
    error = "seasonal3" ,
    hts = TRUE ,
    aggregate = TRUE , 
    levelNames = NULL ,
    agg_level =  NULL , # levelNames[2] # if NULL, while ignore admin levels 
    pi_levels = 89 , # for plot trends...
    model = "ARIMA" ,
    modelSpecs = NULL , 
    transform = FALSE ,
    covariates = NULL ,
    simulate = FALSE , 
    bootstrap = FALSE , 
    times = 100 ,
    remove.aggregate = TRUE  , 
    .cat = TRUE 
){
    cat( "\n* data Functions.R pre_impact_fit:")
    period = dataPeriod( ml.data )
    
    # prepare data with error and selected facilities
    d = ml.data %>% 
      error_factor %>% 
      cleanedData( . , 
                   error =  error , .cat = .cat ) %>%
      
      selectedData(  startingMonth = startingMonth ,
                     endingMonth = endingMonth ,
                     missing_reports = missing_reports ,
                     .cat = .cat ) %>% as_tibble()
    
    # d %>% group_by( Include , RTSS,  Selected ) %>%
    #   summarise( facilities = n_distinct( orgUnit ) ,
    #              errorFlags = sum( value & is.na( dataCol ) ) ,
    #              `%` = percent( errorFlags / sum( value ) , 0.1 )
    #   )
  
    group_by_cols = groupByCols( period = dataPeriod( d ), 
                                 hts = hts , 
                                 levelNames = levelNames, 
                                 split = split , .cat = .cat  )
    
   
    num_facilities = mostFrequentReportingOUs( ml.data ,
                                               startingMonth = startingMonth ,
                                               endingMonth = endingMonth ,
                                               missing_reports = missing_reports , 
                                               .cat = .cat ) %>% length()
    
    dataSets = unique( mvip.ml$dataSet ) 
    num_datasets = length( dataSets )
    
    data.total = dataTotal( data = d , 
                            group_by_cols = group_by_cols , 
                            dataSets = dataSets, 
                            covariates = covariates , 
                            .cat = .cat  )
  
    if ( hts ){
      
      hts_formula = htsFormula( hts = hts , 
                                levelNames = levelNames ,  
                                agg_level = agg_level , 
                                all.levels = FALSE  , 
                                num_facilities =  num_facilities ,
                                num_datasets = num_datasets , 
                                split = split , .cat = .cat ) 
      
      data.agg.ts = htsData( data = data.total ,  
                          hts = hts , 
                          hts_formula = hts_formula , 
                          covariates = covariates , 
                          group_by_cols = group_by_cols , .cat = .cat )
    } else {
      
      data.agg.ts = aggData( data.total = data.total ,  
                             covariates = covariates , 
                             group_by_cols = group_by_cols , .cat = .cat )
    }
    
  
    trend.data = trendData( .d = data.agg.ts , 
                          levelNames = levelNames , 
                          startingMonth = startingMonth , 
                          endingMonth = endingMonth ,
                          selected.only = selected.only  ,
                          num_facilities =  num_facilities ,
                          num_datasets = num_datasets , 
                          split = split ,
                          agg_level = agg_level  , 
                          remove.aggregate = remove.aggregate, .cat = .cat  )
  
    formula.string = model_formula(
      model = model , 
      modelSpecs = modelSpecs , 
      transform = transform , 
      period = period ,
      covariates = covariates , .cat = .cat 
    )
    
    options( future.rng.onMisuse = "ignore")
  

  # cat( "\n *** saving pre_model_diagnostics.rda")
  # save( data.total, data.hts, group_by_cols, dataSets , covariates, 
  #       trend.data, evaluation_month, model, formula.string, transform,
  #       file =  "pre_model_diagnostics.rda")
  # cat( "\n *** -done")
  
  
    if (.cat) tictoc::tic()
    preModel = tsPreModel( trend.data ,  
                           evaluation_month = evaluation_month , 
                           model = model  , 
                           formula.string = formula.string ,
                           transform = transform , .cat = .cat )
    
   
    cat("\n\n") 
    if (.cat) tictoc::toc()
  
    # testing
    # cat("\n - saving preModel")
    # saveRDS( preModel , "preModel.rds" )
    
  # data.orgUnit = data.tota %>% as_tsibble( index = "Month", key = c('orgUnit') )
  # 
  # preModel.orgUnit = tsPreModel( dt ,  
  #                                evaluation_month = evaluation_month , 
  #                        model = model  , transform = TRUE )
  
  # Remove any NULL models 
    null_models = grepl( fixed("NULL") , 
                         preModel %>% select( ncol(preModel)) %>% pull ) 
    
    if ( all( null_models ) ){
      m = tibble( 
        mape = NA %>% as.numeric() , 
        evaluation_month = evaluation_month , 
        startingMonth = startingMonth  , 
        missing_reports = missing_reports ,
        # num_facilities = num_facilities , 
        agg_level = agg_level , 
        error = error, 
        model = model, 
        transform = transform  ,
        covariates = paste( covariates , collapse = " + " ) 
      )
      return( m )
    }
    
    preModel = preModel[ ! null_models, ]
    
    preForecast = tsPreForecast( trend.data , 
                                 preModel , 
                                 horizon = 12 , 
                                 evaluation_month = evaluation_month , 
                                 period = "Month" , 
                                 covariates = covariates ,
                                 split = split , 
                                 agg_level = agg_level , .cat = .cat  )
    
    m = key.mape( preForecast , trend.data , split = split , 
                  agg_level = agg_level,  .cat = .cat ) %>%
      # select( - .model, - `1` , - just , - grouping_var , -Month, -actual   ) %>%
      mutate( 
        evaluation_month = evaluation_month , 
        startingMonth = startingMonth  , 
        missing_reports = missing_reports ,
        # num_facilities = num_facilities , 
        agg_level = agg_level , 
        error = error, 
        model = model, 
        transform = transform  ,
        covariates = paste( covariates , collapse = " + " ) 
      )
  
}

tsModel = function( trend.data ,  
                    evaluation_month , 
                    model = "ARIMA" , 
                    formula.string = NULL , 
                    period = "Month", .cat = TRUE , ...){
  
  if ( .cat ) cat( '\n* data Function.R tsModel():' , model )
  
  if ( period %in% "Month" ) time_period = as.yearmonth( evaluation_month   ) 
  if ( period %in% "Week" ) time_period = yearweek( evaluation_month  ) 
  
  if ( .cat ) cat("\n - training data ends:" , as.character( time_period ) )
  if ( .cat ) cat("\n - training data keys:" , key_vars( trend.data ) )
  
  fit.data  = trend.data %>%
    filter_index( ~ as.character( time_period ) ,
                  .preserve = TRUE )
  
  # if ( .cat ) cat("\n - fit.data ends:" , as.character( max( fit.data$Month ) ) )

  # Testing:
  # saveRDS( trendData() , 'trendData.rds' )
  # saveRDS( fit.data , 'fit.data.rds' )
  
  if ( is.null( formula.string ) ) formula.string  = model_formula( model , ... )
  if ( .cat ) cat( "\n - formula.string:" , formula.string )
  
  if ( grepl( "~" ,  formula.string , fixed = TRUE ) ) formula.string = as.formula (formula.string )
  
  if ( model %in% 'TSLM (trend)' ){
    fit = fit.data %>% model( l = TSLM( formula.string  ) )
    
    if ( .cat ) cat( '\n - end tsPreModel() TSLM(trend):' )
    return( fit )
  }
  
  if ( model %in% 'TSLM (trend+season)' ){
    fit = fit.data %>% model( l = TSLM( formula.string ) )
    
    if ( .cat ) cat( '\n - end tsPreModel() TSLM(trend + season):' )
    return( fit )
  }
  
  if ( model %in% 'ARIMA' ){
    # if ( .cat ) cat( '\n - fit.data names:' , paste( names(fit.data), collapse = ', ') )
    
    fit = fit.data %>% model(
      arima = ARIMA( formula.string  )
    )
    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )
    
    if ( .cat ) cat( '\n - end tsPreModel(): arima fit' )
    # glimpse( fit )
    # testing model fit for forecasts
    
    # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )
    
    return( fit )
  }
  
  if ( model %in% 'NNETAR' ){
    fit = fit.data %>%
      model(
        nnetar = NNETAR( total  )
      )
    
    if ( transform ) fit = fit.data %>% model( nnetar = NNETAR( fabletools::box_cox( total , lambda = .5  )  ) )
    
    
    if ( .cat ) cat( '\n - end tsModel():' )
    return( fit )
  }
  
  if ( model %in% 'BSTS' ){
    fit = fit.data %>%
      model(
        # b = BSTS( model_formula() )
        bsts = BSTS( formula.string )
      )
    
    if ( .cat ) cat( '\n - end tsPreModel() BSTS:' )
    return( fit )
  }
  
  if ( model %in% 'ETS' ){
    
    fit = fit.data %>% model( ets = ETS( !! formula.string ))
    
    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )
    if ( .cat ) cat( '\n - end tsModel():' )
    return( fit )
  }
  
  if ( model %in% 'SNAIVE' ){
    
    fit = fit.data %>% model( ets = SNAIVE(  formula.string ) )
    
    if ( .cat ) cat( '\n - end tsModel():' )
    return( fit )
  }
  
  if ( model %in% 'Prophet' ){
    
    
    if ( input$transform ){
      
      fit =  fit.data %>% model(
        prophet = prophet( fabletools::box_cox( total , lambda = .5  )  ~
                             
                             growth( type = 'linear',
                                     changepoint_range = 1 ,
                                     changepoint_prior_scale = 1 ,
                                     # capacity = 1e5 ,
                                     # floor = 0
                             ) +
                             season(period = 12,
                                    order = 4 ,
                                    type='multiplicative'),
                           seed = TRUE )
      ) 
    } else {
      
      fit =  fit.data %>% model(
        prophet = prophet( total ~
                             
                             growth( type = 'linear',
                                     changepoint_range = 1 ,
                                     changepoint_prior_scale = 1 ,
                                     # capacity = 1e5 ,
                                     # floor = 0
                             ) +
                             season(period = 12,
                                    order = 4 ,
                                    type='multiplicative'),
                           seed = TRUE )
      )
      
      
    }
    
    if ( .cat ) cat( '\n - end tsPreModel() Prophet:' )
    return( fit )
  }
  
  
}

tsForecast = function( trend.data , 
                        Model , 
                          horizon = 12 , 
                          evaluation_month = NULL  , 
                          period = "Month" , covariates = NULL ,
                          split = NULL , agg_level = NULL ,
                          simulate = FALSE , 
                          bootstrap = FALSE , 
                          times = 100 ,
                          pi_levels = .89 ,
                          .cat = FALSE ){
  
  if ( .cat ) cat( '\n* data Functions.R tsForecast' )
  
  
  if ( period %in% "Month" ){
    time_start = as.yearmonth( evaluation_month  ) 
    time_end =  time_start + horizon - 1
  }
  
  if ( period %in% "Week" ){ 
    time_period = yearweek( evaluation_month  ) 
    horizon = 52 }
  
  if ( .cat ) cat( "\n - evaluation period:", as.character(time_start) , "-" 
                   , as.character(time_end) , "(" ,
                   time_end - time_start , "months)")
  
  
  test.data  = trend.data %>%
    select( - total ) %>%
    filter( Month >= ( time_start )  ,  Month <= time_end )
  
  # filter_index(  format(time_start,  format = "%Y-%m")  ~  format(time_end,  format = "%Y-%m")  ,
  #             .preserve = TRUE )
  
  if ( .cat ) cat( "\n - test.data period:", as.character(min( test.data$Month)) , "-" , as.character(max( test.data$Month)) )
  
  # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
  #          bootstrap = FALSE , Reps = 1000 )
  
  # if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
  # if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )
  

  fcast = Model %>%  fabletools::forecast( new_data = test.data   , 
                                             simulate = simulate , 
                                             bootstrap = bootstrap ,
                                             times = times ) 
    
  
  # preserve tsibble key and index,
  indexVar = index_var( fcast )
  keyVars = key_vars( fcast )
  
  if ( ! is.null( agg_level ) ){
    if ( .cat ) cat( '\n - Adding agg_level' )
    
    fcast = fcast %>%
      mutate( !! agg_level :=
                as.character( !! rlang::sym( agg_level  ) ) )
    
  }
  
  if ( .cat ) cat( '\n - grouping_var' , split )
  if ( any( !split %in% 'None' ) && length( split ) == 1 ){
    
    fcast = fcast %>%
      mutate(
        grouping_var = as.character( !! rlang::sym( split ) )
      )
    
  } else {
    fcast = fcast %>%
      mutate(  grouping_var = 'Total' )
  }
  if ( .cat ) cat( '\n - tsPreForecast grouping_var values:' , unique(fcast$grouping_var) )
  
  # Ensure result is tsibble
  # fcast = fcast %>%
  #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
  #        fill_gaps( .full = TRUE  )
  
  if ( .cat ) cat( '\n - tsPreForecast done.' )
  # print( names( fcast ) )
  
  # Testing:
  # saveRDS( fcast , 'tsPreForecast.rds' )
  
  return( fcast )
}


impact_fit = function(
    # ml.data = ml.data , 
    # startingMonth = "Jan 2015" ,
    # horizon = 12 , 
    # endingMonth = NULL ,
    # selected.only = FALSE ,
    # missing_reports = 0 , 
    # split = NULL ,
    # evaluation_month = "Dec 2018" ,
    # error = "seasonal3" ,
    # hts = TRUE ,
    # aggregate = TRUE , 
    # levelNames = NULL ,
    # agg_level =  NULL , # levelNames[2] # if NULL, while ignore admin levels 
    # prob = FALSE ,
    # pi_levels = 89 , # for plot trends...
    # model = "ARIMA" ,
    # simulate = FALSE , 
    # bootstrap = FALSE , 
    # times = 100 ,
    # distribution = FALSE , 
    # modelSpecs = NULL , 
    # transform = FALSE ,
    # covariates = NULL ,
    # remove.aggregate = TRUE  , 
    # .cat = TRUE ,
    ...
){
  cat( "\n* data Functions.R impact_fit:")
  period = dataPeriod( ml.data )
  
  # prepare data with error and selected facilities
  d = ml.data %>% 
    error_factor %>% 
    cleanedData( . , 
                 error =  error , .cat = .cat ) %>%
    
    selectedData(  startingMonth = startingMonth ,
                   missing_reports = missing_reports ,
                   endingMonth = endingMonth, .cat = .cat  ) %>% as_tibble()
  
  # d %>% group_by( Include , RTSS,  Selected ) %>%
  #   summarise( facilities = n_distinct( orgUnit ) ,
  #              errorFlags = sum( value & is.na( dataCol ) ) ,
  #              `%` = percent( errorFlags / sum( value ) , 0.1 )
  #   )
  
  group_by_cols = groupByCols( period = dataPeriod( d ), 
                               hts = hts , 
                               levelNames = levelNames, 
                               split = split , .cat = .cat  )
  
  
  num_facilities = mostFrequentReportingOUs( d ,
                                             startingMonth = startingMonth ,
                                             missing_reports = missing_reports, 
                                             .cat = .cat ) %>% length()
  
  dataSets = unique( mvip.ml$dataSet ) 
  num_datasets = length( dataSets )
  
  data.total = dataTotal( data = d , 
                          group_by_cols = group_by_cols , 
                          dataSets = dataSets, 
                          covariates = covariates , 
                          .cat = .cat  )
  
  if ( hts ){
    
    hts_formula = htsFormula( hts = hts , 
                              levelNames = levelNames ,  
                              agg_level = agg_level , 
                              all.levels = FALSE  , 
                              num_facilities =  num_facilities ,
                              num_datasets = num_datasets , 
                              split = split , .cat = .cat ) 
    
    data.agg.ts = htsData( data = data.total ,  
                           hts = hts , 
                           hts_formula = hts_formula , 
                           covariates = covariates , 
                           group_by_cols = group_by_cols , .cat = .cat )
  } else {
    
    data.agg.ts = aggData( data.total = data.total ,  
                           covariates = covariates , 
                           group_by_cols = group_by_cols , .cat = .cat )
  }
  
  
  trend.data = trendData( .d = data.agg.ts , 
                          levelNames = levelNames , 
                          startingMonth = startingMonth , 
                          endingMonth = endingMonth ,
                          selected.only = selected.only  ,
                          num_facilities =  num_facilities ,
                          num_datasets = num_datasets , 
                          split = split ,
                          agg_level = agg_level  , 
                          remove.aggregate = remove.aggregate, .cat = .cat  )
  
  formula.string = model_formula(
    model = model , 
    modelSpecs = modelSpecs , 
    transform = transform , 
    period = period ,
    covariates = covariates , .cat = .cat 
  )
  
  options( future.rng.onMisuse = "ignore")
  
  
  # cat( "\n *** saving pre_model_diagnostics.rda")
  # save( data.total, data.hts, group_by_cols, dataSets , covariates, 
  #       trend.data, evaluation_month, model, formula.string, transform,
  #       file =  "pre_model_diagnostics.rda")
  # cat( "\n *** -done")
  
  
  if (.cat) tictoc::tic()
  Model = tsModel( trend.data ,  
                         evaluation_month = evaluation_month , 
                         model = model  , 
                         formula.string = formula.string ,
                         transform = transform , .cat = .cat )
  
  
  cat("\n\n") 
  if (.cat) tictoc::toc()
  
  # data.orgUnit = data.tota %>% as_tsibble( index = "Month", key = c('orgUnit') )
  # 
  # preModel.orgUnit = tsPreModel( dt ,  
  #                                evaluation_month = evaluation_month , 
  #                        model = model  , transform = TRUE )
  
  # Remove any NULL models 
  null_models = grepl( fixed("NULL") , 
                       Model %>% select( ncol(Model)) %>% pull ) 
  if (.cat) cat("\n - Of" , length( null_models ), 
                "models, there were" , sum( null_models ), "null models" )
  
  Model = Model[ ! null_models, ]
  
  if (.cat) cat("\n - evaluation_month" , evaluation_month ) 
  if (.cat) cat("\n - horizon" , horizon ) 
  
  Forecast = tsForecast( trend.data , 
                               Model , 
                               horizon = horizon  , 
                               evaluation_month = evaluation_month , 
                               period = "Month" , 
                               covariates = covariates ,
                               split = split , 
                               agg_level = agg_level , 
                               simulate = simulate , 
                               bootstrap = bootstrap , 
                               times = times ,
                               .cat = .cat  )
  
  if ( distribution ){
    cat( "\n - key.mpe.distribution")
    x = key.mpe.distribution( Forecast , trend.data , times = times,  .cat = .cat )
    i = x$e.dist 
  } else {
    i = key.mpe( Forecast , trend.data , split = split , 
                agg_level = agg_level,  .cat = .cat )
    x = NULL
  }
  
  i = i %>%
    # select( - .model, - `1` , - just , - grouping_var , -Month, -actual   ) %>%
    mutate( 
      evaluation_month = evaluation_month , 
      startingMonth = startingMonth  , 
      missing_reports = missing_reports ,
      # num_facilities = num_facilities , 
      agg_level = agg_level , 
      error = error, 
      model = model, 
      transform = transform  ,
      covariates = paste( covariates , collapse = " + " ) 
    )
  
  return( list( i = i , Forecast = Forecast , x = x ) )
  
}

getForecast = function( forecastData , 
                        model = NULL , 
                        model.string = NULL , 
                        transform = TRUE ,
                        lambda = .5 , 
                        covariates = NULL ,
                        horizon = 12 ,
                        bootstrap = FALSE, Reps = 1000 ,
                        future.seed=TRUE ,
                        split = 'None' ,
                        agg_level = NULL ,
                        agg_method = "None" ,
                        .period = "Month" ,
                        eval_date = as.yearmonth('Jan 2021' ) ,
                        .cat = FALSE ){ 
      
      if ( .cat ) cat( '\n* tsForecast()' )
      
      # if ( bootstrap ){
      #   # remove null models because throws error...
      #   .model = model[ which( !is_null_model( model$arima ) ), ]
      #   
      #   fcast = .model %>%
      #     forecast( new_data = test_data , 
      #               # simulate = TRUE ,
      #               bootstrap = TRUE, 
      #               times = Reps  )
      #   
      # } else {
      #   fcast = .model %>%
      #     forecast( new_data = test_data  ) 
      # }
      
      if ( .period %in% "Month" ) time_period = as.yearmonth( eval_date , "%Y %b" ) # - month(1)
      if ( .period %in% "Week" ) time_period = yearweek( eval_date  )

      fit.data  = forecastData %>%
        filter_index( ~ as.character( time_period ) ,
                      .preserve = TRUE )
      
      # test.data  = forecastData %>%
      #   filter_index( as.character( time_period ) ~  ,
      #                 .preserve = TRUE )
      
      if ( is.null( model.string ) ) model.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() + PDQ() '
      # model.string = 'total  ~  pdq() + PDQ()'
      
      if ( transform ){ 
        model.string = paste( 'fabletools::box_cox( total , lambda =', lambda, ') ~  pdq() ' )
      } else { 
        model.string = 'total ~  pdq() '
        } 

      if (.period %in% "Month" ) model.string = paste0( model.string ,
                                                             '+ PDQ( period = "1 year" )'   )
  
      if ( .period %in% "Week" ) model.string = paste0( model.string ,
                                                             '+ PDQ( period = 52 )'   )
      
      
      if ( !any(is.null( covariates ))) model.string =
             paste( model.string , '+ xreg(' , covariates , ' ) '  )
      
      if ( .cat ) cat( '\n - model:' , model.string )

      fit = fit.data %>% model(
          arima = ARIMA( as.formula( model.string ) )
      )
      
     if ( bootstrap ){

        fcast = fit %>%
          forecast( h = as.numeric( horizon ) ,
                    bootstrap = TRUE,
                    times = as.integer( Reps )
          )
      } else {
        
        if ( !any(is.null( covariates )) ){
        
          forecast.fit.data  = forecastData %>%
            select( - total ) %>%
            filter_index( as.character( time_period ) ~ . ,
                        .preserve = TRUE ) %>%
            filter( 
              Month > time_period ,
              Month <= ( time_period + horizon )  )
          
          fcast = fit %>% forecast( new_data = forecast.fit.data  )
          
        } else {
          
          if ( .cat ) cat( '\n - forecast horizon' , horizon )
          fcast = fit %>% forecast( h = as.numeric( horizon ) )
        }
          
      }
      
      # preserve tsibble key and index,
      # indexVar = index_var( fcast )
      # keyVars = key_vars( fcast )
      #   
      # 
      # fcast = fcast %>%
      #     mutate( !! agg_level := 
      #               as.character( !! rlang::sym( agg_level  ) ) )
      # 
      # if ( !split %in% 'None' ){
      #      cat( '\n - tsForecast grouping_var' , split() ) 
      #      fcast = fcast %>%
      #        mutate( 
      #          grouping_var = as.character( !! rlang::sym( split() ) ) 
      #        )
      # } else {
      #      fcast = fcast %>%
      #        mutate(  grouping_var = 'Total' )
      # } 
      # 
      # # Ensure result is tstible
      # fcast = fcast %>%
      #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
      #        fill_gaps( .full = TRUE  )
      # 
      # # Reconcile
      # if ( input$agg_method %in% "None" ){ 
      #   if ( input$agg_method %in% 'Bottom up' ){
      #       fcast = fcast %>%
      #         reconcile( bu = bottom_up(base) ) 
      #   } 
      #   if ( input$agg_method %in% 'MINT(ols)' ){
      #     fcast = fcast %>%
      #         reconcile( ols = min_trace(base, method = "ols") ) 
      #   } 
      #   if ( input$agg_method %in% 'MINT(cov)' ){
      #     fcast = fcast %>%
      #         reconcile( mint = min_trace(base, method = "mint_cov") ) 
      #   } 
      # }
        
      # saveRDS( fcast , 'tsForecast.rds')
      if ( .cat ) cat( '\n - fcast end:' );  #glimpse( fcast )
  
      return( fcast )
}


key.mpe = function( Forecast_data , 
                     test_data , 
                     period = "Month" , 
                     .split = 'None' , 
                     by_month = FALSE , 
                     agg_level = NULL , 
                     horizon = NULL , 
                     var = ".mean" , 
                      .cat = FALSE  ){
  
  if (.cat) cat('\n* data Functions.R  key.mpe()')
  
  period = dataPeriod( test_data )
  
  predicted = Forecast_data %>%
    rename( pred = {{ var }} )
  
  actual =  test_data %>%
    rename( actual = total )
  
  keyvars = key_vars( actual )
  if (.cat)  cat('\n - keyvars' , keyvars )
  
  truth = predicted %>%
    # select( -dataCol, -grouping_var ) %>%
    inner_join( actual %>% select_at( c(keyvars , period , "actual" ) ), 
                by = c( period , keyvars  ) )
  
  if (.cat)  cat( '\n - truth'); #print( truth )
  
  if ( ! is.null( horizon ) ) mid_point = round( as.integer( horizon ) /2  )
  
  # summarise 
  if ( 'orgUnit' %in% keyvars ){ 
    
    group_no_orgUnit = setdiff( keyvars, 'orgUnit' )
    
    truth = truth %>% as_tibble() %>%
      group_by_at( group_no_orgUnit ) 
    
  } else { 
    truth = truth %>%
      group_by_key() %>%
      index_by( 1 ) 
  }
  
  if ( .split != 'None' )  .split = c( keyvars , .split ) 
  if ( by_month ) .split = c( keyvars , "Month" )
  
  e = truth %>% as_tibble %>% 
    # mutate( Month = as.character( Month ) ) %>%
    group_by_at( .split ) %>%
    summarise(
      # mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
      #                mean( actual - pred  ,
      #                      na.rm = T ) /
      #                  mean( pred , na.rm = T ) ,
      #                NA ) ,
      # 
      # !! rlang::sym( period ) := nth( !! rlang::sym( period )  , mid_point ) ,
      # 
      # actual = ifelse( mpe>=0 , max( actual, na.rm = TRUE ),
      #                  min( actual, na.rm = TRUE  )
      #                  #nth( actual , mid_point )
      # ) ,
      
      predicted = sum( pred ) ,
      # pred_hilo80 = `80%` ,
      # pred_hilo95 = `95%` ,
      pred_upper = sum( `80%`$upper ) ,
      pred_lower = sum( `80%`$lower  ) 
      , actual = sum( actual )
      , diff = actual-predicted
      , abs_diff = abs( diff )
      ,  mpe = ifelse( is.na( predicted ) , NA , diff / predicted )
      ,  mape = ifelse( is.na( predicted ) , NA , abs_diff / predicted )
      , .model = max(.model) 
      , just = ifelse( mpe >= 0 , 2, -2 )
    ) %>%
    as_tibble() 
  
  if (! is.null( agg_level ) ) e = e %>% 
    mutate( !! agg_level :=
              as.character( !! rlang::sym( agg_level  ) ) )
  
  if (  any( ! .split %in% 'None' ) && length( .split ) == 1 ){
    if (.cat)  cat( '\n - key.mpe grouping_var' , .split )
    e = e %>%
      mutate(
        grouping_var = as.character( !! rlang::sym( .split ) )
      )
  } else {
    e = e %>%
      mutate(  grouping_var = 'Total' )
  }
  
  # print( "end key.mpe"); #glimpse(e )
  return( e )
}
  
# key.mpe = function( forecastData = tsForecast , actualData = td , 
#                       .period = 'Month' ,
#                       horizon = 12 ,
#                       agg_level = NULL ,
#                       levelNames = NULL , 
#                       split = 'None' ,
#                       .cat = FALSE ){
# 
#         if ( .cat ) cat('\n* evaluation_widget key.mpe()')
# 
#         predicted = forecastData %>%
#           rename( pred = .mean )
# 
#         actual =  actualData %>%
#           rename( actual = total )
# 
#         keyvars = key_vars( actual )
#         if ( .cat ) cat('\n - keyvars' , keyvars )
# 
#         truth = predicted %>%
#            inner_join( actual , by = c( .period, keyvars  ) )
# 
#         # print( 'truth'); #print( truth )
# 
#         mid_point = round( as.integer( horizon ) /2  )
#         
#         if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
# 
#         if ( is.null( agg_level ) ) agg_level = levelNames[1] 
# 
#         e = truth %>%
#           group_by_key() %>%
#           index_by( 1 ) %>%
#           summarise(
#                 mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
#                              mean( actual - pred  , na.rm = T ) /
#                              mean( pred , na.rm = T ) ,
#                              NA ) ,
#                 !! .period   := nth( !! rlang::sym( .period )  , mid_point ) ,
#                  actual = ifelse( mpe<=0 , max( actual, na.rm = TRUE ),
#                                  min( actual, na.rm = TRUE  )
#                                  #nth( actual , mid_point )
#                 ) ,
#                 pred = ifelse( mpe<=0 , max( pred, na.rm = TRUE ),
#                                  min( pred, na.rm = TRUE  )
#                                  #nth( actual , mid_point )
#                 ) ,
#                 just = ifelse( mpe >= 0 , 1, -1 )
#                 ) %>%
#         as_tibble()  %>%
#             mutate( !! agg_level :=
#                       as.character( !! rlang::sym( agg_level  ) ) )
# 
#       if ( !split %in% 'None' ){
#            if ( .cat ) cat( '\n - key.mape grouping_var' , split )
#            e = e %>%
#              mutate(
#                grouping_var = as.character( !! rlang::sym( split ) )
#              )
#        } else {
#              e = e %>%
#                mutate(  grouping_var = 'Total' )
#            }
# 
#         if ( .cat ) cat( "\n - mpe"  ); #glimpse(e )
#         return( e )
#       }
      
plotTrends = function( trend.data , 
                       scale = FALSE , 
                         legend = FALSE , 
                         label = FALSE ,
                         facet_split = FALSE ,
                         facet_admin = FALSE ,
                         agg_level = 'National' ,
                         pre_evaluation = FALSE ,
                         evaluation = FALSE ,
                         horizon = 12 ,
                         eval_date = as.yearmonth('Jan 2021', "%Y %b") ,
                         pe = TRUE ,  
                       .cat = FALSE , 
                       ... ){

          if ( .cat ) cat( '\n* data Functions.R plotTrends():' )

          .limits =
          if ( scale ){
            c( NA , NA ) } else {
              c( 0 , NA )
          }

          # data.text = paste( unique( plotData$data ), collapse = " + " )
          
          if ( .cat ) cat( '\n - ploTrends .d:') ; #glimpse(.d)

          # if ( !input$filter_display %in% 'All' ) .d = .d %>%
          #         filter( .data[[ split() ]] %in%
          #                   input$filter_display )


          .period = dataPeriod( trend.data )

      ## Main plot ####
          g = trend.data %>%
          filter( !is.na( total ) ) %>%
          # autoplot( total ) +
          ggplot( aes( x = !! rlang::sym( .period ), y = total

                     , group =  grouping_var # as.character( !! rlang::sym( input$agg_level  ) )

                     , color =  grouping_var
                    ) )  +
          geom_line() +
          theme_minimal()

          # Testing
          # save(.d, file = 'plot-trend-test-data.rda')


          if ( .cat ) cat( '\n - basic plot done' ) 

          if ( !legend ) g = g +
            theme( legend.position = "none")

          if ( label ){
            g = g + geom_label_repel(
                       data = .d %>%
                         filter(
                           !! rlang::sym( .period ) == max( .d %>% pull( .period ) ,
                                                            na.rm = T )
                           ) ,
                       aes( label = grouping_var ,
                            group = grouping_var )
                       )
          }

          # Determine number of agg levels available
          # If only one, do not facet (causes error, perhaps because of autoplot?)

          num_agg_levels = count( trend.data %>% as_tibble ,
                                  !! rlang::sym( agg_level ) ) %>%
            nrow()

          # if ( input$agg_level != levelNames()[1] & input$facet_admin ){
          if ( num_agg_levels  > 1 & facet_admin ){
            if ( .cat )  cat( '\n -  admin facets' )

            if ( facet_split ){
              if ( .cat )  cat( '\n -  facet admin - split' )

                g = g +
                    facet_grid( rows = vars( as.character( !! rlang::sym( agg_level ) ) ) ,
                                cols = grouping_var   ,
                                   scales = "free_y" )
          } else {

            g = g +
            facet_wrap( vars( as.character( !! rlang::sym( agg_level ) ) ) ,
                           scales = "free_y" )
          }} else {

            if ( facet_split ){
            if ( .cat ) cat( '\n - facet_split' )
            g = g +
            # facet_wrap( ~ grouping_var   ,
            facet_grid( vars(  RTSS , pbo )   , scales = "free_y" )
          }
            }

          # Time scale
          if ( .cat ) cat( '\n - Evaluation: setting x axis time scale', .period )
          if ( .period %in% 'Month') g = g + scale_x_yearmonth("", 
                                                               date_labels = "%b\n%Y" ,
                                                               date_breaks = "1 year" )
          # Default for weeks seems ok - 6 months
          # if ( .period %in% 'Week') g = g + scale_x_yearweek("", date_breaks = "1 year" )

          g = g +
            scale_y_continuous( label = comma, limits = .limits ) +
            scale_color_discrete( drop = TRUE  ) +
            labs( y = "" , x="" 
                  # title = str_wrap( indicator , 200 ) ,
                  # subtitle = str_wrap( data.text , 200 )
                  # caption =  str_wrap( caption.text() , 200 )
                  )
          if ( .cat ) cat( '\n - axis scales and labs done' )

          # Eval Date
        #     cat( '\n - evaluation date' , input$evaluation_month )
        #     if ( .period %in% 'Month' ) eval_date =   yearmonth( input$evaluation_month  )
        #     if ( .period %in% 'Week' ) eval_date =   yearweek( input$evaluation_month  )
        #     cat( '\n - eval_date:' , eval_date )
        
      # ## Pre-Evaluation trend line #####
      if ( pre_evaluation ){
          if ( .cat ) cat( '\n - pre-evaluation line.  ' )
          if ( .cat ) cat( '\n - pi_levels:' , pi_levels() )

          if ( .cat ) cat( '\n - pre-evaluation date'  )
          if ( .period %in% 'Month' ) pre_eval_date =   as.yearmonth( input$evaluation_month , "%Y %b" ) -12
          if ( .period %in% 'Week' ) pre_eval_date =   yearweek( input$evaluation_month  ) - 25
          if ( .cat ) cat( '\n - pre_eval_date:' , pre_eval_date )

          g = g +
             forecast::autolayer( tsPreForecast()
                       # , level = c(80,90) # ci_levels()
                       , PI = TRUE
                       , color = 'black'
                       , linetype = 'dotted'  , size = 2
                       ,  alpha = .75 ) +
            # geom_line( data = tsPreForecast(), aes(  y = .mean )
            #   # ,   color = 'light blue'
            #   , alpha = .75
            #   , linetype = 'dotted'  , size = 2
            # ) +
            # geom_vline( xintercept = as.Date( pre_eval_date ) ,
            #             color = 'brown' ,
            #             alpha = .25 ) +
            geom_vline( xintercept = as.Date( eval_date ) ,
                        color = 'black', alpha = 1 )

            # if ( input$pe ) g = g +
            #   geom_label_repel( data =  key.mape() ,
            #            aes(  x = !! rlang::sym( period() ) , y = actual ,
            #            label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
            #            hjust = just ) ,
            #            # force_pull = 0 ,
            #            segment.colour = NA
            #            )
          
            if ( .cat ) cat( '\n - pre-evaluation line done' )

          }




      ## Evaluation Trend Line ####
        if ( evaluation ){
          
           if ( .cat ) cat( '\n - evaluation with horizon - ' , horizon )
           tsForecast = getForecast( plotData , horizon = horizon ,  ... )
          
            if ( .cat ) cat( '\n - evaluation line.  ')
            # cat( '\n - evaluation line.  ' , 'pi_levels:' , pi_levels() )

           g = g +
            forecast::autolayer( tsForecast
                       # , level = pi_levels()
                       , color = 'black'
                       # , linetype = 'dashed'
                       , size = 1
                       ,  alpha = .5
                       ) +
            # geom_line( data = tsForecast() , aes( y = .mean )
            #   # ,   color = 'light blue'
            #   , alpha = .75
            #   , linetype = 'dotted'  , size = 2
            # ) +

            geom_vline( xintercept = as.Date( eval_date ) ,
                        color = 'blue', alpha = 1 )

            # annotate( "text" ,
            #           x = as.Date( eval_date ) ,
            #           y = Inf ,
            #           hjust = 0 , vjust = 1 ,
            #           label = paste( "MPE:\n" )
            #           ) +

           
            mpeData = key.mpe( forecastData = tsForecast , actualData = plotData  )
            
            if ( pe ) g = g +
              geom_label_repel( data =  mpeData ,
                       aes(  x = !! rlang::sym( .period ) , y = pred ,
                       label = paste( "MPE:" , percent( mpe, accuracy = 1.0 ) ) ,
                       hjust = just
                       ) ,
                       # force_pull = 0 ,
                       segment.colour = NA
                       )
          }

          if ( .cat ) cat( '\n - evaluation line done' )

      ## End ####
          if ( .cat ) cat( '\n - end plotTrends():' )

          # saveRDS( g, 'plotTrends.rds')
          return( g )
        }
    