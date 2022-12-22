
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


# cleanedData equivalent to MG2 reactive function d()
cleanedData = function( data1 , 
                        .effectiveLeaf = TRUE , 
                        source = 'Original' , 
                        algorithm = 'seasonal3' ,
                        .cat = FALSE ){

      if ( .cat ) cat( '\n* d:')
    
      .period = period( data1 )
      
      if ( .cat ) cat( '\n - filtering by effectiveLeaf' , .effectiveLeaf )
      data1 = data1 %>% filter( effectiveLeaf == .effectiveLeaf )

      if ( nrow( data1 ) == 0 ){
        if ( .cat ) cat('\n - data1 has zero rows')
        return()
      } 

      if ( .cat ) cat('\n - period is', .period )
      
      # TODO for speed -- use data.table ....
      data = data1  %>% mutate( period = !!rlang::sym( .period ) )
      
    if ( source %in% 'Original' ){
      if ( .cat ) cat('\n- source is original')
      data = data %>% mutate( dataCol = original )
    }  
      
    if ( ( source %in% 'Cleaned' ) & ( algorithm %in% names(data) ) ){
      
      if ( .cat ) cat( '\n-' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      
      data = data %>% 
        mutate( dataCol = ifelse( !! rlang::sym( algorithm ) , original, NA  ) )
      
      # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
      if ('mad15' %in% names( data )) data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
      if ('mad10' %in% names( data )) data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
      if ('mad5' %in% names( data )) data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
      if ('seasonal5' %in% names( data )) data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
      if ('seasonal3' %in% names( data )) data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
      
      if ( .cat ) cat( '\n-' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
    
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
    
    # Remove rows where data not translated correctly.  data is 'NA_'
    if ( .cat ) cat( '\n - removing rows where data is NA_: ', sum( data$data %in% "NA_" ) , 'rows')
    
    # TODO speed up with data.table
    data = data %>% filter( ! data %in% 'NA_') 
    
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
    return( data )
}

mostFrequentReportingOUs <- function( 
                          data ,  
                          endingMonth = NULL , 
                          startingMonth = NULL ,
                          period = NULL ,
                          missing_reports = 0 ,
                          count.any = TRUE , 
                          all_categories = TRUE , 
                          data_categories = NULL ,
                         .cat = FALSE ){
  
    if ( .cat ) cat( '\n* selectedOUS' )
    
    if ( is.null( period ) ) period = dataPeriod( data )
    
    if ( is.null( startingMonth ) ) startingMonth = min( data$period , na.rm = TRUE )
    if ( is.null( endingMonth ) ) endingMonth = max( data$period , na.rm = TRUE )
    
    # TODO for speed -- use data.table ....
    
    if ( !count.any & !all_categories  ){
          if ( .cat ) cat( '\n - input$all_categories:'  )
          data = data %>% filter( data %in% data_categories )
       }
  
    if ( period %in% 'Month' ){
         data = data %>% as_tibble %>%
         filter( 
           Month >=  yearmonth( startingMonth , format = "%Y%m" )  ,
           Month <=  yearmonth( endingMonth , format = "%Y%m" )  
                 )
       } 
      
    if ( period %in% 'Week' ){
         if ( .cat ) cat( '\n - selectedOUS by Week')
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
    
    mr = data %>%
        filter( !is.na( original  ) ,   ) %>%
        group_by( year = year( Month ) ) %>%
        distinct( !! rlang::sym( period ) , orgUnit ) %>%
        group_by( year , orgUnit ) %>%
        summarise( months = n() )
    
       
       #print( "mr" ); #print( summary( mr$n ) )
    max_years =  n_distinct( mr$year , na.rm = FALSE ) 
    
    periods_per_year = data %>% 
        distinct( Month ) %>%
        mutate( year = year( Month )) %>%
        count( year ) %>%
        rename( max = n )
    
    s = mr %>%
      inner_join( periods_per_year , by = "year" ) %>%
      ungroup %>%
      group_by( orgUnit ) %>%
      summarise(  
          years = n() ,
          consistent = all( months >= ( max - missing_reports ) ) ,
          champion =  ( years == max_years ) & consistent
        ) %>%
         filter( champion ) %>%
         pull( orgUnit ) %>% unique
       
    if ( .cat ) cat( "\n - number selectedOUs:", length(s), 'orgUnits' ) 
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
#     # if ( length( selectedOUs() > 0 ) ) 
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
                        period = NULL , 
                        levelNames = NULL ,
                        split = NULL , 
                        .cat = FALSE  ){
  
    if( .cat ) cat('\n* groupByCols :')
  
    group_by_cols =  c( period , 'orgUnit', 'Selected',
                        'dataSet' , 'data' ) 
    
    group_by_cols = c( group_by_cols, levelNames )
  
   
    if ( !is.null( split ) && split != 'None' ) group_by_cols = c( group_by_cols , split )
    
    cat( "\n - end group_by_cols()" , unique( group_by_cols )  )
    
    return( unique( group_by_cols ) )

}


# selectedData equivalent to MG2 reactive function plotData()
selectedData = function( data1 ,  
                         levelNames = NULL ,
                         all_categories = TRUE , 
                         data_categories = NULL ,
                         alwaysReporting = TRUE , 
                         missing_reports = 0 ,
                         selectedOUs = NULL ,
                         startingMonth = NULL , 
                         endingMonth = NULL ,
                         source = 'Original' ,
                         level = 'leaf' , 
                         level2 = NULL ,
                         level3 = NULL ,
                         level4 = NULL , 
                         level5 = NULL , 
                         .cat = FALSE ,
                         ... ){

   if ( .cat ) cat( "\n* selectedData:" )
  
    if ( nrow( data1 ) == 0 ){
        cat('\n - data1() has zero rows')
        return()
    }
  
      # NB: setting data = setDT( data1()) has side effect of changing data1() to data.table. 
      data = as.data.table( data1  )
      
      if ( .cat ) cat( '\n - data (d) converted to data.table' )
      
      # period = dataPeriod( data1 )
      # if (.cat ) cat('\n - period is', period )
      # 
      # data = data[ , period := base::get( period )  , ]

      if ( !is_empty( level2 ) ){ data = data[ base::get( levelNames[2] )  %in%  level2 ,, ] }
  
      if ( !is_empty( level3 ) ){ data = data[ base::get( levelNames[3] )  %in%   level3 ,, ] }
  
      if ( !is_empty( level4 ) ){ data = data[ base::get( levelNames[4] )  %in%   level4 ,, ] }
        
      if ( !is_empty( level5 ) ){ data = data[ base::get( levelNames[5] )  %in%   level5  ,, ]  }
    
      if ( level %in% 'leaf' ){  
        
        data = data[ effectiveLeaf == TRUE , , ]
        
      } else {
        
        level. = count( orgUnits() %>% as_tibble, level, levelName ) %>% 
          filter(levelName  %in% input$level  ) %>% pull( level )
        
        data = data[ level  %in% level. , , ] 
      }
      
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
  
    if ( source %in% 'Original' ){
      if ( .cat ) cat('\n - d() source is original')
      
      data = data[ , dataCol := as.numeric( original ) , ] 
    }  
      
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
    
    if ( source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
      if ( .cat ) cat( '\n -' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      
      data = setDT( data )[ , dataCol := fifelse( seasonal3, original, NA_real_  ) , ]
      
      # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
      if ('mad15' %in% names( data )){
        # data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
        data = setDT( data )[, mad15 := fifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) , ] 
        
      }
      
      if ('mad10' %in% names( data )){ 
        # data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
        data = setDT( data )[, mad10 := fifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) , ] 
        
      }
      
      if ('mad5' %in% names( data )){ 
        # data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
        data = setDT( data )[, mad5 := fifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) , ] 
        
      }
      
      if ('seasonal5' %in% names( data )){ 
        # data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
        data = setDT( data )[, seasonal5 := fifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) , ] 
      }
      
      if ('seasonal3' %in% names( data )){ 
        # data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
        data = setDT( data )[, seasonal3 := fifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) , ] 
        
      }
      
      if ( .cat ) cat( '\n -' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
    
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
      
    # filter to selected category
    if ( .cat ) cat( '\n - selectedData filtered by' , data_categories )

    if ( !all_categories )  data = data %>% filter( data %in% data_categories )
   
    if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
   
    # Consistent reporting
    if ( alwaysReporting ){
     
     if ( .cat ) cat( '\n - alwaysReporting' )
     
     if ( is_empty( selectedOUs ) ){
       
       if ( .cat ) cat( "\n - finding most frequently reporting OUs")
         
        selectedOUs  = mostFrequentReportingOUs( data ,
                                             all_categories = all_categories , 
                                             data_categories = data_categories ,
                                             startingMonth = startingMonth , 
                                             endingMonth = endingMonth ,
                                             
                                             )
     } 
   
     # Add var for selected ous
      if ( .cat ) cat( '\n - selectedData length( selectedOUs()): ' , length( selectedOUs ) )
      
      if ( length( selectedOUs ) > 0  ) data = data[ , Selected := fifelse( orgUnit %in% selectedOUs, 
                                                        'Reporting Each Period',
                                                        'Inconsistent Reporting') ] %>%
        as_tibble(.)
      
    # data = data %>% filter( Selected %in% 'Reporting Each Period' )
    } else {
         data = data[ , Selected := "All", ]
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
    period = NULL , 
    startMonth = NULL ,
    endMonth = NULL ,
    merge = NULL ,
    mean.merge = FALSE ,
    .cat = FALSE ){
    
    if ( .cat ) cat( '\n* reporting_widget data.total()' )
  
    if ( is.null( period ) ) period = dataPeriod( data )
      
    if ( !is.null( merge ) && nchar( merge ) > 0 ){
      
      if ( .cat ) cat( "\n - combineSelectDatasets ")
      
      mergeDatasets = merge %>% str_replace_all( fixed("\n"), "") 
      
      if ( any(!is.na( mergeDatasets )) & any(nchar( mergeDatasets ) > 0 )  ){
        
               data = 

                setDT( data )[ any(  mergeDatasets %in% dataSet ) ,  dataSet :=  'Combined'  ] %>%
          
                as_tibble
      }
      
    }
  
      if ( any( grepl( "avg_mm" , names( data ) ) ) ){
          
          if ( .cat )  cat( '\n - with avg_mm' )
        
          data = setDT( data ) %>%
                .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
                        avg_mm = mean( avg_mm , na.rm = TRUE  ) ) , 
                   by =  group_by_cols ] 
          
        } else {
          
          data = setDT( data ) %>%
                .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )) , by =  group_by_cols ] 
        }
      
        
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
      
    #   if ( period %in% 'Month' ){
    # 
    #   data.total = setDT( data )[  which( Month >=  yearmonth( startMonth ) &
    #                                     Month <=  yearmonth( endMonth ) ) , ] 
    #   
    # } 
    # 
    # if ( period %in% 'Week' ){
    # 
    #   data.total = setDT( data )[ which( Week >=  yearweek( startMonth ) &
    #                                     Week <=  yearweek( endMonth ) ) , ] 
    # } 
 
    ## NB does data.total need to be a Tsibble?--it is slow.  
    
    data.total = data[ , total := replace_na( dataCol , 0)  ,] %>%
      as_tibble()

    if ( .cat ) cat('\n - end data.total()')
    
    return( data.total )
    
  }
  
htsFormula = function( 
                levelNames = NULL , 
                agg_level = NULL ,
                hts = TRUE , 
                num_facilities = NULL ,
                num_datasets = NULL ,
                split = 'None' ,
                .cat = FALSE ){   
 
    if ( .cat ) cat("\n* hts():" )

    if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
   
    adms = backtick( levelNames )
    
    if (hts){ 
      hts = paste( adms, collapse = "/" ) 
      
    } else {
      
      if ( .cat ) cat( '\n - adms:',  adms )
      if ( .cat ) cat( '\n - input$agg_level:',  agg_level )
      
      if ( is.null( agg_level ) ) agg_level = levelNames[1] 
      
      hts_level = which( agg_level == levelNames   )
      
      if ( .cat ) cat( '\n - hts_level:',  hts_level )
      
      hts = paste( adms[1:( hts_level + 1 )] , 
                   collapse = "/" ) 
    }
    
    hts = paste( "(" , hts , ")" )
    
    # if >1 Facilities (ie. selected)
    if ( num_facilities > 1 )  hts = paste(
             'Selected *' , hts
             )

    # if >1 dataset
    if ( num_datasets > 1 )  hts = paste(
             'dataSet *' , hts
             )

    # # Cross by split
    if ( !split %in% 'None' ) hts =
      paste( backtick( split ) , '*' ,  hts )
    
    # 
    # Cross by selected and split
    # if ( length( selectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
    #   paste( input$split ,  ' * Facilities * (', hts , ')' )
    
    if ( .cat ) cat("\n - end hts():" , hts )
  
    return( hts )
  }
  
htsData = function( data = NULL , 
                    hts_formula = NULL, 
                    covariates = "" , 
                    group_by_cols = NULL ,
                    period = NULL ,
                    .cat = FALSE , 
                    timing = FALSE , ... ){

    if ( .cat ) cat('\n* data.hts:' )
  
    if ( timing ) tic() 
  
    if ( is.null( hts_formula ) ){
      if ( .cat ) cat('\n - end htsData(): data is missing '  ) 
      return( data )
      } 
  
    if ( is.null( hts_formula ) ){
      if ( .cat ) cat('\n - end htsData(): hts_formula NULL '  ) 
      return( data )
    } 
    
    if ( !is_tsibble( data ) ){
      
      if ( .cat ) cat('\n - preparing data.total as tsibble')
    
      if ( is.null( period ) ) period = dataPeriod( data )
      key.cols = setdiff( group_by_cols , period )
  
      data = data %>% 
        as_tsibble( index = !! rlang::sym( period )  ,
                  key =  all_of(  {{ key.cols }} ) )
    }
    
    # Testing
    # saveRDS( data.total(), 'data.total.hts.rds' )
  
    # exogenous vaiables
     if ( all( covariates %in% names( data ) ) ){
       
      cat( "\n - ",  covariates , "%in% names( data.hts )" )
    
      xreg.var = covariates 
      
      data.hts = data %>%
      aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                      total = sum( total , na.rm = T ) ,
                      # avg_mm = mean( !!rlang::parse_expr( 'avg_mm' ) , na.rm = T )
                      # ipti = sum( !!rlang::parse_expr( 'ipti' ) , na.rm = T ) ,
                      # doses = sum( !!rlang::parse_expr( 'doses' ) , na.rm = T )
                      xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
                      )
    } else {
      
      data.hts = data %>%
      aggregate_key(  .spec = !!rlang::parse_expr( hts_formula ) ,
                      total = sum( total , na.rm = T )
                      )
    }
      
    if ( .cat ) cat('\n - end htsData(): '  ) 
  
    if ( timing ) cat( toc()$callback_msg )

    return( data.hts )
  }
    
trendData = function( .d = data.hts , 
                      selectedOUs = NULL , 
                      period = "Month" ,
                      selected = TRUE ,
                      levelNames = NULL , 
                      split = 'None' ,
                      agg_level = NULL,
                      scale = FALSE ,
                      .cat = FALSE ){

      if ( .cat ) cat( '\n* evaluation_widget: trendData(): ' )
    
      if ( is.null( agg_level ) ){ 
        agg_level = levelNames[1] 
        if ( .cat ) cat( "\n- input$agg_level:", agg_level )
      }
      
      sub_agg = levelNames[ which( agg_level == levelNames ) + 1 ] 
      if ( .cat ) cat( "\n- sub agg level" , sub_agg )
      
      .d = .d %>% 
          filter( 
            ! is_empty( !! rlang::sym( agg_level   ) ) ,
            ! is.na( !! rlang::sym( agg_level   ) ) ,
            # next line is good for level 0
            ! is_aggregated(  !! rlang::sym( agg_level   ) )
          )
              
      if ( .cat ) cat( '\n- !is_empty(sub_agg)' , sub_agg , !is_empty(sub_agg) )

      if ( !is_empty( sub_agg ) ){
        if ( .cat) cat( '\n - filtering by sub_agg' )
        .d = .d %>% filter( 
              is_aggregated( !! rlang::sym( sub_agg  ) )
        )
      }
      
         # preserve tsibble key and index,
         indexVar = index_var( .d )
         keyVars = key_vars( .d )
        
        .d = .d %>%
           mutate( 
             grouping_var = 'Total' ) %>%
             # ensure tsibble before using fill_gaps
             as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
             fill_gaps( .full = TRUE  )
    
         
         if ( .cat ) cat( '\n- .d in trendData' ); # glimpse(.d)
         
         # num_datasets = length( unique( .d$dataSet ))
         # if ( num_datasets > 1 ){
         #   .d = .d %>%
         #   filter( !is_aggregated( dataSet ) ) %>%
         #   mutate( dataSet = as.character( dataSet ) %>%
         #       str_remove_all( "<aggregated>" ) ,
         #       grouping_var = dataSet )
         # 
         # }
    
         # if ( num_facilities > 1 ){
         #   .d = .d %>%
         #   filter( !is_aggregated( Selected )  ) %>%
         #   mutate( Selected = as.character( Selected ) %>%
         #       str_remove_all( "<aggregated>" )  ) 
         # 
         #   cat( '\n- Facilities:' ,  unique(.d$Selected) )
         # }
            
        # if split, remove aggregate grouping
         if ( !split %in% 'None' ){
           if ( .cat ) cat( '\n-input split:' , split )
           .d = .d %>%
             filter( !is_aggregated( !! rlang::sym( split ) ) 
             ) %>%
             mutate( grouping_var = as.character( 
               !! rlang::sym( split ) )
             )
           if ( .cat ) cat( '\n- .d  aggregated split' , unique(.d$grouping_var) )
           # print( glimpse( .d ))
           
         } 
    
      if ( .cat ) cat( '\n- nrow(.d)' , nrow(.d))
         
        # if ( !split() %in% 'None' & !input$filter_data %in% 'All' ){
        #     print( 'filter_data is not null' )
        #     .d = .d %>% 
        #       filter( .data[[ split() ]] %in% input$filter_data )
        # }
      
      if ( scale ) .d = .d %>%
          ungroup() %>%
          group_by( grouping_var ) %>%
          mutate(
            total = scale( total ) + 1
        )
      
      
      # ensure tsibble before using fill_gaps
      .d = .d %>% as_tsibble( key = all_of(keyVars) , index = indexVar  ) 
      
      if ( .cat ) cat( '\n- end trend data():'); # print( glimpse( .d ) ); # print(.d)
      # saveRDS( .d , 'trendData.rds' )
  
  return( .d )
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
                        eval_date = yearmonth('Jan 2021') ,
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
      
      if ( .period %in% "Month" ) time_period = yearmonth( eval_date  ) # - month(1)
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
      
      
      if ( !is.null( covariates )) model.string =
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
        
        if ( !is.null( covariates ) ){
        
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

  key.mpe = function( forecastData = tsForecast , actualData = td , 
                      .period = 'Month' ,
                      horizon = 12 ,
                      agg_level = NULL ,
                      levelNames = NULL , 
                      split = 'None' ,
                      .cat = FALSE ){

        if ( .cat ) cat('\n* evaluation_widget key.mpe()')

        predicted = forecastData %>%
          rename( pred = .mean )

        actual =  actualData %>%
          rename( actual = total )

        keyvars = key_vars( actual )
        if ( .cat ) cat('\n - keyvars' , keyvars )

        truth = predicted %>%
           inner_join( actual , by = c( .period, keyvars  ) )

        # print( 'truth'); #print( truth )

        mid_point = round( as.integer( horizon ) /2  )
        
        if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )

        if ( is.null( agg_level ) ) agg_level = levelNames[1] 

        e = truth %>%
          group_by_key() %>%
          index_by( 1 ) %>%
          summarise(
                mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
                             mean( actual - pred  , na.rm = T ) /
                             mean( pred , na.rm = T ) ,
                             NA ) ,
                !! .period   := nth( !! rlang::sym( .period )  , mid_point ) ,
                 actual = ifelse( mpe<=0 , max( actual, na.rm = TRUE ),
                                 min( actual, na.rm = TRUE  )
                                 #nth( actual , mid_point )
                ) ,
                pred = ifelse( mpe<=0 , max( pred, na.rm = TRUE ),
                                 min( pred, na.rm = TRUE  )
                                 #nth( actual , mid_point )
                ) ,
                just = ifelse( mpe >= 0 , 1, -1 )
                ) %>%
        as_tibble()  %>%
            mutate( !! agg_level :=
                      as.character( !! rlang::sym( agg_level  ) ) )

      if ( !split %in% 'None' ){
           if ( .cat ) cat( '\n - key.mape grouping_var' , split )
           e = e %>%
             mutate(
               grouping_var = as.character( !! rlang::sym( split ) )
             )
       } else {
             e = e %>%
               mutate(  grouping_var = 'Total' )
           }

        if ( .cat ) cat( "\n - mpe"  ); #glimpse(e )
        return( e )
      }
      
plotTrends = function( plotData , scale = FALSE , 
                         legend = FALSE , 
                         label = FALSE ,
                         facet_split = FALSE ,
                         facet_admin = FALSE ,
                         agg_level = 'National' ,
                         pre_evaluation = FALSE ,
                         evaluation = FALSE ,
                         horizon = 12 ,
                         eval_date = yearmonth('Jan 2021') ,
                         pe = TRUE ,  
                       .cat = FALSE , 
                       ... ){

          if ( .cat ) cat( '\n* evaluation_widget plotTrends():' )

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


          .period = period( plotData )

      ## Main plot ####
          g = plotData %>%
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

          num_agg_levels = count( .d %>% as_tibble ,
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
            facet_wrap( ~ grouping_var   ,
                           scales = "free_y" )
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
          if ( .period %in% 'Month' ) pre_eval_date =   yearmonth( input$evaluation_month  ) -12
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
    