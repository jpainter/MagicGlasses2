
period = function( data ){
      cat('\n* period: ')
      
      weekly = any( map_lgl( data ,
                             ~any(class(.x) %in% 'yearweek'  )) )

      period = ifelse( weekly, "Week", "Month" )
      cat( period )
      return( period  )
    }
  
getLevelNames = function( orgUnits ){ 

    if ( 'sf' %in% class( orgUnits ) ) orgUnits = orgUnits %>% st_drop_geometry()
    cat( '\n* levelNames:' )
    l = count( orgUnits %>% as_tibble, level, levelName ) %>% 
      filter( !is.na( level ) ) %>%
      arrange( level ) %>% pull(levelName ) %>% unique
    l = l[ !is.na(l) ]
    cat( '\n - end levelNames:', paste(l, collapse = ", " )  )
    return(l)
}


# cleanedData equivalent to MG2 reactive function d()
cleanedData = function( data1 , source = 'Original' ){

      cat( '\n* d:')
    
      .period = period( data1 )

      if ( nrow( data1 ) == 0 ){
        cat('\n - data1 has zero rows')
        return()
      } 

      cat('\n - period is', .period )
      
      # TODO for speed -- use data.table ....
      data = data1  %>% mutate( period = !!rlang::sym( .period ) )
      
    if ( source %in% 'Original' ){
      cat('\n- source is original')
      data = data %>% mutate( dataCol = original )
    }  
      
    if ( source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
      cat( '\n-' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
      data = data %>% 
        mutate( dataCol = ifelse( seasonal3, original, NA  ) )
      
      # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
      if ('mad15' %in% names( data )) data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
      if ('mad10' %in% names( data )) data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
      if ('mad5' %in% names( data )) data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
      if ('seasonal5' %in% names( data )) data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
      if ('seasonal3' %in% names( data )) data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
      
      cat( '\n-' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
    }  
    
    cat( '\n - nrow( d ):' , nrow( data ))
    
    # Remove rows where data not translated correctly.  data is 'NA_'
    cat( '\n - removing rows where data is NA_: ', sum( data$data %in% "NA_" ) , 'rows')
    
    # TODO speed up with data.table
    data = data %>% filter( ! data %in% 'NA_') 
    
    cat( '\n - nrow( d ):' , nrow( data ))
    return( data )
}

selectedOUs <- function( d ,  endingMonth = NULL , startingMonth = NULL , 
                         count.any = TRUE , all_categories = TRUE , 
                         data_categories = NULL ){
    cat( '\n* selectedOUS' )

    data = d
    
    .period = period( data )
    
    if ( is.null( startingMonth ) ) startingMonth = min( data$period , na.rm = TRUE )
    if ( is.null( endingMonth ) ) endingMonth = max( data$period , na.rm = TRUE )
    
    # TODO for speed -- use data.table ....
    
    if ( !count.any & !all_categories  ){
          cat( '\n - input$all_categories:'  )
          data = data %>% filter( data %in% data_categories )
       }
  
    if ( .period %in% 'Month' ){
         data = data %>% as_tibble %>%
         filter( 
           period >=  yearmonth( startingMonth )  ,
           period <=  yearmonth( endingMonth )  
                 )
       } 
      
    if ( .period %in% 'Week' ){
         cat( '\n - selectedOUS by Week')
         data = data %>% as_tibble %>%
         filter( 
           period >=  yearweek( startingMonth )  ,
           period <=  yearweek( endingMonth )  
                 )
       } 
      

    mr = data %>% 
         filter( !is.na( original  ) ) %>%
         distinct( !! rlang::sym( .period ) , orgUnit ) %>%
         group_by( orgUnit ) %>%
         summarise( n = n() ) %>%
         arrange( desc( n ))
       
       #print( "mr" ); #print( summary( mr$n ) )
  
    s = mr %>%
         filter( n == max( mr$n ) ) %>%
         pull( orgUnit ) %>% unique
       
    cat( "\n - number selectedOUs:", length(s), 'orgUnits' ) 
    return( s )
}  


# selectedData equivalent to MG2 reactive function plotData()
selectedData = function( d,  
                         all_categories = TRUE , 
                         data_categories = NULL ,
                         alwaysReporting = TRUE , ... ){

   cat( "\n* plotData():" )
  
   data = d %>% mutate( Selected = 'All' ) 
    
   if ( alwaysReporting ){
     
     .selectedOUs  = selectedOUs( d , ... )
         # Add var for selected ous
    cat( '\n - selectedData length( selectedOUs()): ' , length( .selectedOUs ) )
    
    if ( length( .selectedOUs  > 0 ) ) data = setDT( data )[ , Selected := fifelse( orgUnit %in% .selectedOUs, 
                                                      'Reporting Each Period',
                                                      'Inconsistent Reporting') ] %>%
      as_tibble(.)
    
   }
   
    # filter to selected category
    cat( '\n - selectedData filtered by' , data_categories )

    if ( !all_categories )  data = data %>% filter( data %in% data_categories )
        
    
    # if ( length( .selectedOUs  > 0 ) ) data = setDT( data )[ , Selected := ifelse(
    #        orgUnit %in% .selectedOUs ,
    #        'Reporting Each Period',
    #        'Inconsistent Reporting' ) ] %>%
    #   as_tibble()
    
    cat( '\n - end  selectedData()' )  ; # #print( names( data )) 
    # TESTING
    # saveRDS( data , "plotData.rds" )
    
  return( data )
}

  
group_by_cols = function( data = NULL , levelNames = NULL, split = NULL, merge = TRUE  ){
    # req( input$split )
    cat("\n* group_by_cols():")
  
   .period = period( data )
    
    group_by_cols =  c(.period , 'orgUnit', 'Selected', 'data' ) 
    
    if ( !merge ) group_by_cols = c( group_by_cols, 'dataSet' )
    
    if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
   
    group_by_cols = c( group_by_cols, levelNames )
  
    cat("\n - group_by_cols():", group_by_cols )
    
    if ( !is.null( split ) ) group_by_cols = c( group_by_cols , split )
    
    # if ( length( selectedOUs() > 0 ) ) 
      # group_by_cols = c( group_by_cols , 'Facilities' )
    
    # # If not merge when total, show separate datsets
    # if ( !input$merge & input$all_categories ) group_by_cols = c( group_by_cols , 'dataSet' )
    #   
    cat( "\n- end group_by_cols()" , unique( group_by_cols )  )
    return( unique( group_by_cols ) )

}


data.total = function( data ,  
                      period = "Month" ,
                      .group_by_cols = NULL ,
                      dataSet = NULL , 
                      merge = FALSE ,
                      dataset_merge_average = FALSE ,
                      startDisplayMonth = NULL  , 
                      endDisplayMonth = NULL 
                       ){
    cat( '\n* data.total():' )
  
    .period = period( data )
  
    cat( '\n - period:' , .period )
    
    .dates = data %>% pull( !!rlang::sym( .period )  )
    if ( is.null( startDisplayMonth ) )  startDisplayMonth = min( .dates , na.rm = TRUE  )
    if ( is.null( endDisplayMonth ) ) endDisplayMonth = max( .dates , na.rm = TRUE   )
    
    if ( is.null( .group_by_cols )) .group_by_cols = group_by_cols( data )
    cat( '\n - data.total .group_by_cols:'  , .group_by_cols )
  
    # Total categories by facilities and datasets
    # data = plotData

      # Merge  datasets 
      # Set all dataSets to Combined and re-summaries taking mean
      # #print( 'data.total datasets' );  #print( dataSets() )
      cat( '\n - merge ', merge )
      cat( '\n - data datsets ' , unique( dataSet) ) 
      
      mergeDatasets = merge %>% str_replace_all( fixed("\n"), "") 
      cat( '\n - mergeDatasets:' , mergeDatasets )
      
     
      if ( merge  ){
  
      combineSelectDatasets = data %>%
                mutate( dataSet = dataSet %>% str_replace_all( fixed("\r\n"), "") 
              ) %>%
                mutate(
                    dataSet = ifelse( 
                        str_replace_all(dataSet, fixed("\n"), "") %in% 
                          mergeDatasets , 'Combined' , dataSet) ,
                    data = 'Total'
                ) %>% 
                setDT() %>%
                .[ , .(dataCol = sum( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
      
      cat('\n - Combining dataSets %in% input$merge:' , mergeDatasets )
      

      } else { combineSelectDatasets = data }
      
      # Testing
      # saveRDS( combineSelectDatasets , 'combineSelectDatasets.rds' )
      
      # data.table sum/mean 
      
      if ( dataset_merge_average ) {
            cat( '\n** merge data.table MEAN') 
            
            dataMerge = combineSelectDatasets %>%
                mutate( dataSet = 'Merged') %>%
                setDT() %>%
                # Mean of dataSets within orgUnit
                .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ] 
  
            cat( '\ndataMerge done' );  # glimpse( dataMerge )
        
      } else {
          dataMerge = combineSelectDatasets
          # cat('\n glimpse( dataMerge )\n' ); #print(glimpse( dataMerge ))
      }
      
      # Testing
      # saveRDS( dataMerge, 'dataMerge.rds' )
      # #print( dataMerge %>% duplicates %>% glimpse )
  
    key.cols = setdiff( .group_by_cols , .period ) 
    cat('\n - key.cols:' ,  key.cols )
    
    data.total = 
        dataMerge %>% 
        # fill_gaps( .full = TRUE  ) %>%
        mutate( 
                total = replace_na( dataCol , 0) 
                )  %>% # for plotting, replace missing with zero 
        as_tsibble( index = !! rlang::sym( .period )  , 
                    key =  all_of(  {{ key.cols }} ) ) 

    cat( '\n - data.total class' , class( data.total ) ) 
    cat( '\n - data.total cols' , names( data.total ) ) 
    
    # Filter display dates
    # cat( '/n - data.total cols:' , names( data.total ) )
    
    if ( .period %in% 'Month' ){
      cat( '\n -  .period %in% Month' )
      data.total = data.total %>% 
        filter( 
          Month >=  yearmonth( startDisplayMonth )  ,
          Month <=  yearmonth( endDisplayMonth )  
        )
    } 
    
    if ( .period %in% 'Week' ){
      cat( '\n -  .period %in% weeks' )
      data.total = data.total %>% 
        filter( 
          Week >=  yearweek( startDisplayMonth )  ,
          Week <=  yearweek( endDisplayMonth )  
        )
    } 
    
  
    # test:
    # saveRDS( data.total, 'data.total.rds')
    
    cat('\n- end data.total()')
    return( data.total )
      
  
}

backtick <- function(x) paste0("`", x, "`")

hts = function( agg_level = NULL , levelNames = NULL , hts = TRUE , 
                .selectedOUs = NULL ,
                split = 'None' ){   
 
    cat("\n* hts():" )

    if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
   
    adms = backtick( levelNames )
    
    if (hts){ 
      hts = paste( adms, collapse = "/" ) 
      
    } else {
      
      cat( '\n - amds:',  adms )
      cat( '\n - input$agg_level:',  agg_level )
      
      if ( is.null( agg_level ) ) agg_level = levelNames[1] 
      
      hts_level = which( agg_level == levelNames   )
      
      cat( '\n - hts_level:',  hts_level )
      
      hts = paste( adms[1:( hts_level + 1 )] , 
                   collapse = "/" ) 
    }
    
    hts = paste( "(" , hts , ")" )

    # num_facilities = length( .selectedOUs )
    # if ( num_facilities > 1 )  
      hts = paste( 
             'Selected *' , hts 
             )
    
    # if >1 dataset 
    # if ( num_datasets > 1 )  
    # hts = paste( 'dataSet *' , hts )
    
    # # Cross by split
    if ( !split %in% 'None' ) hts =
      paste( split , '*' ,  hts )
    # 
    # Cross by selected and split
    # if ( length( selectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
    #   paste( input$split ,  ' * Facilities * (', hts , ')' )
    
    cat("\n - end hts():" , hts )
  
    return( hts )
  }
  
data.hts = function( .d , hts = NULL, ... ){

    if ( is.null( hts ) ) hts = hts()
    cat('\n* data.hts():' )
    
    # Testing
    # saveRDS( data.total(), 'data.total.hts.rds' )
  
   # TODO:  Any way to speed this up???
    .d = .d %>%
      aggregate_key(  .spec = !!rlang::parse_expr( hts ) ,
                      total = sum( total , na.rm = T )
                      ) 
    # }
    
    cat('\n - end data.hts():' ) 

    return(.d)
  }
    
trendData = function( .d = data.hts , 
                      selectedOUs = NULL , 
                      period = "Month" ,
                      selected = TRUE ,
                      levelNames = NULL , 
                      split = 'None' ,
                      agg_level = NULL,
                      scale = FALSE ){

      cat( '\n* evaluation_widget: trendData(): ' )
    
      if ( is.null( agg_level ) ){ 
        agg_level = levelNames[1] 
        cat( "\n- input$agg_level:", agg_level )
      }
      
      sub_agg = levelNames[ which( agg_level == levelNames ) + 1 ] 
      cat( "\n- sub agg level" , sub_agg )
      
      .d = .d %>% 
          filter( 
            ! is_empty( !! rlang::sym( agg_level   ) ) ,
            ! is.na( !! rlang::sym( agg_level   ) ) ,
            # next line is good for level 0
            ! is_aggregated(  !! rlang::sym( agg_level   ) )
          )
              
      cat( '\n- !is_empty(sub_agg)' , sub_agg , !is_empty(sub_agg) )

      if ( !is_empty( sub_agg ) ){
        cat( '\n - filtering by sub_agg' )
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
    
         
         cat( '\n- .d in trendData' ); # glimpse(.d)
         
         # num_datasets = length( unique( .d$dataSet ))
         # if ( num_datasets > 1 ){
         #   .d = .d %>%
         #   filter( !is_aggregated( dataSet ) ) %>%
         #   mutate( dataSet = as.character( dataSet ) %>%
         #       str_remove_all( "<aggregated>" ) ,
         #       grouping_var = dataSet )
         # 
         # }
    
         if ( num_facilities > 1 ){
           .d = .d %>%
           filter( !is_aggregated( Selected )  ) %>%
           mutate( Selected = as.character( Selected ) %>%
               str_remove_all( "<aggregated>" )  ) 
    
           cat( '\n- Facilities:' ,  unique(.d$Selected) )
         }
            
        # if split, remove aggregate grouping
         if ( !split %in% 'None' ){
           cat( '\n-input split:' , split )
           .d = .d %>%
             filter( !is_aggregated( !! rlang::sym( split ) ) 
             ) %>%
             mutate( grouping_var = as.character( 
               !! rlang::sym( split ) )
             )
           cat( '\n- .d  aggregated split' , unique(.d$grouping_var) )
           # print( glimpse( .d ))
           
         } 
    
      cat( '\n- nrow(.d)' , nrow(.d))
         
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
      
      cat( '\n- end trend data():'); # print( glimpse( .d ) ); # print(.d)
      # saveRDS( .d , 'trendData.rds' )
  
  return( .d )
}

getForecast = function( test_data , model = NULL , 
                        bootstrap = FALSE, Reps = 1000 ,
                        future.seed=TRUE ,
                        split = 'None' ,
                        agg_level = NULL ,
                        agg_method = "None" ){ 
      
      cat( '\n* tsForecast()' )
      
      if ( bootstrap ){
        # remove null models because throws error...
        .model = model[ which( !is_null_model( model$arima ) ), ]
        
        fcast = .model %>%
          forecast( new_data = test_data , 
                    # simulate = TRUE ,
                    bootstrap = TRUE, 
                    times = Reps  )
        
      } else {
        fcast = model %>%
          forecast( new_data = test_data ) 
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
      cat( '\n - fcast end:' );  #glimpse( fcast )
  
      return( fcast )
}

  plotTrends = function( plotData , scale = FALSE , 
                         legend = FALSE , label = FALSE ,
                         facet_split = FALSE ,
                         facet_admin = FALSE ,
                         agg_level = 'National' ,
                         pre_evaluation = FALSE ,
                         evaluation = FALSE ){

          cat( '\n* evaluation_widget plotTrends():' )

          .limits =
          if ( scale ){
            c( NA , NA ) } else {
              c( 0 , NA )
          }

          # data.text = paste( unique( plotData$data ), collapse = " + " )
          
          cat( '\n - ploTrends .d:') ; #glimpse(.d)

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


          cat( '\n - basic plot done' ) 

          if ( legend ) g = g +
            theme(legend.position = "none")

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
            cat( '\n -  admin facets' )

            if ( facet_split ){
              cat( '\n -  facet admin - split' )

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
            cat( '\n - facet_split' )
            g = g +
            facet_wrap( ~ grouping_var   ,
                           scales = "free_y" )
          }
            }

          # Time scale
          cat( '\n - Evaluation: setting x axis time scale', .period )
          if ( .period %in% 'Month') g = g + scale_x_yearmonth("", date_breaks = "1 year" )
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
          cat( '\n - axis scales and labs done' )

          # Eval Date
      #     cat( '\n - evaluation date' , input$evaluation_month )
      #     if ( .period %in% 'Month' ) eval_date =   yearmonth( input$evaluation_month  )
      #     if ( .period %in% 'Week' ) eval_date =   yearweek( input$evaluation_month  )
      #     cat( '\n - eval_date:' , eval_date )
      # 
      # ## Pre-Evaluation trend line #####
      if ( pre_evaluation ){
          cat( '\n - pre-evaluation line.  ' )
          cat( '\n - pi_levels:' , pi_levels() )

          cat( '\n - pre-evaluation date'  )
          if ( .period %in% 'Month' ) pre_eval_date =   yearmonth( input$evaluation_month  ) -12
          if ( .period %in% 'Week' ) pre_eval_date =   yearweek( input$evaluation_month  ) - 25
          cat( '\n - pre_eval_date:' , pre_eval_date )

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

            if ( input$pe ) g = g +
              geom_label_repel( data =  key.mape() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual ,
                       label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
                       hjust = just ) ,
                       # force_pull = 0 ,
                       segment.colour = NA
                       )

          }

          cat( '\n - pre-evaluation line done' )


      ## Evaluation Trend Line ####
          if ( evaluation ){
            cat( '\n - evaluation line.  ')
            cat( '\n - evaluation line.  ' , 'pi_levels:' , pi_levels() )

           g = g +
            forecast::autolayer( tsForecast()
                       , level = pi_levels()
                       , color = 'black'
                       , linetype = 'dashed', size = 1
                       ,  alpha = .5 ) +
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

            if (input$pe) g = g +
              geom_label_repel( data =  key.mpe() ,
                       aes(  x = !! rlang::sym( period() ) , y = actual ,
                       label = paste( "MPE:" , percent( mpe, accuracy = 1.0 ) ) ,
                       hjust = just
                       ) ,
                       # force_pull = 0 ,
                       segment.colour = NA
                       )
          }

          cat( '\n - evaluation line done' )

      ## End ####
          cat( '\n - end plotTrends():' )

          # saveRDS( g, 'plotTrends.rds')
          return( g )
        }
    