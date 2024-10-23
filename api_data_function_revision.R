api_data = function(      periods = NA ,
                          periodType = 'Monthly' ,
                          YrsPrevious = 1 ,
                          orgUnits = "LEVEL-1" ,
                          elements = NA ,
                          baseurl = NA , 
                          username = NULL , 
                          password = NULL , 
                          formula = NA , 
                          parallel = FALSE ,
                          print = TRUE ,
                          update = FALSE ,
                          check_previous_years = 2 , 
                          previous_dataset_file = '' ,
                          prev.data =  NA ,
                          level1.id = NA , #when comparing current data with previous
                          dir = country.dir ,
                          shinyApp = FALSE ,
                          childOnly = FALSE ,
                          ...
){
  cat('\n** api_data function...')
  # tic()
  ##### cycle through each period, each data element...
  
  # Periods ####
  # if ( periods %in% 'months_last_year' ) periods = date_code( monthsPrevious = 12 )
  # if ( periods %in% 'months_last_2_years' ) periods = date_code( YrsPrevious = 2 )
  # if ( periods %in% 'months_last_3_years' ) periods = date_code( YrsPrevious = 3 )
  # if ( periods %in% 'months_last_4_years' ) periods = date_code( YrsPrevious = 4 )
  # if ( periods %in% 'months_last_5_years' ) periods = date_code( YrsPrevious = 5 )
  # if ( periods %in% 'weeks_last_5_years' ) periods = date_code_weekly( YrsPrevious = 5 )
  
  cat('\n - setting periods - ') 
  # if ( all( is.na( periods )  ) ) 
  # # check for last x years only 
  if ( periodType == 'Monthly' ) periods = date_code( YrsPrevious = YrsPrevious ) # 'months_last_5_years' # 
  if ( periodType == 'Weekly') periods = date_code_weekly( YrsPrevious = YrsPrevious )
  
  # periods = date_code( YrsPrevious = YrsPrevious )
  cat('\n -  ' , periods )  
  
  # if ( childOnly ){
  #   # for api/dataValueSets current month may be missing
  #   period_vectors = strsplit( periods , ";" , fixed = TRUE )
  #   
  # } else {
    period_vectors = strsplit( periods , ";" , fixed = TRUE )[[1]]
    
  # }

  
  if ( print ) cat( '\n - Requesting data for periods:\n' ,
                    paste( period_vectors , collapse = ';') , "\n" )
  
  if ( update & !file.exists( previous_dataset_file ) ){
    cat('\n - previous data file missing') 
  } 
    
  # if childOnly - i.e. using api/dataSets call to only entered data, 
  # cannot use categoryOptions - just data element.
  if ( childOnly ){
      if ( any( grepl( "." , elements$id )) ){
        elements$id = sub("\\..*", "", elements$id )  
        elements$name = sub("\\..*", "", elements$name ) 
        elements = distinct( elements )
      } 
    }
  
  ## UPDATE data options ####
  cat('\n - testing for previous dataset if update is TRUE:', update )
  if ( update &&  ( file_test("-f", previous_dataset_file ) | !is_empty( prev.data ) ) ){
    
    cat('\n - retrieving details of previous dataset ')
    
    # # check for last x years only 
    if ( periodType == 'Monthly') periods = date_code( YrsPrevious = check_previous_years ) # 'months_last_5_years' # 
    if ( periodType == 'Weekly') periods = date_code_weekly( YrsPrevious = check_previous_years )
    # cat('\n - periodType is' ,  periodType)
    
    # excel version
    # prev.data = read_excel(previous_dataset_file , 
    #                          sheet = 'formulaData') %>%
    #           filter( !is.na( dataElement  ) )
    
    if ( is_empty( prev.data ) && file_test("-f", previous_dataset_file ) ){
      cat('\n - reading previous dataset')
      prev.data = readRDS( previous_dataset_file ) # %>% select( - starts_with('aggr'))
    } 
    
    if ( nrow( prev.data ) == 0 ){
      cat('\n - previous data file empty'); next()
    }  
    
    first.period = min( prev.data$period  )
    last.period =  max( prev.data$period )
    
    # excel version
    # des = prev.data %>% select( dataElement, dataElement.id , Categories ,
    #                      categoryOptionCombo.ids ) %>%
    #   unique 
    
    cat('\n - date range:' , first.period , last.period )
    
    #NB: use *.id if downloaded data was prepared and saved 
    cat('\n - extracting previously downloaded elements'  )
    if ( 'dataElement.id' %in% names( prev.data ) ){
      prev.data = prev.data %>% as_tibble %>% 
        ungroup() %>%
        select(  orgUnit, period , 
                 dataElement.id,  categoryOptionCombo.ids ,
                 COUNT, SUM ) %>%
        rename( dataElement = dataElement.id ,
                categoryOptionCombo = categoryOptionCombo.ids ) %>%
        filter( ! is.na( COUNT ) & COUNT != 0 )  %>%
        distinct 
    } else {
      prev.data = prev.data %>% as_tibble %>%
        ungroup() %>%
        select( orgUnit, period , 
                dataElement,  categoryOptionCombo ,
                COUNT, SUM  ) %>%
        filter( !is.na( COUNT )  ) %>%
        distinct 
    }
    
    # Confirm there is previous data for level1
    prev = prev.data %>%
      filter( orgUnit == level1.id  
              # , period %in% period_vectors 
      ) 
    
    # Stop if no national data 
    if ( nrow( prev ) == 0 ){
      cat( '\n - previous data is missing national data' ) 
      return()
    } else {
      cat( '\n - previous data has:' ,  nrow( prev ), 'rows' ) 
    }
    
    des = prev.data %>% 
      select(  dataElement,   categoryOptionCombo ) %>%
      distinct 
    
    # excel version
    # elements = paste( des$dataElement.id , des$categoryOptionCombo.ids , sep = ".") %>%
    #   paste( collapse = ";")
    
    # rds version 
    prev.elements = paste( des$dataElement , des$categoryOptionCombo , sep = ".") %>%
      paste( collapse = ";") 
    
    if ( childOnly ){
      prev.elements = des$dataElement  %>% unique()
    }
    
    cat('\n - prev.elements:' , prev.elements )
    
    # limit to those in this request (new.elements)
    # elements.vector = str_split( elements, ";") %>% unlist 
    # prev.elements.vector = str_split( prev.elements, ";") %>% unlist 
    # prev.elements = prev.elements.vector[ prev.elements.vector %in% elements ] %>%
    #   paste0( ., collapse = ';')
    
    new.elements = paste( elements$id , collapse = ';' ) 
    cat( '\n - new.elements:' ,  new.elements )
    
    # Lookup national counts for last month, then get current counts ####
    
    prev.periods = prev.data %>% pull( period ) %>% unique %>%  paste(. , collapse = ';')
    cat( '\n - previous periods:' ,  prev.periods )
    
    prev.periods.vector = str_split_1( prev.periods, ";")
    prev.periods.years = prev.periods.vector %>% str_sub(1,4) %>% unique()
    prev.periods.years = prev.periods.years[ order( prev.periods.years )]
    cat( '\n - previous periods years:' ,  prev.periods.years )
    
    
    ## FETCH CURRENT VALUES of previous data 
    cat( '\n - checking previous counts\n' ) 
    # current.count = fetch(  baseurl , new.elements , prev.periods , orgUnits. = "LEVEL-1" , "COUNT" )  %>%
    #   mutate(current.count = as.integer( value ) ) %>%
    #   select( - value )
    
    showModal(
      modalDialog( title = "Checking for updated data counts", 
                   easyClose = TRUE , size = 'm' , 
                   footer = 
                     "Looking to see if national monthly COUNTs are the same as when last downloaded"
      )
    )
    
    ## NB: Some servers return null when requesting mulitple years (e.g. Malawi when data on both sides of 2020) 
    
    current.counts = list( length( prev.periods.years ) )
    
    for ( i in seq_along( prev.periods.years ) ) {
      
      prev.periods.in.year = prev.periods.vector[grepl( prev.periods.years[i] , prev.periods.vector )] %>%  unique()
      prev.periods.in.year = prev.periods.in.year[ order(prev.periods.in.year) ] %>%  paste(. , collapse = ';')
      
      cat( '\n - for' , prev.periods.in.year )
      
      current.counts[[i]] = fetch_get(  baseurl. = baseurl , 
                                        username = username , 
                                        password = password ,
                                        de. = new.elements , 
                                        periods. = prev.periods.in.year , 
                                        orgUnits. = "LEVEL-1" , 
                                        aggregationType. = "COUNT" ,
                                        get.print = print ,
                                        childOnly = FALSE 
                                        ) %>%
        mutate( current.count = as.integer( value )  )
    }
    
    cat( '\n - combining previous counts \n' )
    current.counts = bind_rows( current.counts )
    cat( '\n - rows =' , nrow( current.counts ) , "\n")
    
    removeModal()
    
    showModal(
      modalDialog( title = "Checking for updated data values", 
                   easyClose = TRUE , size = 'm' , 
                   footer = 
                     "Looking to see if national monthly SUMs are the same as when last downloaded"
      )
    )
    
    cat( '\n - checking previous values:' )
    
    current.values = list( length( prev.periods.years ) )
    
    for ( i in seq_along( prev.periods.years ) ) {
      
      # prev.periods.in.year = str_extract_all( prev.periods, 
      #                                         paste0( prev.periods.years[i] ,"[0-9][0-9]" )
      #                                         )[[1]] %>%  unique()
      # prev.periods.in.year = prev.periods.in.year[ order(prev.periods.in.year) ] %>%  paste(. , collapse = ';')
      # 
      # cat( '\n - for' , prev.periods.in.year )
      prev.periods.in.year = prev.periods.vector[grepl( prev.periods.years[i] , prev.periods.vector )] %>%  unique()
      prev.periods.in.year = prev.periods.in.year[ order(prev.periods.in.year) ] %>%  paste(. , collapse = ';')
      
      
      current.values[[i]] = fetch_get(  baseurl. = baseurl , 
                                        username = username , 
                                        password = password ,
                                        de. = new.elements , 
                                        periods. = prev.periods.in.year ,
                                        orgUnits. = "LEVEL-1" , 
                                        aggregationType. = "SUM" ,
                                        childOnly = FALSE ,
                                        get.print = print ) %>%
        mutate( current.value = as.integer( value )  )
      
    }
    
    cat( '\n - combining previous values \n' )
    # saveRDS( current.values, 'current.values.rds')
    
    current.values = bind_rows( current.values )
    cat( '\n - rows =' , nrow( current.values ) , "\n")
    
    #TESTING
    # saveRDS( current.counts, 'current.counts.rds')
    # saveRDS( current.values, 'current.values.rds')
    
    # current.values = bind_rows( current.values )
    # current.values = bind_rows( lapply( current.values, function(x) if( !is.data.frame(x) ) NULL else x) )
    
    # cat( '\n - rows =' , nrow( current.values ) , "\n")
    
    removeModal()  
    
    
    # Compare with previous data just for level-1
    ## for excel
    # prev = prev.data %>% filter( level == 1 , period %in% period_vectors ) 
    
    ## for rds, need orgunit for level 1 
    if ( is_empty( level1.id ) ){
      message('\n api_data function: need level1.id')
      return()
    }
    
    # If request does not include categoryOptionCombo, do not use in joins
    if ( 'categoryOptionCombo' %in% names( current.values ) ){
      by_cols = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
    } else {
      by_cols = c("dataElement", "period", "orgUnit")
    }
    
    update_compare = inner_join( current.counts, current.values , 
                                 by = by_cols
    ) %>%
      
      left_join( prev %>% 
                   mutate( prev.count = as.integer( COUNT ) ,
                           prev.value = as.numeric( SUM ) ) %>%
                   select( - COUNT, -SUM ), 
                 # Excel version  
                 # by = c( 'dataElement' = 'dataElement.id' , 
                 #         'categoryOptionCombo' = 'categoryOptionCombo.ids' ,
                 #         'period' = 'period' , 
                 #         'orgUnit' = 'orgUnit' )
                 # rds version  
                 by = by_cols
                 
      ) %>%
      mutate( same = ( current.count == prev.count ) & 
                ( current.value == prev.value ) 
      )
    
    
    saveRDS( update_compare ,  paste0( dir, 'update_compare_', formula,"_", Sys.Date() , ".rds") )
    
    prev.periods.same.data = update_compare %>% 
      group_by( period , dataElement , categoryOptionCombo ) %>%
      filter( all( same ) ) 
    
    cat(  '\n - prev.periods.same.data for ' , 
          paste( prev.periods.same.data %>% 
                   pull( period ) %>% unique , 
                 collapse = ';') , "\n"
    )
    
    update.periods = setdiff( period_vectors, 
                              prev.periods.same.data %>% 
                                pull( period ) %>% unique )
    
    cat(  '\n - Need to update or get new data data for ' , 
          paste( update.periods , collapse = ';') , "\n"
    )
    
    period_vectors = update.periods
    
  } else {
    if ( print ) cat( '\n - Requesting data for periods:\n' ,
                      paste( period_vectors , collapse = ';') , "\n" )
  } 
  
  
  if ( print ) cat( '\n - Requesting data for orgUnits:\n' , 
                    paste( orgUnits , collapse = ';') , "\n" 
  )
  
  # Elements
  # elements = strsplit( elements , ";" , fixed = TRUE )[[1]]
  if ( print ) cat( ' - Requesting data for elements:\n' ,
                    paste( elements$name , collapse = ';') , "\n"
  )
  
  ## Fetch requests ####
  
  if ( parallel ){ 
    plan( multisession ) # plan( multisession ) for windows
  } else { plan( sequential ) }
  
  # v2 <- expand_grid( period_vectors , orgUnits )
  # df of imputs for parallel mapping (pmap)

  pmap.df = expand.grid(  orgUnits, period_vectors, elements$id ) 
  
  # Testing
  saveRDS( pmap.df, 'pmap.df.rds' )
  
  if ( print ){
    cat( '\n Making' , 
         nrow( pmap.df ), "data requests" , "\n" 
    )  
    cat('\n glimpse( pmap.df ) '); print( head( pmap.df , n=10 ) )
  } 
  
  # if used within shiny, use withProgressShiny()
  # 12/2021 ipc package for asynchronous progress with future
  if (shinyApp == TRUE ){
    
    if ( parallel ){
      cat( '\n starting the future package parallel requests\n')
      
      # Create a progress bar
      # progress <- AsyncProgress$new( #session ,
      #                                message="Requesting data" ,
      #                                min = 1 , max = nrow( pmap.df ) ,
      #                                detail = paste( "for", length(period_vectors) , "months" )
      #                                                
      # )
      
      fut <- future({
        future_pmap( 
          pmap.df
          ,
          .f = function( Var1, Var2, Var3 ){
            cat( Var1, Var2, Var3 , "\n" )
            
            # if ( !is.null( p ) ) p()
            progress$inc(1/ nrow( pmap.df )) # Increment progress bar
            # progress$set( value = i , detail = paste( "SUM" , i ) )
            
            # if ( print ) message( paste( "" , formula , " : " , .x , .y ,
            #               parse_date_time( Sys.time(), '%I:%M:%S %p') )  )
            
            
            d.sum = fetch_get(  baseurl. = baseurl ,
                                username = username , 
                                password = password ,
                                de. = Var3 , 
                                periods. = Var1, 
                                orgUnits. = Var2 , 
                                aggregationType. = "SUM" ,
                                childOnly = childOnly ,
                                get.print = print )  
            
            # progress$set( value = i , detail = paste( "COUNT" , i ) )
            d.count = fetch_get(  baseurl. = baseurl , 
                                  username = username , 
                                  password = password ,
                                  de. = Var3 , 
                                  periods. = Var1  , orgUnits. = Var2 , 
                                  aggregationType. = "COUNT" ,
                                  childOnly = childOnly ,
                                  get.print = print) 
            
            #if elements have a category , then include categoryOptionCombo
            if ( any( str_detect( Var3  , fixed(".") )) ){
              .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
              
              cat( "\n - joining sum and count downloads by" , .by )
              
              # Join d.sum and d.count
              d = d.count %>%
                rename( COUNT = value ) %>%
                full_join( d.sum %>% rename( SUM = value ) 
                           # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                           , by = .by
                )
              
            } else {
              .by = c("dataElement", "period", "orgUnit")
              
              cat( "\n - joining sum and count downloads by" , .by )
              
              d = d.count %>%
                select( - categoryOptionCombo ) %>%
                rename( COUNT = value ) %>%
                full_join( d.sum %>% rename( SUM = value )
                           # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                           , by = .by
                )
              
            }
            
            
            
            
            d = d %>% select(-starts_with('aggreg'))
            
            cat( '\n' , nrow(d), 'records with sum and count\n' )
            
            
            # Save data up to this point
            
            
            return( d )
            
          } 
        )
      })
      
      # future.promise = then(fut, 
      #      onFulfilled = function(result) {
      #       progress$close()
      #       result 
      #   } )
      
      d = future::value( fut )
      # d <- NULL
      # fut %...>% { d <<- . }
      progress$close()
      cat( '\n the future is over')
      cat( '\n' , nrow(d), 'records with sum and count' )
      cat( '\n' , 'of these, there was no value for' , sum( is.na(d$SUM) ), 'records \n' )
      
      return( d )
      
    } else {
      ### SHINY - NOT PARALLEL ####
      plan( sequential ) 
      cat('\n begining request for data' )
      d = list()
      
      
      withProgress(     message = "Requesting data",
                        detail = "starting ...",
                        value = 0, {
                          
                          # p <- progressor( steps = nrow( pmap.df ) )
                          
                          for ( i in 1:nrow( pmap.df )){
                            setProgress( 
                              # value = 1 / nrow( pmap.df ) ,
                              detail = sprintf("requesting data entered for %s in %s (%d of %d requests)", 
                                               # length(pmap.df[i, 3]) , 
                                               elements[ which(elements$id == pmap.df[i, 3] ), 'name' ]  , # element name 
                                               pmap.df[i, 2]  , # orgUnit 
                                               # pmap.df[i, 1] , 
                                               i , nrow( pmap.df )   )  # period
                            )
                            
                            incProgress( amount = 1 / nrow( pmap.df ) )
                            
                            # Speed-up... 
                            ## If previous count was missing, null, or zero, 
                            ## and current level is not LEVEL-1, 
                            ## should go to next iteration
                            cat( "\n - ", i )
                            if ( i > 1 && pmap.df$Var2[i-1] %in% "LEVEL-1" && ( is.na( d[[i-1]]$COUNT ) || d[[i-1]]$COUNT == 0 ) && ! pmap.df$Var2[i] %in% "LEVEL-1" ){
                              cat("\n - previous", pmap.df[i-1, 1] , "was empty so skipping next lower level,", pmap.df[i, 1]) 
                              next
                            }
                            
                            cat('\n - api_data for:', baseurl, username )
                            
                            d.sum = fetch_get(  baseurl. = baseurl , 
                                                username = username , 
                                                password = password ,
                                                de. = pmap.df[i, 3] , 
                                                periods. = pmap.df[i, 2] , 
                                                orgUnits. = pmap.df[i, 1] , 
                                                aggregationType. = "SUM" ,
                                                childOnly = childOnly ,
                                                get.print = print) 
                            
                            
                            # progress$set( value = i , detail = paste( "COUNT" , i ) )
                            # p( sprintf("requesting count for %d of %d", i , nrow( pmap.df ) ) )
                            # setProgress( 
                            #   detail = sprintf("requesting COUNT for %d of %d", i , nrow( pmap.df ) ) 
                            #   )
                            
                            if ( !childOnly ){
                              
                              d.count = fetch_get(  baseurl. = baseurl , 
                                                  username = username , 
                                                  password = password ,
                                                  de. = pmap.df[i, 3] , 
                                                  periods. = pmap.df[i, 2] , 
                                                  orgUnits. = pmap.df[i, 1] , 
                                                  aggregationType. = "COUNT" ,
                                                  childOnly = childOnly ,
                                                  get.print = print) 
                              
                              # Join d.sum and d.count
                              # progress$set( detail = paste( "combining" , i ) )
                              # p( sprintf("combining %d", i ) )
                              # setProgress( 
                              #   detail = sprintf("Combining SUM and COUNT for %d of %d", i , nrow( pmap.df ) ) 
                              #   )
                              #if elements have a period, then include categoryOptionCombo
                              #if elements have a category , then include categoryOptionCombo
                              if ( any( str_detect( pmap.df[i, 3]  , fixed(".") )) ){
                                .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
                              } else {
                                .by = c("dataElement", "period", "orgUnit")
                              }
                              
                              # Testing
                              cat( "\n - ", i , "-joining sum and count downloads by" , .by , "..." )
                              
                              d[[i]] = d.count %>%
                                rename( COUNT = value ) %>%
                                full_join( d.sum %>% rename( SUM = value ) 
                                           # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                                           , by = .by
                                ) 
                              
                            } else {
                              
                              d[[i]] = d.sum  %>% rename( SUM = value ) %>%  
                                mutate( COUNT = 1 ) 
                            }
                            
                            cat(  nrow(d[[i]]), 'records\n')
                            
                            
                            #Testing
                            saveRDS( d, "d.rds")
                            # cat("\n done")
                            
                          }
                        }
      )
      
      # progress$close() # Close the progress bar
      
      
    }
  } else {
    
    # Non shiny evalution: 
    handlers(list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
      )
    ))
    
    with_progress({
      # p <- progressor( steps = nrow( v2 ) )
      p <- progressor( steps = nrow( pmap.df ) )
      
      d = 
        # future_map2( 
        future_pmap( 
          pmap.df
          # v2$period_vectors, 
          # v2$orgUnits 
          ,
          .f = function( Var1, Var2, Var3 ){
            
            if ( !is.null( p ) ) p()
            
            # tic()
            # if ( print ) message( paste( "" , formula, " : " , .x , .y , 
            #               parse_date_time( Sys.time(), '%I:%M:%S %p') )  )
            
            # cat( Var1, Var2, Var3 )
            
            d.sum = fetch_get(  baseurl. = baseurl , 
                                username = username , 
                                password = password ,
                                de. = Var3 , 
                                periods. = Var2, 
                                orgUnits. = Var1 , 
                                aggregationType. = "SUM" ,
                                childOnly = childOnly ,
                                get.print = print)  
            
            # Testing
            # saveRDS( d.sum,  'd.sum.rds')
            
            if ( !childOnly ){
              
              d.count = fetch_get(  baseurl. = baseurl , 
                                    username = username , 
                                    password = password ,
                                    de. = Var3 , 
                                    periods. = Var2  , orgUnits. = Var1 , 
                                    aggregationType. = "COUNT" ,
                                    childOnly = childOnly ,
                                    get.print = print) 
              
              
              # Testing
              # saveRDS( d.count, 'd.count.rds')
              
              #if elements have a period, then include categoryOptionCombo
              #if elements have a category , then include categoryOptionCombo
              if ( any( str_detect( Var3  , fixed(".") )) ){
                .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
              } else {
                .by = c("dataElement", "period", "orgUnit")
              }
              
              cat( "\n - joining sum and count downloads by" , .by , "\n")
              
              # Join d.sum and d.count
              d = d.count %>%
                rename( COUNT = value ) %>%
                full_join( d.sum %>% rename( SUM = value ) 
                           # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                           , by = .by
                )
              
            } else {
              
              d = d.sum  %>% rename( SUM = value ) %>%  mutate( COUNT = 1 )

            }
            
            
            # d = d %>% select(-starts_with('aggreg'))
            
            # Save data up to this point
            
            # Testing
            # saveRDS( d, 'd.rds')
            
            return(d)
            
          } 
        )
    })
  }
  
  if ( print ) message( "binding downloads" )
  
  cat('\n\n compiling requested data ')
  d = bind_rows( d )
  
  # Testing
  saveRDS( d, 'data_download.rds')
  
  cat( '\n' , nrow(d), 'records with sum and count' )
  cat( '\n' , 'of these, there was no value for' , sum( is.na(d$SUM) ), 'records \n' )

  
  # update value in most_recent_data_file
  if ( update &&  nrow( prev.data ) > 0 ){
    
    cat( '\n Updating data')
    
    good.prev.data = prev.data %>% 
      filter( !period %in% unique( d$period )) %>%
      mutate_all( as.character )
    
    cat("\n - glimpse good.prev.data:\n") ; glimpse( good.prev.data )
    cat("\n - glimpse d:\n") ; glimpse( d )
    
    d = d %>% mutate_all( as.character )
    
    updated.data  = bind_rows( good.prev.data , d ) %>% 
      filter( !is.na( COUNT ) | ! COUNT %in% "0.0" ) %>%
      arrange( period , orgUnit, dataElement , categoryOptionCombo  ) 
    
    cat("\n - glimpse updated.data:\n") ; glimpse( updated.data )
    
    return( updated.data )
  }
  
  return( d )
}