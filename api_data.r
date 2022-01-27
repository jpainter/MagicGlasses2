# api_data with option for parallel processing


# library(pacman)
# p_load( knitr, scales, tidyverse, readxl, patchwork, 
#         tsibble, fable, fabletools, feasts, slider, anomalize, lubridate ,
#         furrr, tictoc, ggrepel , sf , ggspatial, mdthemes , extrafont ,
#         hrbrthemes ,tidyfast , progressr, gt , rlist )

# dplyr option to set summarise grouping behavior (as 'keep')
# options( dplyr.summarise.inform = FALSE )



# Functions ####
# Login ####
loginDHIS2<-function( baseurl, username, password) {
  
  url<-paste0( baseurl, "api/me" )
  
  r <-  GET( url, authenticate( username, password) ) 
  
  assert_that( r$status_code == 200L ) 
}

# Retry function to use when querying database
# borrowed from: https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error

# library(futile.logger)
# library(utils)
retry <- function( expr, isError=function(x) "try-error" %in% class(x), 
                  maxErrors = 3, sleep = 1) {
  attempts = 0
  retval = try( eval(expr) )
  
  while ( isError(retval) ) {
    
    print( eval(expr)  )
    
    attempts = attempts + 1
    
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
      
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      # flog.error(msg)
      # warning(msg)
    }
    
    if (sleep > 0) Sys.sleep(sleep)
    
    retval = try( eval(expr) )
  }
  
  return(retval)
}

# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , .print = TRUE , json = TRUE , ...){
  
  # Login if credentials provided
  if ( exists( 'baseurl' ) ){
    if ( !loginDHIS2( baseurl, username, password) ) return('no connection')
  }
    
  # https://stackoverflow.com/questions/57198836
  httr::set_config(httr::config(ssl_verifypeer=0L))
  
  if ( .print ){
    cat( paste( "\n downloading from" , source_url , "...") ) 
    # print( Sys.time() )
  } 
  
  from_url =  GET( source_url ) 
  
  # print( 'GET completed')
  # print( from_url )
  
  get_content = content( from_url , "text")
  
  # print( 'get_content')
  
  if ( from_url$status_code != 200 ){
    # showModal( modalDialog( get_content[[1]] ) )
    # return( get_content[[1]] )
    cat( paste( '\nStatus code' , from_url$status_code ) )
    cat( paste( '\nget_content class' , class( get_content ) ) )
    cat( get_content )
    return( get_content )
  } 
  
  # test if return valid content
  is.json = jsonlite::validate( get_content )
  
  # if ( .print )  print( paste( 'testing if json' , is.json ) )
  
  if ( is.json ){ 
    
    if ( !is.json ){
      # print( get_content )
      cat( '\n get() did not return json \n')
      return( NA )
      
    } else {

      g = fromJSON( get_content ) 
      
      if (length(g) == 0) return( NA ) 
      
      return( g )
    }
    
  } else {
    g = get_content 
    return( g )
  }
  
  
  
}

get_in_parts = function( baseurl. , de. , periods. , orgUnits. , aggregationType. ){
  
  # withProgress( message = 'requesting data' ,  value = 0, {
  #   
  #   de = str_split( de., ";" ) %>% unlist()
  #   n = length( de )
  #   data = list( n )
  #   
  #   for (i in 1:n) {
  #     
  #     # Each time through the loop, add another row of data. This is
  #     # a stand-in for a long-running computation.
  #     de.i = de.[ i ]
  #     
  #     url = api_url( baseurl. , de. , periods. , orgUnits. , aggregationType. )
  #     
  #     url = paste0( baseurl , de.i , endingurl )
  #     data <- get( url )
  #     
  #     # Increment the progress bar, and update the detail text.
  #     incProgress(1/n, detail = paste("Doing request for", de.i ))
  #     
  #   }
  #   
  #   return( bind_rows( data ) )
  #   
  # })
  
}


# Function to create string of dates. #####
# Default is for every month during last five years 

date_code = function( 
  years = NULL , 
  months = NULL ,
  startPeriod = NULL , 
  YrsPrevious = 1 ,
  monthsPrevious = NULL , 
  currentMonth = TRUE  # include current month
  ){
  
  if ( is.null( months ) )  months = 1:12
  
  if ( is.null( years ) ){
    
    this.year = year( Sys.Date() )
    YrsPrevious = this.year - YrsPrevious
    
    years = YrsPrevious:this.year
  }
  
  # endMonth: get current month.  
  endMonth = Sys.yearmon()
  
  # start month
  if ( !is.null( startPeriod ) ){
    startMonth = as.yearmon( startPeriod , "%Y%m")
    } else {
      if ( !is.null( monthsPrevious )){
        startMonth =  endMonth - monthsPrevious/12
        } else {
          startMonth = as.yearmon( YrsPrevious )
        }
    }
  
  months = seq( startMonth, endMonth , 1/12 ) %>% format(., "%Y%m")
  
  # remove current month ;
  if (currentMonth == FALSE )
  months = months[ 1:( length(months) - 1)]
  
  period = paste( months, collapse = ";" )
  return( period )
}


api_formula_elements = function( formulaName , dir = NULL , ...  ){
  
  if ( is.null( dir ) ) dir = file.dir( ... )
  
  formula.file = files('formula' , country = country , dir = dir )
  
  formula.elements = read_excel( 
    paste0( dir , formula.file[1]  ), 
    sheet = 'Formula Elements') 
  
  de = filter( formula.elements , 
               Formula.Name %in% formulaName ) %>%
    select( dataElement.id , categoryOptionCombo.ids ) %>%
    unite( "de" , sep = "." ) %>% pull(de ) %>% paste( collapse =  ";" )
  
  return( de )
}


files = function(  search = 'All' , 
                   type = 'xlsx' , 
                   other = "" , 
                   dir = NULL , ... ){
                        
                      if ( is.null( dir ) ) dir = file.dir( ... )
                      if ( !dir.exists( dir ) ){ 
                        message("Files function: directory not found")
                        return()
                        # assert_that( dir.exists( dir ) )
                      }
                      
                      dir.files = list.files( dir )
                      search_and_type =  
                          str_detect( dir.files, fixed( search , ignore_case=TRUE )  )  &
                           grepl( paste0( type , '$' ) , dir.files, 
                                  ignore.case =  TRUE  ) &
                          grepl( other , dir.files , ignore.case = TRUE ) 
                      files = dir.files[  search_and_type   ]
                      return( files[ rev( order( files )) ]  )
                      }

file.dir = function( country = country ,
                     dir.base = NULL ){
  if ( is_empty( dir.base )  ){
    dir.base = '../dataDictionary/dhis2_dictionary/Formulas/' 
    dir.base = paste0( dir.base , country , "/")
  } 
  return( dir.base )
  }


is_date = function(x, format = NULL) {
  formatted = try(as.Date(x, format), silent = TRUE)
  is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
  is_date[is.na(x)] = NA  # Insert NA for NA in x
  return(is_date)
}


most_recent_file = function( file_list_with_date ){
  
  rdsFileSplit = str_split( file_list_with_date, "_")
  
  download_date = map( rdsFileSplit ,
                       ~str_split( .x[ length(.x)] , "\\.")[[1]]
  ) %>% map_chr(1) 
  
  dates  = map_chr( download_date , ~ anydate(.x)  )
  
  if ( identical( dates , character(0) ) ) return( NA )
  
  # most recent file 
  file = file_list_with_date[ which( dates == max(dates, na.rm = T ) ) ]
  
  return ( file )
}


dir = function( country ){ file.dir( country ) }

metadata.file = function( dir = NULL, country = country ){
  if ( is.null( dir ) ) dir  = dir( country )
  paste0( dir, 
          files( 'meta' , dir = dir , country = country  ) )  %>%
    most_recent_file 
}
                        
OrgUnitLevels = function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file  , 
                       sheet = 'OrgUnitLevels')
}
OrgUnits =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , 
              sheet = 'OrgUnits')
}
DataElements =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file  , 
              sheet = 'DataElements')
}
DataSets =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , sheet = 'DataSets')
}
Indicators =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , sheet = 'Indicators')
}

# update fetch for non shiny environ
fetch <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. , .print = FALSE ){
    
    if (.print ) print( paste( 'period is ', periods. ) )
    periods_vector = str_split( periods. , ";" ) %>% unlist
    
    n_periods = length( periods_vector )
    if (.print )  print( paste( 'n_periods' , n_periods ))
    
    # translate level name to LEVEL-1, etc
    # level = ousLevels() %>% filter( levelName %in% orgUnits.) %>% pull( level )
    # ouLevel = paste0( "LEVEL-", level )
    # print( paste( level, ouLevel) )
    
    # Version before furr and progressor
    # data = list( n_periods )
    # 
    #   for ( i in 1:n_periods ){
    #     
    #     data[[i]] = fetch_get( baseurl. , de. , periods_vector[i] , orgUnits. , aggregationType.,
    #                            get.print = .print )
    #   }
    
    handlers(list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
      )
    ))
    
 
    with_progress({
      p <- progressor( steps = n_periods )
      
      data = 
        future_map( 
          1:n_periods , 
          ~{
            if ( !is.null( p ) ) p()
            
            fetch_get( baseurl. , de. , periods_vector[.x] , orgUnits. , aggregationType.,
                       get.print = .print )
            
          } )
    })
    
    return( bind_rows( data ) )
}

api_url = function( baseurl, de ,  periods, orgUnits , aggregationType ){
  
  cat( '\n* api_url:' , baseurl ,  de , periods , orgUnits ) 
  # # print( orgUnits ); print( aggregationType )
  
  url <- paste0( baseurl , 
                 # username, password ,
                 "api/analytics/dataValueSet.json?" ,
                 # "api/analytics.json?" ,
                 "dimension=ou:", orgUnits , 
                 "&dimension=pe:" , periods ,
                 "&dimension=dx:" , de ,
                 # "&displayProperty=NAME",
                 "&aggregationType=" , aggregationType )
  
  # print( url )
  return( url )
}

  
fetch_get <- function( baseurl. , de. , periods. , orgUnits. , 
                       aggregationType. ,
                       get.print = FALSE, username. = NULL , password. = NULL ){
  
    cat( '\n* fetch_get:' , baseurl. ,  de. , periods. , orgUnits.) 
    url = api_url( baseurl. , de. , periods. , orgUnits. , aggregationType. )
    
    
    fetch = retry( get( url , .print = get.print , username., password. )[[1]] ) # if time-out or other error, will retry 
    # fetch = get( url , .print = get.print )
    
    # print( paste( 'fetch class' , class( fetch ) ) )
    # print( paste( 'fetch class[[1]]' , class( fetch[[1]] ) ) ) 
    
    # if returns a data frame of values (e.g. not 'server error'), then keep
    # print( paste( 'did fetch return data frame?' , is.data.frame( fetch )))
    
    if ( is.data.frame( fetch ) ){ 
      
    # remove unneeded cols
      
      cols = colnames( fetch ) 
      
      unneeded.cols = which( cols %in% c( 'storedBy', 'created', 'lastUpdated', 'comment' ))
      
      data.return = fetch %>% select( -all_of(unneeded.cols) ) %>% as_tibble()
      
      # if (get.print) print( paste( 'col names data', 
      #               paste( colnames( data.return ) , collapse = "," ) 
      #               )
      # )
      
      if ( get.print ) cat( paste( nrow( fetch ) , 'records\n' ) )
      
      } else {
      
      de.cat = str_split( de. , fixed(".")) %>% unlist  
      
      data.return  = tibble( 
        dataElement = de.cat[1] , 
        categoryOptionCombo = de.cat[2] , 
        period =  periods. ,
        orgUnit =  orgUnits. ,
        aggregationType = aggregationType. ,
        value = NA
      )
      
      if ( get.print ) cat( "\n" , 
                            de. , periods. , orgUnits. , aggregationType. ,
                            ": no records" )
    }
    
    return( data.return )
  }
  
translate_fetch = function( df , formulaElements = NULL , ous = NULL ){
  
  cat( '\na*translate_fetch:' , baseurl ,  de. , periods. , orgUnits.) 
  
  if ( is.null( formulaElements )) formulaElements = DataElements()
  if ( is.null( ous )) ous = ous()
    
  # prepare formula elements as one row per de and coc
  # prepare formula elements as one row per de and coc
  multiple_dataElements = count( formulaElements , dataElement ) %>%
    filter( n > 1 ) %>% nrow == 0 
  multiple_categoryOptionCombos = count( formulaElements , categoryOptionCombo.ids ) %>%
    filter( n > 1 ) %>% nrow == 0 
  
  if ( (!multiple_dataElements |  !multiple_categoryOptionCombos ) & !all( is.na( df$categoryOptionCombo ))){
    
      de.coc = formulaElements  %>% 
      separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
      mutate( Categories = Categories %>% str_trim  ,
              categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim ,
              dataElement.id = dataElement.id %>% str_trim ) %>% 
              select( dataElement, dataElement.id ,
                      Categories , categoryOptionCombo.ids )
      
  } else { de.coc = formulaElements %>% 
                   select( dataElement, dataElement.id  ,) %>% 
                   mutate( dataElement.id = dataElement.id %>% str_trim ,
                           Categories = "" ) 
  }
  
      .by =  c( "dataElement.id" , "categoryOptionCombo.ids" )
      if ( all( is.na( df$categoryOptionCombo ))) .by = "dataElement.id"
      
      translated_df = df %>%
      
      rename( dataElement.id = dataElement , 
              categoryOptionCombo.ids = categoryOptionCombo 
      ) %>%
      left_join( de.coc %>% select( dataElement.id , dataElement ) %>% distinct ,  
                 by = 'dataElement.id' ) %>%
      left_join( de.coc  %>% select( categoryOptionCombo.ids , Categories ) %>% distinct , 
                 by = 'categoryOptionCombo.ids' ) %>%
        
      # left_join( de.coc  ,  by = .by ) %>%
      
      left_join( ous %>% 
                   select( id, name, level, levelName )  %>% 
                   rename( orgUnit = id , orgUnitName = name ) ,
                 by = 'orgUnit' 
      )  
      
      return( translated_df )
  }


# api_data ####
api_data = function(      periods = "LAST_YEAR" ,
                          periodType = 'Monthly' ,
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
                          dir = country.dir ,
                          shinyApp = FALSE ,
                          ...
){
  cat('\n**api_data function...')
  # tic()
  ##### cycle through each period, each data element...

  # Periods ####
  if ( periods %in% 'months_last_year' ) periods = date_code( monthsPrevious = 12 )
  if ( periods %in% 'months_last_2_years' ) periods = date_code( YrsPrevious = 2 )
  if ( periods %in% 'months_last_3_years' ) periods = date_code( YrsPrevious = 3 )
  if ( periods %in% 'months_last_4_years' ) periods = date_code( YrsPrevious = 4 )
  if ( periods %in% 'months_last_5_years' ) periods = date_code( YrsPrevious = 5 )
  if ( periods %in% 'weeks_last_5_years' ) periods = date_code_weekly( YrsPrevious = 5 )

  if ( all( is.na( periods )  ) ) periods = date_code( YrsPrevious = 4 )
    
  period_vectors = strsplit( periods , ";" , fixed = TRUE )[[1]]

  if ( update & !file.exists( previous_dataset_file ) ){
        cat('\nprevious data file missing') 
  } 
  
  ## UPDATE data options ####
  if ( update & file.exists( previous_dataset_file ) ){
      
      cat('\n - retrieving details of previous dataset ')
      
      # check for last x years only 
      if ( periodType == 'Monthly') periods = date_code( YrsPrevious = check_previous_years ) # 'months_last_5_years' # 
      if ( periodType == 'Weekly') periods = date_code_weekly( YrsPrevious = check_previous_years )

      cat('\n - periodType is' ,  periodType)
      
      period_vectors = strsplit( periods , ";" , fixed = TRUE )[[1]]
      
      # excel version
      # prev.data = read_excel(previous_dataset_file , 
      #                          sheet = 'formulaData') %>%
      #           filter( !is.na( dataElement  ) )
      
      prev.data = readRDS( previous_dataset_file ) # %>% select( - starts_with('aggr'))
      
      if ( nrow( prev.data ) == 0 ){
         cat('\n - previous data file empty'); next()
      }  
      
      first.month = min(prev.data$period  )
      last.month =  max( prev.data$period )
      
      # excel version
      # des = prev.data %>% select( dataElement, dataElement.id , Categories ,
      #                      categoryOptionCombo.ids ) %>%
      #   unique 
      
      cat('\n - date range:' , first.month , last.month )
      
      des = prev.data %>% 
        filter( !is.na( COUNT ) ) %>%
        select( dataElement,  categoryOptionCombo ) %>%
        unique 
      
      # excel version
      # elements = paste( des$dataElement.id , des$categoryOptionCombo.ids , sep = ".") %>%
      #   paste( collapse = ";")
      
      # rds version 
      prev.elements = paste( des$dataElement , des$categoryOptionCombo , sep = ".") %>%
        paste( collapse = ";") 
      
      # limit to those in the request (elements)
      elements.vector = str_split( elements, ";") %>% unlist 
      prev.elements.vector = str_split( prev.elements, ";") %>% unlist 
      prev.elements = prev.elements.vector[ prev.elements.vector %in% elements.vector ] %>%
        paste0( ., collapse = ';')
      
      # Lookup national counts for last month, then get current counts ####
     
      ## for excel
      # prev = prev.data %>% filter( level == 1 , period %in% period_vectors ) 
      
      ## for rds, need orgunit for level 1 
      # prev = prev.data %>% filter( orgUnit == 'LEVEL-1' , period %in% period_vectors )
      level1 = ous %>% filter( level == 1 ) %>% pull( id )
      prev = prev.data %>% filter( orgUnit == level1  , period %in% period_vectors ) 
      
      prev.periods = prev %>% pull( period ) %>% unique %>%  paste(. , collapse = ';')
      
      ## FETCH CURRENT VALUES of previous data 
      cat( 'checking previous counts\n' )
      current.count = fetch(  baseurl , elements , prev.periods , orgUnits. = "LEVEL-1" , "COUNT" )  %>%
        mutate(current.count = as.integer( value ) ) %>%
        select( - value )
      
      cat( 'checking previous values\n' )
      current.value = fetch(  baseurl , elements , prev.periods , orgUnits. = "LEVEL-1" , "SUM" )  %>%
        mutate(current.value = as.integer( value ) ) %>%
        select( - value )
      
      update_compare = inner_join( current.count, current.value , 
                                   by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
                                   ) %>%
        left_join( prev %>% 
                      mutate( prev.count = as.integer( COUNT ) ,
                              prev.value = as.integer( SUM ) ) %>%
                     select( - COUNT, -SUM ), 
                  # Excel version  
                  # by = c( 'dataElement' = 'dataElement.id' , 
                  #         'categoryOptionCombo' = 'categoryOptionCombo.ids' ,
                  #         'period' = 'period' , 
                  #         'orgUnit' = 'orgUnit' )
                  # rds version  
                  by = c( 'dataElement' , 
                          'categoryOptionCombo',
                          'period' , 
                          'orgUnit' )
                  
                  ) %>%
        mutate( same = ( current.count %in% prev.count ) & 
                  ( current.value %in% prev.value ) 
        )
      
        
        saveRDS( update_compare ,  paste0( dir, 'update_compare_', formula,"_", Sys.Date() , ".rds") )
      
        prev.periods.same.data = update_compare %>% 
          group_by( period ) %>%
          filter( all( same ) ) %>%
          pull( period ) %>% unique 
        
        update.periods = setdiff( period_vectors, prev.periods.same.data )
        
        if ( print ) cat(  'Need to update or get new data data for ' , 
                                   paste( update.periods , collapse = ';') , "\n"
        )
        
        period_vectors = update.periods
        
    } else {
       if ( print ) cat( '\n Requesting data for periods:\n' ,
                         paste( period_vectors , collapse = ';') , "\n" )
    } 

 
  if ( print ) cat( '\nRequesting data for orgUnits:\n' , 
                    paste( orgUnits , collapse = ';') , "\n" 
  )
  
  # Elements
  # elements = strsplit( elements , ";" , fixed = TRUE )[[1]]
  if ( print ) cat( 'Requesting data for elements:\n' ,
                    paste( elements , collapse = ';') , "\n"
  )
  
  ## Fetch requests ####
  
  if ( parallel ){ 
    plan( multisession ) # plan( multisession ) for windows
    } else { plan( sequential ) }

    # v2 <- expand_grid( period_vectors , orgUnits )
    # df of imputs for parallel mapping (pmap)
    pmap.df = expand.grid( period_vectors, orgUnits, elements ) 
    if ( print ){
      cat( '\nMaking' , 
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
  
             
              d.sum = fetch_get(  baseurl. = baseurl , username , password ,
                                  de. = Var3 , 
                                  periods. = Var1, 
                                  orgUnits. = Var2 , 
                                  aggregationType. = "SUM" ,
                                  get.print = print)  
             
              # progress$set( value = i , detail = paste( "COUNT" , i ) )
              d.count = fetch_get(  baseurl. = baseurl , username , password ,
                                    de. = Var3 , 
                                    periods. = Var1  , orgUnits. = Var2 , 
                                    aggregationType. = "COUNT" ,
                                    get.print = print) 
              
              #if elements have a period, then include categoryOptionCombo
              if ( any(str_detect( elements  , fixed(".") )) ){
                .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
              } else {
                .by = c("dataElement", "period", "orgUnit")
              }
              
              # Join d.sum and d.count
              d = d.count %>%
                    rename( COUNT = value ) %>%
                    full_join( d.sum %>% rename( SUM = value ) 
                              # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                              , by = .by
                              )
              
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
      ### NOT PARALLEL ####
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
                detail = sprintf("requesting SUM and COUNT for %d elements in %s (%d of %d requests)", 
                                 length(pmap.df[i, 3]) , pmap.df[i, 1] , i , nrow( pmap.df )   ) 
                )
            
             incProgress( amount = 1 / nrow( pmap.df ) )
            
              cat('\napi_data for:', baseurl, username )
              
              d.sum = fetch_get(  baseurl. = baseurl , username , password ,
                                  de. = pmap.df[i, 3] , 
                                  periods. = pmap.df[i, 1] , 
                                  orgUnits. = pmap.df[i, 2] , 
                                  aggregationType. = "SUM" ,
                                  get.print = print)  
              
              # progress$set( value = i , detail = paste( "COUNT" , i ) )
              # p( sprintf("requesting count for %d of %d", i , nrow( pmap.df ) ) )
              # setProgress( 
              #   detail = sprintf("requesting COUNT for %d of %d", i , nrow( pmap.df ) ) 
              #   )
              d.count = fetch_get(  baseurl. = baseurl , username , password ,
                                    de. = pmap.df[i, 3] , 
                                    periods. = pmap.df[i, 1] , 
                                    orgUnits. = pmap.df[i, 2] , 
                                    aggregationType. = "COUNT" ,
                                    get.print = print) 
              
              # Join d.sum and d.count
              # progress$set( detail = paste( "combining" , i ) )
              # p( sprintf("combining %d", i ) )
              # setProgress( 
              #   detail = sprintf("Combining SUM and COUNT for %d of %d", i , nrow( pmap.df ) ) 
              #   )
              #if elements have a period, then include categoryOptionCombo
              if ( any(str_detect( elements  , fixed(".") )) ){
                .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
              } else {
                .by = c("dataElement", "period", "orgUnit")
              }
              
              d[[i]] = d.count %>%
                    rename( COUNT = value ) %>%
                    full_join( d.sum %>% rename( SUM = value ) 
                              # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                              , by = .by
                              ) %>% select(-starts_with('aggreg'))
             
          }
                        }
      )
      
    # progress$close() # Close the progress bar
    
      cat('\n compiling requested data ')
      d = bind_rows( d )
      cat( '\n' , nrow(d), 'records with sum and count' )
      cat( '\n' , 'of these, there was no value for' , sum( is.na(d$SUM) ), 'records \n' )
   
      return( d )
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
            
            d.sum = fetch_get(  baseurl. = baseurl , de. = Var3 , 
                                periods. = Var1, orgUnits. = Var2 , "SUM" ,
                                get.print = print)  
           
            d.count = fetch_get(  baseurl. = baseurl , de. = Var3 , 
                                  periods. = Var1  , orgUnits. = Var2 , "COUNT" ,
                                  get.print = print) 
            
            #if elements have a period, then include categoryOptionCombo
            if ( any(str_detect( elements  , fixed(".") )) ){
              .by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
            } else {
              .by = c("dataElement", "period", "orgUnit")
            }
            
            # Join d.sum and d.count
            d = d.count %>%
                  rename( COUNT = value ) %>%
                  full_join( d.sum %>% rename( SUM = value ) 
                            # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                            , by = .by
                            )
            
            d = d %>% select(-starts_with('aggreg'))
            
            cat(  Var1, Var2, Var3, nrow(d), 'records\n')
            
            # Save data up to this point
            
            
            return( d )
                
            } 
        )
    })
    }
    
     if ( print ) message( "binding downloads" )
     d = bind_rows( d )
     
     if ( print ) cat( comma( nrow( d ) ), "records downloaded"  ,  "\n" )
     if ( print ) message( toc() )
     
     # update value in most_recent_data_file
     if ( update & file.exists( previous_dataset_file ) ){

       good.prev.data = prev.data %>% filter( !period %in% unique( d$period ))
       
       updated.data  = bind_rows( good.prev.data , d ) %>% 
         filter( !is.na( COUNT ) ) %>%
         arrange( period , orgUnit, dataElement , categoryOptionCombo  ) 

       return( updated.data )
     }
     
     return( d )
}

# Testing
# # Formulas  ----
# library(progressr)
# files('Formula' , country = country )[1]
# 
# f = formulas( country ) %>% 
#   filter( !is.na(Formula.Name)) %>% 
#   pull(Formula.Name )
# f= f[4]
# 
# ## login and credentials ----
#     credentials = read_excel( paste0( "../dataDictionary/dhis2_dictionary/Formulas/",
#                                      "_Instances_jp.xlsx")) %>%
#     filter( Instance == country )
#     
#     baseurl = credentials$IPaddress
#     username = credentials$UserName
#     password = credentials$Password
# 
#     loginurl <-paste0( baseurl , "api/me" )
#     GET( loginurl, authenticate( username, password ) )
# 
#     periods = 'months_last_year' # date_code( YrsPrevious  = 4 )
#     level = 'All levels'
#     elements = formula_elements( f[i] )
#     start = Sys.time()
#     y  = api_data( periods, level , elements, baseurl , formula = f[i]  
#                    # , parallel = TRUE 
#                    )
#     print( paste( 'finished downloading' , f[i] ) )
#     stop = Sys.time(); stop - start
# 
