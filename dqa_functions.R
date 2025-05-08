# DQA functions

# Reporting ####
data_ous = function( dqa_data ) dqa_data %>% as_tibble %>% ungroup %>%
  count( orgUnit, orgUnitName , level ) %>%
  select( -n )

dqa_years = function( dqa_data ) dqa_data %>% 
  as_tibble %>% ungroup %>%
  count( Year = year( Month ) ) %>%
  select( -n )

dqa_reporting = function( dqa_data , missing_reports = 0 , count.any= TRUE , .cat = FALSE, ... ){
  
  startingMonth = yearmonth( paste0( dqa_years(dqa_data)$Year , "Jan") )
  
  # Excluding last month data reported
  endingMonth = 
    dqa_data %>% as_tibble %>% ungroup %>%
    group_by( year( Month ) ) %>%
    summarize(latest_month = max(Month), .groups = "drop") %>%
    pull( latest_month ) 
  endingMonth[ length(endingMonth) ] = endingMonth[ length(endingMonth) ] - 1
  
  cat( "\n *dqa_reporting - mostFrequentReportingOUs")
  reportingOUS = map( 1:length( startingMonth ) ,  
                      ~mostFrequentReportingOUs( data = dqa_data , 
                            startingMonth = startingMonth[.x] ,
                            endingMonth = endingMonth[.x] ,
                            missing_reports = missing_reports,
                            count.any = count.any,
                            .cat = FALSE )
                      
  )
  
  numberReporting = map_dbl( reportingOUS , length )
  
  return( numberReporting )
}

dqaPercentReporting = function( dqa_data  , .cat = FALSE ){
    if (.cat ) cat('\n*  dqa_functions.R dqaPercentReporting')
    
     year = dqa_years( dqa_data )
    
     if (.cat ) cat('\n -  years:', paste( year, collapse = ","  ))
    
    n_frequently_reporting = dqa_reporting( dqa_data, missing_reports = 0 )
    n_facilities = nrow( data_ous( dqa_data = dqa_data) )
    pr = n_frequently_reporting / n_facilities
    
    if (.cat ) cat('\n -  pr:', paste( pr, collapse = ","  ) )
    
    data = tibble( year, n_frequently_reporting , n_facilities, pr , label = percent(pr, 0.1) )

    if (.cat ) print( data ) 
    return( data )
  }

dqa_reporting_plot = function( data ,  text_size = 18  ){
  
  n_facilities = max( data$n_facilities )
  
  g = ggplot( data = data , aes( x = as.character( Year ) , y = pr, label = label, group = 1  ) ) + 
          geom_line(  linewidth = 1.25 ) +
          geom_text( vjust = -1 , size = text_size / 3  ) +
          scale_y_continuous(labels = scales::percent, 
                             limits = c(0,1) ,
                             expand = expansion(mult = c(0.0, 0.2)) 
                             ) +
          labs( x = "Year" , 
                y = "Percent" , 
                title = "Percent of facilities consistently reporting each year",
                subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" ) ,
                caption = paste( "'Consistently reporting' is reporting every month of the year. Excluding last month data reported." )
                ) + 
          theme_minimal( base_size = text_size  ) 
          # theme( plot.margin = margin(10,10,10,10, "points" ) )
  
  return( g )
}

# Testing Reporting.
# dqa_data = readRDS( 'dqa_data.rds' ) %>% as_tibble()
# glimpse( dqa_data )
# 
# 
# dqa_reporting( dqa_data, missing_reports = 0 )
# dqa_reporting( dqa_data, missing_reports = 1 )
# dqa_reporting( dqa_data, missing_reports = 2 )
# 
# dqa_reporting( dqa_data, missing_reports = 0 , count.any = FALSE )
# 
# # percent frequently reporting
# year = dqa_years( dqa_data )
# n_frequently_reporting = dqa_reporting( dqa_data, missing_reports = 0 )
# n_facilities = nrow( data_ous( dqa_data = dqa_data))
# pr = n_frequently_reporting / n_facilities
# percent(pr, 0.1)
# data = tibble( year, n_frequently_reporting , n_facilities, pr , label = percent(pr, 0.1) )
# 
# ggplot( data = data , aes( x = as.character( Year ) , y = pr, label = label, group = 1  ) ) +
#   geom_line() +
#   geom_text( vjust = -1 ) +
#   ylim( 0, 1 ) +
#   labs( x = "Year" , y = "Percent" , title = "Percent of facilities reporting all 12 months of the year",
#         subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" )
#         )
# dqa_data %>% dqaPercentReporting() %>% dqa_reporting_plot()

# Outliers ####
dqa_outliers = function( yearly.outlier.summary ){
  
  yearly_no_flags = yearly.outlier.summary %>%
    filter( data %in% 'All' ) %>%
    mutate( percent_no_error = 1 - pn , 
            percent_no_error_chr = percent( percent_no_error , 0.1 ) ,
            percent_value_no_error = pe , 
            percent_value_no_error_chr = percent( percent_value_no_error , 0.1 )
    ) 
    
  return( yearly_no_flags )
}

yearly.outlier.summary_plot = function( data, text_size = 18 , label_size = 6 ){
  
 data = data %>%
    select( year, starts_with( 'percent') & ! ends_with( 'chr' ) ) %>%
    pivot_longer( -year ) %>%
    mutate( label = percent( value , 0.1 ))
 
 
 g = ggplot( data = data , aes( x = as.character( year ) , 
                                y = value , color = name , 
                                label = label , group = name   ) ) + 
          geom_line( linewidth = 1.25) +
          geom_text( vjust = -1.5 , size = label_size ) +
   
          annotate("text", x = -1 , y = max(data[ grepl( 'value' , data$name ) , ]$value ), 
                   label = "Magnitude of flagged values relative to sum of all values", 
                   vjust = -3.5 , hjust = -.5 , size = label_size) +
   
          annotate("text", x = -1, y = min(data[ ! grepl( 'value' , data$name ) , ]$value ), 
                   label = "Percentage of values with NO error flags" , 
                   vjust = 2 , hjust = -1 , size = label_size  ) +
   
          scale_y_continuous( labels = scales::percent, limits = c(0,1) , 
                              expand = expand_scale(mult = c(0.1, 0.2)) ) +
          scale_color_hue(  l=40, c=35 ) +
          guides( color = "none" ) +
          labs( x = "Year" , y = "Percent" , 
                title = "Potential Reporting Errors" ,
                subtitle  = paste( "Data flagged as incorrect through outlier algorithms" ) 
                ) + 
          theme_minimal( base_size = text_size ) 
          # theme( plot.margin = margin(50,10,10,10, "points" ) )
  
  return( g )
}

# Testing Outliers
# data.directory = "../HMIS/Formulas/Malawi/"
# dqa_data <- readRDS( paste0( data.directory ,
#                          #"cas confirme_All-levels_5yrs_2022-08-02.rds"
#                           "confirmed cases ipd, opd, and chw_All-levels_9yrs_2023-07-24.rds"
#                          )
#                  ) %>%
#   filter( COUNT > 0 )
# 
# data = dqa_data %>% monthly.outlier.summary() %>%
#   yearly.outlier.summary() %>%
#   dqa_outliers 
#   
# yearly.outlier.summary_plot( data )


# MASE ####
mase_year = function( dqa_data , .year ){
  
  d_all_dataElements = setDT( dqa_data )[ 
          year( Month ) <= .year 
          , 
          .( 
               expected = sum( expected, na.rm = TRUE ) ,
               original = sum( original, na.rm = TRUE ) 
            ) ,
          by = c(  'orgUnit', 'orgUnitName' , "Month") ] %>% 
    as_tibble() %>%
    group_by( orgUnit, orgUnitName )
  
  d.mase = setDT( d_all_dataElements )[ 
          ,
          .(   MASE = mase( actual = original, predicted = expected  ) ,
                n = sum( !is.na( original ) ) ,
               total_expected =  sum( expected , na.rm = T )
          ) ,
          by = c(  'orgUnit', 'orgUnitName' ) ] %>%
          
          as_tibble() %>%
          group_by( orgUnit, orgUnitName ) 
  
  # median.mase = median( d.mase$MASE , na.rm = T )
  
  mean.mase = weighted.mean( d.mase$MASE[ d.mase$MASE<Inf ] , 
                             w = d.mase$total_expected[ d.mase$MASE<Inf ] , 
                             na.rm = TRUE )
  
  n_facilities = nrow( d.mase )
  
  # percent_MASE_under_20 = sum( d.mase$MASE < .2 , na.rm = T ) / sum( d.mase$MASE<Inf , na.rm = T ) 
  
  # ggplot( d.mase ) + geom_histogram( aes(MASE), binwidth = .1 )
  
  return( tibble( Year = .year , Facilities = n_facilities ,  Mean_MASE = mean.mase , label = percent( mean.mase , 0.1 )) )
  
}


dqa_mase = function( dqa_data ){
  
  years = dqa_years( dqa_data )$Year
  
  
  dqa_mase = map_df( years, ~mase_year( dqa_data, .x )  ) 
  
  # summarise begining with 3rd year of data
  dqa_mase[1:2, 3:ncol( dqa_mase) ] = NA
  
  return( dqa_mase )
}

dqa_mase_plot = function( data ){
  mase_txt = "Estimated as 2x the mean absolute scaled error (MASE) of the previouse values
  - The smaller this value, the more accurate the data is, and the more likely that a program can attribute the change to program implementation
  - Year to year change less than this is likely due to random variation"
  
  max_Facilities = max( data$Facilities , na.rm = T )
  
  g = ggplot( data = data , aes( x = as.character( Year ) , y = Mean_MASE , label = label, group = 1  ) ) + 
          geom_line(  linewidth = 1.25 ) +
          geom_text( vjust = -1 , size = 6 ) +
          scale_y_continuous(labels = scales::percent , limits = c(0, 1.5*max(data$Mean_MASE, na.rm = T ))) +
          labs( x = "Year" , y = "Percent" , title = "Minimum Detectable Change for Program Evaluation",
                subtitle  = mase_txt  ,
                caption = "NOTE: MASE calculated begining with 3rd year of data"
                ) + 
          theme_minimal( base_size = 18 )
  
  return( g )
}

# TESTING MASE
# dqa_mase( dqa_data = dqa_data )
# dqa_data %>% dqa_mase %>% dqa_mase_plot
