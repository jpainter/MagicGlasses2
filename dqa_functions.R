# DQA functions


data_ous = function( dqa_data ) dqa_data %>% as_tibble %>%
  count( orgUnit, orgUnitName , level ) %>%
  select( -n )

dqa_years = function( dqa_data ) dqa_data %>% as_tibble %>%
  count( Year = year( Month ) ) %>%
  select( -n )


dqa_reporting = function( dqa_data , missing_reports = 0 , count.any= TRUE , .cat = FALSE, ... ){
  
  startingMonth = yearmonth( paste0( dqa_years(dqa_data)$Year , "Jan") )
  endingMonth = yearmonth( paste0( dqa_years(dqa_data)$Year , "Dec") )
  
  
  reportingOUS = map( 1:length( startingMonth ) ,  
                      ~mostFrequentReportingOUs( data = dqa_data , 
                            startingMonth = startingMonth[.x] ,
                            endingMonth = endingMonth[.x] ,
                            missing_reports = missing_reports,
                            count.any = count.any )
                      
  )
  
  numberReporting = map_dbl( reportingOUS , length )
  
  return( numberReporting )
}

dqaPercentReporting = function( dqa_data  , .cat = FALSE ){
    cat('\n*  dqa_functions.R dqaPercentReporting')
    
     year = dqa_years( dqa_data )
    
     if (.cat ) cat('\n -  years:', paste( year, collapse = ","  ))
    
    n_frequently_reporting = dqa_reporting( dqa_data, missing_reports = 0 )
    n_facilities = nrow( data_ous( dqa_data = dqa_data) )
    pr = n_frequently_reporting / n_facilities
    
    cat('\n -  pr:', paste( pr, collapse = ","  ) )
    
    data = tibble( year, n_frequently_reporting , n_facilities, pr , label = percent(pr, 0.1) )

    if (.cat ) print( data ) 
    return( data )
  }

dqa_reporting_plot = function( data ){
  
  n_facilities = max( data$n_facilities )
  
  g = ggplot( data = data , aes( x = as.character( Year ) , y = pr, label = label, group = 1  ) ) + 
          geom_line() +
          geom_text( vjust = -1 ) +
          scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
          labs( x = "Year" , y = "Percent" , title = "Percent of facilities reporting all 12 months of the year",
                subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" ) 
                )
  
  return( g )
}

dqa_outliers = function( yearly.outlier.summary ){
  
  yearly_no_flags = yearly.outlier.summary %>%
    filter( data %in% 'All' ) %>%
    mutate( percent_no_error = 1 - pn , 
            percent_no_error_chr = percent( percent_no_error , 0.1 )
    )
    
  return( yearly_no_flags )
}

yearly.outlier.summary_plot = function( data ){
  
 g = ggplot( data = data , aes( x = as.character( year ) , 
                                y = percent_no_error, 
                                label = percent_no_error_chr, group = 1  ) ) + 
          geom_line() +
          geom_text( vjust = 3 ) +
          scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
          labs( x = "Year" , y = "Percent" , 
                title = "Percent of data with no error flags"
                # subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" ) 
                )
  
  return( g )
}
# Testing.
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

# Outliers
# data.directory = "../HMIS/Formulas/Malawi/"
# dqa_data <- readRDS( paste0( data.directory ,
#                          #"cas confirme_All-levels_5yrs_2022-08-02.rds"
#                           "confirmed cases ipd, opd, and chw_All-levels_9yrs_2023-07-24.rds"
#                          )
#                  ) %>%
#   filter( COUNT > 0 )
# 
# dqa_data %>% monthly.outlier.summary() %>%
#   yearly.outlier.summary() %>%
#   dqa_outliers %>%
#   yearly.outlier.summary_plot()
