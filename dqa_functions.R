# DQA functions


data_ous = function( dqa_data ) dqa_data %>%
  count( orgUnit, orgUnitName , level ) %>%
  select( -n )

dqa_years = function( dqa_data ) dqa_data %>%
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
# percent(pr, 1.1)
# data = tibble( year, n_frequently_reporting , n_facilities, pr , label = percent(pr, 1.1) )
# 
# ggplot( data = data , aes( x = as.character( Year ) , y = pr, label = label, group = 1  ) ) +
#   geom_line() +
#   geom_text( vjust = -1 ) +
#   ylim( 0, 1 ) +
#   labs( x = "Year" , y = "Percent" , title = "Percent of facilities reporting all 12 months of the year",
#         subtitle  = paste( 'Out of the number of facilities that have ever reported (' , n_facilities , ")" )
#         )

                         