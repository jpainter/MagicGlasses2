# Ingest data downoaded from data dictionary tool

ingest_formula_data = function( 
                          filename , 
                          leaf = TRUE , # Filter to leaf facilities
                          levelNumber = NULL , # if not leaf, filter to level number
                          summary = TRUE , # Include summary data
                          total.name = 'Total' ,  # Name for summary data 
                          guessMax = 21474836 , # max number of rows to search for coltype 
                          pivotWider = FALSE 
                          ){
            
            # formula elements
            data = readxl::read_excel( filename , 'formulaData' , guess_max = guessMax )  %>%
                mutate(  period = zoo::as.yearmon( period, "%Y%m") %>%
                            yearmonth( . ) 
                ) %>%  
                unite( 'de' , dataElement, Categories , remove = TRUE ) %>%
                select( - dataElement.id , - categoryOptionCombo.ids , -COUNT ) %>%
                mutate( SUM = as.numeric( SUM ) )
            
            if ( pivotWider | summary ){
                
                data = data %>%  
                    pivot_wider( names_from = de , 
                                 values_from = SUM ) 
            
                if ( any( grepl( 'NA_NA' , names( data ))) ) data = select( data , -NA_NA )
            } 

            if ( summary ){
                
                sd = read_excel( filename , 'summaryData' , guess_max = guessMax )
                
                # Ensure the column named 'sum' or 'Total', is 'Total'                        )  
                if ( any(grepl( 'sum', names( sd ), ignore.case = T )) ) sd = sd %>% rename( Total = sum )
         
                summaryData = sd %>%
                    
                    select(orgUnit, period, Total , Count.Any , Count.Complete ) %>%
                    
                    mutate(  period = zoo::as.yearmon( period, "%Y%m") %>%
                                            yearmonth( . ) 
                                        ) %>%
                    rename( !! total.name := Total )
                
                data  =     left_join( data , 
                                      summaryData , 
                                      by = c( 'orgUnit',  'period' ) )

            }
            
            if ( leaf ){
                
                if ( 'effectiveLeaf' %in% names( data )){
                    data = data %>% 
                    filter( effectiveLeaf == TRUE  )
                    
                } else {
                    data = data %>% 
                    filter( leaf == TRUE  ) 
                }
                
                
            } else {
                
                if ( !is.null( levelNumber )) data = data %>% filter( level == levelNumber  )
            }
            
}
# Test:
# library( tidyverse ) ; library( readxl ) ; library( tsibble ) ;
# library( data.tree ); library( igraph )
# dir = '../dataDictionary/dhis2_dictionary/Formulas/Burkina Faso/BF_DQA/'
# 
# f = paste0( dir , "Burkina Faso_1. OPD Attendance_All levels_months_last_4_years_2020-10-23.xlsx")
# 
# d = ingest_formula_data( filename = f , total.name = 'totalAttendance' , leaf = TRUE )
# glimpse( d )
