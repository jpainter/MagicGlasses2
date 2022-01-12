# Functions for deviation from expected 

files = function(  search = 'All' , type = 'xlsx' , other = "" , ... ){
                        
                      dir = file.dir( ... )
                      dir.files = list.files( dir )
                      search_and_type =  
                          str_detect( dir.files, fixed( search , ignore_case=TRUE )  )  &
                           grepl( paste0( type , '$' ) , dir.files, 
                                  ignore.case =  TRUE  ) &
                          grepl( other , dir.files , ignore.case = TRUE ) 
                      files = dir.files[  search_and_type   ]
                      return( files[ rev( order( files )) ]  )
                      }

file.dir = function( country = 'Sierra Leone' ,
                     dir.base = '../dataDictionary/dhis2_dictionary/Formulas/' ){
  paste0( dir.base , country , "/")
}

string_wrap = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
string_wrap = Vectorize(string_wrap)

