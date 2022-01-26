libraries =  readLines( 'magicGlasses2Libraries.txt') 
include = !( grepl( '#' , libraries ) | nchar( libraries ) == 0 ) 
library.list = libraries[ include ]
pacman::p_load( library.list , update = TRUE )
