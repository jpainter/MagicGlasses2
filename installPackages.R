if (!require( 'pacman' )){
  install.packages( 'pacman' , dep=TRUE )
  if( !require( 'pacman' ) ) stop("Package not found")
}

libraries =  readLines( 'magicGlasses2Libraries.txt' ) 
include = !( grepl( '#' , libraries ) | nchar( libraries ) == 0 ) 
library.list = libraries[ include ]
pacman::p_load( char = library.list , update = TRUE )