if (!require( 'pacman' )){
  install.packages( 'pacman' , dep=TRUE )
  if( !require( 'pacman' ) ) stop("Package not found")
}

if (!require( 'devtools' )){
  install.packages( 'devtools' , dep=TRUE )
  if( !require( 'devtools' ) ) stop("Package not found")
}

# latest packages required for describer
devtools::install_github("glin/reactable")
devtools::install_github("kcuilla/reactablefmtr")
devtools::install_github("agstn/describer")

libraries =  readLines( 'magicGlasses2Libraries.txt' ) 
include = !( grepl( '#' , libraries ) | nchar( libraries ) == 0 ) 
library.list = libraries[ include ]
pacman::p_load( char = library.list , update = TRUE )