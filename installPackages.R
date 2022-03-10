if (!require( 'pacman' )){
  install.packages( 'pacman' , dep=TRUE )
  if( !require( 'pacman' ) ) stop("Package not found")
}

if (!require( 'devtools' )){
  install.packages( 'devtools' , dep=TRUE )
  if( !require( 'devtools' ) ) stop("Package not found")
}

# latest packages required for describer
# devtools::install_github("glin/reactable")
# devtools::install_github("kcuilla/reactablefmtr")
# devtools::install_github("agstn/describer")

# MagicGlasses2Libraries
libraries =  readLines( 'magicGlasses2Libraries.txt' ) 
include = !( grepl( '#' , libraries ) | nchar( libraries ) == 0 ) 
library.list = trimws( libraries[ include ] )

# pacman::p_load( char = library.list , update = TRUE )

renv::install( packages = library.list )

# save libraries
renv::snapshot()

renv::restore(packages = "renv")
