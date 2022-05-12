
install.packages("yaml")
renv::dependencies()
# renv::snapshot()



if (!require( 'pacman' )){
  install.packages( 'pacman' , dep=TRUE )
  if( !require( 'pacman' ) ) stop("Package not found")
}


install.packages("remotes")
remotes::install_github("davidtedfordholt/fable.bsts")

# if (!require( 'devtools' )){
#   install.packages( 'devtools' , dep=TRUE )
#   if( !require( 'devtools' ) ) stop("Package not found")
# }

# latest packages required for describer
# devtools::install_github("glin/reactable")
# devtools::install_github("kcuilla/reactablefmtr")
# devtools::install_github("agstn/describer")

# MagicGlasses2Libraries
libraries =  readLines( 'magicGlasses2Libraries.txt' ) 
include = !( grepl( '#' , libraries ) | nchar( libraries ) == 0 ) 
library.list = trimws( libraries[ include ] )

installed = installed.packages()

needed = setdiff( library.list , installed )

# pacman::p_load( char = library.list , update = TRUE )

if ( length( needed ) > 0 ) renv::install( packages = needed, type = "binary" )

# save libraries
renv::snapshot()

# renv::restore(packages = "renv")
