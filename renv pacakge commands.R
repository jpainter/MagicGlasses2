
# Initial method to update packages with renv library, but it may fail.  
renv::restore()

# update packages so that not trying to install old version
renv::update()

# Restore packages except those that tend to be problematic
renv::install(exclude = c('brolgar', 'bsts', 'rethinking', 'cmdstanr', 'terra'))

# Check to see if any packages orphaned
renv::status()

# May need to install non-CRAN packages
renv::install(  'class' )
renv::install(  'terra' )
renv::install( 'bsts' )

getOption("repos")
repos = c( CRAN = "https://cloud.r-project.org" , cmdstanr = "https://mc-stan.org/r-packages/" )
options( repos = repos )
install.packages( "cmdstanr" )
cmdstanr::install_cmdstan( overwrite = TRUE )

renv::install( c("coda","mvtnorm","devtools","loo", "dagitty") , prompt = FALSE )
devtools::install_github("rmcelreath/rethinking")

# recheck
renv::status()

# Save changes
renv::snapshot( exclude = c( 'rethinking', 'cmdstanr' ) )
