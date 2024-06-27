
# Initial method to update packages with renv library, but it may fail.  
renv::restore()

# Compare installed libraries with those required for project (renv::dependencies)
installed = installed.packages()
dependencies = renv::dependencies()
setdiff( rownames(installed) , dependencies$Package )

# update packages so that not trying to install old version
renv::update()

# Restore packages except those that tend to be problematic
renv::install(exclude = c( 'bsts', 'rethinking', 'cmdstanr', 'terra'))

# Check to see if any packages orphaned
renv::status()

# May need to install non-CRAN packages
renv::install(  'class' )
renv::install(  'terra' )
renv::install( 'bsts' )

# see https://mc-stan.org/cmdstanr/
getOption("repos")
repos = c( CRAN = "https://cloud.r-project.org" , cmdstanr = "https://mc-stan.org/r-packages/" )
options( repos = repos )
install.packages( "cmdstanr" )
install_cmdstan(cores = 2)

# windows...
# Download the files for cmdstan here 2 and unzip the content so that you have all the files inside <your user>/Documents/.cmdstan/cmdstan-2.34.1.
# In the console run cmdstanr::set_cmdstan_path(). If this does not work, then type cmdstanr::set_cmdstan_path("<your user>/Documents/.cmdstan/cmdstan-2.34.1").

renv::install( c("coda","mvtnorm","devtools","loo", "dagitty") , prompt = FALSE )

# If github token needed...if no token, run first two lines
# usethis::create_github_token() 
# gitcreds::gitcreds_set()
usethis::git_sitrep()

devtools::install_github("rmcelreath/rethinking" )

# recheck
renv::status()

# Save changes
renv::snapshot( exclude = c( 'rethinking', 'cmdstanr' ) )
renv::status()

## miniCRAN
library( miniCRAN )
# packages in use by MG2
packages.all = installed.packages()

makeRepo( packages.all , path = "./magiCRAN", type = "win.binary", repos = repos )
makeRepo( c( 'rethinking', 'cmdstanr' ) , path = "./magiCRAN", type = "source" , repos = repos )

# List all files in miniCRAN
packages.magiCRAN  = list.files( path = "./magiCRAN" , recursive = TRUE)
pkgAvail(repos = "./magiCRAN", type = "win.binary")

# utils::install.packages(  packages.all , repos = "./magiCRAN" )
