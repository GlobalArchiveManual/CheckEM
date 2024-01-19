# APP & PACKAGE DEVELOPMENT -----
# Load the package
devtools::load_all()

# Test the shiny app
# CheckEM::runCheckEM()

# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()

# Add  Code > Insert Roxygen Skeleton for every function in the R folder
# Fill out the details
# The next line turns all the R function documentation into .Rd files for the man folder
devtools::document()

# PACKAGE DOWN WEBSITE ----

# Install released version from CRAN
# install.packages("pkgdown")
library(pkgdown)

# This builds the website with every function.
pkgdown::build_reference()

# Run once to create a favicon
# build_favicons()

# Create vignettes to turn into articles (these can either be based on a function or more documentation e.g. workflows)
# usethis::use_vignette("runCheckEM")
pkgdown::build_articles()
pkgdown::build_articles_index()

# Build and create a preview of the website
pkgdown::build_site()

# Turn into a github page (need to commit and push changes for it to show up)
usethis::use_pkgdown_github_pages() # CHOOSE NO TO OVERWRITING THE YML FILE

# PACKAGE VERSIONS
#> What should the new version be? 
#> 
#> 1: major --> 1.0
#> 2: minor --> 0.2
#> 3: patch --> 0.1.1
#> 4:   dev --> 0.1.0.9000