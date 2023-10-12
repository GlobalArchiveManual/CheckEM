# Load CheckEM
devtools::load_all()

# Install released version from CRAN
# install.packages("pkgdown")
library(pkgdown)

# Create vignettes to turn into articles
usethis::use_vignette("runCheckEM")
pkgdown::build_articles()
pkgdown::build_articles_index()

# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()

# Build and create a preview
pkgdown::build_site()

# Turn into a github page
usethis::use_pkgdown_github_pages()


# Promoting
# Once your finalized site is built and published on the web, you should publicize its URL in a few places:
#   
#   The URL field of your package DESCRIPTION, alongside a link to its source:
#   
#   URL: https://pkgdown.r-lib.org, https://github.com/r-lib/pkgdown
# (usethis::use_pkgdown_github_pages() does this for you.)
# 
# Your repository description on GitHub.
# 
# On Twitter (make sure to include #rstats).