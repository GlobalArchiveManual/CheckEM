% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_rds_from_github.R
\name{load_rds_from_github}
\alias{load_rds_from_github}
\title{Load an RDS File from a Private GitHub Repository}
\usage{
load_rds_from_github(url, pat = NULL)
}
\arguments{
\item{url}{A character string specifying the raw URL to the RDS file in the private GitHub repository.}

\item{pat}{A character string containing the GitHub Personal Access Token (PAT) with access to the repository.
If not provided, the function will attempt to retrieve it using \code{gitcreds} or prompt the user to enter it.}
}
\value{
The content of the RDS file as an R object, or \code{NULL} if access is denied or if the file cannot be loaded.
}
\description{
This function loads an RDS file from a private GitHub repository. It uses a provided or interactive GitHub
Personal Access Token (PAT). If the PAT is not provided, the function will try to retrieve it using \code{gitcreds}
or prompt the user to enter it manually. It also checks if the user has access to the repository and provides
instructions for requesting access if needed.
}
\details{
If the PAT is not provided, the function will attempt to retrieve it from \code{gitcreds}. If \code{gitcreds} does not
provide a PAT, the user will be prompted to enter one manually. If the function encounters a 404 status code,
it will inform the user to contact Brooke Gibbons at brooke.gibbons@uwa.edu.au with their GitHub username to request
access to the repository.
}
\examples{
# Define the URL to the RDS file in your private GitHub repository
rds_url <- "https://raw.githubusercontent.com/GlobalArchiveManual/australia-synthesis-2024/main/data/tidy/australian-synthesis_covariates.RDS"

# Call the function to load the RDS file
data <- load_rds_from_github(rds_url)

# Print or work with the data
if (!is.null(data)) {
  print(data)
}

}
