% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_api_count.R
\name{ga_api_count}
\alias{ga_api_count}
\title{Retrieve Count Data from the GlobalArchive API}
\usage{
ga_api_count(token, synthesis_id, include_life_history = TRUE)
}
\arguments{
\item{token}{A character string representing your GlobalArchive token for API authentication.}

\item{synthesis_id}{A character string or numeric value representing the GlobalArchive synthesis ID
for which the count data should be retrieved.}

\item{include_life_history}{A logical value indicating whether life history information should
be included in the retrieved data. Defaults to \code{TRUE}. If \code{FALSE}, only basic species information
is included.}
}
\value{
A data frame containing the count data retrieved from the GlobalArchive API. The data frame
includes count data for various subjects, with optional life history details depending on the
\code{include_life_history} parameter.
}
\description{
This function retrieves count data from a GlobalArchive synthesis using an API call. It allows you to
include or exclude life history information in the retrieved data and processes the data accordingly.
}
\examples{
\dontrun{
# Retrieve count data including life history information
count_data <- ga_api_count(username = "your_username", password = "your_password", 
                           synthesis_id = "your_synthesis_id", include_life_history = TRUE)

# Retrieve count data without life history information
count_data <- ga_api_count(username = "your_username", password = "your_password", 
                           synthesis_id = "your_synthesis_id", include_life_history = FALSE)
}
}
