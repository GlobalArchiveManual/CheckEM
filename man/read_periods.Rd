% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_periods.R
\name{read_periods}
\alias{read_periods}
\title{A function to read EventMeasure format period data (_Period.txt)}
\usage{
read_periods(dir, method = "BRUVs", recursive = FALSE)
}
\arguments{
\item{dir}{The directory where the .txt files are saved}
}
\value{
A data frame which contains all Period annotations, with a column for campaignID and sample
}
\description{
A function to read EventMeasure format period data (_Period.txt)
}
