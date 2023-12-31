% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_ids.R
\name{read_id}
\alias{read_id}
\title{Read Gene Symbols from CSV file into up and down regulated lists}
\usage{
read_id(csv_path)
}
\arguments{
\item{csv_path}{absolute path of CSV file containing gene symbols and sign}
}
\value{
list containing up_regulated and down_regulated genes symbols
}
\description{
The function is used to read symbols from a CSV file. The gene symbols should be in the first column
}
\author{
Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
}
