% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_pipeline.R
\name{run_pipeline}
\alias{run_pipeline}
\title{Run the main pipeline for getting drugs from differentail expression profile}
\usage{
run_pipeline(degs_path, output_path = NULL)
}
\arguments{
\item{degs_path}{path to csv file containing degs see example file at https://raw.githubusercontent.com/MohmedSoudy/datasharing/master/differential-expression.csv}

\item{output_path}{absolute path to output directory}
}
\value{
significant drug data frame after the re-ranking step
}
\description{
The function is used to run the main pipeline by extracting the drug list given differential expressed genes
}
\author{
Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
}
