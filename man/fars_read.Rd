% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read filename}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the name of the file to read.}
}
\value{
This function returns a dataframe with the data contained in the specified file.
}
\description{
This function reads a csv file.
}
\examples{
\dontrun{
fars_read("accident_2013.csv.bz2")
}

}
