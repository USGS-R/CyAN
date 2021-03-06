% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{add_EPA_recreational_threshold}
\alias{add_EPA_recreational_threshold}
\title{Add EPA recreational threshold}
\usage{
add_EPA_recreational_threshold(cyan_data)
}
\arguments{
\item{cyan_data}{a data frame from \code{get_cyan_data()} with
\code{collect = TRUE}}
}
\value{
cyan_data with an additional column EPA_REC_EXCEEDED - a logical column
indicating whether the applicable EPA recreational treshold was exceeded
}
\description{
Add a column indicating whether the EPA recreational trehsold was exceeded
}
\examples{

#Connect to the example database bundled with CyAN
path <- system.file("extdata", "example.db", package = "CyAN")
db_connection <- connect_cyan(path)

#Get all of the data for microcystis and cylindrospermopsin
ks_toxins <- get_cyan_data(db_connection, collect = TRUE,
                             parameters = c("P0073", "P0074"),
                             states = "KS")

#Add any applicable recreational for toxins
ks_toxins_wEPA <- add_EPA_recreational_threshold(ks_toxins)

}
