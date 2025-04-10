% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcAgCapLabourShare.R
\name{calcAgCapLabourShare}
\alias{calcAgCapLabourShare}
\title{calcAgCapLabourShare}
\usage{
calcAgCapLabourShare(fillWithRegression = TRUE, projection = FALSE)
}
\arguments{
\item{fillWithRegression}{boolean: should missing values be filled based on a regression between capital share
and GDPpcPPP (calibrated to countries)}

\item{projection}{either FALSE or SSP on which projections should be based. Only relevant if fillWithRegression is
TRUE.}
}
\value{
MAgPIE object
}
\description{
This function calculates historical capital shares (Capital + Labour)
of the factor requirements using USDA
}
\examples{
\dontrun{
calcOutput("calcAgCapLabourShare")
}
}
\seealso{
[calcAgCapLabourShare()]
}
\author{
Edna J. Molina Bacca, Debbora Leip
}
