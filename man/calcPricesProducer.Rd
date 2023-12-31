% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPricesProducer.R
\name{calcPricesProducer}
\alias{calcPricesProducer}
\title{calcPricesProducer}
\usage{
calcPricesProducer(
  products = "kcr",
  calculation = "VoP",
  weighting = "production"
)
}
\arguments{
\item{products}{either "kcr" or "kcl"}

\item{calculation}{type of calculation "FAO" (directly reads the data), "VoP"
calculates as VoP/Production, only "FAO" available for "kli" products.}

\item{weighting}{either "production" (default) or "consumption" based weighting}
}
\value{
magpie object. prices in year specific annual
}
\description{
producer prices for agricultural products. 05USDMER/tDM prices from FAO, no currency conversion
}
\examples{
\dontrun{
a <- calcOutput("PricesProducer")
}

}
\seealso{
[calcOutput()]
}
\author{
Edna J. Molina Bacca
}
