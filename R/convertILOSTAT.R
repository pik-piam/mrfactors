#' @title convertILOSTAT
#' @description transforms currencies where applicable, and fills missing countries in ILOSTAT data with 0
#' @param x unconverted magpie object from read-script
#' @return Data as MAgPIE object with common country list
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' a <- readSource("ILOSTAT", subtype = "EmplByActivityModelled", convert = TRUE)
#' }
#'
convertILOSTAT <- function(x) {
  # fill missing countries
  x <- toolCountryFill(x, fill = 0, no_remove_warning = c("KOS", "CHA"))

  # transform currencies if applicable
  if ("currency" %in% getSets(x)) {
    if ("2021 PPP $" %in% getNames(x, dim = "currency")) {
      x[, , "2021 PPP $"] <- toolConvertGDP(x[, , "2021 PPP $"],
                                            unit_in = "constant 2021 Int$PPP",
                                            unit_out = "constant 2017 Int$PPP",
                                            replace_NAs = c("linear", "no_conversion"))
    }

    x[, , "US dollars"] <- toolConvertGDP(x[, , "US dollars"],
                                          unit_in = "current US$MER",
                                          unit_out = paste("constant 2017 US$MER"),
                                          replace_NAs = c("linear", "no_conversion"))

    # update unit description
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "Local currency"] <- "current LCU"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "2021 PPP $"] <- "US$PPP2017"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "US dollars"] <- "US$MER2017"
  }

  # set missing values to 0
  x[!is.finite(x)] <- 0

  return(x)
}
