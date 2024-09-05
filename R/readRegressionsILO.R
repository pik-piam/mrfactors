#' @title readRegressionsILO
#' @description Read regression coefficients which are used to fill missing values of ILO datasets
#' @param subtype Type of ILOSTAT data for which regression coefficients should be read
#' \itemize{
#' \item `AgEmplShare`: regression coefficients for sqr(ag empl share) ~ log(GDP pc PPP)
#' \item `HourlyLaborCosts`: regression coefficients for hourly labor costs ~ GDP pc MER (old version) or
#' log(hourly labor costs) ~ log(GDP pc MER) (new version)
#' }
#' The version of regression and underlying data can be chosen by adding a suffix to the subtype, "" for the oldest
#' version, or "_monthYear" (e.g. "_Aug24") for newer version
#' @return regression coefficients as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("RegressionsILO", "AgEmpl")
#' }
#' @importFrom magclass read.magpie

readRegressionsILO <- function(subtype = "AgEmplShare") {

  # valid regression type?
  regrType <- str_split(subtype, "_")[[1]][1]
  if (!(regrType %in% c("AgEmplShare", "HourlyLaborCosts"))) stop("Invalid subtype.")

  # data version
  dataVersion <- str_split(subtype, "_")[[1]][2]
  if (!is.na(dataVersion)) {
    month <- match(substr(dataVersion, 1, 3), month.abb)
    month <- ifelse(month < 10, paste0("0", month), as.character(month))
    dataVersion <- as.numeric(paste0(substr(dataVersion, 4, 5), month))
  } else {
    dataVersion <- 0
  }
  if (dataVersion < 2408) {
    stop("This version of the regression has been created before the GDP update in August 2024 and is thus based ",
         "on GDP using 2005 instead of 2017 as base year. It is incompatible with the data used in this version of ",
         "mrfactors. Please used mrfactors 0.4.0 or ealier when using this ILO data version.")
  }

  # valid version?
  if (!file.exists(paste0(subtype, ".csv"))) {
    stop(paste0("This version of the regression (", subtype, ") has not been added to the source folder yet."))
  }

  # return regression coefficients
  res <- read.magpie(paste0(subtype, ".csv"))
  return(res)
}
