#' @title readRegressionsILO
#' @description Read regression coefficients which are used to fill missing values of ILO datasets
#' @param subtype Type of ILOSTAT data for which regression coefficients should be read
#' \itemize{
#' \item `AgEmplShare`: regression coefficients for sqr(ag empl share) ~ log(GDP pc PPP)
#' \item `HourlyLaborCosts`: regression coefficients for hourly labor costs ~ GDP pc MER (old version) or 
#' log(hourly labor costs) ~ log(GDP pc MER) (new version)
#' }
#' @param version version of regression and underlying data. "" for the oldest version, or "monthYear" (e.g. "July23")
#' for newer version
#' @return regression coefficients as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("RegressionsILO", "AgEmpl")
#' }
#' @importFrom magclass read.magpie

readRegressionsILO <- function(subtype = "AgEmplShare", version = "July23") {

  # valid subtype?
  if (!(subtype %in% c("AgEmplShare", "HourlyLaborCosts"))) stop("Invalid subtype.")

  # valid version?
  subtype <- ifelse(version == "", subtype, 
                      paste(subtype, version, sep = "_"))
  if (!file.exists(paste0(subtype, ".csv"))) {
    stop("This version of the regression has not been added to the source folder yet.")
  }

  # return regression coefficients
  res <- read.magpie(paste0(subtype, ".csv"))
  return(res)
}
