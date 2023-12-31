#' @title readILOSTAT
#' @description Read in ILOSTAT data that has been downloaded from the ILOSTAT website
#'
#' @param subtype Type of ILOSTAT data that should be downloaded, version specified by suffic "_MonthYear" (month and
#' year of download)
#' \itemize{
#' \item `EmplByActivityModelled`: "Employment by sex and economic activity -- ILO modelled estimates,
#' Nov. 2020 (thousands)"
#' \item `WeeklyHoursByActivity`: "Mean weekly hours actually worked per employed person by sex
#' and economic activity"
#' \item `HourlyLaborCostsByActivity`: "Mean nominal hourly labour cost per employee by economic activity"
#' \item `EmplByISIC2`: "Employment by sex and economic activity - ISIC level 2 (thousands) | Annual"
#' \item `EmplByActivityMonthly`: "Employment by sex and economic activity (thousands) | Monthly"
#' \item `EmplByActivityMonthlyAdj`: "Employment by sex and economic activity, seasonally adjusted
#' series (thousands) | Monthly"
#' \item `EmplByActivityAndStatus`: "Employment by sex, status in employment and economic activity
#' (thousands) | Annual"
#' \item `WeeklyHoursByActivityMonthly`: "Mean weekly hours actually worked per employee by sex and economic
#'  activity | Monthly"
#'  \item `WeeklyHoursByISIC2`: "Mean weekly hours actually worked per employed person by sex and economic
#'  activity - ISIC level 2 | Annual"
#'  \item `WeeklyHoursEmployeesByISIC2`: "Mean weekly hours actually worked per employee by sex and economic
#'  activity - ISIC level 2 | Annual"
#'  \item `LaborIncomeShareGDPModelled`: "Labour income share as a percent of GDP -- ILO modelled estimates,
#'  July 2019 (%) | Annual"
#'  \item `OutputPerWorkerModelled`: "Output per worker (GDP constant 2010 US $) -- ILO modelled estimates,
#'  Nov. 2020 | Annual"
#' }
#' @return ILOSTAT data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("ILOSTAT", "EmplByActivityModelled")
#' }
#' @importFrom utils read.table
#' @importFrom magclass as.magpie getSets
#' @importFrom stringr str_split
#' @importFrom dplyr arrange

readILOSTAT <- function(subtype) {

  # date to separate different versions of data
  indicator <- str_split(subtype, "_", simplify = TRUE)[1, 1]

  # get indicator ID of dataset
  indicatorIDs <- c(
    EmplByActivityModelled       = "EMP_2EMP_SEX_ECO_NB_A",
    WeeklyHoursByActivity        = "HOW_TEMP_SEX_ECO_NB_A",
    HourlyLaborCostsByActivity   = "LAC_4HRL_ECO_CUR_NB_A",
    EmplByISIC2                  = "EMP_TEMP_SEX_EC2_NB_A",
    EmplByActivityMonthly        = "EMP_TEMP_SEX_ECO_NB_M",
    EmplByActivityMonthlyAdj     = "EMP_TEM1_SEX_ECO_NB_M",
    EmplByActivityAndStatus      = "EMP_TEMP_SEX_STE_ECO_NB_A",
    WeeklyHoursByActivityMonthly = "HOW_XEES_SEX_ECO_NB_M",
    WeeklyHoursByISIC2           = "HOW_TEMP_SEX_EC2_NB_A",
    WeeklyHoursEmployeesByISIC2  = "HOW_XEES_SEX_EC2_NB_A",
    LaborIncomeShareGDPModelled  = "LAP_2GDP_NOC_RT_A",
    OutputPerWorkerModelled      = "GDP_205U_NOC_NB_A"
  )
  indicatorID <- toolSubtypeSelect(indicator, indicatorIDs)

  # read data
  ilo <- read.table(paste0(indicatorID, ".csv"), header = TRUE)

  # remove unreliable data
  if ("obs_status" %in% colnames(ilo)) {
    ilo <- ilo[!(ilo$obs_status %in% c("Unreliable", "Break in series", "Not significant")), ]
    ilo <- ilo[, setdiff(colnames(ilo), "obs_status")]
  }

  # clean up descriptions
  for (n in seq(1, ncol(ilo))) {
    if (colnames(ilo)[n] %in% c("ref_area", "time", "obs_value")) next
    namesSplit <- str_split(ilo[, n], ": ", simplify = TRUE)
    if (length(unique(namesSplit[, 1])) == 1) {
      namesSplit[, 1] <- str_split(namesSplit[, 1], ", ", simplify = TRUE)[, 1]
      subSplit <- str_split(namesSplit[, 1], " \\(|\\)$", simplify = TRUE)
      colnames(ilo)[n] <- gsub("[()]", "", gsub(" |-|, |\\.", "_", tolower(subSplit[1, 1])))
      if (dim(subSplit)[2] > 1) namesSplit[, 2] <- paste(subSplit[, 2], namesSplit[, 2], sep = ": ")
      ilo[, n] <- gsub("\\.", "", namesSplit[, 2])
    } else {
      categoriesSplit <- str_split(namesSplit[, 1], "( \\()|\\)", simplify = TRUE)
      if (length(unique(categoriesSplit[, 1])) > 1) stop("Different categories")
      colnames(ilo)[n] <- gsub(" |-|\\.", "_", tolower(unique(categoriesSplit[, 1])))
      ilo[, n] <- gsub("\\.", "", paste0(gsub("-", "_", categoriesSplit[, 2]), ": ", namesSplit[, 2]))
    }
  }
  if (length(grep("^X[0-9]", ilo[, "ref_area"])) > 0) ilo <- ilo[-grep("^X[0-9]", ilo[, "ref_area"]), ]

  # split time dimension for monthly data
  if (length(grep("M", ilo$time)) > 0) {
    ilo$month <- as.integer(str_split(ilo$time, "M", simplify = TRUE)[, 2])
    ilo$time <- paste0("y", str_split(ilo$time, "M", simplify = TRUE)[, 1])
    ilo <- arrange(ilo, ilo$month)
    ilo$month <- month.abb[ilo$month]
    ilo <- ilo[, c(setdiff(colnames(ilo), "obs_value"), "obs_value")]
  }

  # transform into magclass object
  ilo <- arrange(ilo, ilo$time)
  ilo <- as.magpie(ilo, temporal = c("time", "month"), spatial = "ref_area")
  getSets(ilo)[1:2] <- c("region", "year")

  return(ilo)
}
