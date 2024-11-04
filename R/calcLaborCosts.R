#' @title calcLaborCosts
#' @description calculates total labor costs in mio. US$MER05 (coverage depends on source: crop, livestock and
#' fish production for USDA, and additionally forestry for GTAP and ILO)
#' @param datasource data source on which the labor costs should be based. Available are ILO, USDA (which also uses data
#' on VoP from FAO), and GTAP.
#' @param dataVersionILO If source is ILO, the version can be chosen. "" for the oldest version, or "monthYear" (e.g.
#' "Aug24") for a newer version)
#' @param subsectors boolean: should output be aggregated or split into available subsectors (crops, livst, forestry,
#' fishery)
#' @param inclFish boolean: should fish labor costs be included?
#' @param inclForest boolean: should forestry labor costs be included (only available for ILO and GTAP)?
#' @param otherLivst boolean: should other_livst category (i.e. beeswax, wool, silkworms, and honey) be included
#' in livestock (only relevant for datasource USDA)?
#' @param gtapVar variable name to use from GTAP (only relevant if source is "GTAP")
#' @param addSubsidies boolean: should subsidy data (from IFPRI) should be added to VoP before applying USDA cost
#' shares (only relevant if datasource is "USDA")
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("LaborCosts", datasource = "ILO")
#' }
#' @importFrom magclass setNames dimSums time_interpolate
#' @importFrom GDPuc toolConvertGDP
#' @importFrom stringr str_split

calcLaborCosts <- function(datasource = "ILO", dataVersionILO = "Aug24", subsectors = TRUE, inclFish = FALSE,
                           inclForest = FALSE, otherLivst = TRUE, gtapVar = "NVFA", addSubsidies = FALSE) {

  # get data from specified source
  if (datasource == "ILO") {

    # ILO employment in agriculture (mio. people)
    iloEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = subsectors,
                          inclFish = inclFish, inclForest = inclForest, dataVersionILO = dataVersionILO)

    # ILO mean weekly hours actually worked per employed person in agriculture (h)
    iloWeeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE, dataVersionILO = dataVersionILO)

    # subset to joint years
    years <- intersect(getItems(iloEmpl, dim = 2), getItems(iloWeeklyHours, dim = 2))
    iloEmpl <- iloEmpl[, years, ]
    iloWeeklyHours <- iloWeeklyHours[, years, ]

    # mean nominal hourly labor cost per employee in agriculture (US$2017MER/h) based on USDA_FAO calculation
    # and regression based on ILO raw data
    iloLaborCosts <- calcOutput("HourlyLaborCosts", dataVersionILO = dataVersionILO, datasource = datasource,
                                fillWithRegression = TRUE, projection = "SSP2", # scenario has no influence until 2020
                                aggregate = FALSE)[, years, ]

    # combine data sets to get total labor costs
    iloTotalHours <- 52.1429 * iloWeeklyHours[, , , drop = TRUE] * iloEmpl # mio. hours
    out <- iloTotalHours * iloLaborCosts[, , , drop = TRUE] # mio. US$MER2017

  } else if (datasource == "USDA") {

    if (isTRUE(inclForest)) stop("Forest labor costs not available for this datasource")

    # factor costs in mio. US$MER2017
    facCostsLivst <- calcOutput("FactorCostsLivst", datasource = "USDA", inclFish = inclFish, 
                                other = otherLivst, aggregate = FALSE)
    facCostsCrops <- calcOutput("FactorCostsCrops", datasource = "USDA", aggregate = FALSE)

    if (isTRUE(inclFish)) {
      facCostsFish <- facCostsLivst[, , "Fisheries"]
      facCostsLivst <- facCostsLivst[, , "Fisheries", invert = TRUE]
    }

    facCostsLivst <- setNames(dimSums(facCostsLivst, dim = 3), "Livestock")
    facCostsCrops <- setNames(dimSums(facCostsCrops, dim = 3), "Crops")

    years <- intersect(getYears(facCostsLivst, as.integer = TRUE), getYears(facCostsCrops, as.integer = TRUE))
    facCosts <- mbind(facCostsLivst[, years, ], facCostsCrops[, years,])
    if (isTRUE(inclFish)) facCosts <- mbind(facCosts, facCostsFish[, years, ])

    # add subsidies to factor costs
    if (isTRUE(addSubsidies)) {
      subsidies <- calcOutput("NonMAgPIEFactorCosts", subtype = "subsidies", aggregate = FALSE)
      # to not loose years, we keep subsidies constant for past years which are not covered by the dataset
      subsidies <- time_interpolate(subsidies,
                                    interpolated_year = setdiff(years, getItems(subsidies, dim = 2)),
                                    integrate_interpolated_years = TRUE,
                                    extrapolation_type = "constant")

      facCosts[, , "Crops"] <- facCosts[, , "Crops"] + subsidies[, years , "Crops"]
      facCosts[, , "Livestock"] <- facCosts[, , "Livestock"] + subsidies[, years , "Livestock"]
    }

    # get labor-capital shares (same for all sectors)
    capitalShare <- calcOutput("AgCapLabourShare", fillWithRegression = TRUE, aggregate = FALSE, projection = "SSP2")

    # interpolate between the five-year-steps
    capitalShare <- time_interpolate(capitalShare,
                               interpolated_year = setdiff(years, getYears(capitalShare, as.integer = TRUE)),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)[, years, ]

    # estimate total labor costs as share of factor costs
    out <- facCosts * (1 - capitalShare)

    # aggregate if subsectors is FALSE, but only in cases where both crops and livestock (and fisheries) are included
    if (isFALSE(subsectors)) {
      incl <- out[, , "Livestock"] * out[, , "Crops"] 
      if (isTRUE(inclFish)) incl <- incl * out[, , "Fisheries"]
      out <- dimSums(out, dim = 3)
      out[incl == 0] <- 0
    }

  } else if (datasource == "GTAP") {

    # gtap data in mio. current US$MER
    if (gtapVar == "NVFA") {
      gtap <- dimSums(readSource("GTAPv8v9", "9:SF01"), dim = c("dir", "purchvalue"))
    } else if (gtapVar == "VFM") {
      gtap <- readSource("GTAPv8v9", "9:VFM")
      getSets(gtap) <- c("reg", "year", "demd_comm", "prod_comm")
    } else if (gtapVar == "EVFA") {
      gtap <- readSource("GTAPv8v9", "9:AG03")
      getSets(gtap) <- c("reg", "year", "demd_comm", "prod_comm")
    } else {
      stop("This GTAP variable is not available for labor costs")
    }

    labor <- c("unsklab", "sklab")

    # https://www.gtap.agecon.purdue.edu/databases/contribute/detailedsector.asp
    kcr <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr")
    kli <- c("ctl", "oap", "rmk")
    fish <- c("fsh")
    forest <- c("frs")

    # get sectors according to function inputs
    sectors <- list(kcr, kli)
    sectorNames <- c("Crops", "Livestock")
    if (isTRUE(inclFish)) {
      sectors <- c(sectors, fish)
      sectorNames <- c(sectorNames, "Fisheries")
    }
    if (isTRUE(inclForest)) {
      sectors <- c(sectors, forest)
      sectorNames <- c(sectorNames, "Forestry")
    }

    .getSector <- function(items, name) {
      return(setNames(dimSums(gtap[, , list("demd_comm" = labor, "prod_comm" = items)],
                              dim = c("demd_comm", "prod_comm")), name))
    }

    laborCosts <- mbind(Map(.getSector, items = sectors, name = sectorNames))

    # aggregate if subsectors == FALSE
    if (isFALSE(subsectors)) laborCosts <- dimSums(laborCosts, dim = 3)

    # convert to USDMER2017 (for countries missing the inflation factor, we assume no inflation)
    out <- toolConvertGDP(laborCosts, unit_in = "current US$MER",
                          unit_out = "constant 2017 US$MER", replace_NAs = "no_conversion")

  } else {
    stop("Data source not available")
  }

  return(list(x = out,
              weight = NULL,
              unit = "mio. USD2017MER",
              description = "labor costs in agriculture"))
}
