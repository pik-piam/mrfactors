#' @title calcFactorCostsCrops
#' @description calculates factor costs for crop production in mio. US$MER05
#' @param datasource only source available is "USDA" (calculates factor costs by applying factor cost share from USDA
#' to VoP from FAO)
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("FactorCostsCrops")
#' }
#' @importFrom magclass setNames dimSums time_interpolate

calcFactorCostsCrops <- function(datasource = "USDA", unit = "constant 2017 US$MER") {

  if (datasource == "USDA") {
    # Value of Production for livestock in US$MER2017
    vopCrops <- calcOutput("VoPcrops", fillGaps = TRUE, aggregate = FALSE, unit = "constant 2017 US$MER")

    # no VoP data before 1991, data for 2019 incomplete
    years <- setdiff(getYears(vopCrops, as.integer = TRUE), c(1960:1990, 2019))

    # USDA labor cost shares
    shares <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE)
    shares <- dimSums(shares[, , c("Labor", "Capital")], dim = 3)

    # closest 5-year step before and after start of VoP data needed for interpolation of shares
    y <- intersect(paste0("y", seq(min(years) - min(years) %% 5, max(years) - max(years) %% 5 + 5, 5)),
                   getItems(shares, dim = 2))

    # filling missing values with region average, using production as weight
    h12 <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
    weight <- dimSums(collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"]), dim = 3.1)
    weight <- time_interpolate(weight, interpolated_year = setdiff(y, getItems(weight, dim = 2)),
                               extrapolation_type = "constant", integrate_interpolated_years = TRUE)[, y, ]
    shares <- toolFillWithRegionAvg(shares[, y, ], valueToReplace = 0, weight = weight,
                                    regionmapping = h12, verbose = FALSE, warningThreshold = 1)

    # interpolate between the five-year-steps
    shares <- time_interpolate(shares,
                               interpolated_year = setdiff(years, getYears(shares, as.integer = TRUE)),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)[, years, ]

    # estimate total labor costs as share of VoP
    out <- vopCrops[, years, ] * shares
  } else {
    stop("Datasource not available")
  }

  if (unit != "constant 2017 US$MER") {
    out <- toolConvertGDP(out,
                          unit_in = "constant 2017 US$MER",
                          unit_out = unit,
                          replace_NAs = "no_conversion")
  }


  units <- paste0("mio ", unit)

  return(list(x = out,
              weight = NULL,
              unit = units,
              description = "Factor costs for crop production"))
}
