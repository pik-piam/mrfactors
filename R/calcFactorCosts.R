#' @title calcFactorCosts
#' @description combines factor costs for crop and livestock production in mio. US$MER05
#' @param datasource only source available is "USDA" (calculates factor costs by applying factor cost share from USDA
#' to VoP from FAO)
#' @param otherLivst boolean: should FAO livestock categories that can't be matched to MAgPIE categories (i.e. beeswax,
#' wool, silkworms, and honey) be reported as "livst_other"?
#' @param subsectors boolean: should output be aggregated or reported by crop and livestock subsectors?
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("FactorCosts")
#' }
#' @importFrom magclass setNames dimSums time_interpolate

calcFactorCosts <- function(datasource = "USDA", otherLivst = FALSE, subsectors = FALSE,
                            unit = "constant 2017 US$MER") {

  facCostsCrops <- calcOutput("FactorCostsCrops", datasource = datasource, unit = unit, aggregate = FALSE)
  facCostsLivst <- calcOutput("FactorCostsLivst", datasource = datasource, otherLivst = otherLivst,
                              unit = unit, aggregate = FALSE)

  factorCosts <- mbind(facCostsCrops, facCostsLivst)

  if (isFALSE(subsectors)) {
    factorCosts <- dimSums(factorCosts, dim = 3)
  }

  units <- paste0("mio ", unit)

  return(list(x = factorCosts,
              weight = NULL,
              unit = units,
              description = "Factor costs for crop and livestock production"))
}
