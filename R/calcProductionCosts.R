#' @title calcProductionCosts
#' @description calculates agricultural production costs (split into different cost categories)
#' @param datasource Datasource of production costs, currently only "Vittis"
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("ProductionCosts", source = "Vittis")
#' }
#'
calcProductionCosts <- function(datasource = "Vittis", unit = "constant 2017 US$MER") {
  if (datasource == "Vittis") {
    costs <- readSource("Vittis")                                        # US$17/ha
    areas <- calcOutput("Croparea", sectoral = "kcr", aggregate = FALSE) # "million ha"

    # converting to absolute costs
    costs <- costs * areas[, getYears(costs), getNames(costs, dim = 2)]

  } else {
    stop("Source not available")
  }

  if (unit != "constant 2017 US$MER") {

    costs <- toolConvertGDP(costs,
                            unit_in = "constant 2017 US$MER",
                            unit_out = unit,
                            replace_NAs = "no_conversion")
  }

  units <- paste0("mio ", unit)

  return(list(x = costs,
              weight = NULL,
              unit = units,
              description = "production costs"))
}
