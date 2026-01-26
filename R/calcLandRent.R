#' @title calcLandRent
#' @description Calculates factor intensity in crop production for ladn rent from USDA (Inputs share)
#' and FAO (Value of Production) and productivity in constant 2017 US$MER per ha.
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return magpie object of the land factor requirements in USD/ha per crop
#' @author Edna J. Molina Bacca
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("LandRent")
#' }
#'
calcLandRent <- function(unit = "constant 2017 US$MER") {
  # Cropland Area
  cropAreaAll  <- collapseDim(calcOutput("Croparea", aggregate = FALSE))
  cropAreaAll[cropAreaAll < 1e-3] <- 0 # remove very small values to avoid strange results when dividing

  # Value of production
  vopCrops <- calcOutput("VoPcrops", aggregate = FALSE, unit = "constant 2017 US$MER")
  vopCrops[vopCrops < 1e-4] <- 0 # remove very small values to avoid strange results when dividing

  gnames <- intersect(getNames(vopCrops), getNames(cropAreaAll))
  gyears <- intersect(getYears(vopCrops), getYears(cropAreaAll))
  gcells <- intersect(getCells(vopCrops), getCells(cropAreaAll))

  # Value of production per hectar
  vopPerHa <- vopCrops[gcells, gyears, gnames] / cropAreaAll[gcells, gyears, gnames]
  vopPerHa[!is.finite(vopPerHa)] <- 0

  # Fraction of each land input in overall value of production
  fractionInputs <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , c("Land")]

  fyears <- intersect(getYears(fractionInputs), getYears(vopPerHa))

  # Calculation of land intensities
  intensity <- fractionInputs[, fyears, ] * vopPerHa[, fyears, ]

  weight <- cropAreaAll[, fyears, gnames]
  weight[!is.finite(intensity)] <- 0
  weight[intensity == 0] <- 0


  units <- "constant 2017 US$MER/ hectare"


  x <- toolConvertGDP(intensity,
                      unit_in = "constant 2017 US$MER",
                      unit_out = unit,
                      replace_NAs = "no_conversion")


  weight <- weight + 1e-20

  return(list(x = x,
              weight = weight,
              mixed_aggregation = NULL,
              unit = units,
              description = "Land requirements requirements for different crops"))
}
