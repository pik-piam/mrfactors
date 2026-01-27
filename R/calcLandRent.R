#' @title calcLandRent
#' @description Calculates factor intensity in crop production for ladn rent from USDA (Inputs share)
#' and FAO (Value of Production) and productivity in constant 2017 US$MER per ha.
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @param rent rent type cropland average "cropland" or per crop type "perCrop"
#' @return magpie object of the land factor requirements in USD/ha per crop
#' @author Edna J. Molina Bacca
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("LandRent")
#' }
#'
calcLandRent <- function(unit = "constant 2017 US$MER", rent = "cropland") {
  
  # Cropland Area
  vopCrops <- calcOutput("VoPcrops", aggregate = FALSE, unit = "constant 2017 US$MER")

  if (rent == "perCrop") {
  cropAreaAll  <- collapseDim(calcOutput("Croparea", aggregate = FALSE)) 
  
  } else if (rent == "cropland"){
  cropAreaAll <- dimSums(calcOutput("LanduseInitialisation", nclasses = "seven", aggregate = FALSE, 
                 cellular = FALSE)[, , c("crop")], dim = 3)
  vopCrops <- dimSums(vopCrops,dim=3)
   
  }
  
  cropAreaAll[cropAreaAll < 1e-4] <- NA # remove very small values to avoid strange results when dividing 
   vopCrops[vopCrops < 1e-5] <- NA # remove very small values to avoid strange results when dividing

  if (rent == "perCrop") gnames <- intersect(getNames(vopCrops), getNames(cropAreaAll))
  gyears <- intersect(getYears(vopCrops), getYears(cropAreaAll))
  gcells <- intersect(getCells(vopCrops), getCells(cropAreaAll))

  # Value of production per hectare
  if (rent == "perCrop") vopPerHa <- vopCrops[gcells, gyears, gnames] / cropAreaAll[gcells, gyears, gnames]
  if (rent == "cropland") vopPerHa <- vopCrops[gcells, gyears, ] / cropAreaAll[gcells, gyears, ]
  vopPerHa[!is.finite(vopPerHa)] <- 0

  # Fraction of each land input in overall value of production
  fractionInputs <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , c("Land")]

  fyears <- intersect(getYears(fractionInputs), getYears(vopPerHa))

  # Calculation of land intensities
  intensity <- setNames(fractionInputs[, fyears, ], NULL) * vopPerHa[, fyears, ]
  if (rent == "perCrop") intensity[, , c("begr", "betr")] <- intensity[, , c("maiz")]

  # strange behavior in  PSE ans MUS
  intensity[c("PSE","MUS"),,] <- NA

  weight <- cropAreaAll[, fyears, ]
  weight[!is.finite(intensity)] <- 0
  weight[intensity == 0] <- 0


  units <- paste0(unit, "/hectare")


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
