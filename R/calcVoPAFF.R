#' @title calcVoPAFF
#' @description Calculates the overall value of production of the agriculture,
#' forestry and fisheries sectors. Forestry and Fisheries are calculated from exports values.
#'
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return magpie object. in mio. USD
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr intersect
#' @importFrom magclass dimSums
#' @importFrom GDPuc toolConvertGDP
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoPAFF")
#' }
#'
calcVoPAFF <- function(unit = "constant 2017 US$MER") {

  #### Value of production for Agriculture (crops and livestock)
  vopCrops <- calcOutput("VoPcrops", aggregate = FALSE, unit = unit)
  vopLivst <- calcOutput("VoPlivst", other = TRUE, aggregate = FALSE, unit = unit)

  vopAg <- setNames(dimSums(vopCrops, dim = 3) + dimSums(vopLivst, dim = 3), "Agriculture")

  #### Value of production fisheries

  # export value and quantity of fish and other aquatic products
  exportFishValue <- readSource("FishstatJ_FAO", subtype = "exportsValue") # 1000 current USD MER
  exportFishTonNet <- readSource("FishstatJ_FAO", subtype = "exportsQuantity") # ton_net

  fishCat <- c("2961|Aquatic Products, Other + (Total).production",
               "2960|Fish, Seafood + (Total).production")

  prodFishTonNet <- dimSums(readSource("FAO", "CBLive")[, , fishCat], dim = 3.1) # ton net


  # common years
  yearsFish <- intersect(intersect(getYears(exportFishValue),
                                   getYears((exportFishTonNet))), getYears(prodFishTonNet))
  cellsFish <- intersect(intersect(getCells(exportFishValue),
                                   getCells((exportFishTonNet))), getCells(prodFishTonNet))


  # Value of production for fish and aquatic products -> Production*export_price
  vopFish <- exportFishValue[cellsFish, yearsFish, ] / exportFishTonNet[cellsFish, yearsFish, ] *
    prodFishTonNet[cellsFish, yearsFish, ] / 1000  # mio. current USD
  vopFish <- toolConvertGDP(vopFish,
                            unit_in = "current US$MER",
                            unit_out = unit,
                            replace_NAs = "no_conversion")

  vopFish[!is.finite(vopFish)] <- 0
  getNames(vopFish) <- "Fisheries"


  #### Value of production forestry

  forestCat <- c("Roundwood.Export_Value_(Mio_US$)",
                 "Roundwood.Export_Quantity_(m3)",
                 "Roundwood.Production_(m3)")

  vopForestryData <- readSource("FAO", "ForestProdTrade")[, , forestCat]

  priceForestry <- vopForestryData[, , "Roundwood.Export_Value_(Mio_US$)"] /
    vopForestryData[, , "Roundwood.Export_Quantity_(m3)"]

  # Base year change for exports value
  priceForestry <- toolConvertGDP(priceForestry,
                                  unit_in = "current US$MER",
                                  unit_out = unit,
                                  replace_NAs = "no_conversion")

  priceForestry[!is.finite(priceForestry)] <- 0


  years <- intersect(getYears(priceForestry), getYears(vopForestryData))

  vopForestry <- toolCountryFill(x = vopForestryData[, years, "Roundwood.Production_(m3)"] *
                                   priceForestry[, years, ], fill = 0)
  getNames(vopForestry) <- "Forestry"

  ################

  # magpie object to return
  yearsVoP <- intersect(intersect(getYears(vopAg), getYears((vopFish))), getYears(vopForestry))
  cellsVoP <- intersect(intersect(getCells(vopAg), getCells((vopFish))), getCells(vopForestry))

  x <- mbind(vopAg[cellsVoP, yearsVoP, ], vopFish[cellsVoP, yearsVoP, ],
             vopForestry[cellsVoP, yearsVoP, ])
  x[!is.finite(x)] <- 0

  units <- paste0("mio ", unit)

  return(list(x = x,
              weight = NULL,
              mixed_aggregation = NULL,
              unit = units,
              description = " Value of production for the agriculture, forestry and fisheries sector"))
}
