#' @title calcVoPcrops
#' @description Calculates the value of production of individual production items or
#' its fraction compared to overall Value of Production (Agriculture, Fish, Forestry).
#'
#' @param fillGaps boolean: should gaps be filled using production * prices (where production data is available)?
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return magpie object. in mio. USD or fraction
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr intersect
#' @importFrom GDPuc toolConvertGDP
#' @importFrom magpiesets findset
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoPcrops")
#' }
#'
calcVoPcrops <- function(fillGaps = TRUE, unit = "constant 2017 US$MER") {
  # Value of production of individual items (US$MER17)
  item <- "Gross_Production_Value_(USDMER17)_(1000_US$)"
  vopAll <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. US$MER17

  getNames(vopAll) <- gsub("\\..*", "", getNames(vopAll))
  getNames(vopAll)[getNames(vopAll) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"

  # items for aggregation
  mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrlandcore")
  itemsIntersect <- intersect(getNames(vopAll), unique(mappingFAO$ProductionItem))
  mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% itemsIntersect, ]

  # Aggregation to magpie objects
  vopKcrAggregated <- toolAggregate(vopAll[, , itemsIntersect], rel = mappingFAO, from = "ProductionItem",
                                    to = "kcr", weight = NULL, dim = 3)

  # VoP in North Korea too high? -> excluded
  vopKcrAggregated["PRK", , ] <- 0

  # filling gaps based on production and prices (only works for 1991-2013, other years stay the same)
  if (isTRUE(fillGaps)) {
    kcr <- findset("kcr")
    vopKcrAggregated <- add_columns(vopKcrAggregated, setdiff(kcr, getNames(vopKcrAggregated)), dim = 3, fill = 0)

    production <- collapseDim(calcOutput("Production", products = "kcr", attributes = "dm", aggregate = FALSE))
    prices <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = FALSE,
                                     unit = "constant 2017 US$MER"))

    kPrices <- intersect(kcr, getNames(prices))
    prices <- prices[, , kPrices]

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO",
                                             aggregate = TRUE, regionmapping = "regionmappingH12.csv",
                                             unit = "constant 2017 US$MER"))
    pricesRegional <- toolAggregate(pricesRegional,
                                    rel = toolGetMapping("regionmappingH12.csv", where = "mappingfolder",
                                                         type = "regional"),
                                    from = "RegionCode", to = "CountryCode")[, , kPrices]
    prices[prices == 0] <- pricesRegional[prices == 0]

    # fill remaining gaps with global averages
    pricesGLO <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = "GLO",
                                        unit = "constant 2017 US$MER"))[, , kPrices]
    pricesGLOfilledISO <- prices
    pricesGLOfilledISO[, , ] <- pricesGLO
    prices[prices == 0] <- pricesGLO[prices == 0]

    # add missing prices from calcIniFoodPrice for 2005 adjusted by applying average price development over time
    missingCommodities <- setdiff(kcr, getNames(prices))
    prices <- add_columns(prices, addnm = missingCommodities, dim = 3.1, fill = 0)[, , kcr]

    pricesGLOnormalized <- pricesGLO / setYears(pricesGLO[, 2005, ], NULL)
    weight <- dimSums(production, dim = 1)[, getYears(pricesGLOnormalized), getNames(pricesGLOnormalized)]
    averagePriceDevelopment <- dimSums(pricesGLOnormalized * (weight / dimSums(weight, dim = 3)), dim = 3)
    iniPrice <- calcOutput("IniFoodPrice", products = missingCommodities, aggregate = FALSE)
    iniPrice <- iniPrice * averagePriceDevelopment

    prices[, , missingCommodities] <- iniPrice[, , missingCommodities]

    # fill gaps in VoP (where production is available)
    years <- intersect(getYears(prices), getYears(production))
    calculatedVoP <- prices[, years, getNames(vopKcrAggregated)] * production[, years, getNames(vopKcrAggregated)]
    tmp <- vopKcrAggregated[, years, ]
    tmp[tmp == 0] <- calculatedVoP[tmp == 0]
    vopKcrAggregated[, years, ] <- tmp
  }

  vopKcrAggregated[!is.finite(vopKcrAggregated)] <- 0

  # we exclude all year-country combinations for which less than 5 crop categories are reported
  repValues <- dimSums(vopKcrAggregated > 0, dim = 3)
  l5 <- repValues < 5
  vopKcrAggregated[l5] <- 0


  weight <- NULL

  if (unit != "constant 2017 US$MER") {
    vopKcrAggregated <- toolConvertGDP(vopKcrAggregated,
                                       unit_in = "constant 2017 US$MER",
                                       unit_out = unit,
                                       replace_NAs = "no_conversion")
  }


  units <- paste0("mio ", unit)

  # remove years with no data
  years <- where(dimSums(vopKcrAggregated, dim = c(1, 3)) == 0)$true$years
  vopKcrAggregated <- vopKcrAggregated[, years, , invert = TRUE]

  return(list(x = vopKcrAggregated,
              weight = weight,
              mixed_aggregation = NULL,
              unit = units,
              description = " Value of production for individual crops"))
}
