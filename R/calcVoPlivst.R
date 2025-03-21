#' @title calcVoPlivst
#' @description Calculates the value of production of individual livestock categories
#'
#' @param other boolean: should FAO livestock categories that can't be matched to MAgPIE categories (i.e. beeswax, wool,
#' silkworms, and honey) be reported as "livst_other"?
#' @param fillGaps boolean: should gaps be filled using production * prices (where production data is available)?
#' @param unit output currency unit based on the toolConvertGDP function from the  GDPuc library
#' @return magpie object
#' @author Debbora Leip
#' @importFrom GDPuc toolConvertGDP
#' @importFrom magpiesets findset
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoPlivst")
#' }
#'
calcVoPlivst <- function(other = FALSE, fillGaps = TRUE, unit = "constant 2017 US$MER") {

  # Value of production of individual items (US$MER05)
  item <- "Gross_Production_Value_(USDMER17)_(1000_US$)"
  vop <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. US$MER17

  # mapping for aggregation
  mappingFAO <- toolGetMapping("FAO_VoP_kli.csv", type = "sectoral", where = "mrcommons")
  if (isFALSE(other)) {
    mappingFAO <- mappingFAO[mappingFAO$kli != "livst_other", ]
  }
  items <- mappingFAO$FAO_item

  # aggregation to magpie objects (and livst_other)
  vopLivst <- toolAggregate(vop[, , items], rel = mappingFAO, from = "post1124_FAO_Item", to = "kli", weight = NULL, dim = 3)

  # VoP in North Korea too high? -> excluded
  vopLivst["PRK", , ] <- 0

  # filling gaps based on production and prices
  if (isTRUE(fillGaps)) {
    kli <- findset("kli")
    production <- collapseDim(calcOutput("Production", products = "kli", attributes = "dm", aggregate = FALSE))
    prices <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO",
                                     aggregate = FALSE, unit = "constant 2017 US$MER"))

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO",
                                             aggregate = TRUE, regionmapping = "regionmappingH12.csv",
                                             unit = "constant 2017 US$MER"))
    pricesRegional <- toolAggregate(pricesRegional,
                                    rel = toolGetMapping("regionmappingH12.csv", where = "mappingfolder",
                                                         type = "regional"),
                                    from = "RegionCode", to = "CountryCode")
    prices[prices == 0] <- pricesRegional[prices == 0]

    # fill remaining gaps with global averages
    pricesGLO <- prices
    pricesGLO[, , ] <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO",
                                              aggregate = "GLO", unit = "constant 2017 US$MER"))
    prices[prices == 0] <- pricesGLO[prices == 0]

    prices <- prices[, , kli]

    # fill gaps in VoP (where production is available)
    years <- intersect(getYears(prices), getYears(production))
    if (isTRUE(other)) {
      vopOther <- vopLivst[, , "livst_other"]
      vopLivst <- vopLivst[, , "livst_other", invert = TRUE]
    }
    calculatedVop <- prices[, years, getNames(vopLivst)] * production[, years, getNames(vopLivst)]
    tmp <- vopLivst[, years, ]
    tmp[tmp == 0] <- calculatedVop[tmp == 0]
    vopLivst[, years, ] <- tmp
    if (isTRUE(other)) vopLivst <- mbind(vopLivst, vopOther)
  }


  if (unit != "constant 2017 US$MER") {
    vopLivst <- toolConvertGDP(vopLivst,
                               unit_in = "constant 2017 US$MER",
                               unit_out = unit,
                               replace_NAs = "no_conversion")
  }

  units <- paste0("mio ", unit)
  
  # remove years with no data
  years <- where(dimSums(vopLivst, dim = c(1, 3)) == 0)$true$years
  vopLivst <- vopLivst[, years, , invert = TRUE]
  
  return(list(x = vopLivst,
              weight = NULL,
              unit = units,
              description = " Value of production for individual livestock categories"))
}
