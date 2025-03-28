#' @title calcPricesProducer
#' @description producer prices for agricultural products. 05USDMER/tDM prices from FAO, no currency conversion
#'
#'
#' @param products either "kcr" or "kcl"
#' @param calculation type of calculation "FAO" (directly reads the data), "VoP"
#' calculates as VoP/Production, only "FAO" available for "kli" products.
#' @param weighting either "production" (default) or "consumption" based weighting
#' @return magpie object. prices in year specific annual
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets findset
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("PricesProducer")
#' }
#'
calcPricesProducer <- function(products = "kcr", calculation = "VoP", weighting = "production") {
  # Read Prices producer with FAO format

  if (products == "kcr") {
    if (calculation == "FAO") {
      # items for aggregation

      pricesProdFAO <- readSource("FAO_online", "PricesProducerAnnual") # USD2017MER per ton wet matter

      getNames(pricesProdFAO)[getNames(pricesProdFAO) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"

      # weight: Production
      if (weighting == "production") {
        mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrlandcore")
        itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))
        weightProd <- collapseNames(readSource("FAO_online", "Crop")[, , "production"])
        getNames(weightProd)[getNames(weightProd) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"
        names <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , itemsIntersect]))
        years <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , itemsIntersect]))
        mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% names, ]

        pricesProdFAOkcr <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO,
                                          from = "ProductionItem", to = "kcr", weight = weightProd[, years, names],
                                          dim = 3, wdim = 3, zeroWeight = "allow")[, , "remaining", invert = TRUE]
      } else if (weighting == "consumption") {
        # map prices to FoodBalanceITems first
        mappingFAO <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")

        weightProd <- collapseNames(readSource("FAO_online", "Crop")[, , "production"])
        witemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))

        wnames <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , witemsIntersect]))
        wyears <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , witemsIntersect]))
        pricesProdFAO <- toolAggregate(pricesProdFAO[, wyears, wnames], rel = mappingFAO,
                                       from = "ProductionItem", to = "FoodBalanceItem",
                                       weight = weightProd[, wyears, wnames],
                                       partrel = TRUE, dim = 3, wdim = 3, zeroWeight = "allow")

        weightPrice <- collapseNames(calcOutput("FAOharmonized", aggregate = FALSE)[, , "food"])
        itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$FoodBalanceItem))
        names <- intersect(getNames(weightPrice), getNames(pricesProdFAO[, , itemsIntersect]))
        years <- intersect(getYears(weightPrice), getYears(pricesProdFAO[, , itemsIntersect]))
        mappingFAO <- mappingFAO[mappingFAO$FoodBalanceItem %in% names, ]

        pricesProdFAOkcr <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO,
                                          from = "FoodBalanceItem", to = "k", weight = weightPrice[, years, names],
                                          dim = 3, wdim = 3, zeroWeight = "allow")
      } else {
        stop("only production and consumption weights")
      }

      missing <- setdiff(findset("kcr"), getNames(pricesProdFAOkcr))

      # Fill with maiz' value the missing crops (betr,begr,foddr)
      pricesProdFAOkcr <- add_columns(pricesProdFAOkcr, addnm = missing, dim = 3.1)
      pricesProdFAOkcr[, , missing] <- pricesProdFAOkcr[, , "maiz"]

      # convert from wet matter to dry matter
      attributes <- collapseNames(calcOutput("Attributes", aggregate = FALSE)[, , "wm"])
      citems <- intersect(getItems(pricesProdFAOkcr, dim = 3), getItems(attributes, dim = 3))
      pricesProdFAOkcr <- pricesProdFAOkcr[, , citems] * attributes[, , citems]

      # output
      x <- pricesProdFAOkcr
      if (weighting == "production") {
        weight <- collapseNames(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))
      } else if (weighting == "consumption") {
        kcr <- findset("kcr")
        weight <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "dm"][, , c("food", "flour1")]
        weight  <- collapseNames(dimSums(weight, dim = 3.2))[, , kcr]
      } else {
        stop("invalid type")
      }

      # years and names subseting
      years <- intersect(getYears(weight), getYears(x))
      names <- intersect(getNames(weight), getNames(x))

      weight <- weight[, years, names]
      x <- x[, years, names]

    } else if (calculation == "VoP") {
      # Value of production (USD17MER)
      vop <- calcOutput("VoPcrops", aggregate = FALSE)

      production <- collapseNames(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

      years <- intersect(getYears(production), getYears(vop))
      names <- intersect(getNames(production), getNames(vop))
      prices <- vop[, years, names] / production[, years, names]

      # categories with all NaN (begr and betr, which have no VoP and no production data)
      missing <- setdiff(findset("kcr"), where(is.finite(prices))$true$data)
      prices[, , missing] <- prices[, , "maiz"] # fill with maiz values

      # set rest of missing values to zero
      prices[!is.finite(prices)] <- 0

      weight <- production[, years, getNames(prices)]
      weight[prices == 0] <- 0
      x <- prices[, years, ]

    } else {
      stop("Type not valid")
    }

  } else if (products == "kli") {

    if (calculation == "FAO") {

      pricesProdFAO <- readSource("FAO_online", "PricesProducerAnnual") # USD2017 per ton


      # get mapping
      mappingFAO <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder") # Reads mapping

      if (weighting == "production") {
        itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))
        # Prod. of livestock primary prod
        weightProd <- collapseNames(readSource("FAO_online", "LivePrim")[, , "production"])
        # subseting of items
        names <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , itemsIntersect]))
        years <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , itemsIntersect]))
        mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% names, ]

        # Aggregation to magpie objects
        pricesProdFAOkli <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO, from = "ProductionItem",
                                          to = "k", weight = weightProd[, years, names], dim = 3, wdim = 3,
                                          zeroWeight = "allow")

      } else if (weighting == "consumption") {
        # map prices to FoodBalanceITems first
        mappingFAO <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")

        weightProd <- collapseNames(readSource("FAO_online", "LivePrim")[, , "production"])
        witemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))

        wnames <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , witemsIntersect]))
        wyears <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , witemsIntersect]))

        pricesProdFAO <- toolAggregate(pricesProdFAO[, wyears, wnames], rel = mappingFAO,
                                       from = "ProductionItem", to = "FoodBalanceItem",
                                       weight = weightProd[, wyears, wnames],
                                       partrel = TRUE, dim = 3, wdim = 3, zeroWeight = "allow")

        weightPrice <- collapseNames(calcOutput("FAOharmonized", aggregate = FALSE)[, , "food"])
        itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$FoodBalanceItem))
        names <- intersect(getNames(weightPrice), getNames(pricesProdFAO[, , itemsIntersect]))
        years <- intersect(getYears(weightPrice), getYears(pricesProdFAO[, , itemsIntersect]))
        mappingFAO <- mappingFAO[mappingFAO$FoodBalanceItem %in% names, ]

        pricesProdFAOkli <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO,
                                          from = "FoodBalanceItem", to = "k", weight = weightPrice[, years, names],
                                          dim = 3, wdim = 3, zeroWeight = "allow")
      } else {
        stop("only production and consumption weights")
      }

      # convert from wet matter to dry matter
      attributes <- collapseNames(calcOutput("Attributes", aggregate = FALSE)[, , "wm"])
      citems <- intersect(getItems(pricesProdFAOkli, dim = 3), getItems(attributes, dim = 3))
      pricesProdFAOkli <- pricesProdFAOkli[, , citems] * attributes[, , citems]

      x <- pricesProdFAOkli
      # weight setup
      if (weighting == "production") {
        weight <- collapseNames(calcOutput("Production", products = "kli", aggregate = FALSE, attributes = "dm"))
      } else if (weighting == "consumption") {
        kli <- findset("kli")
        weight  <- collapseNames(dimSums(calcOutput("FAOmassbalance",
                                                    aggregate = FALSE)[, , "dm"][, , c("food", "flour1")],
                                         dim = 3.2))[, , kli]
      } else {
        stop("invalid type")
      }

      years <- intersect(getYears(weight), getYears(x))
      names <- intersect(getNames(weight), getNames(x))

      weight <- weight[, years, names]
      x <- x[, years, names]

    } else {
      stop("Type not valid")
    }
  }

  units <- "2017USDMER/tDM"


  return(list(x = x,
              weight = weight,
              mixed_aggregation = NULL,
              unit = units,
              description = "Producer prices in 2017USDMER/tDM"))
}
