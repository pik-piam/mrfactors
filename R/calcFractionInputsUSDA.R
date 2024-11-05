#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for crop or livestock production from USDA'S Inputs shares.
#'
#' @param products either "kcr" for crops, or "kli" for livestock
#' @param keepConstantExtrapolation boolean: should constant extrapolation from Fuglie et al. be kept?
#' @param interpolate boolean: should the data be interpolated to the middle of the decade?
#' @return magpie object of the shares of the factor requirements in agriculture (capital, labor, materials, land).
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr  intersect
#' @importFrom magclass magpiesort
#' @importFrom magclass time_interpolate
#' @importFrom magclass collapseDim
#' @importFrom magclass collapseNames
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("FractionInputsUSDA")
#' }
#'
calcFractionInputsUSDA <- function(products = "kcr", keepConstantExtrapolation = TRUE, interpolate = TRUE) {

  # raw cost shares from USDA
  tfpShares <- readSource("TFPUSDA")

  # assuming the same share in the middle of the decade
  if (isTRUE(interpolate)) {
    years <- getYears(tfpShares, as.integer = TRUE) + 5
    years <- years[years <= max(getYears(tfpShares, as.integer = TRUE))]
    tfpShares <- magpiesort(time_interpolate(tfpShares,
                                             interpolated_year = years,
                                             extrapolation_type = "constant", integrate_interpolated_years = TRUE))
  }

  # remove constant extrapolation
  if (isFALSE(keepConstantExtrapolation)) {
    mapping <- toolGetMapping("caseStudiesUSDATFP.csv", where = "mrfactors", type = "regional")

    .cleanCaseStudy <- function(cs) {
      countries <- mapping[mapping$CaseStudiesUsed == cs, "ISO"]
      tmp <- tfpShares[countries, , ]
      tmpArray <- as.array(tmp)[1, , ]

      # special case where all years have the same values
      # (currently only SSA where estimates are over the period 1965-2008, we assign value to middle of period, 1985)
      if (all(sweep(tmpArray, MARGIN = 2, STATS = tmpArray[1, ], FUN = "=="))) {
        years <- getYears(tmp, as.integer = TRUE)
        keep  <- round((min(years) + max(years)) / 2 / 5) * 5
        tmp[countries, setdiff(years, keep), ] <- 0
        return(tmp)
      }

      removeYears <- c()

      # extrapolation to past
      test <- FALSE
      i <- 1
      while (!test) {
        if (all(tmpArray[i, ] == tmpArray[i + 1, ])) {
          removeYears <- c(removeYears, i)
          i <- i + 1
        } else {
          test <- TRUE
        }
      }

      # extrapolation to past
      test <- FALSE
      i <- nrow(tmpArray)
      while (!test) {
        if (all(tmpArray[i, ] == tmpArray[i - 1, ])) {
          removeYears <- c(removeYears, i)
          i <- i - 1
        } else {
          test <- TRUE
        }
      }

      if (length(removeYears) > 0) tmp[countries, removeYears, ] <- 0

      return(tmp)
    }

    caseStudies <- unique(mapping$CaseStudiesUsed)

    tfpShares <- magpiesort(mbind(lapply(caseStudies, .cleanCaseStudy)))

    # refill dropped countries with zeros
    tfpShares <- toolCountryFill(tfpShares, fill = 0)
  }

  # read value of production
  voPAll <- readSource("FAO_online", "ValueOfProd")
  years <- intersect(getYears(tfpShares), getYears(voPAll))
  voPAll <- voPAll[, years, ]
  tfpShares <- tfpShares[, years, ]

  # mio. USD VoP (constant 2014_2016 thousand US$ has values before 1991, current_thousand_US$ does not)
  vopCrops <- voPAll[, ,  "2041|Crops.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"] / 1000
  vopLivst <- voPAll[, , "2044|Livestock.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"] / 1000
  ratio <- collapseDim(vopCrops / (vopCrops + vopLivst))

  # fill gaps in ratio with global average
  gloRatio <- dimSums(vopCrops, dim = 1) / (dimSums(vopCrops, dim = 1) + dimSums(vopLivst, dim = 1))
  for (y in getYears(ratio)) {
    ratio[where(is.na(ratio[, y, ]))$true$regions, y, ] <- gloRatio[, y, ]
  }

  # split labor and machinery costs between crops and livestock
  sharedInput <- c("Machinery", "AG_Labour") # factors that convene livestock and crops production
  if (products == "kli") ratio <- 1 - ratio
  tfpShares[, , sharedInput] <- tfpShares[, , sharedInput] * ratio

  # mappping of categories
  if (products == "kcr") {
    mapping <- data.frame("to" = c("Labor", "Land", "Materials", "Capital"),
                          "from" = c("AG_Labour", "AG_Land", "Materials_Crops", "Machinery"))
  } else if (products == "kli") {
    mapping <- data.frame("to" = c("Labor", "Materials", "Capital", "Live Animals"),
                          "from" = c("AG_Labour", "Materials_Animals", "Machinery", "Livestock"))
  } else {
    stop("Invalid product type")
  }

  tfpShares <- toolAggregate(tfpShares[, , mapping$from], rel = mapping, from = "from", to = "to", dim = 3)
  tfpShares <- tfpShares / dimSums(tfpShares, dim = 3)
  tfpShares[!is.finite(tfpShares)] <- 0

  # vop as aggregation weight
  vop <- vopCrops + vopLivst
  weight <- tfpShares
  weight[, , ] <- vop[, getYears(tfpShares), ]
  weight[tfpShares == 0] <- 0
  weight <- weight + 1e-20

  return(list(x = tfpShares,
              weight = weight,
              mixed_aggregation = NULL,
              unit = "fraction",
              description = "Factor shares for crops from USDA TFP data"))

}
