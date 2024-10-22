#' @title calcAgCapLabourShare
#'
#' @description This function calculates historical capital shares (Capital + Labour)
#' of the factor requirements using USDA 
#'
#' @param fillWithRegression boolean: should missing values be filled based on a regression between capital share
#' and GDPpcPPP (calibrated to countries)
#' @param projection either FALSE or SSP on which projections should be based. Only relevant if fillWithRegression is
#' TRUE.
#' @return MAgPIE object
#' @author Edna J. Molina Bacca, Debbora Leip
#' @seealso [calcAgCapLabourShare()]
#'
#' @examples
#' \dontrun{
#' calcOutput("calcAgCapLabourShare")
#' }
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseDim collapseNames dimSums getYears
#'
calcAgCapLabourShare <- function(fillWithRegression = TRUE, projection = FALSE) {
  # raw USDA cost shares -- labor-capital ratio is the same for kcr and kli, as both are shared inputs
  # We use kcr here, but use full factor costs (crops + livst) as aggregation weight below
  shares <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE, 
                       keepConstantExtrapolation = FALSE, interpolate = TRUE)

  # Determines fraction between capital and labour
  fractionCapital <- shares[, , "Capital"] / (shares[, , "Labor", drop = TRUE] + shares[, , "Capital", drop = TRUE])
  fractionCapital[is.na(fractionCapital)] <- 0

  # fill gaps with regression
  if (isTRUE(fillWithRegression)) {

    regCoeff <- calcOutput("RegFactorShare", aggregate = FALSE)

    # calculate GDPpc [USD2017MER] for regression
    gdp <- calcOutput("GDPpc", naming = "scenario", unit = "constant 2017 Int$PPP", aggregate = FALSE)
    if (!isFALSE(projection)) {
      years <- setdiff(getItems(gdp, dim = 2), paste0("y", seq(2105, 2150, 5)))
      gdp <- gdp[, years, projection]
    } else {
      gdp <- gdp[, getItems(fractionCapital, dim = 2), "SSP2"]
    }

    # add years with GDP data to hourlyCosts object
    fractionCapital <- magpiesort(add_columns(fractionCapital, dim = 2, fill = 0,
                                              addnm = setdiff(getItems(gdp, dim = 2), 
                                                              getItems(fractionCapital, dim = 2))))
    years <- getItems(fractionCapital, dim = 2)

    .fillTimeseries <- function(var, country, regressor) {
      ctryEst <- regCoeff[, , "intercept", drop = TRUE] + regCoeff[, , "slope", drop = TRUE] * regressor[ctry, , ]
      y <- where(var[ctry, , ] != 0)$true$years
      if (length(y) == 0) {
        var[ctry, , ] <- ctryEst
      } else {
        y <- as.integer(str_split(y, "y", simplify = TRUE)[, 2])
        yPast <- years[years < min(y)]
        yFuture <- years[years > max(y)]
        yGaps <- setdiff(years[(years >= min(y)) & (years <= max(y))], y)

        if (length(yGaps) > 0) {
          var[ctry, sort(c(y, yGaps)), ] <- time_interpolate(dataset = var[ctry, y, ],
                                                             interpolated_year = yGaps,
                                                             integrate_interpolated_years = TRUE)
        }

        if (length(yPast) > 0) {
          calibPast <- var[ctry, min(y), ] - ctryEst[, min(y), ]
          var[ctry, yPast, ] <- ctryEst[, yPast, ] + calibPast
        }

        if (length(yFuture) > 0) {
          calibFuture <- var[ctry, max(y), ] - ctryEst[, max(y), ]
          var[ctry, yFuture, ] <- ctryEst[, yFuture, ] + calibFuture
        }
      }
      return(var)
    }

    logGdp <- log(gdp, base = 10)

    for (ctry in getItems(fractionCapital, dim = 1)) {
      fractionCapital <- .fillTimeseries(var = fractionCapital, country = ctry, regressor = logGdp)
    }
  }

  # factor costs as weight
  factorCostsCrops <- dimSums(calcOutput("FactorCostsCrops", aggregate = FALSE), dim = 3.1)
  factorCostsLivst <- dimSums(calcOutput("FactorCostsLivst", aggregate = FALSE), dim = 3.1)
  weight <- factorCostsCrops + factorCostsLivst

  # keep weight constant for missing years in the past and projection
  missingYears <- setdiff(getYears(fractionCapital, as.integer = TRUE), getYears(weight, as.integer = TRUE))
  weight <- time_interpolate(dataset = weight, interpolated_year = missingYears, 
                             integrate_interpolated_years = TRUE, extrapolation_type = "constant")
  weight <- weight[, getItems(fractionCapital, dim = 2), ]


  # Give 0 weigh to countries with unexpectedly high capital shares
  # countries with capital share > 0.7 "BHS" "BLZ" "CRI" "CUB" "DOM" "GTM" "GUF" "GUY" "HND" "HTI" "JAM" "MEX"
  # "NIC" "PAN" "PRI" "SLV" "SUR" "TTO" "VEN" (all from Mexico case study)
  weight[where(fractionCapital > 0.7)$true$regions, , ] <- 0

  weight[fractionCapital == 0] <- 0
  x <- setNames(fractionCapital, NULL)

  return(list(x = x,
              weight = weight,
              unit = "Fraction capital",
              description = "Share of capital of the factor requirements"))
}
