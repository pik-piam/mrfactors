#' @title calcRegFactorShare
#'
#' @description This function calculates the regression parameters (a and b) for the function Share=a*log10(GDP)+b
#' Where share is the adjusted share between capital and labour.
#'
#'
#' @return MAgPIE object at global level with slope and intersect as items
#' @author Debbora Leip, Edna J. Molina Bacca
#' @seealso [calcOutput()],[calcFactorIntensity()]
#' @param datasource Only USDA available
#' @param caseStudies The case studies to be used for the regression (either CountryCaseStudies or
#' CaseStudiesDirectMapping). Default is CountryCaseStudies, as including regional case studies weakens the direct
#' link to GDP per capita (and results in a regression with non-normally distributed residuals).
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseDim dimSums getCells getYears
#' @importFrom stats lm
#'
#' @examples
#' \dontrun{
#' calcOutput("calcRegFactorShare")
#' }
#'
calcRegFactorShare <- function(datasource = "USDA", caseStudies = "CountryCaseStudies") {

  if (datasource == "USDA") {
    # raw USDA cost shares -- labor-capital ratio is the same for kcr and kli, as both are shared inputs
    # We use kcr here, but use full factor costs (crops + livst) as aggregation weight and regression weight
    shares <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE, keepConstantExtrapolation = FALSE)
    capShare <- shares[, , "Capital"] / (shares[, , "Labor", drop = TRUE] + shares[, , "Capital", drop = TRUE])
    capShare[is.na(capShare)] <- 0

    # factor costs as weight (keeping constant for missing years in the past)
    facCosts  <- setNames(dimSums(calcOutput("FactorCostsCrops", aggregate = FALSE), dim = 3) +
                            dimSums(calcOutput("FactorCostsLivst", aggregate = FALSE), dim = 3), "FactorCosts")
    facCosts  <- time_interpolate(facCosts, interpolated_year = seq(1960, 1990),
                                  extrapolation_type = "constant", integrate_interpolated_years = TRUE)

    # dependent variable
    gdp <- calcOutput("GDPpc", scenario = "SSP2", unit = "constant 2017 Int$PPP", aggregate = FALSE)
    gdp <- setNames(gdp, "GDP_pc")

    # mapping to case studies
    mapping <- madrat::toolGetMapping("caseStudiesUSDATFP.csv", where = "mrfactors", type = "regional")
    mapping <- mapping[, c("ISO", caseStudies)]
    mapping <- mapping[!is.na(mapping[, caseStudies]), ]
    countries <- unique(mapping$ISO)

    # aggregate shares using factor costs as weight
    capShare <- capShare[countries, , ]
    weight  <- facCosts[countries, getYears(capShare), ]
    weight[capShare == 0] <- 0
    weight <- weight + 1e-12
    capShare <- toolAggregate(capShare, rel = mapping, weight = weight,
                              from = "ISO", to = caseStudies, dim = 1)

    # aggregate GDP per capita using population as weight
    pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)
    weight2 <- pop[countries, getYears(capShare), ]
    weight2[weight == 1e-12] <- 1e-12
    gdp <- toolAggregate(gdp[countries, getYears(capShare), ], rel = mapping,
                         weight = weight2, from = "ISO", to = caseStudies, dim = 1)

    # aggregate factor costs as regression weight
    weightRegr <- toolAggregate(facCosts[countries, getYears(capShare), ], rel = mapping,
                                weight = NULL, from = "ISO", to = caseStudies, dim = 1)

    # combine data
    data <- mbind(capShare, gdp, weightRegr)
    data <- as.data.frame(data)[, 2:5]
    data <- reshape(data, idvar = c("Region", "Year"), timevar = "Data1", direction = "wide")
    colnames(data) <- c("Region", "Year", "CapitalShare", "GDP_pc", "FactorCosts")
    data <- data[data$CapitalShare != 0, ]


    # regression
    fit <- lm(CapitalShare ~ log(GDP_pc, base = 10), data = data, weights = data$FactorCosts)

    # create magclass object
    res <- new.magpie(names = c("slope", "intercept"), sets = c("Region", "Year", "coefficients"))
    res[, , "slope"] <- as.numeric(fit[["coefficients"]][2])
    res[, , "intercept"] <- as.numeric(fit[["coefficients"]][1])
  } else {
    stop("Data source not available")
  }


  return(list(x = res,
              weight = NULL,
              unit = "Share",
              description = paste("Regression parameters for capital share out of factor requirements (capital+labor) ",
                                  "based on log10(GDPpc)")))
}
