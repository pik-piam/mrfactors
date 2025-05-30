#' @title calcAgEmplILO
#' @description calculates complete dataset of number of people employed in agriculture, forestry and fishery based
#' on ILO modelles dataset and GDPpcPPP05 for regression
#' @param subsectors boolean: should overall values be split into the sub-sectors agriculture, forestry and fishery
#' based on their relative share of people employed, and agriculture further split into crops and livestock based
#' on VoP
#' @param inclFish boolean: should employment in fisheries be included?
#' @param inclForest boolean: should emplyoment in forestry be included?
#' @param dataVersionILO which version of the ILO input data to use. "" for the oldest version and
#' old regression, or "monthYear" (e.g. "Aug24") for newer data
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @importFrom stringr str_split
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("AgEmplILO")
#' }

calcAgEmplILO <- function(subsectors = TRUE, inclFish = FALSE, inclForest = FALSE, dataVersionILO = "Aug24") {

  # read original employment dataset from ILO (convert from thous. to mil.)
  dataType <- ifelse(dataVersionILO == "", "EmplByActivityModelled",
                     paste("EmplByActivityModelled", dataVersionILO, sep = "_"))
  iloEmpl <- readSource("ILOSTAT", dataType)[, , list("Total", "Aggregate: Agriculture"), drop = TRUE]
  iloEmpl[iloEmpl == 0] <- NA

  # from thousands to millions
  iloEmpl <- iloEmpl / 1000

  # calculate estimates of people employed in agriculture for missing countries
  # regression: sqrt(share s of total population that is employed in agriculture) ~ log10(GDP PPP per capita)
  # above a certain value of GDPpcPPP, the share of people employed in agriculture is kept constant
  subtype <- ifelse(dataVersionILO == "", "AgEmplShare", paste("AgEmplShare", dataVersionILO, sep = "_"))
  regCoeff <- readSource("RegressionsILO", subtype)

  gdpPPPpc <- calcOutput("GDPpcPast", aggregate = FALSE) # in constant 2017 Int$PPP

  estShare <- (regCoeff[, , "slope", drop = TRUE] * log10(gdpPPPpc) + regCoeff[, , "intercept", drop = TRUE]) ** 2
  const <- (regCoeff[, , "slope"] * log10(regCoeff[, , "threshold"]) + regCoeff[, , "intercept"]) ** 2
  estShare[gdpPPPpc > regCoeff[, , "threshold"]] <- const

  pop <- calcOutput("PopulationPast", aggregate = FALSE) # million people
  years <- intersect(getItems(estShare, dim = 2), getItems(pop, dim = 2))
  estEmpl <- estShare[, years, ] * pop[, years, , drop = TRUE]

  # reduce years to those with estimations, fill missing countries
  regions <- where(!is.finite(iloEmpl))$true$regions
  years <- intersect(getYears(iloEmpl), years)
  iloEmpl <- iloEmpl[, years, ]
  iloEmpl[regions, , ] <- estEmpl[regions, years, ]

  # function to fill gaps in datasets used for disaggregation, and calculate shares out of absolute values
  .calcShares <- function(data) {

    # interpolate for each country
    .interpolateCountry <- function(reg, x) {
      tmp <- x[reg, , ]
      completeYears <- gsub("y", "", where(is.finite(dimSums(tmp, dim = 3)))$true$years)
      if (length(completeYears) > 0) {
        years <- paste0("y", min(completeYears):max(completeYears))
        for (nm in getNames(tmp)) {
          obsYears <- intersect(years, where(is.finite(dimSums(tmp, dim = 3)))$true$years)
          tmp[, years, nm] <- time_interpolate(tmp[, obsYears, nm],
                                               interpolated_year = years,
                                               integrate_interpolated_years = TRUE,
                                               extrapolation_type = "constant")
        }
      }
      return(tmp)
    }
    data <- mbind(lapply(getItems(data, dim = 1), .interpolateCountry, data))

    # set cases to 0 for which not all of the sectors report values
    # (so we don't have a bias between extrapolated and given values)
    incomplete <- dimSums(data)
    incomplete[is.finite(incomplete)] <- 1
    data <- data * incomplete

    # extrapolate data (constant to have constant extrapolation of shares)
    .extrapolateCountry <- function(reg, x) {
      tmp <- x[reg, , ]
      if (any(is.finite(tmp))) {
        tmp <- time_interpolate(tmp[, where(is.finite(tmp))$true$years, ],
                                interpolated_year = getYears(tmp),
                                integrate_interpolated_years = TRUE,
                                extrapolation_type = "constant")
      }
      return(tmp)
    }
    data <- mbind(lapply(getItems(data, dim = 1), .extrapolateCountry, data))

    # calculate shares
    shares <- data / dimSums(data, dim = 3)
    shares[!is.finite(shares)] <- 0

    # world average shares (weighted with total of values) for missing countries
    weights <- dimSums(data, dim = 3)
    weights[!is.finite(weights)] <- 0
    weights <- weights / dimSums(weights, dim = 1)
    avg <- dimSums(shares * weights, dim = 1)

    # fill shares of missing countries with world average
    missingRegions <- where(shares == 0)$true$regions
    shares[missingRegions, , ] <- avg

    return(shares)
  }

  # disaggregate total ag. empl (crop, livst, forestry, fishery) into sectors based on more detailed employment data
  # for agriculture (crop+livst), forestry and fishery; and further disaggregating crops and livestock based on VoP:

  # shares between agriculture, forestry, fishery based on ag. empl.
  dataType <- ifelse(dataVersionILO == "", "EmplByISIC2",
                     paste("EmplByISIC2", dataVersionILO, sep = "_"))
  agEmplISIC2 <- readSource("ILOSTAT", dataType)[, , "Total", drop = TRUE]

  agEmpltotal <- new.magpie(cells_and_regions = getItems(agEmplISIC2, dim = 1),
                            years = getItems(agEmplISIC2, dim = 2),
                            names = c("Agriculture", "Forestry", "Fisheries"))

  namesAgriculture <- c("ISIC_Rev31: 01 - Agriculture, hunting and related service activities",
                        "ISIC_Rev4: 01 - Crop and animal production, hunting and related service activities")
  namesForestry    <- c("ISIC_Rev31: 02 - Forestry, logging and related service activities",
                        "ISIC_Rev4: 02 - Forestry and logging")
  namesFisheries   <- c("ISIC_Rev31: 05 - Fishing, aquaculture and service activities incidental to fishing",
                        "ISIC_Rev4: 03 - Fishing and aquaculture")

  agEmpltotal[, , "Agriculture"] <- dimSums(agEmplISIC2[, , namesAgriculture], dim = 3)
  agEmpltotal[, , "Forestry"] <- dimSums(agEmplISIC2[, , namesForestry], dim = 3)
  agEmpltotal[, , "Fisheries"] <- dimSums(agEmplISIC2[, , namesFisheries], dim = 3)

  agEmpltotal[agEmpltotal == 0] <- NA
  agEmpltotal["USA", , ] <- NA # remove ag. empl. for USA as ratio between agriculture and fishery seems unrealistic

  sharesAgEmpl <- .calcShares(agEmpltotal)

  # shares between crop and livestock based on VoP (in mio. current USDMER, as currency is irrelevant for shares between
  # sectors)
  ag <- list(c("2041|Crops", "2044|Livestock"), "Gross_Production_Value_(current_thousand_US$)_(1000_US$)")
  vop <- readSource("FAO_online", "ValueOfProd")[, , ag, drop = TRUE] / 1000 # mio current USDMER
  getNames(vop) <- str_split(getNames(vop), "\\|", simplify = TRUE)[, 2]
  vop[vop == 0] <- NA
  sharesVoP <- .calcShares(vop)

  # combine shares
  years <- intersect(getItems(sharesVoP, dim = 2), getItems(sharesAgEmpl, dim = 2))
  sharesVoP <- sharesVoP[, years, ] * sharesAgEmpl[, years, "Agriculture", drop = TRUE]

  shares <- mbind(sharesVoP, sharesAgEmpl[, years, c("Forestry", "Fisheries")])

  # apply shares to total labor costs
  years <- intersect(getItems(shares, dim = 2), getItems(iloEmpl, dim = 2))
  iloEmpl <- iloEmpl[, years, , drop = TRUE] * shares[, years, ]

  # remove subsectors that should not be included
  if (isFALSE(inclFish)) iloEmpl <- iloEmpl[, , "Fisheries", invert = TRUE]
  if (isFALSE(inclForest)) iloEmpl <- iloEmpl[, , "Forestry", invert = TRUE]

  # aggregate subsectors
  if (isFALSE(subsectors)) iloEmpl <- setNames(dimSums(iloEmpl, dim = 3), NULL)

  return(list(x = iloEmpl,
              weight = NULL,
              unit = "mio. people",
              description = "Employment in agriculture, forestry and fishery (based on ILO modelled estimates)"))
}
