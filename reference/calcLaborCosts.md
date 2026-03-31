# calcLaborCosts

calculates total labor costs in mio. US\$MER05 (coverage depends on
source: crop, livestock and fish production for USDA, and additionally
forestry for GTAP and ILO)

## Usage

``` r
calcLaborCosts(
  datasource = "ILO",
  dataVersionILO = "Aug24",
  subsectors = TRUE,
  inclFish = FALSE,
  inclForest = FALSE,
  otherLivst = TRUE,
  gtapVar = "NVFA",
  addSubsidies = FALSE
)
```

## Arguments

- datasource:

  data source on which the labor costs should be based. Available are
  ILO, USDA (which also uses data on VoP from FAO), and GTAP.

- dataVersionILO:

  If source is ILO, the version can be chosen. "" for the oldest
  version, or "monthYear" (e.g. "Aug24") for a newer version)

- subsectors:

  boolean: should output be aggregated or split into available
  subsectors (crops, livst, forestry, fishery)

- inclFish:

  boolean: should fish labor costs be included?

- inclForest:

  boolean: should forestry labor costs be included (only available for
  ILO and GTAP)?

- otherLivst:

  boolean: should other_livst category (i.e. beeswax, wool, silkworms,
  and honey) be included in livestock (only relevant for datasource
  USDA)?

- gtapVar:

  variable name to use from GTAP (only relevant if source is "GTAP")

- addSubsidies:

  boolean: should subsidy data (from IFPRI) should be added to VoP
  before applying USDA cost shares (only relevant if datasource is
  "USDA")

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LaborCosts", datasource = "ILO")
} # }
```
