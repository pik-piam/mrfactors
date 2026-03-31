# calcNonMAgPIEFactorCosts

Calculates factor costs that should affect agricultural employment but
are not included in MAgPIE factor costs

## Usage

``` r
calcNonMAgPIEFactorCosts(
  subtype = "subsidies",
  aggSubsidies = FALSE,
  extrapolate = TRUE
)
```

## Arguments

- subtype:

  either factor cost share of "subsidies" (which don't enter MAgPIE
  labor costs as they should not affect prices), or of "missingVoP"
  (which refers to livestock VoP that can't be mapped to MAgPIE
  livestock categories, i.e. wool, beeswax, honey, silk-worms)

- aggSubsidies:

  boolean: if subtype is "subsidies", should crop and livestock
  subsidies be reported separately or as aggregate?

- extrapolate:

  boolean: should values be extrapolate (by keeping constant) until
  2150?

## Value

magpie object. in mio. USD

## See also

\[calcOutput()\]

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("NonMAgPIEFactorCosts", subtype = "subsidies")
} # }
```
