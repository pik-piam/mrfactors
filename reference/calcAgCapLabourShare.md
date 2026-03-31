# calcAgCapLabourShare

This function calculates historical capital shares (Capital + Labour) of
the factor requirements using USDA

## Usage

``` r
calcAgCapLabourShare(fillWithRegression = TRUE, projection = FALSE)
```

## Arguments

- fillWithRegression:

  boolean: should missing values be filled based on a regression between
  capital share and GDPpcPPP (calibrated to countries)

- projection:

  either FALSE or SSP on which projections should be based. Only
  relevant if fillWithRegression is TRUE.

## Value

MAgPIE object

## See also

\[calcAgCapLabourShare()\]

## Author

Edna J. Molina Bacca, Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("calcAgCapLabourShare")
} # }
```
