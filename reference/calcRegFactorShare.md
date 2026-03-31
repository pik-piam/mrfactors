# calcRegFactorShare

This function calculates the regression parameters (a and b) for the
function Share=a\*log10(GDP)+b Where share is the adjusted share between
capital and labour.

## Usage

``` r
calcRegFactorShare(datasource = "USDA", caseStudies = "CountryCaseStudies")
```

## Arguments

- datasource:

  Only USDA available

- caseStudies:

  The case studies to be used for the regression (either
  CountryCaseStudies or CaseStudiesDirectMapping). Default is
  CountryCaseStudies, as including regional case studies weakens the
  direct link to GDP per capita (and results in a regression with
  non-normally distributed residuals).

## Value

MAgPIE object at global level with slope and intersect as items

## See also

\[calcOutput()\],\[calcFactorIntensity()\]

## Author

Debbora Leip, Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("calcRegFactorShare")
} # }
```
