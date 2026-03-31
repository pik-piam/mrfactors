# calcFractionInputsUSDA

Calculates the factor factor shares for crop or livestock production
from USDA'S Inputs shares.

## Usage

``` r
calcFractionInputsUSDA(
  products = "kcr",
  keepConstantExtrapolation = TRUE,
  interpolate = TRUE
)
```

## Arguments

- products:

  either "kcr" for crops, or "kli" for livestock

- keepConstantExtrapolation:

  boolean: should constant extrapolation from Fuglie et al. be kept?

- interpolate:

  boolean: should the data be interpolated to the middle of the decade?

## Value

magpie object of the shares of the factor requirements in agriculture
(capital, labor, materials, land).

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca, Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("FractionInputsUSDA")
} # }
```
