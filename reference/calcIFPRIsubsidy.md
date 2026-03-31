# calcIFPRIsubsidy

Adds non-allocated subsidies to crop subsidies (as most subsidies are
linked to land area), and excludes NRP subsidies (as those are border
measures, which are already reflected in ag. prices)

## Usage

``` r
calcIFPRIsubsidy(fillGaps = TRUE)
```

## Arguments

- fillGaps:

  boolean, should gaps in the dataset be filled using interpolation?

## Value

magpie object. in mio. USD

## See also

\[calcOutput()\]

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("IFPRIsubsidy")
} # }
```
