# MADRaT based package on factor inputs

R package **mrfactors**, version **0.1.0**

[![CRAN status](https://www.r-pkg.org/badges/version/mrfactors)](https://cran.r-project.org/package=mrfactors)  [![R build status](https://github.com/pik-piam/mrfactors/workflows/check/badge.svg)](https://github.com/pik-piam/mrfactors/actions) [![codecov](https://codecov.io/gh/pik-piam/mrfactors/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrfactors) 

## Purpose and Functionality

This package provides functions for MAgPIE input data on factor inputs to agricultural production (with a focus on capital and labor).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrfactors")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Debbora Leip <leip@pik-potsdam.de>.

## Citation

To cite package **mrfactors** in publications use:

Leip D (2023). _mrfactors: MADRaT based package on factor inputs_. R package version 0.1.0, <URL: https://github.com/pik-piam/mrfactors>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrfactors: MADRaT based package on factor inputs},
  author = {Debbora Leip},
  year = {2023},
  note = {R package version 0.1.0},
  url = {https://github.com/pik-piam/mrfactors},
}
```
