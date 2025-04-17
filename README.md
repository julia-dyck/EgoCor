
# {EgoCor}: Simple Presentation of Estimated Exponential Semi-Variograms

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/EgoCor)](https://CRAN.R-project.org/package=EgoCor)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/last-month/EgoCor)](https://cran.r-project.org/package=EgoCor)
[![R-CMD-check](https://github.com/julia-dyck/EgoCor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/julia-dyck/EgoCor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Description

User friendly interface based on the R package ‘gstat’ to fit
exponential parametric models to empirical semi-variograms in order to
model the spatial correlation structure of health data. Geo-located
health outcomes of survey participants may be used to model spatial
effects on health in an ego-centred approach. The package contains a
range of functions to help explore the spatial structure of the data as
well as visualize the fit of exponential models for various
metaparameter combinations with respect to the number of lag intervals
and maximal distance. Furthermore, the outcome of interest can be
adjusted for covariates by fitting a linear regression in a preliminary
step before the semi-variogram fitting process.

### See also 

Dyck, J., Koslik, J.-O., &amp; Sauzet, O. (2025). EgoCor: An R Package to Facilitate the Use of Exponential Semi-Variograms for Modelling the Local Spatial Correlation Structure in Social Epidemiology. <i>Journal of Open Research Software</i>, <i>13</i>(1), 6. https://doi.org/10.5334/jors.517

for information on the purpose and usage of EgoCor.

## Installation

You can install the EgoCor package from
[CRAN](https://cran.r-project.org/web/packages/EgoCor/index.html) with:

``` r
install.packages("EgoCor")
```

and the development version from [GitHub](https://github.com) with:

``` r
# install.packages("devtools") # if not installed, yet
# remove.packages("EgoCor")    # if an old version of the package is installed
devtools::install_github(repo = "julia-dyck/EgoCor", build_vignettes = TRUE)
```

## Documentation

Take a look at the vignette “Introduction to EgoCor” with:

``` r
browseVignettes("EgoCor")
```
