[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# EgoCor: Simple Presentation of Estimated Exponential Semi-Variograms

## Description

User friendly interface based on the R package 'gstat' to fit
    exponential parametric models to empirical semi-variograms in order to
    model the spatial correlation structure of health data. Geo-located
    health outcomes of survey participants may be used to model spatial
    effects on health in an ego-centred approach.  The package contains a
    range of functions to help explore the spatial structure of the data
    as well as visualize the fit of exponential models for various
    metaparameter combinations with respect to the number of lag intervals
    and maximal distance.  Furthermore, the outcome of interest can be
    adjusted for covariates by fitting a linear regression in a
    preliminary step before the semi-variogram fitting process.


## Installation

You can install the development version of the EgoCor package from [GitHub](https://github.com/) with:

    # install.packages("devtools") # if not installed, yet
    # remove.packages("EgoCor")    # if an old version of the package is installed
    devtools::install_github(repo = "julia-dyck/EgoCor", build_vignettes = TRUE)
    library(EgoCor)
   
## Documentation

Take a look at the vignette with:

    browseVignettes("EgoCor")
