#' Spatially correlated  birthweight data with artificial coordinates
#'
#' A simulated dataset containing geo-coded birthweight data with covariates.
#' It is provided for exemplary applications of the functions
#' from the R package EgoCor.
#'
#'
#' @format  A data frame with 903 rows and 8 variables:
#' \describe{
#'   \item{x}{x-coordinate given in Cartesian format in meters,}
#'   \item{y}{y-coordinate given in Cartesian format in meters,}
#'   \item{birthweight}{weight of the child in gram,}
#'   \item{primiparous}{binary: 1 = first child birth, 0 = not the first child birth,}
#'   \item{datediff}{difference between due date and birth,}
#'   \item{bmi}{BMI (body mass index) of the mother at the beginning of pregnancy,}
#'   \item{weight}{weight of the mother in kg,}
#'   \item{inc}{income quintiles 0 (low),1,2,3,4 (high).}
#'}
#'
#' @details The dataset is loosely based on the BaBi study dataset
#'  referred to in \insertCite{BaBi;textual}{EgoCor}. The spatial correlation structure
#'  was already modeled using an exponential model in \insertCite{sauzet2021;textual}{EgoCor}.
#'
#'
#' @references
#' \insertAllCited{}
#'
"birth"
