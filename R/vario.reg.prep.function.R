#' Adjustment for covariates before semi-variogram model fitting
#'
#' Adjustment for covariates provides the option to eliminate non-spatial effects
#' on the variable of interest.
#' Given a linear regression output of class 'lm' or 'lmerMod'
#' with the attribute of interest as dependent variable,
#' the function provides a dataset containing the coordinates of the original observations
#' and the studentized residuals of the regression model.
#'
#'
#'
#' @param reg An object of class 'lm' obtained as result of a linear regression using the function
#'            \code{lm} from the package \code{stats} or an object of class 'lmerMod' obtained as
#'            result of a linear mixed model regression using the function
#'            \code{lmer} from the package \code{lme4}.
#' @param data Only needed if the data argument within the regression function \code{lm}/\code{lmer} is not provided:
#'             A data frame containing the geo-coded dataset containing
#'             the Cartesian x- and y-coordinates in the first and second column,
#'             the outcome of interest in the third column and
#'             all covariates used for the regression in further columns.
#'
#'
#' @return A data frame with three columns:
#' \item{x}{x-coordinate in the first column.}
#' \item{y}{y-coordinate in the second column.}
#' \item{adj}{Studentized residuals to be used as new variable adjusted for covariates.}
#'
#'
#' @seealso  \code{lm} in the \code{stats} package for information on the fitting of a linear regression model;
#'
#'           \code{lmer} in the \code{lme4} package for information on the fitting of a linear mixed regression model;
#'
#'           \code{rstudent} in the \code{stats} package for information on how the attribute of interest is
#'           adjusted for covariates.
#'
#' @details The adjusted outcome is defined as the student residuals
#' of the linear or linear mixed regression model. They are calculated using the \code{rstudent} function
#' from package \code{stats}.
#' In case of a mixed model, the adjusted variable vector resembles the conditional studentized residuals.
#'
#' The geo-coded dataset used for the regression is extracted from the current environment. In order to work,
#' the dataset has to be loaded into the environment prior to the use of \code{vario.reg.prep}.
#'
#' If the data argument was specified within the regression function \code{lm/lmer},
#' \code{vario.reg.prep} automatically extracts the name of the dataset used for regression and calls
#' it from the current environment.
#' Otherwise, the dataset has to be provided manually as input argument within \code{vario.reg.prep}.
#'
#'
#'
#' @examples ## Example 1
#' head(birth) #geo-coded dataset
#' hist(birth$birthweight) # attribute of interest
#'
#' # Linear regression model
#' mod1 = lm(birthweight ~ primiparous + datediff + bmi
#' + inc, data = birth)
#' summary(mod1)
#' data.adj1 = vario.reg.prep(mod1)
#'
#' head(data.adj1)
#' hist(data.adj1$adj) # adjusted attribute of interest
#' # The data frame can be used as input for the vario.mod function.
#'
#'
#' ## Example 2
#' # No data argument provided within lm (not recommended, but possible):
#' mod2 = lm(birth$birthweight ~ birth$primiparous + birth$datediff + birth$bmi
#' + birth$inc)
#' summary(mod2)
#' # In this case, make sure to provide the data argument here:
#' data.adj2 = vario.reg.prep(reg = mod2, data = birth)
#'
#'
#' if (requireNamespace("lme4", quietly = TRUE)) {
#' ## Example 3
#' # Linear mixed regression model
#' mod3 = lme4::lmer(birthweight ~ primiparous + datediff
#'                 + bmi + (1|inc), data = birth)
#' summary(mod3)
#' data.adj3 = vario.reg.prep(mod3)
#'
#'
#' ## Example 4
#' # Data argument within lmer not provided (not recommended, but possible):
#' mod4 = lme4::lmer(birth$birthweight ~ birth$primiparous + birth$datediff
#'             + birth$bmi + (1|birth$inc))
#' summary(mod4)
#' # In this case, make sure to provide the data argument here:
#' data.adj4 = vario.reg.prep(reg = mod4, data = birth)
#' }
#'
#' @export


vario.reg.prep = function(reg, data = NULL){
  #### necessary packages
  #stats

  if((!(inherits(reg, "lm"))) & (!(inherits(reg,"lmerMod")))){stop('Argument reg has to be an object of class lm or lmerMod.')}

  if(missing(data)){
    if(inherits(reg, "lm")){
      data = try(eval(reg$call$data),silent=T)
      data.name.ch = as.character(reg$call$data)}
    else if(inherits(reg,"lmerMod")){
      data = try(eval(reg@call$data),silent=T)
      data.name.ch = as.character(reg@call$data)
      }

  #if(is.null(data)){stop("The geo-coded dataset used for regression has to be provided as data argument either within the lm or lmer function (recommended) or directly within the vario.reg.prep fct.")}

  if((is.null(data)) || (!exists(data.name.ch))){stop("The geo-coded dataset used for regression cannot be recalled from the current environment.\n Please make sure to:\n    - load the geo-coded dataset into the environment before using vario.reg.prep,\n    - provide the data argument within the regression function lm/lmer(recommended)\n      or within the vario.reg.prep function.")}
  }
  message(paste('Message:\n',
                'Are the columns in the dataset used for regression in correct order?\n '
                ,paste('Data interpretation:',
                   '    column 1: Cartesian x-coordinates in meters',
                   '    column 2: Cartesian y-coordinates in meters',
                   '    column 3: variable of interest',
                   '    further columns: covariates \n \n',sep="\n")))


  ###################################################
  if(inherits(reg, "lm")){
    if(is.null(reg$na.action)){
      coordinates = data[,1:2]
    }
    else{
      omitted.rows = reg$na.action
      coordinates = data[-omitted.rows,1:2]
    }
  }
  else if(inherits(reg, "lmerMod")){
    if(is.null(stats::na.action(reg@frame))){
      coordinates = data[,1:2]
    }
    else{
      omitted.rows = stats::na.action(reg@frame)
      coordinates = data[-omitted.rows,1:2]
    }
  }


  # calculation of the studentized residuals
  # in case of a mixed model, these resemble the conditional studentized residuals
  resi = stats::rstudent(reg)
  dat.res = cbind(x =coordinates[,1], y= coordinates[,2], adj = resi)
  dat.res = as.data.frame(dat.res)
  return(dat.res)
}




