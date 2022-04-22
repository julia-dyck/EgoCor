#' Semi-variogram parameter uncertainty
#'
#' Standard error estimates for the nugget effect \eqn{c_0},  partial sill \eqn{\sigma_0^2} and
#' shape parameter \eqn{\phi} of a fitted exponential semi-variogram model.
#'
#' @param vario.mod.output An output of the \code{vario.mod} function containing the
#'                         information of the estimated exponential semi-variogram model of interest.
#' @param mod.nr The index number specifiying one of the exponential semi-variogram models
#'               listed in the \code{vario.mod.ouput}.
#' @param par.est A vector of length three containing the estimated parameters:
#'                the nugget effect, the partial sill and the shape parameter
#'                of the estimated exponential semi-variogram model.
#'                It is automatically extracted from the \code{vario.mod.output}, if provided.
#' @param data The data frame or matrix used to estimate the exponential semi-variogram of interest
#'             containing the x-coordinates in meters in the first column,
#'             the y-coordinates in meters in the second column and the data values in the third column.
#'             It is automatically extracted from the vario.mod.output, if provided.
#' @param max.dist The maximal distance used for the estimation of the
#'                 exponential semi-variogram model of interest.
#'                It is automatically extracted from the vario.mod.output, if provided.
#' @param nbins The number of bins used for the estimation of the exponential
#'              semi-variogram model of interest.
#'              It is automatically extracted from the vario.mod.output, if provided.
#' @param B The number of bootstrap repetitions to generate a set of re-estimates
#'          of each parameter.
#'
#' @details \strong{Two alternative approaches for the input of the arguments:}
#'
#'          1. Provide the arguments
#'          vario.mod.output (output object from vario.mod function) and
#'          mod.nr (number of the model in the infotable).
#'
#'          2. Provide the arguments manually, namely
#'          par.est (vector with estimated nugget, partial sill and shape parameters),
#'          data (used to estimate the semi-variogram model parameters),
#'          max.dist (semi-variogram parameter, numeric of length 1) and
#'          nbins (semi-variogram parameter, numeric of length 1).
#'
#'
#'          \strong{Filtered bootstrap method}:
#'
#'          For the semi-variogram model parameter estimation, the weighed least squares method is used
#'          in order to make the numerical calculation possible for large sample sizes.
#'          A filter is set up within the bootstrapping process to remove all
#'          bootstrap estimates for which the estimation algorithm for the semi-variogram
#'          model parameters did not converge.
#'
#'          The parameter standard errors are estimated using the generalized bootstrap
#'          method with check-based filtering.
#'          The semi-variogram structure from the given model is used to remove the
#'          spatial correlation structure within the original dataset. Then,
#'          classical bootstrap sampling with replacement is used to generate B
#'          bootstrap samples from the uncorrelated data.
#'          Each bootstrap sample inherits the correlation structure back and is used to estimate
#'          the nugget effect, partial sill and shape parameter for an exponential model.
#'          Within the bootstrap repetitions, a test is performed to check whether
#'          the estimated parameters lie within a probable range.
#'          If the total variance of the bootstrap model exceeds the empirical variance
#'          of the data times the treshold factor \eqn{\tau = 1.2}, ie.
#'          \deqn{c_{0 b}^* + \sigma_{0 b}^{2*} > \tau  \widehat{Var(\mathbf{z})}}
#'          for the bth bootstrap estimate, it is discarded. Otherwise, it is saved.
#'          This procedure is performed until B bootstrap estimates have aggregated.
#'          Based on the latter, the standard error for the nugget effect, partial sill
#'          and shape parameter are each estimated by the empirical standard deviation.
#'
#'          Details about the algorithm used to obtain standard errors for the parameters
#'          of the exponential semi-variogram model are provided in \insertCite{dyck_sv_ses;textual}{EgoCor}.
#'
#'          \strong{Reproducibility}:
#'
#'          In order to generate reproducible bootstrap results, set a random seed with the command \code{set.seed()}
#'          before using the \code{par.uncertainty} function.
#'
#' @return
#' Returns parameter estimates and corresponding standard error estimates
#' together with a list with the following objects:
#' \item{se}{A vector of length 3 containing the estimated standard errors of the
#'           nugget effect, the partial sill and the shape parameter.}
#' \item{unc.table}{A matrix containing the parameter estimates and the corresponding standard errors.}
#' \item{re_estimates}{A matrix with B rows containing the set of bootstrap re-estimates for each parameter.}
#' \item{re_estimate.mean}{A vector containing the mean parameter estimates based on the set of bootstrap re-estimates for each parameter.}
#' \item{call}{The function call.}
#'
#'
#'
#' @examples
#' ## Example 1
#' # Estimate semi-variogram models:
#' mods = vario.mod(data = birth, max.dist = c(1000,600), nbins = 13,
#'                  shinyresults = FALSE, windowplots = FALSE)
#' print(mods$infotable)
#'
#' # Estimate the parameter standard errors:
#' \donttest{
#' se.mod1 = par.uncertainty(vario.mod.output = mods, mod.nr = 1, B = 1000)
#' se.mod2 = par.uncertainty(vario.mod.output = mods, mod.nr = 2, B = 1000)
#' }
#'
#' ## Example 2
#' # Type in the specifications of the estimated exponential semi-variogram manually:
#' \donttest{
#' se.mod1.man = par.uncertainty(par.est = c(1021.812, 225440.3, 0),
#'               data = birth, max.dist = 1000, nbins = 13, B = 1000)
#'
#' se.mod2.man = par.uncertainty(par.est = c(121895.486, 107232.6, 63.68720),
#'               data = birth, max.dist = 600, nbins = 13, B = 1000)
#' }
#'
#'
#' @references
#'   \insertAllCited{}
#'
#' @export



par.uncertainty = function(vario.mod.output, mod.nr,
                           par.est = NULL, data= NULL, max.dist=NULL,nbins=NULL,
                           B = 1000){
  vario.mod.output.arg <- deparse(substitute(vario.mod.output))
  mod.nr.arg <- deparse(substitute(mod.nr))
  par.est.arg <- deparse(substitute(par.est))
  data.arg <- deparse(substitute(data))
  max.dist.arg <- deparse(substitute(max.dist))
  nbins.arg <- deparse(substitute(nbins))
  B.arg <- deparse(substitute(B))

  message("Two possible approaches regarding the input arguments:\n 1. Provide the arguments\n    - vario.mod.output (output object from vario.mod function),\n    - mod.nr (number of the model in the vario.mod.output$infotable). \n 2. Provide the arguments\n    - par.est (vector with estimated nugget, partial sill and shape parameters),\n    - data (used to estimate the semi-variogram model parameters),\n    - max.dist (semi-variogram parameter, numeric of length 1),\n    - nbins (semi-variogram parameter, numeric of length 1).\n\n")

  if((missing(vario.mod.output)+missing(mod.nr)>0) & (missing(par.est))+(missing(data))+(missing(max.dist))+(missing(nbins))>0){
    stop("One approach regarding the input arguments has to be chosen.\n  and arguments have to be provided accordingly.\n")}

  # 2. approach:
  if(missing(vario.mod.output) & missing(mod.nr)){ # manual argument specification
    # check if alternative input is meaningful
    if(missing(par.est)){stop("Argument par.est has to be provided.\n")}
    if(missing(data)){stop("Argument data has to be provided.\n")}
    if(missing(max.dist)){stop("Argument max.dist has to be provided.\n")}
    if(missing(nbins)){stop("Argument nbins has to be provided.\n")}

    if(!(class(par.est)=="numeric") || !(length(par.est)==3)){stop("Argument par.est has to be a numeric vector of length 3 containing the nugget effect,\n partial sill and shape of the exponential semi-variogram model.")}
    if(!((class(data)=="data.frame")||(class(data)=="matrix"))){stop("Argument data has to be a matrix or data frame with\n the x-coordinates in the first column,\n the y-coordinates in the second column\n and the attribute values of interest in the third column. \n It is required to be the same dataset used for the\n estimation of the semi-variogram model of interest.")}

    #### data input: formatting
    if(ncol(data)<3){stop("Data matrix contains less than 3 columns. It must contain\n the x-coordinates in the first column,\n the y-coordinates in the second column\n and the attribute values of interest in the third column. \n")}
    if(ncol(data)>3){warning(paste('Data matrix contains more than 3 columns. Are the columns in correct order?\n ',paste('Input data interpretation:',
                                                                                                                          '    column 1: Cartesian x-coordinates in meters',
                                                                                                                          '    column 2: Cartesian y-coordinates in meters',
                                                                                                                          '    column 3: outcome variable \n \n',sep="\n")))}
    data <- cbind(data[,1], data[,2], data[,3])
    data <- as.data.frame(data.frame(geoR::jitterDupCoords(data[,1:2],max=0.01),data[,3]))
    data.ge <- geoR::as.geodata(data, coords.col = 1:2, data.col = 3, na.action = "ifany")

    if(!(class(max.dist)=="numeric") || !(length(max.dist)==1)){stop("Argument max.dist has to be a numeric of length 1.")}
    if(!((class(nbins)=="numeric")||(class(nbins)=="integer")) || !(length(nbins)==1)){stop("Argument nbins has to be a numeric of length 1.")}


    vario.mod.output = list()
    vario.mod.output$input.arguments = list(data = data, max.dist = max.dist)
    vario.mod.output$info.table = c(max.dist = max.dist, nbins = nbins,
                             nugget = par.est[1], partial.sill = par.est[2], shape = par.est[3],
                             prac.range = NA, RSV =NA, rel.bias = NA)
  }
  else{# 1. approach:
    # insert the vario.mod.output and specify the model to estimate
    # the parameter standard errors for
    ## check argument format/specification
    if(missing(vario.mod.output)){stop("Argument vario.mod.output has to be provided.\n")}
    if(missing(mod.nr)){stop("Argument mod.nr has to be provided.\n")}

    if(!class(vario.mod.output) == "vario.mod.output"){stop("Argument vario.mod.output has to be an output of the vario.mod()-function.") }
    if(!is.numeric(mod.nr)|| length(mod.nr)>1){stop("Argument mod.nr has to be a numeric of length 1.\n")}

    vario.mod.output$info.table = vario.mod.output$infotable[mod.nr,]
    par.est = as.numeric(c(vario.mod.output$info.table$nugget,
                           vario.mod.output$info.table$partial.sill,
                           vario.mod.output$info.table$shape))

  }
  sample = vario.mod.output$input.arguments$data[,1:3]
  max.dist = as.numeric(vario.mod.output$info.table[1])
  nbins = as.numeric(vario.mod.output$info.table[2]) #input nbins (not corrected ones! in case of co-locatted observations)
  emp.variance = stats::var(sample[,3])
  # check whether the inserted sv model seems probable:
  tau = 1.2
  if(par.est[1]+par.est[2] > tau*emp.variance){warning("The overall variance according to the estimated semi-variogram model does not represent\n the empirical variance of the data well.\n Please check whether the semi-variogram model is appropriate before using the\n parameter and uncertainty estimates.")}

  # print(paste0("Check filter method with tau = 1.2 is applied."))


  ### apply bootstrap.unc.check fct.
  message("\n Bootstrap started.\n This can take a few minutes depending on the number\n of bootstrap samples B to be generated.\n\n")
  unc.est = bootstrap.unc.check(sample=sample, max.dist=max.dist, nbins = nbins, B = B, thr = tau)
  ### save the results

  #return(unc.est)
  ### FORMAT THE RESULTS
  unc.table = cbind((par.est), unc.est$sds)
  rownames(unc.table) = c("nugget effect", "partial sill", "shape")
  colnames(unc.table) = c("Estimate", "Std. Error")

  ses = unc.est$sds
  names(ses) =  c("se(nugget)","se(partial.sill)","se(shape)")

  re_estimates = unc.est$par.re_est
  colnames(re_estimates) = c("nugget.star","part.sill.star","shape.star")
  reest.means = colMeans(re_estimates)
  names(reest.means) =  c("mean(nugget)","mean(partial.sill)","mean(shape)")


  # Fct. call
  c = paste("par.uncertainty(vario.mod.output =", vario.mod.output.arg,
            ", mod.nr =", mod.nr.arg, ", par.est =", par.est.arg,
            ", data=", data.arg, ", max.dist=", max.dist.arg,
            ", nbins =", nbins.arg, ", B =", B.arg, ")")

  cat("\n Call: ", c,"\n\n")
  c.call = as.call(str2lang(c))

  print(unc.table)

  ### RETURN RESULTS
  #return(list(se= ses, unc.table = unc.table, call = c.call))
  invisible(list(se = ses,
                 unc.table = unc.table,
                 re_estimates = unc.est$par.re_est,
                 re_estimate.means = reest.means,
                 call = c.call))
  ### print sth automatically?

}






