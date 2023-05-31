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
#' @param threshold.factor The threshold factor specifies the filter within the filtered
#'                         bootstrap method (see details). If not specified, a default value of 1.2 is used.
#' @param fit.method The fit method used in the semivariogram estimation with the gstat package.
#' @param mc.cores The number of cores used for bootstrapping, utilizing the parallel R-package. More than one core is not supported on windows systems.
#'
#' @details \strong{Two alternative approaches for the input of the arguments:}
#'
#'          1. Provide the arguments
#'          vario.mod.output (output object from vario.mod function) and
#'          mod.nr (number of the model in the infotable).
#'
#'          2. Provide the necessary information manually, namely
#'          \code{par.est} (vector with estimated nugget, partial sill and shape parameters),
#'          \code{data} (used to estimate the semi-variogram model parameters),
#'          \code{max.dist} (semi-variogram parameter, numeric of length 1) and
#'          \code{nbins} (semi-variogram parameter, numeric of length 1).
#'
#'
#'          \strong{Filtered bootstrap method}:
#'
#'          For the semi-variogram model parameter estimation, the weighted least squares method is used
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
#'          of the data times the treshold factor \eqn{\tau}, ie.
#'          \deqn{c_{0 b}^* + \sigma_{0 b}^{2*} > \tau  \widehat{Var(\mathbf{z})}}
#'          for the bth bootstrap estimate, it is discarded. Otherwise, it is saved.
#'          This procedure is performed until B bootstrap estimates have aggregated.
#'          The empirical standard deviation calculated from the bootstrap estimates provides the
#'          uncertainty estimate for each parameter.
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
#' The function returns parameter estimates and corresponding standard error estimates
#' together and provides a list with the following objects:
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
                           par.est = NULL, data = NULL, max.dist = NULL, nbins = NULL,
                           B = 1000, threshold.factor = 1.2, fit.method = 7, mc.cores = 1){

  vario.mod.output.arg <- deparse(substitute(vario.mod.output))
  mod.nr.arg <- deparse(substitute(mod.nr))
  par.est.arg <- deparse(substitute(par.est))
  data.arg <- deparse(substitute(data))
  max.dist.arg <- deparse(substitute(max.dist))
  nbins.arg <- deparse(substitute(nbins))
  B.arg <- deparse(substitute(B))
  threshold.factor.arg <- deparse(substitute(threshold.factor))

  cat("Two approaches regarding the input arguments:\n 1. Provide the arguments\n    - vario.mod.output (output object from vario.mod function),\n    - mod.nr (number of the model in the vario.mod.output$infotable). \n 2. Provide the arguments\n    - par.est (vector with estimated nugget, partial sill and shape parameters),\n    - data (used to estimate the semi-variogram model parameters),\n    - max.dist (semi-variogram parameter, numeric of length 1),\n    - nbins (semi-variogram parameter, numeric of length 1).\n In both cases a threshold factor can be set. If not specified a default value of 1.2 is used.\n\n")

  if((missing(vario.mod.output)+missing(mod.nr)>0) & (missing(par.est))+(missing(data))+(missing(max.dist))+(missing(nbins))>0){
    stop("One approach regarding the input arguments has to be chosen.\n  and arguments have to be provided accordingly.\n")}

  # 2. approach:
  if(missing(vario.mod.output) & missing(mod.nr)){ # manual argument specification
    # check if alternative input is meaningful
    if(missing(par.est)){stop("Argument par.est has to be provided.\n")}
    if(missing(data)){stop("Argument data has to be provided.\n")}
    if(missing(max.dist)){stop("Argument max.dist has to be provided.\n")}
    if(missing(nbins)){stop("Argument nbins has to be provided.\n")}

    if(!(is.numeric(par.est)) || !(length(par.est)==3)){stop("Argument par.est has to be a numeric vector of length 3 containing the nugget effect,\n partial sill and shape of the exponential semi-variogram model.")}
    if(!((is.data.frame(data))||(is.matrix(data)))){stop("Argument data has to be a matrix or data frame with\n the x-coordinates in the first column,\n the y-coordinates in the second column\n and the attribute values of interest in the third column. \n It is required to be the same dataset used for the\n estimation of the semi-variogram model of interest.")}

    #### data input: formatting
    if(ncol(data)<3){stop("Data matrix contains less than 3 columns. It must contain\n the x-coordinates in the first column,\n the y-coordinates in the second column\n and the attribute values of interest in the third column. \n")}
    if(ncol(data)>3){warning(paste('Data matrix contains more than 3 columns. Are the columns in correct order?\n ',paste('Input data interpretation:',
                                                                                                                          '    column 1: cartesian x-coordinates in meters',
                                                                                                                          '    column 2: cartesian y-coordinates in meters',
                                                                                                                          '    column 3: outcome variable \n \n',sep="\n")))}
    data <- cbind(data[,1], data[,2], data[,3])
    data.ge = as.data.frame(data)
    data.ge = stats::na.omit(data.ge)
    colnames(data.ge)[1:2] = c("x", "y")
    sp::coordinates(data.ge) = ~x+y

    if(!(is.numeric(max.dist)) || !(length(max.dist)==1)){stop("Argument max.dist has to be a numeric of length 1.")}
    # ! for is.integer?
    if(!((is.numeric(nbins))||(is.integer(nbins))) || !(length(nbins)==1)){stop("Argument nbins has to be a numeric of length 1.")}
    if(!((is.numeric(fit.method)) || !(length(nbins)==1))){stop("Argument fit.method has to be a numeric of length 1.")}


    vario.mod.output = list()
    vario.mod.output$input.arguments = list(data = data, max.dist = max.dist, fit.method = fit.method)
    vario.mod.output$info.table = c(max.dist = max.dist, nbins = nbins,
                                    nugget = par.est[1], partial.sill = par.est[2], shape = par.est[3],
                                    prac.range = NA, RSV = NA, rel.bias = NA)
  }
  else{# 1. approach:
    # insert the vario.mod.output and specify the model to estimate
    # the parameter standard errors for
    ## check argument format/specification
    if(missing(vario.mod.output)){stop("Argument vario.mod.output has to be provided.\n")}
    if(missing(mod.nr)){stop("Argument mod.nr has to be provided.\n")}

    if(!inherits(vario.mod.output,"vario.mod.output")){stop("Argument vario.mod.output has to be an output of the vario.mod()-function.") }
    if(!is.numeric(mod.nr)|| length(mod.nr)>1){stop("Argument mod.nr has to be a numeric of length 1.\n")}
    # durch is() oder inherits() ersetzen
    vario.mod.output$info.table = vario.mod.output$infotable[mod.nr,]
    par.est = as.numeric(c(vario.mod.output$info.table$nugget,
                           vario.mod.output$info.table$partial.sill,
                           vario.mod.output$info.table$shape))

  }
  sample = vario.mod.output$input.arguments$data[,1:3]
  sample = stats::na.omit(sample)
  max.dist = as.numeric(vario.mod.output$info.table[1])
  nbins = as.numeric(vario.mod.output$info.table[2]) #input nbins (not corrected ones! in case of co-locatted observations)
  fit.method = as.numeric(vario.mod.output$input.arguments$fit.method)
  emp.variance = stats::var(sample[,3])
  # check whether the inserted sv model seems probable:
  tau = threshold.factor[1]
  if(par.est[1]+par.est[2] > tau*emp.variance){warning("The overall variance according to the estimated semi-variogram model does not represent\n the empirical variance of the data well.\n Please check whether the semi-variogram model is appropriate before using the\n parameter and uncertainty estimates.")}
  if(par.est[3] < 0){warning("The shape parameter phi according to the estimated semi-variogram model is < 0.\n Please check whether the semi-variogram model is appropriate before using the\n parameter and uncertainty estimates.")}

  ### apply bootstrap.unc.check fct.
  cat("\n Bootstrap started.\n This can take a few minutes depending on the number\n of bootstrap samples B to be generated.\n\n")

  sample.geo = as.data.frame(sample)
  colnames(sample.geo)[1:2] = c("x", "y")
  sp::coordinates(sample.geo) = ~x+y
  coords = sp::coordinates(sample.geo)
  z = sample.geo[[1]]

  # (1) nscore transformation
  nscore.obj = nscore(z)
  y = nscore.obj$nscore
  y.with.coords = cbind(coords,y)
  y.geo = as.data.frame(y.with.coords)
  sp::coordinates(y.geo) = ~x+y
  # (2) prep sv-model
  emp.sv = gstat::variogram(object = y.geo[[1]] ~ 1, data = y.geo, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill = 1
  ini.shape = max.dist/3
  ini.values = c(ini.partial.sill, ini.shape)

  # if (fit.method == 8){
  #   theta.star0 = log(c(.1, ini.partial.sill, ini.shape))
  #   sv.mod = stats::nlm(loss, p = theta.star0, h = emp.sv$dist, gamma_hat = emp.sv$gamma,
  #             n_h = emp.sv$np)
  #   mod.pars = exp(sv.mod$estimate)
  # }

  # no fit.method 8 anymore
  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                fit.method = fit.method,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])

  # (3)
  Dist_mat = SpatialTools::dist1(coords) # NxN distance matrix

  # function for predicting the covariance based on distance
  expmod = function(distvec, psill, phi) {psill*exp(-distvec/phi)}

  Cov_mat = apply(X = Dist_mat, MARGIN = 1, FUN = expmod, psill = mod.pars[2], phi = mod.pars[3])

  # NxN Covariance matrix, contains all point-pairs' estimated Covariances
  # based on sv.mod
  # (4) Cholesky decomposition -> fertige Fkt. existieren

  # Adding diag(epsilon) on the diagonal to force it to be positve definite (numerical reasons)
  Cov_mat = Cov_mat + diag(rep(1e-15, nrow(sample)))

  L = t(chol(Cov_mat))
  # (5) transform y in an iid sample
  y.iid = solve(L)%*%y

  par.est.b = t(parallel::mclapply(rep(0, B), one_resample_analysis_check, y.iid=y.iid, L=L,
                       nscore.obj = nscore.obj, coords = coords,
                       max.dist = max.dist, nbins = nbins,
                       threshold.factor=threshold.factor,
                       fit.method = fit.method,
                       mc.cores = mc.cores))

  par.est.b = matrix(unlist(par.est.b), nrow = length(par.est.b), byrow = T)
  col_names = rep(NA, length(threshold.factor))

  colnames(par.est.b) = c("nugget", "partial_sill", "range", "conv_filter", paste0("thr_filter_", threshold.factor))

  nr_estimates = length(which(apply(par.est.b[,-(1:3)], 1, sum) == 0))

  cat("Initial estimation finished\n", nr_estimates, "of", B ,"Models converged.\n")
  cat("Reestimating:\n")

  counter = 0
  while(nr_estimates < B){
    if(B-nr_estimates < 20) mc.cores = 1
    re.par.est = t(parallel::mclapply(rep(0, B-nr_estimates), one_resample_analysis_check, y.iid=y.iid, L=L,
                                      nscore.obj = nscore.obj, coords = coords,
                                      max.dist = max.dist, nbins = nbins,
                                      threshold=threshold.factor,
                                      fit.method = fit.method,
                                      mc.cores = mc.cores))
    re.par.est = matrix(unlist(re.par.est), nrow = length(re.par.est), byrow = T)
    par.est.b = rbind(par.est.b, re.par.est)
    nr_estimates = length(which(apply(par.est.b[,-(1:3)], 1, sum) == 0))
    counter = counter +1

    cat('\r', nr_estimates, "of", B, "models converged.")
    flush.console()
  }

  # evaluating the sds of the parameter estimates
  nr.thr = length(threshold.factor)
  par.sds = numeric(3*nr.thr)
  cis = numeric(6*nr.thr)
  for(i in 1:nr.thr){
    par.est.cleaned = par.est.b[which(par.est.b[,4+i] == 0 & par.est.b[,4] == 0),][1:B,]
    par.sds[(i-1)*3+(1:3)] = apply(par.est.cleaned[,1:3], 2, stats::sd)
    cis[(i-1)*6+(1:6)] = c(stats::quantile(par.est.cleaned[,1], probs=c(0.025,0.975)),
                           stats::quantile(par.est.cleaned[,2], probs=c(0.025,0.975)),
                           stats::quantile(par.est.cleaned[,3], probs=c(0.025,0.975)))
  }

  names(par.sds) = paste0(rep(c("n.sd","s.sd","p.sd"),nr.thr), as.vector(sapply(threshold.factor,rep, times=3)))
  names(cis) = paste0(c(rep("n",2),rep("s",2),rep("p",2)), ".ci",c("l","u"),as.vector(sapply(threshold.factor,rep, times=6)))
  rownames(par.est.b) = NULL
  par.re_est = par.est.b[which(apply(par.est.b[,-(1:3)], 1, sum) == 0),1:3]
  unc.est = list(sds = par.sds, cis = cis, par.re_est = par.re_est)


  #return(unc.est)
  ### FORMAT THE RESULTS
  est = vario.mod.output$infotable[mod.nr, 4:6]
  unc.table = as.data.frame(cbind(as.numeric(rep(est, length(threshold.factor))), unc.est$sds))
  if (nrow(unc.table) == 3){rownames(unc.table) = c("nugget effect", "partial sill", "shape")}
  colnames(unc.table) = c("Estimate", "Std. Error")

  ses = unc.est$sds
  names(ses) =  c("se(nugget)","se(partial.sill)","se(shape)")

  re_estimates = unc.est$par.re_est
  colnames(re_estimates) = c("nugget.star","part.sill.star","shape.star")
  reest.means = colMeans(re_estimates)
  names(reest.means) =  c("mean(nugget)","mean(partial.sill)","mean(shape)")

  # find number of reestimates for each threshold
  nr_reest_gstat = sum(par.est.b[,4])
  nr_reest_thr = numeric(length(threshold.factor))
  nr_overlap = numeric(length(threshold.factor))
  for (i in 1:length(threshold.factor)){
    nr_reest_thr[i] = sum(par.est.b[,4+i])
    nr_overlap[i] = sum(par.est.b[,4] == 1 & par.est.b[,4+i] == 1)
  }
  # make table
  nr_reest_table = matrix(c(rep(nr_reest_gstat, length(threshold.factor)), nr_reest_thr, nr_overlap),
                            ncol = 3, nrow = length(threshold.factor))
  colnames(nr_reest_table) = c("nr_reest_gstat", "nr_reest_thr", "nr_overlap")
  rownames(nr_reest_table) = paste0("thr_", threshold.factor)


  # Fct. call
  c = paste("par.uncertainty(vario.mod.output =", vario.mod.output.arg,
            ", mod.nr =", mod.nr.arg, ", par.est =", par.est.arg,
            ", data=", data.arg, ", max.dist=", max.dist.arg,
            ", nbins =", nbins.arg, ", B =", B.arg, ", threshold.factor =", threshold.factor.arg, ")")

  cat("\n Call: ", c,"\n\n")
  c.call = as.call(str2lang(c))

  print(unc.table)

  ### RETURN RESULTS
  #return(list(se= ses, unc.table = unc.table, call = c.call))
  invisible(list(se = ses,
                 unc.table = unc.table,
                 re_estimates = unc.est$par.re_est,
                 re_estimate.means = reest.means,
                 call = c.call, all_est = par.est.b,
                 nr_reest = nr_reest_table))
  ### print sth automatically?

}

