
# par.uncertainty function with added argument "threshold.factor"
# to tune the threshold manually


par.uncertainty.thr = function(vario.mod.output, mod.nr,
                           par.est = NULL, data= NULL, max.dist=NULL,nbins=NULL,
                           B = 1000, threshold.factor = 1.2){
  vario.mod.output.arg <- deparse(substitute(vario.mod.output))
  mod.nr.arg <- deparse(substitute(mod.nr))
  par.est.arg <- deparse(substitute(par.est))
  data.arg <- deparse(substitute(data))
  max.dist.arg <- deparse(substitute(max.dist))
  nbins.arg <- deparse(substitute(nbins))
  B.arg <- deparse(substitute(B))
  threshold.factor.arg <- deparse(substitute(threshold.factor))

  cat("Two approaches regarding the input arguments:\n 1. Provide the arguments\n    - vario.mod.output (output object from vario.mod function),\n    - mod.nr (number of the model in the vario.mod.output$infotable). \n 2. Provide the arguments\n    - par.est (vector with estimated nugget, partial sill and shape parameters),\n    - data (used to estimate the semi-variogram model parameters),\n    - max.dist (semi-variogram parameter, numeric of length 1),\n    - nbins (semi-variogram parameter, numeric of length 1).\n\n")

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
                                                                                                                          '    column 1: cartesian x-coordinates in meters',
                                                                                                                          '    column 2: cartesian y-coordinates in meters',
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
  tau = threshold.factor
  if(par.est[1]+par.est[2] > tau*emp.variance){warning("The overall variance according to the estimated semi-variogram model does not represent\n the empirical variance of the data well.\n Please check whether the semi-variogram model is appropriate before using the\n parameter and uncertainty estimates.")}



  ### apply bootstrap.unc.check fct.
  cat("\n Bootstrap started.\n This can take a few minutes depending on the number\n of bootstrap samples B to be generated.\n\n")
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
                 call = c.call))
  ### print sth automatically?

}

