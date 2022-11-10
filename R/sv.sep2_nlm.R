sv.sep2_nlm = function(data, coords, max.dist, nbins){
  data.ge = as.data.frame(cbind(coords,data))
  colnames(data.ge)[1:2] = c("x", "y")
  sp::coordinates(data.ge) = ~x+y
  emp.sv = gstat::variogram(object = data.ge[[1]] ~ 1, data = data.ge, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill <- stats::var(data.ge[[1]])
  ini.shape <- max(emp.sv$dist)/3
  ini.values <- c(ini.partial.sill, ini.shape)

  theta.star0 = log(c(1, ini.partial.sill, ini.shape))
  exp.variogram.mod = tryCatch(stats::nlm(loss, p = theta.star0, h = emp.sv$dist, gamma_hat = emp.sv$gamma,
                                          n_h = emp.sv$np, check.analyticals = F),
                                error = function(e) e)

  if (methods::is(exp.variogram.mod , "error")){
    return(c(0, 0, Inf))
  }
  else{
    return(exp(exp.variogram.mod$estimate))
  }
}

