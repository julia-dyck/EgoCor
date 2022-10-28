#
# needed packages
# ## gstat


sv.sep2 = function(data, coords, max.dist, nbins, fit.method){
  data.ge = as.data.frame(cbind(coords,data))
  colnames(data.ge)[1:2] = c("x", "y")
  sp::coordinates(data.ge) = ~x+y
  emp.sv = gstat::variogram(object = data.ge[[1]] ~ 1, data = data.ge, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill <- stats::var(data.ge[[1]])
  ini.shape <- max(emp.sv$dist)/3
  ini.values <- c(ini.partial.sill, ini.shape)
  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  warning = tryCatch(gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                fit.method = fit.method,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE),
                    warning = function(w) w)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                fit.method = fit.method,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
  if (methods::is(warning, "warning")){
    warning("No convergence")
  }
  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])
  return(mod.pars)
}
