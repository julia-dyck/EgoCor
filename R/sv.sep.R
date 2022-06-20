
# needed packages
# ## gstat


sv.sep = function(data, coords, max.dist, nbins){
  # emp.sv = geoR::variog(coords=coords, data=data, estimator.type="classical",
  #                     max.dist = max.dist, uvec = nbins,
  #                      messages = F)
  emp.sv = gstat::variogram(object = data[[1]] ~ 1, data = data, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill <- stats::var(data)
  ini.shape <- emp.sv$max.dist/3
  ini.values <- c(ini.partial.sill, ini.shape)
  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  #sv.mod <- geoR::variofit(emp.sv, ini.cov.pars = ini.values,
  #                         cov.model = "exponential", messages = F)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])
  return(mod.pars)
}
