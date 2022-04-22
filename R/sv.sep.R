
# needed packages
# ## geoR


sv.sep = function(data, coords, max.dist, nbins){
  emp.sv = geoR::variog(coords=coords, data=data, estimator.type="classical",
                        max.dist = max.dist, uvec = nbins,
                        messages = F)
  ini.partial.sill <- stats::var(data)
  ini.shape <- emp.sv$max.dist/3
  ini.values <- c(ini.partial.sill, ini.shape)
  sv.mod <- geoR::variofit(emp.sv, ini.cov.pars = ini.values,
                           cov.model = "exponential", messages = F)
  mod.pars = c(sv.mod$nugget, sv.mod$cov.pars[1],sv.mod$cov.pars[2])
  return(mod.pars)
}
