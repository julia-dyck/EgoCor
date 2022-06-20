##########################################################
#### bootstrap uncertainty fct. using the checkfilter ####
##########################################################

# necessary packages
# ## gstat
# ## sp
# ## SpatialTools
# ## stats

bootstrap.unc.check = function(sample, max.dist, nbins, B = 1000, thr=c(1.1,1.5,2.0,2.5,3.0)){
  # sample.geo = geoR::as.geodata(sample)
  sample.geo = as.data.frame(sample)
  colnames(sample.geo)[1:2] = c("x", "y")
  sp::coordinates(sample.geo) = ~x+y
  coords = sp::coordinates(sample.geo)
  z = sample.geo[[1]]

  # (1) nscore transformation
  nscore.obj = nscore(z)
  y = nscore.obj$nscore
  y.with.coords = cbind(coords,y)
  # y.geo = geoR::as.geodata(y.with.coords)
  y.geo = as.data.frame(y.with.coords)
  sp::coordinates(y.geo) = ~x+y
  # (2) prep sv-model
  # emp.sv = geoR::variog(y.geo, estimator.type="classical", max.dist = max.dist, uvec = nbins, messages = F)
  emp.sv = gstat::variogram(object = y.geo[[1]] ~ 1, data = y.geo, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill = stats::var(y.geo[[1]])
  ini.shape = max(emp.sv$dist)/3
  ini.values = c(ini.partial.sill, ini.shape)
  # sv.mod <- geoR::variofit(emp.sv, ini.cov.pars = ini.values, cov.model = "exponential", messages = F)
  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                            fit.sills = TRUE,
                                            fit.ranges = TRUE,
                                            debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
  # mod.pars = c(sv.mod$nugget, sv.mod$cov.pars[1],sv.mod$cov.pars[2])
  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])
  # (3)
  Dist_mat = SpatialTools::dist1(coords) # NxN distance matrix

  expmod = function(distvec, psill, phi){
    return(psill*exp(-distvec/phi))
  }

  Cov_mat = apply(X = Dist_mat, MARGIN = 1, FUN = expmod, psill = mod.pars[2], phi = mod.pars[3])

  # Cov_mat = geoR::cov.spatial(Dist_mat, cov.model=c("exponential","pure.nugget"),
  #                      cov.pars = rbind(c(mod.pars[2],mod.pars[3]),c(mod.pars[1],0)))  # hier kein Analogon gefunden
  # NxN Covariance matrix, contains all point-pairs' estimated Covariances
  # based on sv.mod
  # (4) Cholesky decomposition -> fertige Fkt. existieren

### zum testen: Damit Matrix positiv definit ist
  Cov_mat = Cov_mat + diag(rep(0.0001, nrow(sample)))

  L = t(chol(Cov_mat))
  # (5) transform y in an iid sample
  y.iid = solve(L)%*%y

  # (6),(7),(8) and (10)
  #par.est = one.resample.analysis(platzhalter = NULL, y.iid=y.iid,
  #                                L=L, nscore.obj = nscore.obj,
  #                                coords = coords, max.dist = max.dist, nbins = nbins)
  par.est = t(sapply(rep(0, B), one_resample_analysis_check, y.iid=y.iid, L=L,
                     nscore.obj = nscore.obj, coords = coords,
                     max.dist = max.dist, nbins = nbins,
                     threshold=thr))
  par.est = stats::na.omit(par.est)
  nr_reestimates = length(stats::na.omit(par.est[,1]))
  while(nr_reestimates < B){
    next.est = one_resample_analysis_check(platzhalter=NULL, y.iid=y.iid, L=L,
                                           nscore.obj=nscore.obj, coords=coords,
                                           max.dist=max.dist, nbins = nbins, threshold=thr)
    if(is.na(next.est[1])){
      par.est = rbind(par.est, next.est)
    }
    else{
      par.est = rbind(par.est, next.est)
      nr_reestimates= nr_reestimates + 1
    }
  }
  # evaluating the sds of the parameter estimates
  nr.thr = length(thr)
  par.sds = numeric(length = 3*nr.thr)
  cis = numeric(length = 6*nr.thr)
  for(i in 1:nr.thr){
    par.est.cleaned = stats::na.omit(par.est[,(i-1)*3+(1:3)])[1:B,]
    par.sds[(i-1)*3+(1:3)] = apply(par.est.cleaned, 2, stats::sd)
    cis[(i-1)*6+(1:6)] = c(stats::quantile(par.est.cleaned[,1], probs=c(0.025,0.975)),
                           stats::quantile(par.est.cleaned[,2], probs=c(0.025,0.975)),
                           stats::quantile(par.est.cleaned[,3], probs=c(0.025,0.975)))
  }

  names(par.sds) = paste0(rep(c("n.sd","s.sd","p.sd"),nr.thr), as.vector(sapply(thr,rep, times=3)))
  names(cis) = paste0(c(rep("n",2),rep("s",2),rep("p",2)), ".ci",c("l","u"),as.vector(sapply(thr,rep, times=6)))
  rownames(par.est) = NULL
  colnames(par.est) = c("n.re_est","s.re_est","p.re_est")

  output = list(sds = par.sds, cis = cis, par.re_est = stats::na.omit(par.est))
  return(output)
}




# buc = bootstrap.unc.check(sample = test$input.arguments$data,max.dist = test$infotable[5,1], B = 10, thr = 1.2)
# buc$sds
#
# buc$cis
# buc$par.re_est
# dim(buc$par.re_est)




