


one_resample_analysis_check = function(platzhalter, y.iid, L, nscore.obj, coords, max.dist, nbins, threshold){
  # (6) resampling from y.iid
  resmpl = sample(y.iid, size = length(y.iid), replace = T)

  # (7) recorrelate the resmpl
  resmpl = L%*%resmpl

  # (8) backtransformation of the sample
  resmpl = backtr(resmpl, nscore = nscore.obj, tails="none", draw=F)

  # (9) repeat steps (6)-(8) <- repeating (6),(7),(8) and (10) is done by
  #                             repeating this function application

  # (10) semivariogram model estimation, wls
  wls.est = sv.sep(resmpl, coords = coords, max.dist = max.dist, nbins = nbins)
  emp.var = stats::var(resmpl)
  mod.var = wls.est[1] + wls.est[2]

  ## check, whether estimated by the model (c_0+sigma_sq) exceeds
  # the empirical variance*factor
  # applied factors and nr of factors according to input argument "threshold"
  nr.thr = length(threshold)
  wls.threshold.outcomes = numeric(length = 3*nr.thr)
  for(i in 1:nr.thr){
    if(mod.var > threshold[i]*emp.var){
      wls.threshold.outcomes[(i-1)*3+(1:3)] = rep(NA, 3)
    }
    else{
      wls.threshold.outcomes[(i-1)*3+(1:3)] = wls.est
    }
  }
  return(wls.threshold.outcomes) #outcome is a vector of 3*length(threshold)
}
