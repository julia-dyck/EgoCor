one_resample_analysis_check_alpha = function(platzhalter, y.iid, L, nscore.obj, coords, max.dist, nbins, threshold.factor, alpha, fit.method = 7){
  # (6) resampling from y.iid
  resmpl = sample(y.iid, size = length(y.iid), replace = T)

  # (7) recorrelate the resmpl
  resmpl = L%*%resmpl

  # (8) backtransformation of the sample
  resmpl = backtr(resmpl, nscore = nscore.obj, tails="none", draw=F)

  # (9) repeat steps (6)-(8) <- repeating (6),(7),(8) and (10) is done by
  #                             repeating this function application

  # (10) semivariogram model estimation, wls

  # if (fit.method == 8){
  #   wls.est = sv.sep2_nlm(resmpl, coords = coords, max.dist = max.dist, nbins = nbins)
  #   warning = NULL
  # }
  # else{

  wls = sv.sep(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method)
  wls.est = wls$mod.pars

  emp.var = stats::var(resmpl)
  mod.var = wls.est[1] + wls.est[2]

  ## check, whether
  # ## 1. variance estimated by the model (c_0+sigma_sq) exceeds
  #       the empirical variance*factor OR
  # ## 2. re_estimated shape parameter phi < 0 (<- actually happens in model fitting!)
  # applied factors and nr of factors according to input argument "threshold.factor"
  nr.thr = length(threshold.factor)
  wls.threshold.outcomes = rep(0, 5 + nr.thr) # 1-3 estimates, 4 convergence check, 5 CI check, 6-... check filter
  wls.threshold.outcomes[1:3] = wls.est

  if(wls$warning | wls.est[3] == "Inf") wls.threshold.outcomes[4] = 1

  if(ci_check(n=length(resmpl), emp.var=emp.var, mod.var=mod.var, alpha=alpha)==F) wls.threshold.outcomes[5] = 1

  for(i in 1:nr.thr){
    if(mod.var > threshold.factor[i]*emp.var | wls.est[3] < 0){ # check filter
        wls.threshold.outcomes[5 + i] = 1
    }
  }
  # vector with:
  # - estimates
  # - convergence warning information
  # - check filter information
  return(wls.threshold.outcomes)
}
