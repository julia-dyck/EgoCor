


one_resample_analysis_check2 = function(platzhalter, y.iid, L, nscore.obj, coords, max.dist, nbins, threshold.factor, fit.method = 7){
  # (6) resampling from y.iid
  resmpl = sample(y.iid, size = length(y.iid), replace = T)

  # (7) recorrelate the resmpl
  resmpl = L%*%resmpl

  # (8) backtransformation of the sample
  resmpl = backtr(resmpl, nscore = nscore.obj, tails="none", draw=F)

  # (9) repeat steps (6)-(8) <- repeating (6),(7),(8) and (10) is done by
  #                             repeating this function application

  # (10) semivariogram model estimation, wls
  wls.est = sv.sep2(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method)
  warning = tryCatch(sv.sep2(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method),
                       warning = function(w) w)

  emp.var = stats::var(resmpl)
  mod.var = wls.est[1] + wls.est[2]

  ## check, whether
  # ## 1. variance estimated by the model (c_0+sigma_sq) exceeds
  #       the empirical variance*factor OR
  # ## 2. re_estimated shape parameter phi < 0 (<- actually happens in model fitting!)
  # applied factors and nr of factors according to input argument "threshold"
  nr.thr = length(threshold.factor)
  wls.threshold.outcomes = rep(0, 4 + nr.thr) # 1-3 estimates, 4 convergence check, 5-... check filter
  wls.threshold.outcomes[1:3] = wls.est
  for(i in 1:nr.thr){
    if(is(warning, "warning")){ # change this
      wls.threshold.outcomes[4] = 1
    }
    else{
      if(mod.var > threshold.factor[i]*emp.var | wls.est[3] < 0){ # check filter
        wls.threshold.outcomes[4 + i] = 1
      }
    }
  }
  # vector with:
  # - estimates
  # - convergence warning information
  # - check filter information
  return(wls.threshold.outcomes)
}
