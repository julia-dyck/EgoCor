# function that performs a check if the variance of the estimated semivariogram lies in the 1-alpha confindence interval
CI_check = function(data, # vector
                    par, # estimated parameters
                    alpha) # significance level
  {
  n = length(data)
  mod.var = par[1]+par[2]
  s_sq = var(data)
  CI1 = (n-1)*s_sq/qchisq(p = 1-alpha/2, df = n-1) # lower
  CI2 = (n-1)*s_sq/qchisq(p = alpha/2, df = n-1) # upper

  if(CI1 <= mod.var & mod.var <= CI2){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
