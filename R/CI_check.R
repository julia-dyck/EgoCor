# function that performs a check if the variance of the estimated semivariogram lies in the 1-alpha confindence interval
ci_check = function(n, emp.var, mod.var, alpha){
  CI1 = (n-1)*emp.var/qchisq(p = 1-alpha/2, df = n-1) # lower
  CI2 = (n-1)*emp.var/qchisq(p = alpha/2, df = n-1) # upper

  if(CI1 <= mod.var & mod.var <= CI2){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
