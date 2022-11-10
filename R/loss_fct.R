# weighted least squares loss with weights 1/N_h
loss = function(theta.star, h, gamma_hat, n_h){
  theta = exp(theta.star)
  expr1 = exp(-h/theta[3])
  expr2 = (1 - expr1)
  gamma_exp = theta[1] + theta[2]*expr2
  expr3 = (gamma_hat - gamma_exp)
  out = sum(n_h*expr3^2)
  return(out)
}
