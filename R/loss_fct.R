loss = function(theta.star, h, gamma_hat, n_h){
  # weighted least squares with weights 1/N_h
  theta = exp(theta.star)
  expr1 = exp(-h/theta[3])
  expr2 = (1 - expr1)
  gamma_exp = theta[1] + theta[2]*expr2
  expr3 = (gamma_hat - gamma_exp)
  out = sum(n_h*expr3^2)

  attr(out, "gradient") = c(
    -2*sum(n_h*expr3),
    -2*sum(n_h*expr3*expr2),
    2*theta[2]/(theta[3]^2)*sum(expr3*expr1*h)
  )
  return(out)
}
