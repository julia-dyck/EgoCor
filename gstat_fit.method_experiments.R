##

library(gstat)


str(birth)
birth.reformatted = birth
sp::coordinates(birth.reformatted) = ~x+y
variog.gstat = gstat::variogram(object = birthweight ~ 1, data = birth.reformatted, cutoff = 500, width = 50)

# prep exponential model (with or without starting values for the fitting):
v <- vgm("Exp")                        # variogram model (without starting values)
v2 <- vgm(psill = var(birth$birthweight),  # plausible starting value for psill = sample variance
          model = "Exp",
          range = 500/3,     # plausible starting value for range: maxdist/3
          nugget = 0)      # plausible starting value for nugget

vfit.gstat = fit.variogram(variog.gstat, model=v2,  # fitting the model with starting model
                           fit.sills = TRUE,
                           fit.ranges = TRUE,
                           fit.method = 7,
                           debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit.gstat
plot(variog.gstat, vfit.gstat, cutoff=1000)

attr(vfit.gstat,"SSErr") # sum of squared errors (= loss function value)

### manually compute the SSEs (= loss function value) :
# to verify the weights in the loss function
exp.sv = function(lag, nugget, psill, phi){
  ret = nugget + psill*(1-exp(-lag/phi))
  return(ret)
}

gamma.mod = exp.sv(lag = variog.gstat$dist, nugget = vfit.gstat$psill[1], vfit.gstat$psill[2], phi = vfit.gstat$range)

weig = variog.gstat$np                       # if fit.method = 1 :)
weig = variog.gstat$np/variog.gstat$gamma^2  # if fit.method = 2 :)
# fit method 3 - 5 not implemented for fit.variogram
weig = rep(1, length(variog.gstat$np))       # if fit.method = 6 :)

weig = variog.gstat$np/variog.gstat$dist^2   # if fit.method = 7 :(

sse = sum(weig*(variog.gstat$gamma - gamma.mod)^2)
sse
# compare to
attr(vfit.gstat,"SSErr") # sum of squared errors acc. to gstat output


















