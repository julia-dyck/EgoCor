## Goal:
## Verify the weights in the loss function according to gstat::fit.variogram
## documentation details regarding fit.method argument

library(gstat)
library(sp)

exp.sv = function(lag, nugget, psill, phi){ #exp. sv. model values for lag distance
  ret = nugget + psill*(1-exp(-lag/phi))
  return(ret)
}

# spatial sample
data(meuse)

sp::coordinates(meuse) = ~x+y

# fit empirical semivariogram model
variog = gstat::variogram(object = log(zinc) ~ 1, data = meuse, cutoff = 1500, width = 150)

# prep exponential model
v <- vgm(psill = var(log(meuse$zinc)),  # plausible starting value for psill = sample variance
          model = "Exp",
          range = 1500/3,     # plausible starting value for range: maxdist/3
          nugget = 0)      # plausible starting value for nugget

# ---------------------------------------------------------------------------------

# Try out different fit methods:

##################
# fit.method = 1 #
##################
vfit = fit.variogram(variog, model=v,  # fitting the model with starting model
                           fit.sills = TRUE,
                           fit.ranges = TRUE,
                           fit.method = 1,
                           debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit
plot(variog, vfit, cutoff=1500)

# manually compute the SSEs (= loss function value)
gamma.mod = exp.sv(lag = variog$dist, nugget = vfit$psill[1], psill = vfit$psill[2], phi = vfit$range[2])



weig = variog$np                       # for fit.method = 1
sse1 = sum(weig*(variog$gamma - gamma.mod)^2)
sse1
# compare to
sse1.fit = print(attr(vfit,"SSErr")) # sum of squared errors acc. to gstat output

##################
# fit.method = 2 #
##################
vfit = fit.variogram(variog, model=v,  # fitting the model with starting model
                     fit.sills = TRUE,
                     fit.ranges = TRUE,
                     fit.method = 2,
                     debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit
plot(variog, vfit, cutoff=1500)

# manually compute the SSEs (= loss function value)
gamma.mod = exp.sv(lag = variog$dist, nugget = vfit$psill[1], psill = vfit$psill[2], phi = vfit$range[2])

weig = variog$np/variog$gamma^2  # if fit.method = 2
sse2 = sum(weig*(variog$gamma - gamma.mod)^2)
# compare to
sse2.fit = print(attr(vfit,"SSErr")) # sum of squared errors acc. to gstat output



##################
# fit.method = 6 #
##################
vfit = fit.variogram(variog, model=v,  # fitting the model with starting model
                     fit.sills = TRUE,
                     fit.ranges = TRUE,
                     fit.method = 6,
                     debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit
plot(variog, vfit, cutoff=1500)

# manually compute the SSEs (= loss function value)
gamma.mod = exp.sv(lag = variog$dist, nugget = vfit$psill[1], psill = vfit$psill[2], phi = vfit$range[2])

weig = rep(1, length(variog$np))       # if fit.method = 6
sse6 = sum(weig*(variog$gamma - gamma.mod)^2)
# compare to
sse6.fit = print(attr(vfit,"SSErr")) # sum of squared errors acc. to gstat output



##################
# fit.method = 7 #
##################
vfit = fit.variogram(variog, model=v,  # fitting the model with starting model
                     fit.sills = TRUE,
                     fit.ranges = TRUE,
                     fit.method = 7,
                     debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit
plot(variog, vfit, cutoff=1500)

# manually compute the SSEs (= loss function value)
gamma.mod = exp.sv(lag = variog$dist, nugget = vfit$psill[1], psill = vfit$psill[2], phi = vfit$range[2])

weig = variog$np/variog$dist^2   # if fit.method = 7
sse7 = sum(weig*(variog$gamma - gamma.mod)^2)
# compare to
sse7.fit = print(attr(vfit,"SSErr")) # sum of squared errors acc. to gstat output

##################
# sse comparison #
##################

sses = matrix(c(sse1, sse2, sse6, sse7, sse1.fit, sse2.fit, sse6.fit, sse7.fit), ncol = 2, byrow = F)
sses = data.frame(sses)
colnames(sses) = c("manually calculated SSE", "SSE acc. to fit.variogram output")

sses














