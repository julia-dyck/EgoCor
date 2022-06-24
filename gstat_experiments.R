


# ------------------------------------------
# geor estimation for comparison reasons
library(EgoCor)

v.egocor = vario.mod(data = birth, max.dist = 500, nbins = 10, shinyresults = F, windowplots = T)
variog.egocor = v.egocor$variog.list[[1]]
variog.egocor

vmod.egocor = v.egocor$vmod.list[[1]]
vmod.egocor


### experiment with gstat package

#install.packages("gstat")
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
                      fit.method = 2,
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


### graphical comparison of emp. sv
plot(variog.egocor$uvec, variog.egocor$v, xlim = c(0,520), ylim = c(0, 300000))
abline(v = variog.egocor$uvec, col = 5, lty = 3) # middle of lag acc. to geor estimate
abline(v=variog.gstat$dist, col = 3, lty = 2) # middle of lag acc. to gstat estimate
plot(vmod.egocor)



plot(variog.gstat)


### graphical comparison of the sv model

# geor
plot(variog.egocor$uvec, variog.egocor$v, xlim = c(0,520), ylim = c(0, 300000))
graphics::lines(vmod.egocor)

curve(vmod.egocor$nugget + vmod.egocor$cov.pars[1]*(1 - exp(-x/vmod.egocor$cov.pars[2])),
      from = 0, to = 500, ylim = c(0,230000))
abline(v = -log(0.05*(1+(vmod.egocor$nugget/vmod.egocor$cov.pars[1])))*vmod.egocor$cov.pars[2], col = "blue")
abline(v = geoR::practicalRange("exp", vmod.egocor$cov.pars[2]), col = "red")
abline(h = 0.95*(vmod.egocor$nugget + vmod.egocor$cov.pars[1]), col = "blue")




# gstat
abline(v = -log(0.05)*vfit.gstat$range[2], col = "blue")
abline(h = 0.95*sum(vfit.gstat$psill), col = "blue")

plot(variog.gstat)
plot(variog.gstat, vfit.gstat, cutoff=1000)


curve(vfit.gstat$psill[1] + vfit.gstat$psill[2]*(1 - exp(-x/vfit.gstat$range[2])),
      from = 0, to = 500)

abline(v = -log(0.05*(1+(vfit.gstat$psill[1]/(vfit.gstat$psill[2]))))*vfit.gstat$range[2], col = "blue")
abline(h = 0.95*sum(vfit.gstat$psill), col = "blue")
prac.range.value = -log(0.05*(1+(vfit.gstat$psill[1]/(vfit.gstat$psill[2]))))*vfit.gstat$range[2]
prac.range.value
