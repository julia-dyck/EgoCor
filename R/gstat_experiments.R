


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
                      fit.method = 7,
                      debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit.gstat



### graphical comparison of emp. sv
plot(variog.egocor$uvec, variog.egocor$v, xlim = c(0,520), ylim = c(0, 300000))
abline(v = variog.egocor$uvec, col = 5, lty = 3) # middle of lag acc. to geor estimate
abline(v=variog.gstat$dist, col = 3, lty = 2) # middle of lag acc. to gstat estimate

plot(variog.gstat)


### graphical comparison of the sv model
plot(variog.egocor$uvec, variog.egocor$v, xlim = c(0,520), ylim = c(0, 300000))
graphics::lines(vmod.egocor)

plot(variog.gstat)
plot(variog.gstat, vfit.gstat, cutoff=500)

