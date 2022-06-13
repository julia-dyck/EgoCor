


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

head(birth)
str(birth)
birth.reformatted = birth
sp::coordinates(birth.reformatted) = ~x+y
head(birth.reformatted)
variog.gstat = gstat::variogram(object = birthweight ~ 1, data = birth.reformatted, cutoff = 500, width = 50)
variog.gstat$dist

# prep exponential model (with or without starting values for the fitting):
v <- vgm("Exp")                        # variogram model (without starting values)
v2 <- vgm(psill = var(birth$birthweight),  # plausible starting value for psill = sample variance
          model = "Exp",
          range = 500/3,     # plausible starting value for range: maxdist/3
          nugget = 0)      # plausible starting value for nugget



vfit.gstat = fit.variogram(variog.gstat, model=v2,  # fitting the model with starting model
                      fit.sills = TRUE,
                      fit.ranges = TRUE,
                      fit.method = 1,
                      debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
vfit.gstat
vfit.gstat[1,2] # Nugget
vfit.gstat[2,2] # Partial sill
vfit.gstat[2,3] # Shape
summary(vfit.gstat)
extractPar(vfit.gstat)

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




birth
vario.mod(birth, max.dist = c(500,600,700))
# konvergiert nur für max dist 500



data = birth

colnames(data)[1:2] = c("x", "y")
data.ge = data[,1:3]
#  data <- as.data.frame(data.frame(geoR::jitterDupCoords(data[,1:2],max=0.01),data[,3])) # was macht diese Zeile?
sp::coordinates(data.ge) = ~x+y
#-> list containing [[1]]variable
sample.var = stats::var(data.ge[[1]])

data.ge


est.variog = gstat::variogram(object = data.ge[[1]] ~ 1, data = data.ge, cutoff = 500, width = 500 / 13)
plot(est.variog)
dim(est.variog)[1]

vario = est.variog
ini.partial.sill <- sample.var # partial sill parameter of the exp. model (also called sigmasq)
ini.shape <- max(vario$dist)/3 # oder /4; shape parameter of the exp. model (also called phi)
ini.values <- c(ini.partial.sill, ini.shape)
v = gstat::vgm(psill = sample.var, model = "Exp", range = 500/3, nugget = 0)
exp.variogram.mod <- gstat::fit.variogram(vario, model = v,  # fitting the model with starting model
                                          fit.sills = TRUE,
                                          fit.ranges = TRUE,
                                          debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)

plot(vario, model = exp.variogram.mod, col = "#000000")

summary(exp.variogram.mod)

plot(vario$dist, vario$gamma)
curve(exp.variogram.mod$psill[1] + exp.variogram.mod$psill[2]*(1 - exp(-x/exp.variogram.mod$range[2])), add = T)
title("Hallo")





?variogram
v = variogram(birthweight ~ 1, birth.reformatted)
vg = fit.variogram(v, vgm(model = "Exp", psill = sample.var, range = max(v$dist)/3, nugget = 0))
vg
plot(v)
plot(vg, cutoff = 500)
?fit.variogram

library(sp)
demo(meuse, ask = FALSE, echo = FALSE) # load meuse data set
v = variogram(log(zinc)~1, meuse)
v.fit = fit.variogram(v, vgm("Exp"))
v.fit

l = list(vario, v)
l[[1]]$dist

plt = plot(l[[1]], model = l2[[1]], col = "#000000")
plt
l2 = list(vg, v.fit)

plot(l2[[1]])
plot(v.fit, cutoff = 4000)
points(v$dist, v$gamma)
v
plot(v, model = v.fit)

# Practical range formula
prac.range.exp.with.nugget = -log(0.05*(1+(exp.variogram.mod$psill[1]/(exp.variogram.mod$psill[2]))))*exp.variogram.mod$range[2]
prac.range.exp.with.nugget


# Visualisierung
curve(exp.variogram.mod$psill[1] + exp.variogram.mod$psill[2]*(1 - exp(-x/exp.variogram.mod$range[2])),
      from = 0, to = 500) # passe ggf den plotting bereich an

abline(v = -log(0.05*(1+(exp.variogram.mod$psill[1]/(exp.variogram.mod$psill[2]))))*exp.variogram.mod$range[2], col = "blue") # practical range (sollte die 95% linie schneiden)
abline(h = 0.95*sum(exp.variogram.mod$psill), col = "blue") # 95% vom gesamt sill



?axis

plt = plot(l[[1]], model = l2[[1]], xaxt = "n", yaxt = "n")
graphics::title(paste("Test", "Test"),
                adj = 0,
                cex.main = 0.8)
plt
graphics::axis(1, cex.axis = 0.8)
graphics::axis(2, cex.axis = 0.8)
