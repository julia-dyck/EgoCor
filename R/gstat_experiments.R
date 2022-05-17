


# ------------------------------------------
# geor estimation for comparison reasons
library(EgoCor)

v.egocor = vario.mod(data = birth, max.dist = 500, nbins = 10, shinyresults = F, windowplots = T)
variog.egocor = v.egocor$variog.list[[1]]
variog.egocor

vmod.egocor = v.egocor$vmod.list
vmod.egocor


### experiment with gstat package

install.packages("gstat")
library(gstat)

str(birth)
birth.reformatted = birth
sp::coordinates(birth.reformatted) = ~x+y
v = gstat::variogram(object = birthweight ~ 1, data = birth.reformatted, cutoff = 500, width = 50)



# graphical comparison of emp. sv
plot(variog.egocor$uvec, variog.egocor$v, xlim = c(0,520), ylim = c(0, 300000))
abline(v = variog.egocor$uvec, col = 5, lty = 3) # middle of lag acc. to geor estimate
abline(v=v$dist, col = 3, lty = 2) # middle of lag acc. to gstat estimate

plot(v)
# cannot add lines to the gstat plot





# -------------------------------
## example for input values
library(sp)
data(meuse)


head(meuse)
class(meuse)
coordinates(meuse) = ~x+y
gstat::variogram(log(zinc)~1, meuse)
