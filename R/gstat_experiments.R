### experiment with gstat package

install.packages("gstat")

str(birth)
coordinates(birth) = ~x+y
v = gstat::variogram(object = birthweight ~ 1, data = birth, cutoff = 500, width = 50)

plot(v)
summary(v)








# ------------------------------------------
# geor estimation for comparison reasons


v01 = vario.mod(data = birth) # variomod fct does not work ...why?



library(sp)
data(meuse)


head(meuse)
class(meuse)
coordinates(meuse) = ~x+y
gstat::variogram(log(zinc)~1, meuse)
