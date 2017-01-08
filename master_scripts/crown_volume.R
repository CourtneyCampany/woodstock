##Should we calculate crown volume ???

##Volume of a cylinder = π*h*r^2
##Volume of a Sphere = 4/3π*r^3
##Volume of a cone = 1/3π*r^2

shape <- read.csv("raw_data/crown_shape.csv")



x$crownvolume <- with(x, (pi* (maxbranchradius)^2 * (crownlength/3))/1000000) #units in cm