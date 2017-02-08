

shape <- read.csv("data/tree_shape.csv")

si_clim <- read.csv("data/si_climate.csv")
#if we want to merge, then need to take the first 10 trees (or up to 10)

plot(branchper30 ~ climate_region, data=shape)
plot(crown_spread ~ climate_region, data=shape)
