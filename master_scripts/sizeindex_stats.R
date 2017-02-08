
library(visreg)
library(nlme)

# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
shape <- read.csv("data/tree_shape.csv")
standard <- read.csv("data/container_assessment.csv")

# model size index (H*D) with climate -------------------------------------


si_mod <- lm(sizeindex ~ MAT*volume, data=si_clim)


visreg(si_mod, "volume", by = "MAT", overlay=TRUE)
