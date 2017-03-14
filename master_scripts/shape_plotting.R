library(scales)
evercol <- alpha("forestgreen", .4)
decidcol <- alpha("darkgoldenrod3", .4)

shape <- read.csv("data/tree_shape.csv")


plot(branchper30 ~ volume, data=shape,col=c(decidcol,evercol)[leaf_type], pch=16)
plot(log10(crown_spread) ~ log10(volume), data=shape, col=c(decidcol,evercol)[leaf_type], pch=16)
