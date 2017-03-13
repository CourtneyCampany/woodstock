
# tree shape data analysis ------------------------------------------------

shape <- read.csv('data/tree_shape.csv')
shape$internode <- with(shape, branchper30/30)


##means dataframe
library(doBy)
shape_agg <- summaryBy(crown_spread + branchper30 + crown_ratio~
                       species+volume+origin+leaf_type+nursery+climate_region, 
                       FUN=mean, keep.names=TRUE, data=shape)

library(scales)
evercol <- alpha("forestgreen", .5)
decidcol <- alpha("goldenrod1", .5)

plot(branchper30 ~ volume, data=shape_agg, col=c(decidcol,evercol)[leaf_type], pch=16)
plot(crown_spread ~ volume, data=shape_agg, col=c(decidcol,evercol)[leaf_type], pch=16)
plot(crown_ratio ~ volume, data=shape_agg, col=c(decidcol,evercol)[leaf_type], pch=16)

library(lme4)
library(visreg)
lme_br <- lmer(crown_spread ~ volume*leaf_type + (1|nursery/species), data=shape)
visreg(lme_br, "volume", by="leaf_type", overlay=TRUE)



##merge means si and shape datasets

si <- read.csv("data/si_means_climate.csv")
  si <- si[, c(1:17, 42:43)]

trees <- merge(si, shape_agg, all = TRUE)  

#a few dont have shape, mostly winter bareroot that have dropped branches
#arborwest = jacaranda35, platnatus 45
#alpine euc_siderox_rosea missing

trees_clean <- trees[-c(120, 137,50),]

write.csv(trees_clean, "data/trees_clean.csv", row.names = FALSE)

