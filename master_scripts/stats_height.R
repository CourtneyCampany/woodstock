##need to investigate patterns of SI stats on individual components
##we know that more variation exists with height so we start there

library(doBy)
library(lme4)
library(visreg)
library(car)
library(gplots)
library(Hmisc)
library(MuMIn)
library(raster)

# read data ---------------------------------------------------------------
tree_stats <- read.csv("data/tree_stats.csv")

fitH <- lm(logH ~ logvol, data=tree_stats)
fitD <- lm(logD ~ logvol, data=tree_stats)
#use fit con 1 if standizing to the specified AS2303 criteria

#standardize SI by container volume, using intercept from fitted model
tree_stats$logH_stand <- with(tree_stats, logH / (logvol^coef(fitH)[[2]]))
tree_stats$logD_stand <- with(tree_stats, logD / (logvol^coef(fitD)[[2]]))

#remove missing values
tree_stats_2 <- tree_stats[complete.cases(tree_stats),]
cv(tree_stats_2$height_m)
cv(tree_stats_2$calliper300)
cv(tree_stats_2$logH_stand)
cv(tree_stats_2$logD_stand)

var(tree_stats_2$logD)
var(tree_stats_2$logH)

#cv for size index of different clases
cv(tree_stats_2[tree_stats_2$volume <200, "sizeindex"])
cv(tree_stats_2[tree_stats_2$volume >=400, "sizeindex"])

# full models as with SI--------------------------------------------------------------
#height
lmeH_full <- lmer(logH_stand ~ origin+MAT+MAP+climate_region+leaf_type
                  + (1|nursery/species), data=tree_stats)
library(arm)
display(lmeH_full)
summary(lmeH_full)
Anova(lmeH_full, test="F")
AICc(lmeH_full)
r.squaredGLMM(lmeH_full)
visreg(lmeH_full, "")

leafH <- lmer(logH_stand ~ leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafH, "leaf_type", overlay=TRUE)
leafH2 <- lmer(logH ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafH2, "leaf_type", by="logvol",overlay=TRUE)
Anova(leafH2, test="F")
leafH3 <- lmer(logH ~ logvol+ (1|nursery/species), data=tree_stats)
visreg(leafH3 )

#diameter
lmeD_full <- lmer(logD_stand ~ origin+MAT+MAP+climate_region+leaf_type
                  + (1|nursery/species), data=tree_stats)
display(lmeD_full)
summary(lmeD_full)
Anova(lmeD_full, test="F")
AICc(lmeD_full)
r.squaredGLMM(lmeD_full)

leafD <- lmer(logD_stand ~ leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafH, "leaf_type", overlay=TRUE)
Anova(leafD, test="F")

leafD2 <- lmer(logD ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafH2, "leaf_type", by="logvol",overlay=TRUE)
Anova(leafD2, test="F")

leafD3 <- lmer(logD_stand ~ origin + (1|nursery/species), data=tree_stats)
visreg(leafD3, "origin", overlay=TRUE)
Anova(leafD3, test="F")

# height parameters -------------------------------------------------------
lmeH0 <- lmer(logH ~ logvol + (1|nursery/species), data=tree_stats)

#leaf type
lmeH4 <- lmer(logH ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
visreg(lmeH4, "logvol", by="leaf_type", overlay=TRUE)
#canopy spread
lmeH6 <- lmer(logH ~ logvol*crown_spread + (1|nursery/species), data=tree_stats)
visreg(lmeH6, "logvol", by="crown_spread", overlay=TRUE)
#branchiness
lmeH7 <- lmer(logH ~ logvol*branchper30 + (1|nursery/species), data=tree_stats)
visreg(lmeH7, "logvol", by="branchper30", overlay=TRUE)

r.squaredGLMM(lmeH0)
r.squaredGLMM(lmeH4)
r.squaredGLMM(lmeH6)
r.squaredGLMM(lmeH6)
Anova(lmeH4)
Anova(lmeH6)
Anova(lmeH7)
#step on full model
library(lmerTest)
step(lmeH_full)

# calliper parameters -------------------------------------------------------
lmeD0 <- lmer(logD ~ logvol + (1|nursery/species), data=tree_stats)

#leaf type
lmeD4 <- lmer(logD ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
visreg(lmeD4, "logvol", by="leaf_type", overlay=TRUE)
#canopy spread
lmeD6 <- lmer(logD ~ logvol*crown_spread + (1|nursery/species), data=tree_stats)
visreg(lmeD6, "logvol", by="crown_spread", overlay=TRUE)
#origin
lmeD5 <- lmer(logD ~ logvol*origin + (1|nursery/species), data=tree_stats)
visreg(lmeD5, "logvol", by="origin", overlay=TRUE)

r.squaredGLMM(lmeD0)
r.squaredGLMM(lmeD4)
r.squaredGLMM(lmeD6)
r.squaredGLMM(lmeD5)
Anova(lmeD4)
Anova(lmeD6)
Anova(lmeD5)
#step on full model
step(lmeD_full)

# conclusions -------------------------------------------------------------

#1: both hieght and diameter are greater in smaller sized deciduous trees than evergreen
#2: crown spread is affects both height and diameter (probably related more to leaf type)
#3: branchiness appears to affet height*volume but not really clear
#4: natives appear to have greater diameter than non-natives at larger sizes (>500)

