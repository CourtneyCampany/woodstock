##need to investigate patterns of SI stats on individual components
##we know that more variation exists with height so we start there

library(doBy)
library(lme4)
library(visreg)
library(car)
library(gplots)
library(Hmisc)

# read data ---------------------------------------------------------------
tree_stats <- read.csv("data/tree_stats.csv")

# full models as with SI--------------------------------------------------------------
#height
lmeH_full <- lmer(logH ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
                  + (1|nursery/species), data=tree_stats)
library(arm)
display(lmeH_full)
summary(lmeH_full)
Anova(lmeH_full)
#diameter
lmeD_full <- lmer(logD ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
                  + (1|nursery/species), data=tree_stats)
display(lmeD_full)
summary(lmeD_full)
Anova(lmeD_full)


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

library(MuMIn)
r.squaredGLMM(lmeH0)
r.squaredGLMM(lmeH4)
r.squaredGLMM(lmeH6)
r.squaredGLMM(lmeH6)
Anova(lmeH4)
Anova(lmeH6)
Anova(lmeH7)


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


# conclusions -------------------------------------------------------------

#1: both hieght and diameter are greater in smaller sized deciduous trees than evergreen
#2: crown spread is affects both height and diameter (probably related more to leaf type)
#3: branchiness appears to affet height*volume but not really clear
#4: natives appear to have greater diameter than non-natives at larger sizes (>500)

