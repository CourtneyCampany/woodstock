library(doBy)
library(lme4)
library(visreg)
library(car)
library(gplots)
library(Hmisc)

# read data ---------------------------------------------------------------
tree_stats <- read.csv("data/tree_stats.csv")

# Size Index stats ------------------------------------------------------
nullmod <- lmer(logSI ~ 1 + (1|species), data=tree_stats)  # ~20% of the variance explained at species level
nullmod2 <- lmer(logSI ~ 1 + (1|nursery/species), data=tree_stats) # do I add variance terms?

###should nursery be included as a fixed effect to test differences in nursery practices (in the absense of fert/irrig data)?
###log data or raw?
###stepwise selection for all fixed effects?
###interaction terms
###variance paritioning on best model

lme0 <- lmer(logSI ~ logvol + (1|nursery/species), data=tree_stats)
summary(lme0)
visreg(lme0)

lme1 <- lmer(logSI ~ logvol + (1|species), data=tree_stats)
visreg(lme1)


# individual fixed effects ------------------------------------------------

#mean annual temp
lme2 <- lmer(logSI ~ logvol*MAT + (1|nursery/species), data=tree_stats)
visreg(lme2, "logvol", by="MAT", overlay=TRUE)

#mean annual precip
lme2a <- lmer(logSI ~ logvol*MAP + (1|nursery/species), data=tree_stats)
visreg(lme2a, "logvol", by="MAP", overlay=TRUE)

#climate region
lme3 <- lmer(logSI ~ logvol*climate_region + (1|nursery/species), data=tree_stats)
visreg(lme3, "logvol", by="climate_region", overlay=T)
contrasts(tree_stats$climate_region)

#deciduous or evergreen
lme4 <- lmer(logSI ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
visreg(lme4, "logvol", by="leaf_type", overlay=TRUE)

#native or nonnative
lme5 <- lmer(logSI ~ logvol*origin + (1|nursery/species), data=tree_stats)
visreg(lme5, "logvol", by="origin", overlay=TRUE)

#canopy spread
lme6 <- lmer(logSI ~ logvol*crown_spread + (1|nursery/species), data=tree_stats)
visreg(lme6, "logvol", by="crown_spread", overlay=TRUE)

#branchiness
lme7 <- lmer(logSI ~ logvol*branchper30 + (1|nursery/species), data=tree_stats)
visreg(lme7, "logvol", by="branchper30", overlay=TRUE)



# full model --------------------------------------------------------------
lme_full <- lmer(logSI ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
             + (1|nursery/species), data=tree_stats)

library(arm)
display(lme_full)
summary(lme_full)
Anova(lme_full)
AIC(lme_full)

#step on full model
step(lme_full)


bestmod <- lme4::lmer(formula = logSI ~ logvol + origin + leaf_type + crown_spread + 
                        branchper30 + (1 | nursery/species), data = tree_stats)
                      #contrasts = list(origin = "contr.SAS", leaf_type = "contr.SAS"))
AIC(bestmod)
r.squaredGLMM(bestmod)

# r <- ranef(lme0)
# windows()
# par(mar=c(12,4,2,2), las=2)
# barplot(sort(r$nursery[[1]]),
#         names.arg=rownames(r$nursery))



# R2 ------------------------------------------------------
library(MuMIn)

#nursery and species as random effects
r.squaredGLMM(lme0)
#only species as random effect
r.squaredGLMM(lme1)

#SI by volume and MAT
r.squaredGLMM(lme2)
#SI by volume and MAP
r.squaredGLMM(lme2a)
 ##neither climate variable contribute vary much

#SI by volume and region
r.squaredGLMM(lme3)
  ##climate region doesnt alter much (possible dph and WA)

#SI by tree type
r.squaredGLMM(lme4)
Anova(lme4)
 ##deciduous trees bigger in trees less <600L 

r.squaredGLMM(lme5)
Anova(lme5) #sig in full model

#SI by crown structure
r.squaredGLMM(lme6)
Anova(lme6)
  ##Tree with large crows are bigger
r.squaredGLMM(lme7)
Anova(lme7)
  ##appears to also be affect of branchiness (less branches bigger in small containers)



# questions to address ----------------------------------------------------

#1) do deciduous trees have bigger crowns, during early development

leafmod <- lmer(crown_spread ~ leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafmod, "leaf_type", overlay=TRUE)
summary(leafmod)
Anova(leafmod)
#1: no for all data, yes if you define lower volumes (<=400)

#2) are deciduous less branchy then evergreen during early development
leafmod2 <- lmer(branchper30 ~ leaf_type + (1|nursery/species), data=tree_stats)
visreg(leafmod2, "leaf_type", overlay=TRUE)
summary(leafmod)
Anova(leafmod2)
#2: yes 

#3) when are decidous bigger during production
leafmod3 <- lmer(logSI ~ logvol*leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume >300,])
visreg(leafmod3, "logvol", by="leaf_type", overlay=TRUE)
Anova(leafmod3)
#3: somewhere around 200L and above decid and evergreen are not different (really around 400L)


