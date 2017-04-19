library(doBy)
library(lme4)
library(visreg)
library(car)
library(gplots)
library(Hmisc)
library(lmerTest)

#read data ---------------------------------------------------------------
tree_stats <- read.csv("data/tree_stats.csv")
  tree_stats$crown_shape <- with(tree_stats, crown_spread/crown_length)
  
  # si_assess <- read.csv("data/container_assessment.csv")
  # fitcon <- lm(log10(max_size_index) ~ log10(container_volume), data=si_assess)
  fitcon2 <- lm(logSI ~ logvol, data=tree_stats)
  #use fit con 1 if standizing to the specified AS2303 criteria
  
  #standardize SI by container volume, using intercept from fitted model
  tree_stats$logSI_stand <- with(tree_stats, logSI / (logvol^coef(fitcon2)[[2]]))
  #standarize other crown variables
  fitspread <- lm(log10(crown_spread) ~ logvol, data=tree_stats)
  tree_stats$crown_stand <- with(tree_stats, log10(crown_spread) / (logvol^coef(fitspread)[[2]]))
  fitslender<- lm(log10(slenderness) ~ logvol, data=tree_stats)
  tree_stats$slender_stand <- with(tree_stats, log10(slenderness) / (logvol^coef(fitslender)[[2]]))
  
#remove missing values
tree_stats_2 <- tree_stats[complete.cases(tree_stats),]
  

#full model stats----------------------------------------------------------
lme_full <- lmer(logSI ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30+crown_shape
                   + (1|nursery/species), data=tree_stats)
  summary(lme_full)
  #leaftype/origin are most important as with volume
  #the other sig factors are most likely related to tree type
  #test this later by dropping them and checking R2
  
nullmod <- lmer(logSI ~ 1 + (1|species), data=tree_stats)  # ~20% of the variance explained at species level
nullmod2 <- lmer(logSI ~ 1 + (1|nursery/species), data=tree_stats) # do I add variance terms?
  
#contributions of random effects to the null model

lme0 <- lmer(logSI ~ logvol + (1|nursery/species), data=tree_stats)
summary(lme0)
visreg(lme0)
#obvioulsy massive effect of volume on size index, which is not super important.


##standardize SI by tree container tize and remove volume from analysis---------------------------

lme_full_stnd <- lmer(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30+crown_shape+slenderness
                   + (1|nursery/species), data=tree_stats)

VarCorr(lme_full_stnd)
library(MuMIn)
r.squaredGLMM(lme_full_stnd)
Anova(lme_full_stnd, test="F")
summary(lme_full_stnd)
#the variation due to container volume has been removed, which is good.  

#1:Then we see that leaftype is important and apparently all the shape parameters
lme_noshape <- update(lme_full_stnd, . ~ . - crown_spread) #- crown_spread - branchper30
r.squaredGLMM(lme_noshape)
#individual testing shows crownspread doesnt affect R2 much and neither does branchper 30 
#later test whether these are simply a covariate of leaftype (expected)


#2: what is the impact of tree form on the R2 values
lme_noform <- update(lme_full_stnd, . ~ . -crown_shape) #-slenderness - crownshape
r.squaredGLMM(lme_noform)
# slenderness has very little impact on R2 but crown shape does
#later test whether crownshape is also a convariate of leaftype (possible)


#3: #re-run the model with just the main parameters that we have relate to the central question 
lme_full_stnd2 <- lmer(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type + (1|nursery/species), data=tree_stats)
VarCorr(lme_full_stnd2)
#equal contribution of species and species within nursery
#RESULT: nursery effect is larger than species within nursery effect, 
#meaning that exploring species or variety level grouping of trees would not be helpful
r.squaredGLMM(lme_full_stnd2)
#nursery and species account for more variation tham climate or tree functional type
Anova(lme_full_stnd2, test="F")
#leaftype is most important, no significant effect of growing region or mean climate variables
lme_noorgin <- update(lme_full_stnd2, . ~ . -origin) #-slenderness - crownshape
r.squaredGLMM(lme_noorgin)
#dropping origin results in a minor drop on R2 as there is not compete overlap between natives and tree type
#keep it in the model and report why not as strong (but obviously still significant)

#deciduous or evergreen------------------------------------------------------------------------------------------
leafmod <- lmer(logSI ~ logvol*leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume >=1000,])
visreg(leafmod, "logvol", by="leaf_type", overlay=TRUE)

#4. test the correlation of shape parameters with leaf type or if they are independent------------------------
#???do deciduous trees have bigger crown spread?
leafmod1 <- lmer(crown_spread ~ leaf_type + (1|nursery/species), data=tree_stats)
  visreg(leafmod1, "leaf_type", overlay=TRUE)
  summary(leafmod1)
  Anova(leafmod1)
  #no for all data, yes if you define lower volumes (<=400)
  
  leafmod1b <- lmer(crown_spread ~ leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume <= 400,])
  Anova(leafmod1b, test="F") #lower spread in deciduous for smaller trees then higher for bigger trees
  visreg(leafmod1b, "leaf_type")
  
  leafmod1c <- lmer(crown_spread ~ logvol*leaf_type + (1|nursery/species), data=tree_stats)
  Anova(leafmod1c) #can see this with the interaction
  visreg(leafmod1c, xvar="logvol", by="leaf_type", overlay=TRUE)
  
  leafmod1d <- lmer(crown_stand ~ leaf_type + (1|nursery/species), data=tree_stats)
  Anova(leafmod1d) #removed tree size effect (logvol), do I want to do this?
  visreg(leafmod1d,"leaf_type")

  leafmod1e <- lmer(crown_spread ~ leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume > 400,])
  Anova(leafmod1e, test="F") #same at higher volumes
  visreg(leafmod1e,"leaf_type")
  
  leafmod1f <- lmer(crown_spread ~ height_m * leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume <= 400,])
  visreg(leafmod1f, xvar="height_m",by="leaf_type", overlay=TRUE)
  summary(leafmod1f)
  Anova(leafmod1f)
  
#???re deciduous less branchy then evergreen during early development???
leafmod2 <- lmer(branchper30 ~ leaf_type + (1|nursery/species), data=tree_stats)
  visreg(leafmod2, "leaf_type", overlay=TRUE)
  summary(leafmod)
  Anova(leafmod2)
  #2: yes, evergreens are more branchy 


#???is crown shape different between leaftypes???
leafmod3 <- lmer(crown_shape ~ leaf_type + (1|nursery/species), data=tree_stats)
  visreg(leafmod3, "leaf_type", overlay=TRUE)
  summary(leafmod3)
  Anova(leafmod3)
  #Yes, deciduous and evergreen trees have different shape (obviously there is a crown spread effect, what about crown height)
  leafmod4 <- lmer(crown_length ~ leaf_type + (1|nursery/species), data=tree_stats)
  visreg(leafmod4, "leaf_type", overlay=TRUE)
  Anova(leafmod4)
  #what about early development
  leafmod4b <- lmer(crown_length ~ leaf_type + (1|nursery/species), data=tree_stats[tree_stats$volume <= 400,])
  Anova(leafmod4b)
  #So the effect of crown shape arises from eary age differences in crown spread not crown length with tree type
  
#??? crown shape was different was trunk shape?  
stemmod <-lmer(slenderness ~ leaf_type * climate_region +(1|nursery/species), data=tree_stats)
visreg(stemmod, "leaf_type", by="climate_region",overlay=TRUE)
summary(stemmod)
Anova(stemmod)

stemmod2 <-lmer(slender_stand ~ leaf_type + (1|nursery/species), data=tree_stats)
visreg(stemmod2, "leaf_type")
summary(stemmod)
Anova(stemmod2)

stemmod3 <-lmer(slenderness ~ climate_region + (1|nursery/species), data=tree_stats)
visreg(stemmod3, "climate_region")
summary(stemmod3)
Anova(stemmod3)

##we are justified in dumping branhciness and crown shape from model as they are inherent in the leaftype effect
##interaction with container size and crown spread by leaftype, so drop it too
    
#5. Choose final model and drop parameters and compare R2
lme_final <- lmer(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type + (1|nursery), data=tree_stats)
  
  VarCorr(lme_final)
  summary(lme_final)
  #use this as the random error in report ()
  r.squaredGLMM(lme_final)
  Anova(lme_final, test="F")
  #r2=.1559

nullmod3 <- lmer(logSI_stand ~ 1 + (1|species), data=tree_stats) #23.6%
nullmod4 <- lmer(logSI_stand ~ 1 + (1|nursery/species), data=tree_stats) 
  r.squaredGLMM(nullmod5) #41% explained by nursery and species
nullmod5 <- lmer(logSI_stand ~ 1 + (1|nursery), data=tree_stats)  #22.6%

##### quantify variance of the fixed effects.................. 

##remove the smallest effect first, because each will be a new model and
#we want to have best estimates possible
lme_final1 <- update(lme_final, . ~ . - MAT)
  r.squaredGLMM(lme_final1)  
  #r2=.1547 (<1% explainede my MAT)

lme_final2 <- update(lme_final, . ~ . -MAT - MAP )
  r.squaredGLMM(lme_final2)
  #r2=.1449 (6.3% explained by MAP)
  
lme_final3 <- update(lme_final, . ~ . -MAT - MAP - climate_region)
  r.squaredGLMM(lme_final3)
  #r2=.0751 (44.7% explained by climate region)
  
lme_final4 <- update(lme_final, . ~ .  -MAT - MAP - climate_region - origin)
  r.squaredGLMM(lme_final4)
  #r2=.057 this is the leaf_type effect (36.5% explained by origin)
  #r2= mod4-3 = is the origin effect (11.6%)
  

##take out origin?
  
#6. Variable importance for the chosen final model---------------------------------
# library(randomForest)
# 
# rf2 <- randomForest(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type,
#                     data=tree_stats_2)
# varImpPlot(rf2)


# library(nlshelper)
# l <- loess(logSI ~ logvol, data=tree_stats, span=0.5)
# plot_loess(l)

# tree_stats_3 <- subset(tree_stats_2, select=c(logSI, logvol,MAT, MAP, origin, nursery, species))
# 
# #need to scale continuous fixed effects, categorical already become (0-1)
# scale_fun <- function(x)(x - min(x))/(max(x) - min(x))
# 
# for(i in 1:4)tree_stats_3[,i] <- scale_fun(tree_stats_3[,i])
# ##this isnt working
# 
# library(lmerTest)
# lmetry <- lmer(logSI ~ logvol + origin + MAT + MAP + (1|nursery/species), data=tree_stats_3)
# summary(lmetry)



