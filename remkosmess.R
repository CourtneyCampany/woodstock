

lme_full <- lmer(logSI ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
                 + (1|nursery/species), data=tree_stats)

library(randomForest)

tree_stats$logSI_stand <- with(tree_stats, logSI / (logvol^coef(fitcon)[[2]]))
tree_stats$log_crownshape <- with(tree_stats, log(crown_spread/height_m))

tree_stats_2 <- tree_stats[complete.cases(tree_stats),]



rf <- randomForest(logSI ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30,
                   data=tree_stats_2)



con <- read.csv("data/container_assessment.csv")
fitcon <- lm(log(max_size_index) ~ log(container_volume), data=con)


library(nlshelper)
l <- loess(logSI ~ logvol, data=tree_stats, span=0.5)
plot_loess(l)

rf2 <- randomForest(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type+log_crownshape+branchper30,
                    data=tree_stats_2)
varImpPlot(rf2)



lme_full <- lmer(logSI ~ logvol+origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
                 + (1|nursery/species), data=tree_stats)

tree_stats_3 <- subset(tree_stats_2, select=c(logSI, logvol,MAT, MAP, origin, nursery, species))

scale_fun <- function(x)(x - min(x))/(max(x) - min(x))
for(i in 1:4)tree_stats_3[,i] <- scale_fun(tree_stats_3[,i])

library(lmerTest)
lmetry <- lmer(logSI ~ logvol + origin + MAT + MAP
               + (1|nursery/species), data=tree_stats_3)
summary(lmetry)




lme_full_2 <- lmer(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type+crown_spread+branchper30
                 + (1|nursery/species), data=tree_stats)

VarCorr(lme_full_2)
r.squaredGLMM(lme_full_2)
Anova(lme_full_2, test="F")
summary(lme_full_2)


lme_full_3 <- lmer(logSI_stand ~ origin+MAT+MAP+climate_region+leaf_type+
                   + (1|nursery/species), data=tree_stats)
VarCorr(lme_full_3)
r.squaredGLMM(lme_full_3)
Anova(lme_full_3, test="F")

lme_full_4 <- update(lme_full_3, . ~ . - leaf_type - climate_region)

