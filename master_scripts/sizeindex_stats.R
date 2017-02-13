

# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
si_range <- si_clim[si_clim$volume >= 18,]

si_range <- transform(si_range,
                      ID = paste(nursery, batch_id, sep="_"))

library(doBy)
sia <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE,
                 id= ~ species+origin+leaf_type+nursery+climate_region,
                 data=si_range)


# mixed effect modelling --------------------------------------------------

library(lme4)
library(visreg)
library(car)

lme0 <- lmer(logSI ~ logvol + (1|nursery/species), data=sia)

lme1 <- lmer(logSI ~ logvol + (1|species), data=sia)

lme2 <- lmer(logSI ~ logvol*MAT + (1|nursery/species), data=sia)
visreg(lme2, "logvol", by="MAT", overlay=TRUE)

sia2 <- subset(sia, volume < 550)

lme3 <- lmer(logSI ~ logvol*climate_region + (1|nursery/species), data=sia2)
visreg(lme3, "logvol", by="climate_region", overlay=T)


lme4 <- lmer(logSI ~ logvol*leaf_type + (1|nursery/species), data=sia)
visreg(lme4, "logvol", by="leaf_type", overlay=TRUE)

lme5 <- lmer(logSI ~ logvol*origin + (1|nursery/species), data=sia)
visreg(lme5, "logvol", by="origin", overlay=TRUE)
