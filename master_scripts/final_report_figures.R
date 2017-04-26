library(scales)
library(doBy)

# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")

#means data
si_means <- read.csv("data/si_means_climate.csv")

# trim data so AS2303 range 

si_range <- si_clim[si_clim$volume >= 18,]
si_means_range <- si_means[si_means$volume >= 18,]

fitcon <- lm(logSI ~ logvol, data=si_range)
#use fit con 1 if standizing to the specified AS2303 criteria

#standardize SI by container volume, using intercept from fitted model
si_range$logSI_stand <- with(si_range, logSI / (logvol^coef(fitcon)[[2]]))

passfail <- read.csv("data/si_passfail.csv")

# si vs climate -----------------------------------------------------------

windows (7,7)

png(filename = "output/ssi_climate.png", width = 7, height = 7, units = "in", res= 600)
par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1,las=0,mgp=c(3,1,0))
boxplot(logSI_stand ~ climate_region, data=si_range, outline=FALSE, ylab="Standarized size index", names=FALSE, ylim=c(.6, 1.8))
mtext(side=1, at=1:6, text=c("New South\nWales", "Northern \nTerritory","Queensland \n ","South \nAustralia",
                         "Victoria \n ","Western \nAustralia"), line=2)
dev.off()


# passfail table ----------------------------------------------------------

pf2 <- passfail[, c("toobig", "toosmall", "balanced", "volume")]
library(doBy)
big <- summaryBy(toobig ~ volume, data=pf2[pf2$toobig=="big",], FUN=length, keep.names = TRUE)
small <- summaryBy(toosmall ~ volume, data=pf2[pf2$toosmall=="small",], FUN=length, keep.names = TRUE)
norm <- summaryBy(balanced ~ volume, data=pf2[pf2$balanced=="pass",], FUN=length, keep.names = TRUE)

pf3<- Reduce(function(...)merge(..., all=TRUE),list(big, small, norm))
pf3$total <- with(pf3, toosmall+toobig+balanced)
pf3$percnorm <- with(pf3, 1-(total-balanced)/total)
pf3$perchigh <- with(pf3, 1-(total-toobig)/total)
pf3$perclow <- with(pf3, 1-(total-toosmall)/total)

table("volume, passfail")

trees <- summaryBy(toobig ~ volume, FUN=length, data=passfail)

test <- passfail[passfail$volume==45,]
norm <-test[test$balanced=="pass",]
big<- test[test$toobig=="big",]
small<-test[test$toosmall=="small",]


# top trees  --------------------------------------------------------------
toptrees <- summaryBy(sizeindex +nursery +climate_region ~ species, FUN=function(x) 
  length(unique(x)), data=passfail, keep.names = TRUE)

toptrees <- toptrees[order(-toptrees[,2]),]
