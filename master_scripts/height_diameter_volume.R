
## full data plots of height and diameter vs container volume and H~D
source("functions/functions.R")
library(doBy)
library(magicaxis)
library(RColorBrewer)
library(scales)

# read data ---------------------------------------------------------------
variety <- read.csv("data/species_list.csv")
genusspecies <- read.csv("data/genusspecies_list.csv")
si_means_clim <- read.csv("data/si_means_climate.csv") 
si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")

#trim to range of data
si_range <- si_clim[si_clim$volume >= 18,]
si_mean_range <- si_means_clim[si_means_clim$volume >= 18,]

si_mean_range$logvol <- with(si_mean_range, log10(volume))
si_mean_range$logH <- with(si_mean_range, log10(height_m.mean))
si_mean_range$logD <- with(si_mean_range, log10(calliper300.mean))

# two panels H and D ------------------------------------------------------
everdat <- si_mean_range[si_mean_range$leaf_type=="evergreen",]
deciddat <- si_mean_range[si_mean_range$leaf_type=="deciduous",]

#plot bits 

evercol <- alpha("forestgreen", .7)
decidcol <- alpha("goldenrod1", .7)

#height and diameter models for CI
h_mod_evg <- lm(logH ~ logvol, data=everdat)
h_mod_dec <- lm(logH ~ logvol, data=deciddat)

d_mod_evg <- lm(logD ~ logvol, data=everdat)
d_mod_dec <- lm(logD ~ logvol, data=deciddat)

#plot rows randomly
 #windows(8,10)

png(filename = "heightdiam.png", width = 7, height = 7, units = "in", res= 600)

par(cex.axis=1, cex.lab=1, las=1,mgp=c(3.5,1,0),mfrow=c(2,1))

# height
par(mar=c(0,5,2,2),cex.axis=1, cex.lab=1,las=0,mgp=c(3,1,0))
with(si_mean_range[sample(nrow(si_mean_range)),],
plot(logH ~ jitter(logvol, .5), xlab="Container volume (L)", 
     ylab="Height (m)", xlim=c(1,3.7),ylim=c(.01,1),
     axes=FALSE, cex=1.25, col=c(decidcol,evercol)[leaf_type], pch=16))

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE, labels=c(FALSE, TRUE))

legend("topleft", c("Deciduous", "Evergreen"), col=c("goldenrod1","forestgreen"),inset=.03, 
       cex=1, bty='n', pch=16, pt.cex=1 )

ablineclip(h_mod_evg, col="forestgreen",lwd=2, x1=min(everdat$logvol), 
           x2=max(everdat$logvol), y1=min(everdat$logH), 
           y2=max(everdat$logH))
ablineclip(h_mod_dec, col="darkgoldenrod3",lwd=2,x1=min(deciddat$logvol), 
           x2=max(deciddat$logvol), y1=min(deciddat$logH), 
           y2=max(deciddat$logH))

box()
text("A", x=3.65, y=.95, cex=1.25)
#diameter

par(mar=c(5,5,0,2))
with(si_mean_range[sample(nrow(si_mean_range)),],
plot(logD ~ jitter(logvol, .5), xlab="Container volume (L)", 
     ylab="Diameter @ 30cm (mm)", xlim=c(1,3.7),ylim=c(.75,2.7),
     axes=FALSE, cex=1.25, col=c(decidcol,evercol)[leaf_type], pch=16))

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

ablineclip(d_mod_evg, col="forestgreen",lwd=2, x1=min(everdat$logvol), 
           x2=max(everdat$logvol), y1=min(everdat$logD), 
          y2=max(everdat$logD))
ablineclip(d_mod_dec, col="darkgoldenrod3",lwd=2,x1=min(deciddat$logvol), 
           x2=max(deciddat$logvol), y1=min(deciddat$logD), 
           y2=max(deciddat$logD))

box()
text("B", x=3.65, y=2.6, cex=1.25)
dev.off()
