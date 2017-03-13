
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

#plot bits -----------------------------------------------------

silab <- expression(Size~index~range~~(calliper~x~height))

palette(c(alpha("forestgreen", .5), alpha("goldenrod1",.5), alpha("navyblue", .5), alpha("firebrick2", .5),
          alpha("darkorchid3", .5), alpha("deepskyblue1", .5)))

crlab <- as.vector(unique(si_means_clim$climate_region))


library(ggsci)
mypal <- pal_lancet("lanonc", alpha = 0.4)(5)
palette(mypal) 

evercol <- alpha("forestgreen", .4)
decidcol <- alpha("darkgoldenrod3", .4)

# hieght -------------------------------------------

si_mean_range$logvol <- with(si_mean_range, log10(volume))
si_mean_range$logH <- with(si_mean_range, log10(height_m.mean))

h_mod_evg <- lm(logH ~ logvol, data=si_mean_range[si_mean_range$leaf_type=="evergreen",])
h_mod_dec <- lm(logH ~ logvol, data=si_mean_range[si_mean_range$leaf_type=="deciduous",])

windows()
par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))
plot(logH ~ jitter(logvol, .5), data=si_mean_range, xlab="Container volume (L)", 
     ylab="Height (m)", xlim=c(1,3.7),ylim=c(.01,1),
     axes=FALSE, cex=1.25, col=c(decidcol,evercol)[leaf_type], pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("topleft", c("Deciduous", "Evergreen"), col=c("goldenrod1","forestgreen"),inset=.03, 
       cex=1, bty='n', pch=16, pt.cex=1 )

predline(h_mod_evg, col="forestgreen",lwd=2)
predline(h_mod_dec, col="darkgoldenrod3",lwd=2)

box()


# diameter -------------------------------------------

si_mean_range$logD <- with(si_mean_range, log10(calliper300.mean))

d_mod_evg <- lm(logD ~ logvol, data=si_mean_range[si_mean_range$leaf_type=="evergreen",])
d_mod_dec <- lm(logD ~ logvol, data=si_mean_range[si_mean_range$leaf_type=="deciduous",])

windows()
par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))
plot(logD ~ jitter(logvol, .5), data=si_mean_range, xlab="Container volume (L)", 
     ylab="Diameter @ 30cm (mm)", xlim=c(1,3.7),ylim=c(.75,2.5),
     axes=FALSE, cex=1.25, col=c(decidcol,evercol)[leaf_type], pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("topleft", c("Deciduous", "Evergreen"), col=c("darkgoldenrod1","forestgreen"),inset=.03, 
       cex=1, bty='n', pch=16, pt.cex=1 )

predline(d_mod_evg, col="forestgreen",lwd=2)
predline(d_mod_dec, col="darkgoldenrod3",lwd=2)

box()

##diameter and height ----------------------------------------------------------------

hd_mod_evg <- lm(logH ~ logD, data=si_range[si_range$leaf_type=="evergreen",])
hd_mod_dec <- lm(logH ~ logD, data=si_range[si_range$leaf_type=="deciduous",])


windows()
par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))
plot(logH ~ jitter(logvol, .5), data=si_range, ylab="Height (m)", 
     xlab="Diameter @ 30cm (mm)", 
     axes=FALSE, cex=1.25, col=c(decidcol,evercol)[leaf_type], pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)


predline(hd_mod_evg, col=evercol,lwd=2)
predline(hd_mod_dec, col="darkgoldenrod3",lwd=2)

# par(new=TRUE)
# smoothplot(logD, logH, leaf_type,data=si_range, kgam=1,ylab="", xlab="",
#            linecol=c(decidcol,evercol),pch="")


legend("topleft", c("Deciduous", "Evergreen"), col=c("goldenrod1","forestgreen"),inset=.03, 
       cex=1, bty='n', pch=16, pt.cex=1 )

box()
