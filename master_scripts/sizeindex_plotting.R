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

#plot bits -----------------------------------------------------

silab <- expression(Size~index~range~~(calliper~x~height))

palette(c(alpha("forestgreen", .5), alpha("goldenrod1",.5), alpha("navyblue", .5), alpha("firebrick2", .5),
          alpha("darkorchid3", .5), alpha("deepskyblue1", .5)))

crlab <- as.vector(unique(si_means_clim$climate_region))

svlab <- expression(Stem~volume~index~~(cm^3))

# trim data so AS2303 range --------------------------------------------------------------

si_range <- si_clim[si_clim$volume >= 18,]
si_mean_range <- si_means_clim[si_means_clim$volume >= 18,]

  si_range$stemvolume <- with(si_range, (height_m*100) * ((calliper300/10)^2))

#plot SI data -----------------------------------------------------------

##means
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(log10(sizeindex.mean) ~ log10(volume), data=si_mean_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1, col=alpha("forestgreen", .6), pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend(x=2.95, y=1.35 , crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


##all data
png(filename = "hia_meeting/img/allsi.png", width = 11, height = 8.5, units = "in", res= 600)

# windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, bg=alpha("olivedrab3", .5), pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1.25)
# legend(x=2.95, y=1.35 , crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()

dev.off()


##all data
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(logD ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab="Diameter @ 30cm (mm)", xlim=c(1,3.7),ylim=c(.75,2.75),
     axes=FALSE, cex=.6, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("bottomright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


# taper plotting ----------------------------------------------------------
si_range$taper <- with(si_range, height_m/calliper300)

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(log10(taper) ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab="Slenderness Index (H/D)", ylim=c(-2.5, -.1), xlim=c(.75,3.75),
     axes=FALSE, cex=.6, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("topright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


# stem volume -------------------------------------------------------------


##all data
png(filename = "hia_meeting/img/stemvolume.png", width = 11, height = 8.5, units = "in", res= 600)

par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))

plot(log10(stemvolume) ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=svlab, xlim=c(1,3.7),ylim=c(1,6),
     axes=FALSE, cex=1.25, bg=alpha("olivedrab3", .5), pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

dev.off()



# variation plotting ------------------------------------------------------

# plot(volume ~ sizeindex.mean, data=si_mean_range, ylab="Container volume (L)", xlab = silab, type='n')
# segments(x0=23.8, y0=100, x1=346.4, y1=100, col="royalblue", lwd=2)
# segments(x0=96.36, y0=1000, x1=1239.1, y1=1000, col="royalblue", lwd=2)
# segments(x0=6.52, y0=20, x1=117.36, y1=20, col="royalblue", lwd=2)
# segments(x0=12.09, y0=45, x1=12.09, y1=45, col="royalblue", lwd=2)
# segments(x0=98.69845, y0=400, x1=12.09, y1=400, col="royalblue", lwd=2)
# segments(x0=654.003, y0=1500, x1=12.09, y1=1500, col="royalblue", lwd=2)
# segments(x0=921.8318, y0=2500, x1=1690.2100, y1=2500, col="royalblue", lwd=2)
# segments(x0=255.9656, y0=75, x1=909.0930, y1=75, col="royalblue", lwd=2)
# 
# ###make this a function/loop
# for(i in unique(si_range$volume)){
#   range(si_range[si_range$volume == 750, "sizeindex"])
# }
# 
# lines(container_volume[1:36] ~ min_size_index[1:36], data=standard,lwd=1, col="grey")
# lines(container_volume[1:36] ~ max_size_index[1:36], data=standard,lwd=1, col="grey")
