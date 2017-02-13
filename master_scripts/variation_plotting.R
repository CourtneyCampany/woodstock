library(doBy)
library(ggplot2)
library(magicaxis)
library(RColorBrewer)
library(scales)

# read data ---------------------------------------------------------------

si_means_clim <- read.csv("data/si_means_climate.csv") 
si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")

# trim data so AS2303 range --------------------------------------------------------------

si_range <- si_clim[si_clim$volume >= 18,]
si_means_range <- si_means_clim[si_means_clim$volume >= 18,]

# plot bits ---------------------------------------------------------------


silab <- expression(Size~index~range~~(calliper~x~height))

palette(c(alpha("forestgreen", .4), alpha("goldenrod1",.4), alpha("navyblue", .4), alpha("firebrick2", .4),
          alpha("darkorchid3", .4), alpha("deepskyblue1", .4)))

crlab <- as.vector(unique(si_means_clim$climate_region))
transcol <- alpha("firebrick1", alpha=.4)

# plotting ----------------------------------------------------------------

##which are the most frequent used sizes?
table(si_means_range$volume)
##20, 45, 100, 200, 400, 1000

zero <- 0
twofifty <- 250

##create dataframe for shiny freq by species
si_freq <- si_range[si_range$volume %in% c(20, 45, 100, 200, 400, 1000),c("volume","sizeindex", "species" )]
si_freq$volume <- as.factor(si_freq$volume)

coords_dat <- data.frame(volume=c(20, 45, 100, 200, 400, 1000), xleft = c(24, 51,102,185, 330, 739 ),
                         ybottom = 0, xright =c(37, 75, 150, 272, 494, 1087), ytop=c(600,1500,600,300,100, 50))
coords_dat$volume <- as.factor(coords_dat$volume)

si_freq2 <- merge(si_freq, coords_dat, by="volume")


write.csv(si_freq2, "hia_meeting/data/si_freq.csv", row.names=FALSE)
  
##20 liter

png(filename = "hia_meeting/img/freq20.png", width = 11, height = 8.5, units = "in", res= 600)

hist(si_range[si_range$volume ==20,"sizeindex"], breaks=15, xlab = silab, main="", ylim=c(0,250))
box()
rect(24,0,37,600, col=transcol, border=NA)
legend("topright", "Container Volume = 20 L",bty='n', inset=.01)

dev.off()

##45 liter

hist(si_range[si_range$volume ==45,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,1200))
box()
rect(51,0,75,1500, col=transcol, border=NA)
legend("topright", "Container Volume = 45 L",bty='n', inset=.01)

##100 liter

hist(si_range[si_range$volume ==100,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,500))
box()
rect(102,0,150,600, border=NA, col=transcol)
legend("topright", "Container Volume = 100 L",bty='n', inset=.01)

##200 liter

hist(si_range[si_range$volume ==200,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,250))
box()
rect(185,0,272,300, border=NA, col=transcol)
legend("topright", "Container Volume = 200 L",bty='n', inset=.01)

##400 liter

hist(si_range[si_range$volume ==400,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,75))
box()
rect(330,0,494,100, border=NA, col=transcol)
legend("topright", "Container Volume = 400 L",bty='n', inset=.01)

##1000 liter

hist(si_range[si_range$volume ==1000,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,30))
box()
rect(739,0,1087,50, border=NA, col=transcol)
legend("topleft", "Container Volume = 1000 L",bty='n', inset=.001)

