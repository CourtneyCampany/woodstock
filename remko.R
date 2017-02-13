library(doBy)
library(lme4)
library(visreg)
library(car)
library(gplots)
library(Hmisc)

# read data ---------------------------------------------------------------



si_clim <- read.csv("data/si_climate.csv")
si_range <- si_clim[si_clim$volume >= 18,]

si_range <- transform(si_range,
                      ID = paste(nursery, batch_id, sep="_"))

standard <- read.csv("data/container_assessment.csv")

# format data -------------------------------------------------------------


sia <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE,
                 id= ~ species+origin+leaf_type+nursery+climate_region,
                 data=si_range)

sort(table(sia$volume))

# plot bits ---------------------------------------------------------------

silab <- expression(Size~index~range~~(calliper~x~height))


# stats with climate ------------------------------------------------------
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


r <- ranef(lme0)
windows()
par(mar=c(12,4,2,2), las=2)
barplot(sort(r$nursery[[1]]),
        names.arg=rownames(r$nursery))


sia45 <- subset(sia, volume == 45)

with(sia45, plot(species, logSI))

sort(table(sia$species))



# species plotting function -----------------------------------------------


palette(rich.colors(6))

plot_species <- function(spec){

  mag <- subset(sia, species == spec)
  silab <- expression(Size~index~range~~(calliper~x~height))
  
  plot(logSI ~ logvol, data=mag, xlab="Container volume (L)", 
       ylab=silab, xlim=c(1,3.7), ylim=c(0.3,3.7),
       axes=FALSE, pch=19, col=climate_region, 
       main=capitalize(gsub("_"," ",spec)))
  
  lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  
  magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
  legend("bottomright", levels(sia$climate_region), pch=19, col=palette(), bty='n')
}

sp <- names(sort(table(sia$species), decreasing=TRUE))[1:10]

pdf("species.pdf")
for(x  in sp)plot_species(x)
dev.off()



# SI by deciduous/evergreen -----------------------------------------------

palette(c("blue","red"))
plot(logSI ~ jitter(logvol,0.5), data=sia, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7), ylim=c(0.3,3.7),
     axes=FALSE, pch=19, col=leaf_type)

lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

# si by taper with stats -------------------------------------------------------------

windows(10,6)
par(mar=c(12,4,2,2), las=2, cex.axis=0.7)


# quantcut
x <- with(sia, tapply(height_m/calliper300, species, median))
y <- cut(x, c(0,0.055, 0.09, 100))
dfr <- data.frame(species=names(x), slenderclass=y)
sia <- merge(sia, dfr, by="species")

barplot(sort(x))

lme6 <- lmer(logSI ~ logvol*slenderclass + (1|nursery/species), data=sia)
visreg(lme6, "logvol", by="slenderclass", overlay=TRUE)


palette(rich.colors(3))
plot(logSI ~ jitter(logvol,1), data=sia, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7), ylim=c(0.3,3.7),
     axes=FALSE, pch=19, col=slenderclass)

lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend("bottomright", levels(sia$slenderclass), pch=19, col=palette())
box()


# si by nursery -----------------------------------------------------------

plot_nurs <- function(nurs){
  
  plot(logSI ~ logvol, data=sia, xlab="Container volume (L)", 
       ylab=silab, xlim=c(1,3.7), ylim=c(0.3,3.7),
       axes=FALSE, pch=19, col="grey", main=nurs)
  with(subset(sia, nursery == nurs), points(logvol, logSI, pch=19, col="red"))
  
  lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  
  magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
}


pdf("nurseries.pdf")
for(lev in levels(sia$nursery))plot_nurs(lev)
dev.off()

