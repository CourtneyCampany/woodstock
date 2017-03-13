# species plotting function -----------------------------------------------
silab <- expression(Size~index~range~~(calliper~x~height))

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

