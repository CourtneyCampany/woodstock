
# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")


# simple model and predict CI ---------------------------------------------

si_mod <- lm(logSI ~ logvol, data=si_clim)

##seqeunce of volumes (log10) to predict from
volseq <- seq(min(si_clim$logvol), max(si_clim$logvol), length=501)

si_pred <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction")

##ci
# si_upr <- si_pred$fit + (2*si_pred$se.fit)
# si_lwr <- si_pred$fit - (2*si_pred$se.fit)


# plot CI -----------------------------------------------------------------
silab <- expression(Size~index~range~~(calliper~x~height))
si_range <- si_clim[si_clim$volume >= 18,]

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, col=alpha("lightgrey", .1), pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

lines(volseq, si_pred[,2], lty=2, lwd=2,col="royalblue")
lines(volseq, si_pred[,3], lty=2, lwd=2,col="royalblue")
# lines(volseq, si_pred[,1], lty=1, lwd=2,col="royalblue")

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

box()