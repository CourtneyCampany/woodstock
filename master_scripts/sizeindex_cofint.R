library(scales)

# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")

#means data
si_means <- read.csv("data/si_means_climate.csv")

# trim data so AS2303 range --------------------------------------------------------------

si_range <- si_clim[si_clim$volume >= 18,]
si_means_range <- si_means[si_means$volume >= 18,]

# simple model and predict CI ---------------------------------------------

si_mod <- lm(logSI ~ logvol, data=si_range)

##seqeunce of volumes (log10) to predict from
volseq <- seq(min(si_range$logvol), max(si_range$logvol), length=501)
si_pred95 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.95)

silab <- expression(Size~index~range~~(calliper~x~height))

# plot CI -----------------------------------------------------------------
#density color vector
cols<- with(si_range, densCols(logvol, logSI,colramp = colorRampPalette(blues9[-(1:2)])))
#alpha("lightgrey", .2)
windows(7,7)

png(filename = "output/si_alltrees.png", width = 7, height = 7, units = "in", res= 600)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, col=cols, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)

lines(volseq, si_pred95[,2], lty=3, lwd=2,col="black")
lines(volseq, si_pred95[,3], lty=3, lwd=2,col="black")
# lines(volseq, si_pred[,1], lty=1, lwd=2,col="royalblue")

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1)
legend(x=.93, y= 3.63,"95% Prediction Interval" ,lty=3, lwd=2,bty='n', col="black", inset=.01, cex=1)

 # text(x=3.2, y=.75, "45L tree size index ranges from 20.6 - 135.6", cex=1.1)
 # text(x=3.21, y=.5, "100L tree size index ranges from 40.2 - 263.3", cex=1.1)

box()

dev.off()


# means by tree type ------------------------------------------------------
evercol <- alpha("forestgreen", .7)
decidcol <- alpha("goldenrod1", .7)
ever <- si_range[si_range$leaf_type == "evergreen",]
decid <- si_range[si_range$leaf_type == "deciduous",]
si_ever <- lm(logSI ~ logvol, data=ever)
si_dec <- lm(logSI ~ logvol, data=decid)

windows(7,7)

 png(filename = "output/si_leaftype.png", width = 7, height = 7, units = "in", res= 600)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1,las=0,mgp=c(3,1,0))

with(si_means_range[sample(nrow(si_means_range)),],
     plot(log10(sizeindex.mean) ~ jitter(log10(volume)), xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),axes=FALSE, type='n'))

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
# segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
# segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)
# polygon(x=c(1.3, 3.39,3.39, 1.3), y=c(1.38, 3.21, 3.37,1.56), col=alpha("grey", .4), border=NA)
with(si_means_range[sample(nrow(si_means_range)),],
     points(log10(sizeindex.mean) ~ jitter(log10(volume)), col=c(decidcol,evercol)[leaf_type], pch=16, cex=1.5))

library(plotrix)
ablineclip(si_ever, col="forestgreen", lwd=2, x1=min(ever$logvol), x2=max(ever$logvol), y1=min(ever$logSI), y2=max(ever$logSI))
ablineclip(si_dec, col="goldenrod1", lwd=2,x1=min(decid$logvol), x2=max(decid$logvol), y1=min(decid$logSI), y2=max(decid$logSI))

# legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1)
legend("bottomright", c("Evergreen", "Deciduous") ,pch=16,bty='n', inset=.02, pt.cex=1.5, 
       col=c("forestgreen","goldenrod1"))

box()

dev.off()


# plot with different levels of PI ----------------------------------------

si_pred75 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.75)
si_pred50 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.50)
si_pred25 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.25)
si_pred10 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.10)

par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, col=alpha("lightgrey", .2), pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)

#lines of different PI
lines(volseq, si_pred95[,2], lty=2, lwd=2,col="royalblue")
lines(volseq, si_pred95[,3], lty=2, lwd=2,col="royalblue")
 # lines(volseq, si_pred95[,1], lty=1, lwd=2,col="royalblue")

 lines(volseq, si_pred50[,2], lty=2, lwd=2,col="forestgreen")
 lines(volseq, si_pred50[,3], lty=2, lwd=2,col="forestgreen")

# lines(volseq, si_pred25[,2], lty=2, lwd=2,col="firebrick1")
# lines(volseq, si_pred25[,3], lty=2, lwd=2,col="firebrick1")

# lines(volseq, si_pred10[,2], lty=2, lwd=2,col="goldenrod2")
# lines(volseq, si_pred10[,3], lty=2, lwd=2,col="goldenrod2")

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1.25)

legend("bottomright",title = "95% Prediction Interval",legend = c("95%", "50%"), lty=2, lwd=2,bty='n',
       col=c("royalblue", "forestgreen"), inset=.02, cex=1.25)

box()



# figure with fitting points highlighted ----------------------------------

library(extrafont)
library(showtext)
halfgreen <- alpha("forestgreen", .2)
halfred <- alpha("firebrick3", .2)
evercol <- alpha("forestgreen", .3)
decidcol <- alpha("goldenrod1", .3)

# googfonts <- font.families.google()

si_passfail <- read.csv( "data/si_passfail.csv")

# font.files()
# font.paths()
font.add(regular ="GothamNarrow-Book.otf", "gotham")
#font.add(regular ="ChronicleTextG1-Roman-Pro.otf", "chronic")

# png(filename = "output/si_passfail.png", width = 11, height = 8.5, units = "in", res= 600)

###png output doesnt work properly with showtext (cex is messed up???)

# windows()
# showtext.begin()
png(filename = "output/sifig.png", width = 11, height = 8.5, units = "in", res= 600)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, col=alpha("lightgrey", .2), pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)

#add assessment
segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)

lines(volseq, si_pred95[,2], lty=2, lwd=2,col="royalblue")
lines(volseq, si_pred95[,3], lty=2, lwd=2,col="royalblue")

points(logSI ~ logvol, data=si_passfail[si_passfail$balanced=="pass",], col=halfred, pch=16)

legend(x=.92, y= 3.75, c("AS2303 Size Index Range" ,"95% Prediction Interval","32% of trees fit inside specified range"),
       lty=c(1,2,0), pch=c(NA,NA,16), pt.cex=1,inset=01, bty='n', col=c("black","black","firebrick")) 

##inset

par(fig=c(0.55, 0.95, 0.15,0.5), new=T, mar=c(2,2,0,0), cex=.7, las=0, cex.axis=.7, cex.lab=.7, tcl=-.25,mgp=c(2,1,0))
plot(logSI ~ jitter(logvol, .1), data=si_range[si_range$volume==35,], xlab="Container volume (L)", 
     ylab=silab, xlim=c(1.4,1.7),ylim=c(.9,2.2), col=c(decidcol,evercol)[leaf_type],
     axes=FALSE, cex=1.25, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE, labels=FALSE)
axis(2, at=c(log10(10), log10(20),log10(50), log10(100)), labels =c(10,20,50, 100), tick=FALSE, line=-1)
axis(1, at=c(log10(30), log10(40), log10(50)), labels =c(30,40,50), tick=FALSE, line=-1)

#add assessment
segments(x0=1.47,y0=1.55, x1=1.6, y1=1.66, lwd=2)
segments(x0=1.47,y0=1.72, x1=1.6, y1=1.83, lwd=2)

legend("topright", c("Deciduous", "Evergreen"), col=c("goldenrod1","forestgreen"),inset=.03, 
       cex=1, bty='n', pch=16, pt.cex=1, title= "35 L" )

# showtext.end()

# dev.copy2pdf(file= "output/si_passfail.pdf")  
dev.off() 

