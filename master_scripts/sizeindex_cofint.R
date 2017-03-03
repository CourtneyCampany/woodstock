library(scales)

# read data ---------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")

# trim data so AS2303 range --------------------------------------------------------------

si_range <- si_clim[si_clim$volume >= 18,]

# simple model and predict CI ---------------------------------------------

si_mod <- lm(logSI ~ logvol, data=si_range)

##seqeunce of volumes (log10) to predict from
volseq <- seq(min(si_range$logvol), max(si_range$logvol), length=501)
si_pred95 <- predict(si_mod, newdata = data.frame(logvol = volseq), interval="prediction", level=0.95)

silab <- expression(Size~index~range~~(calliper~x~height))

# plot CI -----------------------------------------------------------------

# windows(7,7)

# png(filename = "hia_meeting/img/si_95pred2.png", width = 11, height = 8.5, units = "in", res= 600)

par(mar=c(5,5,2,1),cex.axis=1.25, cex.lab=1.5,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1.25, col=alpha("lightgrey", .2), pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
segments(x0=1.30,y0=1.38, x1=3.39, y1=3.21, lwd=2)
segments(x0=1.30,y0=1.56, x1=3.39, y1=3.37, lwd=2)

lines(volseq, si_pred95[,2], lty=2, lwd=2,col="royalblue")
lines(volseq, si_pred95[,3], lty=2, lwd=2,col="royalblue")
# lines(volseq, si_pred[,1], lty=1, lwd=2,col="royalblue")

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1.25)
legend(x=.93, y= 3.63,"95% Prediction Interval" ,lty=2, lwd=2,bty='n', col="royalblue", inset=.01, cex=1.25)

# text(x=2.9, y=1, "45L tree size index ranges from 20.6 - 135.6", cex=1.25)
# text(x=2.91, y=.75, "100L tree size index ranges from 40.2 - 263.3", cex=1.25)

box()

# dev.off()


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

windows()
showtext.begin()

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

# legend(x=.92, y= 3.75, "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01, cex=1)
# legend(x=.92, y= 3.55,"95% Prediction Interval" ,lty=2, lwd=2,bty='n', col="royalblue", inset=.01, cex=1)
# 
#  legend(x=1, y= 3.35, "32% of trees inside specified range", inset=.01, 
#         cex=1, bty='n', pch=16, col="firebrick", pt.cex=1)
 
legend(x=.92, y= 3.75, c("AS2303 Size Index Range" ,"95% Prediction Interval","32% of trees fit inside specified range"),
       lty=c(1,2,0), pch=c(NA,NA,16), pt.cex=1,inset=01, bty='n', col=c("black","black","firebrick")) 

##inset

par(fig=c(0.55, 0.95, 0.15,0.5), new=T, mar=c(2,2,0,0), cex=.7, las=0, cex.axis=.7, cex.lab=.7, tcl=-.25,mgp=c(2,1,0))
plot(logSI ~ logvol, data=si_range[si_range$volume==35,], xlab="Container volume (L)", 
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

showtext.end()

dev.copy2pdf(file= "output/si_passfail.pdf")  
dev.off() 

