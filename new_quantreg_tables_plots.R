

library(dplyr)
library(magicaxis)
library(quantreg)
library(scales)
library(extrafont)
#loadfonts(device="win")


poly_rqs <- function(mod1, mod2, ...){
  
  pu <- par("usr")
  x <- pu[1:2]
  
  newdat <- data.frame(volume=10^x)
  y1 <- predict(mod1, newdata=newdat)
  y2 <- predict(mod2, newdata=newdat)
  
  polygon(x=c(x, rev(x)), y=c(y1, rev(y2)), border=NA, ...)
}

sitree <- read.csv("data/si_climate.csv") %>%
  dplyr::select(volume, sizeindex, leaf_type) %>%
  rename(si = sizeindex) %>%
  filter(volume >= 18)

sitree_small_ever <- filter(sitree, volume < 100, leaf_type=="evergreen")
sitree_small_deci <- filter(sitree, volume < 100, leaf_type=="deciduous")
sitree_large <- filter(sitree, volume >= 100)


fit_small_ever_10 <- rq(log10(si) ~ log10(volume), data=sitree_small_ever, tau=0.1)
fit_small_ever_25 <- rq(log10(si) ~ log10(volume), data=sitree_small_ever, tau=0.25)
fit_small_ever_75 <- rq(log10(si) ~ log10(volume), data=sitree_small_ever, tau=0.75)
fit_small_ever_90 <- rq(log10(si) ~ log10(volume), data=sitree_small_ever, tau=0.9)

fit_small_deci_10 <- rq(log10(si) ~ log10(volume), data=sitree_small_deci, tau=0.1)
fit_small_deci_25 <- rq(log10(si) ~ log10(volume), data=sitree_small_deci, tau=0.25)
fit_small_deci_75 <- rq(log10(si) ~ log10(volume), data=sitree_small_deci, tau=0.75)
fit_small_deci_90 <- rq(log10(si) ~ log10(volume), data=sitree_small_deci, tau=0.9)

fit_large_10 <- rq(log10(si) ~ log10(volume), data=sitree_large, tau=0.1)
fit_large_25 <- rq(log10(si) ~ log10(volume), data=sitree_large, tau=0.25)
fit_large_75 <- rq(log10(si) ~ log10(volume), data=sitree_large, tau=0.75)
fit_large_90 <- rq(log10(si) ~ log10(volume), data=sitree_large, tau=0.9)

# Small, deciduous
windows()
par(yaxs="i", xaxs="i", las=1, tcl=0, cex.lab=1.2, mgp=c(2.4, 0.25, 0), cex.axis=0.7, pty='s',
    family="Gotham Narrow Book")
xat <- seq(10,100,by=10)
yat <- c(seq(10, 100, by=10), 200)

with(sitree_small_deci, plot(log10(volume), log10(si), pch=16, cex=0.5, 
                        type='n',
                        panel.first={
                          abline(v=log10(xat), col="grey")
                          abline(h=log10(yat), col="grey")
                          
                          poly_rqs(fit_small_deci_10, fit_small_deci_25, col=alpha("lightgrey", 0.6))
                          poly_rqs(fit_small_deci_25, fit_small_deci_75, col=alpha("grey", 0.6))
                          poly_rqs(fit_small_deci_75, fit_small_deci_90, col=alpha("lightgrey", 0.6))
                        },
                        ylim=log10(c(10,200)),
                        xlim=log10(c(18, 90)),
                        xlab="Container Volume (L)",
                  ylab="Size Index (calliper x height)",
                  col="darkgrey", axes=FALSE))
abline(fit_small_deci_10, lty=2, lwd=1)
abline(fit_small_deci_25, lty=2, lwd=1)
abline(fit_small_deci_75, lty=2, lwd=1)
abline(fit_small_deci_90, lty=2, lwd=1)

axis(1, at=log10(xat), labels=xat)
axis(2, at=log10(yat), labels=yat)
box()

xt <- log10(25)
text(xt, log10(22), "10%", srt=20, font=2)
text(xt, log10(29), "25%", srt=20, font=2)
text(xt, log10(52), "75%", srt=20, font=2)
text(xt, log10(68), "90%", srt=20, font=2)

text(log10(40), log10(37), "Small", srt=20, font=2, col="darkgrey", adj=0)
text(log10(40), log10(60), "Medium", srt=20, font=2, col="darkgrey", adj=0)
text(log10(40), log10(88), "Large", srt=20, font=2, col="darkgrey", adj=0)
title(main="Small trees, Deciduous", adj=0)
dev.copy2pdf(file="sitree_small_deci.pdf")


# Small, evergreen
windows()
par(yaxs="i", xaxs="i", las=1, tcl=0, cex.lab=1.2, mgp=c(2.4, 0.25, 0), cex.axis=0.7, pty='s',
    family="Gotham Narrow Book")
xat <- seq(10,100,by=10)
yat <- c(8, 9, seq(10, 100, by=10), 200)

with(sitree_small_ever, plot(log10(volume), log10(si), pch=16, cex=0.5, 
                             type='n',
                             panel.first={
                               abline(v=log10(xat), col="grey")
                               abline(h=log10(yat), col="grey")
                               
                               poly_rqs(fit_small_ever_10, fit_small_ever_25, col=alpha("lightgrey", 0.6))
                               poly_rqs(fit_small_ever_25, fit_small_ever_75, col=alpha("grey", 0.6))
                               poly_rqs(fit_small_ever_75, fit_small_ever_90, col=alpha("lightgrey", 0.6))
                             },
                             ylim=log10(c(8,200)),
                             xlab="Container Volume (L)",
                             ylab="Size Index (calliper x height)",
                             col="darkgrey", axes=FALSE))
abline(fit_small_ever_10, lty=2, lwd=1)
abline(fit_small_ever_25, lty=2, lwd=1)
abline(fit_small_ever_75, lty=2, lwd=1)
abline(fit_small_ever_90, lty=2, lwd=1)

axis(1, at=log10(xat), labels=xat)
axis(2, at=log10(yat), labels=yat)
box()
xt <- log10(25)
d <- 0.1
text(xt, log10(10)+d, "10%", srt=24, font=2)
text(xt, log10(13.7)+d, "25%", srt=24, font=2)
text(xt, log10(26.8)+d, "75%", srt=24, font=2)
text(xt, log10(36.6)+d, "90%", srt=24, font=2)

text(log10(40), log10(27), "Small", srt=26, font=2, col="darkgrey")
text(log10(40), log10(44), "Medium", srt=26, font=2, col="darkgrey")
text(log10(40), log10(67), "Large", srt=26, font=2, col="darkgrey")
title(main="Small trees, Evergreen", adj=0)
dev.copy2pdf(file="sitree_small_ever.pdf")


# Large
windows()
par(yaxs="i", xaxs="i", las=1, tcl=0, cex.lab=1.2, mgp=c(2.4, 0.25, 0), cex.axis=0.7, pty='s',
    family="Gotham Narrow Book")
xat <- c(seq(100, 1000, by=100), 2000, 3000)
yat <- c(seq(50, 100, by=10), seq(200, 1000, by=100), 2000, 3000)

with(sitree_large, plot(log10(volume), log10(si), pch=16, cex=0.5, 
                        type='n',
                        panel.first={
                          abline(v=log10(xat), col="grey")
                          abline(h=log10(yat), col="grey")
                          
                          poly_rqs(fit_large_10, fit_large_25, col=alpha("lightgrey", 0.6))
                          poly_rqs(fit_large_25, fit_large_75, col=alpha("grey", 0.6))
                          poly_rqs(fit_large_75, fit_large_90, col=alpha("lightgrey", 0.6))
                        },
                        ylim=log10(c(50,3000)),
                        xlab="Container Volume (L)",
                        ylab="Size Index (calliper x height)",
                        col="darkgrey", axes=FALSE))
abline(fit_large_10, lty=2, lwd=1)
abline(fit_large_25, lty=2, lwd=1)
abline(fit_large_75, lty=2, lwd=1)
abline(fit_large_90, lty=2, lwd=1)

xat2 <- c(seq(100, 800, by=100), 1000, 2000, 3000)
axis(1, at=log10(xat2), labels=xat2)
axis(2, at=log10(yat), labels=yat)
box()
xt <- log10(150)

text(xt, log10(77), "10%", srt=30, font=2, adj=0)
text(xt, log10(104), "25%", srt=30, font=2, adj=0)
text(xt, log10(180), "75%", srt=30, font=2, adj=0)
text(xt, log10(240), "90%", srt=30, font=2, adj=0)

text(log10(600), log10(290), "Small", srt=35, font=2, col="darkgrey", adj=0)
text(log10(600), log10(450), "Medium", srt=35, font=2, col="darkgrey", adj=0)
text(log10(600), log10(645), "Large", srt=35, font=2, col="darkgrey", adj=0)
title(main="Large trees", adj=0)
dev.copy2pdf(file="sitree_large.pdf")




with(sitree, plot(log10(volume), log10(si), type='n', axes=FALSE))
abline(fit_large_10, lty=1, lwd=1)
abline(fit_small_deci_10, lty=2, lwd=1)
abline(fit_small_ever_10, lty=5, lwd=1)
magaxis(side=1:2, unlog=1:2)
abline(v=log10(c(90,100)), col="red")

with(sitree, plot(log10(volume), log10(si), type='n', axes=FALSE))
abline(fit_large_25, lty=1, lwd=1)
abline(fit_small_deci_25, lty=2, lwd=1)
abline(fit_small_ever_25, lty=5, lwd=1)
magaxis(side=1:2, unlog=1:2)
abline(v=log10(c(90,100)), col="red")

with(sitree, plot(log10(volume), log10(si), type='n', axes=FALSE))
abline(fit_large_75, lty=1, lwd=1)
abline(fit_small_deci_75, lty=2, lwd=1)
abline(fit_small_ever_75, lty=5, lwd=1)
magaxis(side=1:2, unlog=1:2)
abline(v=log10(c(90,100)), col="red")

with(sitree, plot(log10(volume), log10(si), type='n', axes=FALSE))
abline(fit_large_90, lty=1, lwd=1)
abline(fit_small_deci_90, lty=2, lwd=1)
abline(fit_small_ever_90, lty=5, lwd=1)
magaxis(side=1:2, unlog=1:2)
abline(v=log10(c(90,100)), col="red")






vols <- sort(unique(sitree$volume))

vols_small <- vols[vols < 100]
vols_large <- vols[vols >= 100]

tab_small_deci <- data.frame(volume = vols_small,
                             si_10 = 10^predict(fit_small_deci_10, newdata=data.frame(volume=vols_small)),
                             si_25 = 10^predict(fit_small_deci_25, newdata=data.frame(volume=vols_small)),
                             si_75 = 10^predict(fit_small_deci_75, newdata=data.frame(volume=vols_small)),
                             si_90 = 10^predict(fit_small_deci_90, newdata=data.frame(volume=vols_small))
                             )

tab_small_ever <- data.frame(volume = vols_small,
                             si_10 = 10^predict(fit_small_ever_10, newdata=data.frame(volume=vols_small)),
                             si_25 = 10^predict(fit_small_ever_25, newdata=data.frame(volume=vols_small)),
                             si_75 = 10^predict(fit_small_ever_75, newdata=data.frame(volume=vols_small)),
                             si_90 = 10^predict(fit_small_ever_90, newdata=data.frame(volume=vols_small))
)

tab_large <- data.frame(volume = vols_large,
                             si_10 = 10^predict(fit_large_10, newdata=data.frame(volume=vols_large)),
                             si_25 = 10^predict(fit_large_25, newdata=data.frame(volume=vols_large)),
                             si_75 = 10^predict(fit_large_75, newdata=data.frame(volume=vols_large)),
                             si_90 = 10^predict(fit_large_90, newdata=data.frame(volume=vols_large))
)








