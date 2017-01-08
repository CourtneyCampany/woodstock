library(magicaxis)
library(RColorBrewer)
library(scales)
library(doBy) 

oz_sizeindex <- read.csv("raw_data/oz_sizeindex.csv")

# means dataset of size index parameters ----------------------
 
oz_si_agg <- summaryBy(logSI ~ species + nursery+volume,
                       data=oz_sizeindex, FUN=mean, keep.names = TRUE)

standard <- read.csv("raw_data/sizeindex_specs.csv")

# plot size index ---------------------------------------------------------


##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

###nice color palette
cols <-  brewer.pal(6, "Set1")
cols2 <- c(alpha(cols[5], .65), alpha(cols[3], .65), alpha(cols[2], .65), alpha(cols[1], .65))

windows(7,7)
par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ log10(volume), data=oz_si_agg, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg="olivedrab3",pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
#legend("bottomright", c("New South Wales", "Northern Territory", "Perth", "Victoria"), pch=21, pt.bg =cols2, inset=.025)
title(main="Australia Tree Nurseries")
box()