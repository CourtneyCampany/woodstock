#this script makes Figure 1 of the HIA report as a map of nureseries visited.
library(scales)
library(oz)
locations <- read.csv("hia_meeting/data/nursery_locations.csv")
locations2 <- locations[1:23,2:3]

region <- c("New South Wales = 4", "Northern Territory = 1", "Queensland = 5", "South Australia = 6", 
            "Victoria = 4", "Western Australia = 3")

# windows()

png(filename = "output/ozmap.png", width = 7, height = 7, units = "in", res= 600)
par(mar=c(0,0,0,0))
oz()

with(locations2, points(jitter(long) ,jitter(lat), pch = 21, cex = 1.5, bg = alpha("firebrick1", .5)))
# legend("bottomleft", title=expression(underline("Total Nurseries")), legend=region, bty='n', inset=.15)
legend("bottomleft", title=expression(underline("Total Nurseries")), legend=region, bty='n', inset=.09)
dev.off()
