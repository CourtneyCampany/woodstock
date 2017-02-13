library(doBy)
library(ggplot2)
source("functions/functions.R")

# read data ---------------------------------------------------------------
si_clim <- read.csv("data/si_climate.csv")
si_means_clim <- read.csv("data/si_means_climate.csv")
origin <- read.csv("data/species_origin.csv")

# trim data so AS2303 range --------------------------------------------------------------

si_range <- si_clim[si_clim$volume >= 18,]
si_mean_range <- si_means_clim[si_means_clim$volume >= 18,]

# species info plotting ---------------------------------------------------
origin_means <- summaryBy(species ~ origin + leaf_type, data=origin,FUN=length, keep.names = TRUE)
origin_means$treetype <- paste(origin_means$origin, origin_means$leaf_type, sep = " ")
groups <- origin_means$species

# theme_set(theme_bw())
# science_theme <- theme(panel.border      = element_rect(color = "black"),
#                        panel.grid.major  = element_blank(), 
#                        panel.grid.minor  = element_blank(), 
#                        legend.position   = c(.91, .91),
#                        legend.title      = element_blank(),
#                        legend.background = element_blank(),
#                        legend.key        = element_blank(),
#                        legend.key.width  = unit(2.5, "lines"),
#                        legend.key.height = unit(.8, "lines"),
#                        axis.ticks.length = unit(-.2, "lines"),
#                        axis.text.x       = element_text(margin = margin(5)),
#                        axis.text.y       = element_text(margin = margin(0, 5)),
#                        axis.title.y      = element_text(margin = margin(0, 10)))


# origin plots ------------------------------------------------------------

png(filename = "hia_meeting/img/treetype.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5,1,1), cex.axis=.95, cex.lab = 1, las=1)
barplot(origin_means$species, ylab="Trees Measured", names.arg = origin_means$treetype, ylim=c(0,100))
box()
axis(side = 1, labels=groups, at <- c(.7, 1.9, 3.1, 4.3), line=-2.5, lty="blank", cex.axis=1.25)

dev.off()


# ggplot(data=origin_means, aes(x=treetype, y=species, color="lightgrey")) +
#   geom_bar(colour="black", stat="identity", size=.8, fill="lightgrey") +
#   xlab("") + ylab("Total Species Measured") +
#   science_theme +
#   theme(legend.position="none") +
#   ylim(0,100)


# region counts -----------------------------------------------------------
region_counts <- summaryBy(sizeindex ~ climate_region, data=si_clim, FUN=length, keep.names = TRUE)
region_counts2 <- region_counts[c(2,4,1,3,6,5),]
counts <- region_counts2$sizeindex

png(filename = "hia_meeting/img/treecounts.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5,1,1), cex.axis=.95, cex.lab = 1, las=1)
barplot(sort(region_counts$sizeindex, decreasing = FALSE), ylab="Trees Measured",
        names.arg = region_counts2$climate_region, ylim=c(0,4000))
box()
axis(side = 1, labels=counts, at <- c(.7, 1.9, 3.1, 4.3, 5.5, 6.7), line=-3, lty="blank", cex.axis=1.25)

dev.off()

# ggplot(data=region_counts, aes(x=reorder(climate_region, sizeindex), y=sizeindex)) +
#   geom_bar(colour="black", stat="identity", size=.8, fill="lightgrey") +
#   xlab("") + ylab("Total Trees Measured") +
#   science_theme +
#   theme(legend.position="none")

# volume and species specs ------------------------------------------------

##most common species
spec <- sort(table(si_range$species), decreasing = TRUE)
spec2 <- data.frame(spec)
spec3 <- spec2[1:10,]
names(spec3)[1:2] <- c("Species", "Trees Measured")
spec3$Species <- gsub("_", " ", spec3$Species)

length(unique(si_range$species))
length(unique(si_range$genus_species))

write.csv(spec3, "hia_meeting/data/spectop10.csv", row.names = FALSE)

##most common volume
volumes <- sort(table(si_mean_range$volume), decreasing = TRUE)
volumes <- data.frame(volumes)
names(volumes)[1:2] <- c("Volume", "Batches")

volumes2 <- volumes[1:11,]
write.csv(volumes2, "hia_meeting/data/volumestop11.csv", row.names = FALSE)
