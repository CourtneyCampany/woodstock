si_clim <- read.csv("data/si_climate.csv")

si_range <- si_clim[si_clim$volume >= 18,]
si_range <- transform(si_range,ID = paste(nursery, batch_id, sep="_"))


shape <- read.csv("data/tree_shape.csv")

shape_range <- shape[shape$volume>=18,]
shape_range <- transform(shape_range,ID = paste(nursery, batch_id, sep="_"))


# format data and merge data -------------------------------------------------------------
si_agg <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE,
                    id= ~ species+origin+leaf_type+nursery+climate_region,
                    data=si_range)

shape_agg <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE,
                       id= ~ species+origin+leaf_type+nursery+climate_region,
                       data=shape_range)

tree_dat <- merge(si_agg, shape_agg, all=TRUE)
#by using the full size index dataset I havent deleted the batches with only one tree (done in shape data)
#there are a few batches where no shape data exists
#delete these batches by removing data with NA in tree number column
rowstodelete <- which(is.na(tree_dat$treenumb))
#also trim out monthly met data

tree_stats <- tree_dat[-rowstodelete,c(1:19,44:50)]
write.csv(tree_stats, "data/tree_stats.csv", row.names = FALSE)