#this script makes Tabl1 of the HIA report as nursery summary table

locations <- read.csv("hia_meeting/data/nursery_locations.csv")
  locations2 <- locations[1:23,]
  names(locations2) <- c("Nursery", "Latitude", "Longitude", "Trees Measured")

#clean nursery names -------------------------------------------------------
nurseryname_format <- function(x) {
  x$Nursery <- gsub("alpine", "Alpine Nurseries", x$Nursery)
  x$Nursery <- gsub("andreasens", "Andreasens Green - Kemps Creek", x$Nursery)
  x$Nursery <- gsub("mangrove mountain", "Andreasens Green - Mangrove Mountain", x$Nursery)
  x$Nursery <- gsub("trees impact", "Trees Impact", x$Nursery)
  x$Nursery <- gsub("greenstock", "Greenstock Nurseries", x$Nursery)
  x$Nursery <- gsub("ibrox", "Ibrox Park Nursery", x$Nursery)
  x$Nursery <- gsub("pallara", "Pallara Trees", x$Nursery)
  x$Nursery <- gsub("fleming", "Fleming's Nurseries", x$Nursery)
  x$Nursery <- gsub("speciality", "Speciality Trees", x$Nursery)
  x$Nursery <- gsub("established tree transplanters", "Established Tree Transplanters", x$Nursery)
  x$Nursery <- gsub("mt william", "Mt William Advanced Tree Nursery", x$Nursery)
  x$Nursery <- gsub("darwin plant wholesalers", "Darwin Plant wWholesalers", x$Nursery)
  x$Nursery <- gsub("benara", "Benara Nurseries", x$Nursery)
  x$Nursery <- gsub("arborwest", "Arborwest Tree Farm", x$Nursery)
  x$Nursery <- gsub("ellenby tree farm", "Ellenby Tree Farm", x$Nursery)
  x$Nursery <- gsub("adelaide advanced", "Adelaide Advanced Trees", x$Nursery)
  x$Nursery <- gsub("adelaide tree farm", "Adelaide Tree Farm", x$Nursery)
  x$Nursery <- gsub("heynes", "Heynes's Nursery", x$Nursery)
  x$Nursery <- gsub("cleveland", "Cleveland Nursery", x$Nursery)
  x$Nursery <- gsub("manor", "Manor Nurseries", x$Nursery)
  x$Nursery <- gsub("logans", "logans Nursery", x$Nursery)
  x$Nursery <- gsub("freshford", "Freshford Nurseries", x$Nursery)
  x$Nursery <- gsub("plantsdirect", "Plants Direct", x$Nursery)
  return(x)
} 

table_data <- nurseryname_format(locations2)


#add climate data ------------------------------------------
climate <- read.csv("data/nursery_climate_history.csv")
climate2 <- climate[,c(1:3, 28:29)]  
  names(climate2) <- c("Nursery", "Latitude", "Longitude", "MAT", "MAP")

table_data2 <- merge(table_data, climate2[,2:5])
table_data3 <- table_data2[,c(3,1,2,5,6,4)]
table_data4 <- table_data3[order(table_data3$Nursery),]
  table_data4$MAT <- round(table_data4$MAT, 1)

#save table 1
write.csv(table_data4, "master_scripts/table1.csv", row.names=FALSE)
  
