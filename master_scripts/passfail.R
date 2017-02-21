source("functions/size_index_format.R")

# read data ----------------------------------------------------------------

si_clim <- read.csv("data/si_climate.csv")
standard <- read.csv("data/container_assessment.csv")
si_range <- si_clim[si_clim$volume >= 18,]
si_range$balanced <- "fail"

sort(unique(si_range$volume))

# Does Fit Size Index? ---------------------------------------------------------
doesfit_func <- function(x) {
  x$balanced <- ifelse(x$volume == 18 & x$sizeindex >=23 & x$sizeindex <=34, "pass", "fail")
  x$balanced <- ifelse(x$volume == 20 & x$sizeindex >=24 & x$sizeindex <=37, "pass",x$balanced)
  x$balanced <- ifelse(x$volume == 25 & x$sizeindex >=31 & x$sizeindex <=45, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 27 & x$sizeindex >= 32.7 & x$sizeindex <=48.4, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 30 & x$sizeindex >=36 & x$sizeindex <=53, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 35 & x$sizeindex >=41 & x$sizeindex <=61, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 45 & x$sizeindex >=51 & x$sizeindex <=75, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 50 & x$sizeindex >=56 & x$sizeindex <=82, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 52 & x$sizeindex >= 57.7 & x$sizeindex <=85.2, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 65 & x$sizeindex >=70 & x$sizeindex <=113, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 75 & x$sizeindex >=79 & x$sizeindex <=117, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 90 & x$sizeindex >= 92.6& x$sizeindex <=126.7, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 100 & x$sizeindex >=102 & x$sizeindex <=150, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 150 & x$sizeindex >=144 & x$sizeindex <=212, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 200 & x$sizeindex >=185 & x$sizeindex <=272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 250 & x$sizeindex >= 223.5& x$sizeindex <=329.6, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 300 & x$sizeindex >= 261.6& x$sizeindex <=385.7, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 350 & x$sizeindex >= 299& x$sizeindex <=440.4, "pass",x$balanced)
  x$balanced <- ifelse(x$volume == 400 & x$sizeindex >=330 & x$sizeindex <=494, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 500 & x$sizeindex >=407 & x$sizeindex <=599, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 600 & x$sizeindex >=476 & x$sizeindex <=700, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 700 & x$sizeindex >= 543& x$sizeindex <=800, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 750 & x$sizeindex >=577 & x$sizeindex <=849, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 800 & x$sizeindex >=610 & x$sizeindex <=898, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1000 & x$sizeindex >=739 & x$sizeindex <=1087, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1200 & x$sizeindex >=865 & x$sizeindex <=1272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1500 & x$sizeindex >=1048 & x$sizeindex <=1542, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1800 & x$sizeindex >= 1227& x$sizeindex <=1805, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2000 & x$sizeindex >=1343 & x$sizeindex <=1975, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2500 & x$sizeindex >=1627 & x$sizeindex <=2393, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 3000 & x$sizeindex >= 1906& x$sizeindex <=2804, "pass", x$balanced)
  return(x)
}

test_si <- doesfit_func(si_range)

#subset of test_si for plotting
si_passfail <- test_si[, c(1:20, 49)]
write.csv(si_passfail, "data/si_passfail.csv", row.names = FALSE)


table(test_si$balanced)

slices <- c(9508,4312) 
lbls <- c("FAIL", "PASS")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels

png(filename = "hia_meeting/img/passfail.png", width = 11, height = 8.5, units = "in", res= 600)
pie3D(slices,labels=lbls,explode=0.1,shade=.5, theta=1, col=c("red", "forestgreen"))
dev.off()
