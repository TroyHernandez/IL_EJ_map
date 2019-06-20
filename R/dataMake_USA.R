# dataMake.R

##
## GWL 2016-10-10
## Putting 20_Map_dev_v1.R into map_demo.Rmd
##

##==============================================================================
## INITIALIZE
##==============================================================================

library(leaflet)
# install geos on RHEL to install tigris package
# sudo yum install geos geos-devel
# sudo yum install udunits2-devel
# sudo yum install gdal
# Had to build 2.2 from source
# https://stackoverflow.com/questions/46181614/install-r-sf-package-on-centos-gdal-shared-libary-error

install.packages('sf')
# install tigris on redhat
install.packages("tigris")
source("R/download_census_tracts.R")

##==============================================================================
## LOAD DATA
##==============================================================================
download.file("ftp://newftp.epa.gov/EJSCREEN/2018/EJSCREEN_2018_USPR_csv.zip", "data/EJSCREEN_2018_USPR_csv.zip")
unzip("data/EJSCREEN_2018_USPR_csv.zip", exdir = "data/")

# Should probably change this to data.table's fread
dat <- read.csv("data/EJSCREEN_Full_USPR_2018.csv", stringsAsFactors = FALSE)

fips <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

# Old version only needed IL, new version needs all of them!
# census_tracts <- download_census_tracts(infile = "data/censustracts_IL.Rds")
# Download all of the census tract shape files 
for(i in 1:50){
  # census_tracts <- tigris::tracts(state = fips$fips[i])
  saveRDS(tigris::tracts(state = fips$fips[i], class = "sf"),
          file = paste0("data/censustracts/", fips$state_abbr[i], ".Rds"))
}

# also Washington DC
saveRDS(tigris::tracts(11, class = "sf"),
        file = paste0("data/censustracts/", "DC", ".Rds"))

# New version still needs geoid
geoid <- c()
lf <- list.files("data/censustracts")
geoid <- unlist(lapply(lf, FUN = function(x){readRDS(paste0("data/censustracts/", x))$GEOID}))

##==============================================================================
## AGGREGATE EJ DATA
##==============================================================================

# library(sf)
# library(tigris)
# state = "WI"
# lat <- 43.8138
# lon <- -91.2519
# 
# State <- tigris::tracts(state, class = "sf")
# state_tracts <- State[, c("GEOID", "TRACTCE")]
# 
# # convert the points to same CRS
# my_points_tract <- sf::st_join(sf::st_as_sf(data.frame(x = lon, y = lat) , coords = c("x", "y"),
#                                             crs = sf::st_crs(state_tracts)),
#                                state_tracts)



## Here's the data without aggregating:
# IL.ind <- which(substr(as.character(dat$ID), 1, 2) == "17")
# dat.IL <- dat[IL.ind, ]

# Not run first time
ID.tract <- substr(as.character(dat$ID), 1, 11)

dat.env <- dat[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE", "PRE1960",
                   "PTRAF", "PRMP", "PTSDF", "PNPL", "PWDIS")]
for(i in 1:ncol(dat.env)){
  temp <- as.numeric(dat.env[, i])
  temp[is.na(temp)] <- 0
  dat.env[, i] <- temp
}
dat.demo <- dat[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT", "LINGISOPCT",
                    "UNDER5PCT", "OVER64PCT")]

dat2 <- cbind(dat.env, dat.demo)

dat3 <- aggregate(dat2, by = list(ID.tract), FUN = "mean")
colnames(dat3)[1] <- "ID"

# # Check to see all tracts covered
sum(as.character(dat3$ID)!=sort(geoid))
intersect(as.character(dat3$ID), geoid)
# # Needs to be 0
# # [1] 0
##########################################
perc.rank <- function(x) trunc(rank(x)) / length(x)
dat4 <- data.frame(ID = as.character(dat3[, 1]),
                   apply(dat3[, -1], 2, perc.rank))
rownames(dat4) <- as.character(dat4$ID)
dat5 <- dat4[geoid, ]
dat.env2 <- rowMeans(dat5[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE",
                              "PRE1960", "PTRAF", "PRMP", "PTSDF", "PNPL",
                              "PWDIS")])
dat.demo2 <- rowMeans(dat5[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT",
                               "LINGISOPCT", "UNDER5PCT", "OVER64PCT")])
dat.EnviroScore <- (dat.env2 * dat.demo2)
# dat.EJ <- (dat.EnviroScore > quantile(dat.EnviroScore)[4])

dat.shiny.percentile <- data.frame(CensusTract = geoid,
                                   # EJcommunity = dat.EJ,
                                   EnviroScore = perc.rank(dat.EnviroScore),
                                   EnvironmentalIndicator = perc.rank(dat.env2),
                                   DemographicIndicator = perc.rank(dat.demo2),
                                   dat5[, -1])
dat6 <- data.frame(ID = as.character(dat3[, 1]),
                   dat3[, -1])
rownames(dat6) <- as.character(dat6$ID)
dat7 <- dat6[geoid, ]

dat.shiny <- data.frame(CensusTract = geoid,
                        # EJcommunity = dat.EJ,
                        EnviroScore = dat.EnviroScore,
                        EnvironmentalIndicator = dat.env2,
                        DemographicIndicator = dat.demo2,
                        dat7[, -1])

# dat.shiny <- dat.shiny[order(dat.shiny$EnviroScore, decreasing = TRUE), ]
row.names(dat.shiny) <- NULL
saveRDS(dat.shiny, "data/ShinyDat_USA.RDS")
# dat.shiny <- readRDS("data/ShinyDat.RDS")
row.names(dat.shiny.percentile) <- NULL
saveRDS(dat.shiny.percentile, "data/ShinyDatPercentile_USA.RDS")
# dat.shiny <- readRDS("data/ShinyDatPercentile.RDS")
##==============================================================================
## BASE MAP
##==============================================================================
## Originally I planned to make a map "m" and then modify it on the fly within
## shiny using "generate_map", I ended up not using these functions, but I
## wanted to keep the syntax for an example.

pal <- colorQuantile("Reds", NULL, n = 9)
# pal <- heat.colors(n = length(dat.EnviroScore))[length(dat.EnviroScore):1][order(dat.EnviroScore)]
m <- leaflet() %>%
  addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
  addPolygons(data=census_tracts, weight=1, fillOpacity=.5, color="black",
              fillColor = ~pal(dat.EnviroScore),
              label=paste0("Census Tract ", census_tracts$GEOID, "\n",
                           "EnviroScore Percentile ",
                           round(perc.rank(dat.EnviroScore), 2)),
              smoothFactor=.02)
m
# generate_map(m, dat, rev(POSSIBLE_DATES)[1], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[2], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[3], "sum")
