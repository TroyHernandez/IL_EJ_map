# dataMake.R

##
## GWL 2016-10-10
## Putting 20_Map_dev_v1.R into map_demo.Rmd
##

##==============================================================================
## INITIALIZE
##==============================================================================

library(leaflet)
source("functions/download_census_tracts.R")

##==============================================================================
## LOAD DATA
##==============================================================================

# Download from: ftp://newftp.epa.gov/EJSCREEN/2017/
dat <- read.csv("data/EJSCREEN_2017_USPR_Public.csv", stringsAsFactors = FALSE)
census_tracts <- download_census_tracts(infile = "data/censustracts_IL.Rds")

##==============================================================================
## MAPBOX KEY
## Register at mapbox.com and create a map... or use the one I made
##==============================================================================
MAPBOX_STYLE_TEMPLATE <- paste0("https://api.mapbox.com/styles/v1/coc375492/",
                                "cirqd7mgf001ygcnombg4jtb4/tiles/256/{z}/{x}/{y}",
                                "?access_token=pk.eyJ1IjoiY29jMzc1NDkyIiwiYSI6ImN",
                                "pcnBldzVqMTBmc3J0N25rZTIxZ3ludDIifQ.DgJIcLDjC1h9MtT8CaJ-pQ")
mb_attribution <- paste("© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> ",
                        "© <a href='http://www.openstreetmap.org/about/'>OpenStreetMap</a>")

##==============================================================================
## AGGREGATE EJ DATA
##==============================================================================

## Here's the data without aggregating:
IL.ind <- which(substr(as.character(dat$ID), 1, 2) == "17")
dat.IL <- dat[IL.ind, ]

ID.tract <- substr(as.character(dat.IL$ID), 1, 11)
dat.env <- dat.IL[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE", "PRE1960",
                      "PTRAF", "PRMP", "PTSDF", "PNPL", "PWDIS")]
for(i in 1:ncol(dat.env)){
  temp <- as.numeric(dat.env[, i])
  temp[is.na(temp)] <- 0
  dat.env[, i] <- temp
}
dat.demo <- dat.IL[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT", "LINGISOPCT",
                       "UNDER5PCT", "OVER64PCT")]

dat2 <- cbind(dat.env, dat.demo)
dat3 <- aggregate(dat2, by = list(ID.tract), FUN = "mean")
colnames(dat3)[1] <- "ID"

# Check to see all tracts covered
sum(as.character(dat3$ID)!=sort(census_tracts$GEOID))
# Needs to be 0
# [1] 0
##########################################
perc.rank <- function(x) trunc(rank(x)) / length(x)
dat4 <- data.frame(ID = as.character(dat3[, 1]),
                   apply(dat3[, -1], 2, perc.rank))
rownames(dat4) <- as.character(dat4$ID)
dat5 <- dat4[census_tracts$GEOID, ]
dat.env2 <- rowMeans(dat5[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE",
                              "PRE1960", "PTRAF", "PRMP", "PTSDF", "PNPL",
                              "PWDIS")])
dat.demo2 <- rowMeans(dat5[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT",
                               "LINGISOPCT", "UNDER5PCT", "OVER64PCT")])
dat.EnviroScore <- (dat.env2 * dat.demo2)
dat.EJ <- (dat.EnviroScore > quantile(dat.EnviroScore)[4])

dat.shiny.percentile <- data.frame(CensusTract = census_tracts$GEOID,
                                   EJcommunity = dat.EJ,
                                   EnviroScore = perc.rank(dat.EnviroScore),
                                   EnvironmentalIndicator = perc.rank(dat.env2),
                                   DemographicIndicator = perc.rank(dat.demo2),
                                   dat5[, -1])
dat6 <- data.frame(ID = as.character(dat3[, 1]),
                   dat3[, -1])
rownames(dat6) <- as.character(dat6$ID)
dat7 <- dat6[census_tracts$GEOID, ]

dat.shiny <- data.frame(CensusTract = census_tracts$GEOID,
                        EJcommunity = dat.EJ,
                        EnviroScore = dat.EnviroScore,
                        EnvironmentalIndicator = dat.env2,
                        DemographicIndicator = dat.demo2,
                        dat7[, -1])

# dat.shiny <- dat.shiny[order(dat.shiny$EnviroScore, decreasing = TRUE), ]
row.names(dat.shiny) <- NULL
saveRDS(dat.shiny, "data/ShinyDat.RDS")
# dat.shiny <- readRDS("data/ShinyDat.RDS")
row.names(dat.shiny.percentile) <- NULL
saveRDS(dat.shiny.percentile, "data/ShinyDatPercentile.RDS")
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
