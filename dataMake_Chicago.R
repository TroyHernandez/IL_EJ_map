# dataMake.R
library(leaflet)

##==============================================================================
## LOAD DATA
##==============================================================================

# Download from: ftp://newftp.epa.gov/EJSCREEN/2017/
dat <- read.csv("data/EJSCREEN_2017_USPR_Public.csv", stringsAsFactors = FALSE)
chicago_tracts <- geojsonio::geojson_read("data/ChicagoCensusTracts.geojson",
                                          what = "sp")

##==============================================================================
## AGGREGATE EJ DATA
##==============================================================================

## Here's the data without aggregating:
Chi.ind <- which(substr(as.character(dat$ID), 1, 11) %in% as.character(chicago_tracts$geoid10))

# IL.ind <- which(substr(as.character(dat$ID), 1, 2) == "17")
dat.Chi <- dat[Chi.ind, ]

ID.tract <- substr(as.character(dat.Chi$ID), 1, 11)
dat.env <- dat.Chi[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE", "PRE1960",
                      "PTRAF", "PRMP", "PTSDF", "PNPL", "PWDIS")]
for(i in 1:ncol(dat.env)){
  temp <- as.numeric(dat.env[, i])
  temp[is.na(temp)] <- 0
  dat.env[, i] <- temp
}
dat.demo <- dat.Chi[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT", "LINGISOPCT",
                       "UNDER5PCT", "OVER64PCT")]

dat2 <- cbind(dat.env, dat.demo)

# This takes the average of census tracts with multiple entries
dat3 <- aggregate(dat2, by = list(ID.tract), FUN = "mean")
colnames(dat3)[1] <- "ID"

# Check to see all tracts covered
sum(as.character(dat3$ID)!=sort(chicago_tracts$geoid10))
# Needs to be 0
# [1] 0
##########################################
# Calculating Perc.rank
perc.rank <- function(x) trunc(rank(x)) / length(x)
# quantile.score <- function(x) round(rank(x) / length(x) / 5 * 100)
dat4 <- data.frame(ID = as.character(dat3[, "ID"]),
                   apply(dat3[, -1], 2, perc.rank))
rownames(dat4) <- as.character(dat4$ID)

dat5 <- dat4[chicago_tracts$geoid10, ]
dat.env2 <- rowMeans(dat5[, c("CANCER", "RESP", "DSLPM", "PM25", "OZONE",
                              "PRE1960", "PTRAF", "PRMP", "PTSDF", "PNPL",
                              "PWDIS")])

dat.demo2 <- rowMeans(dat5[, c("LOWINCPCT", "MINORPCT", "LESSHSPCT",
                               "LINGISOPCT", "UNDER5PCT", "OVER64PCT")])
dat.EnviroScore <- (dat.env2 * dat.demo2)
dat.EJ <- (dat.EnviroScore > quantile(dat.EnviroScore)[4])

dat.shiny.percentile <- data.frame(CensusTract = chicago_tracts$geoid10,
                                   EJcommunity = dat.EJ,
                                   EnviroScore = perc.rank(dat.EnviroScore),
                                   EnvironmentalIndicator = perc.rank(dat.env2),
                                   DemographicIndicator = perc.rank(dat.demo2),
                                   dat5[, -1])
dat6 <- data.frame(ID = as.character(dat3[, 1]),
                   dat3[, -1])
rownames(dat6) <- as.character(dat6$ID)
dat7 <- dat6[chicago_tracts$geoid10, ]

dat.shiny <- data.frame(CensusTract = chicago_tracts$geoid10,
                        EJcommunity = dat.EJ,
                        EnviroScore = dat.EnviroScore,
                        EnvironmentalIndicator = dat.env2,
                        DemographicIndicator = dat.demo2,
                        dat7[, -1])

# dat.shiny <- dat.shiny[order(dat.shiny$EnviroScore, decreasing = TRUE), ]
row.names(dat.shiny) <- NULL
saveRDS(dat.shiny, "data/ChicagoShinyDat.RDS")
# dat.shiny <- readRDS("data/ShinyDat.RDS")
row.names(dat.shiny.percentile) <- NULL
saveRDS(dat.shiny.percentile, "data/ChicagoShinyDatPercentile.RDS")
