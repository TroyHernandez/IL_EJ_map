# CensusTractLatLon.R
# https://stackoverflow.com/questions/52248394/get-census-tract-from-lat-lon-using-tigris/52260988
# library(tigris)
# library(sf)

WI <- tigris::tracts("WI", class = "sf")
wi_tracts <- WI[, c("GEOID", "TRACTCE")]

lat <- 43.8138
lon <- -91.2519

# convert the points to same CRS
my_points_tract <- sf::st_join(sf::st_as_sf(data.frame(x = lon, y = lat) , coords = c("x", "y"),
                                    crs = sf::st_crs(wi_tracts)),
                           wi_tracts)
