# Plumber_FSS.R

# #* @get /mean
# normalMean <- function(samples=10){
#   data <- rnorm(samples)
#   mean(data)
# }
# 
#* @post /sum
# addTwo <- function(a, b){
#   as.numeric(a) + as.numeric(b)
# }

# lat = 43.8138
# lon = -91.2519
# state = "WI"

# curl -H "Content-Type: application/json" --data '{"lat": 43.8138, "lon": -91.2519, "state": "WI"}' localhost:8000/enviro

dat.shiny <- readRDS("/home/IL_EJ_map/data/ShinyDat_USA.RDS")

#* @post /enviro
EnviroInd <- function(lat, lon, state){
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)
  state <- as.character(state)

  State <- readRDS(paste0("/home/IL_EJ_map/data/censustracts/", state, ".Rds")) # tigris::tracts(state, class = "sf")
  state_tracts <- State[, c("GEOID", "TRACTCE")]
  
  # convert the points to same CRS
  my_points_tract <- sf::st_join(sf::st_as_sf(data.frame(x = lon, y = lat) , coords = c("x", "y"),
                                              crs = sf::st_crs(state_tracts)),
                                 state_tracts)$GEOID
  dat.shiny[my_points_tract, "EnvironmentalIndicator"]
}
