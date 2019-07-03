
fips <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

# Download all of the census tract shape files 
for(i in 1:50){
  # census_tracts <- tigris::tracts(state = fips$fips[i])
  saveRDS(tigris::tracts(state = fips$fips[i], class = "sf"),
          file = paste0("/home/IL_EJ_map/data/censustracts/", fips$state_abbr[i], ".Rds"))
}

# also Washington DC
saveRDS(tigris::tracts(11, class = "sf"),
        file = paste0("/home/IL_EJ_map/data/censustracts/", "DC", ".Rds"))
