##
## Gene 3/22/2018: 
## This function creates a smaller version of the original using gsimplify
##


simplify_map_data <- function(infile = "data/censustracts_IL.Rds",
                              outfile = "data/censustracts_IL_gsimplified.Rds",
                              tol = .0005){
  
    require(rgdal) #for reading/writing geo files
    require(rgeos) #for simplification
    require(sp)
    
    ## Simplify census block file
    map_data_orig <- readRDS(infile)
    map_data_mod <- gSimplify(map_data_orig, tol=tol)
    
    map_data_mod <- SpatialPolygonsDataFrame(map_data_mod, 
                                             map_data_orig@data, 
                                             match.ID = TRUE)
    
    
    saveRDS(map_data_mod, outfile)
    
    return()
}
