library(sp)
library(maps)
library(maptools)

create_world_map <- function(){
  world <- map("world", fill=TRUE, plot=FALSE)
  world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
  world_map <- SpatialPolygonsDataFrame(world_map,
                                        data.frame(country=names(world_map), 
                                                   stringsAsFactors=FALSE), 
                                        FALSE)
  world_map$iso3 <- countrycode(world_map$country, "country.name", "iso3c", warn=FALSE)
  world_map <- subset(world_map, !is.na(iso3))
}