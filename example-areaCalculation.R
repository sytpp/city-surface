devtools::install_github('osmdatar/osmdata')
library(osmdata)
library(osmar)
library(sp)
library(sf)
library(rgeos)

osmO <- get_osm(center_bbox(-0.167919, 51.5072682, 2000, 2000))

ids_relations <- osmO$relations$tags[osmO$relations$tags$v=="park","id"]
ids_ways <- osmO$ways$tags[osmO$ways$tags$v=="park","id"]
ids_sub <- find_down(osmO, way(c(ids_relations, ids_ways)))
sp_sub_park <- as_sp(subset(osmO, ids = ids_sub), "polygons")

spplot(sp_sub_park, c("version"), colorkey = FALSE, col.regions=c('green'))

## area in the '@area' slot
a1 <- sapply(sp_sub_park@polygons, function(x) x@area)

## area from projection 1
bg_poly_t2 <- spTransform(sp_sub_park, CRS("+proj=longlat +datum=WGS84"))
a2 <- rgeos::gArea(bg_poly_t2, byid=TRUE)

## from stackoverflow
## https://stackoverflow.com/questions/44549920/osm-rgeos-osmar-area-calculation-does-not-add-up/44554351#44554351

## area from projection in British National Grid
bg_poly_t3 <- spTransform(sp_sub_park, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "))
a3 <- rgeos::gArea(bg_poly_t3, byid = TRUE)

## area using 'spherical geometry'
bg_poly_t4 <- st_as_sf(sp_sub_park)
a4 <- st_area(bg_poly_t4)
sum(a4)





