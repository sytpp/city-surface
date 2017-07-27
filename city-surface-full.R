setwd("~/XCHANGE/FCC/ETC-citysurface/city-surface")

require(devtools)
install_github('ropensci/osmdata')
library(osmdata)
library(sp)
library(sf)
library(rgeos)

## #########################################################
## helper function to get features which intersect with a bounding box

get_osm_features_byBB_multi <- function(cityName, key_value_df){
  if(exists("city_feature")){
    rm(city_feature)
  }
  
  key_value_df <- as.data.frame(t(key_value_df))
  
  for(i in seq(1, dim(key_value_df)[1])){
    key = as.character(key_value_df[i,"V1"])
    value = as.character(key_value_df[i,"V2"])
    
    q <- opq(bbox = cityName)
    q <- add_osm_feature(q, key=key, value=value, key_exact = FALSE, value_exact = FALSE, match_case = FALSE)
    q_sf <- osmdata_sf(q)
    
    
    if(length(q_sf$osm_points)>0){
      print(paste("X : ", key, value, sep=" "))
      
      if(exists("city_feature")){
        city_feature <- c(city_feature, q_sf)
      }else{
        city_feature <- q_sf
      }
      
    }else{
      print(paste("O : ", key, value, sep=" "))
      next
    } 
  }
  return(city_feature)  
}

## #########################################################
## pick and choose the features of interest

cityName = "hackney london uk"

# PARKS
parkQ = data.frame( d1 = c("leisure","park"),
                    d2 = c("leisure","playground"),
                    d3 = c("leisure","garden"),
                    d4 = c("leisure","nature_reserve"))
parks <- get_osm_features_byBB_multi(cityName,parkQ)
plot(parks$osm_polygons$geometry, axes=T)

# STREETS
streetQ = data.frame( d1 = c("highway","."))
streets <- get_osm_features_byBB_multi(cityName,streetQ)
plot(streets$osm_lines$geometry)

# WATERS
waterQ = data.frame( d1 = c("natural","water"),
                     d3 = c("water", "river"))
waters <- get_osm_features_byBB_multi(cityName,waterQ)
plot(waters$osm_multipolygons$geometry)

# BUILDINGS
buildQ = data.frame( d1 = c("building","."))
buildings <- get_osm_features_byBB_multi(cityName,buildQ)
plot(buildings$osm_polygons$geometry)

# PARKING
parkingQ = data.frame(d1 = c("amenity","parking"))
parking <- get_osm_features_byBB_multi(cityName,parkingQ)
plot(parking$osm_polygons$geometry)

## #########################################################
## plot

library(ggplot2)

q <- opq(bbox = cityName)
bb = data.frame(ymin = as.numeric(as.character(strsplit(q$bbox, ",")[[1]][1])),
                xmin = as.numeric(as.character(strsplit(q$bbox, ",")[[1]][2])),
                ymax = as.numeric(as.character(strsplit(q$bbox, ",")[[1]][3])),
                xmax = as.numeric(as.character(strsplit(q$bbox, ",")[[1]][4])))

sp_street <- as(streets$osm_lines$geometry,"Spatial")
sp_street_f <- fortify(sp_street)
sp_street_f$col <- "0"

sp_leisure <- as(parks,"Spatial")
sp_leisure_f <- fortify(sp_leisure)
sp_leisure_f$col <- "1"

sp_water <- as(waters,"Spatial")
sp_water_f <- fortify(sp_water)
sp_water_f$col <- "2"

sp_buildings <- as(buildings,"Spatial")
sp_buildings_f <- fortify(sp_buildings)
sp_buildings_f$col <- "3"

sp_parking <- as(parking,"Spatial")
sp_parking_f <- fortify(sp_parking)
sp_parking_f$col <- "4"

## #########################################################
## calculate the total area 

q <- opq(bbox = cityName)
# https://gis.stackexchange.com/questions/206929/r-create-a-boundingbox-convert-to-polygon-class-and-plot

x1 = as.numeric(strsplit(q$bbox,",")[[1]][1])
y1 = as.numeric(strsplit(q$bbox,",")[[1]][2])
x2 = as.numeric(strsplit(q$bbox,",")[[1]][3])
y2 = as.numeric(strsplit(q$bbox,",")[[1]][4])
coords = matrix(c(x1, y1, x1, y2,x2, y2,x2, y1,x1, y1), ncol = 2, byrow = TRUE)

bbox = Polygon(coords)
bbox_sp = SpatialPolygons(list(Polygons(list(bbox), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
bbox_area <- st_area(st_as_sf(bbox_sp))[[1]]
paste("bounding box total area: ",as.character(round(bbox_area/10^6,digits = 1)), " km^2",sep="")
#plot(bbox_sp, axes = TRUE)

## #########################################################
## calculate the percentage of sub areas

lane_width_m = 3
nr_lanes = 4

city_street_area <- sum(st_length(st_as_sf(streets)))[[1]]*lane_width_m*nr_lanes
city_leisure_area <- sum(st_area(st_as_sf(parks)))[[1]]
city_water_area <- sum(st_area(st_as_sf(waters)))[[1]]
city_build_area <- sum(st_area(st_as_sf(buildings)))[[1]]
city_parking_area <- sum(st_area(st_as_sf(parking)))[[1]]

aoi <- city_parking_area
paste("area of interest: ",as.character(round(aoi/10^6,digits = 1)), " km^2",
      " = ", as.character(round(aoi/bbox_area*100,digits=1)), "% of total area", sep="")

## #########################################################
## TREEMAP
## https://github.com/wilkox/treemapify
## http://mlbernauer.github.io/R/20150309_treemaps_with_ggplot2.html

myturf <- data.frame(name = c("streets", "buildings","parking","leisure", "water"),
           area = c(city_street_area, city_build_area, city_parking_area, city_leisure_area, city_water_area))
myturf <- rbind.data.frame(myturf, data.frame(name="therest", area=bbox_area-sum(myturf$area)))
myturf$name <- factor(myturf$name, levels =  c("streets", "buildings", "parking","leisure", "water","therest"))

library(treemapify)
treemap_coords <- treemapify(myturf, area="area", fill="name", label="name")
treemap_coords_trans = treemap_coords

treemap_coords_trans$xmin = ((treemap_coords$xmin / 100) * (bb$xmax-bb$xmin)) + bb$xmin
treemap_coords_trans$xmax = ((treemap_coords$xmax / 100) * (bb$xmax-bb$xmin)) + bb$xmin
treemap_coords_trans$ymin = ((treemap_coords$ymin / 100) * (bb$ymax-bb$ymin)) + bb$ymin
treemap_coords_trans$ymax = ((treemap_coords$ymax / 100) * (bb$ymax-bb$ymin)) + bb$ymin


# cols      darkdark    dark       red   green     blue      white
mapcols <- c("#323232","darkgrey","red","#b2df8a","#1f78b4","white")

p1 <- ggplot() + 
  geom_rect(data=bb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, col="red", cex=2) +
  geom_polygon(data=sp_leisure_f, aes(x = long, y = lat, group = group), fill=mapcols[4], col=NA) +
  geom_polygon(data=sp_buildings_f, aes(x = long, y = lat, group = group), fill=mapcols[2], col=NA) +
  geom_polygon(data=sp_water_f, aes(x = long, y = lat, group = group), fill=mapcols[5], col=NA) +
  geom_polygon(data=sp_parking_f, aes(x = long, y = lat, group = group), fill=mapcols[3], col=NA) +
  geom_path(data=sp_street_f,aes(x = long, y = lat, group = group), col=mapcols[1]) +
  theme_nothing() +
  coord_quickmap(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = TRUE)

p2 <- ggplot() + 
  geom_rect(data=bb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, col="red", cex=2) +
  geom_rect(data=treemap_coords_trans, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),color="white") +
  scale_fill_manual(values=mapcols) +
  theme_nothing() + guides(fill=FALSE) +
  coord_quickmap(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = TRUE)


plot_grid(p1, p2)

## STUFF TO DEAL WITH

## what is the actual street width for various street types?
## bounding box vs bounding polygon
## strictly overlap


