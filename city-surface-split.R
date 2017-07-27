setwd("~/XCHANGE/FCC/ETC-citysurface/city-surface")

#require(devtools)
#install_github('ropensci/osmdata')
library(osmdata)
library(sp)
library(sf)
library(rgeos)

source("get-osm-data.R")
source("build-treemap.R")