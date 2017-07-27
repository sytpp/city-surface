library (magrittr)
dath <- opq('city of london uk') %>% add_osm_feature(key = 'highway', value='primary') %>%
  osmdata_sf (quiet = FALSE)
datg <- opq('city of london uk') %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf (quiet = FALSE)
bb <- getbb ('city of london uk')

install_github("ropensci/osmplotr")
library (osmplotr)
map <- osm_basemap (bbox = bb, bg = "gray95") %>%
  add_osm_objects (datg$osm_polygons, col = "lightgreen") %>%
  add_osm_objects (dath$osm_lines, col = "gray50") %>%
  print_osm_map ()

bb_poly <- getbb ('city of london uk', format_out = 'polygon')
dath <- trim_osmdata (dath, bb_poly, exclude = FALSE)
datg <- trim_osmdata (datg, bb_poly, exclude = FALSE)

dath <- trim_osmdata (dath, bb_poly, exclude = TRUE)
datg <- trim_osmdata (datg, bb_poly, exclude = TRUE)
