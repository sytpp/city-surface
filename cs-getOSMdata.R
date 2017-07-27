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

