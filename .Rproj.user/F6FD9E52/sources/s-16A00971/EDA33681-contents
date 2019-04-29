projection_to_raster=function(input, crs='auto'){

if(crs=='longlat'){

  df_coord=df[,3:2]
  spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                   CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  worldMap <- ne_countries(scale = 10, type = "map_units", returnclass = 'sf')
  map <- worldMap %>% st_crop(extent(spdf))

  r=raster(ncol=84, nrow=77,extent(spdf)*zoom_out, resolution=resolution,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  }

if(crs=='cea') {

  df_coord=df[,3:2]
  spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                   CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))

  worldMap <- ne_countries(scale = 10, type = "map_units", returnclass = 'sf')
  map <- worldMap %>% st_crop(extent(spdf))

  r=raster(ncol=84, nrow=77,extent(spdf)*zoom_out, resolution=resolution*100000,crs=CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
}

if(crs=='auto'){

  if (max(df$Latitude)^2>1000){
    df_coord=df[,3:2]
    spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                     CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))

    r=raster(ncol=84, nrow=77,extent(spdf)*zoom_out, resolution=resolution*100000,crs=CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
  }
}
}
