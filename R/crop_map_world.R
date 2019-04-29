#' @title gg_rasta
#' @description A function to plot biodiversity data.
#' @param input  df or raster
#' @param crs projection of data provided ('longlat'/'cea'/'auto')
#' @examples
#' crop_map_world(df)
#' crop_map_world(df,crs='longlat')
#' @export
crop_map_world=function(input, crs='auto',zoom_out=1){

  worldMap <- ne_countries(scale = 10, type = "map_units", returnclass = 'sf')

  if(any(class(input)=='data.frame')){
    df=input
    df=input[,c(1,2,3)]
    if(grepl(pattern = "(x|long)" , x = colnames(input[2]), ignore.case = T))
      df=input[,c(1,3,2)]
    colnames(df)=c('species','Latitude','Longitude')

  if(crs=='longlat'){

    df_coord=df[,3:2]
    spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    map <- worldMap %>% st_crop(extent(spdf))
   }

  if(crs=='cea') {

    df_coord=df[,3:2]
    spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                     CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))

    map = st_transform(worldMap, '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

    map <- map %>% st_crop(extent(spdf))
  }

  if(crs=='auto'){

    if (max(df$Latitude)^2>1000){
      df_coord=df[,3:2]
      spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                       CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
      map = st_transform(worldMap, '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

      map <- map %>% st_crop(extent(spdf))
    }
    else{
      df_coord=df[,3:2]
      spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                       CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))

      map = st_transform(worldMap, '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

      map <- map %>% st_crop(extent(spdf))

    }
  }#'auto'
  }#'daframe'
  else{
    temp=as.list(bbox(input))
#raster_input

      if ((grepl(pattern = '+proj=cea', x = proj4string(input)))|(temp[[1]]^2>1000))
      {

        worldMap = st_transform(worldMap, '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

      }

      map <- worldMap %>% st_crop(extent(input)*zoom_out)


    }
  return(map)
}
