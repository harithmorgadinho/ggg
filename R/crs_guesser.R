#' @title crs_guesser
#' @description A function to guess crs of input.
#' @param input  df/raster/spdf
#' @examples
#' crs=crs_guesser(input)
#' @export

crs_guesser=function(input){
  if (any(class(input)=='dataframe' )| any(class(input)=='data.frame')){
    if (max(input[,2])^2>100000) {
      crs_input='cea'}
    else {
      crs_input='longlat'
    }
    
  }#df
  
  if (any(class(input)=='RasterLayer')|any(class(input)=='SpatialPointsDataFrame')|any(class(input)=="SpatialPolygonsDataFrame")){
    
    max_ras=as.data.frame(bbox(input))
    
    if (any(max_ras$max^2>100000)) {
      crs_input='cea'}
    else {
      crs_input='longlat'
    }
    
  }#
  
  return(crs_input)
}