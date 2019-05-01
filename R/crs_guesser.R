#' @title crs_guesser
#' @description A function to guess crs of input.
#' @param input  df/raster/spdf
#' @examples
#' crs=crs_guesser(input)
#' @export

crs_guesser=function(input){
  if (any(class(input)=='dataframe' )| any(class(input)=='data.frame')){
    if (max(input[,2])^2>100000) {
    crs='cea'}
  else {
    crs='longlat'
  }

  }#df
  if (any(class(input)=='RasterLayer')){

    max_ras=as.data.frame(bbox(input))

    if (any(max_ras$max^2>100000)) {
    crs='cea'}
  else {
    crs='longlat'
  }

  }#raster

  if (any(class(input)=='SpatialPointsDataFrame')){

    max_ras=as.data.frame(bbox(input))

    if (max(input[,2])^2>100000) {
    crs='cea'}
  else {
    crs='longlat'
  }
}#df
  return(crs)
}
