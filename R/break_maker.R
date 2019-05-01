#' @title break_maker
#' @description A function to make breaks for plotting.
#' @param input  raster
#' @param exclude_0 in case you want 0 values to be treated as NA
#' @param round_to rounding breaks to
#' @param break_n number of breaks
#' @examples
#' crop_map_world(df)
#' crop_map_world(df,crs='longlat')
#' @export


break_maker=function(input,exclude_0=NULL,round_to=0,break_n=7){
  
if(is.infinite(min(na.omit(values(input))))){
  input[is.infinite(values(input))]=NA}

if (exclude_0=='auto'){
  
  if(min(na.omit(input[input]))==0){
    
    x=c(min(na.omit(input[input>0])),max(na.omit(input[input])))
    
    breaks=unique(round(seq(min(x)+min(x)*0.01, max(x)+max(x)*0.01,len=break_n),round_to))}
  else{
    x=c(min(na.omit(input[input])),max(na.omit(input[input])))
    breaks=unique(round(seq(min(x)+min(x)*0.01, max(x)+max(x)*0.01,len=break_n),round_to))
    
  }
  
}
if (exclude_0=='yes'){
  
  x=c(min(na.omit(input[input>0])),max(na.omit(input[input])))
  breaks=unique(round(seq(min(x)+min(x)*0.01, max(x)+max(x)*0.01,len=break_n),round_to))
  
  
}
if (exclude_0=='no'){
  x=c(min(na.omit(input[input])),max(na.omit(input[input])))
  breaks=unique(round(seq(min(x)+min(x)*0.01, max(x)+max(x)*0.01,len=break_n),round_to))
}
  return(breaks)
}
