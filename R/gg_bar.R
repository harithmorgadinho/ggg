#' @title gg_bar
#' @description A function to plot bar charts.
#' @param input  df/list/raster
#' @param crs 
#' @param zoom_out 
#' @examples
#' gg_bar(df)
#' gg_bar(df)
#' @export
#' 
#' 

gg_bar=function(input,x=NULL,y=NULL,fill=input[,1]){
  
  if (is.null(x)){
  x=input[,1]}
  if (is.null(y)){
  y=input[,2]}
  if (is.null(fill)){
  fill=input[,2]}
  
  ggplot(input, aes(x = x, y = y, fill = fill)) + 
  geom_bar(stat = "identity")
}

#gg_bar(c, x=c$b,y=c$a,fill=c$b)



  #scale_fill_manual(legend_title,values = filling,na.value=na,limits=c(levels(r_df$cuts)),labels=label)+
 # theme(axis.text.x = element_blank(),
        #axis.ticks = element_blank(),
        #axis.title.x = element_blank(),axis.title.y = element_blank())