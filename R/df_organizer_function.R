#df_organizer
#' @title df_organizer
#' @description A function to organize df into value, y, x form.
#' @param input  df
#' @examples
#' df=df_organizer(input)
#' @export

df_organizer=function(input){
df=input
df=input[,c(1,2,3)]
if(grepl(pattern = "(x|long)" , x = colnames(input[2]), ignore.case = T))
  df=input[,c(1,3,2)]
colnames(df)=c('species','Latitude','Longitude')
return(df)
}
