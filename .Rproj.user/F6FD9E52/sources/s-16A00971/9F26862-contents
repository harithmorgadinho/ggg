#' @title gg_bee
#' @description A function to plot biodiversity data in grids.
#' @param icon_color  choose color to change icon
#' @param filling choose pallete to color the diferent levels
#' @param na color for NA cells
#' @param  map map to plot on top of input
#' @param  img forg/lizard/bird/mammal
#' @param  y_sf y position for icon
#' @param  x_sf x position for icon
#' @param  size size of icon
#' @param  breaks choose breaks for color legend
#' @param  breaks_n number of breaks for color legend
#' @param  scal_cont TRUE for gradient scale
#' @param  low color for gradient scale
#' @param  high color for gradient scale
#' @param  alpha icon transparency 0-1
#' @param  map_col color for map border
#' @param  panel_background color for plot background
#' @param  width width of map
#' @param  breaks_x choose breaks for x axis
#' @param breaks_y=choose breaks for y axis)
#' @param labels information to display in the legend
#' @param resolution grid resolution
#' @param crs projection of data provided ('longlat'/'cea')
#' @param zoom_out choose the best zoom to visualize your data
#' @param cellsize chose size of grids - default is 2
#' @param proj_b TRUE if using Berhman projection - default: FALSE
#' @param break_hex Decide if you want the grid to be cropped by countries borders, default - NULL
#' @param hex_col hexagon color defualt -'black'
#' @param hex_width hexagon border width, default - 0.5
#' @param square TRUE if square grid, FALSE produces an hexagon grid, default - FALSE
#'
#' @examples
#' gg_rasta(input,icon_color='blue',legend_title="WEGE",filling=c(pal(7),palred(5)), na='red',map=africa_shape_sp,
#' img=img,y_sf=-20,x_sf=-10,size=10,breaks=c(0,1,2,3,4))
#'
#' gg_rasta(input)
#' @export
gg_bee=function(input,
                density=NULL,
                icon_color="black",
                legend_title="input",
                filling = NULL,
                na ='white',
                map = NULL,
                img= NULL,
                y_sf = NULL,
                x_sf = NULL,
                breaks = NULL,
                size=NULL,
                scal_cont = NULL,
                break_n = 7,
                low = "lightgoldenrod",
                high = "darkgoldenrod",
                alpha=0.7,
                map_col='grey30',
                panel_background='grey95',
                width=0.25,
                breaks_x=NULL,
                breaks_y=NULL,
                map_title=NULL,
                proj_b=NULL,
                cellsize=2,
                break_hex=NULL,
                hex_col='black',
                hex_width=0.5,
                square=NULL)
{

  map_spdf=crop_map_spdf_maker(input)
  map=map_spdf[[1]]
  sp_as_sf=map_spdf[[3]]


  if(is.null(break_hex)){


    map=st_union(map)
  }

  if ((grepl(pattern = '+proj=cea', x = proj4string(input)))|(temp[[1]]^2>1000))
  {
    cellsize=100000*cellsize
  }

  if(is.null(square)){
  CRGrid <- map %>%
    st_make_grid(cellsize = cellsize,square = FALSE ) %>%
    st_intersection(map) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())
  }else{
    CRGrid <- map %>%
      st_make_grid(cellsize = cellsize,square = TRUE ) %>%
      st_intersection(map) %>%
      st_cast("MULTIPOLYGON") %>%
      st_sf() %>%
      mutate(cellid = row_number())
  }


  richness_grid2 <- st_join(CRGrid,sp_as_sf)
  richness_grid2=na.omit(richness_grid2)
  richness_grid2=group_by(richness_grid2,cellid)
  richness_grid2=plyr::summarize(richness_grid2,num_species = n())

  if (is.null(breaks)){
    x=c(min(na.omit(richness_grid2$num_species)),max(na.omit(richness_grid2$num_species)))
    breaks=seq(min(x), max(x),len=break_n)
  }


  if (is.null(img)){
    img <- image_data("b199a5f5-20c4-4cc9-9c54-1b51578c2487", size = "512")[[1]]
  }

  if(is.null(size)){
    temp=as.list(st_bbox(map))
    size=(temp$xmax-temp$xmin)/4
  }
  if (!is.null(img) && img == 'frog'){
    img <- image_data("43497e8a-45e7-4fa2-a8a0-ffadac8401fc", size = "512")[[1]]}

  if (!is.null(img) && img == 'bird'){
    img <- image_data("0fc00dbf-322f-48b5-8b41-459bde803693", size = "512")[[1]]}

  if (!is.null(img) && img == 'lizard'){
    img <- image_data("f2c8db98-c34c-4140-a868-029bf4b557b1", size = "512")[[1]]}
  if (!is.null(img) && img == 'fox'){
    img <- image_data("f2c8db98-c34c-4140-a868-029bf4b557b1", size = "512")[[1]]}

  if (is.null(y_sf)){
    temp=as.list(st_bbox(map))
    y_sf=temp$ymin+((temp$ymax-temp$ymin)*0.15)}


  if (is.null(x_sf)){
    temp=as.list(st_bbox(map))
    x_sf=temp$xmin+((temp$xmax-temp$xmin)*0.15) }


  if (is.null(filling)){

    temp_pallette <- colorRampPalette(c(low,high ))
    filling=temp_pallette(break_n)}


  if(!is.null(proj_b)){

    richness_grid2 <- st_transform(richness_grid2, '+proj=cea +lat_ts=30')

  }


  if(is.null(scal_cont)) {
    ###################################



    richness_grid2$cuts=cut(richness_grid2$num_species,
                            breaks= breaks)

    temp=as.list(st_bbox(map))


    p=ggplot(richness_grid2)+
      geom_sf(data=map,fill=map_col,size=0.1)+
      geom_sf(aes(fill=richness_grid2$cuts),color=hex_col,size=hex_width)+
      scale_fill_manual(legend_title,values = filling,na.value=na)+
      theme_grey()+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background = element_rect(fill =panel_background))+
      scale_x_continuous(breaks= breaks_x)+
      scale_y_continuous(breaks= breaks_y)+
      add_phylopic(img, alpha = alpha, x = x_sf, y = y_sf, ysize = size,
                   color = icon_color)+
      ggtitle(map_title)+
      theme(plot.title = element_text(hjust = 0.5))

  }
  else {
    p <-
      ggplot(richness_grid2)+
      geom_sf(data=map,fill=map_col,size=0.1)+
      geom_sf(aes(fill=num_species),color=hex_col,size=hex_width)+
      scale_fill_continuous(legend_title,low = low, high = high, na.value=na)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background = element_rect(fill =panel_background))+
      scale_x_continuous(breaks= breaks_x)+
      scale_y_continuous(breaks= breaks_y)+
      add_phylopic(img, alpha = alpha, x = x_sf, y = y_sf, ysize = size,
                   color = icon_color)+
      ggtitle(map_title)+
      theme(plot.title = element_text(hjust = 0.5))
  }

  print(p)

}

