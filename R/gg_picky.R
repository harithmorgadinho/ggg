#' @title gg_picky
#' @description A function to plot your map in 131 different color palettes.
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
#' @param label information to display in the legend
#' @param resolution grid resolution
#' @param crs projection of data provided ('longlat'/'cea')
#' @param crs projection of data provided ('longlat'/'cea')
#' @param zoom_out choose the best zoom to visualize your data. Higher the number - the more perspective you get.
#' @examples
#' gg_picky(input,icon_color='blue',legend_title="WEGE",filling=c(pal(7),palred(5)), na='red',map=africa_shape_sp,
#' img=img,y_sf=-20,x_sf=-10,size=10,breaks=c(0,1,2,3,4))
#'
#' gg_picky(input, pal='qual')
#' @export

gg_picky=function (input,pal='qual',show_c_names=NULL,
                   gfun='richness',
                   icon_color="black",
                   legend_title=NULL,
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
                   low = "yellow",
                   high = "blueviolet",
                   alpha=0.7,
                   map_col='grey30',
                   panel_background='grey95',
                   width=0.25,
                   breaks_x=NULL,
                   breaks_y=NULL,
                   map_title=NULL,
                   label=NULL,
                   resolution=1,
                   crs=crs,
                   exclude_0='auto',
                   zoom_out=zoom_out,
                   round_to=3){

  #create pallete lists
  #brewer
  #redmonder
  #weanderson
  #ggci

  color_palletes <- system.file('color_palletes.RDS', package = 'ggg')
  load(color_palletes)


  if (pal=='seq'){
    #list_sequential_palettes
    list_palletes=c(brewer_seq,redmonder_seq,ggsci_seq)
    names=names(list_palletes)
  }

  if (pal=='div'){
    #list_diverging_palettes
    list_palletes=c(brewer_div,redmonder_div,ggsci_div_f)
    names=names(list_palletes)
  }
  if (pal=='qual'){
    #list_qualitative_palettes
    list_palletes=c(brewer_qual,redmonder_qual,wesanderson,ggsci_qual)
    names=names(list_palletes)
  }

  if (pal=='all'){
    list_palletes=c(brewer_seq,redmonder_seq,ggsci_seq,brewer_div,
                    redmonder_div,
                    ggsci_div_f,brewer_qual,
                    redmonder_qual,
                    wesanderson,
                    ggsci_qual)
    names=names(list_palletes)
  }
  #############################

  pallette_final=vector("list", length(list_palletes))

  for (i in seq_along(list_palletes)){

    pal_temp=list_palletes[[i]]

    palette_colors=round(seq(1,length(pal_temp),len=break_n))

    pallette_sub=vector("list", length(palette_colors))

    for (j in seq_along(palette_colors)){

      color_n=palette_colors[j]

      pallette_sub[j]=pal_temp[[color_n]]

    }
    pallette_final[[i]]=unlist(pallette_sub)
  }
  names(pallette_final)=names


  pdf('pick_your_colors.pdf')

  for (k in seq_along(pallette_final)){

    pal_c=pallette_final[[k]]

    cat('.... .... .... palette [', k, '/', length(pallette_final), ']\n',
        sep = '')



    if(!is.null(show_c_names)){


      gg_rasta(input,legend_title=paste0(names[[k]]),label = pal_c,filling = pal_c,break_n = break_n,label = pal
               ,gfun=gfun,
               icon_color=icon_color,
               na =na,
               map = map,
               img= img,
               y_sf = y_sf,
               x_sf = x_sf,
               breaks = breaks,
               size=size,
               scal_cont = scal_cont,
               low = low,
               high = high,
               alpha=alpha,
               map_col=map_col,
               panel_background=panel_background,
               width=width,
               breaks_x=breaks_x,
               breaks_y=breaks_y,
               map_title=map_title,
               resolution=resolution,
               crs=crs,
               zoom_out=zoom_out,
               exclude_0='auto',
               round_to=3
               )





    }
else {
    gg_rasta(input,legend_title=paste0(names[[k]]),filling = pal_c, break_n = break_n,gfun=gfun,label = NULL ,icon_color=icon_color,na =na,map = map,img= img,y_sf = y_sf,x_sf = x_sf,breaks = breaks,size=size,scal_cont = scal_cont,low = low,high = high,alpha=alpha,map_col=map_col,panel_background=panel_background,width=width,breaks_x=breaks_x,breaks_y=breaks_y,map_title=map_title,resolution=resolution,crs=crs,zoom_out=zoom_out,exclude_0='auto',
             round_to=3)


  }
  }
  dev.off()
}
