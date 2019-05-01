
#' @title gg_rasta
#' @description A function to plot biodiversity data.
#' @param icon_color  choose color to change icon
#' @param filling choose pallete to color the diferent levels
#' @param na color for NA cells
#' @param gfun = 'richness'/'sum'/'density'
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
#' @param zoom_out choose the best zoom to visualize your data. Higher the number - the more perspective you get.
#' @examples
#' gg_rasta(input,icon_color='blue',legend_title="WEGE",filling=c(pal(7),palred(5)), na='red',map=africa_shape_sp,
#' img=img,y_sf=-20,x_sf=-10,size=10,breaks=c(0,1,2,3,4))
#'
#' gg_rasta(input)
#' @export

gg_rasta=function(input,
                  gfun='richness',
                  icon_color="black",
                  legend_title="input",
                  filling = NULL,
                  na ='white',
                  map = NULL,
                  img= 'mammal',
                  y_sf = NULL,
                  x_sf = NULL,
                  breaks = NULL,
                  size=NULL,
                  scal_cont = NULL,
                  break_n = 7,
                  low = "khaki1",
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
                  zoom_out=1,
                  exclude_0='auto',
                  round_to=3,
                  map_bg=NA,
                  invert_palette=NULL,
                  return='map',
                  rem_map_leg=NULL,
                  rem_hist_leg=NULL,add_values_hist=NULL,
                  silent=NULL)
{

  color_palletes <- system.file('color_palletes.RDS', package = 'ggg')
  load(color_palletes)
  palette=c(brewer_seq,redmonder_seq,ggsci_seq,brewer_div,
                  redmonder_div,
                  ggsci_div_f,brewer_qual,
                  redmonder_qual,
                  wesanderson,
                  ggsci_qual)

  crs=crs_guesser(input)

  if(any(class(input)=='data.frame')){
    df=df_organizer(input)
  }

    if(crs=='longlat'){

if(any(class(input)=='data.frame')){

      df_coord=df[,3:2]
      spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))}

if(any(class(input)=='SpatialPointsDataFrame')){
  spdf=input
      r=raster(ncol=84, nrow=77,extent(spdf)*zoom_out, resolution=resolution,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))}
}
    if(crs=='cea') {
      if(any(class(input)=='data.frame')){
      df_coord=df[,3:2]
      spdf <- SpatialPointsDataFrame(df_coord, df,proj4string =
                                       CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
      }
      if(any(class(input)=='SpatialPointsDataFrame')){
        spdf=input
        r=raster(ncol=84, nrow=77,extent(spdf)*zoom_out, resolution=resolution*100000,crs=CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
      }
    }
  if(any(class(input)=='data.frame') | any(class(input)=='SpatialPointsDataFrame')){

    if(gfun=='richness'|is.null(gfun)){

      spdf_raster=rasterize(spdf,r, fun=function(x, ...) {length(unique(na.omit(x)))})

      input=spdf_raster$species
    }

    if (gfun=='density'){

    spdf_raster=rasterize(spdf,r, fun='count')

    input=spdf_raster$ID}


  if (gfun=='sum'){
    spdf_raster=rasterize(spdf,r, fun='sum')
    input=spdf_raster$species
  }
}

  suppressWarnings({
    ###################################
    if (is.null(map)){


      #worldMap <- ne_countries(scale = 10, type = "countries", returnclass = 'sf')
      flpth <- system.file('wmap_hr.RDS', package = 'ggg')
      load(flpth)
      #flpth <- system.file('biomes_lr.RDS', package = 'ggg')
      #load(flpth)
      #worldMap=shape_sf_lr

      print('map not selected = downloading and using one that suits the input. - medium resolution')


      if(crs=='cea'){

        map = st_transform(worldMap, '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
        map<- st_crop(map,extent(input)*zoom_out)
        if(!is.null(size)){
          size=size*1000000
        }
        if (!is.null(y_sf)){
         y_sf=y_sf*1000000}

        if (!is.null(x_sf)){
        x_sf=x_sf*100000}



      }
      else{
        map <- worldMap %>% st_crop(extent(input)*zoom_out)
      }
      }

    if (is.null(breaks)){

      breaks=break_maker(input,exclude_0 = exclude_0, round_to = round_to, break_n =  break_n)
      
    }


    if (img == 'mammal'){
      img <- system.file('img_mammal.RDS', package = 'ggg')
      load(img)
      #img <- image_data("ebd8b68c-b9db-4466-b0ae-1edebd18153c", size = "512")[[1]]
    }

    if(is.null(size)){
      temp=as.list(st_bbox(map))
      size=(temp$xmax-temp$xmin)/3
    }

    if (!is.null(img) && img == 'frog'){
      img <- image_data("43497e8a-45e7-4fa2-a8a0-ffadac8401fc", size = "512")[[1]]}

    if (!is.null(img) && img == 'bird'){
      img <- image_data("0fc00dbf-322f-48b5-8b41-459bde803693", size = "512")[[1]]}

    if (!is.null(img) && img == 'lizard'){
      img <- image_data("f2c8db98-c34c-4140-a868-029bf4b557b1", size = "512")[[1]]}

    if (is.null(y_sf)){
      temp=as.list(st_bbox(map))
      y_sf=temp$ymin+((temp$ymax-temp$ymin)*0.15)}


    if (is.null(x_sf)){
      temp=as.list(st_bbox(map))
      x_sf=temp$xmin+((temp$xmax-temp$xmin)*0.15) }



    if (is.null(filling)){

      temp_pallette <- colorRampPalette(c(low,high))
      filling=temp_pallette(break_n)}

    if (filling %in% names(palette)){

      tempc=which(names(palette)==filling)

      temp_p=unlist(palette[tempc])
      names(temp_p)=NULL
      color_n= round(seq(1,length(temp_p),len=break_n))

      paltemp=vector("list", length(color_n))
      for (j in seq_along(color_n)){

        col_right=color_n[j]
        col_subset=temp_p[col_right]

        paltemp[[j]]=col_subset
      }
      filling=unlist(paltemp)

    names(filling)=NULL
    }

if(is.null(invert_palette)) {
  filling=filling
}else{
  filling=rev(filling)
}


    success <- FALSE
    while (!success) {

      if(is.null(scal_cont)) {
        ###################################
        xt=mask(input,map)
        r_points = rasterToPoints(xt)
        r_df = data.frame(r_points)

        #r_df$cuts=cut(na.omit(r_df[,3]),breaks= breaks,include.lowest = TRUE)
        r_df$cuts=cut(r_df[,3],breaks=breaks,include.lowest = TRUE)

        temp=as.list(st_bbox(map))


        if (is.null(label)){
          label=c(levels(r_df$cuts))

        }

        p=ggplot(data=r_df) +
          geom_sf(data=map, col=map_col,bg=map_bg,size= width)+
          geom_tile(aes(x=x,y=y,fill=cuts))+
          geom_sf(data=map, col=map_col,bg=NA,size= width)+
          scale_fill_manual(legend_title,values = filling,na.value=na,limits=c(levels(r_df$cuts)),labels=label)+
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.background = element_rect(fill =panel_background))+
          scale_x_continuous(breaks= breaks_x)+
          scale_y_continuous(breaks= breaks_y)+
          add_phylopic(img, alpha = alpha, x = x_sf, y = y_sf, ysize = size,
                       color = icon_color)+
          ggtitle(map_title)+
          theme(plot.title = element_text(hjust = 0.5))

       #test=as.data.frame(na.omit(input@data@values))
        #test$cuts=cut(na.omit(input@data@values),breaks=breaks,include.lowest = TRUE)

        df=r_df %>% group_by(cuts) %>% summarise(freq = n())

        h=ggplot(df, aes(x = cuts, y = freq, fill =cuts)) + ## Note the new aes fill here
          geom_bar(stat = "identity")+
          scale_fill_manual(legend_title,values = filling,na.value=na,limits=c(levels(r_df$cuts)),labels=label)+
          theme(axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),axis.title.y = element_blank())

        if(!is.null(add_values_hist)){
         h= h+geom_text(aes(label=freq), vjust=0,size=2)+theme(axis.text.y = element_blank())}

if(!is.null(rem_hist_leg)){
  h=h+theme(legend.position = "none")
}
        if(!is.null(rem_map_leg)){
          p=p+theme(legend.position = "none")
        }


      } else{

        xt=mask(input,map)
        r_points = rasterToPoints(xt)
        r_df = data.frame(r_points)

        temp=as.list(st_bbox(map))


        p=ggplot(data=r_df) +
          geom_sf(data=map, col=map_col,bg=map_bg, size=width)+
          geom_tile(aes(x=x,y=y,fill=r_df[,3]))+
          geom_sf(data=map, col=map_col,bg=NA, size=width)+
          scale_fill_gradient(legend_title,low=low, high=high,na.value=na,limits=c(min(na.omit(r_df[,3])),max(na.omit(r_df[,3]))))+
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.background = element_rect(fill =panel_background))+
          scale_x_continuous(breaks= breaks_x)+
          scale_y_continuous(breaks= breaks_y)+
          add_phylopic(img, alpha = alpha, x = x_sf, y = y_sf, ysize = size, color = icon_color)+
          ggtitle(map_title)+
          theme(plot.title = element_text(hjust = 0.5))

       f=hist(xt, breaks=break_n,plot=FALSE)
        dat <- data.frame(counts= f$counts,breaks = f$mids)
        h=ggplot(dat, aes(x = breaks, y = counts, fill = breaks)) +
          geom_bar(stat = "identity")+
          scale_fill_gradient(legend_title,low=low, high=high,na.value=na)+
          theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())

        if(!is.null(add_values_hist)){
          h= h+geom_text(aes(label=counts), vjust=0,size=2)+theme(axis.text.y = element_blank())}
        if(!is.null(rem_hist_leg)){
          h=h+theme(legend.position = "none")
        }
        if(!is.null(rem_map_leg)){
          p=p+theme(legend.position = "none")
        }

      }#scale_cont else

      if(is.null(silent)){
        tryCatch({
        print(p)
        }, error = function(e){
          return(e)
        })
        if (!grepl(pattern = 'polygon edge not found', x = p)) {
          success <- TRUE
        } else {
          cat('Hmmm... looks like the computation failed. Let\'s try again\n')
        }
          }


        if((return=='map')){
          return(p)
        }

        if(return=='hist'){
          return(h)
        }

        if(return=='raster'){
          return(r)
        }

        if(return=='all'){
          p_h_r=list(p,h,xt)
          return(p_h_r)
        }




    }
  })
}



