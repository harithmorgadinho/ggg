wege_final=function(x){

  # 1 - status and WEGE assignment ----
  ge_assignments <- c(DD = 0, UA = 0, LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EW = 5, EX = 6)

  wege_func <- function(we, ge) {
    log((1+we) * 2^ge)
  }

  # 2 - calc nsp per site ----
  # unique site ids
  site_ids <- paste0(x$longitude, ':', x$latitude)
  x$site_id <- site_ids
  spp <- unique(x$species)
  # vector by species
  nsites_per_sp <- vector(length = length(spp))
  # calculate the unique number of species per site
  names(nsites_per_sp) <- spp
  for (sp in spp) {
    sites <- x[which(x$species == sp), 'site_id']
    nsites_per_sp[[sp]] <- length(unique(sites))
  }

  # 3 - calculate WE and WEGE per site ----
  # convert df_all from by species to by site
  unique_site_ids <- unique(site_ids)
  res <- data.frame(id = NA, we = NA, wege = NA, nsp = NA)
  for (site_id in unique_site_ids) {
    part <- x[which(site_ids == site_id), ]
    part <- part[!duplicated(part$species), ]
    wes <- 1/nsites_per_sp[part$species]
    # calls the ge_assignments determined in 1.
    ges <- ge_assignments[part$status]
    # calls the wege_func determined in 1.
    weges <- wege_func(we = wes, ge = ges)
    res <- rbind(res, data.frame(id = site_id, we = sum(wes),
                                 wege = sum(weges), nsp = length(spp)))
  }
  res <- res[-1, ]

  # for the formula:
  # min(res$wege)

  # 4 - add long and lat ----
  lonlat <- strsplit(x = res$id, split = ':')
  res$longitude <- sapply(X = lonlat, FUN = function(x) {
    as.numeric(x[[1]])
  })
  res$latitude <- sapply(X = lonlat, FUN = function(x) {
    as.numeric(x[[2]])
  })


  #library(ggplot2)
  print(ggplot(data = res, aes(x = longitude, y = latitude, colour = log(wege))) + geom_point() + scale_color_gradient2())

  coord = res[,5:6]
  spdf <- SpatialPointsDataFrame(coord, res,proj4string =
                                   CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  return(rasterize(spdf,r ,field=spdf$wege))
}


