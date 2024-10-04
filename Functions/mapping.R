
generate_web_maps <- function(df,
                              canmap = canmap,
                              basemap = basemap){

  df <- as.data.frame(df)
  dfstrata = df[which(df$region_type == "stratum"),]
  for(j in 1:nrow(df)){
    if(df[j,"for_web"]){

      mapname <- (df[j,"mapfile"])

      sts = unlist(strsplit(df[j,"strata_included"],split = " ; "))

      mapo = map_f(st = dfstrata,
                   stinc = sts,
                   map = canmap,
                   basemap = basemap)

      png(filename = paste0("website/WebMaps/",mapname),
          bg = "white",width = 4*480, height = 4*320,
          res = 300)
      print(mapo)
      dev.off()



    }

  }



  return(invisible(NULL))

}

map_f <- function(st, #dataframe of trends
                  map = canmap,
                  slope = F,
                  stinc = sts,
                  basemap = basemap){


  stplot = st[which(st$region %in% c(stinc)),]

  breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
  labls = c(paste0("< ",breaks[1]),
            paste0(breaks[-c(length(breaks))],
                   ":",
                   breaks[-c(1)]),
            paste0("> ",breaks[length(breaks)]))
  labls = paste0(labls, " %")
  #labls[length(labls)+1] <- "X"
  map <- map %>%
    mutate(row_num = row_number()) %>%
    left_join(., stplot,
              by = c("strata_name" = "region")) %>%
    arrange(row_num)

  # map@data$row_num <- 1:nrow(map@data)
  # map@data <- merge(map@data, stplot, by.x = "ST_12", by.y = "Region", all.x = T,sort = F)
  # map@data <- map@data[order(map@data$row_num), ]
  if(slope){
    map <- map %>%
      mutate(Trend = cut(slope_trend, breaks = c(-Inf, breaks,Inf),
                         labels = labls,
                         ordered_result = T)) %>%
      select(Trend)
  }else{
    map <- map %>%
      mutate(Trend = cut(trend, breaks = c(-Inf, breaks,Inf),
                         labels = labls,
                         ordered_result = T)) %>%
      select(Trend)
    }

  # map@data$Trend <- cut(map@data$Trend, breaks = c(-Inf, breaks,Inf),
  #                       labels = labls,
  #                       ordered_result = T)


  map_palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                   "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  names(map_palette) <- labls

  map_plot <- ggplot()+
    geom_sf(data = map,
            aes(fill = Trend),
            linewidth = 0.6)+
    geom_sf(data = basemap,
            fill = NA,
            linewidth = 0.6)+
    theme_void(base_size=12*(1.5))+
    #theme_void()+
    scale_fill_manual(values = map_palette,
                      na.translate = F)+
    guides(fill = ggplot2::guide_legend(reverse = TRUE,
                                        title = ""))
  map_plot


  return(

map_plot

    # sp::spplot(map,
    #            col.regions = map_palette,
    #            edge.col = grey(0.5),
    #            sp.layout = list(basmap, edge.col = grey(0.5),
    #                             fill="transparent", first=FALSE),
    #            par.settings = list(axis.line = list(col = 'transparent')))
  )
}
