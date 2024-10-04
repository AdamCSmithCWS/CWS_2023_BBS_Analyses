
## compile trend and index files for
### 1 - website
YYYY <- 2022

webmaps <- TRUE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)

source("functions/mapping.R")
source("functions/loess_func.R")
# custom functions to calculate reliability categories and determine website inclusion


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

trends <- readRDS(paste0("Website/All_BBS_Trends_",YYYY,".rds"))
indices <- readRDS(paste0("Website/All_BBS_Full_Indices_",YYYY,".rds"))
indices_smooth <- readRDS(paste0("Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))


# Website trends ----------------------------------------------------------


web <- trends %>%
  filter(for_web == TRUE,
         region != "continent",
         region_type != "bcr",
         trend_time != "Three-generation") %>%
  mutate(prob_decrease_0_25_percent = prob_decrease_0_percent-prob_decrease_25_percent,
         prob_decrease_25_50_percent = prob_decrease_0_percent - (prob_decrease_0_25_percent + prob_decrease_50_percent),
         prob_increase_0_33_percent = prob_increase_0_percent-prob_increase_33_percent,
         prob_increase_33_100_percent = prob_increase_0_percent - (prob_increase_0_33_percent + prob_increase_100_percent),
         mapfile = paste(bbs_num,region,trend_time,"map.png",sep = "_"),
         strata_included = paste(strata_included,strata_excluded,sep = " ; "),
         strata_excluded = "")

web_species <- read.csv("data/BBS_AvianCore.csv")

names_match <- web %>%
  select(species,espece,bbs_num) %>%
  distinct()

miss_bbs_num <- names_match %>%
  select(bbs_num,species) %>%
  left_join(.,
            web_species,
            by = c("bbs_num" = "bbsNumber"),
            multiple = "all") %>%
  arrange(bbs_num) %>%
  filter(is.na(commonNameE))

if(nrow(miss_bbs_num) > 0){
  warning("At least one bbs number is missing from Avian Core")

  print(paste("Avian core is missing",
              paste(miss_bbs_num$bbs_num,
                    collapse = ", ")))
  web <- web %>%
    filter(bbs_num %in% web_species$bbsNumber)
}


miss_english_names <- names_match %>%
  select(bbs_num,species,espece) %>%
  left_join(.,
            web_species,
            by = c("species" = "commonNameE"),
            multiple = "all") %>%
  arrange(bbs_num) %>%
  filter(is.na(bbs_num))


if(nrow(miss_english_names) > 0){
  warning("At least one species name is missing from Avian Core")
  web <- web %>%
    filter(species %in% web_species$commonNameE)

}

# generate maps for CWS website -------------------------------------------

if(webmaps){


#loading base maps for the website plots
canmap <- bbsBayes2::load_map("bbs_cws") %>%
  filter(country == "Canada")
basemap <- bbsBayes2::load_map("bbs_usgs") %>%
  filter(country == "Canada")


## looping through species to create the maps in folder webmaps
sp_loop <- unique(web$bbs_num)
for(sp in sp_loop){
  dft <- web %>%
    filter(bbs_num == sp,
           trend_time == "Long-term")


  generate_web_maps(dft,
                    canmap = canmap,
                    basemap = basemap)
  dft <- web %>%
    filter(bbs_num == sp,
           trend_time == "Short-term")
  generate_web_maps(dft,
                    canmap = canmap,
                    basemap = basemap)

  print(round(which(sp_loop == sp)/length(sp_loop),2))
}
## maps have Canadian regions only and show the regions included in each trend

map_dup_test <- any(duplicated(web$mapfile))

test_map <- any(!file.exists(paste0("website/webmaps/",web$mapfile)))

if(test_map){
  map_test <- paste0("website/webmaps/",web$mapfile)

  w_miss <- map_test[which(!file.exists(map_test))]

  stop("At least one map is missing")

}

maps <- list.files("website/webmaps/",
                   pattern = ".png")
test_map_extra <- any((web$mapfile %in% maps) == FALSE)

if(test_map_extra){
  warning("There are extra maps in the webmaps folder")
}

}#end webmaps

clout = c("bbs_num",
          "species",
          "espece",
          "region",
          "trend_time",
          "start_year",
          "end_year",
          "trend",
          "trend_q_0.025",
          "trend_q_0.975",
          "reliability",
          "width_of_95_percent_credible_interval",
          "reliab.cov",
          "backcast_flag",
          "prob_decrease_0_percent",
          "prob_increase_0_percent",
          "prob_decrease_50_percent",
          "prob_decrease_25_50_percent",
          "prob_decrease_0_25_percent",
          "prob_increase_0_33_percent",
          "prob_increase_33_100_percent",
          "prob_increase_100_percent",
          "percent_change",
          "percent_change_q_0.025",
          "percent_change_q_0.975",
          "n_routes",
          "strata_included",
          "strata_excluded",
          "mapfile")

clnms = c("sp","species","espece","geo.area","trendtype",
          "startyear","endyear","trend",
          "llimit","ulimit","reliab.over",
          "reliab.prec","reliab.cov","reliab.pool",
          "p.decrease","p.increase","p.d50","pd50.25",
          "pd25.0","pi0.33","pi33.100","pi100",
          "percent.change","percent.change.llimit",
          "percent.change.ulimit","nroutesduringtrend",
          "strata.inc","st.excl.long","mapfile")

if(any(!clout %in% names(web))){
  print(clout[which(!clout %in% names(web))])
}

web = web[,clout]
names(web) = clnms


readr::write_excel_csv(web, paste0("website/",YYYY," BBS trends for website.csv"))











# Indices for website -----------------------------------------------------



webi_short <- indices %>%
  filter(for_web == TRUE,
         region != "continent",
         region_type != "bcr",
         year > (YYYY-11),
         trend_time == "Short-term")

webi <- indices %>%
  filter(for_web == TRUE,
         region != "continent",
         region_type != "bcr",
         year > 1969,
         trend_time == "Long-term") %>%
  bind_rows(.,webi_short)

webi <- webi %>%
  filter(bbs_num %in% web_species$bbsNumber)

clouti =  c("bbs_num",
            "species",
            "espece",
            "region",
            "trend_time",
            "year",
            "index",
            "index_q_0.05",
            "index_q_0.95")
clnmsi = c("sp","species","espece","geo.area","trendtype",
           "year","an.index",
           "llimit","ulimit")


if(any(!clouti %in% names(webi))){
  print(clouti[which(!clouti %in% names(webi))])
}


webi = webi[,clouti]
names(webi) <- clnmsi



tshort = web %>%
  filter(trendtype == "Short-term")
tlong= web %>%
  filter(trendtype == "Long-term")
index_short <- webi %>%
  filter(year == YYYY-10,
         trendtype == "Short-term")
index_long <- webi %>%
  filter(year == YYYY-10,
         trendtype == "Long-term")

if(nrow(index_long) != nrow(tlong) |
   nrow(index_short) != nrow(tshort)){
warning("The number of indices and trends don't match \n explore index_trend_test")

index_trend_test <- webi %>%
  filter(year == YYYY-10) %>%
  select(species,trendtype,geo.area,an.index) %>%
  full_join(.,web,
            by = c("species","trendtype","geo.area"))
}

readr::write_excel_csv(webi, paste0("website/",YYYY," BBS indices for website.csv"))












