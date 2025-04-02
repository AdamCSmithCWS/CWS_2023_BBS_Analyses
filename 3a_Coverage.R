## calculating BBS coverage by degree block
## uses eBirdst seasonal range maps and BOTW range maps (if eBird not available)
## plus the raw data file used in the model fitting
## to calculate the proportion of a species' range within Canada and the USA
## that falls within 1x1 degree grid cells that contribute data to the species
## trend model and for a given trend period (separate coverage estimates for
## long-, short-term, and three-generation trends)
##
##
## Also, loads the generation times for each species from the R package naturecounts
## for use in defining the time-period for the species-specific three-generation
## trends


# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# remotes::install_github("AdamCSmithCWS/SurveyCoverage@dev")


library(SurveyCoverage)
library(bbsBayes2)
library(tidyverse)
library(ebirdst)
library(sf)
#ebirdst::set_ebirdst_access_key("t9el4omae1c3",overwrite = TRUE)

external_dir <- "F:/CWS_2023_BBS_Analyses"

db <- load_map("latlong") %>%
  rename(grid_cell_name = strata_name,
         area_km2 = area_sq_km)

qual_ebird <- ebirdst_runs

ly <- max(bbsBayes2::load_bbs_data()$route$year)



# load maps of regions ----------------------------------------------------

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

stratum <- load_map("bbs_cws")

prov_state <- load_map("bbs_cws") %>%
  group_by(prov_state) %>%
  summarise() %>%
  mutate(strata_name = prov_state)

bcr <- load_map("bcr")

bcr_by_country <- load_map("bbs_cws") %>%
  group_by(bcr_by_country) %>%
  summarise() %>%
  rename(strata_name = bcr_by_country)

country <- rnaturalearth::ne_countries(continent = "North America") %>%
  filter(admin %in% c("Canada","United States of America")) %>%
  sf::st_transform(crs = sf::st_crs(stratum)) %>%
  rename(strata_name = admin) %>%
  select(strata_name)

continent <- country %>%
  summarise() %>%
  mutate(strata_name = "continent")

regs_maps <- list(continent = continent,
                  country = country,
                  prov_state = prov_state,
                  bcr = bcr,
                  stratum = stratum,
                  bcr_by_country = bcr_by_country)


# Load three generation times from naturecounts ---------------------------

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

# Three generation times --------------------------------------------------
sp_codes <- naturecounts::meta_species_codes() %>%
  filter(authority == "BBS2") %>%
  mutate(aou = as.integer(species_code),
         naturecounts_species_id = species_id) %>%
  select(aou,naturecounts_species_id)

sp_list <- sp_list %>%
  left_join(sp_codes, by = "aou")

sp_id <- naturecounts::meta_species_taxonomy() %>%
  select(english_name,sort_order,scientific_name,species_id) %>%
  rename_with(~paste0("naturecounts_",.x))

sp_list <- sp_list %>%
  left_join(sp_id, by = c("naturecounts_species_id"))

redo_generations <- FALSE
re_naturecounts <- FALSE
if(re_naturecounts){
gen_years_all <- naturecounts::nc_query_table(table = "SpeciesLifeHistory") %>%
  filter(subcategDescr == "Average generation length (years)")
saveRDS(gen_years_all,paste("data/all_naturecounts_generation_data.rds"))
}else{
  gen_years_all <- readRDS(paste("data/all_naturecounts_generation_data.rds"))
}

if(redo_generations){
if(re_naturecounts){

  gen_years <- naturecounts::nc_query_table(table = "SpeciesLifeHistory") %>%
    filter(subcategDescr == "Average generation length (years)",
           (speciesID == subSpeciesID)) %>%
    select(speciesID,value) %>%
    rename(GenLength = value) %>%
    mutate(GenLength = as.double(GenLength)) %>%
    distinct()
  gen_yearsalt <- naturecounts::nc_query_table(table = "SpeciesLifeHistory") %>%
    filter(subcategDescr == "Average generation length (years)",
           (is.na(subSpeciesID))) %>%
    select(speciesID,value) %>%
    rename(GenLength = value) %>%
    distinct() %>%
    group_by(speciesID) %>%
    summarise(GenLength = max(GenLength)) %>%
    mutate(GenLength = as.double(GenLength)) %>%
    ungroup()

  gen_years <- bind_rows(gen_years,gen_yearsalt) %>%
    distinct()


  saveRDS(gen_years,"data/naturecounts_generation_times.rds")

}else{
  gen_years <- readRDS("data/naturecounts_generation_times.rds")
}

sp_list_gen <- sp_list %>%
  left_join(gen_years, by = c("naturecounts_species_id" = "speciesID"))

if(any(is.na(sp_list_gen$GenLength))){
wch_miss <- which(is.na(sp_list_gen$GenLength))

for(j in wch_miss){
  spj <- sp_list_gen$naturecounts_species_id[j]
  gensj <- gen_years_all %>%
    filter(speciesID == spj) %>%
    #select(value) %>%
    distinct() %>%
    mutate( value = as.numeric(value))
 gtmp <- max(gensj$value,na.rm = TRUE) #select the max generation time included for a given speciesID value
 if(!is.finite(gtmp)){
   #if no generationtime included for taxonomic lumps and splits
   # use the mean generation time for the genus
genus <- str_extract(sp_list_gen$naturecounts_scientific_name[j],
                     pattern = "[[:alpha:]]+(?= )")
genus_codes <- naturecounts::search_species(genus)
gensj <- gen_years_all %>%
  filter(speciesID %in% genus_codes$species_id) %>%
  #select(value) %>%
  distinct() %>%
  mutate( value = as.numeric(value))
gtmp <- mean(gensj$value,na.rm = TRUE)
 }
 sp_list_gen[j,"GenLength"] <- gtmp

}

}
saveRDS(sp_list_gen,"sp_list_w_generations.rds")

}

# Coverage loops ----------------------------------------------------------
library(foreach)
library(doParallel)

sp_list_gen <- readRDS("sp_list_w_generations.rds")

# sp_rerun <- c("Northern Shrike","Willow Ptarmigan", "Herring Gull",
#               "Common Loon",
#               "American Pipit",
#               "Redpoll (Common/Hoary)")
# sp_list_gen <- sp_list_gen %>%
#   filter(english %in% sp_rerun)


# load BOTW range maps ----------------------------------------------------
re_do_botw <- FALSE

if(re_do_botw){
botw_all <- readRDS("data/BOTW_valid.rds")

continent_alt <- continent %>%
  sf::st_transform(crs = sf::st_crs(botw_all))

botw_sp_sel <- botw_all %>%
  filter(sci_name %in% sp_list_gen$naturecounts_scientific_name)


botw_seas <- botw_sp_sel %>%
  filter(seasonal %in% c(1,2),
         presence %in% c(1:3))
# %>%
  #sf::st_make_valid()

saveRDS(botw_seas,"data/botw_seas.rds")

}else{
botw_seas <- readRDS("data/botw_seas.rds")
}




n_cores = 15
#n_cores <- floor(parallel::detectCores()/4)-1

cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)

re_run <- TRUE

test <- foreach(i = rev(1:nrow(sp_list_gen)),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "ebirdst",
                              "SurveyCoverage",
                              "sf"),
                .errorhandling = "pass") %dopar%
  {

#for(i in rev(1:nrow(sp_list_gen))){

  sp_sel <- unname(unlist(sp_list_gen[i,"english"]))
  aou <- as.integer(sp_list_gen[i,"aou"])

  if(all(file.exists(paste0(external_dir,"/coverage/coverage_maps_",c("Long-term","Short-term","Three-generation"),"_",aou,".rds"))) & !re_run){
    #sp_list_gen[i,"eBird_range_data"] <- "Used"
    #next
    }

  # filter(n_routes_w_obs > 19,
  #                 !is.na(english),
  #                 !grepl(pattern = "^unid.",english),
  #                 !grepl(pattern = "^\\(",english),
  #                 english != c("Western Grebe (Clark's/Western)")) %>%
  #          mutate(english = ifelse(grepl(pattern = " \\(",
  #                                        english),
  #                                  str_extract(string = english,pattern = ".*(?= \\()"),
  #                                  english))

  if(sp_sel == "Western Grebe (Clark's/Western)"){next} # avoiding confusion with true Western Grebe

print(sp_sel)

  sp_sel1 <- ifelse(grepl(pattern = " \\(",
                         sp_sel),
                   str_extract(string = sp_sel,pattern = ".*(?= \\()"),
                   sp_sel)


  sp_ebird <- ebirdst::get_species(sp_sel1)

  if(sp_sel == "Cattle Egret"){
    sp_sel1 <- "Western/Eastern Cattle Egret"


    sp_ebird <- ebirdst::get_species(sp_sel1)

  }

  if(is.na(sp_ebird) & !grepl(pattern = "*\\(",
                             sp_sel1)){
sp_sel1 <- unname(unlist(sp_list_gen[i,"naturecounts_english_name"]))
sp_ebird <- ebirdst::get_species(sp_sel1)

  }

  if(is.na(sp_ebird) & !grepl(pattern = "*\\(",
                              sp_sel1)){
    sp_sel1 <- unname(unlist(sp_list_gen[i,"naturecounts_scientific_name"]))
    sp_ebird <- ebirdst::get_species(sp_sel1)

  }

  # if no eBird range data, then try BOTW data
    if(is.na(sp_ebird) & !grepl(pattern = "*\\(",
                                sp_sel1)){

    sp_list_gen[i,"eBird_range_data"] <- "Not available"


# search for botw range map -----------------------------------------------

    sp_sci <- unname(unlist(sp_list_gen[i,"naturecounts_scientific_name"]))
    range_map_botw <- botw_seas %>%
      filter(sci_name == sp_sci) %>%
      summarise()

    if(st_is_empty(range_map_botw)){
      sp_list_gen[i,"botw_range_data"] <- "Not available"
      next
    }
    range_info <- try(grid_range(sp_sel1,
                                 coverage_grid_custom = db,
                                 range_map = range_map_botw),silent = TRUE)

    sp_list_gen[i,"botw_range_data"] <- "Used"

    range_map_botw <- NA

  }else{
  qual_sel <- qual_ebird[which(qual_ebird$species_code == sp_ebird),]
  breed_qual <- unname(unlist(qual_sel[,"breeding_quality"]))
  resident_qual <- unname(unlist(qual_sel[,"resident_quality"]))
  resident <- unname(unlist(qual_sel[,"is_resident"]))

  if(length(resident) == 0){
    resident <- FALSE

  }


  if(resident){
    season = "resident"
  }else{
    season = "breeding"
  }


  range_info <- try(grid_range(sp_sel1,
                               coverage_grid_custom = db,
                               seasonal_range = season),silent = TRUE)

  }
# if no successful grid_range with eBird data, then try BOTW data
  if(class(range_info) == "try-error" ){
    #sp_list_gen[i,"eBird_range_data"] <- "failed"
    # search for botw range map -----------------------------------------------
if(!grepl(pattern = "*\\(",
             sp_sel1)){
    sp_sci <- unname(unlist(sp_list_gen[i,"naturecounts_scientific_name"]))
    range_map_botw <- botw_seas %>%
      filter(sci_name == sp_sci) %>%
      summarise()

    if(st_is_empty(range_map_botw)){
      sp_list_gen[i,"botw_range_data"] <- "Not available"
      next
    }
}else{
  range_map_botw <- NA
}
    range_info <- try(grid_range(sp_sel1,
                                 coverage_grid_custom = db,
                                 range_map = range_map_botw),silent = TRUE)
    sp_list_gen[i,"botw_range_data"] <- "Used"


  }

  # if still no range data skip to next species
  if(class(range_info) == "try-error"){
    sp_list_gen[i,"botw_range_data"] <- "failed"
    next}

#} # temp loop end



  strat <- "bbs_cws"
  three_g <- max(c(10,round(as.numeric(sp_list_gen[i,"GenLength"])*3)))


# coverage by trend-period ----------------------------------------------
if(!file.exists(paste0(external_dir,"/Raw_data/Raw_",aou,".rds"))){
  sp_list_gen[i,"eBird_range_data"] <- "not modeled"
  next
}
  raw <- readRDS(paste0(external_dir,"/Raw_data/Raw_",aou,".rds"))
  strat <- "bbs_cws"

  # s <- stratify(by = strat,
  #               release = 2024,
  #               species = sp_sel,
  #               quiet = TRUE) %>%
  #   prepare_data(min_max_route_years = 2,
  #                quiet = TRUE)
  saveRDS(range_info,paste0(external_dir,"/coverage/range_map_coverage_",aou,".rds"))

  if(all(file.exists(paste0(external_dir,"/coverage/coverage_maps_",c("Long-term","Short-term","Three-generation"),"_",aou,".rds")))){
    #sp_list_gen[i,"eBird_range_data"] <- "Used"
    next
  }
for(ttime in c("Long-term","Short-term","Three-generation")){

if(ttime == "Long-term"){fy <- 1966}
  if(ttime == "Short-term"){fy <- ly-10}
  if(ttime == "Three-generation"){

    fy <- ly-three_g
      }


if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
  fy <- max(c(fy,1978)) #5 years after the split
}
if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
  fy <- max(c(fy,1990))  #5 years after the split and first year EUCD observed on > 3 BBS routes
}
if(aou == 6121){ # CAve Swallow
  fy <- max(c(fy,1985))
}


survey_data <- raw %>%
  select(route,latitude,longitude,year,strata_name) %>%
  filter(year >= fy)


sp_coverage <- overlay_range_data(range = range_info,
                                  survey_sites = survey_data,
                                  sites = "route",
                                  years = "year",
                                  x_coord = "longitude",
                                  y_coord = "latitude",
                                  crs_site_coordinates = 4326,
                                  add_survey_sites_to_range = TRUE)

saveRDS(sp_coverage,paste0(external_dir,"/coverage/coverage_maps_",ttime,"_",aou,".rds"))
# cumulative_coverage_map <- basp_coverage$cumulative_coverage_map
# overall_coverage_estimate <- basp_coverage$cumulative_coverage_estimate
#
# coverage_overall <- ggplot()+
#   geom_sf(data = cumulative_coverage_map,
#           aes(fill = coverage))+
#   scale_fill_viridis_d()+
#   labs(title = paste(example_species,"proportion covered = ",round(overall_coverage_estimate$coverage_proportion,2)))
#
# print(coverage_overall)

ann_coverage <- NULL
cumulative_coverage <- NULL

for(reg in regs_to_estimate){

  mp_tmp <- regs_maps[[reg]]
tmp_coverage <- regional_summary(sp_coverage,
                                   regions = mp_tmp,
                                   region_name = "strata_name")

 ann_tmp <- tmp_coverage$regional_annual_coverage_estimate %>%
  filter(coverage) %>%
  mutate(region_type = reg,
         species = sp_sel,
         aou = aou)

 cumulative_tmp <- tmp_coverage$regional_cumulative_coverage_estimate %>%
   filter(coverage) %>%
   mutate(region_type = reg,
          species = sp_sel,
          aou = aou)

 ann_coverage <- bind_rows(ann_coverage,ann_tmp)
 cumulative_coverage <- bind_rows(cumulative_coverage,cumulative_tmp)

} #end of regions loop

# cover_save <- list(annual_coverage = ann_coverage,
#                    cumulative_coverage = cumulative_coverage)
saveRDS(cumulative_coverage,paste0(external_dir,"/coverage/coverage_",ttime,"_",aou,".rds"))
#


} # end of ttime loop

} #end of species loop

#     sp_list_gen <- sp_list_gen %>%
#       mutate(eBird_range_data = ifelse(is.na(botw_range_data),"used",eBird_range_data))
# write_csv(sp_list_gen,"coverage/coverage_summary.csv")

parallel::stopCluster(cluster)




# exporting the coverage maps to pdf --------------------------------------






library(SurveyCoverage)
library(bbsBayes2)
library(tidyverse)
library(ebirdst)
library(sf)
#ebirdst::set_ebirdst_access_key("t9el4omae1c3",overwrite = TRUE)
sp_list_gen <- readRDS("sp_list_w_generations.rds")

external_dir <- "F:/CWS_2023_BBS_Analyses"

db <- load_map("latlong") %>%
  rename(grid_cell_name = strata_name,
         area_km2 = area_sq_km)

qual_ebird <- ebirdst_runs

ly <- max(bbsBayes2::load_bbs_data()$route$year)

strat <- "bbs_cws"

base_map <- load_map(strat)

for(i in rev(1:nrow(sp_list_gen))){

  sp_sel <- unname(unlist(sp_list_gen[i,"english"]))
  aou <- as.integer(sp_list_gen[i,"aou"])
  esp <- as.character(sp_list_gen[i,"french"])
  species_f_bil <- gsub(paste(esp,sp_sel),pattern = "[[:space:]]|[[:punct:]]",
                        replacement = "_")




three_g <- max(c(10,round(as.numeric(sp_list_gen[i,"GenLength"])*3)))

if(!any(file.exists(paste0(external_dir,"/coverage/coverage_maps_",c("Long-term","Short-term","Three-generation"),"_",aou,".rds")))){
  next
}
range_maps <- readRDS(paste0(external_dir,"/coverage/range_map_coverage_",aou,".rds"))
range_map <- range_maps$range_map
# coverage by trend-period ----------------------------------------------
if(!file.exists(paste0(external_dir,"/Raw_data/Raw_",aou,".rds"))){
  next
}
raw <- readRDS(paste0(external_dir,"/Raw_data/Raw_",aou,".rds"))


pdf(paste0("coverage_maps/coverage_maps_",species_f_bil,".pdf"))

for(ttime in c("Long-term","Short-term","Three-generation")){

  if(ttime == "Long-term"){fy <- 1966}
  if(ttime == "Short-term"){fy <- ly-10}
  if(ttime == "Three-generation"){

    fy <- ly-three_g
  }

  if(!file.exists(paste0(external_dir,"/coverage/coverage_maps_",ttime,"_",aou,".rds"))){
    next
  }
    sp_coverage <- readRDS(paste0(external_dir,"/coverage/coverage_maps_",ttime,"_",aou,".rds"))

  if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
    fy <- max(c(fy,1978)) #5 years after the split
  }
  if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
    fy <- max(c(fy,1990))  #5 years after the split and first year EUCD observed on > 3 BBS routes
  }
  if(aou == 6121){ # CAve Swallow
    fy <- max(c(fy,1985))
  }

survey_data <- raw %>%
  select(route,latitude,longitude,year,strata_name) %>%
  filter(year >= fy) %>%
  group_by(route,strata_name,latitude,longitude) %>%
  summarise(n_years = n(),
            .groups = "drop") %>%
  sf::st_as_sf(coords = c("longitude","latitude"))
survey_data <- st_set_crs(survey_data, 4326)


cumulative_coverage_map <- sp_coverage$cumulative_coverage_map
overall_coverage_estimate <- sp_coverage$cumulative_coverage_estimate

survey_data <- sf::st_transform(survey_data,crs = st_crs(cumulative_coverage_map))

bb <- sf::st_bbox(cumulative_coverage_map)

coverage_overall <- ggplot()+
  geom_sf(data = cumulative_coverage_map,
          aes(fill = coverage))+
  geom_sf(data = survey_data,aes(colour = n_years), inherit.aes = FALSE,
          size = 0.5)+
  geom_sf(data = range_map, fill = NA, colour = "darkorange")+
  geom_sf(data = base_map, fill = NA, colour = grey(0.5))+
  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")])+
  scale_fill_viridis_d(begin = 0.5, direction = -1)+
  scale_colour_viridis_c(option = "F", direction = -1, name = "Number of years \n with BBS counts")+
  theme_bw()+
  labs(subtitle = paste(sp_sel,ttime,"\n coverage since ",fy," = ",round(overall_coverage_estimate$coverage_proportion,2)*100,"% of the species' range"),
       caption = "1-degree latitude by longitude grid-cells considered covered if they include
       BBS observations from routes within that grid cell
       and years included in the trend period.
       The orange polygon outlines the species' breeding-season range based on eBird,
       or Birds of the World range maps.
       The points indicate start locations for included routes and their colour reflects
       the number of annual surveys.")

print(coverage_overall)

}
dev.off()

}#end species loop
