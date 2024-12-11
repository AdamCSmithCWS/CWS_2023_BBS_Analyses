## calculating BBS coverage by degree block

# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# remotes::install_github("AdamCSmithCWS/SurveyCoverage")


library(SurveyCoverage)
library(bbsBayes2)
library(tidyverse)
library(ebirdst)
#ebirdst::set_ebirdst_access_key("t9el4omae1c3",overwrite = TRUE)


db <- load_map("latlong") %>%
  rename(grid_cell_name = strata_name,
         area_km2 = area_sq_km)

qual_ebird <- ebirdst_runs

ly <- max(bbsBayes2::load_bbs_data()$route$year)


# load maps of regions ----------------------------------------------------


strata <- load_map("bbs_usgs")
prov_state <- load_map("prov_state")
bcrs <- load_map("bcr")
countries <- rnaturalearth::ne_countries(continent = "North America") %>%
  filter(sovereignt %in% c("Canada","United States of America"))


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


re_naturecounts <- TRUE
gen_years_all <- naturecounts::nc_query_table(table = "SpeciesLifeHistory") %>%
  filter(subcategDescr == "Average generation length (years)")

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



for(i in rev(1:nrow(sp_list_gen))){

  sp_sel <- unname(unlist(sp_list_gen[i,"english"]))

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

  sp_sel1 <- ifelse(grepl(pattern = " \\(",
                         sp_sel),
                   str_extract(string = sp_sel,pattern = ".*(?= \\()"),
                   sp_sel)


  sp_ebird <- ebirdst::get_species(sp_sel1)
  if(is.na(sp_ebird)){
    sp_list_gen[i,"eBird_range_data"] <- "Not a species"

    next
  }
  qual_sel <- qual_ebird[which(qual_ebird$species_code == sp_ebird),]
  breed_qual <- unname(unlist(qual_sel[,"breeding_quality"]))
  resident_qual <- unname(unlist(qual_sel[,"resident_quality"]))
  resident <- unname(unlist(qual_sel[,"is_resident"]))

  if(resident){
    season = "resident"
  }else{
    season = "breeding"
  }

  range_info <- try(grid_range(sp_sel1,
                               coverage_grid_custom = db,
                               seasonal_range = season),silent = TRUE)


  if(class(range_info) == "try-error"){
    sp_list_gen[i,"eBird_range_data"] <- "failed"

    next}

  aou <- as.integer(sp_list_gen[i,"aou"])
  strat <- "bbs_cws"
  three_g <- max(c(10,round(sp_list_gen[i,"GenLength"]*3)))
#     #   print(paste(sp,aou))
#     # }
#     # }
#     # identifying first years for selected species ----------------------------
#     fy <- 1970
#     if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
#       fy <- 1978 #5 years after the split
#     }
#     if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
#       fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
#     }
#     if(aou == 6121){ # CAve Swallow
#       fy = 1985
#     }
#
#
#   s <- stratify(by = strat,
#                 release = 2024,
#                 species = sp_sel,
#                 quiet = TRUE) %>%
#     prepare_data(min_max_route_years = 2,
#                  quiet = TRUE,
#                  min_year = fy)
#
#   survey_data <- s$raw_data %>%
#     select(route,latitude,longitude,year)
#
#
#  sp_coverage <- overlay_range_data(range = range_info,
#                                       survey_sites = survey_data,
#                                       sites = "route",
#                                       years = "year",
#                                       x_coord = "longitude",
#                                       y_coord = "latitude",
#                                       crs_site_coordinates = 4326,
#                                       add_survey_sites_to_range = TRUE)
#
# strat_coverage <- regional_summary(sp_coverage,
#                              regions = strata,
#                              region_name = "strata_name")
#
# # country_coverage <- regional_summary(sp_coverage,
# #                                      regions = countries,
# #                                      region_name = "sovereignt")
#
# saveRDS(strat_coverage,paste0("coverage/coverage_",aou,".rds"))
#


# coverage by trend-period ----------------------------------------------


for(ttime in c("Long-term","Short-term","Three-generation")){

if(ttime == "Long-term"){fy <- 1970}
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

strat <- "bbs_cws"

s <- stratify(by = strat,
              release = 2024,
              species = sp_sel,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE,
               min_year = fy)

survey_data <- s$raw_data %>%
  select(route,latitude,longitude,year)


sp_coverage <- overlay_range_data(range = range_info,
                                  survey_sites = survey_data,
                                  sites = "route",
                                  years = "year",
                                  x_coord = "longitude",
                                  y_coord = "latitude",
                                  crs_site_coordinates = 4326,
                                  add_survey_sites_to_range = TRUE)

strat_coverage <- regional_summary(sp_coverage,
                                   regions = strata,
                                   region_name = "strata_name")

# country_coverage <- regional_summary(sp_coverage,
#                                      regions = countries,
#                                      region_name = "sovereignt")

saveRDS(strat_coverage,paste0("coverage/coverage_",ttime,"_",aou,".rds"))


}



