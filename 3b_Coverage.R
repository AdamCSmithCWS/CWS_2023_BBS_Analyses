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

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

db <- load_map("latlong") %>%
  rename(grid_cell_name = strata_name,
         area_km2 = area_sq_km)

qual_ebird <- ebirdst_runs


# load maps of regions ----------------------------------------------------


strata <- load_map("bbs_usgs")
prov_state <- load_map("prov_state")
bcrs <- load_map("bcr")
countries <- rnaturalearth::ne_countries(continent = "North America") %>%
  filter(sovereignt %in% c("Canada","United States of America"))

for(i in 1:nrow(sp_list$english)){

  sp_sel <- unname(unlist(sp_list[i,"english"]))

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
                         sp_sel1),
                   str_extract(string = sp_sel1,pattern = ".*(?= \\()"),
                   sp_sel1)


  sp_ebird <- ebirdst::get_species(sp_sel1)
  if(is.na(sp_ebird)){
    sp_list[i,"eBird_range_data"] <- "Not a species"

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
    sp_list[i,"eBird_range_data"] <- "failed"

    next}

  aou <- as.integer(sp_list[i,"aou"])

    #   print(paste(sp,aou))
    # }
    # }
    # identifying first years for selected species ----------------------------
    fy <- NULL
    if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
      fy <- 1978 #5 years after the split
    }
    if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
      fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
    }
    if(aou == 6121){ # CAve Swallow
      fy = 1985
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

country_coverage <- regional_summary(sp_coverage,
                                     regions = countries,
                                     region_name = "sovereignt")


}



