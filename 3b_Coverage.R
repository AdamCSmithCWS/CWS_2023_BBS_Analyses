## calculating BBS coverage by degree block

# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# remotes::install_github("AdamCSmithCWS/SurveyCoverage")


library(SurveyCoverage)
library(bbsBayes2)
library(tidyverse)
library(ebirdst)
ebirdst::set_ebirdst_access_key("t9el4omae1c3",overwrite = TRUE)

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

db <- load_map("latlong")

qual_ebird <- ebirdst_runs


for(sp_sel in sp_list$english){

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

  sp_sel <- ifelse(grepl(pattern = " \\(",
                         sp_sel),
                   str_extract(string = sp_sel,pattern = ".*(?= \\()"),
                   sp_sel)


  sp_ebird <- ebirdst::get_species(sp_sel)
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

  range_info <- try(grid_range(sp_sel,
                               coverage_grid_custom = db,
                               seasonal_range = season),silent = TRUE)


  if(class(range_info) == "try-error"){
    sp_list[i,"eBird_range_data"] <- "failed"

    next}




}



