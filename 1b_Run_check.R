## testing bbsBayes2 parallel in HRE env

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)

#setwd("C:/github/CWS_2022_BBS_Analyses")
#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2022_BBS_Analyses")

#output_dir <- "F:/CWS_2022_BBS_Analyses/output"
output_dir <- "D:/output_BBS"
#output_dir <- "output"


  sp_list <- readRDS("species_list.rds") %>%
    filter(model == TRUE)

  sp_track <- sp_list

  complete_running_only <- FALSE # change to FALSE if assessing whether species could be run

for(i in 1:nrow(sp_list)){

  sp <- as.character(sp_list[i,"english"])
  aou <- as.integer(sp_list[i,"aou"])

  if(!file.exists(paste0(output_dir,"/fit_",aou,".rds"))){
  if(file.exists(paste0(output_dir,"/fit_",aou,"-1.csv"))){
    sp_track[i,"test"] <- paste0("Running_",as.character(Sys.info()["nodename"]))
    next
  }
    if(complete_running_only){next}
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

    s <- try(stratify(by = strat,
                  species = sp,
                  quiet = TRUE) %>%
      prepare_data(min_max_route_years = 2,
                   quiet = TRUE,
                   min_year = fy),silent = TRUE)


    if(class(s) == "try-error"){sp_track[i,"test"] <- as.character(paste(s[1]))
    next}
    ## bbsBayes2 models do not currently work unless n_strata > 1
    if(nrow(s$meta_strata) == 1){ sp_track[i,"test"] <- "One-stratum"}

    if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
      sp_track[i,"test"] <- "Sufficient data but Missing"
    }


  }else{# end of if file.exists

    sp_track[i,"test"] <- "Complete"

  }








}


  saveRDS(sp_track,paste0("sp_track",as.character(Sys.info()["nodename"]),".rds"))


  sp_complete <- sp_track

  # sp_track_alt <- readRDS("sp_trackWNCRLABN72960.rds") %>%
  #   ungroup() %>%
  #   mutate(test = ifelse(aou %in% c(5110,5600,4980,3900),
  #                        "Running_ECCClaptop",test)) %>%
  #   filter(test == "Complete" | grepl("Running",test))
  #
  # sp_complete <- sp_track %>%
  #   filter(!aou %in% sp_track_alt$aou) %>%
  #   bind_rows(.,sp_track_alt)


  sp_done <- sp_complete %>%
    filter(test == "Complete" | grepl("Running",test))

  sp_miss <- sp_complete %>% filter(grepl("Sufficient",test))

  saveRDS(sp_miss,"species_missing.rds")





  # combine and compare with last year --------------------------------------


  lastyear = read_csv("data/All_2021_BBS_trends.csv") %>%
    filter(Region == "Continental",
           Trend_Time == "Long-term")

