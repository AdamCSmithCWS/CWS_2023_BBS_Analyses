## generate indices for BBS results
##
## Uses the fitted model objects (output of bbsBayes2::run_model()) to estimate
## the full suite of annual indices of abundance (both "full" indices that include
## the annual fluctuations and "smooth" indices that are just the GAM-smooth from the
## GAMYE model).
## Estimates the indices for all regions and sub-regions
## saves indices as .rds files for use in following scripts
##
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2023_BBS_Analyses")
#setwd("C:/GitHub/CWS_2023_BBS_Analyses")

# set output_dir to the directory where the saved modeling output rds files are stored
output_dir <- "F:/CWS_2023_BBS_Analyses/output"
external_dir <- "F:/CWS_2023_BBS_Analyses"

# output_dir <- "output"
# output_dir <- "F:/CWS_2023_BBS_Analyses/output"

n_cores = 6 # if desired, can be run in parallel across many species

re_run <- TRUE # if TRUE will recalculate and overwrite previous saved indices for each species
# if FALSE, will skip species with saved indices files

sp_list <- readRDS("sp_list_w_generations.rds") %>%
  filter(model == TRUE)




# final list of species where model did not converge (Eastern Screech Owl)
 sp_drop <- readRDS("species_rerun_converge_fail_2024-12-11.rds")
# # list of species for which the gam model was run because gamye would not converge
# # includes American Goshawk, Eastern Screech-Owl, and Sharp-shinned Hawk
 sp_gam <- readRDS("species_rerun_converge_fail_2024-12-04.rds")
# sp_rerun <- readRDS("species_rerun_converge_fail_2024-11-13.rds")
 sp_rerun <- c("Long-tailed Duck","Northern Shrike","Willow Ptarmigan", "Herring Gull",
                "Common Loon",
                "American Pipit",
                "Redpoll (Common/Hoary)")
 sp_list <- sp_list %>%
  filter(english %in% sp_rerun)

regs_to_estimate <- c("continent","country","prov_state","bcr","bcr_by_country","stratum")



# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)

order_random <- sample(1:nrow(sp_list),nrow(sp_list))

test <- foreach(i = order_random,
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr"),
                .errorhandling = "pass") %dopar%
  {

 #for(i in rev(1:nrow(sp_list))){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])


    if(sp %in% sp_drop){next}


    if(file.exists(paste0(output_dir,"/fit_",aou,".rds")) &
       (!file.exists(paste0("Indices/Inds_",aou,".rds")) | re_run)){

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


      fit <- readRDS(paste0(output_dir,"/fit_",aou,".rds"))
if("geom" %in% names(fit$meta_strata)){
  fit$meta_strata <- sf::st_drop_geometry(fit$meta_strata)
}

      ind <- generate_indices(fit,
                              alternate_n = "n",
                              hpdi = TRUE,
                              regions = regs_to_estimate,
                              max_backcast = 15)
      saveRDS(ind,paste0(external_dir,"/Indices/Ind_plot_",aou,".rds"))


      if(sp %in% sp_gam){
        inds <- ind

        saveRDS(inds,paste0(external_dir,"/Indices/Inds_",aou,".rds"))
        }else{
      inds <- generate_indices(fit,
                               alternate_n = "n_smooth",
                               hpdi = TRUE,
                               regions = regs_to_estimate,
                               max_backcast = 15
                               )
      saveRDS(inds,paste0(external_dir,"/Indices/Inds_",aou,".rds"))
        }


      raw_data <- fit$raw_data

      saveRDS(raw_data,paste0(external_dir,"/Raw_data/Raw_",aou,".rds"))

    }

print(round(i/nrow(sp_list),2))
  }


parallel::stopCluster(cluster)


