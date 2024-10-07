## testing bbsBayes2 parallel in HRE env

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)
library(cmdstanr)
#setwd("C:/github/CWS_2022_BBS_Analyses")
#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2022_BBS_Analyses")


# set output_dir to the directory where the saved modeling output rds files will be stored
# necessary on most systems because these output files are very large ( up to 5GB for broad-ranging species)
#output_dir <- "F:/CWS_2023_BBS_Analyses/output"
output_dir <- "output"

re_run <- FALSE # set to TRUE if re-running poorly converged models


miss <- FALSE
csv_recover <- FALSE

machine = 1
n_cores = 4

#n_cores <- floor((detectCores()-1)/4) # requires 4 cores per species

if(!is.null(machine)){
sp_list <- readRDS("species_list.rds") %>%
  filter(vm %in% machine,
         model == TRUE)
}else{
  sp_list <- readRDS("species_list.rds") %>%
    filter(model == TRUE)
}

if(miss){
  sp_list <- readRDS("species_missing.rds") %>%
    filter(model == TRUE)
}

if(re_run){
  sp_list <- sp_list %>%
    filter(english %in% sp_rerun)
}
# completed_files <- list.files("output",pattern = "fit_")
# completed_aou <- as.integer(str_extract_all(completed_files,
#                              "[[:digit:]]{1,}",
#                              simplify = TRUE))
# sp_list <- sp_list %>%
#     filter(!aou %in% completed_aou)
#
# sp_list <- sp_list %>% filter(!aou %in% c(6882,5630,4090))
#
# i <- which(sp_list$aou == 6882)
# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = c(35:43),#rev(1:nrow(sp_list)),
        .packages = c("bbsBayes2",
                      "tidyverse",
                      "cmdstanr"),
        .errorhandling = "pass") %dopar%
  {

   # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

    if(!file.exists(paste0(output_dir,"/fit_",aou,".rds")) |
       re_run){

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
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE,
               min_year = fy)#,
               #min_n_routes = 1)

   ## bbsBayes2 models do not currently work unless n_strata > 1
   if(nrow(s$meta_strata) == 1){stop(paste("Only 1 stratum for",sp,"skipping to next species"))}

   if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
  bbs_dat <- prepare_spatial(s,
                  strata_map = load_map(strat)) %>%
  prepare_model(.,
                model = "gamye",
                model_variant = "spatial")

   }else{
     bbs_dat <- prepare_model(s,
                     model = "gamye",
                     model_variant = "hier")
   }

   if(csv_recover){
     fit <- bbs_dat
     csv_files <- paste0(output_dir,"/fit_",aou,"-",c(1:4),".csv")
      fit[["model_fit"]] <- cmdstanr::as_cmdstan_fit(files = csv_files)
      save_model_run(fit,retain_csv = TRUE)

     next}

if(re_run){
fit <- run_model(model_data = bbs_dat,
                 refresh = 400,
                 iter_warmup = 2000,
                 iter_sampling = 2000,
                 thin = 2,
                 #output_dir = output_dir,
                 output_basename = paste0("fit_",aou),
                 save_model = FALSE,
                 overwrite = TRUE,
                 init = 1)

# Summ <- fit$model_fit$summary()




}else{
  fit <- run_model(model_data = bbs_dat,
                   refresh = 400,
                   output_basename = paste0("fit_",aou),
                   save_model = FALSE,
                   overwrite = TRUE,
                   init = 1)

}

   bbsBayes2::save_model_run(fit,
                             retain_csv = FALSE,
                             save_file_path = paste0(output_dir,
                                                     "/fit_",
                                                     aou,
                                                     ".rds"))



    }# end of if file.exists

  }

parallel::stopCluster(cluster)


