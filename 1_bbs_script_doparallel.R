## Script to fit BBS models in parallel
##

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)
library(cmdstanr)

setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2023_BBS_Analyses")

# set output_dir to the directory where the saved modeling output rds files will be stored
# necessary on most of my machines and VMs because these output files are very large
# ( > 5GB/species for broad-ranging species)
output_dir <- "F:/CWS_2023_BBS_Analyses/output"
#output_dir <- "output"

write_over <- TRUE # set to TRUE if overwriting previously run models
re_fit <- FALSE# set to TRUE if re-running poorly converged models

if(re_fit){
  #sp_re_fit <- readRDS(paste0("species_rerun_converge_fail_",as_date(Sys.Date()),".rds"))
  sp_re_fit <- readRDS(paste0("species_rerun_converge_fail_2024-12-04.rds"))
  sp_re_fit <- c("American Robin")
}

miss <- FALSE
csv_recover <- FALSE
machine = NULL
#machine = 1




#n_cores <- floor((parallel::detectCores()-1)/4) # requires 4 cores per species

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

if(re_fit){
  sp_list <- sp_list %>%
    filter(english %in% sp_re_fit)
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
#


# build cluster -----------------------------------------------------------

n_cores = 5
#n_cores <- floor(parallel::detectCores()/4)-1

cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(1:nrow(sp_list)),
        .packages = c("bbsBayes2",
                      "tidyverse",
                      "cmdstanr"),
        .errorhandling = "pass") %dopar%
  {

   # for(i in 1:4){
    for(i in rev(1:nrow(sp_list))){  # tmp_clr){ #
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

    if((!file.exists(paste0(output_dir,"/fit_gam_",aou,".rds")) &
       !file.exists(paste0("fit_",aou,"-",c(1),".csv"))) |  # checks to see if the model has been fit or if it is currently running
       (write_over & !file.exists(paste0("fit_gam_",aou,"-",c(1),".csv"))) |  # if TRUE and model is not currently running
       (re_fit & (!file.exists(paste0(output_dir,"/fit_",aou,".rds")) &
                  !file.exists(paste0("fit_gam_",aou,"-",c(1),".csv"))) ) | # if refitting and model is not currently running
       csv_recover){ # if TRUE then doesn't re-fit just reads in the csv files that may have failed to save to external disk

      if(csv_recover & !file.exists(paste0("fit_",aou,"-",c(1),".csv"))){next}

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
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE,
               min_year = fy)

   if("CA-NU-3" %in% s$meta_strata$strata_name){
     strat_alt <- load_map(strat) %>%
       filter(strata_name != "CA-NU-3")
     s <- stratify(by = strat,
                   strata_custom = strat_alt,
                   release = 2024,
                   species = sp,
                   quiet = TRUE) %>%
       prepare_data(min_max_route_years = 2,
                    quiet = TRUE,
                    min_year = fy)

   }
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
     csv_files <- paste0("fit_",aou,"-",c(1:4),".csv")
      check1 <- try(cmdstanr::as_cmdstan_fit(files = csv_files),
                    silent = TRUE)
      if(class(check1)[1] == "try-error"){
        check1 <- try(cmdstanr::as_cmdstan_fit(files = csv_files),
                      silent = TRUE)
      }
        if(class(check1)[1] == "try-error"){
          print(aou)
          print(check1)
          next}
      check2 <- try(check1$summary(variables = "STRATA"),silent = TRUE)
      if(class(check2)[1] == "try-error"){
        print(aou)
        print(check2)
        next}
      fit[["model_fit"]] <- check1
      save_model_run(fit,retain_csv = FALSE,
                     save_file_path = paste0(output_dir,
                                             "/fit_",
                                             aou,
                                             ".rds"))

     next}

if(re_fit){

  # bbs_dat <- prepare_spatial(s,
  #                            strata_map = load_map(strat)) %>%
  #   prepare_model(model = "gam",
  #                 model_variant = "spatial")

  # bbs_dat <- prepare_spatial(s,
  #                            strata_map = load_map(strat)) %>%
  #   prepare_model(.,
  #                 model = "gamye",
  #                 model_variant = "spatial",
  #                 model_file = "models_alt/gamye_spatial_bbs_CV_COPY.stan")

fit <- run_model(model_data = bbs_dat,
                 refresh = 400,
                 iter_warmup = 6000,
                 iter_sampling = 4000,
                 thin = 4,
                 output_dir = output_dir,
                 #output_basename = paste0("fit_gam_",aou),
                 output_basename = paste0("fit_",aou),
                 save_model = FALSE,
                 overwrite = write_over,
                 show_exceptions = FALSE,
                 init_alternate = 1)

# Summ <- fit$model_fit$summary()




}else{
  fit <- run_model(model_data = bbs_dat,
                   refresh = 400,
                   output_basename = paste0("fit_",aou),
                   save_model = FALSE,
                   overwrite = write_over,
                   init_alternate = 1)

}

   bbsBayes2::save_model_run(fit,
                             retain_csv = FALSE,
                             save_file_path = paste0(output_dir,
                                                     "/fit_",
                                                     #"/fit_gam_",
                                                     aou,
                                                     ".rds"))



    }# end of if file.exists

  }

parallel::stopCluster(cluster)


