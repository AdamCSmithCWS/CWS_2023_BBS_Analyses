## convergence confirmation for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2022_BBS_Analyses")
#setwd("C:/GitHub/CWS_2023_BBS_Analyses")


# set output_dir to the directory where the saved modeling output rds files are stored
# output_dir <- "D:/CWS_2023_BBS_Analyses/output"
# output_dir <- "output"
 output_dir <- "F:/CWS_2022_BBS_Analyses/output"



n_cores = 6
re_run <- TRUE # set to TRUE if re-assessing convergence of models


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)
# sp_re_fit <- readRDS(paste0("species_rerun_converge_fail_2024-12-04.rds"))
# sp_list <- sp_list %>%
#   filter(english %in% sp_re_fit)
#
# sp_rerun <- c("Northern Shrike","Willow Ptarmigan", "Herring Gull",
#               "Common Loon",
#               "American Pipit",
#               "Redpoll (Common/Hoary)")
# sp_list <- sp_list %>%
#   filter(english %in% sp_rerun)
#

# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(1:nrow(sp_list)),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr"),
                .errorhandling = "pass") %dopar%
  {

     #for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

    if(file.exists(paste0(output_dir,"/fit_",aou,".rds")) &
       (!file.exists(paste0("Convergence/summ_",aou,".rds")) | re_run )){

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

      summ <- get_summary(fit)
      saveRDS(summ,paste0("Convergence/summ_",aou,".rds"))


    }


  }


parallel::stopCluster(cluster)

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)


# Compile convergence values ----------------------------------------------
re_compile <- TRUE
if(re_compile){
summ_comb <- NULL

for(i in 1:nrow(sp_list)){

  sp <- as.character(sp_list[i,"english"])
  aou <- as.integer(sp_list[i,"aou"])

  if(file.exists(paste0("Convergence/summ_",aou,".rds"))){


summ <- readRDS(paste0("Convergence/summ_",aou,".rds")) %>%
  mutate(species = sp,
         sp_n = aou)

summ_comb <- bind_rows(summ_comb,
                       summ)


  }

}

saveRDS(summ_comb,"Convergence/All_species_convergence_summary.rds")
}else{
summ_comb <- readRDS("Convergence/All_species_convergence_summary.rds")
}
sp_run <- summ_comb %>%
  group_by(sp_n) %>%
  summarise(max_rhat = max(rhat, na.rm = TRUE),
            min_ess = min(ess_bulk,na.rm = TRUE))

sp_not_run <- sp_list %>%
  left_join(.,sp_run,
            by = c("aou" = "sp_n")) %>%
  filter(is.na(max_rhat)) %>%
  arrange(-n_routes)

sp_not_run_but_should <- sp_not_run %>%
  filter(n_years > 20, n_routes > 20,
         n_obs > 500)
if(nrow(sp_not_run_but_should) > 0){
stop(paste(nrow(sp_not_run_but_should),"species or more are missing, including",
           paste(sp_not_run_but_should$english,collapse = ", "),
           "confirm that they each have < 1 stratum"))
}

fail <- summ_comb %>%
  #filter(rhat > 1.05) %>%
  mutate(variable_type = str_extract(variable,"^\\w+"),
         rhat_fail = ifelse(rhat > 1.05,TRUE,FALSE),
         ess_fail = ifelse(ess_bulk < 100, TRUE, FALSE)) %>%
  group_by(species,sp_n,variable_type,rhat_fail,ess_fail) %>%
  summarise(n_fail = n(),
            max_rhat = max(rhat),
            mean_rhat = mean(rhat),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))


fail_ess <- summ_comb %>%
  mutate(variable_type = str_extract(variable,"^\\w+"),
         ess_fail = ifelse(ess_bulk < 100, TRUE, FALSE)) %>%
  group_by(species,sp_n,variable_type,ess_fail) %>%
  summarise(n_fail = n(),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))

sp_fail_ess <- fail_ess %>%
  ungroup() %>%
  filter(ess_fail) %>%
  select(species) %>%
  distinct() %>%
  unlist()

ess_fail_sum <- fail_ess %>%
  filter(species %in% sp_fail_ess,
         !is.na(ess_fail)) %>%
  pivot_wider(id_cols = c(species,sp_n,variable_type),
              names_from = ess_fail,
              values_from = c(n_fail,min_ess,mean_ess)) %>%
  mutate(n_fail_FALSE = ifelse(is.na(n_fail_FALSE),0,n_fail_FALSE),
         n_fail_TRUE = ifelse(is.na(n_fail_TRUE),0,n_fail_TRUE),
         p_fail = n_fail_TRUE/(n_fail_FALSE+n_fail_TRUE))

ess_fail_rerun <- ess_fail_sum %>%
  filter(p_fail >= 0.01 | (p_fail > 0 & grepl("^n",variable_type)))

### one off decision to not re-run RWBL - ess-fail only for 1.2% of the strata-intercepts (2/163)
ess_fail_rerun <- ess_fail_rerun %>%
  filter(species != "Red-winged Blackbird")


paste(unique(ess_fail_rerun[,c("sp_n","species")]),collapse = ", ")

# rhat fails --------------------------------------------------------------


fail_rhat <- summ_comb %>%
  mutate(variable_type = str_extract(variable,"^\\w+"),
         rhat_fail = ifelse(rhat > 1.05, TRUE, FALSE)) %>%
  group_by(species,sp_n,variable_type,rhat_fail) %>%
  summarise(n_fail = n(),
            max_rhat = max(rhat),
            mean_rhat = mean(rhat))

sp_fail_rhat <- fail_rhat %>%
  ungroup() %>%
  filter(rhat_fail) %>%
  select(species) %>%
  distinct() %>%
  unlist()

rhat_fail_sum <- fail_rhat %>%
  filter(species %in% sp_fail_rhat,
         !is.na(rhat_fail)) %>%
  pivot_wider(id_cols = c(species,sp_n,variable_type),
              names_from = rhat_fail,
              values_from = c(n_fail,max_rhat,mean_rhat)) %>%
  mutate(n_fail_FALSE = ifelse(is.na(n_fail_FALSE),0,n_fail_FALSE),
         n_fail_TRUE = ifelse(is.na(n_fail_TRUE),0,n_fail_TRUE),
         p_fail = n_fail_TRUE/(n_fail_FALSE+n_fail_TRUE))

rhat_fail_rerun <- rhat_fail_sum %>%
  filter(p_fail >= 0.01 | (p_fail > 0 & grepl("^n",variable_type)))

paste(unique(rhat_fail_rerun[,c("sp_n","species")]),collapse = ", ")


species_re_run_combined <- unique(c(unique(rhat_fail_rerun$species),
                             unique(ess_fail_rerun$species)))

saveRDS(species_re_run_combined,
        file = paste0("species_rerun_converge_fail_",as_date(Sys.Date()),".rds"))


# copy_model_file("gamye","spatial",
#                 dir = "models_alt")
# above is to provide a model file to set up some alternate priors
# for the convergence fails

# # explore site effect fails for a selected species ------------------------
#
#
# sp <- "Sandhill Crane"
# sp_sel <- summ_comb %>%
#   filter(species == sp) %>%
#   mutate(variable_type = str_extract(variable,"^\\w+"))
#
#
# sdste <- sp_sel %>%
#   filter(grepl("sdste",variable_type))
# ste <- sp_sel %>%
#   filter(grepl("ste_raw",variable_type)) %>%
#   mutate(scaled_mean = mean*as.numeric(sdste$mean),
#          scaled_lci = q5*as.numeric(sdste$mean),
#          scaled_uci = q95*as.numeric(sdste$mean))
#
