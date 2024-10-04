## generate indices for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)
library(patchwork)

YYYY <- 2022
short_time <- 10

#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2022_BBS_Analyses")
setwd("C:/github/CWS_2022_BBS_Analyses")


# custom functions to calculate reliability categories and determine website inclusion
source("functions/web_trends.R")
source("functions/reliability.R")



n_cores <- 10
re_run <- TRUE


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

# load previous coverage data -----------------------------------------------------------

lastyear = read_csv("data/All_2021_BBS_trends.csv")
covs = lastyear[,c("species","bbs_num","Region","Region_alt","Trend_Time","reliab.cov")] %>%
  mutate(Region = ifelse(Region == "Continental","continent",Region),
         Region = ifelse(Region_alt == "Canada","Canada",Region),
         Region = ifelse(Region == "US","United States of America",Region))

three_gens <- read_csv("data/full_bbs_species_list_w_generation_length.csv")

three_gens <- three_gens %>%
  select(aou,GenLength)

sp_list <- sp_list %>%
  inner_join(.,three_gens,
             by = c("aou"))



# CV_threshold <- function(m,ci,thresh = 100){
#   y <- ifelse(ci/m > thresh,TRUE,FALSE)
#   return(y)
# }
#



# reliability category definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100
names(prec_cuts) <- c("High","Medium")

cov_cuts = c(0.5,0.25)
names(cov_cuts) <- c("High","Medium")

pool_cuts = c(0.33,0.1)
names(pool_cuts) <- c("High","Medium")

backcast_cuts = c(0.90,0.75)
names(backcast_cuts) <- c("High","Medium")




# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(1:nrow(sp_list)),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr",
                              "patchwork"),
                .errorhandling = "pass") %dopar%
  {

     #for(i in c(nrow(sp_list):(nrow(sp_list)-4))){
    sp <- as.character(sp_list[i,"english"])
    esp <- as.character(sp_list[i,"french"])
    aou <- as.integer(sp_list[i,"aou"])
    species_f_bil <- gsub(paste(esp,sp),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")


    cov_sp <- covs %>%
      filter(bbs_num == aou)

    if(file.exists(paste0("Indices/Inds_",aou,".rds")) &
       (!file.exists(paste0("Trends/",aou,"_trends.rds")) | re_run)){



      # identifying first years for selected species ----------------------------
      fy <- 1970
      if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
        fy <- max(fy,1978) #5 years after the split
      }
      if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
        fy <- max(fy,1990) #5 years after the split and first year EUCD observed on > 3 BBS routes
      }
      if(aou == 6121){ # CAve Swallow
        fy = max(fy,1985)
      }

      ## set three generations
      ## unless < 10, then 10   or  unless > number of years available, then n-years
      gen3 <- min((YYYY-fy),max(10,round(as.numeric(sp_list[i,"GenLength"])*3)))


      inds <- readRDS(paste0("Indices/Inds_",aou,".rds"))

      ind <- readRDS(paste0("Indices/Ind_plot_",aou,".rds"))


# Estimate trends for long- short- and three-gen --------------------------

      first_year_long <- fy
      first_year_short <- YYYY-10
      first_year_three <- YYYY-gen3


      maps_out <- vector("list",3)
      names(maps_out) <- c("Long-term","Short-term","Three-generation")


      maps_out_quart <- vector("list",3)
      names(maps_out_quart) <- c("Long-term","Short-term","Three-generation")


      trends_out <- NULL
      inds_out <- NULL

      start_years <- c(first_year_long,first_year_short,first_year_three)
      names(start_years) <- c("Long-term","Short-term","Three-generation")


      for(j in names(start_years)){

        cov_sp_y <- cov_sp %>%
          filter(Trend_Time == j) %>%
          select(bbs_num,Region,reliab.cov)

        if(nrow(cov_sp_y) == 0){
          cov_sp_y <- cov_sp %>%
            filter(Trend_Time == "Short-term") %>%
            select(bbs_num,Region,reliab.cov)

        }

          ssy <- start_years[j]

        trends_tmp <- generate_trends(inds,
                                      min_year = ssy,
                                      quantiles = c(0.025, 0.05, 0.10, 0.25, 0.75, 0.9, 0.95, 0.975),
                                      prob_decrease = c(0,25,30,50),
                                      prob_increase = c(0,33,100),
                                      hpdi = TRUE)

        map_tmp <- plot_map(trends_tmp,
                            title = FALSE) +
          labs(title = j)

        maps_out[[j]] <- map_tmp

        map_tmp2 <- plot_map(trends_tmp,
                    title = FALSE,
                    alternate_column = "trend_q_0.25") +
          labs(title = paste(j,"25% CI (trend_q_0.25)"))

        map_tmp3 <- plot_map(trends_tmp,
                             title = FALSE,
                             alternate_column = "trend_q_0.75") +
          labs(title = paste(j,"75% CI (trend_q_0.75)"))

        map_tmp4 <- map_tmp2 + map_tmp3 + plot_layout(guides = "collect")
        maps_out_quart[[j]] <- map_tmp4





          trend_sv <- trends_tmp$trends %>%
            mutate(species = sp,
                   espece = esp,
                   bbs_num = aou,
                   trend_time = j,
                   for_web = for_web_func(strata_included,strata_excluded)) %>%
            left_join(.,cov_sp_y,by = c("bbs_num",
                                     "region" = "Region"))



        trends_out <- bind_rows(trends_out,trend_sv)

        ind_tmp <- ind$indices %>%
          filter(year >= ssy) %>%
          mutate(species = sp,
                 espece = esp,
                 bbs_num = aou,
                 trend_time = j,
                 for_web = for_web_func(strata_included,strata_excluded),
                 indices_type = "full") %>%
          mutate(across(where(is.double) & !contains("year") &
                          !starts_with("n_") & !starts_with("bbs_num"),~signif(.,3)))



        inds_out <- bind_rows(inds_out,ind_tmp)

        ind_tmp <- inds$indices %>%
          filter(year >= ssy) %>%
          mutate(species = sp,
                 espece = esp,
                 bbs_num = aou,
                 trend_time = j,
                 for_web = for_web_func(strata_included,strata_excluded),
                 indices_type = "smooth")%>%
          mutate(across(where(is.double) & !contains("year") &
                          !starts_with("n_") & !starts_with("bbs_num"),~signif(.,3)))

        inds_out <- bind_rows(inds_out,ind_tmp)


      }

      trends_out <- trends_out  %>%
        mutate(precision = reliab_func_prec(width_of_95_percent_credible_interval),
               coverage = reliab_func_cov(reliab.cov),
               backcast_reliab = reliab_func_backcast(backcast_flag),
               reliability = reliability_func(precision,coverage,backcast_reliab)) %>%
        mutate(across(where(is.double) & !contains("year") &
                        !starts_with("n_") & !starts_with("bbs_num"),~signif(.,3)))

      saveRDS(trends_out, file = paste0("Trends/",aou,"_trends.rds"))

      saveRDS(inds_out, file = paste0("Indices/list_",aou,"_indices.rds"))


    saveRDS(maps_out,file = paste0("Figures/temp_rds_storage/",aou,"_maps.RDS"))
    saveRDS(maps_out_quart,file = paste0("Figures/temp_rds_storage/",aou,"_quart_maps.RDS"))


    }


  }


parallel::stopCluster(cluster)



