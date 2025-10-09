### Custom trend analysis
### estimates trends based on comparisons of means of annual indices across a collection of years
### here it's comparing mean indices during the years of the second Ontario Breeding Bird Atlas
### with mean indices during the year of the third Ontario Breeding Bird Atlas
library(tidyverse)
library(bbsBayes2)


source("functions/alt_trend_utilities.R")
source("functions/alt_generate_trends.R")
trend_first_years <- c(2001:2005)
trend_last_years <- c(2021:2023)
trend_start <- 2002
trend_end <- 2022

full_dir <- "F:/CWS_2023_BBS_Analyses/"

all_trends <- readRDS(paste0(full_dir,"website/All_BBS_Trends_2023.rds"))

regions <- c("CA-ON-12",
             "CA-ON-8",
             "CA-ON-13")


# species with trends in at least one of those regions
species_list <- all_trends %>%
  filter(region %in% regions) %>%
  select(species,bbs_num) %>%
  distinct()
#
# species_list <- readRDS("data/prairie_sp_table.rds")
trends_out <- NULL
for(j in 1:nrow(species_list)){

  species <- unlist(species_list[j,"species"])
  aou <-unlist(species_list[j,"bbs_num"])

  inds <- readRDS(paste0(full_dir,"indices/Inds_",aou,".rds")) #n_smooth

  indices_regs <- inds[["indices"]] %>%
    filter(region %in% regions)

  for(r in regions){


  samples <- inds$samples[[paste0("stratum_",r)]]

  if(is.null(samples)){next} # species regions combinations that don't exist

  indices_sel <- indices_regs %>%
    filter(region == r)
  mean_inds_start <- rowMeans(samples[,as.character(trend_first_years)])
  mean_inds_end <- rowMeans(samples[,as.character(trend_last_years)])

  indices_samples <- list(matrix(data = c(mean_inds_start,mean_inds_end),
                      ncol = 2,
                      dimnames = list(iter = as.character(1:length(mean_inds_end)),
                                      year = c(trend_start,trend_end))))

  names(indices_samples) <- paste0(unique(indices_sel$region_type),
                                   "_",
                                   unique(indices_sel$region))

  trends <- alt_generate_trends(ind_samples = indices_samples,
                            start_year = trend_start,
                            end_year = trend_end,
                            hpdi = TRUE,
                            quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975),
                            prob_decrease = c(0,30,50),
                            indx = indices_sel)

  trends <- trends %>%
    mutate(english_name = species,
           bbs_num = aou,
           version = "Atlas_period_means")
  trends_out <- bind_rows(trends_out,trends)
  }

  trends_stand <- bbsBayes2::generate_trends(inds,
                                             min_year = trend_start,
                                             max_year = trend_end,
                                             hpdi = TRUE,
                                             quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975),
                                             prob_decrease = c(0,30,50))

  trends_stand_out <- trends_stand$trends %>%
    filter(region %in% regions)  %>%
    mutate(english_name = species,
           bbs_num = aou,
           version = "standard_bbsBayes2")

  trends_out <- bind_rows(trends_out,trends_stand_out)

}

trends_pair <- trends_out %>%
  select(english_name,
         version,
         trend,
         trend_q_0.05,
         trend_q_0.95,
         width_of_95_percent_credible_interval,
         region) %>%
  pivot_wider(values_from = trend:width_of_95_percent_credible_interval,
              id_cols = c(english_name,region),
              names_from = version)

tst <- ggplot(data = trends_pair)+
  geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(x = trend_standard_bbsBayes2,
                 y = trend_Atlas_period_means,
                 colour = region))
tst

tst <- ggplot(data = trends_pair)+
  geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(y = width_of_95_percent_credible_interval_Atlas_period_means,
                 x = width_of_95_percent_credible_interval_standard_bbsBayes2,
                 colour = region))
tst


trends_share <- trends_out %>%
  filter(version == "Atlas_period_means")
write_csv(trends_share,
          paste0("Ontario_Atlas_trends_means_by_atlas_period_",
                 trend_start,"_",trend_end,".csv"))
