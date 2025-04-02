
### Generates the csv tables of trends and annual indices for sharing through the
### Google Drive and for upload into the NatureCounts database to support
###  - State of Canada's Birds, etc.
###
YYYY <- 2023

webmaps <- FALSE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(readxl)
#setwd("C:/GitHub/CWS_2023_BBS_Analyses")

external_dir <- "F:/CWS_2023_BBS_Analyses"

source("functions/mapping.R")
source("functions/loess_func.R")
# custom functions to calculate reliability categories and determine website inclusion


sp_list <- readRDS("sp_list_w_generations.rds") %>%
  filter(model == TRUE)

#
# avian_core <- read_csv("data/ECCC Avian Core 20230601.csv") %>%
#   rename_with(.,.fn = ~paste0(.x,"_core")) %>%
#   mutate(aou = as.integer(BBS_Number_core))
#
# rep_aou <- avian_core %>% group_by(aou) %>% summarise(n = n()) %>% filter(n > 1, !is.na(aou))
# rep_core <- avian_core %>%
#   filter(aou %in% rep_aou$aou)
#
# avian_core <- avian_core %>%
#   filter(!(aou %in% rep_aou$aou & Full_Species_core == "No"))
#
# sp_list <- sp_list %>%
#   inner_join(.,avian_core,by = "aou")
#
# naturecounts_codes <- naturecounts::meta_species_codes() %>%
#   filter(authority == "BBS2") %>%
#   select(species_id2,species_code) %>%
#   mutate(aou = as.integer(species_code),
#          naturecounts_species_id = species_id2) %>%
#   distinct() %>%
#   select(-c(species_id2,species_code))
#
# sp_list <- sp_list %>%
#   left_join(.,naturecounts_codes,
#             by = "aou")
#
# tmp2 <- sp_list %>% filter(is.na(naturecounts_species_id)) %>%
#   ungroup() %>%
#   select(aou,english,BBS_Number_core,naturecounts_species_id)
# if(nrow(tmp2) > 0){
#   stop("Species don't match with nature counts")
# }



re_collect <- TRUE
# Compile all trends and indices ------------------------------------------------------

if(re_collect){
trends <- NULL
indices <- NULL
indices_smooth <- NULL

for(i in 1:nrow(sp_list)){


  aou <- as.integer(sp_list[i,"aou"])

  if(file.exists(paste0(external_dir,"/Indices/list_",aou,"_indices.rds"))){
  inds_1 <- readRDS(paste0(external_dir,"/Indices/list_",aou,"_indices.rds"))

  trends_1 <- readRDS(paste0(external_dir,"/Trends/",aou,"_trends.rds"))

  trends <- bind_rows(trends,trends_1)

  inds_1s <- inds_1 %>%
    filter(indices_type == "smooth")
  inds_1f <- inds_1 %>%
    filter(indices_type == "full")

  indices_smooth <- bind_rows(indices_smooth,inds_1s)
  indices <- bind_rows(indices,inds_1f)
}


  print(round(i/nrow(sp_list),2))



}

saveRDS(trends,"output/trends_collected.rds")
saveRDS(indices,"output/indices_collected.rds")
saveRDS(indices_smooth,"output/indices_smooth_collected.rds")

}else{


  trends <- readRDS("output/trends_collected.rds")
  indices <- readRDS("output/indices_collected.rds")
  indices_smooth <- readRDS("output/indices_smooth_collected.rds")

}

# Compare to last year's trends -------------------------------------------

avian_core <- readxl::read_xlsx("data/ECCC Avian Core 20241025.xlsx") %>%
  filter(Full_Species == "Yes") %>%
  select(Species_ID, BBS_Number, Sort_Order) %>%
  rename_with(.,.fn = ~paste0(.x,"_core")) %>%
  distinct() %>%
  mutate(aou = as.integer(BBS_Number_core))

core_link <- sp_list %>%
  ungroup() %>%
  select(naturecounts_sort_order,aou,naturecounts_species_id) %>%
  left_join(avian_core, by = "aou")



lastyear = read_csv("data/All_BBS_trends_2022.csv")

ly_trends <- lastyear[,c("species","bbs_num","region","trend_time",
                         "n_strata_included","n_routes",
                         "trend",
                         "trend_q_0.05","trend_q_0.95",
                         "width_of_95_percent_credible_interval",
                         "reliab.cov","coverage")] %>%
  rename(trend_2022 = trend,
         trend_q_0.05_2022 = trend_q_0.05,
         trend_q_0.95_2022 = trend_q_0.95,
         number_of_strata_2022 = n_strata_included,
         number_of_routes_2022 = n_routes,
         CI_2022 = width_of_95_percent_credible_interval,
         reliab_cov_2022 = reliab.cov,
         coverage_2022 = coverage) %>%
  filter(region %in% c("continent","Canada","United States of America")) %>%
  select(-c(species))



trends_comp <- trends %>%
  inner_join(.,ly_trends,
             by = c("bbs_num",
                    "region",
                    "trend_time")) %>%
  left_join(.,core_link,by = c("bbs_num" = "aou")) %>%
  mutate(diff_trend = trend - trend_2022,
         diff_coverage = reliab.cov - reliab_cov_2022) %>%
  rename(CI = width_of_95_percent_credible_interval)


comp_xy <- ggplot(data = trends_comp,
                  aes(x = trend_2022,
                      y = trend,
                      alpha = 1/CI))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  facet_grid(cols = vars(trend_time),
             rows = vars(region),
             scales = "free")

comp_xy




comp_xy_cov <- ggplot(data = trends_comp,
                  aes(x = reliab_cov_2022,
                      y = reliab.cov,
                      colour = coverage))+
  geom_point(alpha = 0.2)+
  geom_abline(intercept = 0,slope = 1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  facet_grid(cols = vars(trend_time),
             rows = vars(region),
             scales = "free")

comp_xy_cov



trends_comp_sel <- trends_comp %>%
  filter(diff_trend > 1 | diff_trend < -1,
         region_type == "continent")


comp_xy_sel <- ggplot(data = trends_comp_sel,
                  aes(x = trend_2022,
                      y = trend,
                      colour = factor(bbs_num)))+
  geom_point()+
  geom_errorbar(aes(ymin = trend_q_0.05, ymax = trend_q_0.95),
                alpha = 0.4)+
  geom_errorbarh(aes(xmin = trend_q_0.05_2022, xmax = trend_q_0.95_2022),
                 alpha = 0.4)+
  geom_abline(intercept = 0,slope = 1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_text_repel(aes(label = factor(bbs_num)))+
  facet_grid(rows = vars(trend_time),
             cols = vars(region),
             scales = "free")

comp_xy_sel



# Compile and organize trends and indices for SOCB ------------------------

# reconcile with template and Catherine's email


trends <- trends %>%
  mutate(prob_LD = prob_decrease_50_percent,
         prob_MD = prob_decrease_25_percent - prob_decrease_50_percent,
         prob_LC = (prob_decrease_0_percent-prob_decrease_25_percent)+(prob_increase_0_percent-prob_increase_33_percent) ,
         prob_MI = prob_increase_33_percent - prob_increase_100_percent,
         prob_LI = prob_increase_100_percent,
         region_type = factor(region_type,
                              levels = c("continent","country","prov_state","bcr","bcr_by_country","stratum"),
                              ordered = TRUE)) %>%
  left_join(.,core_link,by = c("bbs_num" = "aou"))%>%
  relocate(region,region_type,species,espece,trend_time,start_year,end_year,
           starts_with("trend"),
           starts_with("percent"),
           width_of_95_percent_credible_interval,
           starts_with("prob_"),
           rel_abundance, n_routes, mean_n_routes, n_strata_included, backcast_flag) %>%
  arrange(naturecounts_sort_order,region_type,region,start_year)


test_probs <- trends %>%
  mutate(prob_test = prob_LD+prob_MD+prob_LC+prob_MI+prob_LI)

if(any(round(test_probs$prob_test,2) != 1)){stop("probabilites of change categories don't sum properly")}


# Indices reorder ---------------------------------------------------------


indices_smooth <- indices_smooth %>%
  left_join(.,core_link,by = c("bbs_num" = "aou")) %>%
  mutate(region_type = factor(region_type,
                              levels = c("continent","country","prov_state","bcr","bcr_by_country","stratum"),
                              ordered = TRUE),
         trend_time = factor(trend_time,
                             levels = c("Long-term","Three-generation","Short-term"),
                             ordered = TRUE)) %>%
  relocate(region,region_type,year,species,espece,trend_time,indices_type,
           starts_with("index"),
           starts_with("n_"),
           obs_mean, backcast_flag)%>%
    arrange(naturecounts_sort_order,region_type,region,trend_time,year)

indices <- indices %>%
  left_join(.,core_link,by = c("bbs_num" = "aou")) %>%
  mutate(region_type = factor(region_type,
                              levels = c("continent","country","prov_state","bcr","bcr_by_country","stratum"),
                              ordered = TRUE),
         trend_time = factor(trend_time,
                             levels = c("Long-term","Three-generation","Short-term"),
                             ordered = TRUE)) %>%
  relocate(region,region_type,year,species,espece,trend_time,indices_type,
           starts_with("index"),
           starts_with("n_"),
           obs_mean, backcast_flag)%>%
  arrange(naturecounts_sort_order,region_type,region,trend_time,year)



# csv files with trends and indices for Google Drive ----------------------

saveRDS(indices,paste0(external_dir,"/Website/All_BBS_Full_Indices_",YYYY,".rds"))
saveRDS(indices_smooth,paste0(external_dir,"/Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))
saveRDS(trends,paste0(external_dir,"/Website/All_BBS_Trends_",YYYY,".rds"))

# trends <- readRDS(paste0(external_dir,"/Website/All_BBS_Trends_",YYYY,".rds"))
# indices_smooth <- readRDS(paste0(external_dir,"/Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))
# indices <- readRDS(paste0(external_dir,"/Website/All_BBS_Full_Indices_",YYYY,".rds"))
#
# tst <- indices_smooth %>% filter(region == "continent", trend_time == "Long-term") %>% group_by(species) %>% summarise(n_test = n())

readr::write_excel_csv(indices,paste0(external_dir,"/Website/All_BBS_Full_Indices_",YYYY,".csv"))
readr::write_excel_csv(indices_smooth,paste0(external_dir,"/Website/All_BBS_Smoothed_Indices_",YYYY,".csv"))
readr::write_excel_csv(trends,paste0(external_dir,"/Website/All_BBS_Trends_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("continent","country"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Full_Indices_continent_country_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("prov_state"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Full_Indices_prov_state_",YYYY,".csv"))


inds_select <- indices %>%
  filter(region_type %in% c("bcr"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Full_Indices_bcr_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("bcr_by_country"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Full_Indices_bcr_by_country_",YYYY,".csv"))


inds_select <- indices_smooth %>%
  filter(region_type %in% c("continent","country"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Smoothed_Indices_continent_country_",YYYY,".csv"))

inds_select <- indices_smooth %>%
  filter(region_type %in% c("prov_state"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Smoothed_Indices_prov_state_",YYYY,".csv"))


inds_select <- indices_smooth %>%
  filter(region_type %in% c("bcr"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Smoothed_Indices_bcr_",YYYY,".csv"))

inds_select <- indices_smooth %>%
  filter(region_type %in% c("bcr_by_country"))
readr::write_excel_csv(inds_select,paste0(external_dir,"/Website/BBS_Smoothed_Indices_bcr_by_country_",YYYY,".csv"))




trends_select <- trends %>%
  filter(region_type %in% c("continent","country","prov_state"))
readr::write_excel_csv(trends_select,paste0(external_dir,"/Website/BBS_Trends_continent_country_prov_state_",YYYY,".csv"))
trends_select <- trends %>%
  filter(region_type %in% c("bcr","bcr_by_country"))
readr::write_excel_csv(trends_select,paste0(external_dir,"/Website/BBS_Trends_bcr_bcr_by_country_",YYYY,".csv"))
trends_select <- trends %>%
  filter(region_type %in% c("stratum"))
readr::write_excel_csv(trends_select,paste0(external_dir,"/Website/BBS_Trends_strata_",YYYY,".csv"))


sp_no_coverage <- trends %>%
  mutate(w_cov = ifelse(is.na(reliab.cov),FALSE,TRUE)) %>%
  group_by(species,trend_time,w_cov) %>%
  summarise(n_regions = n(),
            .groups = "drop") %>%
  pivot_wider(.,values_from = n_regions,names_from = w_cov,names_prefix = "cov") %>%
  mutate(covTRUE = ifelse(is.na(covTRUE),0,covTRUE),
         covFALSE = ifelse(is.na(covFALSE),0,covFALSE),
         p_missing_coverage = covFALSE/covTRUE)


saveRDS(sp_no_coverage,"species_coverage_summary.rds")

# SOCB upload files -------------------------------------------------------


socb_areas <- read_csv("data/SOCB_regions.csv") %>%
  filter(results_code == "BBS") %>%
  select(area_code,area_name)

#
# trends_out <- trends %>%
#   filter((for_web == TRUE | region %in% c("continent","United States of America")))

trends_out2 <- trends  %>%
  mutate(years = paste(start_year,end_year,sep = "-"),
         results_code = "BBS",
         season = "breeding",
         version = YYYY,
         area_code = ifelse(region == "continent","Continental",region),
         area_code = gsub(area_code,pattern = "United States of America",
                          replacement = "USA"),
         area_code = ifelse(region_type == "bcr",paste0("BCR_",region),area_code),
         model_type = "GAMYE",
         index_type = "mean_predicted_count",
         sample_size_units = "number of routes",
         trend_time = ifelse(trend_time == "Three-generation","3Gen-Recent",trend_time)) %>%
  left_join(.,socb_areas, by = "area_code") %>%
  #select(-area_code) %>%
  rename(species_name = species,
         species_code = Species_ID_core,
         species_id = naturecounts_species_id,
         period = trend_time,
         year_start = start_year,
         year_end = end_year,
         trnd = trend,
         lower_ci = trend_q_0.025,
         upper_ci = trend_q_0.975,
         percent_change = percent_change,
         percent_change_low = percent_change_q_0.025,
         percent_change_high = percent_change_q_0.975,
         prob_decrease_0 = prob_decrease_0_percent,
         prob_decrease_25 = prob_decrease_25_percent,
         prob_decrease_30 = prob_decrease_30_percent,
         prob_decrease_50 = prob_decrease_50_percent,
         prob_increase_0 = prob_increase_0_percent,
         prob_increase_33 = prob_increase_33_percent,
         prob_increase_100 = prob_increase_100_percent,
         precision_num = width_of_95_percent_credible_interval,
         precision_cat = precision,
         coverage_num = reliab.cov,
         coverage_cat = coverage,
         sample_size_alt = mean_n_routes,
         sample_size = n_routes,
         prob_LD = prob_LD,
         prob_MD = prob_MD,
         prob_LC = prob_LC,
         prob_MI = prob_MI,
         prob_LI = prob_LI)




trends_socb <- trends_out2 %>%
  select(results_code,
           version,
           area_code,
           area_name,
           season,
           period,
           species_name,
           species_code,
           species_id,
           years,
           year_start,
           year_end,
           trnd,
           lower_ci,
           upper_ci,
           index_type,
           model_type,
           percent_change,
           percent_change_low,
           percent_change_high,
           prob_decrease_0,
           prob_decrease_25,
           prob_decrease_30,
           prob_decrease_50,
           prob_increase_0,
           prob_increase_33,
           prob_increase_100,
           precision_num,
           precision_cat,
           coverage_num,
           coverage_cat,
           sample_size,
           sample_size_units,
           prob_LD,
           prob_MD,
           prob_LC,
           prob_MI,
           prob_LI)

readr::write_excel_csv(trends_socb,
                       paste0(external_dir,"/website/BBS_",YYYY,"_trends_for_socb.csv"))

# tmp <- trends_socb %>%
#   filter(species_name == "Killdeer")
# readr::write_excel_csv(tmp,
#                        paste0("website/BBS_",YYYY,"_trends_for_Killdeer.csv"))
#


# SOCB extra trends -------------------------------------------------------
#
# trends_out <- trends %>%
#   filter((for_web == FALSE & !(region %in% c("continent","United States of America"))))
#
# trends_out2 <- trends_out  %>%
#   mutate(years = paste(start_year,end_year,sep = "-"),
#          results_code = "BBS",
#          season = "breeding",
#          version = YYYY,
#          area_code = ifelse(region == "continent","Continental",region),
#          area_code = gsub(area_code,pattern = "United States of America",
#                           replacement = "USA"),
#          area_code = ifelse(region_type == "bcr",paste0("BCR_",region),area_code),
#          model_type = "GAMYE",
#          index_type = "mean_predicted_count",
#          sample_size_units = "number of routes",
#          trend_time = ifelse(trend_time == "Three-generation","3Gen-Recent",trend_time)) %>%
#   left_join(.,socb_areas, by = "area_code") %>%
#   #select(-area_code) %>%
#   rename(species_name = species,
#          species_code = Species_ID_core,
#          species_id = naturecounts_species_id,
#          period = trend_time,
#          year_start = start_year,
#          year_end = end_year,
#          trnd = trend,
#          lower_ci = trend_q_0.025,
#          upper_ci = trend_q_0.975,
#          percent_change = percent_change,
#          percent_change_low = percent_change_q_0.025,
#          percent_change_high = percent_change_q_0.975,
#          prob_decrease_0 = prob_decrease_0_percent,
#          prob_decrease_25 = prob_decrease_25_percent,
#          prob_decrease_30 = prob_decrease_30_percent,
#          prob_decrease_50 = prob_decrease_50_percent,
#          prob_increase_0 = prob_increase_0_percent,
#          prob_increase_33 = prob_increase_33_percent,
#          prob_increase_100 = prob_increase_100_percent,
#          precision_num = width_of_95_percent_credible_interval,
#          precision_cat = precision,
#          coverage_num = reliab.cov,
#          coverage_cat = coverage,
#          sample_size_alt = mean_n_routes,
#          sample_size = n_routes,
#          prob_LD = prob_LD,
#          prob_MD = prob_MD,
#          prob_LC = prob_LC,
#          prob_MI = prob_MI,
#          prob_LI = prob_LI)
#
#
#
#
# trends_socb <- trends_out2 %>%
#   select(results_code,
#          version,
#          area_code,
#          area_name,
#          season,
#          period,
#          species_name,
#          species_code,
#          species_id,
#          years,
#          year_start,
#          year_end,
#          trnd,
#          lower_ci,
#          upper_ci,
#          index_type,
#          model_type,
#          percent_change,
#          percent_change_low,
#          percent_change_high,
#          prob_decrease_0,
#          prob_decrease_25,
#          prob_decrease_30,
#          prob_decrease_50,
#          prob_increase_0,
#          prob_increase_33,
#          prob_increase_100,
#          precision_num,
#          precision_cat,
#          coverage_num,
#          coverage_cat,
#          sample_size,
#          sample_size_units,
#          prob_LD,
#          prob_MD,
#          prob_LC,
#          prob_MI,
#          prob_LI)
#
# readr::write_excel_csv(trends_socb,
#                        paste0(external_dir,"/website/BBS_",YYYY,"_extra_trends_for_socb.csv"))
#
#



# SOCB indices ------------------------------------------------------------


smooth_join <- indices_smooth %>%
  select(species,region,region_type,trend_time,
         year,index) %>%
  rename(smooth_index = index)

indices_socb <- indices %>%
  #filter((for_web == TRUE | region %in% c("continent","United States of America"))) %>%
  inner_join(.,smooth_join,
             by = c("species",
                    "region",
                    "region_type",
                    "trend_time",
                    "year")) %>%
  group_by(species,region,region_type,trend_time) %>%
  mutate(LOESS_index = loess_func(index,year),
         results_code = "BBS",
         season = "breeding",
         version = YYYY,
         area_code = ifelse(region == "continent","Continental",region),
         area_code = gsub(area_code,pattern = "United States of America",
                          replacement = "USA"),
         area_code = ifelse(region_type == "bcr",paste0("BCR_",region),area_code),
         trend_time = as.character(trend_time),
         trend_time = ifelse(trend_time == "Three-generation","3Gen-Recent",trend_time)) %>%
  ungroup() %>%
  left_join(.,socb_areas, by = "area_code") %>%
  #select(-area_code) %>%
  rename(species_name = species,
         species_code = Species_ID_core,
         species_id = naturecounts_species_id,
         period = trend_time,
         upper_ci = index_q_0.95,
         lower_ci = index_q_0.05) %>%
  select(-c(index_q_0.025,
            index_q_0.975,
            region_type,
            region)) %>%
  relocate(results_code,
           version,
           area_code,
           season,
           period,
           species_name,
           species_code,
           species_id,
           year,
           index,
           upper_ci,
           lower_ci,
           LOESS_index,
           smooth_index) %>%
  select(results_code,
         version,
         area_code,
         species_id,
         area_name,
         season,
         period,
         species_name,
         species_code,
         year,
         index,
         upper_ci,
         lower_ci,
         LOESS_index,
         smooth_index)

readr::write_excel_csv(indices_socb,
                       file = paste0(external_dir,"/website/BBS_",YYYY,"_annual_indices_for_socb.csv"))


# tmp <- indices_socb %>%
#   filter(species_name == "Killdeer")
# readr::write_excel_csv(tmp,
#                        file = paste0("website/BBS_",YYYY,"_annual_indices_for_Killdeer.csv"))
#





# SOCB extra indices ------------------------------------------------------

#
# indices_socb <- indices %>%
#   filter((for_web == FALSE & !(region %in% c("continent","United States of America")))) %>%
#   inner_join(.,smooth_join,
#              by = c("species",
#                     "region",
#                     "region_type",
#                     "trend_time",
#                     "year")) %>%
#   group_by(species,region,region_type,trend_time) %>%
#   mutate(LOESS_index = loess_func(index,year),
#          results_code = "BBS",
#          season = "breeding",
#          version = YYYY,
#          area_code = ifelse(region == "continent","Continental",region),
#          area_code = gsub(area_code,pattern = "United States of America",
#                           replacement = "USA"),
#          area_code = ifelse(region_type == "bcr",paste0("BCR_",region),area_code),
#          trend_time = as.character(trend_time),
#          trend_time = ifelse(trend_time == "Three-generation","3Gen-Recent",trend_time)) %>%
#   ungroup() %>%
#   left_join(.,socb_areas, by = "area_code") %>%
#   #select(-area_code) %>%
#   rename(species_name = species,
#          species_code = Species_ID_core,
#          species_id = naturecounts_species_id,
#          period = trend_time,
#          upper_ci = index_q_0.95,
#          lower_ci = index_q_0.05) %>%
#   select(-c(index_q_0.025,
#             index_q_0.975,
#             region_type,
#             region)) %>%
#   relocate(results_code,
#            version,
#            area_code,
#            season,
#            period,
#            species_name,
#            species_code,
#            species_id,
#            year,
#            index,
#            upper_ci,
#            lower_ci,
#            LOESS_index,
#            smooth_index) %>%
#   select(results_code,
#          version,
#          area_code,
#          area_name,
#          season,
#          period,
#          species_name,
#          species_code,
#          species_id,
#          year,
#          index,
#          upper_ci,
#          lower_ci,
#          LOESS_index,
#          smooth_index)
#
# readr::write_excel_csv(indices_socb,
#                        file = paste0(external_dir,"/website/BBS_",YYYY,"_extra_annual_indices_for_socb.csv"))
#
#
#
#



