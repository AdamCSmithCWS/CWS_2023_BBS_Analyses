


###  - State of Canada's Birds
YYYY <- 2022

webmaps <- FALSE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(ggrepel)
setwd("C:/GitHub/CWS_2022_BBS_Analyses")


source("functions/mapping.R")
source("functions/loess_func.R")
# custom functions to calculate reliability categories and determine website inclusion


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

three_gens <- read_csv("data/full_bbs_species_list_w_generation_length.csv")

three_gens <- three_gens %>%
  select(aou,GenLength)

sp_list <- sp_list %>%
  inner_join(.,three_gens,
             by = c("aou"))

avian_core <- read_csv("data/ECCC Avian Core 20230601.csv") %>%
  rename_with(.,.fn = ~paste0(.x,"_core")) %>%
  mutate(aou = as.integer(BBS_Number_core))

rep_aou <- avian_core %>% group_by(aou) %>% summarise(n = n()) %>% filter(n > 1, !is.na(aou))
rep_core <- avian_core %>%
  filter(aou %in% rep_aou$aou)

avian_core <- avian_core %>%
  filter(!(aou %in% rep_aou$aou & Full_Species_core == "No"))

sp_list <- sp_list %>%
  inner_join(.,avian_core,by = "aou")

nature_counts_codes <- naturecounts::meta_species_codes() %>%
  filter(authority == "BBS2") %>%
  select(species_id2,species_code) %>%
  mutate(aou = as.integer(species_code),
         nature_counts_species_id = species_id2) %>%
  distinct() %>%
  select(-c(species_id2,species_code))

sp_list <- sp_list %>%
  left_join(.,nature_counts_codes,
            by = "aou")

tmp2 <- sp_list %>% filter(is.na(nature_counts_species_id)) %>%
  ungroup() %>%
  select(aou,english,BBS_Number_core,nature_counts_species_id)
if(nrow(tmp2) > 0){
  stop("Species don't match with nature counts")
}



re_collect <- FALSE
# Compile all trends and indices ------------------------------------------------------

if(re_collect){
trends <- NULL
indices <- NULL
indices_smooth <- NULL

for(i in 1:nrow(sp_list)){


  aou <- as.integer(sp_list[i,"aou"])

  if(file.exists(paste0("Indices/list_",aou,"_indices.rds"))){
  inds_1 <- readRDS(paste0("Indices/list_",aou,"_indices.rds"))

  trends_1 <- readRDS(paste0("Trends/",aou,"_trends.rds"))

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

core_link <- sp_list %>%
  ungroup() %>%
  select(Sort_Order_core,Species_ID_core,aou,nature_counts_species_id)

lastyear = read_csv("data/All_2021_BBS_trends.csv")
ly_trends_3g <- read_csv("data/All_2021_BBS_short-term_3_generation_trends.csv") %>%
  select(species,bbs_num,Region,Region_alt,Trend_Time,Trend,Trend_Q0.05,Trend_Q0.95) %>%
  mutate(Trend_Time = "Three-generation")

ly_trends <- lastyear[,c("species","bbs_num","Region","Region_alt","Trend_Time",
                         "Number_of_strata","Number_of_Routes",
                         "Trend",
                         "Trend_Q0.05","Trend_Q0.95",
                         "Width_of_95_percent_Credible_Interval")] %>%
  bind_rows(ly_trends_3g) %>%
  rename(trend_2021 = Trend,
         trend_q_0.05_2021 = Trend_Q0.05,
         trend_q_0.95_2021 = Trend_Q0.95,
         trend_time = Trend_Time,
         Number_of_strata_2021 = Number_of_strata,
         Number_of_Routes_2021 = Number_of_Routes,
         CI_2021 = Width_of_95_percent_Credible_Interval) %>%
  mutate(Region = ifelse(Region == "Continental","continent",Region),
         Region = ifelse(Region_alt == "Canada","Canada",Region),
         Region = ifelse(Region == "US","United States of America",Region)) %>%
  filter(Region %in% c("continent","Canada","United States of America")) %>%
  rename(region = Region) %>%
  select(-c(species,Region_alt))



trends_comp <- trends %>%
  inner_join(.,ly_trends,
             by = c("bbs_num",
                    "region",
                    "trend_time")) %>%
  left_join(.,core_link,by = c("bbs_num" = "aou")) %>%
  mutate(diff_trend = trend - trend_2021) %>%
  rename(CI = width_of_95_percent_credible_interval)


comp_xy <- ggplot(data = trends_comp,
                  aes(x = trend_2021,
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



trends_comp_sel <- trends_comp %>%
  filter(diff_trend > 1 | diff_trend < -1,
         region_type == "continent")


comp_xy_sel <- ggplot(data = trends_comp_sel,
                  aes(x = trend_2021,
                      y = trend,
                      colour = factor(bbs_num)))+
  geom_point()+
  geom_errorbar(aes(ymin = trend_q_0.05, ymax = trend_q_0.95),
                alpha = 0.4)+
  geom_errorbarh(aes(xmin = trend_q_0.05_2021, xmax = trend_q_0.95_2021),
                 alpha = 0.4)+
  geom_abline(intercept = 0,slope = 1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_text_repel(aes(label = factor(Species_ID_core)))+
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
  arrange(Sort_Order_core,region_type,region,start_year)


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
    arrange(Sort_Order_core,region_type,region,trend_time,year)

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
  arrange(Sort_Order_core,region_type,region,trend_time,year)



# csv files with trends and indices for Google Drive ----------------------

saveRDS(indices,paste0("Website/All_BBS_Full_Indices_",YYYY,".rds"))
saveRDS(indices_smooth,paste0("Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))
saveRDS(trends,paste0("Website/All_BBS_Trends_",YYYY,".rds"))


write_csv(indices,paste0("Website/All_BBS_Full_Indices_",YYYY,".csv"))
write_csv(indices_smooth,paste0("Website/All_BBS_Smoothed_Indices_",YYYY,".csv"))
write_csv(trends,paste0("Website/All_BBS_Trends_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("continent","country"))
write_csv(inds_select,paste0("Website/BBS_Full_Indices_continent_country_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("prov_state"))
write_csv(inds_select,paste0("Website/BBS_Full_Indices_prov_state_",YYYY,".csv"))


inds_select <- indices %>%
  filter(region_type %in% c("bcr"))
write_csv(inds_select,paste0("Website/BBS_Full_Indices_bcr_",YYYY,".csv"))

inds_select <- indices %>%
  filter(region_type %in% c("bcr_by_country"))
write_csv(inds_select,paste0("Website/BBS_Full_Indices_bcr_by_country_",YYYY,".csv"))


inds_select <- indices_smooth %>%
  filter(region_type %in% c("continent","country"))
write_csv(inds_select,paste0("Website/BBS_Smoothed_Indices_continent_country_",YYYY,".csv"))

inds_select <- indices_smooth %>%
  filter(region_type %in% c("prov_state"))
write_csv(inds_select,paste0("Website/BBS_Smoothed_Indices_prov_state_",YYYY,".csv"))


inds_select <- indices_smooth %>%
  filter(region_type %in% c("bcr"))
write_csv(inds_select,paste0("Website/BBS_Smoothed_Indices_bcr_",YYYY,".csv"))

inds_select <- indices_smooth %>%
  filter(region_type %in% c("bcr_by_country"))
write_csv(inds_select,paste0("Website/BBS_Smoothed_Indices_bcr_by_country_",YYYY,".csv"))




trends_select <- trends %>%
  filter(region_type %in% c("continent","country","prov_state"))
write_csv(trends_select,paste0("Website/BBS_Trends_continent_country_prov_state_",YYYY,".csv"))
trends_select <- trends %>%
  filter(region_type %in% c("bcr","bcr_by_country"))
write_csv(trends_select,paste0("Website/BBS_Trends_bcr_bcr_by_country_",YYYY,".csv"))
trends_select <- trends %>%
  filter(region_type %in% c("stratum"))
write_csv(trends_select,paste0("Website/BBS_Trends_strata_",YYYY,".csv"))




# SOCB upload files -------------------------------------------------------


socb_areas <- read_csv("data/SOCB_regions.csv") %>%
  filter(results_code == "BBS") %>%
  select(area_code,area_name)


trends_out <- trends %>%
  filter((for_web == TRUE | region %in% c("continent","United States of America")))

trends_out2 <- trends_out  %>%
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
         species_id = nature_counts_species_id,
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
                       paste0("website/BBS_",YYYY,"_trends_for_socb.csv"))

# tmp <- trends_socb %>%
#   filter(species_name == "Killdeer")
# readr::write_excel_csv(tmp,
#                        paste0("website/BBS_",YYYY,"_trends_for_Killdeer.csv"))
#


# SOCB extra trends -------------------------------------------------------

trends_out <- trends %>%
  filter((for_web == FALSE & !(region %in% c("continent","United States of America"))))

trends_out2 <- trends_out  %>%
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
         species_id = nature_counts_species_id,
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
                       paste0("website/BBS_",YYYY,"_extra_trends_for_socb.csv"))





# SOCB indices ------------------------------------------------------------


smooth_join <- indices_smooth %>%
  select(species,region,region_type,trend_time,
         year,index) %>%
  rename(smooth_index = index)

indices_socb <- indices %>%
  filter((for_web == TRUE | region %in% c("continent","United States of America"))) %>%
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
         species_id = nature_counts_species_id,
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
         area_name,
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
         smooth_index)

readr::write_excel_csv(indices_socb,
                       file = paste0("website/BBS_",YYYY,"_annual_indices_for_socb.csv"))


# tmp <- indices_socb %>%
#   filter(species_name == "Killdeer")
# readr::write_excel_csv(tmp,
#                        file = paste0("website/BBS_",YYYY,"_annual_indices_for_Killdeer.csv"))
#





# SOCB extra indices ------------------------------------------------------


indices_socb <- indices %>%
  filter((for_web == FALSE & !(region %in% c("continent","United States of America")))) %>%
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
         species_id = nature_counts_species_id,
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
         area_name,
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
         smooth_index)

readr::write_excel_csv(indices_socb,
                       file = paste0("website/BBS_",YYYY,"_extra_annual_indices_for_socb.csv"))







