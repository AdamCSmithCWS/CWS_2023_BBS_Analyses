


###  - State of Canada's Birds
YYYY <- 2023

webmaps <- FALSE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(foreach)
library(doParallel)
#setwd("C:/GitHub/CWS_2023_BBS_Analyses")

external_dir <- "F:/CWS_2023_BBS_Analyses"


sp_list <- readRDS("sp_list_w_generations.rds") %>%
  filter(model == TRUE)
#
# three_gens <- read_csv("data/full_bbs_species_list_w_generation_length.csv")
#
# three_gens <- three_gens %>%
#   select(aou,GenLength)
#
# sp_list <- sp_list %>%
#   inner_join(.,three_gens,
#              by = c("aou"))
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
#   mutate(species_id = ifelse(species_id2 != species_id,species_id2,species_id)) %>%
#   select(species_id,species_code) %>%
#   mutate(aou = as.integer(species_code),
#          naturecounts_species_id = species_id) %>%
#   distinct() %>%
#   select(-c(species_id,species_code))
#
# sp_list <- sp_list %>%
#   left_join(.,naturecounts_codes,
#             by = "aou") %>%
#   arrange(Sort_Order_core)
#
# # Compile all trends and indices ------------------------------------------------------


# Compare to last year's trends -------------------------------------------


lastyear = read_csv("data/All_BBS_trends_2022.csv")

ly_trends <- lastyear[,c("species","bbs_num","region","trend_time",
                         "n_strata_included","n_routes",
                         "trend",
                         "trend_q_0.05","trend_q_0.95",
                         "width_of_95_percent_credible_interval")] %>%
  filter(region %in% c("continent","Canada","United States of America")) %>%
  select(-c(species))



species_to_run <- sp_list %>%
  arrange(naturecounts_sort_order)

pdf(file = paste0("Figures/BBS_High_level_summary_",YYYY,".pdf"),
    height = 9,
    width = 17)

for(jj in (1:nrow(species_to_run))){

  species <- as.character(species_to_run[jj,"english"])
  espece <- as.character(species_to_run[jj,"french"])
  aou <- as.integer(species_to_run[jj,"aou"])
  if(file.exists(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_maps.RDS"))){
  species_f_bil <- gsub(paste(species,espece),pattern = "[[:space:]]|[[:punct:]]",
                        replacement = "_")

  trends_ly <- ly_trends %>%
    filter(bbs_num == aou) %>%
    mutate(version = "Last year")

  if(nrow(trends_ly) > 0){
  trends_1 <- readRDS(paste0(external_dir,"/Trends/",aou,"_trends.rds")) %>%
    filter(region %in% c("continent","Canada","United States of America")) %>%
    mutate(aou = aou,
           version = "This year") %>%
    bind_rows(.,trends_ly)
  }else{
    trends_1 <- readRDS(paste0(external_dir,"/Trends/",aou,"_trends.rds")) %>%
      filter(region %in% c("continent","Canada","United States of America")) %>%
      mutate(aou = aou,
             version = "This year")
}
tplot <- ggplot(data = trends_1)+
  geom_pointrange(aes(x = region,y = trend,ymin = trend_q_0.05,ymax = trend_q_0.95,
                      colour = version),
                  position = position_dodge(width = 0.5))+
  facet_wrap(vars(trend_time),
             scales = "free_x")+
  scale_colour_viridis_d(direction = -1,
                         name = paste("Trends",YYYY,"\n  and",YYYY-1))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  theme_bw()


  tmaps <- readRDS(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_maps.RDS"))

  trajs <- readRDS(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_highlevel_simple_trajs.RDS"))

  design <- "
135
246
"

  if(is.null(trajs[[2]])){
    layt <- trajs[[1]] + trajs[[3]] + plot_spacer() +
      tplot + tmaps[[1]] + tmaps[[2]] +
      plot_layout(design = design,
                  guides = "collect",
                  widths = 1)
  }else{

    layt <- trajs[[1]] + trajs[[3]] + trajs[[2]] +
      tplot + tmaps[[1]] + tmaps[[2]] +
      plot_layout(design = design,
                  guides = "collect",
                  widths = 1)
}
  print(layt)

  print(species)
  }else{
  print(paste("No estimates for",species,aou))
}
}

dev.off()




# Plotting trend maps -----------------------------------------------------



start_years <- c("Long-term","Short-term","Three-generation")



n_cores <- 12
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(jj = rev(c(1:nrow(species_to_run))),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr",
                              "patchwork"),
                .errorhandling = "pass") %dopar%
  {

  species <- as.character(species_to_run[jj,"english"])
  espece <- as.character(species_to_run[jj,"french"])
  aou <- as.integer(species_to_run[jj,"aou"])

  if(file.exists(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_maps.RDS"))){
    species_f_bil <- gsub(paste(species,espece),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")

    tmaps <- readRDS(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_maps.RDS"))
    qmaps <- readRDS(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_quart_maps.RDS"))

    pdf(paste0("Figures/trend_maps/",species_f_bil,"_trend_maps.pdf"),
        width = 11,
        height = 8.5)


    for(j in (start_years)){
      tt <- tmaps[[j]]+
        labs(title = paste(j,":",espece,"/",species))

    print(tt)

    tt <- qmaps[[j]]+
      plot_annotation(title = paste(j,":",espece,"/",species))

    print(tt)


      }
  dev.off()


    }


  }
parallel::stopCluster(cluster)

