


###  - State of Canada's Birds
YYYY <- 2022

webmaps <- FALSE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(foreach)
library(doParallel)
setwd("C:/GitHub/CWS_2022_BBS_Analyses")



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
  mutate(species_id = ifelse(species_id2 != species_id,species_id2,species_id)) %>%
  select(species_id,species_code) %>%
  mutate(aou = as.integer(species_code),
         nature_counts_species_id = species_id) %>%
  distinct() %>%
  select(-c(species_id,species_code))

sp_list <- sp_list %>%
  left_join(.,nature_counts_codes,
            by = "aou") %>%
  arrange(Sort_Order_core)

# Compile all trends and indices ------------------------------------------------------


# Compare to last year's trends -------------------------------------------



lastyear = read_csv("data/All_2021_BBS_trends.csv")
ly_trends_3g <- read_csv("data/All_2021_BBS_short-term_3_generation_trends.csv") %>%
  select(species,bbs_num,Region,Region_alt,Trend_Time,Trend,Trend_Q0.05,Trend_Q0.95) %>%
  mutate(Trend_Time = "Three-generation")



# All species high-level plots --------------------------------------------

lastyear = read_csv("data/All_2021_BBS_trends.csv")
ly_trends_3g <- read_csv("data/All_2021_BBS_short-term_3_generation_trends.csv") %>%
  select(species,bbs_num,Region,Region_alt,Trend_Time,Trend,Trend_Q0.05,Trend_Q0.95) %>%
  mutate(Trend_Time = "Three-generation")

ly_trends <- lastyear[,c("species","bbs_num","Region","Region_alt","Trend_Time","Trend",
                         "Trend_Q0.05","Trend_Q0.95")] %>%
  bind_rows(ly_trends_3g) %>%
  rename(trend = Trend,
         trend_q_0.05 = Trend_Q0.05,
         trend_q_0.95 = Trend_Q0.95,
         trend_time = Trend_Time) %>%
  mutate(Region = ifelse(Region == "Continental","continent",Region),
         Region = ifelse(Region_alt == "Canada","Canada",Region),
         Region = ifelse(Region == "US","United States of America",Region)) %>%
  filter(Region %in% c("continent","Canada","United States of America")) %>%
  rename(region = Region) %>%
  select(-c(species,Region_alt))



species_to_run <- sp_list

pdf(file = paste0("Figures/BBS_High_level_summary_",YYYY,".pdf"),
    height = 9,
    width = 17)

for(jj in (1:nrow(species_to_run))){

  species <- as.character(species_to_run[jj,"english"])
  espece <- as.character(species_to_run[jj,"french"])
  aou <- as.integer(species_to_run[jj,"aou"])
  if(file.exists(paste0("Figures/temp_rds_storage/",aou,"_maps.RDS"))){
  species_f_bil <- gsub(paste(species,espece),pattern = "[[:space:]]|[[:punct:]]",
                        replacement = "_")

  trends_ly <- ly_trends %>%
    filter(bbs_num == aou) %>%
    mutate(version = "Last year")

  if(nrow(trends_ly) > 0){
  trends_1 <- readRDS(paste0("Trends/",aou,"_trends.rds")) %>%
    filter(region %in% c("continent","Canada","United States of America")) %>%
    mutate(aou = aou,
           version = "This year") %>%
    bind_rows(.,trends_ly)
  }else{
    trends_1 <- readRDS(paste0("Trends/",aou,"_trends.rds")) %>%
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
  coord_flip()+
  geom_hline(yintercept = 0)+
  theme_bw()


  tmaps <- readRDS(paste0("Figures/temp_rds_storage/",aou,"_maps.RDS"))

  trajs <- readRDS(paste0("Figures/temp_rds_storage/",aou,"_highlevel_simple_trajs.RDS"))

  design <- "
135
246
"

  layt <- trajs[[1]] + trajs[[2]] + trajs[[3]] +
    tmaps[[1]] + tmaps[[2]] + tplot +
    plot_layout(design = design,
                guides = "collect")

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

  if(file.exists(paste0("Figures/temp_rds_storage/",aou,"_maps.RDS"))){
    species_f_bil <- gsub(paste(species,espece),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")

    tmaps <- readRDS(paste0("Figures/temp_rds_storage/",aou,"_maps.RDS"))
    qmaps <- readRDS(paste0("Figures/temp_rds_storage/",aou,"_quart_maps.RDS"))

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

