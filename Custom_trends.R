### Custom trend period
library(tidyverse)
library(bbsBayes2)

trend_first_year <- 1978
trend_last_year <- 2022


species_list <- readRDS("data/prairie_sp_table.rds")
trends_out <- NULL
for(j in 1:nrow(species_list)){

  species <- species_list[j,"BirdName"]
  aou <- as.integer(search_species(species)[1,"aou"])

  inds <- readRDS(paste0("indices/Inds_",aou,".rds")) #n_smooth


  trends <- generate_trends(inds,
                            min_year = trend_first_year,
                            max_year = trend_last_year,
                            hpdi = TRUE,
                            prob_decrease = c(0,30,50))

  tmp <- trends$trends %>%
    mutate(BirdName = species)
  trends_out <- bind_rows(trends_out,tmp)

}

write_csv(trends_out,paste0("extra_output/prairie_sp_trends",
                            trend_first_year,"_",trend_last_year,".csv"))
