
## custom data extraction for a selected species (or list of species)
##
##
#
#

species_sel <- c("Grasshopper Sparrow")

outname <- "GRSP"# must supply meaningful file name suffix for output files


YYYY <- 2022

library(bbsBayes2)
library(tidyverse)


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

trends <- readRDS(paste0("Website/All_BBS_Trends_",YYYY,".rds"))
indices <- readRDS(paste0("Website/All_BBS_Full_Indices_",YYYY,".rds"))
indices_smooth <- readRDS(paste0("Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))

tr <- NULL
i <- NULL
ism <- NULL

for(sp_sel in species_sel){
tr <- trends %>%
  filter(species == sp_sel) %>%
  bind_rows(.,tr)

i <- indices %>%
  filter(species == sp_sel) %>%
  bind_rows(.,i)
ism <- indices_smooth %>%
  filter(species == sp_sel) %>%
  bind_rows(.,ism)

}

write_excel_csv(tr,
                paste0("temp/custom_output/trends_",outname,".csv"))
write_excel_csv(i,
                paste0("temp/custom_output/annual_indices_",outname,".csv"))
write_excel_csv(ism,
                paste0("temp/custom_output/smoothed_annual_indices_",outname,".csv"))






# Regional ----------------------------------------------------------------




reg_sel <- c("NB")

outname <- reg_sel# must supply meaningful file name suffix for output files


YYYY <- 2022

library(bbsBayes2)
library(tidyverse)


trends <- readRDS(paste0("Website/All_BBS_Trends_",YYYY,".rds"))
indices <- readRDS(paste0("Website/All_BBS_Full_Indices_",YYYY,".rds"))
indices_smooth <- readRDS(paste0("Website/All_BBS_Smoothed_Indices_",YYYY,".rds"))

tr <- NULL
i <- NULL
ism <- NULL

for(reg in reg_sel){
  tr <- trends %>%
    filter(region == reg) %>%
    bind_rows(.,tr)

  i <- indices %>%
    filter(region == reg) %>%
    bind_rows(.,i)
  ism <- indices_smooth %>%
    filter(region == reg) %>%
    bind_rows(.,ism)

}

write_excel_csv(tr,
                paste0("temp/custom_output/trends_",outname,".csv"))
write_excel_csv(i,
                paste0("temp/custom_output/annual_indices_",outname,".csv"))
write_excel_csv(ism,
                paste0("temp/custom_output/smoothed_annual_indices_",outname,".csv"))
