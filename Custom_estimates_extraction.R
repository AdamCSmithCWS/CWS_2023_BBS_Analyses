
## custom data extraction for a selected species (or list of species)
##
##
#
#

species_sel <- c("Great Horned Owl")

outname <- "grhowl"# must supply meaningful file name suffix for output files


YYYY <- 2023

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


i_plot <- i %>%
  filter(region == "Canada",
         trend_time == "Long-term",
         year > 1994)
tmpnat <- ggplot(data = i_plot,
                 aes(x = year,y = index))+
  geom_ribbon(aes(ymin = index_q_0.05,
                  ymax = index_q_0.95),
              alpha = 0.3)+
  geom_line()+
  scale_y_continuous(limits = c(0,NA))+
  ylab("BBS Annual Index")+
  xlab("")+
  theme_bw()

png(paste0("temp/custom_output/tmp_grhowl_traj_bbs.png"),
    res = 300, height = 5, width = 8,
    units = "in")
print(tmpnat)
dev.off()



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
