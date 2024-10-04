## Comparing Canadian trajectories between teh standard stratification
## and a latlong stratification
##

library(tidyverse)

inds_standard <- read_csv("website/BBS_Full_Indices_continent_country_2022.csv") %>%
  filter(region == "Canada",
         trend_time == "Long-term") %>%
  select(species, region, year, index, index_q_0.05, index_q_0.95,
         n_routes_total) %>%
  mutate(stratification = "bbs_cws")

inds_latlong <- read_csv("data/lat_long_indices_composite_canada.csv") %>%
  select(species, region, year, index, index_q_0.05, index_q_0.95,
         n_routes_total) %>%
  mutate(stratification = "latlong")


inds_all <- bind_rows(inds_standard,
                      inds_latlong)

inds_fy <- inds_all %>%
  filter(year == 2022) %>%
  group_by(species) %>%
  summarise(n_mods = n())




species_sel <- c("Ovenbird",
                 "Red-eyed Vireo",
                 "Hermit Thrush",
                 "Veery",
                 "Canada Warbler",
                 "Black-capped Chickadee")

species_sel <- c("Blackpoll Warbler",
                 "Connecticut Warbler",
                 "Hermit Thrush",
                 "Tennessee Warbler",
                 "Canada Warbler",
                 "Cape May Warbler")

species_sel <- c("Chimney Swift",
                 "Tree Swallow",
                 "Red-winged Blackbird",
                 "Pied-billed Grebe",
                 "Green Heron",
                 "Bobolink")


inds_sel <- inds_all %>%
  filter(species %in% species_sel,
         year > 1969)


txt <- ggplot(data = inds_sel,
              aes(x = year, y = index,
                  colour = stratification,
                  fill = stratification))+
  geom_ribbon(aes(ymin = index_q_0.05,
                  ymax = index_q_0.95),
              alpha = 0.3)+
  geom_line()+
  scale_y_continuous(transform = "log10")+
  facet_wrap(vars(species))


txt


