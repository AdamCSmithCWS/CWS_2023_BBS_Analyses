## calculating BBS coverage by degree block

# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# remotes::install_github("AdamCSmithCWS/SurveyCoverage")


library(SurveyCoverage)
library(bbsBayes2)
library(tidyverse)
library(ebirdst)
ebirdst::set_ebirdst_access_key("t9el4omae1c3",overwrite = TRUE)

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)


for(sp_sel in sp_list$english){




}



