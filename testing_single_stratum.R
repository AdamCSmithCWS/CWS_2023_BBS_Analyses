# testing single stratum model

sp <- "White-crowned Pigeon"

d <- stratify("White-crowned Pigeon",
              by = "bbs_usgs") %>%
  prepare_data() %>%
  prepare_model(model = "gamye")

