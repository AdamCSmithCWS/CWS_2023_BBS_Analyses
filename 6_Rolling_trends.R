## Calculates all possible three-generation trends for every species
## graphs the trend maps, and estimated trends through time to understand
## how the three-generation trends are changing through time.
## Intended to provide some assessment of the acceleration of trends
## slowing declines, increasing declines, etc.
## also the dependency of a trend estimate on the end years

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)
library(patchwork)

YYYY <- 2023
short_time <- 12

#setwd("C:/Users/SmithAC/Documents/GitHub/CWS_2023_BBS_Analyses")
#setwd("C:/GitHub/CWS_2023_BBS_Analyses")


output_dir <- "F:/CWS_2023_BBS_Analyses/output"
external_dir <- "F:/CWS_2023_BBS_Analyses"


# custom functions to calculate reliability categories and determine website inclusion
source("functions/web_trends.R")
source("functions/reliability.R")



#output_dir <- "output"
n_cores = 6
re_run <- TRUE #set to TRUE to overwrite any previous output from this script


sp_list <- readRDS("sp_list_w_generations.rds") %>%
  filter(model == TRUE)


sp_rerun <- c("Northern Shrike","Willow Ptarmigan", "Herring Gull",
              "Common Loon",
              "American Pipit",
              "Redpoll (Common/Hoary)")
sp_list <- sp_list %>%
  filter(english %in% sp_rerun)


regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

# load previous trend data -----------------------------------------------------------

lastyear = read_csv("data/All_BBS_trends_2022.csv")




# CV_threshold <- function(m,ci,thresh = 100){
#   y <- ifelse(ci/m > thresh,TRUE,FALSE)
#   return(y)
# }
#



# reliability category definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100
names(prec_cuts) <- c("High","Medium")

cov_cuts = c(0.5,0.25)
names(cov_cuts) <- c("High","Medium")

pool_cuts = c(0.33,0.1)
names(pool_cuts) <- c("High","Medium")

backcast_cuts = c(0.90,0.75)
names(backcast_cuts) <- c("High","Medium")




# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(c(1:nrow(sp_list))),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr",
                              "patchwork"),
                .errorhandling = "pass") %dopar%
  {

    # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    esp <- as.character(sp_list[i,"french"])
    aou <- as.integer(sp_list[i,"aou"])
    species_f_bil <- gsub(paste(esp,sp),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")


    if(file.exists(paste0(external_dir,"/Indices/Inds_",aou,".rds")) &
       file.exists(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_highlevel_simple_trajs.RDS")) &
       (!file.exists(paste0(external_dir,"/Trends/Rolling_trends/",aou,"_rolling_trends.rds")) | re_run)){



      # identifying first years for selected species ----------------------------
      fy <- 1970
      if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
        fy <- 1978 #5 years after the split
      }
      if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
        fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
      }
      if(aou == 6121){ # CAve Swallow
        fy = 1985
      }

      ## set three generations
      ## unless < 10, then 10   or  unless > number of years available, then n-years
      gen3 <- min((YYYY-fy),max(10,round(as.numeric(sp_list[i,"GenLength"])*3)))


      inds <- readRDS(paste0(external_dir,"/Indices/Inds_",aou,".rds"))


# Rolling trend calculations by three-generations -------------------------


      starts <- c(seq(fy,(YYYY-gen3),by = 1))

      roll_trends_out <- NULL

      trajs <- readRDS(paste0(external_dir,"/Figures/temp_rds_storage/",aou,"_highlevel_simple_trajs.RDS"))


      pdf(paste0(external_dir,"/trends/rolling_trend_maps/",species_f_bil,"_rolling_trend_map.pdf"),
          height = 11,
          width = 8.5)

      for(dd in starts){
        trends_10temp <- generate_trends(inds,
                                         min_year = dd,
                                         max_year = dd+gen3,
                                         prob_decrease = c(0,25,30,50),
                                         prob_increase = c(0,33,100))

        roll_trends_out <- bind_rows(roll_trends_out,trends_10temp$trends)

        ends <- inds$indices %>%
          filter(region == "continent",
                 year %in% c(dd,(dd+gen3)))

        map_tmp <- plot_map(trends_10temp,
                                title = FALSE)

        traj1 <- trajs[["continent"]]+
          geom_errorbar(data = ends,
                    aes(x = year, ymin = 0,
                        ymax = index),
                    width = 0,
                    linewidth = 1,
                     colour = "black")+
          # geom_line(data = ends,
          #               aes(x = year, y = index),
          #               linewidth = 1)+
          theme(axis.title = element_text(size = 6))

        displ <- traj1 + map_tmp + plot_layout(design = "
                                               1111
                                               2222
                                               2222
                                               2222
                                               2222")
        print(displ)
      }
      dev.off()

roll_trends_out <- roll_trends_out%>%
  mutate(species = sp,
         espece = esp,
         bbs_num = aou) %>%
  mutate(across(where(is.double) & !contains("year") &
                  !starts_with("n_") & !starts_with("bbs_num"),~signif(.,3)))

saveRDS(roll_trends_out, file = paste0(external_dir,"/Trends/Rolling_trends/",aou,"_rolling_trends.rds"))

#write_csv(roll_trends_out,file = paste0("Trends/Rolling_trends/",aou,"_14-year_rolling_trends.csv"))
thresh30 = (0.7^(1/gen3)-1)*100
thresh50 = (0.5^(1/gen3)-1)*100

# plot rolling trend values against thresholds ----------------------------

pdf(paste0(external_dir,"/trends/rolling_trend_graphs/",species_f_bil,"_rolling_trends.pdf"),
    width = 11,
    height = 8.5)
regs <- roll_trends_out %>%
  select(region_type,region) %>%
  distinct() %>%
  mutate(region_type = factor(region_type,
                              ordered = TRUE,
                              levels = regs_to_estimate)) %>%
  arrange(region_type)


for(rr in regs$region){
  rttmp <- roll_trends_out %>%
    filter(region == rr)
  rollTrend <- rttmp %>%
    filter(end_year == YYYY) %>%
    select(trend,prob_decrease_30_percent,prob_decrease_50_percent)
  pth_30_labs <- paste0(round(rollTrend[,"prob_decrease_30_percent"],2)*100,
                        "% probability of 30% decrease")
  pth_50_labs <- paste0(round(rollTrend[,"prob_decrease_50_percent"],2)*100,
                        "% probability of 50% decrease")
  rtp <- ggplot(data = rttmp,
                aes(y = trend,x = end_year))+
    geom_errorbar(aes(ymin = trend_q_0.025,
                      ymax = trend_q_0.975),
                  width = 0,alpha = 0.2)+
    geom_errorbar(aes(ymin = trend_q_0.25,
                      ymax = trend_q_0.75),
                  width = 0,alpha = 0.6)+
    geom_point(aes(alpha = backcast_flag))+
    scale_alpha_continuous(range = c(0.1,1))+
    guides(alpha = "none")+
    geom_hline(yintercept = thresh30,colour = "darkorange")+
    geom_hline(yintercept = thresh50,colour = "darkred")+
    geom_hline(yintercept = 0)+
    labs(title = paste(sp,esp,"rolling",gen3,"year trends",rr),
         subtitle = paste("Based on trend in",YYYY,":",pth_30_labs,"and",pth_50_labs))+
    xlab(paste("Ending year of",gen3,"trend"))+
    ylab(paste(gen3,"year trends"))+
    theme_bw()

  print(rtp)


} #temp end loop

dev.off()



}


  }




parallel::stopCluster(cluster)



