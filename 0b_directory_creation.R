### setting up folders that hold saved output not tracked by Git


dirs <- c("trends","data","indices","output","convergence","figures","raw_data","website")

for(d in dirs){
  if(!dir.exists(d)){
    dir.create(d)
  }else{
    print(d)
  }
}

dirs_sub <- c("figures/diagnostic_trajectories",
              "figures/trajectories",
              "figures/temp_rds_storage",
              "trends/rolling_trend_maps",
              "trends/rolling_trend_graphs",
              "trends/rolling_trends")


for(d in dirs_sub){
  if(!dir.exists(d)){
    dir.create(d)
  }else{
    print(d)
  }
}

