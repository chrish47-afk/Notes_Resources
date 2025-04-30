rm(list=ls())

# Map drives
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j/'
  h <- '/homes/helenipp/'
} else {
  j <- 'J:/'
  h <- 'H:/'
}

# Load packages
library(data.table)
library(dplyr)
library(tidyr)

# GET ARGS AND ITEMS
args<-commandArgs(trailingOnly = TRUE)
map_path <-args[1]
out_path <-args[2]
gbd_rnd <- args[3]
gbd_decomp_step <- args[4]
locations <- fread(map_path)
task_id <- Sys.getenv("SGE_TASK_ID")
loc_id <- locations[task_num == task_id, location_id]

# OBJECTS
pud_gd <- data.frame(cause_id = c("527", "528"), 
                     cause_name_full = c("peptic ulcer disease (PUD)", "gastritis and duodenitis (G/D)"))
year_ids <-c(1990:2019)

# SOURCE ---------------------------------------------------
source("/ihme/cc_resources/libraries/current/r/get_draws.R")

draw_cols <- paste0("draw_",0:999)
prev_draws <- data.frame()
for (cause in pud_gd$cause_id) {
  prev_draws <- rbind(prev_draws, 
                      get_draws(
                        gbd_id_type='cause_id',
                        gbd_id=cause,
                        source='como',
                        age_group_id = 27,
                        measure_id=5, #prev
                        sex_id=3,
                        location_id = loc_id,
                        gbd_round_id=gbd_rnd,
                        decomp_step=gbd_decomp_step,
                        status = "best",
                        year_id = year_ids[length(year_ids)]) %>%
                        pivot_longer(all_of(draw_cols), names_to = "draw_num", names_prefix = "draw_", values_to = "val"))
}
agg_prev <- prev_draws %>% pivot_wider(names_from = "cause_id", values_from = "val", names_prefix = "val_") %>%
  mutate(agg_prev = val_527+val_528) %>%
  group_by(location_id) %>%
  mutate(avg_agg = mean(agg_prev))

write.csv(agg_prev, paste0(out_path, "prev_draws/", loc_id, ".csv"))