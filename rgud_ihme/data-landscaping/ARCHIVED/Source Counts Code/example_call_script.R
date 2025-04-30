rm(list=ls())
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
} else {
  j <- "/home/j/"
  user <- Sys.info()[["user"]]
  h <- paste0("/ihme/homes/", user, "/")
  l <- "/ihme/limited_use/"
}

source("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Code/Source Counts Code/source_counts_master.R")

############################################
# CROSSWALK VERSION AND BUNDLE VERSION ID
############################################
#(1) Most common Location Data sourced in:
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7) #Make sure you use the location_data object name
source_counts(cw_id = 31994, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_id/")
source_counts(cw_id = 19694, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_id/")
source_counts(bv_id = 20930, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_id/")


############################################
# CUSTOM DATA TABLE INPUT
############################################
#(2) Custom Location Data sourced in:
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2) #Make sure you use the location_data object name

dt <- read.xlsx("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Testing/testing_dt.xlsx") %>% select(-'location_name.x', -'location_name.y')

source_counts(custom_data_table = dt, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_custom/")

source_counts(custom_data_table = dt, custom_subset_column_names = 'All_data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_custom/")

source_counts(custom_data_table = dt, custom_subset_column_names = 'Vital_Reg', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/testing_custom/")
