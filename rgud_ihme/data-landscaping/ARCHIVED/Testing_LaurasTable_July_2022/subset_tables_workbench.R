# Testing Laura's Table / For Subset Tables
###########################################
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
library(data.table)
library(openxlsx)
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(reshape2)

user <- Sys.getenv("USER")
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
# Location Data
gbd_rnd <- 7
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


# Data table input, for testing purposes, creating different subsets.
dt_original_test <- read.xlsx("/homes/chrish47/Draft_May162022_Mat_Land_US_MMWRAbortionOnly_forChristian - Copy.xlsx")
# Testing only columns cv_onlyelectiveab, cv_onlyelectiveab, and onlyelectiveab 
# -- These columns don't have overlapping values, so tetsted one at a time

###### Custom data input(subset data) - Laura's test table - RGUD vetting meeting ###### FOR MATERNAL MORBIDITY SUBSETS ONLY 
### DATA PREP ##############################################################################################################
Laura_dt_test <- read.xlsx("/ihme/homes/chrish47/Draft_May172022_Mat_Land_US_MMWRAbortionOnly_forChristian - Copy.xlsx")
Laura_dt_test_modified <- Laura_dt_test %>% select(nid, location_id, year_start, year_end, age_start, age_end, 206:229)
abortion_dt <- get_crosswalk_version(20465) %>% select(nid, location_id, year_start, year_end, age_start, age_end)
#final_dt <- left_join(Laura_dt_test_modified, abortion_dt, by = c('nid', 'location_id', 'year_start', 'year_end'))
final_dt <- left_join(Laura_dt_test_modified, abortion_dt, by = 'nid') %>% select(1:30) %>% dplyr::rename(location_id = location_id.x, year_start = year_start.x, year_end = year_end.x, age_start = age_start.x, age_end = age_end.x)
# column list
a <- colnames(final_dt)
column_dt <- data.table(a)

# GENERATING SOURCE COUNTS
column <- as.character(column_dt[28]) #7, 8, 9, 12, 14, 21, 25, 27, 28
out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/20465_Maternal_abortive_outcome/'
if(file.exists(paste0(out_path, paste0(column)))){
  print('folder already exists')
} else {
  dir.create(paste0(out_path, paste0(column)))
  print('new folder created')
}

source_counts(custom_data_table = final_dt, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/20465_Maternal_abortive_outcome/",column, "/"))

# Combine!!!!!!!!!!!!! I NEED TO AUTOMATE THIS PART!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# exist_path <- paste0(out_path, column, "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
# 
# if(!file.exists(paste0(out_path, column, "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"))){
#   a <- 'region_id'
#   b <- 'location_name'
#   c <- 'n_adj'
#   d <- 'hierarchy_key'
#   dt <- data.table(a, b, c, d)
# } else {
#   dt <- read.xlsx(paste0(out_path, column, "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 4)
#   column <- as.character(column_dt[7])
#   dt <- dt %>% rename(column = n_adj)
# }
# following tables have source counts
dt1 <- read.xlsx(paste0(out_path, as.character(column_dt[7]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 4) 
dt1 <- dt1 %>% dplyr::rename("All.ages.or.10-54yo" = n_adj)
dt1[] <- lapply(dt1, as.character) # This can be integrated in the source_counts functions. REVIEW! As a character or string. REVIEWEEEEEW
dt2 <- read.xlsx(paste0(out_path, as.character(column_dt[12]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 4)
dt2 <- dt2 %>% dplyr::rename("Comprehensive.subnationals" = n_adj)
dt2[] <- lapply(dt2, as.character)
dt3 <- read.xlsx(paste0(out_path, as.character(column_dt[14]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 4)
dt3 <- dt3 %>% dplyr::rename("Live.Birth" = n_adj)
dt3[] <- lapply(dt3, as.character)
# following tables don't have source counts
dt4 <- read.xlsx(paste0(out_path, as.character(column_dt[8]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt4 <- dt4 %>% dplyr::rename("15-49yo" = n_adj)
dt4[] <- lapply(dt4, as.character)
dt5 <- read.xlsx(paste0(out_path, as.character(column_dt[9]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt5 <- dt5 %>% dplyr::rename("Narrower.age-group" = n_adj)
dt5[] <- lapply(dt5, as.character)
dt6 <- read.xlsx(paste0(out_path, as.character(column_dt[21]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt6 <- dt6 %>% dplyr::rename("Age" = n_adj)
dt6[] <- lapply(dt6, as.character)
dt7 <- read.xlsx(paste0(out_path, as.character(column_dt[25]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt7 <- dt7 %>% dplyr::rename("Community.only" = n_adj)
dt7[] <- lapply(dt7, as.character)
dt8 <- read.xlsx(paste0(out_path, as.character(column_dt[27]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt8 <- dt8 %>% dplyr::rename("Admin.data" = n_adj)
dt8[] <- lapply(dt8, as.character)
dt9 <- read.xlsx(paste0(out_path, as.character(column_dt[28]), "/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 1)
dt9 <- dt9 %>% dplyr::rename("Self-report" = n_adj)
dt9[] <- lapply(dt9, as.character)
# Left_joining the tables -- IMPORTANT: THIS CAN PRESENT ISSUES. REVIEW THE WHAT OTHER JOINS or AGGREGATES BETTER FIR THIS!
subset_1 <- left_join(dt1, dt2, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_2 <- left_join(subset_1, dt3, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_3 <- left_join(subset_2, dt4, by = c('region_id', 'location_name', 'hierarchy_key')) 
subset_4 <- left_join(subset_3, dt5, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_5 <- left_join(subset_4, dt6, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_6 <- left_join(subset_5, dt7, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_7 <- left_join(subset_6, dt8, by = c('region_id', 'location_name', 'hierarchy_key'))
subset_final <- left_join(subset_7, dt9, by = c('region_id', 'location_name', 'hierarchy_key')) %>% select(1, 2, 4, 3,5:11)

write.xlsx(subset_final, "/ihme/scratch/users/chrish47/source_counts_mb/20465_Maternal_abortive_outcome/subset_final_example.xlsx")

#=========================================================================================================================================
#=========================================================================================================================================
dt_Laura_testingonly <- read.xlsx("/ihme/homes/chrish47/source_counts_utility/Testing_LaurasTable_July_2022/DRAFT_landscape_ForTestingONLYJune302022.xlsx")
#print(unique(dt_Laura_testingonly$bundle_bv))
#duplicated(colnames(dt_Laura_testingonly))
eclampsia_subset_columns <- dt_Laura_testingonly %>% select(1:15)
eclampsia_subset_columns <- eclampsia_subset_columns %>% filter(bundle_bv %in% "eclampsia_bv_20936")

## Pulling in crosswalk_version_id data
dt_eclampsia <- get_crosswalk_version(20033)
dt_eclampsia_edit <- dt_eclampsia %>% select(nid, location_id, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "")
## Joning eclampsia tables
dt_eclampsia_join <- left_join(dt_eclampsia_edit, eclampsia_subset_columns, by = "nid")









