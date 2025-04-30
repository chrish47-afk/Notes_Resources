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
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
## SOURCE COUNTS BY GBD REGION and COUNTRY
a <- c(19694, 20036, 20552, 20792, 20033, 20900, 29777, 17186, 20465, 20810, 26540, 25775, 27353)
ids <- data.table(a)
id_unique <- as.character(ids[10]) #skipped 11, 12, and 13 #Fistula: 17186 has no clinical_data_type column ( no clinical data)
print(id_unique)

# if(file.exists(paste0(out_path, paste0(column)))){
#   print('folder already exists')
# } else {
#   dir.create(paste0(out_path, paste0(column)))
#   print('new folder created')
# }

# Total
out_path_total <- '/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/'
#dir.create(paste0(out_path_total, paste0(id_unique)))
source_counts(cw_id = id_unique, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/",id_unique, "/"), strategy_detail = FALSE)

# Clinical
out_path_clinical <- '/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/'
#dir.create(paste0(out_path_clinical, paste0(id_unique)))
source_counts(cw_id = id_unique, clinical_data_only = TRUE, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/",id_unique, "/"))

# Nonclinical
out_path_nonclinical <- '/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/'
#dir.create(paste0(out_path_nonclinical, paste0(id_unique)))
source_counts(cw_id = id_unique, nonclinical_data_only = TRUE, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/",id_unique, "/"))
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Fistula #
dt <- get_crosswalk_version(17186)
dt <- dt %>% filter(measure %in% 'incidence')
source_counts(custom_data_table = dt, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/",id_unique, "/"))

# Questions / Notes
# • Clinical_data_type(some sources have the following): 'literature' ~ this has been counted under the clinical data input category
# non-clinical data has been filter as NULL values in the clinical_data_type column for each data_input.
# • ID: 17186 ~ This source has both Incident and Prevalent Data.~ This data input doesn't have a clinical_data_type column. ~Hence these were treated as non-clinical data! And only incident data was inputted for the requested table.
# • IDs: 26540, 25775, and 27353 ~ Skipped these three. Ask Mae if these will be included or not? These three data inputs are unique, discuss with Mae!

# Reading files for Total Counts
### Total
country_19694_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/19694/source_counts/final_counts/subset_counts_cw_version_19694_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Hemorrhage" = "n_adj")

country_20036_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20036/source_counts/final_counts/subset_counts_cw_version_20036_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Puerperal Sepsis" = "n_adj")

country_20552_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20552/source_counts/final_counts/subset_counts_cw_version_20552_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Other Maternal Infections" = "n_adj")

country_20792_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20792/source_counts/final_counts/subset_counts_cw_version_20792_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Hypertensive Disorders of Pregnancy" = "n_adj")

country_20033_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20033/source_counts/final_counts/subset_counts_cw_version_20033_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Eclampsia" = "n_adj")

country_20900_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20900/source_counts/final_counts/subset_counts_cw_version_20900_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Severe Pre-eclampsia" = "n_adj")

country_29777_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/29777/source_counts/final_counts/subset_counts_cw_version_29777_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Obstructed Labour Acute Event" = "n_adj")

country_17186_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/17186/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Fistula" = "n_adj")

country_20465_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20465/source_counts/final_counts/subset_counts_cw_version_20465_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Abortive Outcome" = "n_adj")

country_20810_total <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/total/20810/source_counts/final_counts/subset_counts_cw_version_20810_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Ectopic Pregnancy" = "n_adj")

### Clinical
country_19694_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/19694/source_counts/final_counts/subset_counts_cw_version_19694_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Hemorrhage" = "n_adj")

country_20036_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20036/source_counts/final_counts/subset_counts_cw_version_20036_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Puerperal Sepsis" = "n_adj")

country_20552_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20552/source_counts/final_counts/subset_counts_cw_version_20552_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Other Maternal Infections" = "n_adj")

country_20792_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20792/source_counts/final_counts/subset_counts_cw_version_20792_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Hypertensive Disorders of Pregnancy" = "n_adj")

country_20033_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20033/source_counts/final_counts/subset_counts_cw_version_20033_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Eclampsia" = "n_adj")

country_20900_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20900/source_counts/final_counts/subset_counts_cw_version_20900_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Severe Pre-eclampsia" = "n_adj")

country_29777_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/29777/source_counts/final_counts/subset_counts_cw_version_29777_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Obstructed Labour Acute Event" = "n_adj")

#country_17186_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/17186/source_counts/final_counts/"), sheet = 4) %>% dplyr::rename("Fistula" = "n_adj")

country_20465_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20465/source_counts/final_counts/subset_counts_cw_version_20465_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Abortive Outcome" = "n_adj")

country_20810_clinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/clinical_data/20810/source_counts/final_counts/subset_counts_cw_version_20810_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Ectopic Pregnancy" = "n_adj")

### Nonclinical
country_19694_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/19694/source_counts/final_counts/subset_counts_cw_version_19694_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Hemorrhage" = "n_adj")

country_20036_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20036/source_counts/final_counts/subset_counts_cw_version_20036_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Puerperal Sepsis" = "n_adj")

country_20552_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20552/source_counts/final_counts/subset_counts_cw_version_20552_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Other Maternal Infections" = "n_adj")

country_20792_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20792/source_counts/final_counts/subset_counts_cw_version_20792_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Hypertensive Disorders of Pregnancy" = "n_adj")

country_20033_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20033/source_counts/final_counts/subset_counts_cw_version_20033_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Eclampsia" = "n_adj")

country_20900_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20900/source_counts/final_counts/subset_counts_cw_version_20900_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Severe Pre-eclampsia" = "n_adj")

country_29777_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/29777/source_counts/final_counts/subset_counts_cw_version_29777_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Obstructed Labour Acute Event" = "n_adj")

#country_17186_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/17186/source_counts/final_counts/"), sheet = 4) %>% dplyr::rename("Fistula" = "n_adj")

country_20465_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20465/source_counts/final_counts/subset_counts_cw_version_20465_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Maternal Abortive Outcome" = "n_adj")

country_20810_nonclinical <- read.xlsx(paste0("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/nonclinical_data/20810/source_counts/final_counts/subset_counts_cw_version_20810_counts_adj.xlsx"), sheet = 4) %>% dplyr::rename("Ectopic Pregnancy" = "n_adj")


#country_counts <- merge(country_19694, country_20036, country_20552, country_20792, country_20033, country_20900, country_29777, country_17186, country_20465, country_20810)

# Total
country_19694 <- country_19694_total %>% select(1:2, 4)
country_20036 <- country_20036_total %>% select(1:2, 4)
country_20552 <- country_20552_total %>% select(1:2, 4)
country_20792 <- country_20792_total %>% select(1:2, 4)
country_20033 <- country_20033_total %>% select(1:2, 4)
country_20900 <- country_20900_total %>% select(1:2, 4)
country_29777 <- country_29777_total %>% select(1:2, 4)
country_17186 <- country_17186_total %>% select(1:2, 4)
country_20465 <- country_20465_total %>% select(1:2, 4)
country_20810 <- country_20810_total %>% select(1:2, 4)
key_country_name_total <- rbind(country_19694, country_20036, country_20552, country_20792, country_20033, country_20900, country_29777, country_17186, country_20465, country_20810) %>% unique() %>% setorder(region_id, -hierarchy_key)

final_country_join_total <- list(key_country_name_total, country_19694_total, country_20036_total, country_20552_total, country_20792_total, country_20033_total, country_20900_total, country_29777_total, country_17186_total, country_20465_total, country_20810_total) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_join_total[is.na(final_country_join_total)] <- 0
# remember to source in locations from location_data
final_country_join_total <- merge(final_country_join_total, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-"sort_order", -"location_type")

# Clinical
country_19694 <- country_19694_clinical %>% select(1:2, 4)
country_20036 <- country_20036_clinical %>% select(1:2, 4)
country_20552 <- country_20552_clinical %>% select(1:2, 4)
country_20792 <- country_20792_clinical %>% select(1:2, 4)
country_20033 <- country_20033_clinical %>% select(1:2, 4)
country_20900 <- country_20900_clinical %>% select(1:2, 4)
country_29777 <- country_29777_clinical %>% select(1:2, 4)
#country_17186 <- country_17186_clinical %>% select(1:2, 4)
country_20465 <- country_20465_clinical %>% select(1:2, 4)
country_20810 <- country_20810_clinical %>% select(1:2, 4)
key_country_name_clinical <- rbind(country_19694, country_20036, country_20552, country_20792, country_20033, country_20900, country_29777, country_20465, country_20810) %>% unique() %>% setorder(region_id, -hierarchy_key)

final_country_join_clinical <- list(key_country_name_clinical, country_19694_clinical, country_20036_clinical, country_20552_clinical, country_20792_clinical, country_20033_clinical, country_20900_clinical, country_29777_clinical, country_20465_clinical, country_20810_clinical) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_join_clinical[is.na(final_country_join_clinical)] <- 0
final_country_join_clinical <- merge(final_country_join_clinical, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-"sort_order", -"location_type")

# Nonclinical
country_19694 <- country_19694_nonclinical %>% select(1:2, 4)
country_20036 <- country_20036_nonclinical %>% select(1:2, 4)
country_20552 <- country_20552_nonclinical %>% select(1:2, 4)
country_20792 <- country_20792_nonclinical %>% select(1:2, 4)
country_20033 <- country_20033_nonclinical %>% select(1:2, 4)
country_20900 <- country_20900_nonclinical %>% select(1:2, 4)
country_29777 <- country_29777_nonclinical %>% select(1:2, 4)
#country_17186 <- country_17186_nonclinical %>% select(1:2, 4)
country_20465 <- country_20465_nonclinical %>% select(1:2, 4)
country_20810 <- country_20810_nonclinical %>% select(1:2, 4)
key_country_name_nonclinical <- rbind(country_19694, country_20036, country_20552, country_20792, country_20033, country_20900, country_29777, country_20465, country_20810) %>% unique() %>% setorder(region_id, -hierarchy_key)
final_country_join_clinical[is.na(final_country_join_clinical)] <- 0

final_country_join_nonclinical <- list(key_country_name_nonclinical, country_19694_nonclinical, country_20036_nonclinical, country_20552_nonclinical, country_20792_nonclinical, country_20033_nonclinical, country_20900_nonclinical, country_29777_nonclinical, country_20465_nonclinical, country_20810_nonclinical) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_join_nonclinical[is.na(final_country_join_nonclinical)] <- 0
final_country_join_nonclinical <- merge(final_country_join_nonclinical, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-"sort_order",-"location_type")


# NOTE: FISTULA ONLY HAS TOTAL COUNTS < IT DOESN'T HAVE CLINICAL OR NONCLINICAL COUNTS> 
# Final Output #########################################################
#######################################################################
final_test_dt <- full_join(final_country_join_total, final_country_join_clinical, by = c("region_id", "location_name", "hierarchy_key"))
final_test_dt[is.na(final_test_dt)] <- 0

final_test_dt$`Maternal Hemorrhage` <- paste(final_test_dt$`Maternal Hemorrhage.x`, "(", final_test_dt$`Maternal Hemorrhage.y`,")")
final_test_dt$`Puerperal Sepsis` <- paste(final_test_dt$`Puerperal Sepsis.x`, "(", final_test_dt$`Puerperal Sepsis.y`, ")")
final_test_dt$`Other Maternal Infections`<- paste(final_test_dt$`Other Maternal Infections.x`, "(", final_test_dt$`Other Maternal Infections.y`, ")")
final_test_dt$`Hypertensive Disorders of Pregnancy` <- paste(final_test_dt$`Hypertensive Disorders of Pregnancy.x`, "(", final_test_dt$`Hypertensive Disorders of Pregnancy.y`, ")")
final_test_dt$`Eclampsia` <- paste(final_test_dt$Eclampsia.x, "(", final_test_dt$Eclampsia.y, ")")
final_test_dt$`Severe Pre-eclampsia` <- paste(final_test_dt$`Severe Pre-eclampsia.x`, "(", final_test_dt$`Severe Pre-eclampsia.y`, ")")
final_test_dt$`Maternal Abortive Outcome` <- paste(final_test_dt$`Maternal Abortive Outcome.x`, "(", final_test_dt$`Maternal Abortive Outcome.y`, ")")
final_test_dt$`Ectopic Pregnancy` <- paste(final_test_dt$`Ectopic Pregnancy.x`, "(", final_test_dt$`Ectopic Pregnancy.y`, ")")
final_test_dt$`Obstructed Labour Acute Event` <- paste(final_test_dt$`Obstructed Labour Acute Event.x`, "(", final_test_dt$`Obstructed Labour Acute Event.y`, ")")
final_test_dt_final <- final_test_dt %>% select(1:3, 23:31, 11)
final_test_dt_final <- merge(final_test_dt_final, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-'sort_order', -"location_type") #Apheteical order!

# Totals # GBD region and Country
total_gbd_region <- merge(total_gbd_region, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-"sort_order")
total_gbd_region <- final_country_join_total %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:9, 12, 13, 10:11) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
  
total_country <- merge(total_country, location, by = c('region_id','location_name')) %>% setorder(sort_order) %>% dplyr::select(-"sort_order")
total_country <- final_country_join_total %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:9, 12, 13, 10:11) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))


###########
output <- "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/"
out_filename <- "Final_GBD_Country_Region_Micro_Counts_July_2022.xlsx"
wb <- createWorkbook(title = out_filename)

addWorksheet(wb, "Presentation")
writeData(wb, "Presentation", final_test_dt_final)

addWorksheet(wb, "Total Counts")
writeData(wb, "Total Counts", final_country_join_total)

addWorksheet(wb, "Clinical Data Counts")
writeData(wb, "Clinical Data Counts", final_country_join_clinical)

addWorksheet(wb, "Nonclinical Data Counts")
writeData(wb, "Nonclinical Data Counts", final_country_join_nonclinical)

addWorksheet(wb, "T. Source Counts by GBD Region")
writeData(wb, "T. Source Counts by GBD Region", total_gbd_region)

addWorksheet(wb, "T. Source Counts by Country")
writeData(wb, "T. Source Counts by Country", total_country)

saveWorkbook(wb, paste0(output, "/", out_filename), overwrite = TRUE)
###########

######################################################################################################################
######################################################################################################################
######################################################################################################################
# GBD Maps
# • Combinations - Total Source Counts: 
# Maternal Hemorrhage, #ID: 19694
# Maternal Sepsis and Infection(P. Sepsis & Other Maternal Infection), #ID: 20036 & 20552
# Hypertensive Disorders(Hypertensive Disorders of Pregnancy, Eclampsia, Severe Preeclampsia), #ID 20792, 20033, & 20900
# Abortion, #ID: 20465
# Ectopic Pregnancy, #ID: 20810
# Maternal Obstruction(Obstructed.... & Fistula) #ID: 29777 & 17186
# • 
######################################################################################################################
######################################################################################################################
######################################################################################################################
###################### Total Source Counts ######################
# ----------
dt_mh <- get_crosswalk_version(19694) %>% select(nid, location_id, year_start, year_end, location_name)
source_counts(custom_data_table = dt_mh, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_hemorrhage/", strategy_detail = FALSE)
dt_map_mh <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_hemorrhage/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_a <- get_crosswalk_version(20036) %>% select(nid, location_id, year_start, year_end, location_name)
dt_b <- get_crosswalk_version(20552) %>% select(nid, location_id, year_start, year_end, location_name)
dt_soi <- rbind(dt_a, dt_b)
source_counts(custom_data_table = dt_soi, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_sepsis_infections/", strategy_detail = FALSE)
dt_map_soi <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_sepsis_infections/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_c <- get_crosswalk_version(20792) %>% select(nid, location_id, year_start, year_end, location_name)
dt_d <- get_crosswalk_version(20033) %>% select(nid, location_id, year_start, year_end, location_name)
dt_e <- get_crosswalk_version(20900) %>% select(nid, location_id, year_start, year_end, location_name)
dt_hd <- rbind(dt_c, dt_d, dt_e)
source_counts(custom_data_table = dt_hd, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/hypertensive_disorders/", strategy_detail = FALSE)
dt_map_hd <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/hypertensive_disorders/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
dt_ma <- get_crosswalk_version(20465) %>% select(nid, location_id, year_start, year_end, location_name)
source_counts(custom_data_table = dt_ma, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/abortion/", strategy_detail = FALSE)
dt_map_ma <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/abortion/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_ep <- get_crosswalk_version(20810) %>% select(nid, location_id, year_start, year_end, location_name)
source_counts(custom_data_table = dt_ep, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/ectopic_pregnancy/", strategy_detail = FALSE)
dt_map_ep <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/ectopic_pregnancy/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_f <- get_crosswalk_version(29777) %>% select(nid, location_id, year_start, year_end, location_name)
# Remember to filter Fistula, to included only incidence data!
dt_g <- get_crosswalk_version(17186) %>% filter(measure %in% "incidence") %>% select(nid, location_id, year_start, year_end, location_name)
dt_of <- rbind(dt_f, dt_g)
source_counts(custom_data_table = dt_of, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_obstruction/", strategy_detail = FALSE)
dt_map_of <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/total/maternal_obstruction/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)


source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")
#source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2020/inset_maps/gbd2020_map.R")
#source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_PREP_RDATA.r")



#custom function
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  for (i in 2:vector_length) {
    if (i==2) {
      bin_labels[i-1] <- sprintf("<%s",x[i])
    } else if (i==vector_length) {
      label <- sprintf("%s to %s",x[i-1],x[i])
      #label <- sprintf("\U2265%s",x[i-1])
      bin_labels[i-1] <- label
    } else {
      bin_labels[i-1] <- sprintf("%s to <%s",x[i-1],x[i])
    }
  }
  return(bin_labels)
}

# Given the country_name location_ids
#location_location <- location_data %>% select(location_id, location_name)
# -----
dt_map_ep <- dt_map_ep %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Ectopic Pregnancy at the Country-Level, GBD 2020"
data_input <- dt_map_ep
file_name <- "Ectopic_Pregnancy_Total_Counts_2022.pdf"
# -----
dt_map_hd <- dt_map_hd %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Maternal Hypertensive Disorders at the Country-Level, GBD 2020"
data_input <- dt_map_hd
file_name <- "Hypertensive_Disorders_Total_Counts_2022.pdf"
# -----
dt_map_ma <- dt_map_ma %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Maternal Abortive Outcome at the Country-Level, GBD 2020"
data_input <- dt_map_ma
file_name <- "Maternal_Abortive_Outcome_Total_Counts_2022.pdf"
# -----
dt_map_mh <- dt_map_mh %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Maternal Hemorrhage at the Country-Level, GBD 2020"
data_input <- dt_map_mh
file_name <- "Maternal_Hemorrhage_Total_Counts_2022.pdf"
# -----
dt_map_of <- dt_map_of %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Maternal Obstruction at the Country-Level, GBD 2020"
data_input <- dt_map_of
file_name <- "Maternal_Obstruction_Total_Counts_2022.pdf"
# -----
dt_map_soi <- dt_map_soi %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Total Counts for Maternal Sepsis and Infection at the Country-Level, GBD 2020"
data_input <- dt_map_soi
file_name <- "Maternal_Sepsis_Infection_Total_Counts_2022.pdf"

# Bins ###########################
bins <- quantile(data_input[, 'mapvar'], seq(0, 1, 0.1))
bins <- ceiling(bins)
bins <- unique(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)

savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/Total_Source_Counts_Macro/total_maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(data_input, 
        limits=bins,
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= bin_labels,
        pattern=NULL,
        col.reverse=FALSE, 
        na.color = "gray",
        title=title, ## min = 1998 and max = 2013 
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, #legend size 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()

######################################################################################################################
######################################################################################################################
###################### Clinical Source Counts ######################
# ----------
dt_mh_clinical <- get_crosswalk_version(19694)
dt_mh_clinical <- dt_mh_clinical %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
source_counts(custom_data_table = dt_mh_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_hemorrhage/", strategy_detail = FALSE)
dt_map_mh_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_hemorrhage/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_a <- get_crosswalk_version(20036) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "", !clinical_data_type %in% "literature")
dt_b <- get_crosswalk_version(20552) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
dt_soi_clinical <- rbind(dt_a, dt_b)
source_counts(custom_data_table = dt_soi_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_sepsis_infections/", strategy_detail = FALSE)
dt_map_soi_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_sepsis_infections/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_c <- get_crosswalk_version(20792) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
dt_d <- get_crosswalk_version(20033) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
dt_e <- get_crosswalk_version(20900) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
dt_hd_clinical <- rbind(dt_c, dt_d, dt_e)
source_counts(custom_data_table = dt_hd_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/hypertensive_disorders/", strategy_detail = FALSE)
dt_map_hd_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/hypertensive_disorders/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
dt_ma_clinical <- get_crosswalk_version(20465) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
source_counts(custom_data_table = dt_ma_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/abortion/", strategy_detail = FALSE)
dt_map_ma_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/abortion/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_ep_clinical <- get_crosswalk_version(20810) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "")
source_counts(custom_data_table = dt_ep_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/ectopic_pregnancy/", strategy_detail = FALSE)
dt_map_ep_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/ectopic_pregnancy/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_o_clinical <- get_crosswalk_version(29777) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(!clinical_data_type %in% "", !clinical_data_type %in% "literature")
# Remember to filter Fistula, to included only incidence data!#Fistula has no clinical data! No clinical_data_type column!
dt_of_clinical <- dt_o_clinical
source_counts(custom_data_table = dt_of_clinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_obstruction/", strategy_detail = FALSE)
dt_map_of_clinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/clinical/maternal_obstruction/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)


source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")
#source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_PREP_RDATA.r")

#custom function
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  for (i in 2:vector_length) {
    if (i==2) {
      bin_labels[i-1] <- sprintf("<%s",x[i])
    } else if (i==vector_length) {
      label <- sprintf("%s to %s",x[i-1],x[i])
      #label <- sprintf("\U2265%s",x[i-1])
      bin_labels[i-1] <- label
    } else {
      bin_labels[i-1] <- sprintf("%s to <%s",x[i-1],x[i])
    }
  }
  return(bin_labels)
}

# Given the country_name location_ids
#location_location <- location_data %>% select(location_id, location_name)
# -----
dt_map_ep_clinical <- dt_map_ep_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Ectopic Pregnancy at the Country-Level, GBD 2020"
data_input <- dt_map_ep_clinical
file_name <- "Ectopic_Pregnancy_Clinical_Counts_2022.pdf"
# -----
dt_map_hd_clinical <- dt_map_hd_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Maternal Hypertensive Disorders at the Country-Level, GBD 2020"
data_input <- dt_map_hd_clinical
file_name <- "Hypertensive_Disorders_Clinical_Counts_2022.pdf"
# -----
dt_map_ma_clinical <- dt_map_ma_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Maternal Abortive Outcome at the Country-Level, GBD 2020"
data_input <- dt_map_ma_clinical
file_name <- "Maternal_Abortive_Outcome_Clinical_Counts_2022.pdf"
# -----
dt_map_mh_clinical <- dt_map_mh_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Maternal Hemorrhage at the Country-Level, GBD 2020"
data_input <- dt_map_mh_clinical
file_name <- "Maternal_Hemorrhage_Clinical_Counts_2022.pdf"
# -----
dt_map_of_clinical <- dt_map_of_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Maternal Obstruction at the Country-Level, GBD 2020"
data_input <- dt_map_of_clinical
file_name <- "Maternal_Obstruction_Clinical_Counts_2022.pdf"
# -----
dt_map_soi_clinical <- dt_map_soi_clinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Clinical counts for Maternal Sepsis and Infection at the Country-Level, GBD 2020"
data_input <- dt_map_soi_clinical
file_name <- "Maternal_Sepsis_Infection_Clinical_Counts_2022.pdf"

# Bins ###########################
bins <- quantile(data_input[, 'mapvar'], seq(0, 1, 0.1))
bins <- ceiling(bins)
bins <- unique(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)

savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/Total_Source_Counts_Macro/clinical_maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(data_input, 
        limits=bins,
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= bin_labels,
        pattern=NULL,
        col.reverse=FALSE, 
        na.color = "gray",
        title=title, ## min = 1998 and max = 2013 
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()
######################################################################################################################
######################################################################################################################
###################### Nonclinical Source Counts ######################
# ----------
dt_mh_nonclinical <- get_crosswalk_version(19694)
dt_mh_nonclinical <- dt_mh_nonclinical %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
source_counts(custom_data_table = dt_mh_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_hemorrhage/", strategy_detail = FALSE)
dt_map_mh_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_hemorrhage/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_a <- get_crosswalk_version(20036) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
dt_b <- get_crosswalk_version(20552) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
dt_soi_nonclinical <- rbind(dt_a, dt_b)
source_counts(custom_data_table = dt_soi_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_sepsis_infections/", strategy_detail = FALSE)
dt_map_soi_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_sepsis_infections/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_c <- get_crosswalk_version(20792) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
dt_d <- get_crosswalk_version(20033) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
dt_e <- get_crosswalk_version(20900) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
dt_hd_nonclinical <- rbind(dt_c, dt_d, dt_e)
source_counts(custom_data_table = dt_hd_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/hypertensive_disorders/", strategy_detail = FALSE)
dt_map_hd_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/hypertensive_disorders/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
dt_ma_nonclinical <- get_crosswalk_version(20465) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
source_counts(custom_data_table = dt_ma_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/abortion/", strategy_detail = FALSE)
dt_map_ma_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/abortion/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_ep_nonclinical <- get_crosswalk_version(20810) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature")
source_counts(custom_data_table = dt_ep_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/ectopic_pregnancy/", strategy_detail = FALSE)
dt_map_ep_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/ectopic_pregnancy/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
# ----------
dt_o_nonclinical <- get_crosswalk_version(29777) %>% select(nid, location_id, year_start, year_end, location_name, clinical_data_type) %>% filter(clinical_data_type %in% "" | clinical_data_type %in% "literature") %>% select(-clinical_data_type)
dt_f_nonclinical <- get_crosswalk_version(17186) %>% select(nid, location_id, year_start, year_end, location_name)
# Remember to filter Fistula, to included only incidence data!#Fistula has no clinical data! No clinical_data_type column!
dt_of_nonclinical <- rbind(dt_o_nonclinical, dt_f_nonclinical) 
source_counts(custom_data_table = dt_of_nonclinical, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_obstruction/", strategy_detail = FALSE)
dt_map_of_nonclinical <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/map_counts/nonclinical/maternal_obstruction/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)


source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")
#source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_PREP_RDATA.r")

#custom function
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  for (i in 2:vector_length) {
    if (i==2) {
      bin_labels[i-1] <- sprintf("<%s",x[i])
    } else if (i==vector_length) {
      label <- sprintf("%s to %s",x[i-1],x[i])
      #label <- sprintf("\U2265%s",x[i-1])
      bin_labels[i-1] <- label
    } else {
      bin_labels[i-1] <- sprintf("%s to <%s",x[i-1],x[i])
    }
  }
  return(bin_labels)
}

# Given the country_name location_ids
#location_location <- location_data %>% select(location_id, location_name)
# -----
dt_map_ep_nonclinical <- dt_map_ep_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Ectopic Pregnancy at the Country-Level, GBD 2020"
data_input <- dt_map_ep_nonclinical
file_name <- "Ectopic_Pregnancy_nonclinical_Counts_2022.pdf"
# -----
dt_map_hd_nonclinical <- dt_map_hd_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Maternal Hypertensive Disorders at the Country-Level, GBD 2020"
data_input <- dt_map_hd_nonclinical
file_name <- "Hypertensive_Disorders_nonclinical_Counts_2022.pdf"
# -----
dt_map_ma_nonclinical <- dt_map_ma_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Maternal Abortive Outcome at the Country-Level, GBD 2020"
data_input <- dt_map_ma_nonclinical
file_name <- "Maternal_Abortive_Outcome_nonclinical_Counts_2022.pdf"
# -----
dt_map_mh_nonclinical <- dt_map_mh_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Maternal Hemorrhage at the Country-Level, GBD 2020"
data_input <- dt_map_mh_nonclinical
file_name <- "Maternal_Hemorrhage_nonclinical_Counts_2022.pdf"
# -----
dt_map_of_nonclinical <- dt_map_of_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Maternal Obstruction at the Country-Level, GBD 2020"
data_input <- dt_map_of_nonclinical
file_name <- "Maternal_Obstruction_nonclinical_Counts_2022.pdf"
# -----
dt_map_soi_nonclinical <- dt_map_soi_nonclinical %>% select(location_id, n_adj) %>% dplyr::rename(mapvar = n_adj)
title <- "Non-clinical counts for Maternal Sepsis and Infection at the Country-Level, GBD 2020"
data_input <- dt_map_soi_nonclinical
file_name <- "Maternal_Sepsis_Infection_nonclinical_Counts_2022.pdf"

# Bins ###########################
bins <- quantile(data_input[, 'mapvar'], seq(0, 1, 0.1))
bins <- ceiling(bins)
bins <- unique(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)

savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/Total_Source_Counts_Macro/nonclinical_maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(data_input, 
        limits=bins,
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= bin_labels,
        pattern=NULL,
        col.reverse=FALSE, 
        na.color = "gray",
        title=title, ## min = 1998 and max = 2013 
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()
# ##################################################################################################################################
# ##################################################################################################################################
# # Clinical Data Review Analysis
# ##################################################################################################################################
# ##################################################################################################################################
# 
# dt_19694 <- get_crosswalk_version(19694)
# write.xlsx(dt_19694, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_19694.xlsx")
# dt_20036 <- get_crosswalk_version(20036)
# write.xlsx(dt_20036, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20036.xlsx")
# dt_20552 <- get_crosswalk_version(20552)
# write.xlsx(dt_20552, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20552.xlsx")
# dt_20792 <- get_crosswalk_version(20792)
# write.xlsx(dt_20792, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20792.xlsx")
# dt_20033 <- get_crosswalk_version(20033)
# write.xlsx(dt_20033, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20033.xlsx")
# dt_20900 <- get_crosswalk_version(20900)
# write.xlsx(dt_20900, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20900.xlsx")
# dt_29777 <- get_crosswalk_version(29777)
# write.xlsx(dt_29777, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_29777.xlsx")
# dt_17186 <- get_crosswalk_version(17186)
# write.xlsx(dt_17186, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_17186.xlsx")
# dt_20465 <- get_crosswalk_version(20465)
# write.xlsx(dt_20465, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20465.xlsx")
# dt_20810 <- get_crosswalk_version(20810)
# write.xlsx(dt_20810, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20810.xlsx")
# 
# # dt_20930 <- get_bundle_version(20930) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20945 <- get_bundle_version(20945) %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20948 <- get_bundle_version(20948) %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20933 <- get_bundle_version(20933) %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20936 <- get_bundle_version(20936) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) 
# # dt_21185 <- get_bundle_version(21185) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_29876 <- get_bundle_version(29876) %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # #dt_21626 <- get_bundle_version(21626) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20942 <- get_bundle_version(20942) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# # dt_20951 <- get_bundle_version(20951) %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor)
# 
# # # Pulling all crosswalk data bins #This reads all the .xlsx from this directory
# # setwd("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/")
# # file.list <- list.files(pattern='*.xlsx')
# # df.list <- lapply(file.list, read_excel)
# 
# # Unique Metrics for Clinical Data Review
# dt_19694_edit <- dt_19694 %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20036_edit <- dt_20036 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20552_edit <- dt_20552 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20792_edit <- dt_20792 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20033_edit <- dt_20033 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20900_edit <- dt_20900 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_29777_edit <- dt_29777 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# #dt_17186_edit <- dt_17186 %>% filter(!clinical_data_type %in% "") %>% select(nid, source_type, location_id, location_name, year_start, year_end, clinical_data_type)
# dt_20465_edit <- dt_20465 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# dt_20810_edit <- dt_20810 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
# 
# 
# # Cause Combine
# # Maternal Hypertensive Disorders
# dt_mh <- dt_19694_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Hemorrhage' = 'nid')
# dt_hd <- rbind(dt_20792_edit, dt_20033_edit, dt_20900_edit) %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Hypertensive' = 'nid')
# dt_i <- rbind(dt_20036_edit, dt_20552_edit) %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Infections' = 'nid')
# dt_of <- dt_29777_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Obstruction' = 'nid') # No Fistula Clinical Sources
# dt_e <- dt_20810_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Ectopic' = 'nid')
# dt_a <- dt_20465_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Abortion' = 'nid')
# ###########
# output <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/"
# out_filename <- "unique_nids_bycause.xlsx"
# wb <- createWorkbook(title = out_filename)
# 
# addWorksheet(wb, "Maternal Hypertensive Disorders")
# writeData(wb, "Maternal Hypertensive Disorders", dt_hd)
# 
# addWorksheet(wb, "Maternal Hemorrhage")
# writeData(wb, "Maternal Hemorrhage", dt_hd)
# 
# addWorksheet(wb, "Maternal Infection")
# writeData(wb, "Maternal Infection", dt_i)
# 
# addWorksheet(wb, "Maternal Obstruction")
# writeData(wb, "Maternal Obstruction", dt_of)
# 
# addWorksheet(wb, "Maternal Ectopic")
# writeData(wb, "Maternal Ectopic", dt_e)
# 
# addWorksheet(wb, "Maternal Abortion")
# writeData(wb, "Maternal Abortion", dt_a)
# 
# saveWorkbook(wb, paste0(output, "/", out_filename), overwrite = TRUE)
# ###########
# 
# 
# # # Whole Combine
# # nid <- c(413916,125126,124938,125036,125037,120469,109943,120471,124877,109946,124884,125084,125125,125039,124935,413950)
# # table_nids <- data.table(nid)
# # dt_combine <- rbind(dt_19694_edit, dt_20036_edit, dt_20552_edit, dt_20792_edit, dt_20033_edit, dt_20900_edit, dt_29777_edit, dt_20465_edit, dt_20810_edit) %>% left_join(location_data %>% dplyr::select(location_id, location_type), by='location_id')
# # write.xlsx(dt_combine, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/All_Data_Bins_Clinical_crosswalk.xlsx")
# # 
# # dt_combine2 <- rbind(dt_20930, dt_20945, dt_20948, dt_20933, dt_20936, dt_21185, dt_29876, dt_20942, dt_20951) %>% left_join(location_data %>% dplyr::select(location_id, location_type), by='location_id')
# # write.xlsx(dt_combine2, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/All_Data_Bins_Clinical_bundleversion.xlsx")
# # 
# # crosswalk_nids <- left_join(table_nids, dt_combine, by = 'nid') %>% unique
# # write.xlsx(crosswalk_nids, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nids_of_interest_xwalk.xlsx")
# # bundleversion_nids <- left_join(table_nids, dt_combine2, by = 'nid') %>% unique
# # write.xlsx(bundleversion_nids, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nids_of_interest_bv.xlsx")
# # 
# # # Unique Subnationals
# # dt_combine_subnationals <- dt_combine %>% select(nid,location_type) %>% unique()
# # write.xlsx(dt_combine_subnationals, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nid_locationtypes.xlsx")
# # 
# # # Unique Years
# # dt_combine_years <- dt_combine %>% select(nid, year_start, year_end) %>% unique() %>% setorder(nid, year_start, year_end)
# # write.xlsx(dt_combine_years, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nid_years.xlsx")
# 
# 
# ### Unique NIDs ### Generating Component NIDs as well from Merged NIDs
# dt_combine_nids <- rbind(dt_19694_edit, dt_20036_edit, dt_20552_edit, dt_20792_edit, dt_20033_edit, dt_20900_edit, dt_29777_edit, dt_20465_edit, dt_20810_edit) %>% select(nid) %>% unique()
# # nid input for merged nids
# nids_input <- c(3822, 234672, 234758, 287201, 234771, 234740, 331084, 68367, 133665, 234745, 234692, 234764, 284439, 67132, 432298, 234761, 234774, 234742, 292575, 284422, 234747, 234703, 234750, 336851, 334465, 234765, 284442, 234670, 234766, 104241, 234671, 234769, 234738, 104246, 432265, 234674, 433061, 287203, 287207, 411100, 234760, 234772, 234741, 284421, 234746, 234693, 334464, 234762, 284440, 292574, 432266, 321359, 407536, 284419, 433059, 285520, 433115, 408336, 430710, 287204, 411786, 317423, 292437, 337619, 404395, 206640, 234704, 337129, 281819, 336852, 334466, 421046, 354896, 292577, 284444, 411787, 234673, 287202, 68535, 299375, 433060, 422874, 422318, 421047, 433114, 431674, 336847, 408680, 406980, 397812, 397813, 397814, 244369, 244370, 336850, 244371, 336849, 336848, 336203, 336846, 336843, 336838, 336833, 336827, 336825, 336824, 336817, 336845, 336844, 336842, 336841, 336840, 336839, 336837, 336836, 336835, 336834, 336832, 336831, 336830, 336829, 336828, 336826, 336822, 336820, 413916, 125126, 124938, 125036, 125037, 120469, 109943, 120471, 124877, 109946, 124884, 439735, 439739, 439738, 439737, 125084, 125125, 125039, 124935, 413950, 96714)
# 
# dt_combine_merged <- ghdx_merged_citation_component(nid = nids_input)
# write.xlsx(dt_combine_merged, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/merged_nids_component_nids.xlsx")
# 
# dt_combine_nids <- dt_combine_nids %>% select(nid) %>% unique()
# dt_combine_merged <- dt_combine_merged %>% select(`Component NID`) %>% unique() %>% dplyr::rename('nid' = 'Component NID')
# full_nids <- rbind(dt_combine_nids, dt_combine_merged) %>% unique()
# #full_nids[duplicated(full_nids)] #duplicates
# # full_nids_message <- full_nids %>% paste0(collapse=",")
# # message(full_nids_message)
# full_nids_input <- c(3822, 234672, 234758, 287201, 234771, 234740, 331084, 68367, 133665, 234745, 234692, 234764, 284439, 67132, 432298, 234761, 234774, 234742, 292575, 284422, 234747, 234703, 234750, 336851, 334465, 234765, 284442, 234670, 234766, 104241, 234671, 234769, 234738, 104246, 432265, 234674, 433061, 287203, 287207, 411100, 234760, 234772, 234741, 284421, 234746, 234693, 334464, 234762, 284440, 292574, 432266, 321359, 407536, 284419, 433059, 285520, 433115, 408336, 430710, 287204, 411786, 317423, 292437, 337619, 404395, 206640, 234704, 337129, 281819, 336852, 334466, 421046, 354896, 292577, 284444, 411787, 234673, 287202, 68535, 299375, 433060, 422874, 422318, 421047, 433114, 431674, 336847, 408680, 406980, 397812, 397813, 397814, 244369, 244370, 336850, 244371, 336849, 336848, 336203, 336846, 336843, 336838, 336833, 336827, 336825, 336824, 336817, 336845, 336844, 336842, 336841, 336840, 336839, 336837, 336836, 336835, 336834, 336832, 336831, 336830, 336829, 336828, 336826, 336822, 336820, 413916, 125126, 124938, 125036, 125037, 120469, 109943, 120471, 124877, 109946, 124884, 439735, 439739, 439738, 439737, 125084, 125125, 125039, 124935, 413950, 96714, 26333, 86886, 86887, 86888, 86889, 86890, 86891, 86892, 86893, 86894, 86895, 86896, 86897, 86898, 86899, 86900, 86910, 86911, 86912, 86913, 86914, 86915, 86916, 86917, 86949, 86950, 86951, 86952, 86953, 86954, 86955, 86956, 86957, 86958, 86993, 86994, 86995, 86996, 86997, 86998, 86999, 87000, 87001, 87002, 87003, 87004, 87005, 87006, 87007, 87008, 87009, 87010, 87011, 87012, 87013, 87014, 90314, 90315, 90316, 90317, 90318, 90319, 90322, 94170, 94171, 104242, 104243, 104244, 104245, 104246, 104247, 104248, 104249, 104250, 104251, 104252, 104253, 104254, 104255, 104256, 104257, 104258, 114624, 114876, 121272, 121273, 121274, 121275, 121276, 121277, 121278, 121279, 121280, 121281, 121282, 121334, 121405, 121407, 121408, 121415, 121416, 121417, 121418, 121419, 121420, 121421, 121422, 121423, 121424, 121425, 121444, 121445, 121446, 121447, 121448, 121449, 121450, 121451, 121452, 121453, 121454, 121831, 121832, 121841, 121842, 121843, 121844, 121845, 121846, 121847, 121848, 121849, 121850, 121851, 121854, 121855, 121856, 121857, 121858, 121859, 121860, 121862, 121863, 121917, 126517, 126518, 128781, 130051, 130054, 134187, 136997, 140085, 149500, 149501, 149502, 149503, 149504, 150449, 160484, 174085, 193857, 205019, 212492, 212493, 220205, 220786, 220787, 220788, 220789, 220790, 220791, 220792, 220793, 220794, 220795, 220796, 220797, 220798, 220799, 220800, 221323, 222560, 222563, 223670, 223672, 224499, 224500, 224501, 224502, 237756, 239353, 250036, 265423, 265424, 265425, 281773, 281819, 282493, 282496, 282497, 283865, 284235, 292435, 292436, 293984, 305653, 305654, 307815, 307817, 307818, 307819, 307820, 307821, 307822, 307823, 319414, 327619, 331137, 331138, 331139, 331140, 331141, 331142, 331143, 331144, 331145, 331146, 331147, 331148, 333358, 333359, 333360, 333361, 333649, 333650, 336193, 336195, 336197, 336198, 336199, 336200, 336802, 336853, 336854, 336855, 336856, 336860, 354240, 354860, 369475, 369564, 369579, 377002, 377003, 409529, 409530, 418019, 418020, 421046, 428363, 432299, 432300, 432301, 432304, 432305, 432306, 432307)
# full_nids_types <- ghdx_search_by_nid(nid = full_nids_input)



