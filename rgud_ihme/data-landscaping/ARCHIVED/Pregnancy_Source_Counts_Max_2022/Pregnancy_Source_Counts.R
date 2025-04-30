# Appending Files
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

library(openxlsx)
library(dplyr)
library(data.table)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(reshape2)


location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)
nrow(location_data)
# Locations ids that need to be added:44794 44793 44795 44797 44798 44799 44800

# mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='shared', host='epi-modeling-db.ihme.washington.edu')
# # Mimicking the GHDx csv output column names
# query_location_id <- dbGetQuery(mydb, "select location_set_id, location_set_name from shared.location_set where location_set_id in (
# select distinct(location_set_id) from shared.location_hierarchy_history where location_id = 44793);")

#/mnt/share/scratch/users/maxdodge/Prepped_Data/
########################Source Counts for Pregnancy
data_anc1 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/anc1_Oct.xlsx") %>% mutate(All_Data = 1)
#data_anc1 <- data_anc1 %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_anc1 <- data_anc1 %>% select(nid, location_id, year_start, year_end, admin_level_final)
nrow(data_anc1)
unique_nids_anc1 <- anti_join(data_anc1, location_data_1, by = 'location_id')
unique(unique_nids_anc1$location_id)

# colnames(data_anc1)
# unique(data_anc1$admin_level_final)
#/mnt/share/scratch/users/maxdodge/Source_counts_output/
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/Admin0/")
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/Admin1/")
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/GPS/")
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/Other Subnationals/")
source_counts(custom_data_table = data_anc1, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/All Data/")
# Survey Series #DHS, MICS, RHS


###### For Series -- Pregnanacy
###### Second/Third Batch of Source Counts
#anc1, anc4, ifd, mcare, pnc, sba, csec
dt_series_1 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/anc1_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, DHS) #If remove the select(), and process all the columns, the final output it incorrect. I need to investigate why that happens!
dt_series_1[is.na(dt_series_1)] <- 0
source_counts(custom_data_table = dt_series_1, custom_subset_column_names = 'DHS', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc1/DHS/")

dt_series_2 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, MICS)
dt_series_2[is.na(dt_series_2)] <- 0
source_counts(custom_data_table = dt_series_2, custom_subset_column_names = 'MICS', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/MICS/")

dt_series_3 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, RHS)
dt_series_3[is.na(dt_series_3)] <- 0
source_counts(custom_data_table = dt_series_3, custom_subset_column_names = 'RHS', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/RHS/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

dt_series_4 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, Other_Survey)
dt_series_4[is.na(dt_series_4)] <- 0
source_counts(custom_data_table = dt_series_4, custom_subset_column_names = 'Other_Survey', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Other_Survey/")

dt_series_5 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, Other)
dt_series_5[is.na(dt_series_5)] <- 0
source_counts(custom_data_table = dt_series_5, custom_subset_column_names = 'Other', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Other/")

dt_series_6 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, Special_DHS)
dt_series_6[is.na(dt_series_6)] <- 0
source_counts(custom_data_table = dt_series_6, custom_subset_column_names = 'Special_DHS', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Special_DHS/")

#input: J:/temp/maxdodge/Survey_Series_Data
#output: J:/temp/maxdodge/DHS
#############################################################################

data_anc4 <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/anc4_Oct.xlsx") %>% mutate(All_Data = 1)
#data_anc4 <- data_anc4 %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_anc4 <- data_anc4 %>% select(nid, location_id, year_start, year_end, admin_level_final)
unique_nids_anc4 <- anti_join(data_anc4, location_data_1, by = 'location_id')
unique(unique_nids_anc4$location_id)

source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/Admin0/")
source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/Admin1/")
source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/GPS/")
source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/Other Subnationals/")
source_counts(custom_data_table = data_anc4, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/anc4/All Data/")
#############################################################################

data_csec <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx") %>% mutate(All_Data = 1)
data_csec <- data_csec %>% select(nid, location_id, year_start, year_end, admin_level_final)
unique_nids_csec <- anti_join(data_csec, location_data_1, by = 'location_id')
unique(unique_nids_csec$location_id)

source_counts(custom_data_table = data_csec, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Admin0/")
source_counts(custom_data_table = data_csec, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Admin1/")
source_counts(custom_data_table = data_csec, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_csec, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/GPS/")
source_counts(custom_data_table = data_csec, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/Other Subnationals/")
source_counts(custom_data_table = data_csec, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/csec/All Data/")
#############################################################################

data_ifd <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/ifd_Oct.xlsx") %>% mutate(All_Data = 1)
#data_ifd <- data_ifd %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_ifd <- data_ifd %>% select(nid, location_id, year_start, year_end, admin_level_final)
unique_nids_ifd <- anti_join(data_ifd, location_data_1, by = 'location_id')
unique(unique_nids_ifd$location_id)

source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/Admin0/")
source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/Admin1/")
source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/GPS/")
source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/Other Subnationals/")
source_counts(custom_data_table = data_ifd, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/ifd/All Data/")
#############################################################################

data_mcare <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/mcare_Oct.xlsx") %>% mutate(All_Data = 1)
#data_mcare <- data_mcare %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_mcare <- data_mcare %>% select(nid, location_id, year_start, year_end, admin_level_final)
#unique_nids_mcare <- anti_join(data_mcare, location_data_1, by = 'location_id')
#unique(unique_nids_mcare$location_id)

source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/Admin0/")
source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/Admin1/")
source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/GPS/")
source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/Other Subnationals/")
source_counts(custom_data_table = data_mcare, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/mcare/All Data/")
#############################################################################

data_pnc <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/pnc_Oct.xlsx") %>% mutate(All_Data = 1)
#data_pnc <- data_pnc %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_pnc <- data_pnc %>% select(nid, location_id, year_start, year_end, admin_level_final)
#unique_nids_pnc <- anti_join(data_pnc, location_data_1, by = 'location_id')
#unique(unique_nids_pnc$location_id)

source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/Admin0/")
source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/Admin1/")
source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/GPS/")
source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/Other Subnationals/")
source_counts(custom_data_table = data_pnc, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/pnc/All Data/")
#############################################################################

data_sba <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data/sba_Oct.xlsx") %>% mutate(All_Data = 1)
#data_sba <- data_sba %>% select(nid, location_id, year_start, year_end, admin_level_final)
data_sba <- data_sba %>% select(nid, location_id, year_start, year_end, admin_level_final)
unique_nids_sba <- anti_join(data_sba, location_data_1, by = 'location_id')
unique(unique_nids_sba$location_id)

source_counts(custom_data_table = data_sba, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 0, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/Admin0/")
source_counts(custom_data_table = data_sba, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/Admin1/")
source_counts(custom_data_table = data_sba, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 2, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/Admin2/")
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#GPS Other and Other Subnationals -- Change data type?
source_counts(custom_data_table = data_sba, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 3, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/GPS/")
source_counts(custom_data_table = data_sba, custom_subset_column_names = 'admin_level_final', custom_subset_column_values = 4, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/Other Subnationals/")
source_counts(custom_data_table = data_sba, custom_subset_column_names = 'All_Data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/maxdodge/Source_counts_output/sba/All Data/")

#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
dt <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data_Nov22/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, All_data, Other, RHS, Vital_Reg, YARHS)
#anc1, anc4, ifd, mcare, pnc, sba, csec

ncol(dt)
colnames(dt)
column_id <- c(5:9) #6:32
#column_id <- 5
for(i in column_id)
{
  for(j in i)
    column <- colnames(dt)[i] #6-32
  out_path <- '/ihme/scratch/users/maxdodge/Source_Counts_Nov22/csec/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = dt, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/maxdodge/Source_Counts_Nov22/csec/",column,"/"))
}


dt <- read.xlsx("/ihme/scratch/users/maxdodge/Prepped_Data_Oct21/csec_Oct.xlsx") %>% select(nid, location_id, year_start, year_end, DHS, DHS1, DHS2, DHS3, DHS4, DHS5, DHS6, DHS7, MICS, MICS1, MICS2, MICS3, MICS4, MICS5, MICS6, Other, Other_Survey, RHS, Unknown_Phase_DHS, Unknown_Phase_MICS)
# For anc1!!!!!!!!!!!!!!!!!!!!!!!!!
column_id <- c(5:24) #2:29
for(i in column_id){
  for(j in i)
    column <- colnames(dt)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/Source_Counts_Oct21/anc1/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}


