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

source("/mnt/share/homes/maxdodge/data_landscape/scripts/general/source_counts_updated.R")
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

counting_key <- read_xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/input_table/data_type_counting.xlsx")

anc1_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/anc1_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

anc1NIDs <- anc1_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = anc1NIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

anc1_dummy <- left_join(anc1_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(anc1_dummy))
col_id2 <- grep("Special_DHS", colnames(anc1_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(anc1_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc1/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = anc1_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc1/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(anc1_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc1/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(anc1_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    anc1_table <- left_join(anc1_table, test_v)
  }
  else {
    anc1_table <- test_v
  }
}

all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(anc1_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

anc1_table_year <- left_join(anc1_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_anc1_table <- left_join(location_data_cleaned, anc1_table_year, by = c('region_id', 'location_name'))
final_anc1_table[is.na(final_anc1_table)] <- 0

write.xlsx(final_anc1_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/anc1/anc1_table.xlsx")

### ANC4


anc4_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/anc4_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

anc4NIDs <- anc4_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = anc4NIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

anc4_dummy <- left_join(anc4_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(anc4_dummy))
col_id2 <- grep("Special_DHS", colnames(anc4_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(anc4_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc4/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = anc4_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc4/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(anc4_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc4/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(anc4_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    anc4_table <- left_join(anc4_table, test_v)
  }
  else {
    anc4_table <- test_v
  }
}
all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(anc4_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

anc4_table_year <- left_join(anc4_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_anc4_table <- left_join(location_data_cleaned, anc4_table_year, by = c('region_id', 'location_name'))
final_anc4_table[is.na(final_anc4_table)] <- 0

write.xlsx(final_anc4_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/anc4/anc4_table.xlsx")


### CSEC

csec_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/csec_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

csecNIDs <- csec_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = csecNIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

csec_dummy <- left_join(csec_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(csec_dummy))
col_id2 <- grep("Special_DHS", colnames(csec_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(csec_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/csec/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = csec_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/csec/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(csec_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/csec/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(csec_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    csec_table <- left_join(csec_table, test_v)
  }
  else {
    csec_table <- test_v
  }
}
all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(csec_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

csec_table_year <- left_join(csec_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_csec_table <- left_join(location_data_cleaned, csec_table_year, by = c('region_id', 'location_name'))
final_csec_table[is.na(final_csec_table)] <- 0

write.xlsx(final_csec_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/csec/csec_table.xlsx")

### IFD


ifd_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/ifd_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

ifdNIDs <- ifd_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = ifdNIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

ifd_dummy <- left_join(ifd_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(ifd_dummy))
col_id2 <- grep("Special_DHS", colnames(ifd_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(ifd_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/ifd/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = ifd_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/ifd/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(ifd_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/ifd/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(ifd_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    ifd_table <- left_join(ifd_table, test_v)
  }
  else {
    ifd_table <- test_v
  }
}
all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(ifd_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

ifd_table_year <- left_join(ifd_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_ifd_table <- left_join(location_data_cleaned, ifd_table_year, by = c('region_id', 'location_name'))
final_ifd_table[is.na(final_ifd_table)] <- 0

write.xlsx(final_ifd_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/ifd/ifd_table.xlsx")

### MCARE


mcare_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/mcare_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

mcareNIDs <- mcare_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = mcareNIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

mcare_dummy <- left_join(mcare_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(mcare_dummy))
col_id2 <- grep("Special_DHS", colnames(mcare_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(mcare_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/mcare/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = mcare_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/mcare/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(mcare_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/mcare/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(mcare_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    mcare_table <- left_join(mcare_table, test_v)
  }
  else {
    mcare_table <- test_v
  }
}

all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(mcare_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

mcare_table_year <- left_join(mcare_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_mcare_table <- left_join(location_data_cleaned, mcare_table_year, by = c('region_id', 'location_name'))
final_mcare_table[is.na(final_mcare_table)] <- 0

write.xlsx(final_mcare_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/mcare/mcare_table.xlsx")

### PNC


pnc_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/pnc_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

pncNIDs <- pnc_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = pncNIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

pnc_dummy <- left_join(pnc_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(pnc_dummy))
col_id2 <- grep("Special_DHS", colnames(pnc_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(pnc_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/pnc/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = pnc_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/pnc/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(pnc_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/pnc/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(pnc_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    pnc_table <- left_join(pnc_table, test_v)
  }
  else {
    pnc_table <- test_v
  }
}

all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(pnc_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

pnc_table_year <- left_join(pnc_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_pnc_table <- left_join(location_data_cleaned, pnc_table_year, by = c('region_id', 'location_name'))
final_pnc_table[is.na(final_pnc_table)] <- 0

write.xlsx(final_pnc_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/pnc/pnc_table.xlsx")

### SBA


sba_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/pregnancy_care/sba_Oct.xlsx") %>%
  dplyr::rename(location_name = location_name.x) %>%
  select(-location_name.y) 

sbaNIDs <- sba_dummy %>%
  distinct(nid)

data_types <- ghdx_search_nid_data_type(nid = sbaNIDs$nid) %>%
  select(NID, Data_Type, type) %>%
  dplyr::rename('nid' = 'NID', 'data_type' = 'Data_Type')

sba_dummy <- left_join(sba_dummy, data_types, by = join_by(nid))

col_id1 <- grep("All_data", colnames(sba_dummy))
col_id2 <- grep("Special_DHS", colnames(sba_dummy))

column_id <- c(col_id1:col_id2)

for(i in column_id){
  
  column <- colnames(sba_dummy)[i]
  out_path <- '/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/sba/'
  
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } 
  
  else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  source_counts(custom_data_table = sba_dummy, custom_subset_column_names = column, 
                custom_subset_column_values = 1, 
                out_path = paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/sba/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(sba_dummy)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/sba/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  if(sum(sba_dummy[i]) == 0) {
    test_v <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/empty_file/subset_Custom_Table_counts_adj.xlsx", sheet = 4) %>% dplyr::rename(!!column := 'n_adj')
  }
  else {
    test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  }
  test_v[is.na(test_v)] <- 0
  if(i > column_id[1]) {
    sba_table <- left_join(sba_table, test_v)
  }
  else {
    sba_table <- test_v
  }
}

all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(sba_dummy, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

sba_table_year <- left_join(sba_table, most_recent_year)

location_data_cleaned <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

final_sba_table <- left_join(location_data_cleaned, sba_table_year, by = c('region_id', 'location_name'))
final_sba_table[is.na(final_sba_table)] <- 0

write.xlsx(final_sba_table, "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/sba/sba_table.xlsx")


