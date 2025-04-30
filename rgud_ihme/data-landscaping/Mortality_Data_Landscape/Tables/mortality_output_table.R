## ---------------------------
##
## Script name: mortality_tables.R 
##
## Purpose of script: Create final data landscape table
##
## Author: Max Dodge
##
## Last updated: 2024-03-14
##
## Email: maxdodge@uw.edu
##
## ---------------------------
##
## Notes: Run columns of the input table through the source counts code and 
## assemble a final data landscape table. 
##
## ---------------------------

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

## ---------------------------

library(openxlsx)
library(dplyr)
library(data.table)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(reshape2)

## ---------------------------

source("/mnt/share/homes/maxdodge/data_landscape/scripts/general/source_counts_updated.R")

## ---------------------------

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

mortality_input <- read_xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/input_table/mortality_input.xlsx")

counting_key <- read_xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/input_table/data_type_counting.xlsx")

column_id <- c(4:91)

for(i in column_id){
  
  column <- colnames(mortality_input)[i]
  out_path <- '/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/'
  
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
  source_counts(custom_data_table = mortality_input, custom_subset_column_names = column, 
                custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/",column,"/"),
                data_type_detail = counting_key)
}

for(i in column_id){
  column <- colnames(mortality_input)[i]
  print(column)
  readpath <- paste0("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  if(i > column_id[1]) {
    mortality_table <- left_join(mortality_table, test_v)
  }
  else {
    mortality_table <- test_v
  }
}

all_country_regions <- location_data %>% 
  select(region_id, location_name, location_type, sort_order) %>% 
  filter(location_type %in% 'region' | location_type %in% 'admin0')

country_list <- location_data %>%
  separate(path_to_top_parent, into = c("global", "super_region", "region", "country", "admin1", "admin2", "admin3"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(country_id = as.integer(country)) %>%
  select(location_id, country_id)

most_recent_year <- left_join(mortality_input, country_list) %>%
  select(location_id, year_end, country_id) %>%
  filter(!is.na(year_end)) %>%
  group_by(country_id) %>%
  summarise(most_recent_year = max(year_end)) %>%
  dplyr::rename(location_id = country_id)

most_recent_year <- left_join(most_recent_year, location_data) %>%
  select(location_name, most_recent_year)

mortality_table_year <- left_join(mortality_table, most_recent_year)

final_mortality_table <- left_join(all_country_regions, mortality_table_year, by = c('region_id', 'location_name'))
final_mortality_table[is.na(final_mortality_table)] <- 0

subcause_table <- final_mortality_table[,c(1:5, 15:24)]

write.xlsx(final_mortality_table, "/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/mortality_table.xlsx")
write.xlsx(subcause_table, "/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
