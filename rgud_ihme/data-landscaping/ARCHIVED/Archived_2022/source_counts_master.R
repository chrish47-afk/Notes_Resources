## Title: Source Counts, 2022
## Authors: Mae Dirac, Helen Ippolito, and Christian Hernandez
## Edited and Updated on March, 2022 by: Christian Hernandez
## Date Created: Nov 24, 2020
## Date Updated: March 30, 2022

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

############################################################
### SET UP
############################################################
## Loading Packages
## Alternative -- > You can use <pacman::p_load()> To upload your packages!
library(stringr)
library(openxlsx)
library(RMySQL)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape)
library(reshape2)


user <- Sys.getenv("USER")
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

# Pulling Location_data, this can be incorporated into the functions. But for the moment we are calling it globally.
gbd_rnd <- 7
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)


# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# The table has multiple duplicate columns. This is an issue. Discuss this with the Team on what the best approach is to address this!

source_counts <- function(
  cw_id = NULL, 
  custom_data_table, #dt 
  source_type= NULL,#source_type #What should this be named? Should we stay with source_type?
  clinical_data_only = FALSE,
  nonclinical_data_only = FALSE,
  custom_subset_column_names = NULL,#subset 
  custom_subset_column_values = NULL,#subset_values
  strategy_detail = FALSE,
  out_path = "/ihme/scratch/users/chrish47/" #out_path #CHECK THIS!
  ){
  # Creating Directories Automatically, in the out_path.
  # This is to avoid file_path/file errors.
  # For better organization and clarity for users.
  if(file.exists(paste0(out_path, 'source_counts'))){
    counts_path <- paste0(out_path, 'source_counts')
  } else {
    dir.create(paste0(out_path, 'source_counts'))
    counts_path <- paste0(out_path, 'source_counts')
  }
  if(file.exists(paste0(counts_path, '/vetting'))){
    vetting_path <- paste0(counts_path, '/vetting')
  } else {
    dir.create(paste0(counts_path, '/vetting'))
    vetting_path <- paste0(counts_path, '/vetting')
  }
  if(file.exists(paste0(counts_path, '/final_counts'))){
    final_counts <- paste0(counts_path, '/final_counts')
  } else {
    dir.create(paste0(counts_path, '/final_counts'))
    final_counts <- paste0(counts_path, '/final_counts')
  }
  if(file.exists(paste0(counts_path, '/unmerged_data_bins'))){
    unmerged_path <- paste0(counts_path, '/unmerged_data_bins')
  } else {
    dir.create(paste0(counts_path, '/unmerged_data_bins'))
    unmerged_path <- paste0(counts_path, '/unmerged_data_bins')
  }
  # Get crosswalk version if it hasn't already been written to flat file
  if(is.null(cw_id) == FALSE){
    if (file.exists(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))) {
      cw_version <- read.xlsx(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
      cw_version[cw_version == ""] <- NA #Converting all empty cells to NA.
    } else {
      cw_version <- get_crosswalk_version(cw_id)
      write.xlsx(cw_version, paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
      cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. 
    }
  } else if(is.null(custom_data_table) == FALSE){
    cw_version <- as.data.table(custom_data_table)
  } else {
    stop("Please input a get_crosswalk_version id or a df/dt for source counts.")
  }
  
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  if('location_name' %in% colnames(cw_version)) {
    cw_version <- cw_version %>% dplyr::select(-location_name) #This was to avoid overlapping errors for data inputs a crosswalk_version_id and a custome table #Additionally, this is more accurate, as location_name will be used from location_data, instead what the data bins, as location_name can sometimes be succestible to errors.
  }
  # Unmerged Function - Counts
  unmerged_nid_count_unadjusted(cw_id = cw_id, cw_version = cw_version, source_type = source_type, custom_subset_column_names = custom_subset_column_names, custom_subset_column_values = custom_subset_column_values, clinical_data_only = clinical_data_only, nonclinical_data_only = nonclinical_data_only, unmerged_path = unmerged_path)
  # Merged Function - Counts
  merged_nid_count_adjustment(cw_id = cw_id, cw_version = cw_version, source_type = source_type, custom_subset_column_names = custom_subset_column_names, custom_subset_column_values = custom_subset_column_values, clinical_data_only = clinical_data_only, nonclinical_data_only = nonclinical_data_only, unmerged_path = unmerged_path, strategy_detail = strategy_detail, final_counts = final_counts)
}
# From original code -- WORKED
source_counts(cw_id = 12221, source_type = 'inpatient', out_path = "/ihme/scratch/users/chrish47/")
source_counts(cw_id = 12221, source_type = 'claims', out_path = "/ihme/scratch/users/chrish47/")
source_counts(cw_id = 12221, source_type = 'prevalence', out_path = "/ihme/scratch/users/chrish47/")
source_counts(cw_id = 9890, source_type = 'inpatient', out_path = "/ihme/scratch/users/chrish47/")
source_counts(cw_id = 9890, source_type = 'claims', out_path = "/ihme/scratch/users/chrish47/")
source_counts(cw_id = 9890, source_type = 'prevalence', out_path = "/ihme/scratch/users/chrish47/")
# Maternal Abortion -- WORKED
source_counts(cw_id = 20465, source_type = 'incidence', out_path = "/ihme/scratch/users/chrish47/testing_Laura/")
source_counts(cw_id = 20465, source_type = 'inpatient', out_path = "/ihme/scratch/users/chrish47/testing_Laura/")
source_counts(cw_id = 20465, source_type = 'claims, inpatient only', out_path = "/ihme/scratch/users/chrish47/testing_Laura/")
# Crosswalk and Custom column names & values -- WORKED
source_counts(cw_id = 20465, custom_subset_column_names = 'uncertainty_type_value', custom_subset_column_values = 95, out_path = "/ihme/scratch/users/chrish47/testing_Laura/")
# Custom data input(nid,location_id,year_start,year_end) and required columns & Custom column names and values -- WORKED
source_counts(custom_data_table = dt_original_test, custom_subset_column_names = c('cv_test1','cv_test4'), custom_subset_column_values = c(1,1), out_path = "/ihme/scratch/users/chrish47/testing_Laura/")
# Clinical vs. Nonclinical data -- WORKED
source_counts(cw_id = 20036, nonclinical_data_only = TRUE, out_path = "/ihme/scratch/users/chrish47/source_counts_mb/testing/nonclinical/")
#############################################################################################################################
#############################################################################################################################
#' Title
#'
#' @param nid 
#' @param full 
#'
#' @return
#' @export
#'
#' @examples
ghdx_search_by_nid <- function(nid = NULL, full = FALSE){ #http://internal-ghdx.healthdata.org/search-by-nids-all-types/
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  query_data <- query_data %>% dplyr::rename('NID' = 'nid', 'Title' = 'title', 'Content Type' = 'type') 
  query_data <- query_data %>% transform(NID = as.numeric(NID))
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data, NID %in% nid_t$nid)
    return(subset) 
  } else if (full == TRUE){
    return(query_data)
  }
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

ghdx_merged_citation_component <- function(nid, full = FALSE){ #http://internal-ghdx.healthdata.org/merged-citations-components
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data_merged <- dbGetQuery(mydb, "SELECT field_merged_records_target_id, entity_id FROM field_data_field_merged_records;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  # Merging to get Title
  query_data_merged <- query_data_merged %>% dplyr::rename('nid' = 'field_merged_records_target_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid')
  query_data_merged <- query_data_merged %>% dplyr::select(nid, title, entity_id)
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data_merged, entity_id %in% nid_t$nid)
    subset <- subset %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(subset)
  } else if(full == TRUE) {
    query_data_merged <- query_data_merged %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(query_data_merged)
  }
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}
#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################
#' Title
#'
#' @param cw_version 
#' @param cw_id 
#' @param source_type 
#' @param custom_subset_column_names 
#' @param custom_subset_column_values 
#' @param unmerged_path 
#'
#' @return
#' @export
#'
#' @examples
unmerged_nid_count_unadjusted <- function(cw_version, cw_id, source_type = NULL, custom_subset_column_names = NULL, custom_subset_column_values = NULL, clinical_data_only = FALSE, nonclinical_data_only = FALSE, strategy_detail = FALSE, unmerged_path){
  if(is.null(source_type) == FALSE) {
    if (grepl("claims", source_type, fixed = TRUE) == TRUE) {
      source_type_t <- data.table(source_type)
      cw_locs <- cw_version %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      } else if (grepl("inpatient", source_type, fixed = TRUE) == TRUE) {
        source_type_t <- data.table(source_type)
        cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
        } else if (source_type == "incidence") {
          source_type_t <- data.table(source_type)
          cw_locs <- cw_version %>% 
          filter(is.na(clinical_data_type) & measure == "incidence") %>% 
          left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
          mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
            filter(!country_id=="OTHER") %>%
            mutate(country_id = as.integer(country_id)) %>%
            left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
            dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
            write.xlsx(cw_locs, "/ihme/homes/chrish47/dt3_u.xlsx")
            # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the
            # only "IHME" source in this bundle).
            # Review the above filter(!country_id == "OTHER") for further details on why these were filtered out, connect to the above comment on 
            # NID: 416752
            # This particular NID is a dummy NID or an NID for other purposes!
            # It will be important to properly ver this with other crosswalk version ids.
            } else if (source_type == "prevalence") {
              source_type_t <- data.table(source_type)
              cw_locs <- cw_version %>% 
              filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
              left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
              mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
              filter(!country_id=="OTHER") %>%
              mutate(country_id = as.integer(country_id)) %>%
              left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
              dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
              }
    } else if(is.null(custom_subset_column_names) == FALSE && is.null(custom_subset_column_values) == FALSE){
      subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      subset_table <- select(subset_table, -custom_subset_column_values)
      subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
      subset_table <- as.data.table(subset_table) #
      cw_locs <- cw_version %>% 
        left_join(location_data %>% 
                    select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        inner_join(subset_table, by = custom_subset_column_names) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(is.null(custom_subset_column_names) == TRUE && is.null(custom_subset_column_values) == TRUE && clinical_data_only == FALSE && nonclinical_data_only == FALSE && is.null(source_type) == TRUE) {
      print('total')
      #subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      #subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      #subset_table <- select(subset_table, -custom_subset_column_values)
      #subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
      cw_locs <- cw_version %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(clinical_data_only == TRUE) {
      print('test clinical')
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(!clinical_data_type %in% NA,!clinical_data_type %in% "literature") %>% #for maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
                ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(nonclinical_data_only == TRUE) {
      print('test nonclinical')
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% NA | clinical_data_type %in% "literature") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
                ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      } else {
        message("Invalid source_type argument.")
        message("OR the subset inputted for processing is not valid. Please review the arguments for this function!")
    } 
  write.xlsx(cw_locs, "/ihme/homes/chrish47/dt_after_filter_unmerged.xlsx")
  # Check to see if there are any sources to count for the given source type
  if (nrow(cw_locs) == 0) {
    if(is.null(cw_id) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_cw_version_", cw_id, "_counts_unadj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n = 0, hierarchy_key = NA)
      write.xlsx(empty_table, paste0(unmerged_path, "/", out_filename))
      message(cat(paste0("cw_id ", cw_id, ", ", source_type), ": no sources of this data type. An appropriate unadjusted table with zero counts has been generated."))
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_unadj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n = 0, hierarchy_key = NA)
      write.xlsx(empty_table, paste0(unmerged_path, "/", out_filename))
      message(cat(paste0("Custom Table, ", source_type), ": no sources of this data type. An appropriate unadjusted table with zero counts has been generated."))
    }
  } else {
    # Unique NID-location-year_start combinations [most granular locs]
    # Unique counts for all locations #
    count_loc <- cw_locs %>% dplyr::distinct(nid, year_start, year_end, location_name) %>%
      group_by(location_name) %>% 
      dplyr::tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    # The below notes and instructions were automated. This was done manually before.
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # Only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    
      count_loc_merged <- cw_locs %>% 
        dplyr::distinct(nid, year_start, year_end, location_name) %>%
        dplyr::pull(nid) %>%
        unique()
      count_loc_merged_message <- count_loc_merged %>% paste0(collapse=",")
    # Message, providing the NID list for processing OR Searching for Merged NIDs and their NID components
    #message(cat(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
    if(is.null(cw_id) == FALSE){
      message(cat(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
      } else {
        message(cat(paste0("Custom Table, ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
        }
    
    # Sourcing GHDx database and creating appropriate files with Merged NIDs and their NID components
    dt_merged_types <- ghdx_search_by_nid(nid = count_loc_merged)
    dt_component_records <- ghdx_merged_citation_component(full = TRUE)
    
    # Creates the Merge NIDs and component NIDs for Processing. This mimicks what the original manual processes.
    #file_extension_merged_types <- paste0(unmerged_path,"/",cw_id,"_",source_type,"_merge_types.xlsx")
    if(is.null(cw_id) == FALSE){
      #print(source_type)
      file_extension_merged_types <- paste0(unmerged_path,"/",cw_id,"_",ifelse(is.null(source_type) == FALSE, source_type,""),"_merge_types.xlsx")
      } else {
        file_extension_merged_types <- paste0(unmerged_path,"/","Custom_Table",ifelse(is.null(source_type) == FALSE, source_type,""),"_merge_types.xlsx")
        }
    file_extension_merged_component_records <- paste0(unmerged_path,"/", "merged_citation_component_records.xlsx")
    
    write.xlsx(dt_merged_types, file_extension_merged_types)
    write.xlsx(dt_component_records, file_extension_merged_component_records)
    
    # Unique NID-country-year_start combinations [country-level location counts] # 
    #detail <- ifelse(strategy_detail == TRUE, , NULL)
    if(strategy_detail == TRUE){
      count_country <- cw_locs %>% distinct(nid, year_start, year_end, ihme_loc_id, country_name, region_id)
    } else if(strategy_detail == FALSE) {
      count_country <- cw_locs %>% distinct(nid, year_start, year_end, country_name, region_id)      
    }
    count_country <- count_country %>% 
        group_by(region_id, country_name) %>% 
        tally() %>%
        left_join(location_data %>% filter(location_type =="admin0") %>% 
                    select(location_name, location_id), by = c('country_name'='location_name'))
      
    # Unique NID-GBD-Region_year_start combinations # 
      #detail <- ifelse(strategy_detail == TRUE, 'ihme_loc_id', NULL)
    if(strategy_detail == TRUE){
      gbd_region <- cw_locs %>% distinct(nid, year_start, year_end, ihme_loc_id, region_id, region_name)
    } else if(strategy_detail == FALSE) {
      gbd_region <- cw_locs %>% distinct(nid, year_start, year_end, region_id, region_name)      
    }
      gbd_region <- gbd_region %>% 
        group_by(region_id, region_name) %>% 
        tally() 
    
    # Unique GBD region and country counts #
    dt_g <- gbd_region %>% dplyr::rename('location_name' = 'region_name') %>% mutate(hierarchy_key = 'gbd_region')
    dt_c <- count_country %>% dplyr::rename('location_name' = 'country_name') %>% select(-location_id) %>% mutate(hierarchy_key = 'gbd_country')
    country_gbd_regions <- rbind(dt_g, dt_c) %>% setorder('region_id', -'hierarchy_key', 'location_name')
    
    # Create new xlsx workbook and add source counts (separate sheets for country and most granular counts)
    #out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, ".xlsx")
    if(is.null(cw_id) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_cw_version_", cw_id, "_counts_unadj.xlsx")
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_unadj.xlsx")
    }
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "Counts_All Locations_Unadj")
    writeData(wb, "Counts_All Locations_Unadj", count_loc)
    
    addWorksheet(wb, "counts_by_country")
    writeData(wb, "counts_by_country", count_country)
    
    addWorksheet(wb, "counts_by_gbd_region")
    writeData(wb, "counts_by_gbd_region", gbd_region)
    
    addWorksheet(wb, "counts_by_gbd_region_country")
    writeData(wb, "counts_by_gbd_region_country", country_gbd_regions)
    
    saveWorkbook(wb, paste0(unmerged_path, "/", out_filename), overwrite = TRUE)
  }
}
#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################
#' Title
#'
#' @param cw_version 
#' @param cw_id 
#' @param source_type 
#' @param final_counts 
#' @param custom_subset_column_names 
#' @param custom_subset_column_values 
#' @param unmerged_path 
#'
#' @return
#' @export
#'
#' @examples
#' 
merged_nid_count_adjustment <- function(cw_version, cw_id, source_type = NULL, final_counts, custom_subset_column_names = NULL, custom_subset_column_values = NULL, clinical_data_only = FALSE, nonclinical_data_only = FALSE, strategy_detail = FALSE, unmerged_path){
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  if(!file.exists(paste0(unmerged_path,"/", ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_cw_version_", cw_id, "_counts_unadj.xlsx")) && !file.exists(paste0(unmerged_path,"/", ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx")) || !file.exists(paste0(unmerged_path,"/", "merged_citation_component_records.xlsx"))){
    if(is.null(cw_id) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_counts_cw_version_", cw_id, "_counts_adj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n_adj = 0, hierarchy_key = NA)
      #write.xlsx(empty_table, paste0(final_counts, "/", out_filename))
      message(cat(paste0("No ", source_type, " NIDs for cw_id ", cw_id,".An appropriate adjusted table with zero counts has been generated.")))
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_adj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n_adj = 0, hierarchy_key = NA)
      #write.xlsx(empty_table, paste0(final_counts, "/", out_filename))
      message(cat(paste0("No NIDs for Custom table. An appropriate adjusted table with zero counts has been generated.")))
      }
    } else {
      # The following lines of code are not necessary, as the tables don't processed anywhere else this function. The data wrangling and
      # unique counts that takes place in the unmerged_nid_count_unadjusted functions gets repeated in this function, in addition to the
      # merged nid adjustments.
      # The following lines of code were included, as they were included in the original code!
      if(is.null(cw_id) == FALSE) {
        #print(source_type)
        source_type_nids <- read.xlsx(paste0(unmerged_path, "/",cw_id, "_",ifelse(is.null(source_type) == FALSE, source_type,""), "_merge_types.xlsx"))
        unadj_counts_subnat <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_cw_version_", cw_id, "_counts_unadj.xlsx"), sheet = 1) 
        unadj_counts_country <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_cw_version_", cw_id, "_counts_unadj.xlsx"), sheet = 2) 
        # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
        # subset_cw_version_19694_counts_unadj
        # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
        merged_comp_list <- read.xlsx(paste0(unmerged_path, "/","merged_citation_component_records.xlsx")) %>%
          group_by(Merged.Citation.NID) %>%
          tally() %>%
          dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
      } else {
        unadj_counts_subnat <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 1) 
        unadj_counts_country <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 2) 
        unadj_counts_region <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 3) 
        unadj_counts_country_region <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 4) 
        # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
        source_type_nids <- read.xlsx(paste0(unmerged_path, "/","Custom_Table",ifelse(is.null(source_type) == FALSE, source_type,""),"_merge_types.xlsx"))
        
        # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
        merged_comp_list <- read.xlsx(paste0(unmerged_path, "/","merged_citation_component_records.xlsx")) %>%
          group_by(Merged.Citation.NID) %>%
          tally() %>%
          dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
      }
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>% 
      filter(!is.na(n_comp_nids))
    # Format crosswalk version data to prep for source counting 
    if(is.null(source_type) == FALSE) {
      if (grepl("claims", source_type, fixed = TRUE) == TRUE) {
        source_type_t <- data.table(source_type)
        cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (grepl("inpatient", source_type, fixed = TRUE) == TRUE) {
      source_type_t <- data.table(source_type)
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "incidence") {
      cw_locs <- cw_version %>% 
        filter(is.na(clinical_data_type) & measure == "incidence") %>% 
        left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% 
        # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only
        # "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "prevalence") {
      cw_locs <- cw_version %>% 
        filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
        left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% 
        # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only
        # "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    }
    } else if(is.null(custom_subset_column_names) == FALSE && is.null(custom_subset_column_values) == FALSE){
        subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
        subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
        subset_table <- select(subset_table, -custom_subset_column_values)
        subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
        subset_table <- as.data.table(subset_table) #
        cw_locs <- cw_version %>% left_join(location_data %>% 
                                              select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
          inner_join(subset_table, by = custom_subset_column_names) %>% 
          mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
          left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
          dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(is.null(custom_subset_column_names) == TRUE && is.null(custom_subset_column_values) == TRUE && clinical_data_only == FALSE && nonclinical_data_only == FALSE && is.null(source_type) == TRUE) {
      #subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      #subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      #subset_table <- select(subset_table, -custom_subset_column_values)
      #subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
      print('total')
      cw_locs <- cw_version %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(nrow(cw_locs))
    } else if(clinical_data_only == TRUE){
      print('test clinical')
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(!clinical_data_type %in% NA,!clinical_data_type %in% "literature") %>% #for maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(nrow(cw_locs))
    } else if(nonclinical_data_only == TRUE) {
      print('test nonclinical')
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% NA | clinical_data_type %in% "literature") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(nrow(cw_locs))
      } else {
        message("Invalid source_type argument.")
        message("OR the subset inputted for processing is not valid. Please review the arguments for this function!")
      } 
    write.xlsx(cw_locs, "/ihme/homes/chrish47/dt_after_filter_merged.xlsx")
    
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs #
    cw_locs <- transform(cw_locs, nid = as.numeric(nid)) # These are important to avoid incompability issues! ~ chrish47
    key <- transform(key, NID = as.numeric(NID)) # These are important to avoid incompability issues! ~ chrish47
    # 
    count_loc <- cw_locs %>% distinct(nid, year_start, year_end, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>% # This is the line with the incompatible types problems ~ chrish47
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    
    # Unique NID-country-year_start combinations [country-level counts] accounting for component NIDs #
    #detail <- ifelse(strategy_detail == TRUE, 'ihme_loc_id', NULL)
    if(strategy_detail == TRUE){
      count_country <- cw_locs %>% distinct(nid, year_start, year_end, ihme_loc_id, country_name, region_id)
    } else if(strategy_detail == FALSE) {
      count_country <- cw_locs %>% distinct(nid, year_start, year_end, country_name, region_id)      
    }
    count_country <- count_country %>% #For testing purposes exclude/inclide region_id!
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(country_name, region_id) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(country_name, region_id) %>%
      distinct(region_id, country_name, n_adj) %>% 
      left_join(location_data %>% filter(location_type =="admin0") %>% 
                  select(location_name, location_id), by = c('country_name'='location_name'))
    #location_data was subsetted to only include admin0(country) locations. This was necessary in order to avoid duplicates.
    # Ex: Georgia(US state) vs. Georgia(Country) is an issues if doesn't get properly filtered to admin0 or countries only.
    
    # Unique NID-GBD-Region combinations [GBD_region_name] accounting for component NIDs #
    #detail <- ifelse(strategy_detail == TRUE, 'ihme_loc_id', NULL)
    if(strategy_detail == TRUE){
      gbd_region_counts <- cw_locs %>% distinct(nid, year_start, year_end, ihme_loc_id, region_id, region_name)
    } else if(strategy_detail == FALSE) {
      gbd_region_counts <- cw_locs %>% distinct(nid, year_start, year_end, region_id, region_name)      
    }
    gbd_region_counts <- gbd_region_counts %>% 
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(region_id, region_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(region_id, region_name) %>%
      distinct(region_id, region_name, n_adj)
    
    # Lanscaping table, gbd_region and country, ascending by region_id and location_name and descending by hierarchy_key # 
    dt_g <- gbd_region_counts %>% dplyr::rename('location_name' = 'region_name') %>% mutate(hierarchy_key = 'gbd_region')
    dt_c <- count_country %>% dplyr::rename('location_name' = 'country_name') %>% select(-location_id) %>% mutate(hierarchy_key = 'gbd_country')
    country_gbd_regions <- rbind(dt_g, dt_c) %>% setorder('region_id', -'hierarchy_key', 'location_name')
    
    # Create xlsx file with sheets for most granular and country-level source counts. 
    #out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, "_merged_adj.xlsx")
    if(is.null(cw_id) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_counts_cw_version_", cw_id, "_counts_adj.xlsx")
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_adj.xlsx")
    }
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "Counts_All Locations_Adj")
    writeData(wb, "Counts_All Locations_Adj", count_loc)
    
    addWorksheet(wb, "Counts_Country_Adj")
    writeData(wb, "Counts_Country_Adj", count_country)
    
    addWorksheet(wb, "Counts_GBD_Region_Adj")
    writeData(wb, "Counts_GBD_Region_Adj", gbd_region_counts)
    
    addWorksheet(wb, "Counts_GBD_Region_Country_Adj")
    writeData(wb, "Counts_GBD_Region_Country_Adj", country_gbd_regions)
    
    saveWorkbook(wb, paste0(final_counts, "/", out_filename), overwrite = TRUE)
  }  
}
#############################################################################################################################
#############################################################################################################################
# ### Attempts to ordering the gbd_region and country-level data.
# dt_alp <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/testing/source_counts/final_counts/subset_counts_cw_version_19694_counts_adj.xlsx", sheet = 4) %>% group_by(region_id) %>% arrange(desc(hierarchy_key), location_name, .by_group = TRUE)
# 
# dt_alp <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/testing/source_counts/final_counts/subset_counts_cw_version_19694_counts_adj.xlsx", sheet = 4) %>% group_by(region_id) %>% arrange(desc(hierarchy_key), location_name, .by_group = TRUE)
# 
# data_id1 <- transform(dt_alp, ID = as.numeric(factor(region_id, hierarchy_key)))
