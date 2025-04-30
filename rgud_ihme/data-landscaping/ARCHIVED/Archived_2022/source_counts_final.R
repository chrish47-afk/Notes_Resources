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


user <- Sys.getenv("USER")

### Destinations
out_path <- "/ihme/scratch/users/chrish47/source_counts/old/"

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))


############################################################
## GLOBAL OBJECTS - For Source Counts!
############################################################
gbd_rnd <- 6
gbd_decomp_step <- "step5"
#compare_version <- 7244

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)

## Crosswalk Version Ids for Testing
# appendicitis (xv: 26876), gallbladder and biliary diseases (xv: 32579)
# appen <- get_crosswalk_version(26876)
# biliary <- get_crosswalk_version(32579)

## Maternal Crosswalk Verions
Maternal <- get_crosswalk_version(19694)

############################################################
## SOURCE COUNTS
############################################################
# Please review ("H:\upper-digestive\PUD_GD_paper_GBD2019\source_counts_doc_cc.docx") for further details on CC source counts!

### WE CAN ONLY COUNT 'SOURCES' THAT MEET THE FOLLOWING CRITERIA: ############################
### We can only count ‘sources’ that meet the following criteria: ############################
# • It is a GBD project with data that comes through our sources data ingestion pipeline. We don't count custom data outside our sources database.
# • The source data corresponds to valid NIDs that are published in the GHDx. Our source counts are all based on GHDx Record NIDs.
# • The source counts requested can be fulfilled with the data we have in our sources database / source counts tables.

### General Counting Strategy Overview ############################
# (1) NID-country-year

# • Means we will count every NID + country + year_start + year_end  combination for each NID that is reported by a research team. 

# (2) NID Relationships

# • If a Record NID has an underlying NID, the underlying NID is used. [Ask Modelers If their pipeline have these?]
# • If a record NID or replaced underlying NID is a Merged Citation, we break apart the Merged Citation into its associated Record NIDs.

# (3) NID’s Data Type

# • Every GHDx Record NID has a data type (it is required).
# • In some counting strategies, records are counted differently depending on data type. 

# (4) NID Validity 
# • NIDs must be valid, published, Record NIDs from the GHDx. We do not count “file” NIDs, “series or system” NIDs, aggregate NIDs, or any fake NIDs. 

#### PART 1 ############################### Generates unadjusted source counts by pipelien and cliente_data_type, but ultimate purpose is to pull unique NIDs!
# (1) Creating the adequate directories for source counts and sourcing the cw_version(crosswalk version) data table for data wrangling. Data wrangling includes:
#     • Subsetting data table for each source type or data type. This includes every unique type in the clinical_data_type column, as this column distinguishes different            clinical data_type and OTHER data.
#     • Restructuring the data table with the adequate ihme_loc_id and country names. The output will have both subnational and country level counts.
# (2) Grouping by Unique NID-location-year_start combinations
#     • A unique NID list by source_type or client_data_type will be provided to the user to use for the second part.
# (3) Unadjusted files get generated - These files will contain source counts, not including merged citations(nids).
#### PART 2 ############################### Generates the adjusted source counts by pipeline and client_data_type!
# (1) Using the unique NID list from PART 1, you need to use: 
#     • (http://internal-ghdx.healthdata.org/search-by-nids-all-types/) & (http://internal-ghdx.healthdata.org/merged-citations-components) to pull merged citations
# (3) Simialr to PART 1, the same data wrangling is applied to generate the source counts.
# (4) Adjusted files get generated - These files will contain all nonfatal source counts, including merged citations(nids).

# =========================================================================================================================================================================
## NONFATAL SOURCE COUNTING STRATEGY, PART 1      
## Nonfatal source counts function - MUST BE RUN ALONG WITH merged_nid_count_adjustment() TO ACCOUNT FOR MERGED NIDS!!! See note below. 
## Note: source counts generated at the "country" level here represent counts of
## all sources from a given country at any location level, NOT just
## nationally-representative sources (ie. if a crosswalk version's only Chinese
## sources were 1 NID from Beijing and 1 from China, with one year_start each,
## this would be counted as two "country-level" sources for China, which is
## distinct from counting 1 source at the national level.)
# ==========================================================================================================================================================================

source_counts <- function(cw_id, source_type){
  # Dir creation, instead of doing it manually!
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
  if(file.exists(paste0(counts_path, '/merged_nids_adjustment'))){
    merged_path <- paste0(counts_path, '/merged_nids_adjustment')
  } else {
    dir.create(paste0(counts_path, '/merged_nids_adjustment'))
    merged_path <- paste0(counts_path, '/merged_nids_adjustment')
  }
  # Get crosswalk version if it hasn't already been written to flat file
  # As it stand right now, this only process get_crosswalk_version. Check with RGUD what the inputs are going to be!
  if (file.exists(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))) {
    cw_version <- read.xlsx(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  } else {
    cw_version <- get_crosswalk_version(cw_id)
    write.xlsx(cw_version, paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  }
  
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  write.xlsx(cw_version, "/ihme/homes/chrish47/cw_version.xlsx")
  # Format crosswalk version data based on source type; attribute all data to its corresponding country. 
  # The following code commits Data Wrangling - Thank you Helen! 
  # Important points to address: (1) Every bundle is different, so the below code will only work if it contains the required column criteria. Important to consider. Additionally, important to discuss the measure type included in the cw_locs. As this will differ for every elmo_id and/or cause. (2) The following data wrangling, creates an important column(country_id), which is constructed by location_id and path_to_top_parent in addition to the location_id!
  if (source_type == "claims") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "claims") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x) #@ added dplyr:: to every rename call
    write.xlsx(cw_locs, "/ihme/homes/chrish47/cw_locs.xlsx")
  } else if (source_type == "inpatient") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "inpatient") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
  } else if (source_type == "prev") {
    cw_locs <- cw_version %>% 
      filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
      left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
      mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
      filter(!country_id=="OTHER") %>%
      mutate(country_id = as.integer(country_id)) %>%
      left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
    # Review the above filter(!country_id == "OTHER") for further details on why these were filtered out, connect to the above comment on NID: 416752
  } else {
    message("Invalid source_type argument.")
  } 
  
  # Check to see if there are any sources to count for the given source type
  if (nrow(cw_locs) == 0) {
    message(cat(paste0("cw_id ", cw_id, ", ", source_type), ": no sources of this data type."))
  } else {
    
    # Unique NID-location-year_start combinations [most granular locs]
    count_loc <- cw_locs %>% dplyr::distinct(nid, year_start, location_name) %>%
      group_by(location_name) %>% 
      dplyr::tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # Only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    count_loc_merged <- cw_locs %>% 
      dplyr::distinct(nid, year_start, location_name) %>%
      dplyr::pull(nid) %>%
      unique() %>% paste0(collapse=",")
    message(cat(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged)
    # Unique NID-country-year_start combinations [country-level location counts]
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      group_by(country_name) %>% 
      tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = c('country_name' = 'location_name'))
    
    # Create new xlsx workbook and add source counts (separate sheets for country and most granular counts)
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, ".xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_name")
    writeData(wb, "counts_by_loc_name", count_loc)
    
    addWorksheet(wb, "counts_by_country")
    writeData(wb, "counts_by_country", count_country)
    
    saveWorkbook(wb, paste0(counts_path, "/", out_filename), overwrite = TRUE)
  }
}

# Run source counts function for PUD and GD      
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c('prev', 'inpatient', 'claims')){
    source_counts(id, type)
  }
}
for(id in c(12221)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c('prev')){
    source_counts(id, type)
  }
}

# ========================================================================================================================================================================
## NONFATAL SOURCE COUNTING STRATEGY, PART 2      
## NOTE: the above source counting strategy does NOT account for merged NIDs - each component NID needs to be counted individually. 
# Strategy to add counts for component NIDs of merged NIDs:
# 1. Enter 'count_loc_merged' NID list (from output message of source_counts() function) into GHDx tool: http://internal-ghdx.healthdata.org/search-by-nids-all-types/
# 2. Download list of NIDs and source types (see /ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/source_counts/merged_NIDs_adjustment)
# 3. Download list of all merged NIDs and their component NIDs from GHDx: http://internal-ghdx.healthdata.org/merged-citations-components (also at /merged_nids_adjustment path from above)
# 4. Run merged_nid_count_adjustment(). 
# ========================================================================================================================================================================

merged_nid_count_adjustment <- function(cw_id, source_type){
  counts_path <- paste0(out_path, "source_counts/")
  
  # Get crosswalk version if it hasn't already been written to flat file
  if (file.exists(paste0(counts_path, "cw_version_", cw_id, ".xlsx"))) {
    cw_version <- read.xlsx(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  } else {
    cw_version <- get_crosswalk_version(cw_id)
    write.xlsx(cw_version, paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  }
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  
  # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
  merged_comp_list <- read.csv(paste0(counts_path, "merged_nids_adjustment/merged_citation_component_records.csv")) %>% #@ Change the path accordingly.
    group_by(Merged.Citation.NID) %>%
    tally() %>%
    dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
  
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  if(!file.exists(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"))){
    message(paste0("No ", source_type, " NIDs for cw_id ", cw_id))
  } else {
    
    unadj_counts_subnat <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 1) 
    unadj_counts_country <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 2) 
    
    # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
    source_type_nids <- read.csv(paste0(counts_path, "merged_nids_adjustment/", ifelse(cw_id==12221, "pud", "gd"), "_", source_type, "_NID_merge_types.csv")) #@ This will need to be changed, as file names will differ
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>% 
      filter(!is.na(n_comp_nids))
    # Format crosswalk version data to prep for source counting 
    if (source_type == "claims") {
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "claims") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "inpatient") {
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "prev") {
      cw_locs <- cw_version %>% 
        filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
        left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else {message("Invalid source_type argument.")} 
    
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs
    cw_locs <- transform(cw_locs, nid = as.numeric(nid)) #@ These are important to avoid incompability issues!
    key <- transform(key, NID = as.numeric(NID)) #@ These are important to avoid incompability issues!
    count_loc <- cw_locs %>% distinct(nid, year_start, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>% #@ This is the line with the incompatible types problems!!!!! 
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    # Unique NID-country-year_start combinations [country-level counts] accounting for component NIDs
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(country_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(country_name) %>%
      distinct(country_name, n_adj) %>% 
      left_join(location_data %>% select(location_name, location_id), by = c('country_name'='location_name'))
    
    # Create xlsx file with sheets for most granular and country-level source counts. 
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, "_merged_adj.xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_adj")
    writeData(wb, "counts_by_loc_adj", count_loc)
    
    addWorksheet(wb, "counts_by_country_adj")
    writeData(wb, "counts_by_country_adj", count_country)
    
    saveWorkbook(wb, paste0(counts_path, "/", out_filename), overwrite = TRUE)
  }  
}

# Run source counts adjustment for PUD and GD
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c("prev", "inpatient", "claims")){
    merged_nid_count_adjustment(id, type)
  }
}

# 12221 = PUD or Peptic Ulcer Disease
# 9890 = GD or Gastritis/Duodenitis

#### Comparing cc_counts and PUD counts ##########################################################################
claims_cc <- read.csv('/mnt/share/scratch/users/chrish47/source_counts/counts_from_CC/GBD 2019 custom PUD prevalence subnational-level counts for claims NIDs.csv')
inpatient_cc <- read.csv('/mnt/share/scratch/users/chrish47/source_counts/counts_from_CC/GBD 2019 custom PUD prevalence subnational-level counts for inpatient NIDs.csv')
other_cc <- read.csv('/mnt/share/scratch/users/chrish47/source_counts/counts_from_CC/GBD 2019 custom PUD prevalence subnational-level counts for other NIDs.csv')
#
claims_source_counts <- read.xlsx('/mnt/share/scratch/users/chrish47/source_counts/final_counts/claims_counts_cw_version_12221_merged_adj.xlsx')
inpatient_source_counts <- read.xlsx('/mnt/share/scratch/users/chrish47/source_counts/final_counts/inpatient_counts_cw_version_12221_merged_adj.xlsx')
other_source_counts <- read.xlsx('/mnt/share/scratch/users/chrish47/source_counts/final_counts/prevalence_counts_cw_version_12221_merged_adj.xlsx')
#
claims_compare <- merge(claims_cc, claims_source_counts, by = 'location_id') %>% select(1:3,7:8)
claims_compare$diff <- (claims_compare$source_count - claims_compare$n_adj)
inpatient_compare <- merge(inpatient_cc, inpatient_source_counts, by = 'location_id')
inpatient_compare$diff <- (inpatient_compare$source_count - inpatient_compare$n_adj)
other_compare <- merge(other_cc, other_source_counts, by = 'location_id')
other_compare$diff <- (other_compare$source_count - other_compare$n_adj)

write.xlsx(claims_compare, '/mnt/share/scratch/users/chrish47/source_counts/claims_compare.xlsx')
write.xlsx(inpatient_compare, '/mnt/share/scratch/users/chrish47/source_counts/inpatient_compare.xlsx')
write.xlsx(other_compare, '/mnt/share/scratch/users/chrish47/source_counts/other_compare.xlsx')

#################################################################################################################

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
# The above code works and outputs relatively accurate source counts for PUD and GD.
# Next steps:
# (1) Test the above code/functions with other pipelines or causes(set of crosswalk_version_id's)
# (2) The above code depends on one important manual step. The user must manually pull and download csv reports from the GHDx, based on
# NIDs within the crosswalk_version_id data object.
# • Objective for this: Automate this process by directly pulling the necessary data from the GHDx db or establish an appropriate/accurate
# connection the GHDx API. [IN PROGRESS]
# (3) Continue testing and vetting Source counts Requirements on Code by cause/pipeline!
# • Review this for further documentation on Source Counts: "H:\upper-digestive\PUD_GD_paper_GBD2019\source_counts_doc_cc.docx"

# Side note: I need to add/commit/push this. Decide on the best dir where to do this! - Right now it lives in the upper digestive dir!
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
## Query Workbench

# mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
# table_list <- as.data.frame(dbListTables(mydb))
# 
# #lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
# 
# # Target tables: node & field_data_field_merged_records (Confirm with infra how the field_revision_field_merged_records table differs!)
# # Sourcing in the node table for (nid, title, type)
# query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
# query_data <- query_data %>% dplyr::rename('NID' = 'nid', 'Title' = 'title', 'Content Type' = 'type') 
# query_data <- query_data %>% transform(NID = as.numeric(NID))
# # Sourcing in the merge table for (Component and Merged ids or nids)
# query_data_merged <- dbGetQuery(mydb, "SELECT * FROM field_data_field_merged_records;")
# query_data_merged <- query_data_merged %>% dplyr::rename('Component NID' = 'field_merged_records_target_id', 'Merged Citation NID' = 'entity_id')
# query_data_merged <- query_data_merged %>% transform(NID = as.numeric(NID))
# 
# query_data_subset <- query_data %>% subset(NID == dput(count_loc_merged))
# 
# count_loc_merged <- cw_locs %>% 
#   dplyr::distinct(nid, year_start, location_name) %>%
#   dplyr::pull(nid) %>%
#   unique()

# nids <- as.numeric(count_loc_merged)
# nids_numeric <- dput(as.character(count_loc_merged))
#   
# test_nids <- paste0('c("', paste(nids, collapse="\", \""), '")')
# nids <- as.numeric(nids)
# 
# store <- paste0('"', paste(nids, collapse="\", \""), '"')

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
## Mimicking what the GHDx tools do! In order to pull the merged NIDs. TESTING PURPOSES -- I want to create a HELPER FUNCTION from this!
nids <- c(3822,
          431674,
          397812,
          397813,
          397814,
          234188,
          234187,
          234186,
          234193,
          234192,
          234191,
          234190,
          234189,
          191178,
          191177,
          191176,
          191175,
          191174,
          234184)

ghdx_search_by_nid <- function(nid = NULL, full = FALSE){ #http://internal-ghdx.healthdata.org/search-by-nids-all-types/
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  #suppressWarnings(...) The above line produces a MySQL warning message. Deciding to suppress or not? Bad/Good?
  query_data <- query_data %>% dplyr::rename('NID' = 'nid', 'Title' = 'title', 'Content Type' = 'type') 
  query_data <- query_data %>% transform(NID = as.numeric(NID))
  #Filter table with only the inputted NIDs
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data, NID %in% nid_t$nid)
    return(subset) 
  } else if (full == TRUE){
    return(query_data)
  }
  #Disconnecting from database OR else you will get a max 16 connections error
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

dt_1 <- ghdx_search_by_nid(full = TRUE)
dt_1 <- ghdx_search_by_nid(nids)

#http://internal-ghdx.healthdata.org/merged-citations-components (also at /merged_nids_adjustment path from above
ghdx_merged_citation_component <- function(nid, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data_merged <- dbGetQuery(mydb, "SELECT field_merged_records_target_id, entity_id FROM field_data_field_merged_records;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  # Merging to get Title
  query_data_merged <- query_data_merged %>% dplyr::rename('nid' = 'field_merged_records_target_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid')
  query_data_merged <- query_data_merged %>% dplyr::select(nid, title, entity_id)
  # Subset
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data_merged, entity_id %in% nid_t$nid)
    subset <- subset %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(subset)
  } else if(full == TRUE) {
    query_data_merged <- query_data_merged %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(query_data_merged)
  }
  #Changing column names
  #subset <- subset %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
  #query_data_merged <- query_data_merged %>% transform(NID = as.numeric(NID))
  #return(subset)
  #Disconnecting from database OR else you will get a max 16 connections error
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

dt_2 <- ghdx_merged_citation_component(nids, full = TRUE)
dt_2 <- ghdx_merged_citation_component(nids)

# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================
source_counts <- function(cw_id, source_type, out_path = "/ihme/scratch/users/chrish47/"){
  # Dir creation, instead of doing it manually!
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
  # As it stand right now, this only process get_crosswalk_version. Check with RGUD what the inputs are going to be!
  if (file.exists(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))) {
    cw_version <- read.xlsx(paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  } else {
    cw_version <- get_crosswalk_version(cw_id)
    write.xlsx(cw_version, paste0(counts_path, "/cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA #Converting all empty cells to NA. #verify if this needs to be string NA value
  }
  
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  
  # Unmerged Results
  unmerged(cw_version = cw_version, cw_id = cw_id, source_type = source_type, unmerged_path = unmerged_path)
  
  # Merged Results
  merged_nid_count_adjustment(cw_version = cw_version, cw_id = cw_id, source_type = source_type, final_counts = final_counts, unmerged_path = unmerged_path)
}

# Run source counts function for PUD and GD      
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c('prev', 'inpatient', 'claims')){
    source_counts(id, type)
  }
}
for(id in c(12221)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c('prev')){
    source_counts(id, type)
  }
}

# ========================================================================================================================================================================
## NONFATAL SOURCE COUNTING STRATEGY, PART 2      
## NOTE: the above source counting strategy does NOT account for merged NIDs - each component NID needs to be counted individually. 
# Strategy to add counts for component NIDs of merged NIDs:
# 1. Enter 'count_loc_merged' NID list (from output message of source_counts() function) into GHDx tool: http://internal-ghdx.healthdata.org/search-by-nids-all-types/
# 2. Download list of NIDs and source types (see /ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/source_counts/merged_NIDs_adjustment)
# 3. Download list of all merged NIDs and their component NIDs from GHDx: http://internal-ghdx.healthdata.org/merged-citations-components (also at /merged_nids_adjustment path from above)
# 4. Run merged_nid_count_adjustment(). 
# ========================================================================================================================================================================

merged_nid_count_adjustment <- function(cw_version, cw_id, source_type, final_counts, unmerged_path){
  #counts_path <- paste0(out_path, "source_counts/")
  
  # Get crosswalk version if it hasn't already been written to flat file
  # if (!"ihme_loc_id" %in% names(cw_version)) {
  #   cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  # }
  
  # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
  print(1)
  merged_comp_list <- read.xlsx(paste0(unmerged_path, "/","merged_citation_component_records.xlsx")) %>% #@ Change the path accordingly.
    group_by(Merged.Citation.NID) %>%
    tally() %>%
    dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
  
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  print(paste0(unmerged_path,"/", source_type, "_counts_cw_version_", cw_id, ".xlsx"))
  if(!file.exists(paste0(unmerged_path,"/", source_type, "_counts_cw_version_", cw_id, ".xlsx"))){
    print(paste0(unmerged_path,"/", source_type, "_counts_cw_version_", cw_id, ".xlsx"))
    message(paste0("No ", source_type, " NIDs for cw_id ", cw_id))
  } else {
    
    unadj_counts_subnat <- read.xlsx(paste0(unmerged_path, "/",source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 1) 
    unadj_counts_country <- read.xlsx(paste0(unmerged_path, "/",source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 2) 
    
    # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
    source_type_nids <- read.xlsx(paste0(unmerged_path, "/",cw_id, "_", source_type, "_merge_types.xlsx")) #@ This will need to be changed, as file names will differ
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>% 
      filter(!is.na(n_comp_nids))
    # Format crosswalk version data to prep for source counting 
    print(2)
    if (source_type == "claims") {
      cw_locs <- cw_version %>%
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "claims") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "inpatient") {
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "prev") {
      cw_locs <- cw_version %>% 
        filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
        left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else {message("Invalid source_type argument.")} 
    print(3)
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs
    cw_locs <- transform(cw_locs, nid = as.numeric(nid)) #@ These are important to avoid incompability issues!
    key <- transform(key, NID = as.numeric(NID)) #@ These are important to avoid incompability issues!
    count_loc <- cw_locs %>% distinct(nid, year_start, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>% #@ This is the line with the incompatible types problems!!!!! 
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    # Unique NID-country-year_start combinations [country-level counts] accounting for component NIDs
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(country_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(country_name) %>%
      distinct(country_name, n_adj) %>% 
      left_join(location_data %>% select(location_name, location_id), by = c('country_name'='location_name'))
    
    # Create xlsx file with sheets for most granular and country-level source counts. 
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, "_merged_adj.xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_adj")
    writeData(wb, "counts_by_loc_adj", count_loc)
    
    addWorksheet(wb, "counts_by_country_adj")
    writeData(wb, "counts_by_country_adj", count_country)
    print(final_counts)
    saveWorkbook(wb, paste0(final_counts, "/", out_filename), overwrite = TRUE)
  }  
}

# Run source counts adjustment for PUD and GD
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c("prev", "inpatient", "claims")){
    merged_nid_count_adjustment(id, type)
  }
}

##################### Splitting Functions to a Main Function and Two separate functions(Merged and Unmerged). Plus the database helper function!
################################################################################################################################################
unmerged <- function(cw_version, cw_id, source_type, unmerged_path){
  if (source_type == "claims") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "claims") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x) #@ added dplyr:: to every rename call
    write.xlsx(cw_locs, "/ihme/homes/chrish47/cw_locs.xlsx")
  } else if (source_type == "inpatient") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "inpatient") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
  } else if (source_type == "prev") {
    cw_locs <- cw_version %>% 
      filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
      left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
      mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
      filter(!country_id=="OTHER") %>%
      mutate(country_id = as.integer(country_id)) %>%
      left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
    # Review the above filter(!country_id == "OTHER") for further details on why these were filtered out, connect to the above comment on NID: 416752
  } else {
    message("Invalid source_type argument.")
  } 
  
  # Check to see if there are any sources to count for the given source type
  if (nrow(cw_locs) == 0) {
    message(cat(paste0("cw_id ", cw_id, ", ", source_type), ": no sources of this data type."))
  } else {
    
    # Unique NID-location-year_start combinations [most granular locs]
    count_loc <- cw_locs %>% dplyr::distinct(nid, year_start, location_name) %>%
      group_by(location_name) %>% 
      dplyr::tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # Only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    count_loc_merged <- cw_locs %>% 
      dplyr::distinct(nid, year_start, location_name) %>%
      dplyr::pull(nid) %>%
      unique()
    count_loc_merged_message <- count_loc_merged %>% paste0(collapse=",")
    message(cat(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
    
    # Sourcing GHDx database and creating appropriate files with merged data
    dt_merged_types <- ghdx_search_by_nid(nid = count_loc_merged)
    dt_component_records <- ghdx_merged_citation_component(full = TRUE)
    
    file_extension_merged_types <- paste0(unmerged_path,"/",cw_id,"_",source_type,"_merge_types.xlsx")
    file_extension_merged_component_records <- paste0(unmerged_path,"/", "merged_citation_component_records.xlsx")
    
    write.xlsx(dt_merged_types, file_extension_merged_types)
    write.xlsx(dt_component_records, file_extension_merged_component_records)
    
    # Unique NID-country-year_start combinations [country-level location counts]
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      group_by(country_name) %>% 
      tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = c('country_name' = 'location_name'))
    
    # Create new xlsx workbook and add source counts (separate sheets for country and most granular counts)
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, ".xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_name")
    writeData(wb, "counts_by_loc_name", count_loc)
    
    addWorksheet(wb, "counts_by_country")
    writeData(wb, "counts_by_country", count_country)
    
    saveWorkbook(wb, paste0(unmerged_path, "/", out_filename), overwrite = TRUE)
  }
}
#####
#####
merged <- function(){
  
}














