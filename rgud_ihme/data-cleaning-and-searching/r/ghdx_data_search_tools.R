##################################
# Author: Christian Hernandez
# Date: July, 2022+
# Purpose: These GHDx data search functions allow you to pull/extract data directly from the GHDx db accordingly. 
##################################
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
library(stringr)
library(openxlsx)
library(RMySQL)
library(dplyr)

####################################################################################################################################
# IMPORTANT NOTE:
# If for some reason you get a 16max connection error. Run the following line of code to clear your db connections:
#disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Arguments - For All Functions Below -----------------------------------------------------
# nid: Please enter a numeric value here. You can enter a single or multiple numeric values.
# full: Please enter a logical value here. The default value is FALSE. If you want a complete list of all NIDs, please enter 'full = TRUE'
# -----------------------------------------------------------------------------------------
####################################################################################################################################

# This function imitates this existing GHDx tool: http://internal-ghdx.healthdata.org/search-by-nids-all-types/
ghdx_search_by_nid <- function(nid = NULL, full = FALSE){
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

# This function imitates this existing GHDx tool: http://internal-ghdx.healthdata.org/merged-citations-components
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

# This functions pulls in Data Type Values for NID inputs, something similar to one of the internal tools in the GHDx.
ghdx_search_nid_data_type <- function(nid = NULL, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "
  SELECT c.type, c.nid AS NID, c.title AS Title, ftype.name AS Data_Type     
  FROM ghdx.node AS c 
  LEFT JOIN ghdx.field_data_field_type ft ON ft.entity_id = c.nid 
  LEFT JOIN ghdx.taxonomy_term_data AS ftype ON ftype.tid = ft.field_type_tid
  WHERE c.type = 'record' OR c.type = 'merged_citation'
  ORDER BY c.nid;")
  #query_data <- query_data %>% dplyr::rename('Data Type(Detailed)' = 'Data_Type') 
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

# Pulling GHDx year data for all NID inputs(Merged, Component, etc.)
# Example use: The following function was used to accurately depict the year spread in the Maternal Data Landscaping Plots.
ghdx_search_nid_time_period <- function(nid = NULL, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "SELECT entity_id, field_time_value, field_time_value2 FROM field_data_field_time;")
  query_data <- query_data %>% dplyr::rename('NID' = 'entity_id', 'time_start' = 'field_time_value', 'time_end' = 'field_time_value2') 
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

# Function pulls all child (underlying) NIDs for a given parent NID,
# or if all = TRUE pulls all child NIDs in the GHDx.
ghdx_get_underlying_NIDs <- function(nid_list, all = FALSE){

  # Sourcing GHDx database
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data_merged <- dbGetQuery(mydb, "SELECT entity_type, entity_id, field_references_node_target_id FROM field_data_field_references_node;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  
  #Disconnecting from SQL database
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  # Merging to get Title
  query_data_merged <- query_data_merged %>% dplyr::rename('nid' = 'field_references_node_target_id',
                                                           'parent_nid' = 'entity_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid') %>% 
    dplyr::select(nid, title, parent_nid)
  
  if (all == FALSE) {
    query_data_merged <- query_data_merged %>%
      filter(parent_nid %in% nid_list)
    return(query_data_merged)
  }
  else {
    return(query_data_merged)
  }
}

####################################################################################################################################
# Example call
#NID <- c(124802,125782)
#table <- ghdx_search_nid_data_type(NID)
#table_full <- ghdx_search_nid_data_type(full = TRUE)

# Example Function Calls
# test1 <- ghdx_get_underlying_NIDs(all = TRUE)
# test2 <- ghdx_get_underlying_NIDs(387779, all = FALSE)
# test3 <- ghdx_get_underlying_NIDs(c(387779, 233896))