## General Information ---------------------------------------------------------
##
## Script name: GHDx_queries.R 
##
## Purpose of script: Automate key steps of the data landscaping process.
##
## Author: Max Dodge
##
## Last updated: 2024-11-26
##
## Email: maxdodge@uw.edu
##
## Notes -----------------------------------------------------------------------
##
## 
##
## Environment -----------------------------------------------------------------

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

## Libraries and Functions -----------------------------------------------------

library(dplyr)
library(DBI)
library(RMySQL)

## Merged citations ------------------------------------------------------------

ghdx_merged_citations <- function() {
  
  # Sourcing GHDx database
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  
  # Pulling data
  query_data_merged <- dbGetQuery(mydb, "SELECT field_merged_records_target_id, entity_id FROM field_data_field_merged_records;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  
  #Disconnecting from SQL database
  lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect)
  
  # Merging data
  query_data_merged <- query_data_merged %>% 
    dplyr::rename('nid' = 'field_merged_records_target_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid')
  query_data_merged <- query_data_merged %>% 
    dplyr::select(nid, title, entity_id)
  query_data_merged <- query_data_merged %>% 
    dplyr::rename('component_nid' = 'nid', 'nid' = 'entity_id')
  
  return(query_data_merged)
}

## Underlying NIDs -------------------------------------------------------------

ghdx_get_underlying_NIDs <- function(){

  # Sourcing GHDx database
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  
  # Pulling data
  query_data_merged <- dbGetQuery(mydb, "SELECT entity_type, entity_id, field_references_node_target_id FROM field_data_field_references_node;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  
  #Disconnecting from SQL database
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  # Merging data
  query_data_merged <- query_data_merged %>% 
    dplyr::rename('underlying_nid' = 'field_references_node_target_id','nid' = 'entity_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid') %>% 
    dplyr::select(nid, title, underlying_nid)
  
    return(query_data_merged)
}

## Titles and Datatype ---------------------------------------------------------

ghdx_titles_datatype <- function(){
  
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  
  # Mimicking the GHDx csv output column names
  title <- dbGetQuery(mydb, "SELECT nid, title FROM node;")
  dt_id <- dbGetQuery(mydb, "SELECT entity_id AS nid, field_type_tid AS type_id FROM field_data_field_type;")
  dt_name <- dbGetQuery(mydb, "SELECT tid AS type_id, name AS data_type FROM taxonomy_term_data;")
  
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  # Merging data
  data_type <- left_join(dt_id, dt_name)
  merged <- full_join(title, data_type) %>%
    select(nid, title, data_type)

  return(merged)

}


## Citation ---------------------------------------------------------------------

ghdx_citation <- function(){
  
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  
  # Mimicking the GHDx csv output column names
  citation <- dbGetQuery(mydb, "SELECT entity_id AS nid, field_citation_value AS citation FROM field_data_field_citation;")
  
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

  return(citation)
  
}

## Private ---------------------------------------------------------------------

ghdx_private <- function(){
  
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  
  # Mimicking the GHDx csv output column names
  private <- dbGetQuery(mydb, "SELECT entity_id AS nid, field_private_data_tid AS privacy_id FROM field_data_field_private_data;")
  id <- dbGetQuery(mydb, "SELECT tid AS privacy_id, name AS confidentiality FROM taxonomy_term_data;")
  
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  # Merging data
  merged <- left_join(private, id) %>%
    select(nid, confidentiality)
  
  return(merged)
  
}

## Potential tables and fields -------------------------------------------------

mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')

potential_tables <- as.data.frame(dbListTables(mydb))

dbListFields(mydb, "taxonomy_term_data")

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
