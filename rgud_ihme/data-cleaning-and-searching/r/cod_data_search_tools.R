##################################
# Author: Christian Hernandez
# Date: January, 2023
# Purpose: These CoD data search functions allow you to pull/extract data directly from the CoD db accordingly. 
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
cod_search_nid_data_type <- function(parent_nid = NULL, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='cod', host='modeling-cod-db.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb,
                           "SELECT cnm.parent_nid, cnm.nid, cnm.source,dt.data_type_id, dt.data_type_full
                           FROM claude_nid_metadata as cnm 
                           JOIN data_type as dt ON dt.data_type_id = cnm.data_type_id"
                           )
  query_data <- query_data %>% dplyr::rename("underlying_id" = "nid")
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(parent_nid)
    subset <- subset(query_data, parent_nid %in% nid_t$parent_nid) %>% unique()
    return(subset)
  } else if (full == TRUE) {
    query_data <- query_data %>% unique()
    return(query_data)
  }
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}
####################################################################################################################################
# Example
#NID <- 255990
#table <- cod_search_nid_data_type(NID)
#table_full <- cod_search_nid_data_type(full = TRUE)
