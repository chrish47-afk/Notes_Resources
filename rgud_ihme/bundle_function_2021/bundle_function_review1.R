rm(list=ls())
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
  ## Load Packages
  pacman::p_load(data.table, haven, dplyr, survey, Hmisc)
} else {
  j <- "/home/j/"
  user <- Sys.info()[["user"]]
  h <- paste0("/ihme/homes/", user, "/")
  l <- "/ihme/limited_use/"
}

# Uploading Libraries
library(ggplot2)
library(RColorBrewer)
#install.packages("formattable", lib = "/home/j/temp/chrish47")
#install.packages("qwraps2", lib = "/home/j/temp/chrish47")
library(formattable, lib.loc = "/home/j/temp/chrish47")
library(qwraps2, lib.loc = "/home/j/temp/chrish47")
library(tidyverse)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(DT)

source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/get_bundle_version.R")
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")

dt <- get_bundle_data(bundle_id = 292, gbd_round_id =  7, decomp_step = 'iterative')
dt_2 <- get_bundle_data(bundle_id = 75, gbd_round_id = 7, decomp_step = "iterative")
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7)

# --------------------------------------
unique_nids <- function(dt){
  ## nid ---
  dt <- as.data.table(dt) %>% select(nid, underlying_nid) %>% unique()
  write.csv(dt, '~/unique_nids.csv', row.names = FALSE) #export argument? Should be include this?
  
  dt <- DT::datatable(dt, class = 'cell-border hover', rownames = FALSE, filter = 'top', 
                      editable = TRUE, extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',  
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 100 
                      ))
  print(dt)
}

# --------------------------------------
unique_locations <- function(dt){
  dt <- as.data.table(dt) %>% count(vars = c('nid', 'location_name', 'location_id', 'sex', 'source_type', 'year_start', 'year_end', 'measure')) 
  # ---- 
  location_data <- location_data %>% select(location_id, location_name, location_type_id)
  # ----
  dt <- join(location_data, dt)
  # ----
  #drop.cols <- 'freq'
  dt <- as.data.table(dt) %>% select(-one_of('freq'))
  # ---- Deleting NA values
  dt <- na.omit(dt)
  dt <- dt %>% select(location_id, location_name, location_type_id) %>% unique()
  
  # Making table pretty and interactive
  dt <- DT::datatable(dt, class = 'cell-border hover', rownames = FALSE, filter = 'top', # Fix this , DT::datatable. 
                      editable = TRUE, extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',  
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 100 
                      ))
  print(dt)
}

# --------------------------------------
quick_summary <- function(dt) {
  dt <- as.data.table(dt) %>% count(vars = c('nid', 'location_name', 'location_id', 'sex', 'source_type', 'year_start', 'year_end', 'measure', 'age_start', 'age_end', 'mean', 'upper', 'lower')) 
  # ---- 
  location_data <- location_data %>% select(location_id, location_name, location_type_id)
  # ----
  dt <- join(location_data, dt)
  # ----
  #drop.cols <- 'freq'
  dt <- as.data.table(dt) %>% select(-one_of('freq'))
  # ---- Deleting NA values
  dt <- na.omit(dt)
  
  # Creating quick summary table
  our_summary <- list("Value Summary" = 
                        list("min" = ~min(mean),
                             "med" = ~median(mean),
                             "max" = ~max(mean))
                        
    )
  dt <- summary_table(dplyr::group_by(dt, measure), our_summary)
  
  # Making the table pretty and interactive ---
  dt <- DT::datatable(dt, class = 'cell-border hover', rownames = TRUE, filter = 'top', 
                      editable = TRUE, extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',  
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 100
                      )) 
  print(dt)
}


# --------------------------------------
full_summary <- function(dt){
  dt <- as.data.table(dt) %>% count(vars = c('nid', 'location_name', 'location_id', 'sex', 'source_type', 'year_start', 'year_end', 'measure', 'age_start', 'age_end', 'mean', 'upper', 'lower')) 
  # ---- 
  location_data <- location_data %>% select(location_id, location_name, location_type_id)
  # ----
  dt <- join(location_data, dt)
  # ----
  #drop.cols <- 'freq'
  dt <- as.data.table(dt) %>% select(-one_of('freq'))
  # ---- Deleting NA values
  dt <- na.omit(dt)

  dt <- DT::datatable(dt, class = 'cell-border hover', rownames = FALSE, filter = 'top', 
                      editable = TRUE, extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',  
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 100
                      ))
  print(dt)
}

# -------------------------------------- Final Menu
bundle_worker <- function(dt, job){
  if(job == 'unique_nids'){
    unique_nids(dt)
  } else if(job == 'unique_locations'){
    unique_locations(dt)
  } else if (job == 'quick_summary'){
    quick_summary(dt)
  } else if(job == 'full_summary'){
    full_summary(dt)
  } else {
    print('Please choose a job name') # Fix this whoe menu
  }
}

bundle_worker(dt = dt, job = 'full_summary') #quick_summary function is currently not working properly. 

