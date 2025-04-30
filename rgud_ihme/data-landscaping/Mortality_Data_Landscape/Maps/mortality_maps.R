## ---------------------------
##
## Script name: mortality_maps.R 
##
## Purpose of script: Create data landscape maps by sub cause
##
## Author: Max Dodge
##
## Last updated: 2024-03-14
##
## Email: maxdodge@uw.edu
##
## ---------------------------
##
## Notes: Using the mortality output table to create maps of data availability
## by sub cause and country
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

library(data.table)
library(openxlsx)
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(reshape2)
library(rgeos)
library(rworldmap)
library(cowplot)
library(patchwork)
library(gtable)
library(grid)
library(RColorBrewer)

## ---------------------------

source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")


## ---------------------------

#custom function -- Updated for Maternal Landscaping
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  print(bin_labels)
  for (i in 1:vector_length) {
    if (i==1) {
      bin_labels[i-1] <- sprintf("<%s",x[i])
    } else if (i==vector_length) {
      label <- sprintf("%s to %s",x[i-1],x[i])
      bin_labels[i-1] <- label
    } else {
      bin_labels[i-1] <- sprintf("%s to <%s",x[i-1],x[i])
    }
  }
  return(bin_labels)
}

#### Abortion and Miscarriage Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 6)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, abortive_outcome) %>% dplyr::rename(mapvar = abortive_outcome)
title <- "Abortion and Miscarriage Mortality Data"
file_name <- "Abortion_Miscarraige_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 79, 80, 81, 82, 83, 84),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 78", " ","Cause ID: 995", "Bundle ID: 5873", "Bundle Version ID: 39779",
                 "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Ectopic Pregnancy Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 7)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, ectopic) %>% dplyr::rename(mapvar = ectopic)
title <- "Ectopic Pregnancy Mortality Data"
file_name <- "Ectopic_Pregnancy_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 78, 79, 80, 81, 82, 83),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 77", " ","Cause ID: 374", "Bundle ID: 5876", "Bundle Version ID: 39780",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Maternal Obstructed Labor and Uterine Rupture Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 8)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, obstruction) %>% dplyr::rename(mapvar = obstruction)
title <- "Maternal Obstructed Labor and Uterine Rupture Mortality Data"
file_name <- "Maternal_Obstructed_Labor_Uterine_Rupture_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 79, 80, 81, 82, 83, 84),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 78", " ","Cause ID: 370", "Bundle ID: 5867", "Bundle Version ID: 39778",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Maternal Hemorrhage Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 9)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, hemorrhage) %>% dplyr::rename(mapvar = hemorrhage)
title <- "Maternal Hemorrhage Mortality Data"
file_name <- "Maternal_Hemorrhage_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 78, 79, 80, 81, 82, 83),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 77", " ","Cause ID: 367", "Bundle ID: 5858", "Bundle Version ID: 39775",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Maternal Hypertensive Disorders Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 10)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, hdop) %>% dplyr::rename(mapvar = hdop)
title <- "Maternal Hypertensive Disorders Mortality Data"
file_name <- "Maternal_Hypertensive_Disorders_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 79, 80, 81, 82, 83, 84),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 78", " ","Cause ID: 369", "Bundle ID: 5861", "Bundle Version ID: 39776",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Maternal Sepsis and Other Maternal Infections Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 11)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, mternal_sepsis_infection) %>% dplyr::rename(mapvar = mternal_sepsis_infection)
title <- "Maternal Sepsis and Other Maternal Infections Mortality Data"
file_name <- "Maternal_Sepsis_Other_Maternal_Infections_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 79, 80, 81, 82, 83, 84),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 78", " ","Cause ID: 368", "Bundle ID: 5864", "Bundle Version ID: 39777",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Indirect Maternal Deaths Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 12)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, indirect) %>% dplyr::rename(mapvar = indirect)
title <- "Indirect Maternal Deaths Mortality Data"
file_name <- "Indirect_Maternal_Deaths_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 79, 80, 81, 82, 83, 84),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 78", " ","Modelable Entity ID: 10543", "Bundle ID: 5870", "Bundle Version ID: 39781",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Other Direct Maternal Deaths Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 14)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, other_subcause) %>% dplyr::rename(mapvar = other_subcause)
title <- "Other Direct Maternal Deaths Mortality Data"
file_name <- "Other_Direct_Maternal_Deaths_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 74, 75, 76, 77, 78, 79),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 73", " ","Cause ID: 379", "Bundle ID: 5879", "Bundle Version ID: 39782",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()

#### Indirect HIV Maternal Deaths Mortality Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/compiled_tables/subcause_table.xlsx")
dt <- dt %>% select(2, 13)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, indirect_hiv) %>% dplyr::rename(mapvar = indirect_hiv)
title <- "Indirect HIV Maternal Deaths Mortality Data"
file_name <- "Indirect_HIV_Maternal_Deaths_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 42, 43, 44, 45, 46, 47),
        legend=TRUE,
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41+", " ","Cause ID: ", "Bundle ID: ", "Bundle Version ID: ",
                  "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", '#ffffff'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = 1, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)

dev.off()
