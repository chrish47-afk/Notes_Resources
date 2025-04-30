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

source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

#custom function
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  for (i in 2:vector_length) {
    if (i==2) {
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

#### Antenatal Care (1 Visit) Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/anc1/anc1_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Antenatal Care (1 Visit) Data"
file_name <- "Antenatal Care (1 Visit)_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 16, 21, 26, 27, 28, 29, 30, 31),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "21 to 25", " ",
                  "Modelable Entity ID: 10647", "Bundle ID: 7649", "Bundle Version ID: 40867", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### Antenatal Care (4 Visits) Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/anc4/anc4_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Antenatal Care (4 Visits) Data"
file_name <- "Antenatal Care (4 Visits)_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 16, 21, 26, 27, 28, 29, 30, 31),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "21 to 25", " ",
                  "Modelable Entity ID: 25043", "Bundle ID: 7655", "Bundle Version ID: 40868", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### Caesarean Section
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/csec/csec_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Caesarean Section Data"
file_name <- "Caesarean Section_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 16, 21, 28, 29, 30, 31, 32, 33),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "20 to 27", " ",
                  "Modelable Entity ID: 23990", "Bundle ID: 6572", "Bundle Version ID: 41774", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### In-Facility Delivery Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/ifd/ifd_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "In-Facility Delivery Data"
file_name <- "In-Facility Delivery_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 16, 21, 33, 34, 35, 36, 37, 38),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "21 to 32", " ",
                  "Modelable Entity ID: 10651", "Bundle ID: 7661", "Bundle Version ID: 40869", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### Full Maternal Care Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/mcare/mcare_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Full Maternal Care Data"
file_name <- "Full Maternal Care_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1", "2", "3", "4 to 5", "6 to 7", " ",
                  "Modelable Entity ID: 25316", "Bundle ID: 8102", "Bundle Version ID: 40873", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### Postnatal Care Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/pnc/pnc_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Postnatal Care Data"
file_name <- "Postnatal Care_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 2, 3, 5, 7, 9, 10, 11, 12, 13, 14),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1", "2", "3 to 4", "5 to 6", "7 to 8", " ",
                  "Modelable Entity ID: 25315", "Bundle ID: 8099", "Bundle Version ID: 40872", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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


#### Skilled Birth Attendance Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/compiled_tables/sba/sba_table.xlsx")
dt <- dt %>% select(location_name, All_data)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_data) %>% dplyr::rename(mapvar = All_data)
title <- "Skilled Birth Attendance Data"
file_name <- "Skilled Birth Attendance_Map.pdf"
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
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/maps/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 16, 21, 32, 33, 34, 35, 36, 37),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 15", "16 to 20", "21 to 31", " ",
                  "Modelable Entity ID: 10655", "Bundle ID: 7664", "Bundle Version ID: 40870", "GBD 2021"), 
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6', '#ffffff', "#ffffff", '#ffffff', "#ffffff", "#ffffff"),
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
