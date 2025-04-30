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
#source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_PREP_RDATA.r")

#custom function
gen_scale_labels <- function(x) {
  vector_length <- length(x)
  bin_labels <- character(vector_length-1)
  for (i in 2:vector_length) {
    if (i==2) {
      bin_labels[i-1] <- sprintf("<%s",x[i])
    } else if (i==vector_length) {
      label <- sprintf("%s to %s",x[i-1],x[i])
      #label <- sprintf("\U2265%s",x[i-1])
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
      #label <- sprintf("\U2265%s",x[i-1])
      bin_labels[i-1] <- label
    } else {
      bin_labels[i-1] <- sprintf("%s to <%s",x[i-1],x[i])
    }
  }
  return(bin_labels)
}


# # Object/Table/File Names
# dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_Inc_Landscaping_final.xlsx")
# dt <- dt %>% select(2, 24) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
# colnames(dt)
# 
# invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
# gbd_rnd <- 7 #GBD Round ID
# location_set_id <- 35
# location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)
# 
# nrow(location_data)
# location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
# location_map_join <- location_map %>% select(location_id)
# nrow(location_map)
# 
# dt <- merge(dt, location_map, by = "location_name")
# 
# dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
# title <- "Maternal Sepsis and Infection Incidence Data"
# file_name <- "MaternalSepsis&Infection_Map_Oct.pdf"
# nrow(dt_map)
# dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
# dt_map_zero[is.na(dt_map_zero)] <- 0
# nrow(dt_map_zero)
#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction and Fistula Incidence Data -- Obstructed Labor and Uterine Rupture Incidence and Fistula Prevalence Data
#Maternal Hemorrhage Incidence Data

# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map[, 'mapvar'], seq(0, 1, 0.1))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)

# c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6') -- 6
# c('#bdbdbd','#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4') -- 7
# c('#bdbdbd','#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4') -- 8


#### Maternal Sepsis and Infection Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_Inc_Landscaping_final.xlsx")
dt <- dt %>% select(2, 24) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Maternal Sepsis and Infection Incidence Data"
file_name <- "MaternalSepsis&Infection_Map_Oct.pdf"
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
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 51),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 50"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()


##########################################################################################################################################################

#### Maternal Abortion and Miscarriage Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_Inc_Landscaping_final.xlsx")
dt <- dt %>% select(2, 24) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Maternal Abortion and Miscarriage Incidence Data"
file_name <- "MaternalAbortion&Miscarriage_Map_Oct.pdf"
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
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 69),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 68"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()

##########################################################################################################################################################

#### Maternal Ectopic Pregnancy Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_Inc_Landscaping_final.xlsx")
colnames(dt)
dt <- dt %>% select(2, 22) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Maternal Ectopic Pregnancy Incidence Data"
file_name <- "MaternalEctopic_Map_Oct.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.05))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 49),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 48"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()

##########################################################################################################################################################

#### Maternal Hypertension Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_Inc_Landscaping_final.xlsx")
dt <- dt %>% select(2, 24) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Maternal Hypertension Incidence Data"
file_name <- "MaternalHypertesion_Map_Oct.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.05))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 63),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 62"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()
##########################################################################################################################################################

#### Maternal Hemorrhage Incidence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_Inc_Landscaping_final.xlsx")
dt <- dt %>% select(2, 23) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Maternal Hemorrhage Incidence Data"
file_name <- "MaternalHemorrhage_Map_Oct.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.05))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 51),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 50"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()

##########################################################################################################################################################

#### Obstructed Labor and Uterine Rupture Incidence and Fistula Prevalence Data
#############################################
# Object/Table/File Names
dt <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_Inc_Landscaping_final.xlsx", startRow = 2 )
dt <- dt %>% select(2, 6) #%>% dplyr::rename('location_name' = "Location.Name", "All_Data" = "All.Data")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Obstructed Labor and Uterine Rupture Incidence and Fistula Prevalence Data"
file_name <- "MaternalObstruction&Fistula_Map_Oct.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.05))
bins <- ceiling(bins)
bins <- unique(bins)
print(bins)
# generate scale labels
bin_labels <- gen_scale_labels(bins)
print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 6, 11, 21, 41, 51),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 5", "6 to 10", "11 to 20", "21 to 40", "41 to 50"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'),
        col.reverse=FALSE, 
        na.color = "gray",
        title=title,
        fname=NULL,
        legend.title="Counts of Source-Country-Years", 
        legend.columns = NULL, 
        legend.cex=0.8, 
        legend.shift=c(0,0))
ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()
