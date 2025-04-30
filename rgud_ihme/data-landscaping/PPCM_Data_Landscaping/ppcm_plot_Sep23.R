rm(list=ls())
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
  ## Load Packages
  ##pacman::p_load(data.table, haven, dplyr, survey, Hmisc, purr)
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
source("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Code/Source Counts Code/source_counts_master.R")

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

dt_region <- read.xlsx("/ihme/homes/chrish47/sunny_christian_dt.xlsx") %>% mutate("All_Data" = 1) %>% select(nid, location_id, All_Data, year_end, year_start)

# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "September, 2023 PPCM Extractions, Total")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% dplyr::rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% dplyr::rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
nrow(dt_final_country_region_counts)
max(dt_final_country_region_counts$count)

##################################################
# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "September, 2023 PPCM Extractions, Total", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, dt_final_country_region_counts) 
nrow(dt_final_country_region_counts)
##################################################

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "PPCM_Plot_Total.pdf"
savedir <- "/ihme/homes/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 5, 7)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "PPCM Bundle ID") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
#
#
# Incidence Only
#
#
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

dt_region <- read.xlsx("/ihme/homes/chrish47/sunny_christian_dt.xlsx") %>% mutate("All_Data" = 1) %>% filter(measure == "incidence") %>% select(nid, location_id, All_Data, year_end, year_start)

# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "September, 2023 PPCM Extractions, Incidence")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% dplyr::rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% dplyr::rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
nrow(dt_final_country_region_counts)
max(dt_final_country_region_counts$count)

##################################################
# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "September, 2023 PPCM Extractions, Incidence", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, dt_final_country_region_counts) 
nrow(dt_final_country_region_counts)
##################################################

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "PPCM_Plot_Incidence.pdf"
savedir <- "/ihme/homes/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(1, 2, 3)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "PPCM Bundle ID") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
############################################################################################################################
############################################################################################################################

# PPCM, Nov 2nd, 2023

############################################################################################################################
############################################################################################################################

#location_data <- get_ids("ihme_loc")
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Incidence Only
dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM10058_1020 incidenceonly.xlsx")
# LVEF Only
dt2 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 LVEFonly.xlsm")
# Mortality Only
dt3 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 mortalityonly.xlsm")
# NYHA Only
dt4 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 NYHAonly.xlsm")
# Remission Only
dt5 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 remduronly.xlsm")
# Severity Only
dt6 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 severityonly.xlsm")
# Transplant Only
dt7 <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM_extraction_merge1016 transplantonly.xlsm")


source_counts(custom_data_table = dt, out_path = "/ihme/homes/chrish47/Data landscape 1101/Incidence/")
source_counts(custom_data_table = dt2, out_path = "/ihme/homes/chrish47/Data landscape 1101/LVEF/")
source_counts(custom_data_table = dt3, out_path = "/ihme/homes/chrish47/Data landscape 1101/Mortality/")
source_counts(custom_data_table = dt4, out_path = "/ihme/homes/chrish47/Data landscape 1101/NYHA/")
source_counts(custom_data_table = dt5, out_path = "/ihme/homes/chrish47/Data landscape 1101/Remission/")
source_counts(custom_data_table = dt6, out_path = "/ihme/homes/chrish47/Data landscape 1101/Severity/")
source_counts(custom_data_table = dt7, out_path = "/ihme/homes/chrish47/Data landscape 1101/Transplant/")


############### DOT PLOT
###############
###############
###############
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


dt_region <- dt %>% mutate("All_Data" = 1) %>% select(nid, location_id, All_Data, year_end, year_start)

# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Total Incidence for Peripartum Cardiomyopathy, November 2023")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% dplyr::rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% dplyr::rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
nrow(dt_final_country_region_counts)
max(dt_final_country_region_counts$count)

##################################################
# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Total Incidence for Peripartum Cardiomyopathy, November 2023", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, dt_final_country_region_counts) 
nrow(dt_final_country_region_counts)
##################################################

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "PPCM_incidence_plot.pdf"
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(1, 2, 3)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "PPCM Bundle ID: 10058") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)

############### MAP
###############
###############
###############
# Incidence #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/Incidence/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total Incidence for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_incidence_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10058"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# Mortality #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/Mortality/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total Mortality for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_mortality_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: pending"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# Severity #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/Severity/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total Severity for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_severity_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10059"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# LVEF #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/LVEF/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total LVEF for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_lvef_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10059"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# NYHA #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/NYHA/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total NYHA for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_nyha_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10059"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# Remission #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/Remission/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total Remission for Peripartum Cardiomyopathy on November 2023"
file_name <- "PPCM_remission_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10059"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

# Transplant #####################
source("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R")

dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/Transplant/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", sheet = 2)
dt <- dt %>% select(2,3) %>% dplyr::rename('location_name' = "country_name", "All_Data" = "n_adj")
colnames(dt)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
location_data <- get_location_metadata(location_set_id=35, release_id = 9)

nrow(location_data)
location_map <- location_data %>% filter(location_type == 'admin0' | level == 3) %>% select(location_name, location_id)
location_map_join <- location_map %>% select(location_id)
nrow(location_map)

dt <- merge(dt, location_map, by = "location_name")

dt_map <- dt %>% select(location_id, All_Data) %>% dplyr::rename(mapvar = All_Data)
title <- "Total Transplant for Peripartum Cardiomyopathy, November 2023"
file_name <- "PPCM_transplant_Map.pdf"
nrow(dt_map)
dt_map_zero <- right_join(dt_map, location_map_join, by = 'location_id')
dt_map_zero[is.na(dt_map_zero)] <- 0
nrow(dt_map_zero)
#############################################
# Bins ###########################
#bins <- bins[! bins %in% c(0)]
#bins <- quantile(dt_map_zero[, 'mapvar'], seq(0, 1, 0.04))
#bins <- ceiling(bins)
#bins <- unique(bins)
#print(bins)
## generate scale labels
#bin_labels <- gen_scale_labels(bins)
#print(bin_labels)
##########################################
max(dt_map_zero$mapvar)
#['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)
gbd_map(dt_map_zero, 
        limits=c(0, 1, 3, 5, 15, 44, 45, 46),
        legend=TRUE, 
        inset=FALSE,
        sub_nat = "none",
        labels= c("No Data", "1 to 2", "3 to 4", "5 to 14", "15 to 43", " ","Bundle ID: 10059"), 
        #bin_labels #For now, I entered this manually to fulfill SME requests.#c("1", "2", "3", "4 to 5", "6 to 17", "18 to 50")
        pattern=NULL,
        col = c('#bdbdbd','#d7191c', '#fee08b','#fdae61','#2c7bb6', '#ffffff', "#ffffff"),
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

############### Incidence Year Spread Plot
###############
###############
###############
# Incidence #####################
dt <- read.xlsx("/ihme/homes/chrish47/Data landscape 1101/PPCM10058_1020 incidenceonly.xlsx")

library(ggplot2)
#install.packages("geomtextpath", lib = "/ihme/homes/chrish47/R_packages_cluster/") #I usually use my scratch folder or homes(H) drive. #Try to avoid using your J temp if possible.
library(geomtextpath, lib.loc = "/ihme/homes/chrish47/R_packages_cluster/")
library(dplyr)

dt_edit <- dt %>% select(location_id, location_name, year_start, year_end) %>% unique()

location_data <- get_location_metadata(location_set_id=35, release_id = 9)

dt_final <- merge(location_data, dt_edit, by = 'location_id') %>% dplyr::select(year_start, year_end, region_name)


# p=ggplot(dt_final,aes(x=year_start,y=year_end,group=super_region_name))+
#   geom_point(aes(color=super_region_name), size = 4) +
#   geom_textabline(intercept = 0, slope = 1, color = "black", linetype = "solid", label = "partition line", color = "green4", hjust = 0.6, vjust = -0.2) +
#   geom_textabline(intercept = -1980, slope = 2, color = "orange", linetype = "solid", size = 0.5, label = 2) +
#   geom_textabline(intercept = -7920, slope = 5, color = "blue", linetype = "solid", size = 0.5, label = 5) +
#   xlim(1980, 2023) +
#   ylim(1980, 2023) +
#   labs(title = "PPCM by year spread", x = "Year Start", y = "Year End", colour = 'Super Region') +
#   theme(plot.title = element_text(hjust = 0.5))
# p



file_name <- "PPCM_incidence_year_spread_plot.pdf"
savedir <- "/ihme/homes/chrish47/Data landscape 1101/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
p=ggplot(dt_final,aes(x=year_start,y=year_end,group=region_name))+
  geom_point(aes(color=region_name), size = 4) +
  geom_textabline(intercept = 0, slope = 1, color = "black", linetype = "dashed", label = "1 year", hjust = 0.93, vjust = -0.1) +
  geom_textabline(intercept = 10, slope = 1, color = "black", linetype = "dashed", label = "10 years", hjust = 0.93, vjust = -0.1) +
  geom_textabline(intercept = 20, slope = 1, color = "black", linetype = "dashed", label = "20 years", hjust = 0.93, vjust = -0.1) +
  xlim(1980, 2023) +
  ylim(1980, 2023) +
  labs(title = "Incidence Data for Peripartum Cardiomyopathy by year spread, November 2023", x = "Year Start", y = "Year End", colour = 'Region') +
  theme(plot.title = element_text(hjust = 0.5))
p
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()








