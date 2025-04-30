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

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Obstruction & Fistula Incidence and Prevalence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
nrow(dt_final_country_region_counts)
#max(dt_final_country_region_counts$count)

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalObstruction&Fistula_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 14)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Abortion and Miscarriage Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
nrow(dt_final_country_region_counts)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalAbortion&Miscarriage_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 20)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Hemorrhage Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalHemorrhage_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 16)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Hypertension Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalHypertension_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 18)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Ectopic Pregnancy Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalEctopic_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 8, 14)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Sepsis and Infection Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
#dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
#
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid')
dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == TRUE) %>% select(1:7)
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(is.na(`Component NID`) == FALSE) %>% select(`Component NID`, All_Data, location_name, region_name, Cause) %>% rename('nid' = 'Component NID')
dt_component_years <- ghdx_search_nid_time_period(dt_final_country_region_counts_2$nid) %>% mutate(year_start = substr(time_start,1,4), year_end = substr(time_end, 1,4)) %>% rename('nid' = 'NID') %>% select(nid, year_start, year_end)
dt_final_country_region_counts_2 <- merge(dt_final_country_region_counts_2, dt_component_years, by = 'nid') %>% select(nid, All_Data, location_name, year_start, year_end, region_name, Cause)
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2) %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
dt_final_country_region_counts$Year <- dt_final_country_region_counts$Year %>% as.numeric(dt_final_country_region_counts$Year)
sum(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalSepsis&Infection_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 16)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
###################################################################################################################################
###################################################################################################################################################
# Alternative Plot ###########################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Sepsis and Infection Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))

# New Plot
colnames(dt_final_country_region_counts)
ggplot(dt_final_country_region_counts, aes(x = Year, y = count, color = region_name)) +
  geom_point(alpha = 2, size = 5, shape = 20) + #shape = 21
  scale_color_discrete(name = 'GBD Region')




#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# Second Batch of Plots, with unnested Years
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
gbd_rnd <- 7 #GBD Round ID
location_set_id <- 35
location_data <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_rnd)

dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Obstruction & Fistula Incidence and Prevalence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
#max(dt_final_country_region_counts$count)

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalObstruction&Fistula_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 17)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Abortion and Miscarriage Incidence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalAbortion&Miscarriage_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 20)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Hemorrhage Incidence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalHemorrhage_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 21)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Hypertension Incidence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))#max(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalHypertension_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 25)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Ectopic Pregnancy Incidence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))#max(dt_final_country_region_counts$count)#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalEctopic_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 7, 14)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Sepsis and Infection Incidence Data")
nid_unique <- unique(dt_final_country_region_counts$nid) #
nids_data_types <- ghdx_search_nid_data_type(nid = nid_unique) %>% dplyr::rename('nid' = 'NID') %>% select(nid,Data_Type) #
dt_final_country_region_counts <- merge(dt_final_country_region_counts, nids_data_types, by = 'nid') #
unique(dt_final_country_region_counts$Data_Type)

dt_final_country_region_counts_1 <- dt_final_country_region_counts %>% filter(Data_Type == 'Scientific literature' | Data_Type == 'Report') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(-year_start) %>% dplyr::rename('year' = 'year_end') #
dt_final_country_region_counts_1 <- dt_final_country_region_counts_1 %>% select(nid, All_Data, location_name, region_name, Cause, Data_Type, year) # 
dt_final_country_region_counts_2 <- dt_final_country_region_counts %>% filter(Data_Type != 'Scientific literature',Data_Type != 'Report') #
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts_2$year <- mapply(seq,dt_final_country_region_counts_2$year_start,dt_final_country_region_counts_2$year_end,SIMPLIFY=FALSE) 
dt_final_country_region_counts_2 <- dt_final_country_region_counts_2 %>% unnest(year) %>% select(-year_end, -year_start)

# Binding datasets
dt_final_country_region_counts <- rbind(dt_final_country_region_counts_1, dt_final_country_region_counts_2)

# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))#max(dt_final_country_region_counts$count)#max(dt_final_country_region_counts$count)
#max(dt_final_country_region_counts$count)

#Maternal Abortion and Miscarriage Incidence Data
#Maternal Ectopic Pregnancy Incidence Data
#Maternal Hypertension Incidence Data
#Maternal Sepsis and Infection Incidence Data
#Maternal Obstruction & Fistula Incidence and Prevalence Data
#Maternal Hemorrhage Incidence Data

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "MaternalSepsis&Infection_Plot_Oct.pdf"
savedir <- "/ihme/scratch/users/chrish47/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(3, 12), breaks = c(5, 10, 17)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle = 45, size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15))+
  facet_wrap(~Cause, scales = "free_y") 
plt
#
ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

summary(dt_final_country_region_counts$count)
###################################################################################################################################
###################################################################################################################################################
# Alternative Plot ###########################################
dt_region <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx") %>% select(nid, location_id, All_Data, year_end, year_start) %>% filter(All_Data == 1) 
# Location
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_final <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

dt_final <- left_join(dt_region, location_data_final, by = 'location_id') %>% select(-location_id) %>% unique()
dt_final <- dt_final %>% left_join(location_data %>% filter(location_type == 'admin0' | level == 3), by = 'location_name') %>% select(nid, All_Data,location_name, year_start, year_end, region_name)

dt_final_country_region_counts <- dt_final %>% mutate(Cause = "Maternal Sepsis and Infection Incidence Data")
dt_merge <- ghdx_merged_citation_component(nid = dt_final_country_region_counts$nid) %>% dplyr::rename('nid' = 'Merged Citation NID')
dt_final_country_region_counts <- left_join(dt_final_country_region_counts, dt_merge, by = 'nid') %>% select(All_Data, location_name, year_start, year_end, region_name, Cause)
# Adding a ID/index column ~ This replaces the NID/Component_NIDs columns
dt_final_country_region_counts <- dt_final_country_region_counts %>% mutate(ID = row_number())


dt_final_country_region_counts <- dt_final_country_region_counts %>% select(year_end, Cause, region_name, All_Data) %>% dplyr::rename("Year" = "year_end") %>% group_by(Year, Cause, region_name) %>% summarise(count = sum(All_Data))

# New Plot
colnames(dt_final_country_region_counts)
ggplot(dt_final_country_region_counts, aes(x = Year, y = count, color = region_name)) +
  geom_point(alpha = 2, size = 5, shape = 20) + #shape = 21
  scale_color_discrete(name = 'GBD Region')







