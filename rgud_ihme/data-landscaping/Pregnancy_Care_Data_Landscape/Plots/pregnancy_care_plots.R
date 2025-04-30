## ---------------------------
##
## Script name: pregnancy_care_plots.R 
##
## Purpose of script: Create data landscape dot plots by sub cause
##
## Author: Max Dodge
##
## Last updated: 2024-03-18
##
## Email: maxdodge@uw.edu
##
## ---------------------------
##
## Notes: Using the pregnancy care output tables to create dot plots of data availability
## by sub cause and region.
##
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

## ---------------------------

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

## ---------------------------

######################

## ANC1

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc1/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Antenatal Care (1 Visit) Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Antenatal Care (1 Visit) Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Antenatal Care (1 Visit)_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 10647 \n Bundle ID: 7649 \n Bundle Version ID: 40867 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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

######################

## ANC4

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/anc4/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Antenatal Care (4 Visits) Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Antenatal Care (4 Visits) Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Antenatal Care (4 Visits)_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 25043 \n Bundle ID: 7655 \n Bundle Version ID: 40868 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc4_2024_03_18.xlsx") +
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
######################

## CSEC

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/csec/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Caesarean Section Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Caesarean Section Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Caesarean Section_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 23990 \n Bundle ID: 6572 \n Bundle Version ID: 41774 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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
######################

## IFD

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/ifd/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "In-Facility Delivery Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "In-Facility Delivery Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "In-Facility Delivery_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 10651 \n Bundle ID: 7661 \n Bundle Version ID: 40869 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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
######################

## MCARE

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/mcare/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Full Maternal Care Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Full Maternal Care Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Full Maternal Care_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 25316 \n Bundle ID: 8102 \n Bundle Version ID: 40873 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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
######################

## PNC

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/pnc/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Postnatal Care Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Postnatal Care Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Postnatal Care_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 25315  \n Bundle ID: 8099 \n Bundle Version ID: 40872 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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
######################

## SBA

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/pregnancy_care_landscape/column_outputs/sba/All_data/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Skilled Birth Attendance Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

## Adding regions with zero counts
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% 
  filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Skilled Birth Attendance Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Skilled Birth Attendance_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/pregnancy_care_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#
plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) + #max:199 and min:1
  #scale_size_continuous(limits = c(1, 11, 21, 31, 41) , labels = c('1 to 10', '11 to 20', '21 to 30', '31 to 40')) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 10655 \n Bundle ID: 7664 \n Bundle Version IDs: 40870 \n GBD 2021
       Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Pregnancy care_input tables/anc1_2024_03_18.xlsx") +
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
