## ---------------------------
##
## Script name: mortality_plots.R 
##
## Purpose of script: Create data landscape dot plots by sub cause
##
## Author: Max Dodge
##
## Last updated: 2024-03-14
##
## Email: maxdodge@uw.edu
##
## ---------------------------
##
## Notes: Using the mortality output table to create dot plots of data availability
## by sub cause and region
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

## Abortive Outcome

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/abortive_outcome/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Abortion and Miscarriage Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Abortion and Miscarriage Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Abortion_Miscarriage_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 995 \n Bundle ID: 5873 \n Bundle Version ID: 39779 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Ectopic Pregnancy

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/ectopic/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Ectopic Pregnancy Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Ectopic Pregnancy Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Ectopic_Pregnancy_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 374 \n Bundle ID: 5876 \n Bundle Version ID: 39780 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx" ) +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Obstructed Labor

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/obstruction/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Maternal Obstructed Labor and Uterine Rupture Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Maternal Obstructed Labor and Uterine Rupture Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Maternal_Obstructed_Labor_Uterine_Rupture_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 370 \n Bundle ID: 5867 \n Bundle Version ID: 39778 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Hemorrhage

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/hemorrhage/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Maternal Hemorrhage Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Maternal Hemorrhage Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Maternal_Hemorrhage_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 367 \n Bundle ID: 5858 \n Bundle Version ID: 39775 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Hypertensive Disorders of Pregnancy

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/hdop/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Maternal Hypertensive Disorders Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Maternal Hypertensive Disorders Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Maternal_Hypertensive_Disorders_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 369 \n Bundle ID: 5861 \n Bundle Version ID: 39776 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Sepsis

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/mternal_sepsis_infection/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Maternal Sepsis and Other Maternal Infections Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Maternal Sepsis and Other Maternal Infections Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Maternal_Sepsis_Other_Maternal_Infections_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 368 \n Bundle ID: 5864 \n Bundle Version ID: 39777 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Other Direct

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/other_subcause/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Other Direct Maternal Deaths Mortality Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Other Direct Maternal Deaths Mortality Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply picked 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Other_Direct_Maternal_Deaths_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID: 379 \n Bundle ID: 5879 \n Bundle Version ID: 39782 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Indirect

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/indirect/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Indirect Maternal Death Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Indirect Maternal Death Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Indirect_Maternal_Deaths_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Modelable Entity ID: 10543 \n Bundle ID: 5870 \n Bundle Version ID: 39781 \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()

######################

## Indirect HIV

plot_data_raw <- read.xlsx("/ihme/scratch/users/maxdodge/mortality_landscape/column_outputs/indirect_hiv/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx", 5)

plot_data <- plot_data_raw %>%
  mutate(Cause = "Indirect HIV Maternal Death Data") %>%
  dplyr::rename(Year = year_end, count = n_adj)

# Adding Regions with zero counts
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)

location_data <- location_data %>% select(location_type, region_name) %>% filter(location_type %in% 'region') %>% select(-location_type)
location_data <- location_data %>% mutate(Year = 2000, Cause = "Indirect HIV Maternal Death Data", count = 0) %>% select('Year', 'Cause', 'region_name', 'count')
# I simply choosed 2000 randomly. It ultimately doesn't really matter, since its zero counts. So It shouldn't impact the plot in any way. Beside including the Regions with zero counts #in the y-axis as expected.
dt_final_country_region_counts <- rbind(location_data, plot_data) 

IHME_multicolor <- c("#9dcb3b","#009994","#279cba","#e28126","#e7c624","#d93152","#a64792")

#
file_name <- "Indirect_HIV_Maternal_Deaths_Plot.pdf"
savedir <- "/ihme/scratch/users/maxdodge/mortality_landscape/plots/"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 8)
#

plt=ggplot(dt_final_country_region_counts, aes(x=Year, y=region_name, color=Cause)) +
  scale_color_manual(values=IHME_multicolor[1:3]) +
  guides(col = FALSE) +
  guides(size=guide_legend(title="Source-Country-Years of Data Available")) + #"Site Years of Data Available"
  geom_point(aes(size=count),show.legend=TRUE) +
  scale_size_continuous(range = c(-1, 13), breaks = c(3, 6, 12)) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  labs(caption = "Cause ID:  \n Bundle ID:  \n Bundle Version ID:  \n GBD 2021 \n Input Data: /ihme/RTs_and_Projects/GBD/Teams/RGUD/Maternal Morbidity Project/1A_Data Landscaping/_Final report_Aug23/Mortality_input tables/mortality_input_2024_03_14.xlsx") +
  theme(text=element_text(size=20),
        plot.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 0, size=16),
        panel.border=element_blank(),
        legend.text=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=15),
        plot.caption = element_text(hjust = 1.0, size = 9), #face = "italic"
        panel.grid.major = element_line(color = "black",
                                        size = 0.25,
                                        linetype = 1)) +
  facet_wrap(~Cause, scales = "free_y") 
plt

ggsave(filename = pdf_path, width = 20, height = 8)
dev.off()
