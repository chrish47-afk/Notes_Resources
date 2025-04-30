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

#library(plotly)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(maps)
#library(viridis)
library(tidyverse)
library(stringr)
library(rgeos)
library(rworldmap)
library(cowplot)
library(patchwork)
library(gtable)
library(grid)
#install.packages("rworldxtra", lib = "/home/j/temp/chrish47")
library(ggnewscale, lib.loc = "/home/j/temp/chrish47")
library(rworldxtra, lib.loc = "/home/j/temp/chrish47")

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

####-- DATA WRANGLING --#######################################################################################################
###############################################################################################################################


### DATA WRANGLING #############################################################################################################
#### Reading extraction or dt, merging with get_location_metdata for location_ascii_name
#zz <- read.xlsx("/ihme/homes/chrish47/ENDO_epi_lit_GBD2021_Oct192020_For Upload.xlsx") #Natalies Original Extraction Sheet
zz <- read.csv("/homes/chrish47/Endo SR Figure 3 DF_done_2.csv") #Natalies created df
#zz <- zz %>% select(nid,location_id, measure, cases, sample_size, year_start, year_end)
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
location_data <- location_data %>% select(location_id, location_ascii_name, location_type)
df <- merge(zz, location_data, by = "location_id") # Double Check only 5 columns exist
##################################################################
# Countries (Includes only admin0 location_types)
##################################################################
# Getting Centroid Spatial Coordinate
map <- getMap(resolution="high")
centroids <- gCentroid(map, byid=TRUE)
# get a data.frame with centroids
centroids_df <- as.data.frame(centroids)
setDT(centroids_df, keep.rownames = TRUE)[]
centroids_df <- dplyr::rename(centroids_df, location_ascii_name = rn)

## Including admind0 locations only
df <- df %>% subset(location_type == 'admin0') # Including only admin0 locations

#### Findings Differences between the two data tables
differences <- setdiff(df$location_ascii_name, centroids_df$location_ascii_name)
print(differences)

# Adjusting the data to fit the world_map constraints. Unfortunately this is the most efficient and consistent way of doing this process!
# GBD location_ids are not used in this code at the moment(03/05). They potentially can be if more GBD countries need to get incorporated, but
# for the moment they were not. 
df <- df %>% mutate(location_ascii_name = recode(str_trim(location_ascii_name), 
                                                 #"United States of America" = "USA", 
                                                 "Democratic People's Republic of Korea" = "North Korea",
                                                 #"California" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                 #"Iowa" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                 #"Minnesota" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                 #"Greater London" = "UK", 
                                                 #"Sweden except Stockholm" = "Sweden",
                                                 "Iran (Islamic Republic of)" = "Iran", 
                                                 #"South East England" = "UK", 
                                                 "Taiwan (Province of China)" = "Taiwan"
))
# Checking for Differences again to make sure
differences2 <- setdiff(df$location_ascii_name, centroids_df$location_ascii_name)
print(differences2)

## Merging world_final and df for the final inpu table to contain coordinates (Step1)######################################################
plot_p_final_1 <- merge(df, centroids_df, by = 'location_ascii_name')
plot_p_final_1 <- plot_p_final_1 %>% dplyr::rename(lat = y, lon = x)
write.xlsx(plot_p_final_1, "/ihme/scratch/users/chrish47/plot_p_final_1.xlsx")
## Further potential request. Extracting the year range from the aggregated data. 
## By Study vs. Country
##################################################################
# Subnationals (Doesn't included admin0 location_types)
##################################################################
#### World Map Coordinates. Includes both Country and Regional level Coordinates. 
world <- map_data("world") %>% subset(region != 'Antarctica')# Excluding the Antarctica spatial layer.
#world_final <- world %>% select(1,2,4,5,6)
#world_final <- world_final %>% dplyr::rename(location_ascii_name = region)

##########################################################################################################################
##########################################################################################################################
###################################################### Study counts ######################################################
zz <- read.xlsx("/ihme/homes/chrish47/ENDO_epi_lit_GBD2021_Oct192020_For Upload.xlsx") #Natalies Original Extraction Sheet
zz <- zz %>% select(nid,location_id, measure, cases, sample_size, year_start, year_end)
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
location_data <- location_data %>% select(location_id, location_ascii_name, location_type)
df <- merge(zz, location_data, by = "location_id") # Double Check only 5 columns exist

map <- getMap(resolution="high")
centroids <- gCentroid(map, byid=TRUE)
# get a data.frame with centroids
centroids_df <- as.data.frame(centroids)
setDT(centroids_df, keep.rownames = TRUE)[]
centroids_df <- dplyr::rename(centroids_df, location_ascii_name = rn)

# admin0 locations only!
df <- df %>% subset(location_type == 'admin0')

## Aggregating
### For Prevalence##############
plot_p <- df %>% subset(measure == 'prevalence')
plot_p <- plot_p %>% select(8,4,5,2) # years were excluded, but can be included for future requests!
plot_p <- aggregate(.~ location_ascii_name + nid, plot_p, sum) # Sum of sample_size and cases by nid and location
plot_p <- plot_p %>% dcast(location_ascii_name + cases + sample_size ~ nid, value.var = 'nid', length)
plot_p <- aggregate(.~location_ascii_name, plot_p, sum)
plot_p <- plot_p %>% mutate(study_count = select(., 4:ncol(plot_p)) %>% rowSums(na.rm = TRUE))
plot_p <- plot_p %>% select(1:3, ncol(plot_p))

plot_p <- plot_p %>% mutate(location_ascii_name = recode(str_trim(location_ascii_name), 
                                                         #"United States of America" = "USA", 
                                                         "Democratic People's Republic of Korea" = "North Korea",
                                                         #"California" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                         #"Iowa" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                         #"Minnesota" = "USA", # This is a tricky one!, but we want aggregated values, so....
                                                         #"Greater London" = "UK", 
                                                         #"Sweden except Stockholm" = "Sweden",
                                                         "Iran (Islamic Republic of)" = "Iran", 
                                                         #"South East England" = "UK", 
                                                         "Taiwan (Province of China)" = "Taiwan"
))
# Checking for Differences again to make sure
differences2 <- setdiff(plot_p$location_ascii_name, centroids_df$location_ascii_name)
print(differences2)

## Merging world_final and df for the final inpu table to contain coordinates (Step1)######################################################
plot_p_final_2 <- merge(plot_p, centroids_df, by = 'location_ascii_name')
plot_p_final_2 <- plot_p_final_2 %>% dplyr::rename(lat = y, lon = x)
write.xlsx(plot_p_final_2, "/ihme/scratch/users/chrish47/plot_p_final_2.xlsx")

# Re-aarranging data tables
plot_p_final_1_fixed <- plot_p_final_1 %>% arrange(desc(sample_size))

# Making sure the values in desired column are numeric or else it will crash!
# Delete or keep this line?
# plot_data$sample_size <- as.numeric(as.character(plot_data$sample_size))
# plot_data$cases <- as.numeric(as.character(plot_data$cases))