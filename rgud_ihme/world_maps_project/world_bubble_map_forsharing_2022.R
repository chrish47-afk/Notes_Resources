####################################################
## Author: Christian Hernandez
## Date: 03/15/2022
## Description: World Bubble(Proportional) Map
## Other ----------------------------------------
## In collaboration with Natalie Smith (RGUD Team)
## Current Status: In Development 
####################################################

# Description: ############################################################### 
# Cause: Endometriosis (Prevalence and Incidence) ~ As of right now, I have only plotted for Prevalence!

# In collaboration with Natalie Smith's data and feedback, the following code
# will create a world multi-layer bubble(proportional). Consisting of but not
# limited to measure_type data, a sample size(denominator) and number of cases
# (numerator), as well as source_type counts. Additional layers and data inputs
# can be integrated.
# 
# The current objective of this code is to reproduce multiple measure_type data
# from recently extracted sources, for publishing and vetting purposes. The 
# ultimate goal going forward is to automate the code, polish the
# multi-layer plots, and final outputs for source_counts as well bundle analysis.
# 
# The code is still in development as of March 24, 2022!
##############################################################################
##############################################################################
# July 21, 2022 - Notes
# • Change the color gradient [Number of Cases] to Mean Prevalence for each Source
# 
# From Natalie & Mae Inputs
# • 1) We eliminate the color dot for study count. You can visualize the number of studies pretty well with each of the markers for study type.
# • 2) If we can, have the study type be colored by the case counts/ mean. So this would be the shaped markers being filled by the case mean for that study. 
# Change the color and shape of the study type / Combining the mean and study type
# This can potentially impact help with the overlapping!
# Decrease the size of the sample size bubles
# Three different Maps, each representing a different study type!
# 


##############################################################################
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

library(openxlsx)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyverse)
library(stringr)
library(rgeos)
library(rworldmap)
library(cowplot)
library(patchwork)
library(gtable)
library(grid)
library(RColorBrewer)
#install.packages("wesanderson", lib = "/home/j/temp/chrish47")
library(wesanderson, lib.loc = "/home/j/temp/chrish47")
library(ggnewscale, lib.loc = "/home/j/temp/chrish47")
library(rworldxtra, lib.loc = "/home/j/temp/chrish47")

# Shared Functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

####-- DATA WRANGLING --#######################################################################################################
###############################################################################################################################
#### --- First DF --- ####
df_original <- read.csv("/homes/chrish47/Endo SR Figure 3 DF_done_2.csv") # [Natalie's Second df - Editted]
#zz <- zz %>% select(nid,location_id, measure, cases, sample_size, year_start, year_end)
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
location_data <- location_data %>% select(location_id, location_name, location_type)
df <- merge(df_original, location_data, by = "location_id")

### Getting Centroid Spatial Coordinate
map <- getMap(resolution="high")
centroids <- gCentroid(map, byid=TRUE)
### Setting up the Centroid Coordinates to a DF
centroids_df <- as.data.frame(centroids)
setDT(centroids_df, keep.rownames = TRUE)[]
centroids_df <- dplyr::rename(centroids_df, location_name = rn)

### Subsetting to Admin0 locations only -- Subnationals will be addressed later!
df <- df %>% subset(location_type == 'admin0') # Including only admin0 locations

#### Checking Column value(locations) differences. We need to change these or else they won't show up in the final table!
differences <- setdiff(df$location_name, centroids_df$location_name)
print(differences)

### Using the mutate() to change location names
df <- df %>% mutate(location_name = recode(str_trim(location_name), 
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
### Checking for Differences once make to verify!
differences2 <- setdiff(df$location_name, centroids_df$location_name)
print(differences2)

### Merging Centroid DF and Prevalent Table
plot_p_final_1 <- merge(df, centroids_df, by = 'location_name')
plot_p_final_1 <- plot_p_final_1 %>% dplyr::rename(lat = y, lon = x)

#### --- World Map Coordinates --- ####
### World coordinates, Excluded Antarctica
world <- map_data("world") %>% subset(region != 'Antarctica')

#### --- Second DF ---- ####
df2_original <- read.xlsx("/ihme/homes/chrish47/ENDO_epi_lit_GBD2021_Oct192020_For Upload.xlsx") #[Natalie's First-orinal DF]
df2_original <- df2_original %>% select(nid,location_id, measure, cases, sample_size, year_start, year_end)
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
location_data <- location_data %>% select(location_id, location_name, location_type)
df2 <- merge(df2_original, location_data, by = "location_id")

### Getting Centroid Spatial Coordinate
map <- getMap(resolution="high")
centroids <- gCentroid(map, byid=TRUE)
### Setting up the Centroid Coordinates to a DF
centroids_df <- as.data.frame(centroids)
setDT(centroids_df, keep.rownames = TRUE)[]
centroids_df <- dplyr::rename(centroids_df, location_name = rn)

# admin0 locations only!
df2 <- df2 %>% subset(location_type == 'admin0')

### Aggregating Study Counts(Additiona layer)
### For Prevalence Only
plot_p <- df2 %>% subset(measure == 'prevalence')
plot_p <- plot_p %>% select(8,4,5,2) # years were excluded, but can be included for future requests!
plot_p <- aggregate(.~ location_name + nid, plot_p, sum) # Sum of sample_size and cases by nid and location
plot_p <- plot_p %>% dcast(location_name + cases + sample_size ~ nid, value.var = 'nid', length)
plot_p <- aggregate(.~location_name, plot_p, sum)
plot_p <- plot_p %>% mutate(study_count = select(., 4:ncol(plot_p)) %>% rowSums(na.rm = TRUE))
plot_p <- plot_p %>% select(1:3, ncol(plot_p))

### Checking column value(locations) name differences - These will need to be altered!
differences <- setdiff(plot_p$location_name, centroids_df$location_name)
print(differences)

plot_p <- plot_p %>% mutate(location_name = recode(str_trim(location_name), 
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
### Checking for Differences for verification
differences2 <- setdiff(plot_p$location_name, centroids_df$location_name)
print(differences2)

### Merging Centroid DF2 and Prevalent Table
plot_p_final_2 <- merge(plot_p, centroids_df, by = 'location_name')
plot_p_final_2 <- plot_p_final_2 %>% dplyr::rename(lat = y, lon = x)


####-- Saving and Reading Files --#############################################################################################
# The following code is for reproducible and flexibility
# In collaboration with Natalie(RGUD) and for Testing purposes, I want to make this functional to work locally(not in the cluster).
###############################################################################################################################

### Generating .xlsx outputs
write.xlsx(plot_p_final_1, "/home/j/temp/chrish47/plot_p_final_1.xlsx")
write.xlsx(plot_p_final_2, "/home/j/temp/chrish47/plot_p_final_2.xlsx")

### Reading .xlsx tables -- Not necessary If you have these already in your environment!
plot_p_final_1 <- read.xlsx("/home/j/temp/chrish47/plot_p_final_1.xlsx")
plot_p_final_2 <- read.xlsx("/home/j/temp/chrish47/plot_p_final_2.xlsx")

#### --- Plotting --- ############################################################################################################
##################################################################################################################################
### --- PLots(plus layers) and Independent Legends --- ###
### For flexibility and functionality, the Main plots(with layers) and Legends had to be plotted independently!
### This is still being Developed!

### Only Plot
p0 <- ggplot() +
  # World Map Empty Outline
  geom_map(
    data = world, 
    map = world,
    aes(x = long, y = lat, group = group, map_id=region),
    color = "black", fill = "antiquewhite",
    size = 0.1 #Boldness of the country lines
  ) +
  scale_size(range = c(3, 5)) +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme_void() +
  labs(title = "Add Title Here",
       subtitle = "Add Subtitle Here",
       caption = "Add caption here") +
  theme(plot.title = element_text(size=15, hjust=0.5, face="bold", colour="black"), #title edits
        plot.subtitle = element_text(size=10, hjust=0.5, colour="black")) +#subtitle edits
  # Sample size and Number of Cases layer
  geom_point(data = plot_p_final_1,
             aes(x = lon, y = lat, size = sample_size, color = cases),
             position=position_jitter(h=0, w=0), # changing the position of the sample size bubbles
             stroke = FALSE, #Can change the outline of the sample size bubbles
             alpha = 0.1) +
  scale_size_continuous(range = c(10, 35), name = 'Sample Size') + # Bubble Size
  scale_color_gradient(low = "blue", high = "red", name = "Numer of Cases") + #scale_color_continous or other gradient f() 
  new_scale_color() +
  # Study counts layer
  geom_point(data = plot_p_final_2,
             aes(x = lon, y = lat, color = cut(study_count, c(-Inf, 2, 4, Inf))), #Fix this cut and below lines accordingly!
             #size = 1.5,
             show.legend = TRUE,
             alpha = 1) +
  scale_color_manual(name = "Study Counts",
                     values = c("red", "orange", "green", "green", "pink"), # Edit as necessary
                     labels = c("1-2", "3-4", "5>")) +
  scale_fill_manual(guide = guide_legend(reverse = TRUE)) +
  # Study type layer
  geom_point(data = plot_p_final_1, 
             aes(x = lon, y = lat, shape = study_type),
             position=position_jitter(h=1.5, w=1.5), #position = position_dodge(2)
             #size = 1.5
  ) +
  scale_shape_manual(name = 'Study Type', values = c(5, 10, 0), labels = c('Other', 'Self Report', 'Surgical Confirmation')) +
  theme(legend.position = "none")

### Legend for 1st Layer (Sample Size and Number of Cases)
p1 <- ggplot()+
  geom_point(data = plot_p_final_1,
             aes(x = lon, y = lat, size = sample_size, color = cases),
             position=position_jitter(h=0, w=0), # changing the position of the sample size bubbles
             stroke = FALSE, #Can change the outline of the sample size bubbles
             alpha = 0.1) +
  scale_size_continuous(range = c(10, 35), name = 'Sample Size') + # Bubble Size
  #scale_color_gradient2(name = "Number of Cases", limits = c(0, 50000), breaks = c(0, 5, 1000, 25000, 50000), low="blue", high="red", oob = scales::squish) +
  scale_color_continuous(name = "Number of Cases", limits = c(0, 50000), breaks = c(0, 25000, 50000), low = "blue", high = "red") + # do quantile() #values =??
  theme(legend.direction = "horizontal", legend.key=element_blank(), legend.position = "bottom")
#theme(legend.position = 'bottom', text = element_text(color = "black"), legend.key.size = unit(0.2, "cm"))

leg1 <- get_legend(p1 + guides(size = "none"))
leg2 <- get_legend(p1 + guides(color = "none"))
plot_grid(leg1)
### Legend for 2nd Layer for Study Counts
p2 <- ggplot() +
  geom_point(data = plot_p_final_2,
             aes(x = lon, y = lat, color = cut(study_count, c(-Inf, 2, 4, Inf))), #Fix this cut and below lines accordingly!
             #size = 1.5,
             show.legend = TRUE,
             alpha = 1) +
  scale_color_manual(name = "Study Counts",
                     values = c("red", "orange", "green", "green", "pink"), # Edit as necessary
                     labels = c("1-2", "3-4", "5>")) +
  scale_fill_manual(guide = guide_legend(reverse = TRUE)) +
  theme(legend.direction = "horizontal", legend.key=element_blank())
#theme(legend.position = 'bottom', text = element_text(color = "black"), legend.key.size = unit(0.5, "cm"))# Edit as necessary

leg3 <- get_legend(p2)

### Legend for 3rd Layer for Study Type
p3 <- ggplot() +
  geom_point(data = plot_p_final_1, 
             aes(x = lon, y = lat, shape = study_type),
             position=position_jitter(h=1.5, w=1.5), #position = position_dodge(2),
             #size = 1.5
  ) +
  scale_shape_manual(name = 'Study Type', values = c(5, 10, 0), labels = c('Other', 'Self Report', 'Surgical Confirmation')) +
  theme(legend.direction = "horizontal", legend.key=element_blank())

leg4 <- get_legend(p3)

#### --- Combining Legends ---- ##########################################
# Important Notes
# (1) For the Sample Size Legend, I changed the bubble sizes, in order for them to fit better. Issue with this, is that it doesn't align with the 
# actual bubble size on the World Map plot. For testing purposes this was done. But 
#########################################################################
legcombine <- plot_grid(leg1, leg2, leg3, leg4, nrow = 1, ncol = 4, rel_widths = c(0.3, 0.8, 0.3, 0.5))
#legcombine

### Putting everything together (Plot & Legends)
final_p <- plot_grid(p0, legcombine, nrow = 2, ncol = 1, rel_heights = c(1, 0.12))

print(final_p)

### Saving Plot --- We can have more than world map on the PDF if necessary - Confirm with Natalie(RGUD)
# - Review old code for getting multiple World Maps on the same PDF file.
pdf_path <- "/homes/chrish47/Bubble_Map_2022.pdf" # CHANGE THIS ACCORDINGLY
ggsave(filename = pdf_path, width = 20, height = 10)
dev.off()

# ##################### UNCOMMENT THIS SECTION FOR LEGENDS ON THE SIDE ################################ -- TESTING WORKBENCH
# ### Aligning the legends ###Keeping the legends on the side ### This is the only solution I have so far!
# ### Automating this will be tough! As it requires to adequately center and fit the legends. More work neded for this to work properly.
# leg1_grob <- ggplotGrob(plot_grid(leg1))
# leg2_grob <- ggplotGrob(plot_grid(leg2))
# leg3_grob <- ggplotGrob(plot_grid(leg3))
# 
# p1+annotation_custom(leg2_grob, xmin = -200, ymin = -60) +
#   annotation_custom(leg3_grob, xmin = -233, ymin = -85) +
#   annotation_custom(leg1_grob,xmin = -235, ymin = -65)