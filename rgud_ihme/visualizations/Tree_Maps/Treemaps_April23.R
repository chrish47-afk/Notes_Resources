#Treemaps that show: 
#the total burden related to HBV, the total burden related to HCV (shaded within the GBD results)
#the RGUD portfolio (shaded within the GBD results), shading our causes and shading the portions of other causes that are parts of our impairments
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

library(dplyr)
library(purrr)
library(plyr)
library(data.table)
library(openxlsx)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

################################################################################################################################ This ss what I want!
# TEST / EXAMPLE
# read data
# this excludes autocoded nodes (can be selected when exporting data from NVivo)
df <- read.csv("https://raw.githubusercontent.com/mpoudyal/test-data/main/data/nvivo/ex_proj_codes.csv") 
glimpse(df) #check what you've just imported
names(df)[2:3] <- c("cref", "agg_cref") # simple naming for code frequency columns
df <- df[-c(4,5)] # remove unnecessary columns

# remove "Codes\\" string from the `Codes` column
df$Codes <- gsub("Codes\\\\", "", df$Codes, fixed=TRUE)

# prepare data for plotly treemap
# separate nodes (coding terms) into different columns, this is needed as NVivo exports hierarchical coding as single string with `\` separator
df <- df %>%
  separate(.,
           col = Codes,
           into = c("l1node", "l2node","l3node","l4node"),
           sep = "\\\\",
           remove = FALSE,
           extra = "merge")
df <- df %>%
  mutate(ids = case_when(
    !is.na(l4node) ~ paste0(l3node,"-",l4node),
    (is.na(l4node) & !is.na(l3node)) ~ paste0(l2node,"-",l3node),
    (is.na(l3node) & !is.na(l2node)) ~ paste0(l1node,"-",l2node),
    TRUE ~ l1node
  )) %>%
  mutate(labels = case_when(
    !is.na(l4node) ~ l4node,
    (is.na(l4node) & !is.na(l3node)) ~ l3node,
    (is.na(l3node) & !is.na(l2node)) ~ l2node,
    TRUE ~ l1node
  )) %>%
  mutate(parents = case_when(
    labels == l1node ~ "",
    labels == l2node ~ l1node,
    labels == l3node ~ paste0(l1node,"-",l2node),
    labels == l4node ~ paste0(l2node,"-",l3node)
  ))
df

# basic treemap
fig <- plot_ly(
  type = "treemap",
  ids = df$ids,
  labels = df$labels,
  parents = df$parents,
  values = df$cref,
  textinfo = "label+value")

# customise the plot with title and annotations
fig <- fig %>% 
  layout(title = list(text = "Treemap of all coding*",
                      xref = "paper", yref = "paper"),
         annotations = list(x = 1, y = -0.05,
                            text = "*Numbers indicate frequency of occurence for the code",
                            showarrow = F, xref = "paper", yref = "paper",
                            font = list(size = 12, color = "charcoal")))
fig

#https://plotly.com/r/text-and-annotations/

################################################################################################################################
# GBD
library(RMySQL)
library(dplyr)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))


dt <- get_cause_metadata(cause_set_id = 2, release_id = 6)

dt_cause <- get_outputs(topic = "cause", cause_id = "all", rei_id = "all", release_id = 6, location_id = "all", year_id = "all", sex_id = "all", measure_id = "all", age_group_id = "all", compare_version_id = 7244) 
#102(USA) #location_id = "all" for all locations


burden <- get_outputs("rei", rei_id = "all", cause_id = "all", release_id = 6, location_id = "all", year_id = "all", sex_id = "all", measure_id = "all", age_group_id = "all", compare_version_id = 7244) 
#102(USA) #location_id = "all" for all locations
#rei_id = 198(infertility) #red_id = "all' for all r-o



# Imitating results from: https://vizhub.healthdata.org/gbd-compare/#
dt_cause <- get_outputs(topic = "cause", cause_id = 367, rei_id = "all", release_id = 6, location_id = 1, year_id = 2019, sex_id = 3, measure_id = "all", age_group_id = 22, compare_version_id = 7244, metric_id = 2) 
#102(USA) #location_id = "all" for all locations

# Impairment, 0% for maternal hemorrhage
burden <- get_outputs("rei", rei_id = 198, cause_id = 367, release_id = 6, location_id = 1, year_id = 2019, sex_id = 3, measure_id = "all", age_group_id = 22, compare_version_id = 7244) # This will spit out an error, because its an empty table! - For Infertility at least. 


# All Causes and Impairments! 
gbd_compare2019_causes <- get_outputs(topic = "cause", cause_id = "all", release_id = 6, location_id = 1, year_id = 2019, sex_id = 3, measure_id = 3, age_group_id = 22, metric_id = 1, compare_version_id = 7244) #All causes
gbd_compare2019_impairments <- get_outputs(topic = "rei", rei_id = "all", cause_id = "all", release_id = 6, location_id = 1, year_id = 2019, sex_id = 3, measure_id = 3, age_group_id = 22, metric_id = 1, compare_version_id = 7244) #causes with infertiltiy impairment(RGUD impairment)


#################### RGUD Causese and Impairments
cause_metadata <- get_cause_metadata(cause_set_id = 2, release_id = 6)
cause_metadata_1 <- cause_metadata %>% select(cause_id, path_to_top_parent) 
cause_metadata_2 <- cause_metadata %>% select(cause_id, cause_name)
cause_metadata_3 <- cause_metadata %>% select(cause_id, cause_name, path_to_top_parent)
cause_metadata_4 <- cause_metadata %>% select(cause_id, cause_name, level, path_to_top_parent)
impairment_metadata <- get_cause_metadata(cause_set_id = 9, release_id = 6)
########################
# From Rachel: I:\RTs_and_Projects\GBD\Teams\RGUD\Team management\Team information and tracking\GBD_RGUD_Assignments_Rotation_2023Apr
cause_id <- c(393,394,395,396,397,398,399,400,401,402,403,404,962,366,367,368,369,370,995,374,375,376,741,379,526,521,522,523,524,971,525,992,527,528,536,529,530,531,532,1024,1025,533,534,535,541,640,588,594,595,596,597,598,602,603,604,605,606,607,608,609,612,619,1032,625)

dt_rgud_causes <- data.table(cause_id)
dt_rgud_causes <- merge(dt_rgud_causes, cause_metadata_4, by = "cause_id")
dt_rgud_causes <- dt_rgud_causes %>% dplyr::select(cause_id, level, cause_name) %>% mutate(group = "RGUD")

cause_metadata_edit <- cause_metadata_4 %>% select(cause_id, cause_name, level)
gbd_compare2019_causes_edit <- gbd_compare2019_causes %>% select(cause_id, cause_name, val)
gbd_compare2019_causes_edit <- merge(gbd_compare2019_causes_edit, cause_metadata_edit, by = c("cause_id", "cause_name"))
gbd_compare2019_causes_edit <- gbd_compare2019_causes_edit 

final_dt <- left_join(gbd_compare2019_causes_edit, dt_rgud_causes, by = c("cause_id", "cause_name", "level"))
final_dt$group[is.na(final_dt$group)] <- "Others"

dt_input <- final_dt %>% filter(level == 4) %>% unite(id, c("group", "cause_name"), remove = FALSE, sep = "-", na.rm = TRUE)
write.xlsx(dt_input, "/ihme/scratch/users/chrish47/dt_input_treemap.xlsx")
#######################

final_dt <- cause_metadata_1 %>% separate(path_to_top_parent, c("Level_0","Level_1", "Level_2", "Level_3", "Level_4")) #%>% left_join(cause_metadata_3, by = "cause_id")
final_dt$Level_0 <- as.numeric(final_dt$Level_0)
final_dt$Level_1 <- as.numeric(final_dt$Level_1)
final_dt$Level_2 <- as.numeric(final_dt$Level_2)
final_dt$Level_3 <- as.numeric(final_dt$Level_3)
final_dt$Level_4 <- as.numeric(final_dt$Level_4)

final_dt <- final_dt %>% select(-2)
colnames(final_dt)
# Level 1
final_dt_level1 <- final_dt %>% select(1, 2) %>% select(-cause_id) %>% rename("cause_id" = "Level_1") %>% left_join(cause_metadata_2, by = "cause_id") %>% rename("cause_id_level1" = "cause_id") %>% rename("cause_name_level1" = "cause_name") 
nrow(final_dt_level1)
# Level 2
final_dt_level2 <- final_dt %>% select(1, 3) %>% select(-cause_id) %>% rename("cause_id" = "Level_2") %>% left_join(cause_metadata_2, by = "cause_id") %>% rename("cause_id_level2" = "cause_id") %>% rename("cause_name_level2" = "cause_name")
nrow(final_dt_level2)
# Level 3
final_dt_level3 <- final_dt %>% select(1, 4) %>% select(-cause_id) %>% rename("cause_id" = "Level_3") %>% left_join(cause_metadata_2, by = "cause_id") %>% rename("cause_id_level3" = "cause_id") %>% rename("cause_name_level3" = "cause_name")
nrow(final_dt_level3)
# Level 4
final_dt_level4 <- final_dt %>% select(1, 5) %>% select(-cause_id) %>% rename("cause_id" = "Level_4") %>% left_join(cause_metadata_2, by = "cause_id") %>% rename("cause_id_level4" = "cause_id") %>% rename("cause_name_level4" = "cause_name")
nrow(final_dt_level4)

final_dt <- cbind(final_dt_level1, final_dt_level2, final_dt_level3, final_dt_level4)
final_dt <- cbind(cause_metadata_3, final_dt) 
final_dt_edit <- final_dt %>% unite(id, c("cause_name_level1", "cause_name_level2", "cause_name_level3", "cause_name_level4"), remove = FALSE, sep = "-", na.rm = TRUE) %>% select(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 5)
#final_dt <- final_dt[!is.na(cause_name_level1) & !is.na(cause_name_level2) & !is.na(cause_name_level3) & !is.na(cause_name_level4), id := paste(cause_name_level1, cause_name_level2, cause_name_level3, cause_name_level4, sep = '-')]
colnames(final_dt)
nrow(final_dt)
colnames(final_dt_edit)
nrow(final_dt_edit)

write.xlsx(final_dt, "/ihme/scratch/users/chrish47/gbdcompare_allcauses_rgud.xlsx")
write.xlsx(final_dt_edit, "/ihme/scratch/users/chrish47/gbdcompare_allcauses_rgud_id.xlsx")
write.xlsx(cause_metadata_4, "/ihme/scratch/users/chrish47/cause_metadata4.xlsx")

final_dt_edit <- final_dt_edit %>% select(cause_id, cause_name, cause_id_level4, cause_name_level4)


###

join_dt <- left_join(master_burden_sheet_final, gbd_compare2019_causes_edit, by = "cause_name")

join_dt_rowtotals <- left_join(master_burden_sheet, gbd_compare2019_causes_edit, by = c("cause_name", "cause_id")) %>% select(cause_id, cause_name, group, val, level) %>% filter(level %in% 3) %>% select(-level)
join_dt_rowtotals[is.na(join_dt_rowtotals)] <- 0

join_dt_RGUD <- join_dt_rowtotals %>% filter(group %in% "RGUD") %>% select(1:4) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_RGUD"))
join_dt_RGUD <- join_dt_RGUD %>% filter(group %in% 'Total_RGUD') %>% select(val) %>% mutate(cause_name = 'RGUD', id = "RGUD", group = "", cause_id = "")
join_dt_Others <- join_dt_rowtotals %>% filter(group %in% "Others") %>% select(1:4) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_Others"))
join_dt_Others <- join_dt_Others %>% filter(group %in% 'Total_Others') %>% select(val) %>% mutate(cause_name = 'Others', id = "Others", group = "", cause_id = "")

join_dt <- left_join(master_burden_sheet_final %>% filter(cause_name!=c("RGUD","Others")), gbd_compare2019_causes_edit, by = "cause_name")
join_dt <- rbind(join_dt, join_dt_RGUD, join_dt_Others)

write.xlsx(join_dt, "/ihme/scratch/users/chrish47/others_rgud_cause_treemap_data.xlsx")
join_dt <- read.xlsx("/ihme/scratch/users/chrish47/others_rgud_cause_treemap_data.xlsx")
dt_input <- read.xlsx("/ihme/scratch/users/chrish47/gbdcompare_allcauses_rgud_edit.xlsx")
dt_input <- read.xlsx("/ihme/scratch/users/chrish47/dt_input_treemap.xlsx")

# basic treemap
fig <- plot_ly(
  type = "treemap",
  ids = join_dt$id,
  labels = join_dt$cause_name,
  parents = join_dt$group,
  values = join_dt$val,
  textinfo = "label+value",
  textfont = list(color = 'Black', size = 15))
fig

# customise the plot with title and annotations
#https://plotly.com/r/reference/layout/
fig <- fig %>% 
  layout(treemapcolorway = c("#99d8c9", "#fc9272"), #https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
         title = list(text = "The RGUD portfolio (shaded within the GBD results) - Number of YLDs",
                      size = 5,
                      family = "Balto",
                      xref = "paper", yref = "paper"),
         annotations = list(x = 1, y = -0.05,
                            text = "Level 3 and Level 4 cause data",
                            showarrow = F, xref = "paper", yref = "paper",
                            font = list(size = 12, color = "charcoal")))
fig

#https://stackoverflow.com/questions/69678677/r-plotly-treemap-color-only-lowest-labels
#https://stackoverflow.com/questions/21233477/performance-clustermap-in-r