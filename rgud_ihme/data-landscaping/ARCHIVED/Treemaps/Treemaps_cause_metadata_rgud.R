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

cause_metadata <- get_cause_metadata(cause_set_id = 2, release_id = 6)
cause_metadata_1 <- cause_metadata %>% select(cause_id, path_to_top_parent) 
cause_metadata_2 <- cause_metadata %>% select(cause_id, cause_name, level, path_to_top_parent)

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
final_dt <- cbind(cause_metadata_2, final_dt) 
final_dt_edit <- final_dt %>% unite(id, c("cause_name_level1", "cause_name_level2", "cause_name_level3", "cause_name_level4"), remove = FALSE, sep = "-", na.rm = TRUE) %>% select(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 6)
#final_dt <- final_dt[!is.na(cause_name_level1) & !is.na(cause_name_level2) & !is.na(cause_name_level3) & !is.na(cause_name_level4), id := paste(cause_name_level1, cause_name_level2, cause_name_level3, cause_name_level4, sep = '-')]
colnames(final_dt)
nrow(final_dt)
colnames(final_dt_edit)
nrow(final_dt_edit)

# RGUD CAUSE IDs
# From Rachel: I:\RTs_and_Projects\GBD\Teams\RGUD\Team management\Team information and tracking\GBD_RGUD_Assignments_Rotation_2023Apr
cause_id <- c(393,394,395,396,397,398,399,400,401,402,403,404,962,366,367,368,369,370,995,374,375,376,741,379,526,521,522,523,524,971,525,992,527,528,536,529,530,531,532,1024,1025,533,534,535,541,640,588,594,595,596,597,598,602,603,604,605,606,607,608,609,612,619,1032,625)

dt_rgud_causes <- data.table(cause_id)
dt_rgud_causes <- merge(dt_rgud_causes, cause_metadata_2, by = "cause_id")
dt_rgud_causes <- dt_rgud_causes %>% dplyr::select(cause_id, level, cause_name) %>% mutate(group = "RGUD")


# FOR level 3 and 4 Only
# Joining All cause dt to RGUD cause dt
master_burden_sheet <- left_join(final_dt_edit, dt_rgud_causes, by =c("cause_id", "level", "cause_name"))
master_burden_sheet$group[is.na(master_burden_sheet$group)] <- "Others"
# Filtering in only level 3 and level 4 groups
master_burden_sheet <- master_burden_sheet %>% dplyr::filter(level %in% 3 | level %in% 4)
colnames(master_burden_sheet)
master_burden_sheet <- master_burden_sheet %>% select(cause_id, cause_name, level, path_to_top_parent, cause_id_level3, cause_name_level3, cause_id_level4, cause_name_level4, group)

# Level 4
master_burden_sheet_level4groups <- master_burden_sheet %>% dplyr::filter(level %in% 4) %>% unite(id, c("group", "cause_name_level3"), remove = FALSE, sep = "-", na.rm = TRUE) %>% select(cause_name, -group, id, level) %>% dplyr::rename("group" = "id")

# Level 3
master_burden_sheet_level4groups <- master_burden_sheet %>% dplyr::filter(level %in% 4) %>% unite(id, c("group", "cause_name_level3"), remove = FALSE, sep = "-", na.rm = TRUE) %>% select(cause_name, -group, id, level) %>% dplyr::rename("group" = "id")

# final
master_burden_sheet_final <- master_burden_sheet %>% unite(id, c("group", "cause_name_level3", "cause_name_level4"), remove = FALSE, sep = "-", na.rm = TRUE)
master_burden_sheet_final <- master_burden_sheet_final %>% select(cause_name, group, id, level, path_to_top_parent)
new <- data.table(cause_name = c("RGUD", "Others"), group = c("",""), id = c("RGUD", "Others"), level = c(0, 0), path_to_top_parent = c(0, 0))

master_burden_sheet_final <- rbind(new, master_burden_sheet_final)

# Editing level 4 data
master_burden_sheet_level4groups <- master_burden_sheet_level4groups %>% select(-level) %>% left_join(master_burden_sheet_final %>% select(-group), by = 'cause_name')

# Join
View(master_burden_sheet_final)
View(master_burden_sheet_level4groups)

master_burden_sheet_final <- master_burden_sheet_final %>% filter(level %in% c(0,3)) %>% select(-path_to_top_parent, -level)
master_burden_sheet_level4groups <- master_burden_sheet_level4groups %>% select(-path_to_top_parent, -level)

master_burden_sheet_final <- rbind(master_burden_sheet_final, master_burden_sheet_level4groups)

  
  
  
  