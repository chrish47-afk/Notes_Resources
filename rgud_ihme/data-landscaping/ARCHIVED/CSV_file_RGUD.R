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

library(dplyr)
library(data.table)
library(purrr)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

# location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
# location_data <- location_data %>% select(location_id, location_name, location_type)
 
# age_group_id_list <- c(7,8,9,10,11,12,13,14,15)
# year_id_list <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
# dt <- get_model_results(gbd_team = "epi", gbd_id = 1535, release_id = 16, year_id = 1990, location_id = 32, sex_id = 2, age_group_id = age_group_id_list)

################### Draw Level Calculations ##################################
###########################################################################


### Helper Functions for Nonfatal and GDM tables.
####################################

mean_ui <- function(draw_dt) { ### This function collapse everything across, by row. No 
  # Ensure draw_dt is a data.table
  library(data.table)
  dt <- as.data.table(copy(draw_dt))
  
  draws <- paste0("draw_", 0:999) # Adapt based on number of draws
  dt[, val := rowMeans(.SD), .SDcols = draws]
  dt[, lower := apply(.SD, 1, quantile, probs= 0.025), .SDcols = draws]
  dt[, upper := apply(.SD, 1, quantile, probs=0.975), .SDcols = draws]
  dt[, (draws) := NULL] # Remove draw columns after calculations
  return(dt)
}

mean_ui_columns <- function(draw_dt, pattern = "^draw_", group_cols = NULL, drop_draws = TRUE) {
  library(data.table)
  dt <- as.data.table(copy(draw_dt))
  
  # Identify draw columns
  draw_cols <- grep(pattern, names(dt), value = TRUE)
  if (length(draw_cols) == 0) stop("No draw columns found.")
  
  # Validate grouping columns
  if (!is.null(group_cols)) {
    missing <- setdiff(group_cols, names(dt))
    if (length(missing) > 0) stop("Missing group columns: ", paste(missing, collapse = ", "))
    
    # Collapse rows per group: average draws within group
    dt <- dt[, lapply(.SD, mean), by = group_cols, .SDcols = draw_cols]
  }
  
  # Then compute val/lower/upper from the averaged draws
  dt[, val := rowMeans(.SD), .SDcols = draw_cols]
  dt[, lower := apply(.SD, 1, quantile, probs = 0.025), .SDcols = draw_cols]
  dt[, upper := apply(.SD, 1, quantile, probs = 0.975), .SDcols = draw_cols]
  
  if (drop_draws) dt[, (draw_cols) := NULL]
  
  return(dt)
}

get_cols <- function(dt) {
  # Ensure dt is a data.table
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }
  
  # Identify draw columns present in the data.table
  draw_cols <- paste0("draw_", 0:999)
  draw_cols <- draw_cols[draw_cols %in% names(dt)]
  
  # Identify index columns
  index_cols <- c("location_id", "year_id", "age_group_id")
  index_cols <- index_cols[index_cols %in% names(dt)]
  
  # Combine columns to keep
  keep_cols <- c(draw_cols, index_cols)
  
  # Return the subset using .. to evaluate variable
  return(dt[, ..keep_cols])
}

####################################


#########################################################################################################################
# Nonfatal 
#########################################################################################################################

### Non-fatal Inputs
####################################
input_me <- c(1535,
              3085,
              3086,
              1543,
              1544,
              10504,
              1550,  
              1552,
              1555,
              10484,
              27398)
output_me_prev <- c(1536, 
                    1537,
                    3623,
                    3626,
                    2625,
                    3635,
                    1542,
                    3641,
                    1552,
                    3644,
                    10485,
                    27715)
measure_id <- 5 #5 (prevalence),6(incidence)
year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Sourcing in Draw and Location data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data %>% filter(location_type == "region") %>% select(sort_order, region_name)
location_data <- location_data %>% select(location_id, region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")

# Pulling ASFR, Female Pops, and Still Births Ratios
asfr <- get_covariate_estimates(covariate_id = 13,
                                year_id = year_id,
                                age_group_id = most_detailed_ages,
                                release_id = 16,
                                sex_id = 2,
)
asfr <- asfr %>% select(location_id, year_id, age_group_id, mean_value)
fem_pop <- get_population(sex_id = 2,
                          age_group_id = most_detailed_ages,
                          location_id = "all",
                          year_id = year_id,
                          release_id = 16,
                          status = "best"
)
fem_pop <- fem_pop %>% select(location_id, year_id, age_group_id, population)

# Pulling Ages for Sorting
ages <- get_ids(table = "age_group")

## Pull draws as necessary. Copies have been saved to save time. Look below.
for (me in input_me) {
  # Pulling Draws
  dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
                       gbd_id=me,
                       source='epi',
                       measure_id=measure_id,
                       #location_id=locs,
                       year_id=year_id,
                       age_group_id=most_detailed_ages,
                       sex_id=2,
                       release_id = release_id
                       #version_id = 907443
                       )
  saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_",me,".rds"))
  print("ME Draws Completed")
}

#me <- 1535
for (me in input_me) {
  # SaveRDS (uncomment if needed)
  # saveRDS(dt_draw, "/mnt/team/rgud/pub/users/chrish47/me_1535.rds")
  
  dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_incidence/me_", me, ".rds"))
  df_subset <- get_cols(dt_saved_draws)
  df_subset <- df_subset %>%
    merge(location_data, by = "location_id") %>%
    select(-location_type)
  
  ### Incidence Counts
  # Incidence Ratio * Livebirths / by Region(admin0, countries, were used for calculations)
  ####################################
  df_subset_new <- df_subset %>%
    merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>%
    rename(asfr = mean_value)
  
  df_subset_new <- df_subset_new %>%
    merge(fem_pop, by = c("location_id", "year_id", "age_group_id")) %>%
    rename(fem_pop = population)
  
  df_subset_new <- df_subset_new %>%
    mutate(livebirths = asfr * fem_pop) %>%
    select(-asfr, -fem_pop)
  
  draw_cols <- paste0("draw_", 0:999)
  df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * livebirths),
                                  .SDcols = draw_cols,
                                  by = c("location_id", "year_id", "age_group_id")]
  df_subset_new <- df_subset_new %>%
    select(-livebirths)
  
  result_incidence_counts <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "region_name", "age_group_id"))
  result_incidence_counts <- result_incidence_counts %>%
    merge(ages, by = "age_group_id") %>%
    rename("Incidence Counts" = "val", "IC_lower" = "lower", "IC_upper" = "upper") %>%
    select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Counts", "IC_lower", "IC_upper")
  
  ### Incidence Ratio Average / by Region(admin0, countries, were used for calculations)
  #########################################################################################
  result_incidence_ratio <- mean_ui_columns(df_subset, group_cols = c("year_id", "region_name", "age_group_id"))
  result_incidence_ratio <- result_incidence_ratio %>%
    merge(ages, by = "age_group_id") %>%
    rename("Incidence Ratios" = "val", "IR_lower" = "lower", "IR_upper" = "upper") %>%
    select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Ratios", "IR_lower", "IR_upper")
  
  ### Incidence Rate = Incidence ratio * ASFR / by Region(admin0, countries, were used for calculations)
  ##############################################
  df_subset_rate <- df_subset %>%
    merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>%
    rename(asfr = mean_value)
  
  draw_cols <- paste0("draw_", 0:999)
  df_subset_rate <- df_subset_rate[, (draw_cols) := lapply(.SD, function(x) x * asfr),
                                    .SDcols = draw_cols,
                                    by = c("location_id", "year_id", "age_group_id")]
  df_subset_rate <- df_subset_rate %>%
    select(-asfr)
  
  result_incidence_rate <- mean_ui_columns(df_subset_rate, group_cols = c("year_id", "region_name", "age_group_id"))
  result_incidence_rate <- result_incidence_rate %>%
    merge(ages, by = "age_group_id") %>%
    rename("Incidence Rate" = "val", "IRa_lower" = "lower", "IRa_upper" = "upper") %>%
    select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Rate", "IRa_lower", "IRa_upper")
  
  ### Combining all dataframes to one single table output
  ###########################################################
  # List your dataframes
  dfs <- list(result_incidence_ratio, result_incidence_counts, result_incidence_rate)
  
  # Reduce using inner_join
  merged_df <- reduce(dfs, inner_join, by = c("age_group_id", "age_group_name", "year_id", "region_name"))
  
  # Add location info separately
  final <- merged_df %>%
    inner_join(location_data_sort, by = "region_name") %>%
    arrange(year_id, sort_order) %>%
    select(-sort_order) %>%
    mutate(me = me)
  
  # Write to CSV
  write.csv(final, paste0("/mnt/team/rgud/pub/users/chrish47/CSV_files/", me, "_table.csv"), row.names = FALSE)
  print("csv table completed")
}


output_me_prev <- c(1536, 
                    1537,
                    3623,
                    3626,
                    2625,
                    3635,
                    1542,
                    3641,
                    1552,
                    3644,
                    10485,
                    27715)
measure_id <- 5 #5 (prevalence),6(incidence)
year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16


location_data <- get_location_metadata(location_set_id = 35, release_id = 16) 
location_data <- location_data %>% select(location_id, location_name, location_type) %>% filter(location_type == "region")

### Prevalence Data
##############################################
# Prevalence and YLDs. Get_model_results.
dt <- get_model_results(gbd_team = "epi",
                        gbd_id = 1536,
                        year_id = year_id, #1990, etc.
                        age_group_id = most_detailed_ages,
                        measure_id = 5,
                        sex_id = 2,
                        location_id = location_data$location_id,
                        release_id = release_id)



#########################################################################################################################
# GDM 
#########################################################################################################################
year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Sourcing in Draw and Location data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data %>% filter(location_type == "region") %>% select(sort_order, region_name)
location_data <- location_data %>% select(location_id, region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")

# Pulling ASFR, Female Pops, and Still Births Ratios
asfr <- get_covariate_estimates(covariate_id = 13,
                                year_id = year_id,
                                age_group_id = most_detailed_ages,
                                release_id = 16,
                                sex_id = 2,
)
asfr <- asfr %>% select(location_id, year_id, age_group_id, mean_value)
fem_pop <- get_population(sex_id = 2,
                          age_group_id = most_detailed_ages,
                          location_id = "all",
                          year_id = year_id,
                          release_id = 16,
                          status = "best"
)
fem_pop <- fem_pop %>% select(location_id, year_id, age_group_id, population)

# Pulling Ages for Sorting
ages <- get_ids(table = "age_group")


# Run this draw call only if necessary.
# An RDS file was saved to save time. Look below.
dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
                     gbd_id=27714,
                     source='epi',
                     year_id=year_id,
                     age_group_id=most_detailed_ages,
                     sex_id=2,
                     release_id = release_id,
                     version_id = 907443
                     )
#saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714.rds"))

### Pulling Saved Draws
##############################################
dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_incidence/me_27714.rds"))
df_subset <- get_cols(dt_saved_draws)
df_subset <- df_subset %>% merge(location_data, by = "location_id") #%>% select(-location_type)

### Incidence Ratio Average / by Region(admin0, countries, were used for calculations) ####
#########################################################################################
result_gdm_incidence_ratio <- mean_ui_columns(df_subset, group_cols = c("year_id", "region_name", "age_group_id"))
result_gdm_incidence_ratio <- result_gdm_incidence_ratio %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence Ratios" = "val", "IR_lower" = "lower", "IR_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Ratios", "IR_lower", "IR_upper")


### Incidence Counts = GDM draws * # of births (livebirth+stillbirth) for GDM###
# # of births (livebirth+stillbirth) for GDM =female population * ((ASFR * 46/52) + (stillbirth_livebirth_ratio *ASFR * 26/52))
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_new <- df_subset %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_new <- df_subset_new %>% merge(fem_pop, by = c("location_id", "year_id", "age_group_id")) %>% rename(fem_pop=population)
df_subset_new <- df_subset_new %>% merge(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_new <- df_subset_new %>% mutate(births = (fem_pop*asfr*(46/52)) + (sb_ratio*asfr*(26/52))) %>% select(-asfr, -fem_pop, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * births), 
                               .SDcols = draw_cols,
                               by = c("location_id", "year_id", "age_group_id")]
df_subset_new <- df_subset_new %>% select(-births)

result_gdm_incidence_counts <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "region_name", "age_group_id"))
result_gdm_incidence_counts <- result_gdm_incidence_counts %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence Counts" = "val", "IC_lower" = "lower", "IC_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Counts", "IC_lower", "IC_upper")

### Incidence Rate, Incidence ratio * ASFR / by Region(admin0, countries, were used for calculations) ###
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_rate <- df_subset %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_rate <- df_subset_rate %>% merge(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_rate <- df_subset_rate %>% mutate(factor = (asfr*(46/52) + (sb_ratio*asfr*(26/52)))) %>% select(-asfr, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_rate <- df_subset_rate[, (draw_cols) := lapply(.SD, function(x) x * factor), 
                                 .SDcols = draw_cols,
                                 by = c("location_id", "year_id", "age_group_id")]
df_subset_rate <- df_subset_rate %>% select(-factor)

result_gdm_incidence_rate <- mean_ui_columns(df_subset_rate, group_cols = c("year_id", "region_name", "age_group_id"))
result_gdm_incidence_rate <- result_gdm_incidence_rate %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence Rate" = "val", "IRa_lower" = "lower", "IRa_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "region_name", "Incidence Rate", "IRa_lower", "IRa_upper")

### Final Dataframe 
##############################################

dfs <- list(result_gdm_incidence_ratio, result_gdm_incidence_counts, result_gdm_incidence_rate)

# Reduce using inner_join
merged_df <- reduce(dfs, inner_join, by = c("age_group_id", "age_group_name", "year_id", "region_name")) #iteratively applying a function across a list of elements—reducing them down to a single result.

# Add location info separately
final <- merged_df %>%
  inner_join(location_data_sort, by = "region_name") %>%
  arrange(year_id, sort_order) %>%
  select(-age_group_id, -sort_order) %>%
  mutate(cause = "GDM in pregnant women (general female population)")
write.csv(final, paste0("/mnt/team/rgud/pub/users/chrish47/CSV_files/GDM_final_table.csv"), row.names = FALSE)

