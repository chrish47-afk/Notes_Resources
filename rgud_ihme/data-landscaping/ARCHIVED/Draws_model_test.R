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


year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

###################################################
# Using get_model_results()
###################################################

location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
#location_data_input <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))
location_data_input <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_id == 102)

## Incidence Ratios using get_model_results()
dt <- get_model_results(gbd_team = "epi", gbd_id = 27714, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2, measure_id = 18, year_id = year_id)

dt_incidence_ratios <- dt %>%
  merge(location_data_input, by = "location_id") %>%
  rename(
    "Incidence_ratios" = "mean",
    "Incidence_ratios_lower" = "lower",
    "Incidence_ratios_upper" = "upper"
  ) %>%
  select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Incidence_ratios, Incidence_ratios_lower, Incidence_ratios_upper)

###################################################
# Using get_draws()
###################################################
# Pulling ASFR and Female Population for calculating Counts.
asfr <- get_covariate_estimates(covariate_id = 13,
                                year_id = year_id,
                                age_group_id = most_detailed_ages,
                                release_id = 16,
                                sex_id = 2,
                                location_id = "all"
)
asfr <- asfr %>% select(location_id, year_id, age_group_id, mean_value) %>% rename("asfr" = "mean_value")
fem_pop <- get_population(sex_id = 2,
                          age_group_id = most_detailed_ages,
                          location_id = "all",
                          year_id = year_id,
                          release_id = 16,
                          status = "best"
)
fem_pop <- fem_pop %>% select(location_id, year_id, age_group_id, population) %>% rename("fem_pop" = "population")
# Pulling Ages for Sorting
ages <- get_ids(table = "age_group")

year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Sourcing in Draw and Location data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data %>% filter(location_type %in% c("admin0", "nonsovereign", "region", "superregion", "global")) %>% select(sort_order, location_id, location_name)
#location_data <- location_data %>% select(location_id, region_name, super_region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")
location_data_county_names <- location_data %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% filter(location_type %in% c("admin0", "nonsovereign")) %>% select(location_name, country)
location_data <- location_data %>% select(location_id, location_name, ihme_loc_id, region_name, super_region_name, location_type) %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% left_join(location_data_county_names, by = "country") %>% rename(country_name = location_name.y) %>% select(location_id, region_name, super_region_name, location_type, country_name) %>% filter(!is.na(country_name)) %>% mutate(global = "Global")

# This fixes the issue of double counting admin0 with subnationals
subnational_types <- c("admin1", "admin2", "ethnicity", "nonsovereign", "subnational", "urbanicity")
location_data <- location_data %>%
  group_by(country_name) %>%
  mutate(
    has_subnational = any(location_type %in% subnational_types)
  ) %>%
  ungroup() %>%
  filter(
    location_type %in% subnational_types |                     # keep all subnationals
      (location_type == "admin0" & !has_subnational)             # keep admin0 only if no subnationals exist
  ) %>% select(-location_type, -has_subnational)


# Doing USA only
location_data <- location_data %>% filter(country_name == "United States of America")

# step 1: for hemorrhage, US, pull the incidence ratio for every subnational location
### Pulling Draws
##############################################
dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
                     gbd_id=1535,
                     source='epi',
                     year_id=year_id,
                     age_group_id=most_detailed_ages,
                     sex_id=2,
                     release_id = release_id,
                     measure_id = 6,
                     location_id = location_data$location_id
)

df_subset <- get_cols(dt_draw)
df_subset <- df_subset %>% right_join(location_data, by = "location_id") %>% select(-region_name, -super_region_name, -global)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(country_name),  # Columns to stack
                      values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)

# Incidence counts = incidence ratio * livebirths (asfr * fem_pop)
# step 2: pull female population and asfr for every US subnational location to get the number of livebirths for those locations
# step 3: get the incidence counts for each subnational location by: incidence ratio * livebirth
df_subset_new <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id"))
df_subset_new <- df_subset_new %>% merge(fem_pop, by = c("location_id", "year_id", "age_group_id"))
df_subset_new <- df_subset_new %>% mutate(livebirths = asfr*fem_pop) %>% select(-asfr, -fem_pop)
draw_cols <- paste0("draw_", 0:999)
df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * livebirths), 
                               .SDcols = draw_cols,
                               by = c("location_id", "year_id", "age_group_id")]
# step 4: get aggregated incidence ratio for US= the sum of incidence counts for all subnational locations/the sum of livebirths for all subnational locations
df_subset_new <- df_subset_new %>%
  group_by(location_name, year_id, age_group_id) %>%
  summarise(
    livebirths = sum(livebirths, na.rm = TRUE),
    across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )
df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ .x / livebirths))

# Calcualting Counts mean, lower, and upper for draws after calculations.
result_incidence_ratios <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id", "livebirths"))
result_incidence_ratios <- result_incidence_ratios %>%
  merge(ages, by = "age_group_id") %>%
  arrange(year_id, age_group_id) %>%
  rename("Incidence_ratios" = "val", "Incidence_ratios_lower" = "lower", "Incidence_ratios_upper" = "upper")

View(result_incidence_ratios)
View(dt_incidence_ratios)
write.csv(result_incidence_ratios, "/mnt/share/mmbrf/Deliverables/Archived/using_draws_plus_calculations_USA.csv")
write.csv(dt_incidence_ratios, "/mnt/share/mmbrf/Deliverables/Archived/using_get_model_results_USA.csv")

# # step 4: get aggregated incidence ratio for US= the sum of incidence counts for all subnational locations/the sum of livebirths for all subnational locations
# result_incidence_ratios <- result_incidence_counts %>%
#   group_by(location_name, year_id, age_group_id) %>%
#   summarise(
#     livebirths = sum(livebirths, na.rm = TRUE),
#     val_counts        = sum(Incidence_counts, na.rm = TRUE),
#     lower_counts      = sum(Incidence_counts_lower, na.rm = TRUE),
#     upper_counts      = sum(Incidence_counts_upper, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Incidence_ratio   = val_counts / livebirths,
#     Incidence_ratio_lower = lower_counts / livebirths,
#     Incidence_ratio_upper = upper_counts / livebirths
#   )

#############################################################################################################################################
# Testing for all countries, regions, etc.
#############################################################################################################################################
ages <- get_ids(table = "age_group")

year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Sourcing in Draw and Location data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data %>% filter(location_type %in% c("admin0", "nonsovereign", "region", "superregion", "global")) %>% select(sort_order, location_id, location_name)
#location_data <- location_data %>% select(location_id, region_name, super_region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")
location_data_county_names <- location_data %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% filter(location_type %in% c("admin0", "nonsovereign")) %>% select(location_name, country)
location_data <- location_data %>% select(location_id, location_name, ihme_loc_id, region_name, super_region_name, location_type) %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% left_join(location_data_county_names, by = "country") %>% rename(country_name = location_name.y) %>% select(location_id, region_name, super_region_name, country_name) %>% filter(!is.na(country_name)) %>% mutate(global = "Global")


### Pulling Draws
##############################################
dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
                     gbd_id=1535,
                     source='epi',
                     year_id=year_id,
                     age_group_id=most_detailed_ages,
                     sex_id=2,
                     release_id = release_id,
                     measure_id = 6,
                     location_id = location_data$location_id
)

df_subset <- get_cols(dt_draw)
df_subset <- df_subset %>% left_join(location_data, by = "location_id") #%>% select(-region_name, -super_region_name, -global)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(country_name, region_name, super_region_name, global),  # Columns to stack
                      values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)


### Incidence Ratio Average / by Region(admin0, countries, were used for calculations) ####
#########################################################################################
# Using get_draws. Using get_model_results() produces similar results - I tested it.
result_incidence_ratio_all <- mean_ui_columns(df_subset_melt, group_cols = c("year_id", "location_name", "age_group_id"))
result_incidence_ratio_all <- result_incidence_ratio_all %>%
  left_join(ages, by = "age_group_id") %>%
  rename("Incidence_ratios" = "val", "Incidence_ratios_lower" = "lower", "Incidence_ratios_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence_ratios", "Incidence_ratios_lower", "Incidence_ratios_upper")

result_incidence_ratio_all  <- result_incidence_ratio_all %>%
  left_join(location_data_sort, by = "location_name") %>%
  arrange(year_id, sort_order)

View(result_incidence_ratio)

#write.csv(result_incidence_ratio, "/mnt/share/mmbrf/Deliverables/April14_2025_maternal_nonfatal_table_draws_test.csv", row.names = FALSE)





