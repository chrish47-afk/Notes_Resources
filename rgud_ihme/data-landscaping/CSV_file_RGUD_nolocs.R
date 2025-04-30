#############################################################
# Author: Christian Hernandez
# Date: April, 2025
# Purpose: The following script creates a csv file with all non-fatal maternal incident, prevalent, and YLDs data. At the global, super region, region, and admind0(country) level. By age_groups. GDM was done separately due to custom calcualtions.
#############################################################
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

###---------------------------------------------------------------------------------------------
### Helper Functions for Nonfatal and GDM tables. (USE AS NECESSARY)
###---------------------------------------------------------------------------------------------

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

###---------------------------------------------------------------------------------------------
### Non-fatal Sections (Except for GDM)
### 1. Incidence Ratios and Counts, were calculated using Draws
### 2. Incidence Rate, Prevalence Counts, and Prevalence Rates were calcualted pulled or calculated using get_model_results()
### 3. YLDs data was pulled in using get_outputs. Besides summing up sequela level data, no additional calculations were performed.
###---------------------------------------------------------------------------------------------

### Non-fatal Inputs
input_incidence_mes <- c(1535,
                         3085,
                         3086,
                         1543,
                         1544,
                         10504,
                         1550,
                         1552,
                         1555,
                         10484,
                         27398
                         )
output_incidence_mes <- c(3620,
                          3623,
                          3626,
                          3629,
                          3635,
                          1542,
                          3641,
                          1552,
                          3644,
                          10485,
                          28393
                          )
output_prevalence_mes <- c(1536,1537,
                           3623,
                           3626,
                           2625,
                           3635,
                           1542,
                           3641,
                           1552,
                           3644,
                           10485,
                           27705, 27702, 27703, 27704,
                           32529, 32531,32532, #Miscarriage, updated
                           28983, 28985, 28986 #Ectopic, updated
                           )
# Define ME groups (each element is a group to be summed)
sequelae_groups <- list(
  group_1 = c(180, 181, 182, 183, 184),
  group_2 = c(186, 677),
  group_3 = c(675, 23488, 23489, 23490, 937),
  group_4 = c(938),
  group_5 = c(676),
  group_6 = c(185, 187),
  group_7 = c(188),
  group_8 = c(189, 190),
  group_9 = c(191),
  group_10 = c(5165),
  group_11 = c(23464, 23465, 23466, 23467)
  )

year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Location data for the non-draw data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_input <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))


# Location data for Incidence Ratio and Counts seection
location_data_draws <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data_draws %>% filter(location_type %in% c("admin0", "nonsovereign", "region", "superregion", "global")) %>% select(sort_order, location_id, location_name)
#location_data <- location_data %>% select(location_id, region_name, super_region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")
location_data_county_names <- location_data_draws %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% filter(location_type %in% c("admin0", "nonsovereign")) %>% select(location_name, country)
location_data_draws <- location_data_draws %>% select(location_id, location_name, ihme_loc_id, region_name, super_region_name, location_type) %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% left_join(location_data_county_names, by = "country") %>% rename(country_name = location_name.y) %>% select(location_id, region_name, super_region_name, location_type, country_name) %>% filter(!is.na(country_name)) %>% mutate(global = "Global")
# This fixes the issue of double counting admin0 with subnationals
subnational_types <- c("admin1", "admin2", "ethnicity", "nonsovereign", "subnational", "urbanicity")
location_data_draws <- location_data_draws %>%
  group_by(country_name) %>%
  mutate(
    has_subnational = any(location_type %in% subnational_types)
  ) %>%
  ungroup() %>%
  filter(
    location_type %in% subnational_types |                     # keep all subnationals
      (location_type == "admin0" & !has_subnational)             # keep admin0 only if no subnationals exist
  ) %>% select(-location_type, -has_subnational)

# This is used to solved some of the location crashes between North Africa and Middles East and South Asia. Its used when using get_model_results() to match with draws dataframe.
location_data_fix <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_input_fix <- location_data_fix %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))

# Pulling ASFR and Female Population for calculating Counts.
asfr <- get_covariate_estimates(covariate_id = 13,
                                year_id = year_id,
                                age_group_id = most_detailed_ages,
                                release_id = 16,
                                sex_id = 2,
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

###---------------------------------------------------------------------------------------------
## Incidence Ratios (Incidence per Livebirths) - Input ME_ids - Using get_draws()
## Incidence Counts (Incidence x Livebirths(asfr*fem_pop)- Input ME_ids - Using get_draws()
## Incidence Rate(Incidence per Population) - Output ME_ids - Using get_model_results()
###---------------------------------------------------------------------------------------------

# Initialize an empty list to store merged outputs
incidence_results_list <- list()

for (i in seq_along(input_incidence_mes)) {
  me_i <- input_incidence_mes[i]
  me_o <- output_incidence_mes[i]
  
  ########################################################################
  ### Pulling Saved Draws
  ########################################################################
  dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_incidence/me_",me_i,".rds"))
  df_subset <- get_cols(dt_saved_draws)
  df_subset <- df_subset %>% merge(location_data_draws, by = "location_id") #%>% select(-location_type)
  
  # Melting data. Location_name will included super_region_name, region_name, and country_name.
  df_subset_melt <- df_subset %>%
    tidyr::pivot_longer(cols = c(global, super_region_name, region_name, country_name),  # Columns to stack
                        values_to = "location_name"                              # New column with the value
    ) %>% select(-name)
  df_subset_melt <- as.data.table(df_subset_melt)
  
  # Incidence counts = incidence ratio * livebirths (asfr * fem_pop)
  # Incidence ratios = sum(incidence ratio * livebirths)/sum(livebirths) each location(country, region, superregion, and global)
  # step 2: pull female population and asfr for every US subnational location to get the number of livebirths for those locations
  # step 3: get the incidence counts for each subnational location by: incidence ratio * livebirth
  df_subset_new <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id"))
  df_subset_new <- df_subset_new %>% merge(fem_pop, by = c("location_id", "year_id", "age_group_id"))
  df_subset_new <- df_subset_new %>% mutate(livebirths = asfr*fem_pop) %>% select(-asfr, -fem_pop)
  draw_cols <- paste0("draw_", 0:999)
  df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * livebirths), 
                                 .SDcols = draw_cols,
                                 by = c("location_id", "year_id", "age_group_id")]
  # Summing(count draws) and Summing(births) by location_name, year_id, and age_group_id.
  df_subset_new <- df_subset_new %>%
    group_by(location_name, year_id, age_group_id) %>%
    summarise(
      livebirths = sum(livebirths, na.rm = TRUE),
      across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  dt_incidence_counts <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id"))
  dt_incidence_counts <- dt_incidence_counts %>%
    merge(ages, by = "age_group_id") %>%
    rename("Incidence_counts" = "val", "Incidence_counts_lower" = "lower", "Incidence_counts_upper" = "upper") %>%
    select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence_counts", "Incidence_counts_lower", "Incidence_counts_upper")
  
  ### Incidence Ratios
  # step 4: sum of incidence counts for all subnational locations/the sum of livebirths for all locations
  # df_subset_new <- df_subset_new %>%
  #   group_by(location_name, year_id, age_group_id) %>%
  #   summarise(
  #     livebirths = sum(livebirths, na.rm = TRUE),
  #     across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)),
  #     .groups = "drop"
  #   )
  df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ .x / livebirths))

  # Calculating Incidence Ratio mean, lower, and upper for draws after calculations.
  dt_incidence_ratios <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id"))
  dt_incidence_ratios <- dt_incidence_ratios %>%
    merge(ages, by = "age_group_id") %>%
    arrange(year_id, age_group_id) %>%
    rename("Incidence_ratios" = "val", "Incidence_ratios_lower" = "lower", "Incidence_ratios_upper" = "upper")
  
  ########################################################################
  ## Incidence Rate (Output ME)
  dt <- get_model_results(gbd_team = "epi", gbd_id = me_o, measure_id = 6, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2, year_id = year_id)
  
  dt_incidence_rate <- dt %>%
    merge(location_data_input_fix, by = "location_id") %>%
    rename(
      "Incidence_rate" = "mean",
      "Incidence_rate_lower" = "lower",
      "Incidence_rate_upper" = "upper"
    ) %>%
    select(modelable_entity_id, age_group_id, year_id, location_name, Incidence_rate, Incidence_rate_lower, Incidence_rate_upper) %>% unique()
  
  ########################################################################
  ## Merge all three outputs into a final table
  dt_incidence_merged <- dt_incidence_ratios %>%
    merge(dt_incidence_counts, by = c("age_group_id", "age_group_name", "year_id", "location_name")) %>%
    merge(dt_incidence_rate,   by = c("age_group_id", "year_id", "location_name"))
  
  # Optionally store each result with a named entry
  result_name <- paste0("input_", me_i, "_output_", me_o)
  incidence_results_list[[result_name]] <- dt_incidence_merged
  print("Done, next one")
}

########################################################################
## Appending all tables in list
final_incidence_table <- dplyr::bind_rows(incidence_results_list, .id = "source_pair") %>%
  left_join(location_data_sort, by = "location_name") %>%
  arrange(source_pair, year_id, sort_order)

incidence_order <- c("input_1535_output_3620",
                     "input_3085_output_3623",
                     "input_3086_output_3626",
                     "input_1543_output_3629",
                     "input_1544_output_3635",
                     "input_10504_output_1542",
                     "input_1550_output_3641",
                     "input_1552_output_1552",
                     "input_1555_output_3644",
                     "input_10484_output_10485",
                     "input_27398_output_28393")

#Apply custom factor order
final_incidence_table$source_pair <- factor(final_incidence_table$source_pair, level = incidence_order)

#Reorder the table based on your custom factor + usual time sorting
final_incidence_table <- final_incidence_table %>%
  arrange(source_pair, year_id, sort_order)

write.csv(final_incidence_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_incidence_table.csv", row.names = FALSE)

######################################################################################################################################
## Prevalence Rate(Prevalence per Population) - Output ME_ids
## Prevalence Counts - Output ME_ids
######################################################################################################################################
# Get WRA pop
WRA <- get_population(age_group_id = most_detailed_ages, location_id = location_data_input$location_id, year_id = year_id, sex_id = 2, release_id = release_id) %>%
  select(location_id, year_id, age_group_id, population)

WRA_draws <- get_population(age_group_id = most_detailed_ages, location_id = "all", year_id = year_id, sex_id = 2, release_id = release_id) %>%
  select(location_id, year_id, age_group_id, population)

# Initialize output list
prevalence_results_list <- list()

# Loop through MEs
for (i in seq_along(output_prevalence_mes)) {
  me <- output_prevalence_mes[i]
  
  # Special case: summed prevalence from 1536 + 1537
  if (me == 1536) {
    #### Prevalence Rate
    ### 1536 ----
    #dt_1536 <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    dt_saved_draws_1536 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_1536_16_targetedlocs.rds"))
    df_subset_1536 <- get_cols(dt_saved_draws_1536)
    
    ### 1537 ----
    #dt_1537 <- get_model_results(gbd_team = "epi", gbd_id = 1537, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    dt_saved_draws_1537 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_1537_16_targetedlocs.rds"))
    df_subset_1537 <- get_cols(dt_saved_draws_1537)
    
    # Adding Prevalence Rates, 1536 + 1537
    df_sum_1535_1537 <- df_subset_1536 %>% select(location_id, year_id, age_group_id, starts_with("draw_")) %>% mutate(across(starts_with("draw_"), ~ . + df_subset_1537[[cur_column()]]))
    
    
    # Calculating Prevalence Rate mean, lower, and upper after summing.
    dt_prevalence_rates <- mean_ui_columns(df_sum_1535_1537, group_cols = c("year_id", "location_id", "age_group_id"))
    dt_prevalence_rates <- dt_prevalence_rates %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      rename("Prevalence_rate" = "val", "Prevalence_rate_lower" = "lower", "Prevalence_rate_upper" = "upper")

    #### Prevalence Counts
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    # Sum(Counts) by Country, Region, Super_region, and Global
    dt_prevalence_counts <- df_sum_1535_1537 %>% 
      left_join(WRA_draws, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(across(starts_with("draw_"), ~ .x * population))
    
    ###########################################################################
    # Calcualting Prevalence Counts mean, lower, and upper after calcualtions
    dt_prevalence_counts <- mean_ui_columns(dt_prevalence_counts, group_cols = c("year_id", "location_id", "age_group_id"))
    dt_prevalence_counts <- dt_prevalence_counts %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      rename("Prevalence_counts" = "val", "Prevalence_counts_lower" = "lower", "Prevalence_counts_upper" = "upper")
    
    dt_prevalence_1 <- dt_prevalence_rates %>% merge(dt_prevalence_counts, by = c("location_name", "year_id", "age_group_id", "age_group_name"))
    
    prevalence_results_list[["me_1536_plus_1537_draws"]] <- dt_prevalence_1    
    # Special case: summed prevalence from 27705 + 27702 + 27703 + 27704
  } else if (me == 27705) {
    #### Prevalence Rate
    ### 27702 ----
    dt_saved_draws_27702 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_27702_16_targetedlocs.rds"))
    df_subset_27702 <- get_cols(dt_saved_draws_27702)
    
    ### 27703 ----
    dt_saved_draws_27703 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_27703_16_targetedlocs.rds"))
    df_subset_27703 <- get_cols(dt_saved_draws_27703)
    
    ### 27704 ----
    dt_saved_draws_27704 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_27704_16_targetedlocs.rds"))
    df_subset_27704 <- get_cols(dt_saved_draws_27704)
    
    ### 27705 ----
    dt_saved_draws_27705 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_27705_16_targetedlocs.rds"))
    df_subset_27705 <- get_cols(dt_saved_draws_27705)

    
    # Summing the draw columns across me_ids, 27702 + 27703 + 27704 + 27706
    df_sum_all <- df_subset_27702 %>%
      select(location_id, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + df_subset_27703[[cur_column()]]
                    + df_subset_27704[[cur_column()]]
                    + df_subset_27705[[cur_column()]]
      ))
    
    # Calculating Prevalence Rate mean, lower, and upper
    dt_prevalence_rates <- mean_ui_columns(df_sum_all, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_rate = val,
        Prevalence_rate_lower = lower,
        Prevalence_rate_upper = upper
      ) %>% select(-val, -lower, -upper)
    
    # Prevalence Counts
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    # Sum(Counts) by Country, Region, Super_region, and Global
    dt_prevalence_counts <- df_sum_all %>% 
      left_join(WRA_draws, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(across(starts_with("draw_"), ~ .x * population))
    
    ###########################################################################
    # Calculating Prevalence Counts mean, lower, and upper after calculations
    dt_prevalence_counts <- mean_ui_columns(dt_prevalence_counts, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_counts = val,
        Prevalence_counts_lower = lower,
        Prevalence_counts_upper = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_prevalence_2 <- dt_prevalence_rates %>% merge(dt_prevalence_counts, by = c("location_name", "year_id", "age_group_id", "age_group_name"))
    
    prevalence_results_list[["me_27705_plus_others_draws"]] <- dt_prevalence_2
    
  } else if (me == 28983) {
    #### Prevalence Rate
    ### 28983 ----
    dt_saved_draws_28983 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28983_33_targetedlocs.rds"))
    df_subset_28983 <- get_cols(dt_saved_draws_28983)
    
    ### 28985 ----
    dt_saved_draws_28985 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28985_33_targetedlocs.rds"))
    df_subset_28985 <- get_cols(dt_saved_draws_28985)

    ### 28986 ----
    dt_saved_draws_28986 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28986_33_targetedlocs.rds"))
    df_subset_28986 <- get_cols(dt_saved_draws_28986)

    # Summing Prevalence Rates, 28983 + 28985 + 28986
    df_sum_all <- df_subset_28983 %>%
      select(location_id, location_id, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + df_subset_28985[[cur_column()]] + df_subset_28986[[cur_column()]]
      ))
    
    # Calculating Prevalence Rate mean, lower, and upper
    dt_prevalence_rates <- mean_ui_columns(df_sum_all, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_rate_Updated = val,
        Prevalence_rate_lower_Updated = lower,
        Prevalence_rate_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    # Prevalence Counts
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    # Sum(Counts) by Country, Region, Super_region, and Global
    dt_prevalence_counts <- df_sum_all %>% 
      left_join(WRA_draws, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(across(starts_with("draw_"), ~ .x * population))
    
    ###########################################################################
    # Calculating Prevalence Counts mean, lower, and upper after calculations
    dt_prevalence_counts <- mean_ui_columns(dt_prevalence_counts, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_counts_Updated = val,
        Prevalence_counts_lower_Updated = lower,
        Prevalence_counts_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_prevalence_3 <- dt_prevalence_rates %>% merge(dt_prevalence_counts, by = c("location_name", "year_id", "age_group_id", "age_group_name"))
    
    prevalence_results_list[["me_10485_draws"]] <- dt_prevalence_3

  } else if (me == 32529) {
    #### Prevalence Rate
    ### 32529 ----
    dt_saved_draws_32529 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32529_33_targetedlocs.rds"))
    df_subset_32529 <- get_cols(dt_saved_draws_32529)

    ### 32531 ----
    dt_saved_draws_32531 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32531_33_targetedlocs.rds"))
    df_subset_32531 <- get_cols(dt_saved_draws_32531)

    ### 32532 ----
    dt_saved_draws_32532 <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32532_33_targetedlocs.rds"))
    df_subset_32532 <- get_cols(dt_saved_draws_32532)

    # Summing the draw columns for me_ids, 32529 + 32531 + 32532
    df_sum_all <- df_subset_32529 %>%
      select(location_id, location_id, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + df_subset_32531[[cur_column()]] + df_subset_32532[[cur_column()]]
      ))
    
    # Calculating Prevalence Rate mean, lower, and upper
    dt_prevalence_rates <- mean_ui_columns(df_sum_all, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_rate_Updated = val,
        Prevalence_rate_lower_Updated = lower,
        Prevalence_rate_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    # Prevalence Counts
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    # Sum(Counts) by Country, Region, Super_region, and Global
    dt_prevalence_counts <- df_sum_all %>% 
      left_join(WRA_draws, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(across(starts_with("draw_"), ~ .x * population))
    
    ###########################################################################
    # Calculating Prevalence Counts mean, lower, and upper after calculations
    dt_prevalence_counts <- mean_ui_columns(dt_prevalence_counts, group_cols = c("year_id", "location_id", "age_group_id")) %>%
      left_join(location_data_sort %>% select(location_id, location_name), by = "location_id") %>%
      select(-location_id) %>% unique() %>%
      merge(ages, by = "age_group_id") %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        Prevalence_counts_Updated = val,
        Prevalence_counts_lower_Updated = lower,
        Prevalence_counts_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_prevalence_4 <- dt_prevalence_rates %>% merge(dt_prevalence_counts, by = c("location_name", "year_id", "age_group_id", "age_group_name"))
    
    prevalence_results_list[["me_3644_draws"]] <- dt_prevalence_4
    
  } else if (!(me %in% c(1537, 27702, 27703, 27704, 28985, 28986, 32531, 32532))) {
    dt <- get_model_results(gbd_team = "epi", gbd_id = me, measure_id = 5,
                            age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                            release_id = release_id, sex_id = 2, year_id = year_id)
    
    # Prevalence Rates
    dt_prevalence_rate <- dt %>%
      merge(location_data_input, by = "location_id") %>%
      mutate(
        Prevalence_rate = mean,
        Prevalence_rate_lower = lower,
        Prevalence_rate_upper = upper
      ) %>%
      select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    dt_prevalence_counts <- dt_prevalence_rate %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    dt_prevalence_counts <- dt_prevalence_counts %>% select(-location_name) %>%
      merge(location_data_input_fix, by = "location_id") %>%
      select(modelable_entity_id, age_group_id, year_id, location_name, Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper, Prevalence_counts, Prevalence_counts_lower, Prevalence_counts_upper) %>% unique()
    
    #prevalence_results_list[[paste0("me_", me)]] <- dt_prevalence_counts
    prevalence_results_list[[paste0("me_", me, "_model")]] <- dt_prevalence_counts
  }
  print("Done. Next one.")
}

########################################################################
## Appending all tables in list
# Merge me_3644 model and draw versions
prevalence_results_list[["me_3644_model_draws"]] <- prevalence_results_list[["me_3644_model"]] %>%
  dplyr::left_join(prevalence_results_list[["me_3644_draws"]], 
                   by = c("location_name", "year_id", "age_group_id"))

# Merge me_10485 model and draw versions
prevalence_results_list[["me_10485_model_draws"]] <- prevalence_results_list[["me_10485_model"]] %>%
  dplyr::left_join(prevalence_results_list[["me_10485_draws"]], 
                   by = c("location_name", "year_id", "age_group_id"))

# Remove old entries to clean up
prevalence_results_list[["me_3644_model"]] <- NULL
prevalence_results_list[["me_3644_draws"]] <- NULL
prevalence_results_list[["me_10485_model"]] <- NULL
prevalence_results_list[["me_10485_draws"]] <- NULL

final_prevalence_table <- dplyr::bind_rows(prevalence_results_list, .id = "source_pair") %>%
  left_join(location_data_sort, by = "location_name") %>%
  arrange(source_pair, year_id, sort_order) %>% unique()

prevalence_order <- c(
  "me_1536_plus_1537_draws",
  "me_3623_model",
  "me_3626_model",
  "me_2625_model",
  "me_3635_model",
  "me_1542_model",
  "me_3641_model",
  "me_1552_model",
  "me_3644_model_draws",
  "me_10485_model_draws",
  "me_27705_plus_others_draws"
)

# Apply custom factor order
final_prevalence_table$source_pair <- factor(final_prevalence_table$source_pair, levels = prevalence_order)

# Reorder the table based on your custom factor + usual time sorting
final_prevalence_table <- final_prevalence_table %>%
  arrange(source_pair, year_id, sort_order)

write.csv(final_prevalence_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_prevalence_table.csv", row.names = FALSE)

######################################################################################################################################
## YLDs (YLDs per Population) - get_outputs()
## YLDs (YLDs counts) - get_outputs()
## YLDs were only added.
######################################################################################################################################

WRA <- get_population(age_group_id=c(7:15), location_id="all", year_id="all", sex_id=2, with_ui=TRUE, release_id=33)

# Initialize storage list
sequelae_results_list <- list()

# Loop through each sequelae group
for (group_name in names(sequelae_groups)) {
  ids <- sequelae_groups[[group_name]]
  
  # ---- Pull Rate Data for Each Sequela ----
  rate_list <- lapply(ids, function(id) {
    get_outputs(topic = "sequela", sequela_id = id, measure_id = 3,
                age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                metric_id = 3, release_id = release_id, sex_id = 2, year_id = year_id) %>%
      merge(location_data_input, by = c("location_id", "location_name", "location_type")) %>%
      rename(
        !!paste0("mean_rate_", id) := val,
        !!paste0("lower_rate_", id) := lower,
        !!paste0("upper_rate_", id) := upper
      )
  })
  
  # ---- Pull Count Data for Each Sequela ----
  count_list <- lapply(ids, function(id) {
    get_outputs(topic = "sequela", sequela_id = id, measure_id = 3,
                age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                metric_id = 1, release_id = release_id, sex_id = 2, year_id = year_id) %>%
      merge(location_data_input, by = c("location_id", "location_name", "location_type")) %>%
      rename(
        !!paste0("mean_cnt_", id) := val,
        !!paste0("lower_cnt_", id) := lower,
        !!paste0("upper_cnt_", id) := upper
      )
  })
  
  # ---- Combine Rate Tables ----
  dt_rate_combined <- Reduce(function(x, y) {
    full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  }, rate_list)
  
  # ---- Combine Count Tables ----
  dt_count_combined <- Reduce(function(x, y) {
    full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  }, count_list)
  
  # ---- Sum Across MEs in Group ----
  dt_rate_summed <- dt_rate_combined %>%
    mutate(
      YLDs_rate = rowSums(select(., starts_with("mean_rate_")), na.rm = TRUE),
      YLDs_rate_lower = rowSums(select(., starts_with("lower_rate_")), na.rm = TRUE),
      YLDs_rate_upper = rowSums(select(., starts_with("upper_rate_")), na.rm = TRUE)
    ) %>%
    select(age_group_id, year_id, sort_order, location_id, location_name,
           YLDs_rate, YLDs_rate_lower, YLDs_rate_upper)
  
  dt_count_summed <- dt_count_combined %>%
    mutate(
      YLDs_count = rowSums(select(., starts_with("mean_cnt_")), na.rm = TRUE),
      YLDs_count_lower = rowSums(select(., starts_with("lower_cnt_")), na.rm = TRUE),
      YLDs_count_upper = rowSums(select(., starts_with("upper_cnt_")), na.rm = TRUE)
    ) %>%
    select(age_group_id, year_id, sort_order, location_id, location_name,
           YLDs_count, YLDs_count_lower, YLDs_count_upper)
  
  # ---- Merge Rate and Count Results ----
  dt_summed <- dt_rate_summed %>%
    full_join(dt_count_summed, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  
  # ---- Store Result ----
  sequelae_results_list[[group_name]] <- dt_summed
  if(group_name == "group_10"){
    # Ectopic Pregnancy --------
    # ADDING EMILYs code for Usability and ease of use. ---------------
    uncomplicated_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28983_33_targetedlocs.rds")
    complicated_acute_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28985_33_targetedlocs.rds")
    complicated_sub_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28986_33_targetedlocs.rds")
    
    # Making working copies of prevalence draws to adjust
    work_uncomplicated_prev <- uncomplicated_prev %>% get_cols()
    work_complicated_acute_prev <- complicated_acute_prev %>% get_cols()
    work_complicated_sub_prev <- complicated_sub_prev %>% get_cols()

    #Selecting which columns to keep
    WRA<-WRA[, c("location_id", "year_id", "age_group_id", "population")]
    
    # Joining WRA to draws data sets
    work_uncomplicated_prev<-left_join(work_uncomplicated_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    work_complicated_acute_prev<-left_join(work_complicated_acute_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    work_complicated_sub_prev<-left_join(work_complicated_sub_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    
    # Defining DWs for moderate and severe abdominopelvic problem (health states being used)
    mean_dw_mod<- 0.114
    mean_dw_sev<- 0.324
    
    # Using for loops to iterate through each column (draw) and multiply by DW and then by population
    draw_cols <- paste0("draw_", 0:999)
    
    # Make SAFE copies before modifying
    # Rates
    work_uncomplicated_prev_rates <- copy(work_uncomplicated_prev)
    work_uncomplicated_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_mod), 
                                  .SDcols = draw_cols,
                                  by = c("location_id", "year_id", "age_group_id")]
    work_uncomplicated_prev_rates <- work_uncomplicated_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_acute_prev_rates <- copy(work_complicated_acute_prev)
    work_complicated_acute_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_sev), 
                                      .SDcols = draw_cols,
                                      by = c("location_id", "year_id", "age_group_id")]
    work_complicated_acute_prev_rates <- work_complicated_acute_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_sub_prev_rates <- copy(work_complicated_sub_prev)
    work_complicated_sub_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_mod), 
                                    .SDcols = draw_cols,
                                    by = c("location_id", "year_id", "age_group_id")]
    work_complicated_sub_prev_rates <- work_complicated_sub_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    # Counts
    work_uncomplicated_prev_counts <- copy(work_uncomplicated_prev)
    work_uncomplicated_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_mod), 
                                   .SDcols = draw_cols,
                                   by = c("location_id", "year_id", "age_group_id")]
    work_uncomplicated_prev_counts <- work_uncomplicated_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_acute_prev_counts <- copy(work_complicated_acute_prev)
    work_complicated_acute_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_sev), 
                                       .SDcols = draw_cols,
                                       by = c("location_id", "year_id", "age_group_id")]
    work_complicated_acute_prev_counts <- work_complicated_acute_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_sub_prev_counts <- copy(work_complicated_sub_prev)
    work_complicated_sub_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_mod), 
                                     .SDcols = draw_cols,
                                     by = c("location_id", "year_id", "age_group_id")]
    work_complicated_sub_prev_counts <- work_complicated_sub_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    # In Emily's code there is a lot of manual location data wrangling that was skipped, since we already did a lot of this work when merging with location_data_draws.
    # Melting data, just like with Incident and Prevalent draws
    # Melting data. Location_name will included super_region_name, region_name, and country_name.
    
    # Summing all YLDs together, Rates.
    df_sum_all_rates <- work_uncomplicated_prev_rates %>%
      select(location_id, location_name, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + work_complicated_acute_prev_rates[[cur_column()]] + work_complicated_sub_prev_rates[[cur_column()]]
      ))

    # Summing all YLDs together, Counts.
    df_sum_all <- work_uncomplicated_prev_counts %>%
      select(location_id, location_name, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + work_complicated_acute_prev_counts[[cur_column()]] + work_complicated_sub_prev_counts[[cur_column()]]
      ))
    
    # Emily's script had a lot of location data wrangling code. All of these steps were skipped, since we are already have the code for locations.
    # Calculating Prevalence Rate mean, lower, and upper
    # Rates
    dt_YLDs_rates <- mean_ui_columns(df_sum_all_rates, group_cols = c("year_id", "location_name", "age_group_id")) %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        YLDs_rates_Updated = val,
        YLDs_rates_lower_Updated = lower,
        YLDs_rates_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_YLDs_rates <- dt_YLDs_rates %>%
      left_join(location_data_sort, by = "location_name")
    
    
    # Counts
    dt_YLDs_counts <- mean_ui_columns(df_sum_all, group_cols = c("year_id", "location_name", "age_group_id")) %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        YLDs_counts_Updated = val,
        YLDs_counts_lower_Updated = lower,
        YLDs_counts_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_YLDs_counts <- dt_YLDs_counts %>%
      left_join(location_data_sort, by = "location_name")
    
    dt_YLDs <- merge(dt_YLDs_rates, dt_YLDs_counts, by = c("location_id", "location_name", "year_id", "age_group_id", "sort_order"))
    
    sequelae_results_list[["group_10_draws"]] <- dt_YLDs
    
    # ---- Clean up to prevent memory bloating ----
    rm(
      uncomplicated_prev, complicated_acute_prev, complicated_sub_prev,
      work_uncomplicated_prev, work_complicated_acute_prev, work_complicated_sub_prev,
      work_uncomplicated_prev_rates, work_complicated_acute_prev_rates, work_complicated_sub_prev_rates,
      df_sum_all_rates, df_sum_all,
      dt_YLDs_rates, dt_YLDs_counts, dt_YLDs
    )
    gc(verbose = FALSE)
    
    print("group 10 complete.")
  } else if(group_name == "group_9"){
    # Miscarriage --------
    # ADDING EMILYs code for Usability and ease of use. ---------------
    # 32529, 32531,32532
    uncomplicated_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32529_33_targetedlocs.rds")
    complicated_acute_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32531_33_targetedlocs.rds")
    complicated_sub_prev<- readRDS(file = "/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32532_33_targetedlocs.rds")
    
    # Making working copies of prevalence draws to adjust
    work_uncomplicated_prev <- uncomplicated_prev %>% get_cols()
    work_complicated_acute_prev <- complicated_acute_prev %>% get_cols()
    work_complicated_sub_prev <- complicated_sub_prev %>% get_cols()
    
    #Selecting which columns to keep
    WRA<-WRA[, c("location_id", "year_id", "age_group_id", "population")]
    
    # Joining WRA to draws data sets
    #   Next step ->  # Merging with location_data to aggregate later at the Country, Region, Super region, and Global Level
    work_uncomplicated_prev<-left_join(work_uncomplicated_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    work_complicated_acute_prev<-left_join(work_complicated_acute_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    work_complicated_sub_prev<-left_join(work_complicated_sub_prev, WRA, by=c("location_id","year_id", "age_group_id")) #%>% merge(location_data_draws, by = "location_id")
    
    # Defining DWs for moderate and severe abdominopelvic problem (health states being used)
    mean_dw_mod<- 0.114
    mean_dw_sev<- 0.324
    
    # Using for loops to iterate through each column (draw) and multiply by DW and then by population
    draw_cols <- paste0("draw_", 0:999)
    
    # Make SAFE copies before modifying
    # Rates
    work_uncomplicated_prev_rates <- copy(work_uncomplicated_prev)
    work_uncomplicated_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_mod), 
                                  .SDcols = draw_cols,
                                  by = c("location_id", "year_id", "age_group_id")]
    work_uncomplicated_prev_rates <- work_uncomplicated_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_acute_prev_rates <- copy(work_complicated_acute_prev)
    work_complicated_acute_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_sev), 
                                      .SDcols = draw_cols,
                                      by = c("location_id", "year_id", "age_group_id")]
    work_complicated_acute_prev_rates <- work_complicated_acute_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_sub_prev_rates <- copy(work_complicated_sub_prev)
    work_complicated_sub_prev_rates[, (draw_cols) := lapply(.SD, function(x) x * mean_dw_mod), 
                                    .SDcols = draw_cols,
                                    by = c("location_id", "year_id", "age_group_id")]
    work_complicated_sub_prev_rates <- work_complicated_sub_prev_rates %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    # Counts
    work_uncomplicated_prev_counts <- copy(work_uncomplicated_prev)
    work_uncomplicated_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_mod), 
                                   .SDcols = draw_cols,
                                   by = c("location_id", "year_id", "age_group_id")]
    work_uncomplicated_prev_counts <- work_uncomplicated_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_acute_prev_counts <- copy(work_complicated_acute_prev)
    work_complicated_acute_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_sev), 
                                       .SDcols = draw_cols,
                                       by = c("location_id", "year_id", "age_group_id")]
    work_complicated_acute_prev_counts <- work_complicated_acute_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    work_complicated_sub_prev_counts <- copy(work_complicated_sub_prev)
    work_complicated_sub_prev_counts[, (draw_cols) := lapply(.SD, function(x) x * population * mean_dw_mod), 
                                     .SDcols = draw_cols,
                                     by = c("location_id", "year_id", "age_group_id")]
    work_complicated_sub_prev_counts <- work_complicated_sub_prev_counts %>% select(-population) %>% left_join(location_data_sort, by = "location_id")
    
    # In Emily's code there is a lot of manual location data wrangling that was skipped, since we already did a lot of this work when merging with location_data_draws.
    # Melting data, just like with Incident and Prevalent draws
    # Melting data. Location_name will included super_region_name, region_name, and country_name.
    
    # Summing all YLDs together, rates.
    df_sum_all_rates <- work_uncomplicated_prev_rates %>%
      select(location_id, location_name, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + work_complicated_acute_prev_rates[[cur_column()]] + work_complicated_sub_prev_rates[[cur_column()]]
      ))
    
    # Summing all YLDs together, counts.
    df_sum_all <- work_uncomplicated_prev_counts %>%
      select(location_id, location_name, year_id, age_group_id, starts_with("draw_")) %>%
      mutate(across(starts_with("draw_"), ~ 
                      . + work_complicated_acute_prev_counts[[cur_column()]] + work_complicated_sub_prev_counts[[cur_column()]]
      ))
    
    # Emily's script had a lot of location data wrangling code. All of these steps were skipped, since we are already have the code for locations.
    # Calculating Prevalence Rate mean, lower, and upper
    # Rates
    dt_YLDs_rates <- mean_ui_columns(df_sum_all_rates, group_cols = c("year_id", "location_name", "age_group_id")) %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        YLDs_rates_Updated = val,
        YLDs_rates_lower_Updated = lower,
        YLDs_rates_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_YLDs_rates <- dt_YLDs_rates %>%
      left_join(location_data_sort, by = "location_name")
    
    
    # Counts
    dt_YLDs_counts <- mean_ui_columns(df_sum_all, group_cols = c("year_id", "location_name", "age_group_id")) %>%
      arrange(year_id, age_group_id) %>%
      mutate(
        YLDs_counts_Updated = val,
        YLDs_counts_lower_Updated = lower,
        YLDs_counts_upper_Updated = upper
      ) %>% select(-val, -lower, -upper)
    
    dt_YLDs_counts <- dt_YLDs_counts %>%
      left_join(location_data_sort, by = "location_name")
    
    dt_YLDs <- merge(dt_YLDs_rates, dt_YLDs_counts, by = c("location_id", "location_name", "year_id", "age_group_id", "sort_order"))
    
    sequelae_results_list[["group_9_draws"]] <- dt_YLDs
    
    # ---- Clean up to prevent memory bloating ----
    rm(
      uncomplicated_prev, complicated_acute_prev, complicated_sub_prev,
      work_uncomplicated_prev, work_complicated_acute_prev, work_complicated_sub_prev,
      work_uncomplicated_prev_rates, work_complicated_acute_prev_rates, work_complicated_sub_prev_rates,
      df_sum_all_rates, df_sum_all,
      dt_YLDs_rates, dt_YLDs_counts, dt_YLDs
    )
    gc(verbose = FALSE)
    
    print("group 9 complete.")
    
  }
  print("Done. Next one.")
  
  # ---- Clean up to prevent memory bloating ----
  rm(
    rate_list, count_list,
    dt_rate_combined, dt_count_combined,
    dt_rate_summed, dt_count_summed, dt_summed
  )
  gc(verbose = FALSE)
  }

sequelae_results_list[["group_10"]] <- sequelae_results_list[["group_10"]] %>%
  dplyr::left_join(sequelae_results_list[["group_10_draws"]], 
                   by = c("location_id","location_name", "year_id", "age_group_id", "sort_order"))
sequelae_results_list[["group_9"]] <- sequelae_results_list[["group_9"]] %>%
  dplyr::left_join(sequelae_results_list[["group_9_draws"]], 
                   by = c("location_id","location_name", "year_id", "age_group_id", "sort_order"))

sequelae_results_list[["group_10_draws"]] <- NULL
sequelae_results_list[["group_9_draws"]] <- NULL

# Bind all into a single table
final_ylds_table <- dplyr::bind_rows(sequelae_results_list, .id = "group_name") %>%
  arrange(group_name, year_id, sort_order) %>%
  left_join(ages, by = "age_group_id")

# Preserve custom group order
final_ylds_table$group_name <- factor(final_ylds_table$group_name, levels = names(sequelae_groups))

# Re-arrange rows using the factor order + year/sort_order
final_ylds_table <- final_ylds_table %>%
  arrange(group_name, year_id, sort_order)

# Write to CSV
write.csv(final_ylds_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_ylds_table.csv", row.names = FALSE)


################## Final Append ####################
## Stacking them next to eachother
####################################################
# # Removing duplicate columns
final_prevalence_trimmed <- final_prevalence_table %>% select(-c(location_id, location_name, year_id, age_group_id, sort_order))
final_ylds_trimmed <- final_ylds_table %>% select(-c(location_id, location_name, year_id, age_group_id, sort_order))

# Binding Horizontally and excluding some columns from dfs to get rid of duplicates
final_combined_table <- bind_cols(
  final_incidence_table, #%>% select(-source_pair, -modelable_entity_id),
  final_prevalence_trimmed %>% select(-age_group_name),# %>% select(-source_pair, -modelable_entity_id, -age_group_name),
  final_ylds_trimmed %>% select(-age_group_name)
)

# List of cause names. These should align with the same order as the MEIDs and Sequela IDs. Verify as necessary.
causes <- c(
  "Maternal Hemorrhage",
  "Puerperal Sepsis",
  "Other maternal infections",
  "Hypertensive disorders of pregnancy",
  "Eclampsia",
  "Severe pre-eclampsia",
  "Obstructed labour",
  "Fistula",
  "Maternal abortive outcome",
  "Ectopic pregnancy",
  "Peripartum cardiomyopathy incidence ratio (events per live birth - tagged to female population)"
)

# Repeat each cause 20070 times
cause_column <- rep(causes, each = 20970)

# Add to your data frame
final_combined_table$cause <- cause_column

final_combined_table <- final_combined_table %>% select(1:2, age_group_name, sort_order, location_id, location_name, cause, everything())

write.csv(final_combined_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_maternal_nonfatal_table.csv", row.names = FALSE)
write.csv(final_combined_table, "/mnt/share/mmbrf/Deliverables/April_29_2025_maternal_nonfatal_table_nolocs.csv", row.names = FALSE)

#########################################################################################################################
#########################################################################################################################
# GDM 
#########################################################################################################################
#########################################################################################################################
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_input <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))

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
                     release_id = 33,
                     version_id = 926214
)
#saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714.rds"))
saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714_33_926214.rds"))

### Pulling Saved Draws
##############################################
dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714_33_926214.rds"))
df_subset <- get_cols(dt_saved_draws)
df_subset <- df_subset %>% merge(location_data, by = "location_id") #%>% select(-location_type)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(global, super_region_name, region_name, country_name),  # Columns to stack
                      values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)

### Incidence Counts = GDM draws * # of births for GDM###
# # of births (livebirth+stillbirth) for GDM =female population * ((ASFR * 46/52) + (stillbirth_livebirth_ratio *ASFR * 26/52))
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_new <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_new <- df_subset_new %>% merge(fem_pop, by = c("location_id", "year_id", "age_group_id")) %>% rename(fem_pop=population)
df_subset_new <- df_subset_new %>% left_join(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_new <- df_subset_new %>% mutate(births = (fem_pop*asfr*(46/52)) + (sb_ratio*asfr*(26/52))) %>% select(-asfr, -fem_pop, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * births), 
                               .SDcols = draw_cols,
                               by = c("location_id", "year_id", "age_group_id")]
#df_subset_new <- df_subset_new %>% select(-births)

#df_missing <- df_subset_new %>% filter(if_any(starts_with("draw_"), ~ is.na(.) | is.nan(.)))
# Stillbirths has some missing locations_Ids(Very few): 60908, 94364, ....These lccations were simply excluded for GDM. Not taken part in any of the aggregations going forward.
# Confirm with Ke Pan and Sunny
df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ ifelse(is.nan(.), NA, .))) %>%  # Treat NaN as NA
  filter(if_all(starts_with("draw_"), ~ !is.na(.)))

# Summing(count draws) and Summing(births) by location_name, year_id, and age_group_id.
df_subset_new <- df_subset_new %>%
  group_by(location_name, year_id, age_group_id) %>%
  summarise(
    births = sum(births, na.rm = TRUE),
    across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

result_gdm_incidence_counts <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id"))
result_gdm_incidence_counts <- result_gdm_incidence_counts %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence_counts" = "val", "Incidence_counts_lower" = "lower", "Incidence_counts_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence_counts", "Incidence_counts_lower", "Incidence_counts_upper")

result_gdm_incidence_counts <- result_gdm_incidence_counts %>%
  left_join(df_subset_new %>% select(year_id, location_name, age_group_id, births),
            by = c("year_id", "location_name", "age_group_id"))

### Incidence Ratio Average / by Region(admin0, countries, were used for calculations) ####
# Diving Sum(counts) / Sum(Births)
#########################################################################################
# df_subset_new <- df_subset_new %>%
#   group_by(location_name, year_id, age_group_id) %>%
#   summarise(
#     births = sum(births, na.rm = TRUE),
#     across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)),
#     .groups = "drop"
#   )
df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ .x / births))

# Calculating Incidence Ratio mean, lower, and upper for draws after calculations.
result_gdm_incidence_ratio <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id"))
result_gdm_incidence_ratio <- result_gdm_incidence_ratio %>%
  merge(ages, by = "age_group_id") %>%
  arrange(year_id, age_group_id) %>%
  rename("Incidence_ratios" = "val", "Incidence_ratios_lower" = "lower", "Incidence_ratios_upper" = "upper")

### Incidence Rate, using get_model_resuls()
##############################################
## Incidence Rate (Output ME)
location_data_fix <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_input_fix <- location_data_fix %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))

dt <- get_model_results(gbd_team = "epi", gbd_id = 27715, measure_id = 6, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2, year_id = year_id)

result_gdm_incidence_rate <- dt %>%
  merge(location_data_input_fix, by = "location_id") %>%
  merge(ages, by = "age_group_id") %>%
  rename(
    "Incidence_rate" = "mean",
    "Incidence_rate_lower" = "lower",
    "Incidence_rate_upper" = "upper"
  ) %>%
  select(age_group_id, age_group_name, year_id, location_name, Incidence_rate, Incidence_rate_lower, Incidence_rate_upper) %>% unique()

### Final Dataframe 
##############################################
dfs <- list(result_gdm_incidence_ratio, result_gdm_incidence_counts, result_gdm_incidence_rate)

# Reduce using inner_join
merged_df <- reduce(dfs, inner_join, by = c("age_group_id", "age_group_name", "year_id", "location_name")) #iteratively applying a function across a list of elements—reducing them down to a single result.

# Add location info separately
final_incidence_table  <- merged_df %>%
  left_join(location_data_sort, by = "location_name") %>%
  arrange(year_id, sort_order)
# important note: North Africa and Middle East is both a Super Region and a Region. You will get a warning message stating that this is duplicate data. We want this duplicate data, as both of these location hierarchys share the same name, hence, the same data.
write.csv(final_incidence_table, paste0("/mnt/team/rgud/pub/users/chrish47/CSV_files/GDM_final_table.csv"), row.names = FALSE)

### Prevalence and YLDs Inputs (get_mode_results())
######################################################################
######################################################################
output_prevalence_mes <- c(27715)
# Define ME groups (each element is a group to be summed)
sequelae_groups <- list(group_1 = c(23468))

year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_input <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0", "region", "superregion", "global"))

# Pulling ASFR and Female Population for calculating Counts.
asfr <- get_covariate_estimates(covariate_id = 13,
                                year_id = year_id,
                                age_group_id = most_detailed_ages,
                                release_id = 16,
                                sex_id = 2,
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

######################################################################################################################################
## Prevalence Rate(Prevalence per Population) - Output ME_ids
## Prevalence Counts - Output ME_ids
######################################################################################################################################
# Get WRA pop
WRA <- get_population(age_group_id = most_detailed_ages, location_id = location_data_input$location_id, year_id = year_id, sex_id = 2, release_id = release_id) %>%
  select(location_id, year_id, age_group_id, population)

# Initialize output list
prevalence_results_list <- list()

# Loop through MEs
for (me in output_prevalence_mes) {
  
  # Only process ME IDs not included in special cases
  if (!(me %in% c(1537, 27702, 27703, 27704, 1536, 27705))) {
    
    dt <- get_model_results(gbd_team = "epi", gbd_id = me, measure_id = 5,
                            age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                            release_id = release_id, sex_id = 2, year_id = year_id)
    
    # Prevalence Rates
    dt_prevalence_rate <- dt %>%
      merge(location_data_input, by = "location_id") %>%
      rename(
        Prevalence_rate = mean,
        Prevalence_rate_lower = lower,
        Prevalence_rate_upper = upper
      ) %>%
      select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name,
             Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    # Prevalence Counts
    dt_prevalence_counts <- dt_prevalence_rate %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    prevalence_results_list[[paste0("me_", me)]] <- dt_prevalence_counts
  }
}

########################################################################
## Appending all tables in list
final_prevalence_table <- dplyr::bind_rows(prevalence_results_list, .id = "source_pair") %>%
  arrange(source_pair, year_id, sort_order) %>%
  left_join(ages, by = "age_group_id")

prevalence_order <- c(
  "me_27715"
)

# Apply custom factor order
final_prevalence_table$source_pair <- factor(final_prevalence_table$source_pair, levels = prevalence_order)

# Reorder the table based on your custom factor + usual time sorting
final_prevalence_table <- final_prevalence_table %>%
  arrange(source_pair, year_id, sort_order)

write.csv(final_prevalence_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_prevalence_table_gdm.csv", row.names = FALSE)

######################################################################################################################################
## YLDs (YLDs per Population) - get_outputs()
## YLDs (YLDs counts) - get_outputs()
## YLDs were only added.
######################################################################################################################################
# Initialize storage list
sequelae_results_list <- list()

# Loop through each sequelae group
for (group_name in names(sequelae_groups)) {
  ids <- sequelae_groups[[group_name]]
  
  # ---- Pull Rate Data for Each Sequela ----
  rate_list <- lapply(ids, function(id) {
    get_outputs(topic = "sequela", sequela_id = id, measure_id = 3,
                age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                metric_id = 3, release_id = release_id, sex_id = 2, year_id = year_id) %>%
      merge(location_data_input, by = c("location_id", "location_name", "location_type")) %>%
      rename(
        !!paste0("mean_rate_", id) := val,
        !!paste0("lower_rate_", id) := lower,
        !!paste0("upper_rate_", id) := upper
      )
  })
  
  # ---- Pull Count Data for Each Sequela ----
  count_list <- lapply(ids, function(id) {
    get_outputs(topic = "sequela", sequela_id = id, measure_id = 3,
                age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                metric_id = 1, release_id = release_id, sex_id = 2, year_id = year_id) %>%
      merge(location_data_input, by = c("location_id", "location_name", "location_type")) %>%
      rename(
        !!paste0("mean_cnt_", id) := val,
        !!paste0("lower_cnt_", id) := lower,
        !!paste0("upper_cnt_", id) := upper
      )
  })
  
  # ---- Combine Rate Tables ----
  dt_rate_combined <- Reduce(function(x, y) {
    full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  }, rate_list)
  
  # ---- Combine Count Tables ----
  dt_count_combined <- Reduce(function(x, y) {
    full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  }, count_list)
  
  # ---- Sum Across MEs in Group ----
  dt_rate_summed <- dt_rate_combined %>%
    mutate(
      YLDs_rate = rowSums(select(., starts_with("mean_rate_")), na.rm = TRUE),
      YLDs_rate_lower = rowSums(select(., starts_with("lower_rate_")), na.rm = TRUE),
      YLDs_rate_upper = rowSums(select(., starts_with("upper_rate_")), na.rm = TRUE)
    ) %>%
    select(age_group_id, year_id, sort_order, location_id, location_name,
           YLDs_rate, YLDs_rate_lower, YLDs_rate_upper)
  
  dt_count_summed <- dt_count_combined %>%
    mutate(
      YLDs_count = rowSums(select(., starts_with("mean_cnt_")), na.rm = TRUE),
      YLDs_count_lower = rowSums(select(., starts_with("lower_cnt_")), na.rm = TRUE),
      YLDs_count_upper = rowSums(select(., starts_with("upper_cnt_")), na.rm = TRUE)
    ) %>%
    select(age_group_id, year_id, sort_order, location_id, location_name,
           YLDs_count, YLDs_count_lower, YLDs_count_upper)
  
  # ---- Merge Rate and Count Results ----
  dt_summed <- dt_rate_summed %>%
    full_join(dt_count_summed, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
  
  # ---- Store Result ----
  sequelae_results_list[[group_name]] <- dt_summed
}

# Bind all into a single table
final_ylds_table <- dplyr::bind_rows(sequelae_results_list, .id = "group_name") %>%
  arrange(group_name, year_id, sort_order) %>%
  left_join(ages, by = "age_group_id")

# Preserve custom group order
final_ylds_table$group_name <- factor(final_ylds_table$group_name, levels = names(sequelae_groups))

# Re-arrange rows using the factor order + year/sort_order
final_ylds_table <- final_ylds_table %>%
  arrange(group_name, year_id, sort_order)


# Write to CSV
write.csv(final_ylds_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_ylds_table_gdm.csv", row.names = FALSE)

################## Final Append ####################
## Stacking them next to eachother
####################################################
# Binding Horizontally and excluding some columns from dfs to get rid of duplicates
final_combined_table <- bind_cols(
  final_incidence_table %>% select(-age_group_id, -age_group_name, -sort_order, -location_id, -location_name, -year_id),
  final_prevalence_table,# %>% select(-source_pair, -modelable_entity_id, -age_group_name),
  final_ylds_table %>% select(-age_group_id, -age_group_name, -sort_order, -location_id, -location_name, -year_id)
)

# List of cause names. These should align with the same order as the MEIDs and Sequela IDs. Verify as necessary.
causes <- c(
  "GDM in pregnant women (general female population)"
)

# Repeat each cause 20070 times
cause_column <- rep(causes, each = 20970)

# Add to your data frame
final_combined_table$cause <- cause_column

final_combined_table <- final_combined_table %>% select(age_group_id, age_group_name,	location_id, location_name, cause, year_id, sort_order, everything())

write.csv(final_combined_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_maternal_nonfatal_table_gdm.csv", row.names = FALSE)
write.csv(final_combined_table, "/mnt/share/mmbrf/Deliverables/April25_2025_maternal_nonfatal_table_gdm.csv", row.names = FALSE)

#####################################################################################################################################################
#####################################################################################################################################################
# GDM Incidence Ratio Percentage Change
# We want the % change for the aggregated age group 10-54, Incidence Ratio from 1990-2023. Incidence Ratio only for now.
#####################################################################################################################################################
# For GBD only, the collaborator asked for age standardized (10-54) GDM percent change from 1990-2023. I told Christian to aggregate age groups by using location-year-age specific birth and calculate the percent change because I thought that was what GBD did for age standardized incidence as well. However, I WAS WRONG (my apologies). The difference between age standardized vs. age aggregated in GBD is that age aggregated estimates use location,year, age specific livebirths to aggregate the incidence ratio, while age-standardized estimates should use a specific year, global age specific births and apply that to all locations. Therefore, what we need to do is:
#   aggregate locations as we are going to do for other non-fatal causes
# pull xx year (I need to find out which year we use in GBD), global age specific births to calculate the birth proportion (births in age group A/total births in 10-54) for each age group
# age standardized incidence ratio=sum(age specific incidence ratio after location aggregation*age specific birth proportion)
#----------------------------------------------------------------------------------------------------------------------------------------------------
year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

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


# GDM Percentage Change - Incidence Ratio
# We want the % change for the aggregated age group 10-54 Incidence from 1990-2023. Incidence Ratio and Incidence Rate Only.

dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714_33_926214.rds"))
df_subset <- get_cols(dt_saved_draws) %>% filter(year_id %in% c(1990, 2023)) # Do 2023
df_subset <- df_subset %>% merge(location_data, by = "location_id") #%>% select(-location_type)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(global, super_region_name, region_name, country_name),  # Columns to stack
                      values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)

# -----------------------------------------------------------------------------------------------------------------------------
### Incidence Counts = GDM draws * # of births for GDM###
# # of births (livebirth+stillbirth) for GDM =female population * ((ASFR * 46/52) + (stillbirth_livebirth_ratio *ASFR * 26/52))
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_new <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_new <- df_subset_new %>% merge(fem_pop, by = c("location_id", "year_id", "age_group_id")) %>% rename(fem_pop=population)
df_subset_new <- df_subset_new %>% merge(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_new <- df_subset_new %>% mutate(births = (fem_pop*asfr*(46/52)) + (sb_ratio*asfr*(26/52))) %>% select(-asfr, -fem_pop, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_new <- df_subset_new[, (draw_cols) := lapply(.SD, function(x) x * births), 
                               .SDcols = draw_cols,
                               by = c("location_id", "year_id", "age_group_id")]
#df_subset_new <- df_subset_new %>% select(-births)

#df_missing <- df_subset_new %>% filter(if_any(starts_with("draw_"), ~ is.na(.) | is.nan(.)))
# Stillbirths has some missing locations_Ids(Very few): 60908, 94364, ....These lccations were simply excluded for GDM. Not taken part in any of the aggregations going forward.
# Confirm with Ke Pan and Sunny
df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ ifelse(is.nan(.), NA, .))) %>%  # Treat NaN as NA
  filter(if_all(starts_with("draw_"), ~ !is.na(.)))

# Use a specific year (e.g. 2023) for global age-specific births
reference_year <- 2023
global_births <- df_subset_new %>%
  filter(location_name == "Global") %>%
  filter(year_id == reference_year) %>%
  group_by(age_group_id) %>%
  summarise(global_births = sum(births, na.rm = TRUE))

# Calculate age-specific birth proportions
global_births <- global_births %>%
  mutate(birth_proportion = global_births / sum(global_births)) %>%
  select(age_group_id, birth_proportion)

### Incidence Ratio Average / by Region(admin0, countries, were used for calculations) ####
#########################################################################################
# Dividing Incidence Counts by Births to get Incidence Ratios
df_subset_new <- df_subset_new %>% mutate(across(starts_with("draw_"), ~ .x / births)) %>% select(-births)

# Join birth proportions to each draw row
df_standardized <- df_subset_new %>%
  merge(global_births, by = "age_group_id")

# Multiply each draw by global birth proportion
df_standardized <- df_standardized %>% mutate(across(starts_with("draw_"), ~ .x * birth_proportion))

# Sum over age groups for each location(country, region, super region, and global) and year to get age-standardized values
df_standardized_summary <- df_standardized %>%
  group_by(location_name, year_id) %>%
  summarise(across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

df_standardized_summary <- as.data.table(df_standardized_summary)
# -------------------------------------------------------------------------------------------------------------------------------

# Step 1 - Melting the data
df_subset_rate_melt <- melt(data = df_standardized_summary, id.vars = c("location_name", "year_id")) %>% unique() #North Africa and Middle East, South Asia are both regions and super regions

# Step 2 — Reshape wide on year_id
df_wide_year <- dcast(df_subset_rate_melt,
                      location_name + variable ~ year_id,
                      value.var = "value")

# Step 3 — Calculate Percent Change
df_wide_year <- df_wide_year %>% mutate(pct_change = ((`2023` - `1990`) / `1990`) * 100)

# Step 4 — Reshape again — pivot variables
df_wide_year <- dcast(df_wide_year,
                      location_name ~ variable,
                      value.var = "pct_change")

result_gdm_incidence_rate_pct <- mean_ui_columns(df_wide_year, group_cols = c("location_name"))

# List of cause names and age_group
causes <- "GDM in pregnant women (general female population)"
age_group <- "10 to 54"

# Repeat each cause 20070 times
cause_column <- rep(causes, each = 231)
age_column <- rep(age_group, each = 231)

# Add to your data frame
result_gdm_incidence_rate_pct$cause <- cause_column
result_gdm_incidence_rate_pct$age_group <- age_column

# Add location info separately
final_incidence_pct_change  <- result_gdm_incidence_rate_pct %>%
  merge(location_data_sort, by = "location_name") %>%
  arrange(sort_order) %>%
  select(6, 1, everything())

write.csv(final_incidence_pct_change, "/mnt/share/mmbrf/Deliverables/April_25_2025_gdm_pct_2023_1990_10_54.csv", row.names = FALSE)

