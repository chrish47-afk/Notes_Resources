###################
# Author: Christian Hernandez
# Date: April, 2025
# Purpose: The following script creates a csv file with all non-fatal maternal incident, prevalent, and YLDs data. At the global, super region, region, and admind0(country) level. By age_groups. GDM was done separately due to custom calcualtions.
###################
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


### Helper Functions for Nonfatal and GDM tables. (USE AS NECESSARY)
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
                         #27714 #GDM
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
                          #27715 #GDM
                          )
output_prevalence_mes <- c(1536,1537,#
                          3623,#
                          3626,#
                          2625,#
                          3635,#
                          1542,#
                          3641,#
                          1552,#
                          3644,#
                          10485,#
                          27705, 27702, 27703, 27704
                          #27715 #GDM
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
  # group_12 = c(23468)  # GDM, excluded
  )
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
## Incidence Ratios (Incidence per Livebirths) - Input ME_ids
## Incidence Counts - Input ME_ids
## Incidence Rate(Incidence per Population) - Output ME_ids
######################################################################################################################################
# Initialize an empty list to store merged outputs
incidence_results_list <- list()

for (i in seq_along(input_incidence_mes)) {
  me_i <- input_incidence_mes[i]
  me_o <- output_incidence_mes[i]
  
  ########################################################################
  ## Incidence Ratios and Counts (Input ME)
  dt <- get_model_results(gbd_team = "epi", gbd_id = me_i, measure_id = 6, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2, year_id = year_id)
  
  dt_incidence_ratios <- dt %>%
    merge(location_data_input, by = "location_id") %>%
    rename(
      "Incidence_ratios" = "mean",
      "Incidence_ratios_lower" = "lower",
      "Incidence_ratios_upper" = "upper"
    ) %>%
    select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Incidence_ratios, Incidence_ratios_lower, Incidence_ratios_upper)
  
  # Incidence counts = incidence ratio * livebirths (asfr * fem_pop)
  dt_incidence_counts <- dt_incidence_ratios %>%
    merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>%
    merge(fem_pop, by = c("location_id", "year_id", "age_group_id")) %>%
    mutate(livebirths = asfr * fem_pop) %>%
    select(-asfr, -fem_pop) %>%
    mutate(
      Incidence_counts = Incidence_ratios * livebirths,
      Incidence_counts_lower = Incidence_ratios_lower * livebirths,
      Incidence_counts_upper = Incidence_ratios_upper * livebirths
    ) %>%
    select(-Incidence_ratios, -Incidence_ratios_lower, -Incidence_ratios_upper, -livebirths)
  
  ########################################################################
  ## Incidence Rate (Output ME)
  dt <- get_model_results(gbd_team = "epi", gbd_id = me_o, measure_id = 6, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2, year_id = year_id)
  
  dt_incidence_rate <- dt %>%
    merge(location_data_input, by = "location_id") %>%
    rename(
      "Incidence_rate" = "mean",
      "Incidence_rate_lower" = "lower",
      "Incidence_rate_upper" = "upper"
    ) %>%
    select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Incidence_rate, Incidence_rate_lower, Incidence_rate_upper)
  
  ########################################################################
  ## Merge all three outputs into a final table
  dt_incidence_merged <- dt_incidence_ratios %>%
    merge(dt_incidence_counts, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name")) %>%
    merge(dt_incidence_rate,   by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))# %>%
    #mutate(across(c(Incidence_ratios, Incidence_ratios_lower, Incidence_ratios_upper,
    #                Incidence_counts, Incidence_counts_lower, Incidence_counts_upper), round, 2))
  
  # Optionally store each result with a named entry
  result_name <- paste0("input_", me_i, "_output_", me_o)
  incidence_results_list[[result_name]] <- dt_incidence_merged
}

########################################################################
## Appending all tables in list
final_incidence_table <- dplyr::bind_rows(incidence_results_list, .id = "source_pair") %>%
  arrange(source_pair, year_id, sort_order) %>%
  left_join(ages, by = "age_group_id") %>% unique()

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
# Women population
# WRA <- get_population(age_group_id=most_detailed_ages, location_id=location_data_input$location_id, year_id=year_id, sex_id=2, release_id=release_id)
# WRA <- WRA %>% select(location_id, year_id, age_group_id, population)
# 
# ########################################################################
# ## Prevalence Rate and Counts
# dt <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2)
# 
# dt_prevalence_rate <- dt %>%
#   merge(location_data_input, by = "location_id") %>%
#   rename(
#     "Prevalence_rate" = "mean",
#     "Prevalence_rate_lower" = "lower",
#     "Prevalence_rate_upper" = "upper"
#   ) %>%
#   select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
# 
# ########################################################################
# ## Prevalence Counts
# 
# dt_prevalence_counts <- dt_prevalence_rate %>%
#   merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
#   mutate(
#     Prevalence_counts = Prevalence_rate * population,
#     Prevalence_counts_lower = Prevalence_rate_lower * population,
#     Prevalence_counts_upper = Prevalence_rate_upper * population
#   ) %>%
#   select(-population, -Prevalence_rate, -Prevalence_rate_lower, -Prevalence_rate_upper)


#################################################################################
# Get WRA pop
WRA <- get_population(age_group_id = most_detailed_ages, location_id = location_data_input$location_id, year_id = year_id, sex_id = 2, release_id = release_id) %>%
  select(location_id, year_id, age_group_id, population)

# Initialize output list
prevalence_results_list <- list()

# Loop through MEs
for (i in seq_along(output_prevalence_mes)) {
  me <- output_prevalence_mes[i]
  
  # Special case: summed prevalence from 1536 + 1537
  if (me == 1536) {
    dt_1536 <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    
    dt_1537 <- get_model_results(gbd_team = "epi", gbd_id = 1537, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    
    dt_1536 <- dt_1536 %>%
      merge(location_data_input, by = "location_id") %>%
      rename(mean_1536 = mean, lower_1536 = lower, upper_1536 = upper)
    
    dt_1537 <- dt_1537 %>%
      merge(location_data_input, by = "location_id") %>%
      rename(mean_1537 = mean, lower_1537 = lower, upper_1537 = upper)
    
    # Adding Prevalence Rates
    dt_sum <- dt_1536 %>%
      inner_join(dt_1537, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name")) %>%
      mutate(
        Prevalence_rate = mean_1536 + mean_1537,
        Prevalence_rate_lower = lower_1536 + lower_1537,
        Prevalence_rate_upper = upper_1536 + upper_1537
      ) %>%
      select(modelable_entity_id = modelable_entity_id.x,
             age_group_id, year_id, sort_order, location_id, location_name,
             Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    dt_prevalence_counts <- dt_sum %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    prevalence_results_list[["me_1536_plus_1537"]] <- dt_prevalence_counts
    
    # Special case: summed prevalence from 27705 + 27702 + 27703 + 27704
  } else if (me == 27705) {
    ids_277xx <- c(27705, 27702, 27703, 27704)
    
    ## Prevalence Rate and Counts
    
    dt_list <- list()
    
    for (id in ids_277xx) {
      dt_temp <- get_model_results(gbd_team = "epi", gbd_id = id, measure_id = 5,
                                   age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                                   release_id = release_id, sex_id = 2, year_id = year_id) %>%
        merge(location_data_input, by = "location_id") %>%
        rename(
          !!paste0("mean_", id) := mean,
          !!paste0("lower_", id) := lower,
          !!paste0("upper_", id) := upper
        )
      dt_list[[as.character(id)]] <- dt_temp
    }
    
    dt_combined <- Reduce(function(x, y) {
      full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
    }, dt_list)
    
    dt_sum <- dt_combined %>%
      mutate(
        Prevalence_rate = rowSums(select(., starts_with("mean_")), na.rm = TRUE),
        Prevalence_rate_lower = rowSums(select(., starts_with("lower_")), na.rm = TRUE),
        Prevalence_rate_upper = rowSums(select(., starts_with("upper_")), na.rm = TRUE)
      ) %>%
      select(age_group_id, year_id, sort_order, location_id, location_name,
             Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    dt_prevalence_counts <- dt_sum %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    prevalence_results_list[["me_27705_plus_others"]] <- dt_prevalence_counts
    
    # Skip already-included IDs
  } else if (!(me %in% c(1537, 27702, 27703, 27704))) {
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
    
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
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
  "me_1536_plus_1537",
  "me_3623",
  "me_3626",
  "me_2625",
  "me_3635",
  "me_1542",
  "me_3641",
  "me_1552",
  "me_3644",
  "me_10485",
  "me_27705_plus_others"
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

final_combined_table <- final_combined_table %>% select(1:2, age_group_name, location_id, location_name, cause, everything())

write.csv(final_combined_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_maternal_nonfatal_table.csv", row.names = FALSE)
#write.csv(final_combined_table, "/mnt/share/mmbrf/Deliverables/April9_2025_maternal_nonfatal_table.csv", row.names = FALSE)
 
#########################################################################################################################
#########################################################################################################################
# GDM 
#########################################################################################################################
#########################################################################################################################

year_id <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024)
most_detailed_ages <- c(7,8,9,10,11,12,13,14,15)
release_id <- 16

# Sourcing in Draw and Location data
location_data <- get_location_metadata(location_set_id = 35, release_id = 16)
location_data_sort <- location_data %>% filter(location_type %in% c("admin0", "nonsovereign", "region", "superregion", "global")) %>% select(sort_order, location_id, location_name)
#location_data <- location_data %>% select(location_id, region_name, super_region_name, location_type) %>% filter(!location_type %in% c("global", "superregion", "region")) #filter(location_type == "admin0")
location_data_county_names <- location_data %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% filter(location_type %in% c("admin0", "nonsovereign")) %>% select(location_name, country)
location_data <- location_data %>% select(location_id, location_name, ihme_loc_id, region_name, super_region_name, location_type) %>% mutate(country = substr(ihme_loc_id, 1, 3)) %>% left_join(location_data_county_names, by = "country") %>% rename(country_name = location_name.y) %>% select(location_id, region_name, super_region_name, country_name) %>% filter(!is.na(country_name)) %>% mutate(global = "Global")

#location_data_test <- get_location_metadata(location_set_id = 35, release_id = 16)
#location_data_input_test <- location_data %>% select(location_type, location_id, location_name, sort_order) %>% filter(location_type %in% c("nonsovereign","admin0")) #204


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
# dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
#                      gbd_id=27714,
#                      source='epi',
#                      year_id=year_id,
#                      age_group_id=most_detailed_ages,
#                      sex_id=2,
#                      release_id = release_id,
#                      version_id = 907443,
#                      location_id = location_data_input$location_id
# )
# saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714.rds"))

### Pulling Saved Draws
##############################################
dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714.rds"))
df_subset <- get_cols(dt_saved_draws)
df_subset <- df_subset %>% merge(location_data, by = "location_id") #%>% select(-location_type)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(global, super_region_name, region_name, country_name),  # Columns to stack
               values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)


### Incidence Ratio Average / by Region(admin0, countries, were used for calculations) ####
#########################################################################################
result_gdm_incidence_ratio <- mean_ui_columns(df_subset_melt, group_cols = c("year_id", "location_name", "age_group_id"))
result_gdm_incidence_ratio <- result_gdm_incidence_ratio %>%
  left_join(ages, by = "age_group_id") %>%
  rename("Incidence Ratios" = "val", "IR_lower" = "lower", "IR_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence Ratios", "IR_lower", "IR_upper")


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
df_subset_new <- df_subset_new %>% select(-births)

result_gdm_incidence_counts <- mean_ui_columns(df_subset_new, group_cols = c("year_id", "location_name", "age_group_id"))
result_gdm_incidence_counts <- result_gdm_incidence_counts %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence Counts" = "val", "IC_lower" = "lower", "IC_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence Counts", "IC_lower", "IC_upper")

### Incidence Rate = Incidence ratio * ASFR / by Region(admin0, countries, were used for calculations) ###
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_rate <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_rate <- df_subset_rate %>% merge(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_rate <- df_subset_rate %>% mutate(factor = (asfr*(46/52) + (sb_ratio*asfr*(26/52)))) %>% select(-asfr, -sb_ratio) # Check with Pan
#df_subset_rate <- df_subset_rate %>% mutate(factor = asfr) %>% select(-asfr, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_rate <- df_subset_rate[, (draw_cols) := lapply(.SD, function(x) x * factor), 
                                 .SDcols = draw_cols,
                                 by = c("location_id", "year_id", "age_group_id")]
df_subset_rate <- df_subset_rate %>% select(-factor)

result_gdm_incidence_rate <- mean_ui_columns(df_subset_rate, group_cols = c("year_id", "location_name", "age_group_id"))
result_gdm_incidence_rate <- result_gdm_incidence_rate %>%
  merge(ages, by = "age_group_id") %>%
  rename("Incidence Rate" = "val", "IRa_lower" = "lower", "IRa_upper" = "upper") %>%
  select("age_group_id", "age_group_name", "year_id", "location_name", "Incidence Rate", "IRa_lower", "IRa_upper")

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
write.csv(final, paste0("/mnt/team/rgud/pub/users/chrish47/CSV_files/GDM_final_table.csv"), row.names = FALSE)

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
# Women population
# WRA <- get_population(age_group_id=most_detailed_ages, location_id=location_data_input$location_id, year_id=year_id, sex_id=2, release_id=release_id)
# WRA <- WRA %>% select(location_id, year_id, age_group_id, population)
# 
# ########################################################################
# ## Prevalence Rate and Counts
# dt <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5, age_group_id = most_detailed_ages, location_id = location_data_input$location_id, release_id = release_id, sex_id = 2)
# 
# dt_prevalence_rate <- dt %>%
#   merge(location_data_input, by = "location_id") %>%
#   rename(
#     "Prevalence_rate" = "mean",
#     "Prevalence_rate_lower" = "lower",
#     "Prevalence_rate_upper" = "upper"
#   ) %>%
#   select(modelable_entity_id, age_group_id, year_id, sort_order, location_id, location_name, Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
# 
# ########################################################################
# ## Prevalence Counts
# 
# dt_prevalence_counts <- dt_prevalence_rate %>%
#   merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
#   mutate(
#     Prevalence_counts = Prevalence_rate * population,
#     Prevalence_counts_lower = Prevalence_rate_lower * population,
#     Prevalence_counts_upper = Prevalence_rate_upper * population
#   ) %>%
#   select(-population, -Prevalence_rate, -Prevalence_rate_lower, -Prevalence_rate_upper)


#################################################################################
# Get WRA pop
WRA <- get_population(age_group_id = most_detailed_ages, location_id = location_data_input$location_id, year_id = year_id, sex_id = 2, release_id = release_id) %>%
  select(location_id, year_id, age_group_id, population)

# Initialize output list
prevalence_results_list <- list()

# Loop through MEs
for (i in seq_along(output_prevalence_mes)) {
  me <- output_prevalence_mes[i]
  
  # Special case: summed prevalence from 1536 + 1537
  if (me == 1536) {
    dt_1536 <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    
    dt_1537 <- get_model_results(gbd_team = "epi", gbd_id = 1537, measure_id = 5,age_group_id = most_detailed_ages, location_id = location_data_input$location_id,release_id = release_id, sex_id = 2, year_id = year_id)
    
    dt_1536 <- dt_1536 %>%
      merge(location_data_input, by = "location_id") %>%
      rename(mean_1536 = mean, lower_1536 = lower, upper_1536 = upper)
    
    dt_1537 <- dt_1537 %>%
      merge(location_data_input, by = "location_id") %>%
      rename(mean_1537 = mean, lower_1537 = lower, upper_1537 = upper)
    
    # Adding Prevalence Rates
    dt_sum <- dt_1536 %>%
      inner_join(dt_1537, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name")) %>%
      mutate(
        Prevalence_rate = mean_1536 + mean_1537,
        Prevalence_rate_lower = lower_1536 + lower_1537,
        Prevalence_rate_upper = upper_1536 + upper_1537
      ) %>%
      select(modelable_entity_id = modelable_entity_id.x,
             age_group_id, year_id, sort_order, location_id, location_name,
             Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
    dt_prevalence_counts <- dt_sum %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    prevalence_results_list[["me_1536_plus_1537"]] <- dt_prevalence_counts
    
    # Special case: summed prevalence from 27705 + 27702 + 27703 + 27704
  } else if (me == 27705) {
    ids_277xx <- c(27705, 27702, 27703, 27704)
    
    ## Prevalence Rate and Counts
    
    dt_list <- list()
    
    for (id in ids_277xx) {
      dt_temp <- get_model_results(gbd_team = "epi", gbd_id = id, measure_id = 5,
                                   age_group_id = most_detailed_ages, location_id = location_data_input$location_id,
                                   release_id = release_id, sex_id = 2, year_id = year_id) %>%
        merge(location_data_input, by = "location_id") %>%
        rename(
          !!paste0("mean_", id) := mean,
          !!paste0("lower_", id) := lower,
          !!paste0("upper_", id) := upper
        )
      dt_list[[as.character(id)]] <- dt_temp
    }
    
    dt_combined <- Reduce(function(x, y) {
      full_join(x, y, by = c("age_group_id", "year_id", "sort_order", "location_id", "location_name"))
    }, dt_list)
    
    dt_sum <- dt_combined %>%
      mutate(
        Prevalence_rate = rowSums(select(., starts_with("mean_")), na.rm = TRUE),
        Prevalence_rate_lower = rowSums(select(., starts_with("lower_")), na.rm = TRUE),
        Prevalence_rate_upper = rowSums(select(., starts_with("upper_")), na.rm = TRUE)
      ) %>%
      select(age_group_id, year_id, sort_order, location_id, location_name,
             Prevalence_rate, Prevalence_rate_lower, Prevalence_rate_upper)
    
    dt_prevalence_counts <- dt_sum %>%
      merge(WRA, by = c("location_id", "year_id", "age_group_id")) %>%
      mutate(
        Prevalence_counts = Prevalence_rate * population,
        Prevalence_counts_lower = Prevalence_rate_lower * population,
        Prevalence_counts_upper = Prevalence_rate_upper * population
      ) %>%
      select(-population)
    
    prevalence_results_list[["me_27705_plus_others"]] <- dt_prevalence_counts
    
    # Skip already-included IDs
  } else if (!(me %in% c(1537, 27702, 27703, 27704))) {
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
    
    # Calculating Prevalence Counts(Prevalence Rates * WRA)
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

final_combined_table <- final_combined_table %>% select(age_group_id, age_group_name,	location_id, location_name, cause,	year_id, sort_order, everything())

write.csv(final_combined_table, "/mnt/team/rgud/pub/users/chrish47/CSV_files/final_maternal_nonfatal_table_gdm.csv", row.names = FALSE)
write.csv(final_combined_table, "/mnt/share/mmbrf/Deliverables/April9_2025_maternal_nonfatal_table_gdm.csv", row.names = FALSE)

#####################################################################################################################################################
#####################################################################################################################################################
# GDM Percentage Change
# We want the % change for the aggregated age group 10-54 prevalence from 1990-2023.

dt_saved_draws <- readRDS(paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714.rds"))
df_subset <- get_cols(dt_saved_draws) %>% filter(year_id %in% c(1990, 2024)) # Do 2023
df_subset <- df_subset %>% merge(location_data, by = "location_id") #%>% select(-location_type)

# Melting data. Location_name will included super_region_name, region_name, and country_name.
df_subset_melt <- df_subset %>%
  tidyr::pivot_longer(cols = c(global, super_region_name, region_name, country_name),  # Columns to stack
                      values_to = "location_name"                              # New column with the value
  ) %>% select(-name)
df_subset_melt <- as.data.table(df_subset_melt)

### Incidence Rate, Incidence ratio * ASFR / by Region(admin0, countries, were used for calculations) ###
##############################################
stillbirths_ratio <- fread("/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv") %>%
  select(location_id, year_id, age_group_id, mean_value) %>%
  rename("sb_ratio" = "mean_value")

df_subset_rate <- df_subset_melt %>% merge(asfr, by = c("location_id", "year_id", "age_group_id")) %>% rename(asfr=mean_value)
df_subset_rate <- df_subset_rate %>% merge(stillbirths_ratio, by = c("location_id", "year_id", "age_group_id"))
df_subset_rate <- df_subset_rate %>% mutate(factor = (asfr*(46/52) + (sb_ratio*asfr*(26/52)))) %>% select(-asfr, -sb_ratio)
#df_subset_rate <- df_subset_rate %>% mutate(factor = asfr) %>% select(-asfr, -sb_ratio)
draw_cols <- paste0("draw_", 0:999)
df_subset_rate <- df_subset_rate[, (draw_cols) := lapply(.SD, function(x) x * factor), 
                                 .SDcols = draw_cols,
                                 by = c("location_id", "year_id", "age_group_id")]
df_subset_rate <- df_subset_rate %>% select(-factor)

df_subset_rate_melt <- melt(data = df_subset_rate, id.vars = c("age_group_id", "location_id", "location_name", "year_id")) %>% unique() #North Africa and Middle East, South Asia are both regions and super regions

# Step 1 — Aggregate if necessary (optional)
# df_agg <- df_subset_rate_melt[, .(value = mean(value, na.rm = TRUE)),
#                               by = .(age_group_id, location_id, location_name, variable, year_id)]

# Step 2 — Reshape wide on year_id
df_wide_year <- dcast(df_subset_rate_melt,
                      age_group_id + location_id + location_name + variable ~ year_id,
                      value.var = "value")

# Step 3 — Calculate Percent Change
df_wide_year[, pct_change := ((`2024` - `1990`) / `1990`) * 100]
df_wide_year <- df_wide_year %>% mutate(pct_change = ((`2024` - `1990`) / `1990`) * 100)

# Step 4 — Reshape again — pivot variables
df_wide_year <- dcast(df_wide_year,
                      age_group_id + location_id + location_name ~ variable,
                      value.var = "pct_change")

result_gdm_incidence_rate_pct <- mean_ui_columns(df_wide_year, group_cols = c("location_name"))
# mean_ui_columns() is totally appropriate if your goal is to collapse all age_group_id 7 to 16 into a single summary estimate (mean + uncertainty) for 10-54 per location.

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

write.csv(final_incidence_pct_change, "/mnt/share/mmbrf/Deliverables/April11_2025_gdm_pct_2024_1990_10_54.csv", row.names = FALSE)

