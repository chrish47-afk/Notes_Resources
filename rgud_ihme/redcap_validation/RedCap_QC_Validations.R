rm(list = ls())
if (Sys.info()['sysname'] == 'Linux') {
  j_root <- '/home/j/'
  h_root <- '~/'
} else {
  j_root <- 'J:/'
  h_root <- 'H:/'
}

library(dplyr)
library(data.table)
library(openxlsx)
library(readr)
library(readxl)
library(writexl)

# Get system information
system_info <- Sys.info()
# Extract the username
username <- system_info["user"]

#!/usr/bin/env Rscript
# Export Records
token <- "Enter Token"
url <- "https://redcap.iths.org/api/"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter=',',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

## Helper Functions
# Custom function to check for fuzzy matches
# Function for fuzzy matching with partial matches
fuzzy_partial_match <- function(strings, target_words, max_distance = 2) {
  string_words <- str_split(strings, "\\s+")
  
  sapply(string_words, function(words) {
    any(stringdist::stringdistmatrix(words, target_words) <= max_distance)
  })
}


# Function to clean and modify values based on the conditions
clean_symbols <- function(x) {
  x <- as.character(x) # Ensure the column is treated as a character
  
  # Handle ">" by adding 0.01
  x <- ifelse(str_detect(x, "^>"), as.numeric(str_remove(x, "^>")) + 0.01, x)
  
  # Handle "<" by subtracting 0.01
  x <- ifelse(str_detect(x, "^<"), as.numeric(str_remove(x, "^<")) - 0.01, x)
  
  # Handle ">=", "<=", "=<", and "=>" by simply removing the symbols
  x <- ifelse(str_detect(x, "^(>=|<=|=<|=>)"), as.numeric(str_remove(x, "^(>=|<=|=<|=>)")), x)
  
  # Convert back to numeric for further processing
  as.numeric(x)
}

# Function to flag outliers in specified range
flag_outliers <- function(units, lower, upper, range_min, range_max) {
  if (units == 1) {
    # Vectorized check for values outside the range
    lower_outlier <- lower < range_min | lower > range_max
    upper_outlier <- upper < range_min | upper > range_max
    lower_outlier | upper_outlier
  } else if (units == 2) {
    lower_outlier <- lower < range_min | lower > range_max
    upper_outlier <- upper < range_min | upper > range_max
    lower_outlier | upper_outlier
  } else if (units == 3) {
    lower_outlier <- lower < range_min | lower > range_max
    upper_outlier <- upper < range_min | upper > range_max
    lower_outlier | upper_outlier
  } else if (units == 4) {
    lower_outlier <- lower < range_min | lower > range_max
    upper_outlier <- upper < range_min | upper > range_max
    lower_outlier | upper_outlier
  } else if (units == 5) {
    lower_outlier <- lower < range_min | lower > range_max
    upper_outlier <- upper < range_min | upper > range_max
    lower_outlier | upper_outlier
  } else {
    # Default case: no outliers flagged if units are invalid
    rep(FALSE, length(lower))
  }
}


#df <- as.data.frame(result) # For Testing Purposes
quality_check <- function(df, filepath = h_root) {
  # For extractor identification
  df_usernames <- df %>% select(record_id, extractor) %>%
    group_by(record_id) %>%
    mutate(all_na = all(is.na(extractor))) %>% # Check if all rows for record_id are NA
    ungroup() %>%
    filter(all_na | !is.na(extractor)) %>% # Keep groups with all NA or rows with non-NA extractor
    unique() %>% select(-all_na) %>%
    mutate(
      extractor = recode(extractor, `1` = "chrish47", `2` = "kepan", `3` = "sunnypyl", `4` = "marndt", `5` = "maxdodge", `6` = "vcian", `7` = "corbette", `8` = "rsmith35")
    )
  
  # Adding dsr_refid
  df_dsr <- df %>% select(record_id, dsr_refid) %>% filter(!is.na(dsr_refid))
  
  df <- as.data.frame(df) %>%
    mutate(
      extractor = recode(extractor, `1` = "chrish47", `2` = "kepan", `3` = "sunnypyl", `4` = "marndt", `5` = "maxdodge", `6` = "vcian", `7` = "corbette", `8` = "rsmith35")
    )
  
  # Saving Original/Raw df
  filepath_df <- paste0(filepath, "original_redcap_extractions.csv")
  write.csv(df, filepath_df, row.names = FALSE)
  cat(paste0("The original and complete Redcap extraction data frame has been saved here: ", filepath_df, "\n"))
  
  df <- df %>% filter(!is.na(redcap_repeat_instance)) #This was to get rid of the 0 instance that has many NA values. Contains to extraction data by default.
  
  ###################################################################################
  # Effect estimate inconsistencies or errors
  # Columns to check: effect_multiple, effect_multiple_desc, and instance_description & effect_estimate, effect_lower, and effect_upper 
  # Check for adjusted or unadjusted
  df_effect <- df %>%
    select(record_id, redcap_repeat_instance, instance_description, outcome, risk, comparison_group, effect_multiple_desc, effect_estimate, effect_lower, effect_upper) %>% mutate(across(everything(), ~ str_to_lower(.)))
  target_words <- c("adjust", "unadjust", "adjusted", "unadjusted", "crude", "raw", "model", "reference")
  # Apply screening function to each column
  df_effect <- df_effect %>% mutate(matches_effect = fuzzy_partial_match(effect_multiple_desc, target_words, max_distance = 2)) %>%
    mutate(matches_instance = fuzzy_partial_match(instance_description, target_words, max_distance = 2))
  # Checking that effect size(estimate) is in the confidence interval range. In between lower and upper.
  df_effect <- df_effect %>% mutate(effect_estimate = as.numeric(effect_estimate),effect_lower = as.numeric(effect_lower),effect_upper = as.numeric(effect_upper))
  df_effect <- df_effect %>% mutate(in_confidence_range = effect_estimate >= effect_lower & effect_estimate <= effect_upper)
  
  df_effect_size <- df_effect %>%
    filter(matches_effect == TRUE & matches_instance == TRUE) %>%
    group_by(record_id, outcome, risk, comparison_group) %>% # Group by the specified column(s)
    mutate(
      median_value = median(effect_estimate, na.rm = TRUE), # Calculate median within the group
      potential_outlier = ifelse(
        (effect_estimate > median_value * 3) | 
          (effect_estimate < median_value / 3),
        TRUE, FALSE
      )
    ) %>%
    ungroup() %>% # Remove grouping
    filter(potential_outlier == TRUE) %>%
    select(record_id, redcap_repeat_instance, instance_description, effect_multiple_desc, effect_estimate, effect_lower, effect_upper, potential_outlier) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id") %>%
    select(-potential_outlier)
  
  df_effect_outside <- df_effect %>% 
    select(record_id, redcap_repeat_instance,instance_description, effect_multiple_desc, effect_estimate, effect_lower, effect_upper, in_confidence_range) %>%
    filter(in_confidence_range == FALSE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id") %>%
    select(-in_confidence_range)
  
  # FILE: Vetting 'adjusted' and 'unadjusted' values.
  # The goal was to identify any potential outliers in effect_estimate values that deviated significantly.
  # Since direct comparisons between 'adjusted' and 'unadjusted' were challenging, the strategy focused on:
  # 1. Records included were those that fit the Targeted Keywords. Group by record_id, outcome, and risk(exposure) & comparison(reference).
  # 2. Flagging any effect_estimate more than 3 times above or below the group median as potential outliers.
  # Targeted Keywords: "adjust", "unadjust", "adjusted", "unadjusted", "crude", "raw", "model", "reference"

  # FILE: Validating all effect_estimate values to ensure they fall within the confidence interval.
  # Records with effect_estimate values outside the specified confidence interval were flagged for review.
  
  # Function to save data with comments in a separate sheet
  save_with_comments <- function(data, comment_lines, filename) {
    wb <- openxlsx::createWorkbook()
    
    # Add data sheet
    addWorksheet(wb, "Data")
    writeData(wb, "Data", data)
    
    # Add comments sheet
    addWorksheet(wb, "Comments")
    writeData(wb, "Comments", comment_lines)
    
    # Save workbook
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }
  # Comments for effect_potential_outlier.xlsx
  comments_effect_outlier <- c(
    "FILE: Vetting 'adjusted' and 'unadjusted' values.",
    "The goal was to identify outliers in effect_estimate values that deviated significantly.",
    "Since direct comparisons between 'adjusted' and 'unadjusted' were challenging, the strategy focused on:",
    "1. Analyzing each record_id's unique values. Records included were those that fit the Targeted Keywords",
    "2. Flagging any effect_estimate more than 3 times above or below the group median as potential outliers.",
    "Targeted Keywords: adjust, unadjust, adjusted, unadjusted, crude, raw, model, reference"
  )
  # Save effect_potential_outlier.xlsx
  filepath_xlsx1 <- paste0(filepath, "effect_potential_outlier.xlsx")
  save_with_comments(df_effect_size, comments_effect_outlier, filepath_xlsx1)
  cat(paste0("effect_potential_outlier.xlsx has been saved here: ", filepath_xlsx1, "\n"))
  # Comments for effect_outside_range.xlsx
  comments_effect_outside <- c(
    "FILE: Validating all effect_estimate values to ensure they fall within the confidence interval.",
    "Records with effect_estimate values outside the specified confidence interval were flagged for review."
  )
  # Save effect_outside_range.xlsx
  filepath_xlsx2 <- paste0(filepath, "effect_outside_range.xlsx")
  save_with_comments(df_effect_outside, comments_effect_outside, filepath_xlsx2)
  cat(paste0("effect_outside_range.xlsx has been saved here: ", filepath_xlsx2, "\n"))
  
  ###################################################################################
  
  ###################################################################################
  # instance and multiple missing labels
  target_words <- c("unadjust", "unadjusted", "model", "reference")
  df_missing_multiple <- df %>% dplyr::select(record_id, redcap_repeat_instance, instance_description, effect_multiple, effect_multiple_desc) %>%
    mutate(matches_instance = fuzzy_partial_match(instance_description, target_words, max_distance = 2)) %>%
    filter(matches_instance == TRUE & (effect_multiple == 0 | is.na(effect_multiple))) %>% # Ensure proper condition grouping
    filter(!is.na(instance_description)) %>%
    group_by(record_id) %>%
    mutate(potential_instance_multiple_error = all(!is.na(instance_description))) %>% # Use mutate to retain all columns
    ungroup() %>%  # Remove grouping to avoid issues in later steps
    select(record_id, redcap_repeat_instance, instance_description, effect_multiple, effect_multiple_desc) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id") %>%
    unique()
  
  # # Saving the workbook
  # filepath_xlsx3 <- paste0(filepath, "missing_multiple.xlsx")
  # openxlsx::write.xlsx(as.data.frame(df_missing_multiple), filepath_xlsx3, rowNames = FALSE, overwrite = TRUE)
  # cat(paste0("missing_multiple.xlsx has been saved here: ", filepath_xlsx3, "\n"))
  # # FILE: Identifying and flagging record_ids with missing effect_multiple values that may require one.
  # # Records were flagged if the instance_description suggested the need for an effect_multiple value.
  # # Targeted Keywords: "adjust", "unadjust", "adjusted", "unadjusted", "crude", "raw", "model", "reference"
  
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  # Add a sheet for the data
  addWorksheet(wb, "Missing Multiple")
  writeData(wb, "Missing Multiple", df_missing_multiple)
  # Add a separate sheet for comments
  addWorksheet(wb, "Comments")
  comments_text <- c(
    "FILE: Identifying and flagging record_ids with missing effect_multiple values that may require one.",
    "Records were flagged if the instance_description suggested the need for an effect_multiple value.",
    "Targeted Keywords: 'adjust', 'unadjust', 'adjusted', 'unadjusted', 'crude', 'raw', 'model', 'reference'"
  )
  writeData(wb, "Comments", comments_text)
  # Save the workbook
  filepath_xlsx3 <- paste0(filepath, "missing_multiple.xlsx")
  openxlsx::saveWorkbook(wb, filepath_xlsx3, overwrite = TRUE)
  # Confirmation message
  cat(paste0("missing_multiple.xlsx has been saved here: ", filepath_xlsx3, "\n"))
  
  ###################################################################################
  
  ###################################################################################
  # Vetting exposure data. Missing exposure data, 
  df_exposure_data_missing <- df %>% group_by(record_id) %>%
    summarize(no_data = all(is.na(exp_lower_1) | grepl("[a-zA-Z]", exp_lower_1, perl = TRUE)) &
        all(is.na(ref_lower_1) | grepl("[a-zA-Z]", ref_lower_1, perl = TRUE)) &
        all(is.na(exp_upper_1) | grepl("[a-zA-Z]", exp_upper_1, perl = TRUE)) &
        all(is.na(ref_upper_1) | grepl("[a-zA-Z]", ref_upper_1, perl = TRUE)) &
        all(is.na(exp_mean_1) | grepl("[a-zA-Z]", exp_mean_1, perl = TRUE)) &
        all(is.na(ref_mean_1) | grepl("[a-zA-Z]", ref_mean_1, perl = TRUE)) &
        all(is.na(exp_sd_1) | grepl("[a-zA-Z]", exp_sd_1, perl = TRUE)) &
        all(is.na(ref_sd_1) | grepl("[a-zA-Z]", ref_sd_1, perl = TRUE)) &
      # Check if exp_cont is either NA or 0, Contines or NOT.
        all(is.na(exp_cont) | exp_cont == 0)) %>%
    filter(no_data == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  
  df_exposure_vetting <- df %>% anti_join(df_exposure_data_missing, by = "record_id") %>% 
    select(record_id, redcap_repeat_instance, risk, comparison_group, exp_units_1, exp_lower_1, ref_lower_1, exp_upper_1, ref_upper_1, exp_mean_1, ref_mean_1, exp_sd_1, ref_sd_1)
  columns_to_modify <- c("record_id", "exp_lower_1", "ref_lower_1", "exp_upper_1", "ref_upper_1", 
                         "exp_mean_1", "ref_mean_1", "exp_sd_1", "ref_sd_1")
  df_exposure_vetting <- df_exposure_vetting %>% mutate(across(all_of(columns_to_modify), clean_symbols)) %>% suppressWarnings()
  
  #Vetting exposure. Comparing lower vs. upper.
  df_exposure_reexp <- df_exposure_vetting %>%
    mutate(
      risk = recode(risk, `1` = "Low BMI", `2` = "High BMI", `3` = "Short Stature", `4` = "Tall Stature"),
      comparison_group = recode(comparison_group, `1` = "Low BMI", `2` = "High BMI", `3` = "Short Stature", `4` = "Tall Stature")
    ) %>% 
    mutate(
      ref_error = (ref_lower_1 > exp_lower_1 | ref_lower_1 > exp_upper_1),
      #exp_error = (ref_upper_1 > exp_upper_1 | exp_upper_1 < ref_upper_1)
    ) %>%
    filter(!is.na(ref_error)) %>%
    filter(ref_error == TRUE) %>%
    filter(risk != "Low BMI" & comparison_group != "High BMI") %>%
    filter(risk != "Short Stature" & comparison_group != "Tall Stature") %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  # FILE: Commparing exposure and comparison group intervexals. Are there ere any errors. In addition wrangling for the targeted column.
  # Columns risk and comparison_group were used to filter out any records with Low BMI or Low Stature.
  
  
  #Vetting exposure range groups(1:5), BMI and stature measurement bounds AGAINST 'standard' ranges.
  #BMI, kg/m2 = 1 ; Stature: cm = 2, m = 3, ft = 4, in = 5
  df_exposure_vetting_kgm2 <- df_exposure_vetting %>% filter(exp_units_1 == 1) %>% 
    mutate(
      ref_outliers = flag_outliers(1, ref_lower_1, ref_upper_1, 10, 60),
      exp_outliers = flag_outliers(1, exp_lower_1, exp_upper_1, 10, 60)
      ) %>%
    filter(ref_outliers == TRUE | exp_outliers == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  df_exposure_vetting_cm <- df_exposure_vetting %>% filter(exp_units_1 == 2) %>% 
    mutate(
      ref_outliers = flag_outliers(2, ref_lower_1, ref_upper_1, 80, 220),
      exp_outliers = flag_outliers(2, exp_lower_1, exp_upper_1, 80, 220)
    ) %>%
    filter(ref_outliers == TRUE | exp_outliers == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  df_exposure_vetting_m <- df_exposure_vetting %>% filter(exp_units_1 == 3) %>% 
    mutate(
      ref_outliers = flag_outliers(3, ref_lower_1, ref_upper_1, 0.80, 2.2),
      exp_outliers = flag_outliers(3, exp_lower_1, exp_upper_1, 0.80, 2.2)
    ) %>%
    filter(ref_outliers == TRUE | exp_outliers == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  df_exposure_vetting_ft <- df_exposure_vetting %>% filter(exp_units_1 == 4) %>% 
    mutate(
      ref_outliers = flag_outliers(4, ref_lower_1, ref_upper_1, 2, 7.2),
      exp_outliers = flag_outliers(4, exp_lower_1, exp_upper_1, 2, 7.2)
    ) %>%
    filter(ref_outliers == TRUE | exp_outliers == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  df_exposure_vetting_in <- df_exposure_vetting %>% filter(exp_units_1 == 5) %>% 
    mutate(
      ref_outliers = flag_outliers(5, ref_lower_1, ref_upper_1, 32, 87),
      exp_outliers = flag_outliers(5, exp_lower_1, exp_upper_1, 32, 87)
    ) %>%
    filter(ref_outliers == TRUE | exp_outliers == TRUE) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id")
  
  # List of dataframes
  dataframes <- list(
    "exposure_risk_comparison_error" = df_exposure_reexp,
    "exposure_data_missing" = df_exposure_data_missing,
    "exposure_kgm2_outliers" = df_exposure_vetting_kgm2,
    "exposure_cm_outliers" = df_exposure_vetting_cm,
    "exposure_m_outliers" = df_exposure_vetting_m,
    "exposure_ft_outliers" = df_exposure_vetting_ft,
    "exposure_in_outliers" = df_exposure_vetting_in
  )
  # FILE exposure_data_missing: Identify records where all relevant columns for the first exposure cohort. (e.g., exp_lower_1, ref_lower_1) contain either NA or invalid values (e.g., letters). Additionally, check if `exp_cont` is either NA or 0.
  # FILE exposure_kgm2_outliers, exposure_cm_outliers, exposure_m_outliers, exposure_ft_outliers, exposure_in_outliers:
  # Cleaning and validating exposure data:
  # 1. Handle special characters (<, >, >=, =<) by applying post-processing rules:
  #    - Subtract 0.01 for values with `<` (e.g., `<20` → `19.99`).
  #    - Add 0.01 for values with `>` (e.g., `>20` → `20.01`).
  #    - Remove symbols for `>=` and `=<` while keeping the numeric value.
  # 2. Remove any remaining special characters to standardize numeric values.
  # 3. Identify extreme values by checking if values fall outside the valid range (10 to 60).
  # 4. Ensure logical consistency: 
  #    - Lower bound must always be less than the upper bound.
  #    - Perform this check separately for exposure and reference groups.
  
  # Create a new Excel workbook
  wb <- openxlsx::createWorkbook()
  # Add each dataframe as a new sheet
  for (sheet_name in names(dataframes)) {
    openxlsx::addWorksheet(wb, sheet_name) # Add a sheet with the name
    openxlsx::writeData(wb, sheet_name, dataframes[[sheet_name]]) # Write the dataframe to the sheet
  }
  # Add a separate sheet for comments
  openxlsx::addWorksheet(wb, "Comments")
  
  # Define structured comments
  comments_text <- c(
    "FILE: exposure_risk_comparison_error",
    "Commparing exposure and comparison group intervexals. Are there ere any errors. In addition wrangling for the targeted column.",
    "Columns risk and comparison_group were used to filter out any records with Low BMI or Low Stature as the risk.",
    "",
    "FILE: exposure_data_missing",
    "Identifies records where all relevant columns for the first exposure cohort (e.g., exp_lower_1, ref_lower_1) contain either NA or invalid values (e.g., letters).",
    "Additionally, checks if `exp_cont` is either NA or 0.",
    "",
    "FILE: exposure_kgm2_outliers, exposure_cm_outliers, exposure_m_outliers, exposure_ft_outliers, exposure_in_outliers",
    "Cleaning and validating exposure data:",
    "1. Handle special characters (<, >, >=, =<) by applying post-processing rules:",
    "   - Subtract 0.01 for values with `<` (e.g., `<20` → `19.99`).",
    "   - Add 0.01 for values with `>` (e.g., `>20` → `20.01`).",
    "   - Remove symbols for `>=` and `=<` while keeping the numeric value.",
    "2. Remove any remaining special characters to standardize numeric values.",
    "3. Identify extreme values by checking if values fall outside the valid range (10 to 60).",
    "4. Ensure logical consistency:",
    "   - Lower bound must always be less than the upper bound.",
    "   - Perform this check separately for exposure and reference groups."
  )
  
  # Write comments to the Comments sheet
  openxlsx::writeData(wb, "Comments", comments_text)
  
  # Save the workbook
  filepath_xlsx4 <- paste0(filepath, "exposure_vetting_report.xlsx")
  openxlsx::saveWorkbook(wb, filepath_xlsx4, overwrite = TRUE)
  
  # Output the file location
  cat(paste0("exposure_vetting_report.xlsx has been saved here: ", filepath_xlsx4, "\n"))  
  ###################################################################################
  
  ###################################################################################
  # Vetting Cause name vs GBD info. Its important that these field are extracted correctly.
  # Reading gbdinfo file that was created for this this validation check.
  # The file contains outcome ids used in Redcap and GBD cause ids used in-house(IHME)
  gbdinfo <- openxlsx::read.xlsx("/mnt/team/rgud/pub/users/chrish47/redcap_validation/data_dictionary_gbdinfo.xlsx")
  # First Check, the following records are missing a cause information
  df_missing_cause <- df %>%
    select(record_id, redcap_repeat_instance, outcome, out_name, out_gbd_cause) %>%
    group_by(record_id) %>%
    mutate(
      missing_outcome_id = all(is.na(outcome)),
      missing_gbd_id = all(is.na(out_gbd_cause))) %>%
    filter(missing_outcome_id == TRUE | missing_gbd_id == TRUE) %>%
    ungroup() %>%  # Remove grouping
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id") %>%
    unique()
  # FILE: Identifying any record_ids that have a missing outcome.
  
  df_cause_nomatch <- df %>%
    select(record_id, redcap_repeat_instance, outcome, out_name, out_gbd_cause) %>%
    merge(gbdinfo, by = "outcome") %>%
    group_by(record_id) %>%
    mutate(
      outcome_match_gbd = all(out_gbd_cause.x %in% out_gbd_cause.y),
      gbd_match_outcome = all(out_gbd_cause.y %in% out_gbd_cause.x)
    ) %>%
    ungroup() %>%  # Remove grouping to avoid issues
    filter(outcome_match_gbd == FALSE) %>%  # Keep only non-matching records
    select(record_id, redcap_repeat_instance, out_name, out_gbd_cause.x, out_gbd_cause.y, out_gbd_cause_name) %>%
    merge(df_usernames, by = "record_id") %>%
    merge(df_dsr, by = "record_id") %>%
    filter(out_gbd_cause.x != out_gbd_cause.y) %>%
    unique()
  # FILE: Identifying record_ids where the outcome does not have a corresponding GBD cause identifier.
  
  # Function to save data with comments in a separate sheet
  save_with_comments <- function(data, comment_lines, filename) {
    wb <- openxlsx::createWorkbook()
    
    # Add data sheet
    addWorksheet(wb, "Data")
    writeData(wb, "Data", data)
    
    # Add comments sheet
    addWorksheet(wb, "Comments")
    writeData(wb, "Comments", comment_lines)
    
    # Save workbook
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }
  
  # Comments for cause_vetting_missing_outcome.xlsx
  comments_missing_outcome <- c(
    "FILE: Identifying any record_ids that have a missing outcome.",
    "This file includes records where the outcome field is missing or incorrectly formatted."
  )
  
  # Save cause_vetting_missing_outcome.xlsx
  filepath_xlsx5 <- paste0(filepath, "cause_vetting_missing_outcome.xlsx")
  save_with_comments(df_missing_cause, comments_missing_outcome, filepath_xlsx5)
  cat(paste0("cause_vetting_missing_outcome.xlsx has been saved here: ", filepath_xlsx5, "\n"))
  
  # Comments for cause_vetting_nomatch.xlsx
  comments_cause_nomatch <- c(
    "FILE: Identifying record_ids where the outcome does not have a corresponding GBD cause identifier.",
    "This file flags records where the extracted outcome does not match the GBD reference cause."
  )
  
  # Save cause_vetting_nomatch.xlsx
  filepath_xlsx6 <- paste0(filepath, "cause_vetting_nomatch.xlsx")
  save_with_comments(df_cause_nomatch, comments_cause_nomatch, filepath_xlsx6)
  cat(paste0("cause_vetting_nomatch.xlsx has been saved here: ", filepath_xlsx6, "\n"))
  ###################################################################################

}


quality_check(result, "/mnt/team/rgud/pub/users/chrish47/redcap_validation/validation_jan_2025/")
#quality_check(result)