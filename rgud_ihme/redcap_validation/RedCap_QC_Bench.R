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

# Set the directory containing the Excel files
dir_path <- "/mnt/team/rgud/pub/users/chrish47/redcap_validation/validation_jan_2025/"

# List all Excel files in the directory
file_list <- list.files(path = dir_path, pattern = "*.xlsx", full.names = TRUE)

# Function to read and convert all columns to character
read_as_character <- function(file) {
  df <- read_excel(file, col_types = "text")  # Read all columns as text
  df$source_file <- basename(file)  # Optional: Add a column to track source file
  return(df)
}

# Read and stack all Excel files
stacked_data <- bind_rows(lapply(file_list, read_as_character))

# Write to a new Excel file
write_xlsx(stacked_data, "/mnt/team/rgud/pub/users/chrish47/redcap_validation/stacked_output.xlsx")

######################################################################################################################
# Overlapping or Duplicate data(This is an error I wanted to go over)
#effect_multiple
#effect_multiple_desc

# Data Dictionary
#!/usr/bin/env Rscript
token <- "Enter Token"
url <- "https://redcap.iths.org/api/"
formData <- list("token"=token,
                  content='metadata',
                  format='csv',
                  returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
#print(result)

#Columns: outcome and out_gbd_cause
result_edit <- result %>% dplyr::filter(field_name %in% c("outcome","out_gbd_cause"))
result_split <- result_edit %>%
  separate_rows(select_choices_or_calculations, sep = " \\| ") %>%
  separate(select_choices_or_calculations, into = c("id", "cause"), sep = ", ", convert = TRUE)
write.xlsx(as.data.frame(result_split), "/mnt/team/rgud/pub/users/chrish47/Redcap_validation/data_dictionary.xlsx", row.names = FALSE)
