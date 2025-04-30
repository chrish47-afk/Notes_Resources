## ---------------------------
##
## Script name: mortality_input_tables.R 
##
## Purpose of script: Create input mortality tables for  processing by the updated source counts code
##
## Author: Max Dodge
##
## Last edited: 2024-03-14
##
## Email: maxdodge@uw.edu
##
## ---------------------------
##
## Notes:
## We have a finalized NID-indexed table that contains all the grades for our columns of interest.
## Epi tables are all joined together and then joined with the CoD table.
## All the bundle data then is joined to NID-indexed graded table to produce the final inputs.
##
## ---------------------------

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

## ---------------------------

### Pulling in Raw CoD data
cod_data <- read_xlsx("/mnt/team/rgud/priv/Maternal/jenny/for_Max/fatal_cod/fatal_cod_data_parent_with_outliers.xlsx")

### Filtering out the parent NIDs - They have been split out into child NIDs in an earlier step
cod_data <- cod_data %>%
  filter(is.na(underlying_nid)) %>%
  mutate(db = "CoD") %>%
  mutate(year_start = year_id) %>%
  mutate(year_end = year_id) %>%
  mutate(cause_name = "CoD")

### Pulling in raw Epi data
folder <- "/mnt/team/rgud/priv/Maternal/jenny/for_Max/fatal_epi"
file_list <- list.files(path = folder, pattern = "fatal_epi_data", full.names = TRUE)

xlsx_contents <- lapply(file_list, read_xlsx)

epi_data <- do.call(bind_rows, xlsx_contents)
remove(xlsx_contents)

epi_data <- epi_data %>%
  mutate(db = "Epi")

### Merging Cod and Epi data
bundle_data <- bind_rows(cod_data, epi_data) %>%
  select(nid, underlying_nid, location_id, cause_id, data_type_name, cause_name, db, year_start, year_end)

### Pulling in dummy table (each row is an NID)
mortality_dummy <- read_xlsx("/mnt/share/homes/maxdodge/data_landscape/data/mortality/mortality_dummy_2024_03_14.xlsx") %>%
  select(-parent_nid, -title, -most_recent_year) %>%
  dplyr::rename(year_end_dummy = year_end, year_start_dummy = year_start)

### Combining - Relationship set to many to many due to some rows in the dummy table having the same NID
bundle_dummy <- left_join(mortality_dummy, bundle_data, by = join_by(nid, location_id),
                          relationship = "many-to-many")
### Cleaning
final_input_table <- bundle_dummy %>%
  mutate(year_start = ifelse(is.na(year_start), year_start_dummy, year_start)) %>%
  mutate(year_end = ifelse(is.na(year_end), year_end_dummy, year_end)) %>%
  select(-year_start_dummy, -year_end_dummy)

### Final output
write.xlsx(final_input_table, "/ihme/scratch/users/maxdodge/mortality_landscape/input_table/mortality_input.xlsx")
