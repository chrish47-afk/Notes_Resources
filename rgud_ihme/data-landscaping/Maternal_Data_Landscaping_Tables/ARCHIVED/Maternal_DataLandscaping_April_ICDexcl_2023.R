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

library(openxlsx)
library(dplyr)
library(data.table)
library(purrr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(reshape2)

source("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Code/Source Counts Code/source_counts_master.R")


###########################################################################################################################
# Case Definitions - Maternal Obstruction! -- Update ICD Column

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "obstruction_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_obstruction_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/obstruction_landscaping_input_appended.xlsx") 

maternal_obstruction_appended[is.na(maternal_obstruction_appended)] <- 0
length(colnames(maternal_obstruction_appended))

maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.character), as.numeric)) #sapply(dt, class)

maternal_obstruction_appended <- maternal_obstruction_appended %>% replace(is.na(.), 0) %>% mutate(sum = rowSums(.[2:28])) 
length(colnames(maternal_obstruction_appended))

# CREATE MANUALLY. ADD A CASE DEFINITION COLUMN with 1 if sum >= 2
# CREATE MANUALLY. ADD A Data_Join COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
#################### The above process was Automated for Final Tables
transformed_updated_appended_data <- maternal_obstruction_appended %>% mutate('Case_Definition' = case_when(sum >= 2 ~ 1))
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate('Data_Join' = case_when(sum >= 1 ~ 1))
transformed_updated_appended_data[is.na(transformed_updated_appended_data)] <- 0
###################

transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit) %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_obstruction_table <- left_join(transformed_updated_appended_data, append_crosswalk_obstruction, by = 'nid')
write.xlsx(custom_obstruction_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
colnames(custom_obstruction_table)
custom_obstruction_table <- custom_obstruction_table %>% dplyr::rename("prolonged.obstructed.labour.(or.failure.to.progress)" = "prolonged/obstructed.labour.(or.failure.to.progress)", "MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)" = "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)", "near.miss.Uterine.scar.rupture.wo.obstructed.labour" = "near.miss.Uterine.scar.rupture.w/o.obstructed.labour", "near.miss.breech.footing" = "near.miss.breech./.footing", "near.miss.prolonged.obstructed.labor" = "near.miss.prolonged/obstructed.labor")
colnames(custom_obstruction_table)
custom_obstruction_table <- custom_obstruction_table %>% select(-sum, -Data_Join)

colnames(custom_obstruction_table)
# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:30) #6:32
#column_id <- c(29)
# AVOID using /, &, ' in columns
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_obstruction_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_obstruction_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

colnames(custom_obstruction_table)
### Reading Source Counts for each column and converting them to a Data Object!
column_id <- c(2:30) #2:29
#11 no counts
# Community Only has no Source Counts -- All values are zero.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_obstruction_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_obstruction_table)
key_locations_o <- rbind(`All_Data`[-3],`Case_Definition`[-3],`Obstruction.and.rupture`[-3], `Rupture.only`[-3], `Fistula`[-3], `Some.non-near.miss.obstruct.dx`[-3], `cephalopelvic.disproportion`[-3], `malpresentation`[-3], `dystocia.(a.few.sources:.during.1st.and.2nd.stage.of.labor)`[-3], `malposition`[-3], `prolonged.labour`[-3], `prolonged.obstructed.labour.(or.failure.to.progress)`[-3], `Bandl’s.ring`[-3], `ceph-dis.&.prolonged.labour`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `near.miss.uterine.rupture.or.impending.uterine.rupture`[-3], `near.niss.malpresentation`[-3], `near.miss.dystocia`[-3], `near.miss.prolonged.obstructed.labor`[-3], `near.miss.Obstructed.labour.with.uterine.rupture`[-3], `near.miss.Obstructed.labour.without.uterine.rupture`[-3], `near.miss.obstructed.labor`[-3], `near.miss.Uterine.scar.rupture.wo.obstructed.labour`[-3], `near.miss.ceph-pelvic.disproportion`[-3], `near.miss.Bandl’s.ring`[-3], `near.miss.ceph-dis.&.prolonged.labour`[-3], `near.miss.breech.footing`[-3], `near.iss.obstructed.labor`[-3], `ICD`[-3]) %>% unique()
key_locations_o <- left_join(key_locations_o, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_o <- key_locations_o[order(key_locations_o$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_mo <- list(key_locations_o, `All_Data`[2:4],`Case_Definition`[2:4], `Obstruction.and.rupture`[2:4], `Rupture.only`[2:4], `Fistula`[2:4], `Some.non-near.miss.obstruct.dx`[2:4], `cephalopelvic.disproportion`[2:4], `malpresentation`[2:4], `dystocia.(a.few.sources:.during.1st.and.2nd.stage.of.labor)`[2:4], `malposition`[2:4], `prolonged.labour`[2:4], `prolonged.obstructed.labour.(or.failure.to.progress)`[2:4], `Bandl’s.ring`[2:4], `ceph-dis.&.prolonged.labour`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `near.miss.uterine.rupture.or.impending.uterine.rupture`[2:4], `near.niss.malpresentation`[2:4], `near.miss.dystocia`[2:4], `near.miss.prolonged.obstructed.labor`[2:4], `near.miss.Obstructed.labour.with.uterine.rupture`[2:4], `near.miss.Obstructed.labour.without.uterine.rupture`[2:4], `near.miss.obstructed.labor`[2:4], `near.miss.Uterine.scar.rupture.wo.obstructed.labour`[2:4], `near.miss.ceph-pelvic.disproportion`[2:4], `near.miss.Bandl’s.ring`[2:4], `near.miss.ceph-dis.&.prolonged.labour`[2:4], `near.miss.breech.footing`[2:4], `near.iss.obstructed.labor`[2:4], `ICD`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_mo[is.na(final_country_mo)] <- 0


##################################################
# Adding countries with zero counts
location_data <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')
final_country_mo <- left_join(location, final_country_mo, by = c('region_id', 'location_name', 'location_type', 'sort_order'))

##################################################


# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
colnames(final_country_mo)
total_gbd_region <- final_country_mo %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:34) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_mo %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:34) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_mo, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition//") ## filepath to save output .xlsx file
savefilename <- "obstruction_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_obstruction_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/obstruction_landscaping_input_appended.xlsx")
maternal_obstruction_appended[is.na(maternal_obstruction_appended)] <- 0
maternal_obstruction_appended <- maternal_obstruction_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_obstruction_table_noncase_columns <- custom_obstruction_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_obstruction_table_noncase_final <- left_join(maternal_obstruction_appended, custom_obstruction_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
write.xlsx(custom_obstruction_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/custom_obstruction_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_obstruction_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/custom_obstruction_table_noncase_columns.xlsx")


###############################################
# Converting all characters in above dataset to numeric or else the Source COUNTS code will BREAK - This is something that I can perhaps add to the Source Counts Code????????
custom_obstruction_table_noncase_final <- custom_obstruction_table_noncase_final %>% mutate(across(where(is.character), as.numeric)) #sapply(dt, class)
#location_name is also 'character', but its not necessary for source counts. As long as location_id exists, its fine.
###############################################
location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7)

colnames(custom_obstruction_table_noncase_final)
column_id <- c(2:19) #2:19
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_obstruction_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_obstruction_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
column_id <- c(2:14, 16:19)
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_obstruction_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
#colnames(custom_obstruction_table)
key_locations_o <- rbind(`All.ages.or.10-54yo`[-3], `15-49yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Self-report`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_mo <- list(key_locations_o, `All.ages.or.10-54yo`, `15-49yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`, `Self-report`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_mo[is.na(final_country_mo)] <- 0
final_country_mo <- merge(final_country_mo, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

##################################################
# Adding countries with zero counts
location_data <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')
final_country_mo <- left_join(location, final_country_mo, by = c('region_id', 'location_name', 'location_type', 'sort_order'))

##################################################

############### Totals
total_gbd_region <- final_country_mo %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_mo %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_mo, total_gbd_region, total_gbd_country)

# Case Definition

location_data <- get_location_metadata(location_set_id = 35, gbd_round_id = 7)

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

recent_year_data <- custom_obstruction_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]
final_merge_aes[is.na(final_merge_aes)] <- 0

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction_Update_Oct/Final Tables/MaternalObstruction_Inc_Landscaping_final.xlsx')
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# Case Definitions - Maternal Hypertension! --- Update ICD column

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "hypertension_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_hypertension_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/hypertension_landscaping_input_appended.xlsx") 
#maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.numeric), as.character))
maternal_hypertension_appended[is.na(maternal_hypertension_appended)] <- 0
length(colnames(maternal_hypertension_appended))
maternal_hypertension_appended <- maternal_hypertension_appended %>% mutate(sum = rowSums(.[2:18]))
# Modified Appended File
write.xlsx(maternal_hypertension_appended, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/updated_hypertension_landscaping_input_appended.xlsx")
# CREATE MANUALLY. ADD A 'CASE DEFINITION COLUMN' with 1 if sum >= 2
# CREATE MANUALLY. ADD A 'Data_Join' COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
transformed_updated_appended_data <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/updated_hypertension_landscaping_input_appended.xlsx")
colnames(transformed_updated_appended_data)
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)# %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_hypertension_table <- left_join(transformed_updated_appended_data, append_crosswalk_obstruction, by = 'nid')
write.xlsx(custom_hypertension_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx")
#custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_hypertension_table <- custom_hypertension_table %>% select(-sum, -Data_Join)
colnames(custom_hypertension_table)
custom_hypertension_table <- custom_hypertension_table %>% dplyr::rename("MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)"= "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)", "Preeclampsia.to.Eclampsia.spectrum.(no.cHTN.or.gHTN)" = "Preeclampsia.->.Eclampsia.spectrum.(no.cHTN.or.gHTN)", "near.miss.eclampsia.preeclampsia" = "near.miss.eclampsia/preeclampsia")
colnames(custom_hypertension_table)
write.xlsx(custom_hypertension_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx")
custom_hypertension_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx") %>% select(-measure)
colnames(custom_hypertension_table)

# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:20) #6:32
#column_id <- 2
#column_id <- c(29)
# AVOID using /, &, ' in columns
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_hypertension_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_hypertension_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

### Reading Source Counts for each column and converting them to a Data Object!
colnames(custom_hypertension_table)
column_id <- c(2:20) #2:29
# Community Only has no Source Counts -- All values are zero.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_hypertension_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_hypertension_table)
key_locations_h <- rbind(`All_Data`[-3],`Case.Definition`[-3], `All.HDoP,.inc..cHTN.during.pregnancy`[-3], `All.HDoP.excl..cHTN.during.pregnancy`[-3], `Preeclampsia.to.Eclampsia.spectrum.(no.cHTN.or.gHTN)`[-3], `Severe.preeclampsia.&.eclampsia.only.(inc..HELLP)`[-3], `Severe.preeclampsia.only.(inc..HELLP.but.excl..eclampsia)`[-3], `HELLP.only`[-3], `Eclampsia.only`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `near.miss.HDoP`[-3], `near.miss.PIH`[-3], `near.miss.severe.preeclampsia`[-3], `near.miss.HELLP`[-3], `Near.miss.severe.preeclampsia.&.eclampsia.only.(inc..HELLP)`[-3], `near.miss.eclampsia.preeclampsia`[-3], `near.miss.HELLP.&.eclampsia`[-3], `near.miss.eclampsia`[-3], `ICD`[-3]) %>% unique()
key_locations_h <- left_join(key_locations_h, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_h <- key_locations_h[order(key_locations_h$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_mh <- list(key_locations_h, `All_Data`[2:4],`Case.Definition`[2:4], `All.HDoP,.inc..cHTN.during.pregnancy`[2:4], `All.HDoP.excl..cHTN.during.pregnancy`[2:4], `Preeclampsia.to.Eclampsia.spectrum.(no.cHTN.or.gHTN)`[2:4], `Severe.preeclampsia.&.eclampsia.only.(inc..HELLP)`[2:4], `Severe.preeclampsia.only.(inc..HELLP.but.excl..eclampsia)`[2:4], `HELLP.only`[2:4], `Eclampsia.only`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `near.miss.HDoP`[2:4], `near.miss.PIH`[2:4], `near.miss.severe.preeclampsia`[2:4], `near.miss.HELLP`[2:4], `Near.miss.severe.preeclampsia.&.eclampsia.only.(inc..HELLP)`[2:4], `near.miss.eclampsia.preeclampsia`[2:4], `near.miss.HELLP.&.eclampsia`[2:4], `near.miss.eclampsia`[2:4], `ICD`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_mh[is.na(final_country_mh)] <- 0

colnames(final_country_mh)
# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
total_gbd_region <- final_country_mh %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:24) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_mh %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:24) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_mh, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition//") ## filepath to save output .xlsx file
savefilename <- "hypertension_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_hypertension_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/hypertension_landscaping_input_appended.xlsx")
maternal_hypertension_appended[is.na(maternal_hypertension_appended)] <- 0
maternal_hypertension_appended <- maternal_hypertension_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_hypertension_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Case Definition/custom_hypertension_bundlev_appended_data.xlsx")
custom_hypertension_table_noncase_columns <- custom_hypertension_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_hypertension_table_noncase_final <- left_join(maternal_hypertension_appended, custom_hypertension_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
nrow(custom_hypertension_table_noncase_final)
write.xlsx(custom_hypertension_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/custom_hypertension_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_hypertension_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/custom_hypertension_table_noncase_columns.xlsx")

colnames(custom_hypertension_table_noncase_final)
column_id <- c(2:19) #2:19
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_hypertension_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_hypertension_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
column_id <- c(2:14, 16:19)
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_hypertension_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
#colnames(custom_obstruction_table)
key_locations_h <- rbind(`All.ages.or.10-54yo`[-3], `15-49yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Self-report`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_mh <- list(key_locations_h, `All.ages.or.10-54yo`, `15-49yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`, `Self-report`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_mh[is.na(final_country_mh)] <- 0
final_country_mh <- merge(final_country_mh, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

############### Totals
total_gbd_region <- final_country_mh %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_mh %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_mh, total_gbd_region, total_gbd_country)

# Case Definition

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

colnames(custom_hypertension_table_noncase_final)
recent_year_data <- custom_hypertension_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hypertesnsive_HDOP_Update_Oct/Final Tables/MaternalHypertension_Inc_Landscaping_final.xlsx')

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# Case Definitions - Maternal Hemorrhage! - Update for ICD column

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "hemorrhage_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_hemorrhage_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/hemorrhage_landscaping_input_appended.xlsx") 
#maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.numeric), as.character))
maternal_hemorrhage_appended[is.na(maternal_hemorrhage_appended)] <- 0
length(colnames(maternal_hemorrhage_appended))
colnames(maternal_hemorrhage_appended)
maternal_hemorrhage_appended <- maternal_hemorrhage_appended %>% mutate(sum = rowSums(.[2:12]))
# Modified Appended File
write.xlsx(maternal_hemorrhage_appended, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/updated_hemorrhage_landscaping_input_appended.xlsx")
# CREATE MANUALLY. ADD A 'CASE DEFINITION COLUMN' with 1 if sum >= 2
# CREATE MANUALLY. ADD A 'Data_Join' COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
transformed_updated_appended_data <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/updated_hemorrhage_landscaping_input_appended.xlsx")
colnames(transformed_updated_appended_data)
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_hemorrhage <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)# %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_hemorrhage_table <- left_join(transformed_updated_appended_data, append_crosswalk_hemorrhage, by = 'nid')
write.xlsx(custom_hemorrhage_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx")
#custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_hemorrhage_table <- custom_hemorrhage_table %>% select(-sum, -Data_Join)
colnames(custom_hemorrhage_table)
custom_hemorrhage_table <- custom_hemorrhage_table %>% dplyr::rename("MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)"= "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)")
colnames(custom_hemorrhage_table)
write.xlsx(custom_hemorrhage_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx")
custom_hemorrhage_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx")
colnames(custom_hemorrhage_table)

# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:14) #6:32
#column_id <- c(29)
# AVOID using /, &, ' in columns
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_hemorrhage_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_hemorrhage_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

### Reading Source Counts for each column and converting them to a Data Object!
colnames(custom_hemorrhage_table)
column_id <- c(2:14) #2:29
# Community Only has no Source Counts -- All values are zero.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_hemorrhage_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_hemorrhage_table)
key_locations_hem <- rbind(`All_Data`[-3],`Case.Definition`[-3], `Combined.post-partum.and.ante-partum`[-3], `Post-partum.only`[-3], `Ante-partum.only`[-3], `Combined.severe.and.non`[-3], `Severe.only.(by.volume.criterion)`[-3], `Non-severe.only.(by.volume.criteria)`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `near.miss.PPH`[-3], `near.miss.APH`[-3], `near.miss.hemorrhage.(combined.APH,.PPH)`[-3], `ICD`[-3]) %>% unique()
key_locations_hem <- left_join(key_locations_hem, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_hem <- key_locations_hem[order(key_locations_hem$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_hem <- list(key_locations_hem, `All_Data`[2:4],`Case.Definition`[2:4], `Combined.post-partum.and.ante-partum`[2:4], `Post-partum.only`[2:4], `Ante-partum.only`[2:4], `Combined.severe.and.non`[2:4], `Severe.only.(by.volume.criterion)`[2:4], `Non-severe.only.(by.volume.criteria)`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `near.miss.PPH`[2:4], `near.miss.APH`[2:4], `near.miss.hemorrhage.(combined.APH,.PPH)`[2:4], `ICD`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_hem[is.na(final_country_hem)] <- 0

colnames(final_country_hem)
# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
total_gbd_region <- final_country_hem %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:18) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_hem %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:18) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_hem, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/") ## filepath to save output .xlsx file
savefilename <- "hemorrhage_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_hemorrhage_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/hemorrhage_landscaping_input_appended.xlsx")
maternal_hemorrhage_appended[is.na(maternal_hemorrhage_appended)] <- 0
maternal_hemorrhage_appended <- maternal_hemorrhage_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_hemorrhage_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Case Definition/custom_hemorrhage_bundlev_appended_data.xlsx")
custom_hemorrhage_table_noncase_columns <- custom_hemorrhage_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_hemorrhage_table_noncase_final <- left_join(maternal_hemorrhage_appended, custom_hemorrhage_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
nrow(custom_hemorrhage_table_noncase_final)
write.xlsx(custom_hemorrhage_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/custom_hemorrhage_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_hemorrhage_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/custom_hemorrhage_table_noncase_columns.xlsx")

colnames(custom_hemorrhage_table_noncase_final)
column_id <- c(2:19) #2:19
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_hemorrhage_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_hemorrhage_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
colnames(custom_hemorrhage_table_noncase_final)
column_id <- c(2:14, 16, 18, 19)
#column_id <- 15
# No self_report for hemorrhage
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_hemorrhage_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
#colnames(custom_obstruction_table)
key_locations_hem <- rbind(`All.ages.or.10-54yo`[-3], `15-49yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_hem <- list(key_locations_hem, `All.ages.or.10-54yo`, `15-49yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_hem[is.na(final_country_hem)] <- 0
final_country_hem <- merge(final_country_hem, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

length(colnames(final_country_hem))
############### Totals
total_gbd_region <- final_country_hem %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:21) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_hem %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:21) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_hem, total_gbd_region, total_gbd_country)

# Case Definition

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

colnames(custom_hemorrhage_table_noncase_final)
recent_year_data <- custom_hemorrhage_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Hemorrhage_Update_Oct/Final Tables/MaternalHemorrhage_Inc_Landscaping_final.xlsx')

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# Case Definitions - Maternal Infection! -- Update ICD code column

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "infection_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_infection_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/infection_landscaping_input_appended.xlsx") 
#maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.numeric), as.character))
maternal_infection_appended[is.na(maternal_infection_appended)] <- 0
length(colnames(maternal_infection_appended))
colnames(maternal_infection_appended)
maternal_infection_appended <- maternal_infection_appended %>% mutate(sum = rowSums(.[2:8]))
# Modified Appended File
write.xlsx(maternal_infection_appended, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/updated_infection_landscaping_input_appended.xlsx")
# CREATE MANUALLY. ADD A 'CASE DEFINITION COLUMN' with 1 if sum >= 2
# CREATE MANUALLY. ADD A 'Data_Join' COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
transformed_updated_appended_data <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/updated_infection_landscaping_input_appended.xlsx")
colnames(transformed_updated_appended_data)
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_infection <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)# %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_infection_table <- left_join(transformed_updated_appended_data, append_crosswalk_infection, by = 'nid')
write.xlsx(custom_infection_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx")
#custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_infection_table <- custom_infection_table %>% select(-sum, -Data_Join)
colnames(custom_infection_table)
custom_infection_table <- custom_infection_table %>% dplyr::rename("MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)"= "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)")
colnames(custom_infection_table)
write.xlsx(custom_infection_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx")
custom_infection_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx")
colnames(custom_infection_table)

# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:10) #6:32
#column_id <- c(29)
# AVOID using /, &, ' in columns
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_infection_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_infection_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

### Reading Source Counts for each column and converting them to a Data Object!
colnames(custom_infection_table)
column_id <- c(2:10) #2:29
# Community Only has no Source Counts -- All values are zero.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_infection_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_infection_table)
key_locations_i <- rbind(`All_Data`[-3],`Case.Definition`[-3], `Maternal.sepsis.&.other.maternal.infections`[-3], `Maternal.sepsis`[-3], `Other.maternal.infections`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `Chorioamnionitis`[-3], `near.miss.sepsis`[-3], `ICD`[-3]) %>% unique()
key_locations_i <- left_join(key_locations_i, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_i <- key_locations_i[order(key_locations_i$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_i <- list(key_locations_i, `All_Data`[2:4],`Case.Definition`[2:4], `Maternal.sepsis.&.other.maternal.infections`[2:4], `Maternal.sepsis`[2:4], `Other.maternal.infections`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `Chorioamnionitis`[2:4], `near.miss.sepsis`[2:4], `ICD`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_i[is.na(final_country_i)] <- 0

colnames(final_country_i)
# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
total_gbd_region <- final_country_i %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:14) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_i %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:14) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_i, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/") ## filepath to save output .xlsx file
savefilename <- "infection_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_infection_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/infection_landscaping_input_appended.xlsx")
maternal_infection_appended[is.na(maternal_infection_appended)] <- 0
maternal_infection_appended <- maternal_infection_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_infection_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Case Definition/custom_infection_bundlev_appended_data.xlsx")
custom_infection_table_noncase_columns <- custom_infection_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_infection_table_noncase_final <- left_join(maternal_infection_appended, custom_infection_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
nrow(custom_infection_table_noncase_final)
write.xlsx(custom_infection_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/custom_infection_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_infection_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/custom_infection_table_noncase_columns.xlsx")

colnames(custom_infection_table_noncase_final)
column_id <- c(2:20) #2:19
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_infection_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_infection_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
colnames(custom_infection_table_noncase_final)
column_id <- c(2:14,16:19)
#column_id <- 15
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_infection_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
#colnames(custom_obstruction_table)
key_locations_i <- rbind(`All.ages.or.10-54yo`[-3], `15-49yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Self-report`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_i <- list(key_locations_i, `All.ages.or.10-54yo`, `15-49yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`, `Self-report`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_i[is.na(final_country_i)] <- 0
final_country_i <- merge(final_country_i, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

length(colnames(final_country_i))
############### Totals
total_gbd_region <- final_country_i %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_i %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_i, total_gbd_region, total_gbd_country)

# Case Definition

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

colnames(custom_infection_table_noncase_final)
recent_year_data <- custom_infection_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Sepsis_Infection_Update_Oct/Final Tables/MaternalInfection_Inc_Landscaping_final.xlsx')
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# Case Definitions - Maternal Ectopic Pregnancy! -- Updated for ICD Column

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "ectopic_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_ectopic_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/ectopic_landscaping_input_appended.xlsx") 
#maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.numeric), as.character))
maternal_ectopic_appended[is.na(maternal_ectopic_appended)] <- 0
length(colnames(maternal_ectopic_appended))
colnames(maternal_ectopic_appended)
maternal_ectopic_appended <- maternal_ectopic_appended %>% mutate(sum = rowSums(.[2:7]))
# Modified Appended File
write.xlsx(maternal_ectopic_appended, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/updated_ectopic_landscaping_input_appended.xlsx")
# CREATE MANUALLY. ADD A 'CASE DEFINITION COLUMN' with 1 if sum >= 2
# CREATE MANUALLY. ADD A 'Data_Join' COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
transformed_updated_appended_data <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/updated_ectopic_landscaping_input_appended.xlsx")
colnames(transformed_updated_appended_data)
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_ectopic <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)# %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_ectopic_table <- left_join(transformed_updated_appended_data, append_crosswalk_ectopic, by = 'nid')
write.xlsx(custom_ectopic_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx")
#custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_ectopic_table <- custom_ectopic_table %>% select(-sum, -Data_Join)
colnames(custom_ectopic_table)
custom_ectopic_table <- custom_ectopic_table %>% dplyr::rename("MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)"= "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)")
colnames(custom_ectopic_table)
write.xlsx(custom_ectopic_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx")
custom_ectopic_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx")
colnames(custom_ectopic_table)

# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:9) #6:32
#column_id <- c(29)
# AVOID using /, &, ' in columns
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_ectopic_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_ectopic_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

### Reading Source Counts for each column and converting them to a Data Object!
colnames(custom_ectopic_table)
column_id <- c(2, 3, 4, 5, 6, 7, 8, 9)
# No counts: 2,
# Community Only has no Source Counts -- All values are zero.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_ectopic_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_ectopic_table)
key_locations_e <- rbind(`All_Data`[-3],`Case.Definition`[-3], `Ectopic.pregnancy`[-3], `Ruptured.ectopic.pregnancy`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `near.miss.ruptured.ectopic`[-3], `near.miss.ectopic`[-3], `ICD`[-3]) %>% unique()
key_locations_e <- left_join(key_locations_e, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_e <- key_locations_e[order(key_locations_e$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_e <- list(key_locations_e, `All_Data`[2:4],`Case.Definition`[2:4],`Ectopic.pregnancy`[2:4], `Ruptured.ectopic.pregnancy`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `near.miss.ruptured.ectopic`[2:4], `near.miss.ectopic`[2:4], `ICD`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_e[is.na(final_country_e)] <- 0

colnames(final_country_e)
# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
total_gbd_region <- final_country_e %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:13) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_e %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:13) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_e, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/") ## filepath to save output .xlsx file
savefilename <- "ectopic_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_ectopic_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/ectopic_landscaping_input_appended.xlsx")
maternal_ectopic_appended[is.na(maternal_ectopic_appended)] <- 0
maternal_ectopic_appended <- maternal_ectopic_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_ectopic_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Case Definition/custom_ectopic_bundlev_appended_data.xlsx")
custom_ectopic_table_noncase_columns <- custom_ectopic_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_ectopic_table_noncase_final <- left_join(maternal_ectopic_appended, custom_ectopic_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
nrow(custom_ectopic_table_noncase_final)
write.xlsx(custom_ectopic_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/custom_ectopic_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_ectopic_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/custom_ectopic_table_noncase_columns.xlsx")

colnames(custom_ectopic_table_noncase_final)
column_id <- c(2:19) #2:19
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_ectopic_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_ectopic_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
colnames(custom_ectopic_table_noncase_final)
column_id <- c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 18, 19)
#zero counts for 15-40yo or Self report or Community as like with the others!
# No self_report for hemorrhage
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_ectopic_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
colnames(custom_ectopic_table)
key_locations_e <- rbind(`All.ages.or.10-54yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_e <- list(key_locations_e, `All.ages.or.10-54yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_e[is.na(final_country_e)] <- 0
final_country_e <- merge(final_country_e, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

length(colnames(final_country_e))
############### Totals
total_gbd_region <- final_country_e %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:20) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_e %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:20) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_e, total_gbd_region, total_gbd_country)

# Case Definition

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

colnames(custom_ectopic_table_noncase_final)
recent_year_data <- custom_ectopic_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Ectopic_Update_Oct/Final Tables/MaternalEctopic_Inc_Landscaping_final.xlsx')
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# Case Definitions - Maternal Abortion! Updates for October

# Appending Data -- This is run accordingly. Not every time, but only if necessary.
# Data Directory
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/")

savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/") ## filepath to save output .xlsx file
savefilename <- "abortion_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

# Reading in Appended File
maternal_abortion_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/abortion_landscaping_input_appended.xlsx") 
#maternal_obstruction_appended <- maternal_obstruction_appended %>% mutate(across(where(is.numeric), as.character))
maternal_abortion_appended[is.na(maternal_abortion_appended)] <- 0
length(colnames(maternal_abortion_appended))
colnames(maternal_abortion_appended)
maternal_abortion_appended <- maternal_abortion_appended %>% mutate(sum = rowSums(.[2:15]))
# Modified Appended File
write.xlsx(maternal_abortion_appended, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/updated_abortion_landscaping_input_appended.xlsx")
# CREATE MANUALLY. ADD A 'CASE DEFINITION COLUMN' with 1 if sum >= 2
# CREATE MANUALLY. ADD A 'Data_Join' COLUMN with 1 if sum >= 1
# Reading Manually Modified Updated Appended File
transformed_updated_appended_data <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/updated_abortion_landscaping_input_appended.xlsx")
colnames(transformed_updated_appended_data)
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% filter(Data_Join %in% '1') %>% unique()
nrow(transformed_updated_appended_data)
transformed_updated_appended_data <- transformed_updated_appended_data %>% mutate(All_Data = 1)

# Reading all Maternal Bundle Version in. This is necessary in order to adequately include in all the sources for each corresponding cause from the appended data.
m_o_bundle <- get_bundle_version(29876)
m_f_bundle <- get_bundle_version(21626)
m_mh_bundle <- get_bundle_version(20930)
m_ps_bundle <- get_bundle_version(20945)
m_omi_bundle <- get_bundle_version(20948)
m_hd_bundle <- get_bundle_version(20933)
m_e_bundle <- get_bundle_version(20936)
m_sp_bundle <- get_bundle_version(21185)
m_ao_bundle <- get_bundle_version(20942)
m_ep_bundle <- get_bundle_version(20951)
# IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)
m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name, measure)

#Stacking all Maternal Bundle Version Data
append_crosswalk_abortion <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)# %>% select(-measure)
#append_crosswalk_obstruction_vetting <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Joining Case Definition included sources(rows), and left-joining against all the Maternal Bundle versions -- This allows for every source included for every Case Definition group, to be counted.
custom_abortion_table <- left_join(transformed_updated_appended_data, append_crosswalk_abortion, by = 'nid')
write.xlsx(custom_abortion_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx")
#custom_obstruction_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Data Inputs/Case Definition/custom_obstruction_bundlev_appended_data.xlsx")
custom_abortion_table <- custom_abortion_table %>% select(-sum, -Data_Join)
colnames(custom_abortion_table)
custom_abortion_table <- custom_abortion_table %>% dplyr::rename("MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)"= "MNM./.SMM./.SAMM./.SMO./.etc..(by.clinical.criteria)", "other.near.miss.abortion.miscarriage" = "other.near.miss.abortion/miscarriage", "near.miss.hemorrhage.due.to.abortion.ectopic" = "near.miss.hemorrhage.due.to.abortion/ectopic")
colnames(custom_abortion_table)
write.xlsx(custom_abortion_table, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx")
custom_abortion_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx")
colnames(custom_abortion_table)

# GENERATING SOURCE COUNTS FOR EACH CASE DEFINITION column.
# Please adjust the column_id range as necessary and file output for Source Counts.
column_id <- c(2:6, 7:17) #2:17
#column_id <- (6)
#Exclude 6: Complicated.miscarriage.only
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_abortion_table)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_abortion_table, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/"))
}

# Sourcing/calling Location Data
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')

### Reading Source Counts for each column and converting them to a Data Object!
colnames(custom_abortion_table)
column_id <- c(4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
#column_id <- 6
#No counts for: 2(Surgical.abortion.only), 3(Medication.abortion.only)
#No counts for: 6: Complicated.miscarriage.only
for(i in column_id){
  for(j in i)
    column <- colnames(custom_abortion_table)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}

colnames(custom_abortion_table)
key_locations_a <- rbind(`All_Data`[-3],`Case.Definition`[-3], `ICD`[-3], `Abortion.only`[-3], `Miscarriage.only`[-3], `Combined.abortion.and.miscarriage`[-3], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[-3], `near.miss.abortion.(unclear.whether.spontaneous.or.other.abortion)`[-3], `near.miss.miscarriage`[-3], `near.miss.abortion-related.hemorrhage`[-3], `near.miss.infected.abortion`[-3], `other.near.miss.abortion.miscarriage`[-3], `near.miss.hemorrhage.due.to.abortion.ectopic`[-3]) %>% unique()
key_locations_a <- left_join(key_locations_a, location, by = c('region_id','location_name')) %>% arrange('sort_order')
key_locations_a <- key_locations_a[order(key_locations_a$sort_order),] #sorting

#locations_obstruction <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/All_MaternalData/Final Table/Maternal_Obstruction_Incidence_table.xlsx") %>% select(location_name, hierarchy_key)

final_country_a <- list(key_locations_a, `All_Data`[2:4],`Case.Definition`[2:4], `ICD`[2:4], `Abortion.only`[2:4], `Miscarriage.only`[2:4], `Combined.abortion.and.miscarriage`[2:4], `MNM.SMM.SAMM.SMO.etc..(by.clinical.criteria)`[2:4], `near.miss.abortion.(unclear.whether.spontaneous.or.other.abortion)`[2:4], `near.miss.miscarriage`[2:4], `near.miss.abortion-related.hemorrhage`[2:4], `near.miss.infected.abortion`[2:4], `other.near.miss.abortion.miscarriage`[2:4], `near.miss.hemorrhage.due.to.abortion.ectopic`[2:4]) %>% reduce(left_join, by = c("location_name", "hierarchy_key"))
final_country_a[is.na(final_country_a)] <- 0

colnames(final_country_a)
# Adding a Total GBD Region and Total GBD Country rows for Case Definitions
total_gbd_region <- final_country_a %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:18) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% 'Total_GBD_Region')

total_gbd_country <- final_country_a %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:18) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% 'Total_GBD_Country')

# Final Merge
final <- rbind(final_country_a, total_gbd_region, total_gbd_country)

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_CaseDefinition_only.xlsx")

############ Non Case Definition Columns #########################################################
##################################################################################################
##################################################################################################
# Double Check that Im joining the correct data with the appended noncase columns!
#   -- Something is happening that is not adding up. Double check!
# 
#######################
datadir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/") ## location of data to append
#/mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction

###savedir <- paste0("C:\Users\laurakem\Desktop") ## filepath to save output .xlsx file
savedir <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/") ## filepath to save output .xlsx file
savefilename <- "abortion_landscaping_input" ## name you want for output .xlsx file
### gen list of filenames
datanames <- dir(datadir, pattern = ".xlsx")
print(paste0(datanames))

all_data <- data.frame()
for (file in datanames){
  ##print is a check row for debugging; can delete once code works
  print(paste0(file))
  data <- read.xlsx(paste0(datadir, file))
  datalist <- list(all_data, data)
  if(nrow(data) != 0){
    data$filename <- file
  }
  all_data <- rbindlist(datalist, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

write.xlsx(all_data, paste0(savedir, savefilename, "_appended.xlsx"), rowNames = FALSE)

#### Creating the Obstruction Incidence Table for Mae's Meeting on 09/15/22
### Using Appended Data and Crosswalk Data for Data Inputs in the Source Counts Code

maternal_abortion_appended <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/abortion_landscaping_input_appended.xlsx")
maternal_abortion_appended[is.na(maternal_abortion_appended)] <- 0
maternal_abortion_appended <- maternal_abortion_appended %>% unique()

# m_o_bundle <- get_bundle_version(29876)
# m_f_bundle <- get_bundle_version(21626)
# m_mh_bundle <- get_bundle_version(20930)
# m_ps_bundle <- get_bundle_version(20945)
# m_omi_bundle <- get_bundle_version(20948)
# m_hd_bundle <- get_bundle_version(20933)
# m_e_bundle <- get_bundle_version(20936)
# m_sp_bundle <- get_bundle_version(21185)
# m_ao_bundle <- get_bundle_version(20942)
# m_ep_bundle <- get_bundle_version(20951)
# #write.xlsx(m_o_bundle, "/ihme/homes/chrish47/m_o_bundle.xlsx")
# #write.xlsx(m_f_bundle, "/ihme/homes/chrish47/m_f_bundle.xlsx")
# # IMPORTANT COLUMNS TO HAVE: nid, ihme_loc_id, clinical_data_type, measure, location_name, and location_id.
# m_o_bundle_edit <- m_o_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_f_bundle_edit <- m_f_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_mh_bundle_edit <- m_mh_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ps_bundle_edit <- m_ps_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_omi_bundle_edit <- m_omi_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_hd_bundle_edit <- m_hd_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_e_bundle_edit <- m_e_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_sp_bundle_edit <- m_sp_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ao_bundle_edit <- m_ao_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# m_ep_bundle_edit <- m_ep_bundle %>% select(nid, location_id, year_start, year_end, location_name)
# 
# #append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit)
# append_crosswalk_obstruction <- rbind(m_o_bundle_edit, m_f_bundle_edit, m_mh_bundle_edit, m_ps_bundle_edit, m_omi_bundle_edit, m_hd_bundle_edit, m_e_bundle_edit, m_sp_bundle_edit, m_ao_bundle_edit, m_ep_bundle_edit)

# Custom selected columns from Case Definitions
custom_abortion_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Case Definition/custom_abortion_bundlev_appended_data.xlsx")
custom_abortion_table_noncase_columns <- custom_abortion_table %>% select(nid, location_id, year_start, year_end, location_name) %>% unique()

custom_abortion_table_noncase_final <- left_join(maternal_abortion_appended, custom_abortion_table_noncase_columns, by = 'nid') %>% filter(!location_id %in% NA)
nrow(custom_abortion_table_noncase_final)
write.xlsx(custom_abortion_table_noncase_final, "/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/custom_abortion_table_noncase_columns.xlsx")
# I left joined the noncase columns from our appended data for Maternal Obstruction(etc.). All of Laura's data used to create the appended file. However for Clinical, only the Clinical sources in the Maternal Obstruction bundle versions were included in the appended file. This was done accordingly for all causes.
# The main reason why Clinical Sources were excluded for the overall appended file according to each cause, is because the way we reviewed the Clinical sources, was based on presence of ICD codes and length. The Clinical Data itself was not specifically inspected closely, so It would have inaccurate to include sources that are clearly not in a particular bundle version in this final landscaping table.
# I filtered out sources with NA, most of these coming from Laura's whole appended files, because these don't exist in the data bin with a maternal obstruction case definition, etc. Same for other causes.
custom_abortion_table_noncase_final <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/custom_abortion_table_noncase_columns.xlsx")

colnames(custom_abortion_table_noncase_final)
column_id <- c(2:19) #2:19
#column_id <- 2
# Community only and Chart Review only have zeros's. No 1's. So these two will have an empty table. Skip for this step.
for(i in column_id)
{
  for(j in i)
    column <- colnames(custom_abortion_table_noncase_final)[i] #6-32
  out_path <- '/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/'
  if(file.exists(paste0(out_path, paste0(column)))){
    print('folder already exists')
  } else {
    dir.create(paste0(out_path, paste0(column)))
    print('new folder created')
  }
  print(i)
  print(column)
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect) # To avoid the 16 connection max error!
  #custom_obstruction_table_updated <- custom_obstruction_table %>% select(1:5,column)
  source_counts(custom_data_table = custom_abortion_table_noncase_final, custom_subset_column_names = column, custom_subset_column_values = 1, out_path = paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/"))
}

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
location <- location_data %>% select(region_id, location_name, location_type, sort_order) %>% filter(location_type %in% 'region' | location_type %in% 'admin0')


### Doing it manually like this is Annoying, so I NEED TO FIND A BETTER WAY TO DO THIS! #########################################################################
colnames(custom_abortion_table_noncase_final)
column_id <- c(2:14,16:19)
# No self_report for hemorrhage
# Community Only has no Source Counts -- All values are zero. Don't source this one in.
for(i in column_id){
  for(j in i)
    column <- colnames(custom_abortion_table_noncase_final)[i] #6-32
  print(column)
  readpath <- paste0("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Data Inputs/Non_Case Definition/Source Counts/",column,"/source_counts/final_counts/subset_Custom_Table_counts_adj.xlsx")
  print(readpath)
  test_v <- read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj') #renaming using an outside variable
  
  
  do.call("<-",list(column, (read.xlsx(readpath, sheet = 4) %>% dplyr::rename(!!column := 'n_adj'))))
}
# /mnt/share/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Obstruction/Source Counts/Admissions/source_counts/final_counts

###################################################################################################### Putting the tables together!
colnames(custom_abortion_table)
key_locations_a <- rbind(`All.ages.or.10-54yo`[-3], `15-49yo`[-3], `Narrower.age-group`[-3], `National`[-3], `Comprehensive.subnationals`[-3], `Limited.subnationals`[-3], `Live.Birth`[-3], `Pregnancy`[-3], `Woman`[-3], `Deliveries`[-3], `Admissions`[-3], `Age`[-3], `Any.or.not.specified`[-3], `In-facility.only`[-3], `Admin.data`[-3], `Self-report`[-3], `Registry.or.surveillance`[-3]) %>% unique()

#total_counts_countries_key
final_country_a <- list(key_locations_a, `All.ages.or.10-54yo`, `15-49yo`, `Narrower.age-group`, `National`, `Comprehensive.subnationals`, `Limited.subnationals`, `Live.Birth`, `Pregnancy`, `Woman`, `Deliveries`, `Admissions`, `Age`, `Any.or.not.specified`, `In-facility.only`, `Admin.data`,`Self-report`, `Registry.or.surveillance`) %>% reduce(left_join, by = c("region_id", "location_name", "hierarchy_key"))
final_country_a[is.na(final_country_a)] <- 0
final_country_a <- merge(final_country_a, location, by = c('region_id','location_name')) %>% setorder(sort_order) #%>% dplyr::select(-"sort_order", -"location_type") #%>% unique()

length(colnames(final_country_a))
############### Totals
total_gbd_region <- final_country_a %>% filter(hierarchy_key %in% "gbd_region") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Region"))
total_gbd_region <- total_gbd_region %>% filter(location_name %in% "Total_GBD_Region")

total_gbd_country <- final_country_a %>% filter(hierarchy_key %in% "gbd_country") %>% select(1:22) %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total_GBD_Country"))
total_gbd_country <- total_gbd_country %>% filter(location_name %in% "Total_GBD_Country")
##########

final <- rbind(final_country_a, total_gbd_region, total_gbd_country)

# Case Definition

# Most Recent Year #########
location_data_edit <- location_data %>% select(location_id, location_name, ihme_loc_id) %>% mutate(country = substr(ihme_loc_id, 1, 3))
location_data_collapse <- location_data %>% select(location_id, ihme_loc_id, location_name) %>% dplyr::rename(country = ihme_loc_id)
location_data_edit <- left_join(location_data_edit, location_data_collapse, by = 'country') %>% select(1,4:6) %>% dplyr::rename(location_id = location_id.x, location_name = location_name.y) %>% select(-2,-3)

colnames(custom_abortion_table_noncase_final)
recent_year_data <- custom_abortion_table_noncase_final %>% select(location_id, year_end)
final_year_data <- left_join(recent_year_data, location_data_edit, by = "location_id")
final_recent_year <- left_join((final %>% select(location_name)), final_year_data, by = 'location_name') %>% unique()

final_recent_year <- final_recent_year[order(final_recent_year$location_name, -final_recent_year$year_end),]
final_recent_year <- final_recent_year[!duplicated(final_recent_year$location_name),]
final_recent_year <- final_recent_year %>% select(1,3)

# Final
final <- left_join(final, final_recent_year, by = 'location_name')

write.xlsx(final,"/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_NonCaseDefinition_only.xlsx")


##### Putting everything Together, both the Case Definition and Non Case Definition Columns ####
################################################################################################
################################################################################################
noncase <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_NonCaseDefinition_only.xlsx")
case <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_CaseDefinition_only.xlsx") #%>% select(-sort_order)

final_merge_aes <- merge(noncase, case, by = c("region_id", "location_name", "hierarchy_key", "sort_order", "location_type"))
final_merge_aes <- final_merge_aes <- final_merge_aes[order(final_merge_aes$sort_order),]

write.xlsx(final_merge_aes,'/ihme/scratch/users/chrish47/source_counts_mb/Final_Incidence_LandS_Tables/Data Files/Maternal_Abortion_Update_Oct/Final Tables/MaternalAbortion_Inc_Landscaping_final.xlsx')

