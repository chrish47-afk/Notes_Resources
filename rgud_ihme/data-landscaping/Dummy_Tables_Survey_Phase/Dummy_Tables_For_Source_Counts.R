library(dplyr)
library(tidyverse)
library(writexl)

##Strings-----------------------------------------------------------------------
# Strings together the various ways that the different survey phases are referred
# to. Is used later with grepl() to sort surveys into different phases.

MICS1_string = "MICS1|MICS 1 |MICS 1,|first round of the MICS|
first round of the Multiple|round 1 of the MICS|round 1 of the Multiple|
first round of MICS|first round MICS|MICS was done as part of Round 1"

MICS2_string = "MICS2|MICS 2 |MICS 2,|second round of the MICS|
second round of the Multiple|round 2 of the MICS|round 2 of the Multiple|
second round of MICS|second round MICS|MICS was done as part of Round 2"

MICS3_string = "MICS3|MICS 3 |MICS 3,|third round of the MICS|
third round of the Multiple|round 3 of the MICS|round 3 of the Multiple
|third round of MICS|third round MICS|MICS was done as part of Round 3|
3rd round of MICS|Survey (MICS) round 3"

MICS4_string = "MICS4|MICS 4 |fourth round of the survey series|MICS 4,|
fourth round of the MICS|fourth round of the Multiple|round 4 of the MICS|
round 4 of the Multiple|fourth round of MICS|fourth round MICS|
MICS was done as part of Round 4|MICS was conducted as part of Round 4|
MICS is part of Round 4|Round 4 of the survey series"

MICS5_string = "MICS5|MICS 5 |MICS 5,|fifth round of the MICS|
fifth round of the Multiple|round 5 of the MICS|round 5 of the Multiple|
fifth round of MICS|fifth round MICS|MICS was done as part of Round 5
|phase 5 of the MICS|MICS round 5|series phase 5"

MICS6_string = "MICS6|MICS 6 |MICS 6,|sixth round of the MICS|
sixth round of the Multiple|round 6 of the MICS|round 6 of the Multiple|
sixth round of MICS|sixth round MICS|MICS was done as part of Round 6"

DHS1_string = "DHS1|DHS 1 |phase 1 of the Demographic|phase 1 of the DHS|
round 1 of the DHS|round 1 of the Demographic|phase one of the DHS|
phase one of the demographic"

DHS2_string = "DHS2|DHS 2 |phase 2 of the Demographic|phase 2 of the DHS|
round 2 of the DHS|round 2 of the Demographic|phase two of the DHS|
phase two of the demographic"

DHS3_string = "DHS3|DHS 3 |third phase of the DHS|phase 3 of the Demographic|
phase 3 of the DHS|round 3 of the DHS|round 3 of the Demographic|
phase three of the DHS|phase three of the demographic"

DHS4_string = "DHS4|DHS 4 |fourth phase of the DHS|phase 4 of the Demographic|
phase 4 of the DHS|round 4 of the DHS|round 4 of the Demographic|
phase four of the DHS|phase four of the demographic"

DHS5_string = "DHS5|DHS 5 |phase 5 of the Demographic|phase 5 of the DHS|
round 5 of the DHS|round 5 of the Demographic|phase five of the DHS|
phase five of the demographic"

DHS6_string = "DHS6|DHS 6 |phase 6 of the Demographic|phase 6 of the DHS|
round 6 of the DHS|round 6 of the Demographic|phase six of the DHS|
phase six of the demographic"

DHS7_string = "DHS7|DHS 7 |phase 7 of the Demographic|phase 7 of the DHS|
round 7 of the DHS|round 7 of the Demographic|phase seven of the DHS|
phase seven of the demographic"

                           
##anc1--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
anc1_raw <- read.csv("/mnt/share/scratch/users/maxdodge/past_projects/Data_for_tables/anc1_bundle_location.csv")

#Creating variable for 'All Data'
anc1_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
anc1_raw <- anc1_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
anc1_raw <- anc1_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                          grepl("DHS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                          grepl("RHS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                           grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)

anc1_dummy <- anc1_raw[, c(3,64:93)]

##anc4--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
anc4_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/anc4_bundle_location.csv")

#Creating variable for 'All Data'
anc4_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
anc4_raw <- anc4_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
anc4_raw <- anc4_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)


##csec--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
csec_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/csec_bundle_location.csv")

#Creating variable for 'All Data'
csec_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
csec_raw <- csec_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
csec_raw <- csec_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)


##ifd--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
ifd_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/ifd_bundle_location.csv")

#Creating variable for 'All Data'
ifd_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
ifd_raw <- ifd_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
ifd_raw <- ifd_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)


##mcare--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
mcare_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/mcare_bundle_location.csv")

#Creating variable for 'All Data'
mcare_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
mcare_raw <- mcare_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
mcare_raw <- mcare_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)


##pnc--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
pnc_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/pnc_bundle_location.csv")

#Creating variable for 'All Data'
pnc_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
pnc_raw <- pnc_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
pnc_raw <- pnc_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)


##sba--------------------------------------------------------------------------

#Reading in Bundle-GHDx-Geocodebook merged data
sba_raw <- read.csv("/mnt/share/scratch/users/maxdodge/Data_for_tables/sba_bundle_location.csv")

#Creating variable for 'All Data'
sba_raw$All_data <- 1

#Creating variables for 'Vital_reg', Admin0', 'Admin1', "Admin2', 'GPS' and 'Other_Subnational'
sba_raw <- sba_raw %>%
  mutate(Vital_Reg = case_when((Data.type == "Vital registration") ~ 1, TRUE ~ 0)) %>%
  mutate(Admin0 = case_when(admin_level_final == 0 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin1 = case_when(admin_level_final == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(Admin2 = case_when(admin_level_final == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(GPS = case_when(admin_level_final == 3 ~ 1, TRUE ~ 0)) %>%
  mutate(Other_Subnational = case_when(admin_level_final == 4 ~ 1, TRUE ~ 0))

#Creating variables for 'DHS', 'MICS', 'RHS', 'Other Survey', and 'Other'
sba_raw <- sba_raw %>%
  mutate(DHS = case_when(grepl("DHS", Series.or.system) ~ 1,
                         grepl("DHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(MICS = case_when(grepl("MICS", Series.or.system) ~ 1,
                          grepl("MICS", survey_name) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(RHS = case_when(grepl("RHS", Series.or.system) ~ 1,
                         grepl("RHS", survey_name) ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Other_Survey = case_when((DHS == 1) ~ 0,
                                  (MICS == 1) ~ 0,
                                  (RHS == 1) ~ 0,
                                  (Data.type == "Survey") ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(Other = case_when((DHS == 1) ~ 0,
                           (MICS == 1) ~ 0, 
                           (RHS == 1) ~ 0,
                           (Data.type == "Survey") ~ 0,
                           TRUE ~ 1)) %>%
  mutate(MICS1 = case_when(grepl("MICS1", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS1_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS2 = case_when(grepl("MICS2", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS2_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS3 = case_when(grepl("MICS3", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS3_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS4 = case_when(grepl("MICS4", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS4_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS5 = case_when(grepl("MICS5", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS5_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MICS6 = case_when(grepl("MICS6", file_path, ignore.case = TRUE) ~ 1,
                           grepl(MICS6_string, Summary, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0)) %>%
  mutate(DHS1 = case_when(grepl(DHS1_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS1_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS2 = case_when(grepl(DHS2_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS2_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS3 = case_when(grepl(DHS3_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS3_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS4 = case_when(grepl(DHS4_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS4_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS5 = case_when(grepl(DHS5_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS5_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS6 = case_when(grepl(DHS6_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS6_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(DHS7 = case_when(grepl(DHS7_string, file_path, ignore.case = TRUE) ~ 1,
                          grepl(DHS7_string, Summary, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(Total = DHS + MICS + RHS + Other_Survey + Other) %>%
  mutate(MICS_Total = MICS1 + MICS2 + MICS3 + MICS4 + MICS5 + MICS6) %>%
  mutate(DHS_Total = DHS1 + DHS2 + DHS3 + DHS4 + DHS5 + DHS6 + DHS7) %>%
  mutate(DHS_MICS_Total = MICS_Total + DHS_Total)

##Writing into scratch folder---------------------------------------------------
#write_xlsx(anc1_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/anc1_Oct.xlsx")
#write_xlsx(anc4_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/anc4_Oct.xlsx")
#write_xlsx(csec_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/csec_Oct.xlsx")
#write_xlsx(ifd_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/ifd_Oct.xlsx")
#write_xlsx(mcare_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/mcare_Oct.xlsx")
#write_xlsx(pnc_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/pnc_Oct.xlsx")
#write_xlsx(sba_raw,"/mnt/share/scratch/users/maxdodge/Prepped_Data/sba_Oct.xlsx")



