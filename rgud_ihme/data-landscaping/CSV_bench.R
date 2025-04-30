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

output_prevalence_mes <- c(1536,1537,27705, 27702, 27703, 27704)
output_prevalence_mes <- c(32529, 32531,32532, 28983, 28985, 28986)

for (i in output_prevalence_mes) {
#Run this draw call only if necessary.
#An RDS file was saved to save time. Look below.
dt_draw <- get_draws(gbd_id_type='modelable_entity_id',
                     gbd_id=i,
                     source='epi',
                     year_id=year_id,
                     age_group_id=most_detailed_ages,
                     sex_id=2,
                     release_id = release_id, #16, 33
                     measure_id = 5,
                     location_id = location_data_input$location_id #targeted locations
)
saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_",i,"_16_targetedlocs.rds"))
#saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_gdm/me_27714_default_version.rds"))
}


#(28983, 28985, 28986, 32529, 32531, 32532)
#(895995, 895988, 895990, 895992, 895978, 895979)
me <- c(28983, 28985, 28986, 32529, 32531, 32532)
version_id <- c(895995, 895988, 895990, 895992, 895978, 895979)

# Loop over index positions
for (i in seq_along(me)) {
  current_me <- me[i]
  current_version <- version_id[i]
  
  dt_draw <- get_draws(
    gbd_id_type = 'modelable_entity_id',
    gbd_id = current_me,
    source = 'epi',
    year_id = year_id,
    age_group_id = most_detailed_ages,
    sex_id = 2,
    release_id = 33,  # or 16, depending on your context
    measure_id = 5,
    version_id = current_version,
    location_id = location_data_input$location_id
  )
  
  saveRDS(dt_draw, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_", current_me, "_33_targetedlocs.rds"))
}


EP_gbd_uncomp_prev<-get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 28983, measure_id = 5,
                              source = "epi", sex_id = 2, age_group_id=c(7:15), release_id=33, version_id = 895995)
saveRDS(EP_gbd_uncomp_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28983.rds"))

EP_gbd_comp_acute_prev<- get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 28985, measure_id = 5,
                                   source = "epi", sex_id = 2, release_id = 33, age_group_id=c(7:15), version_id=895988)
saveRDS(EP_gbd_comp_acute_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28985.rds"))

EP_gbd_comp_sub_prev<- get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 28986, measure_id = 5,
                                 source = "epi", sex_id = 2, release_id = 33, age_group_id=c(7:15), version_id=895990)
saveRDS(EP_gbd_comp_sub_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_28986.rds"))



misc_gbd_uncomp_prev<-get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 32529, measure_id = 5,
                                source = "epi", sex_id = 2, age_group_id=c(7:15), release_id=33, version_id=895992)
saveRDS(misc_gbd_uncomp_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32529.rds"))

misc_gbd_comp_acute_prev<-get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 32531, measure_id = 5,
                                    source = "epi", sex_id = 2, age_group_id=c(7:15), release_id=33, version_id=895978)
saveRDS(misc_gbd_comp_acute_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32531.rds"))

misc_gbd_comp_sub_prev<-get_draws(gbd_id_type = "modelable_entity_id", gbd_id = 32532, measure_id = 5,
                                  source = "epi", sex_id = 2, age_group_id=c(7:15), release_id=33, version_id=895979)
saveRDS(misc_gbd_comp_sub_prev, paste0("/mnt/team/rgud/pub/users/chrish47/RDS_nf_maternal_prevalence/me_32532.rds"))


#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
library(dplyr)

# Ectopic Pregnancy -------------
#28983, 28985, 28986
dt <- get_model_results(gbd_team = "epi", gbd_id = 28983, measure_id = 5,
                        age_group_id = most_detailed_ages, location_id = 1,
                        release_id = 33, sex_id = 2, year_id = year_id) 
dt <- dt %>% select(age_group_id, year_id, location_id, mean, lower, upper)

dt2 <- get_model_results(gbd_team = "epi", gbd_id = 28985, measure_id = 5,
                        age_group_id = most_detailed_ages, location_id = 1,
                        release_id = 33, sex_id = 2, year_id = year_id)
dt2 <- dt2 %>% select(age_group_id, year_id, location_id, mean, lower, upper)

dt3 <- get_model_results(gbd_team = "epi", gbd_id = 28986, measure_id = 5,
                        age_group_id = most_detailed_ages, location_id = 1,
                        release_id = 33, sex_id = 2, year_id = year_id)
dt3 <- dt3 %>% select(age_group_id, year_id, location_id, mean, lower, upper)


# Bind all three dataframes together
combined_df_ectopic <- bind_rows(dt, dt2, dt3)

# Group and summarize
summed_df <- combined_df_ectopic %>%
  group_by(age_group_id, year_id, location_id) %>%
  summarise(
    mean = sum(mean, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year_id, age_group_id)


# Hemorrhage -------------
#28983, 28985, 28986
dt <- get_model_results(gbd_team = "epi", gbd_id = 1536, measure_id = 5,
                        age_group_id = most_detailed_ages, location_id = c(1, 102),
                        release_id = 16, sex_id = 2, year_id = year_id) 
dt <- dt %>% select(age_group_id, year_id, location_id, mean, lower, upper)

dt2 <- get_model_results(gbd_team = "epi", gbd_id = 1537, measure_id = 5,
                         age_group_id = most_detailed_ages, location_id = c(1, 102),
                         release_id = 16, sex_id = 2, year_id = year_id)
dt2 <- dt2 %>% select(age_group_id, year_id, location_id, mean, lower, upper)

# Bind all three dataframes together
combined_df_ectopic <- bind_rows(dt, dt2)

# Group and summarize
summed_df <- combined_df_ectopic %>%
  group_by(age_group_id, year_id, location_id) %>%
  summarise(
    mean = sum(mean, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year_id, age_group_id)

# Using get_outputs
mis <- get_outputs(topic = "cause", cause_id = 367, location_id = 1, year_id = 1990, measure_id = 5, metric_id = 3, release_id = 16, age_group_id = 7, sex_id = 2)


