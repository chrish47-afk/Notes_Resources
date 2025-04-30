# --------------------------------------
#' Bundle Unique
#'
#' @description This utility bundle function allows you extract unique values for a given bundle_id. Using the specified criteria the user provides. The function will return a unique list matching the provided criteria input. You can use a bundle_id, bundle_version_id, or crosswalk_version_id to adequately pull on unique column values. Please make sure your id's are correct.
#'
#' @param bundle_id bundle_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param bundle_version_id bundle_version_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param crosswalk_version_id crosswalk_version_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param gbd_round_id gbd_round_id, please refer to https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param decomp_step decom_step, please refer to https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param merge_id_columns will merge ID columns (such as sex_id, measure_id, representative_id) onto the dataframe returned
#' @param criteria This is the identifier by which you want to extract unique values'columns' from, for the bundle_id you provide.You can one single criteria or multiple.
#'
#' @return Output with only the unique criteria values you request. Your output will be automatically save into your working directory.
#' @export
#' @import data.table
#' @import plyr
#' @import tidyverse
#' @import dplyr
#'
#' @examples bundle_unique(bundle_id = 1496, criteria = c('nid', 'location_id'))
bundle_unique <- function(bundle_id = NULL, bundle_version_id = NULL, crosswalk_version_id = NULL, gbd_round_id = NULL, decomp_step = NULL, merge_id_columns = FALSE, criteria = NULL){
  invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
  if(is.null(bundle_id) == FALSE){
  dt <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
  id_name <- bundle_id
  as.data.table(dt)
  } else if(is.null(bundle_version_id) == FALSE){
    dt <- get_bundle_version(bundle_version_id = bundle_version_id)
    id_name <- bundle_version_id
    as.data.table(dt)
  } else if(is.null(crosswalk_version_id) == FALSE){
    dt <- get_crosswalk_version(crosswalk_version_id = crosswalk_version_id)
    id_name <- crosswalk_version_id
    as.data.table(dt)
  }
  if (is.null(criteria) == FALSE){
    dt <- distinct_at(dt, criteria, .keep_all = FALSE)
    write.csv(dt, paste0('unique_bundle_report_',id_name, '.csv'), row.names = FALSE)
    note <- cat('You report has been save to your assigned directoy!')
    message(note)
    return(dt)
  } else {
    stop("Please make sure entered the existing criteria your are vetting and/or inputting the correct arguments to pull your bundle data.")
    }
}
# --------------------------------------
#' Quick Summary
#'
#' @description This utility bundle function provides you with a quick summary snapshot of measure_id's by location and nid OR search criteria. The search criteria functionality allows you to extract any values/characters that exist in the data. Please verify that the criteria you are looking for does exist in the data, or else you will get a blank summary report. You can use a bundle_id, bundle_version_id, or crosswalk_version_id to adequately pull a quick_summary report of your choice. Please make sure your id's are correct.
#'
#' @param bundle_id bundle_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param bundle_version_id bundle_version_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param crosswalk_version_id crosswalk_version_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param gbd_round_id gbd_round_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param decomp_step decomp_step, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param merge_id_columns merge_id_columns, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param measure_report For quick measure_id summary by location and nid. - TRUE or FALSE
#' @param search For quick search summary. -- TRUE or FALSE
#' @param criteria Your seearch criteria. Make sure to spell this corrrectly, and that you are certain it exists in your bundle_id.
#' @param pie_chart
#'
#' @return Single output with either your quick measure_id summary or search string criteria summary.
#' @export
#' @import data.table
#' @import plyr
#' @import tidyverse
#' @import dplyr
#'
#' @examples quick_summary(bundle_id = 1496, measure_report = TRUE) #This provides you a quick summary of measure_id types by location and nid. ALl data. :
#'           quick_summary(bundle_id = 1496, search = TRUE, criteria = 'Demographic and Health Survey') #This provides you with only with the search criteria you want.
quick_summary <- function(bundle_id = NULL, bundle_version_id = NULL, crosswalk_version_id = NULL, gbd_round_id = NULL, decomp_step = NULL, merge_id_columns = FALSE, measure_report = FALSE, search = FALSE, criteria = NULL, pie_chart = FALSE) {
  invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
  if(is.null(bundle_id) == FALSE){
    dt <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
    id_name <- bundle_id
    as.data.table(dt)
  } else if(is.null(bundle_version_id) == FALSE){
    dt <- get_bundle_version(bundle_version_id = bundle_version_id)
    id_name <- bundle_version_id
    as.data.table(dt)
  } else if(is.null(crosswalk_version_id) == FALSE){
    dt <- get_crosswalk_version(crosswalk_version_id = crosswalk_version_id)
    id_name <- crosswalk_version_id
    as.data.table(dt)
  }
  if (measure_report == TRUE){
    dt <- dt %>% select('location_id', 'location_name','nid', 'measure')
    dt <- dt %>% dcast(location_id + location_name + nid ~ measure, value.var = 'measure', length)
    output_qs(dt, id_name)
  } else if (search == TRUE){
    dt <- dt %>% filter_all(any_vars(str_detect(., pattern = criteria)))
    output_qs(dt, id_name)
  } else {
    return(dt)
  }
}

output_qs <- function(dt, id_name){
  as.data.table(dt)
  write.csv(dt ,paste0('quick_summary_', id_name, '.csv'), row.names = FALSE)
  note <- cat('You report has been save to your assigned directoy!')
  message(note)
  return(dt)
}
# --------------------------------------
#' Sex and Age split Identifier
#'
#' @description This utility bundle function allows you to identify what rows or groups will need to be age-sex, age, and sex split. You can use a bundle_id, or bundle_version_id to adequately pull a splits_report. Please make sure your id's are correct.
#'
#' @param bundle_id bundle_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param bundle_version_id bundle_version_id , please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param gbd_round_id gbd_round_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param decomp_step decomp_step, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param merge_id_columns merge_id_columns, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#'
#' @return Output with three separate reports: Age-sex, sex, and age splits. For the desired bundle_id or bundle_version_id.
#' @export
#' @import data.table
#' @import plyr
#' @import tidyverse
#' @import dplyr
#'
#' @examples splits_report(bundle_id = 1496)
splits_report <- function(bundle_id = NULL, bundle_version_id = NULL, gbd_round_id = NULL, decomp_step = NULL, merge_id_columns = FALSE){
  invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
  if(is.null(bundle_id) == FALSE){
    dt <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
    id_name <- bundle_id
    as.data.table(dt)
    dt <- dt %>% mutate(age_diff = abs(age_end - age_start))
  } else if(is.null(bundle_version_id) == FALSE){
    dt <- get_bundle_version(bundle_version_id = bundle_version_id)
    id_name <- bundle_version_id
    as.data.table(dt)
    dt <- dt %>% mutate(age_diff = abs(age_end - age_start))
  }
  if(any(dt$sex == 'Both') & any(dt$age_diff >= 25) | any(dt$age_diff < 25)){
    #age-sex split
    dt_age_sex <- dt %>% filter(grepl(pattern = 'Both', sex) & age_diff >= 25)
    dt_age_sex <- dt_age_sex %>% select(nid, source_type, location_id, location_name, sex, age_diff, measure)
    output_sr(dt_age_sex, id_name = id_name, type = 'age_sex')
  } else {
    stop("Please make sure you entered the correct arguments to pull your bundle data, and/or that you requested a full splits report.")
  }
  if(any(dt$sex == 'Both' & dt$age_diff < 25)){
    #sex split
    dt_sex <- dt %>% filter(grepl(pattern = 'Both', sex) & age_diff < 25)
    dt_sex <- dt_sex %>% select(nid, source_type, location_id, location_name, sex, age_diff, measure)
    output_sr(dt_sex, id_name = id_name, type = 'sex')
  } else {
    stop("Please make sure you entered the correct arguments to pull your bundle data, and/or that you requested a full splits report.")
  }
  if(any(dt$age_diff >= 25 & dt$sex == 'Male' | dt$sex == 'Female')){
    #age split
    dt_age <- dt %>% filter(age_diff >= 25 & !grepl(pattern = 'Both', sex))
    dt_age <- dt_age %>% select(nid, source_type, location_id, location_name, sex, age_diff, measure)
    output_sr(dt_age, id_name = id_name, type = 'age')
  } else {
    stop("Please make sure you entered the correct arguments to pull your bundle data, and/or that you requested a full splits report.")
  }
  splits_present <- (any(dt$sex == 'Both' | dt$age_diff >= 25))
  if(splits_present == FALSE){
    print('Your bundle does not contain any age_sex, age, or sex plits for further processing.')
  }
}

output_sr <- function(dt, id_name,type){
  if(type == 'age_sex'){
    as.data.table(dt)
    write.csv(dt ,paste0(type, '_splits_report_','_', id_name, '.csv'), row.names = FALSE)
    note <- cat(paste0('Your ', type, ' splits report has been save to your assigned directoy\n**Please know your directory ahead of time.'))
    message(note)
    return(dt)
  } else if (type == 'sex') {
    as.data.table(dt)
    write.csv(dt ,paste0(type, '_splits_report_','_', id_name, '.csv'), row.names = FALSE)
    note <- cat(paste0('Your ', type, ' splits report has been save to your assigned directoy\n**Please know your directory ahead of time.'))
    message(note)
    return(dt)
  } else if (type == 'age') {
    as.data.table(dt)
    write.csv(dt ,paste0(type, '_splits_report_','_', id_name, '.csv'), row.names = FALSE)
    note <- cat(paste0('Your ', type, ' splits report has been save to your assigned directoy\n**Please know your directory ahead of time.'))
    message(note)
    return(dt)
  }
}
# --------------------------------------
#' Comparison Report
#'
#' @description This bundle utility function will generate a comparison report between a bundle or bundle_version and a crosswalk_version or bundle_version. Collapsed to nid, location_id, and location_name. No actual raw data is included in the final outputs. This will allow you properly vet what was excluded from the native bundle to bundle_version/crosswalk_versions bundles.
#'
#' @param bundle_id bundle_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param bundle_version_id bundle_version_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param crosswalk_version_id crosswalk_version_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param gbd_round_id gbd_round_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param decomp_step decomp_step, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#' @param merge_id_columns merge_id_columns_id, please refer https://scicomp-docs.ihme.washington.edu/elmo/current/functions/functions.html for more details.
#'
#' @return Outputs with different comparison reports. Output files contain all the rows of bundle_id/bundle_version_id in bundle_version_id/crosswalk_version_id. By nid, location_id, and location_name.
#' @export
#' @import data.table
#' @import plyr
#' @import tidyverse
#' @import dplyr
#'
#' @examples comparison_report(bundle_version_id = 34385, crosswalk_version_id = 31013)
#'           comparison_report(bundle_id = 1496, crosswalk_version_id = 31013)
#'           comparison_report(bundle_id = 1496, bundle_version_id = 34385)
comparison_report <- function(bundle_id = NULL, bundle_version_id = NULL, crosswalk_version_id = NULL, gbd_round_id = NULL, decomp_step = NULL, merge_id_columns = FALSE){
  invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
  if (is.null(bundle_id) == FALSE & is.null(bundle_version_id) == FALSE & is.null(crosswalk_version_id) == FALSE){
    dt_bundle <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
    bundle <- comp_dt(dt_bundle)
    dt_version <- get_bundle_version(bundle_version_id = bundle_version_id)
    version <- comp_dt(dt_version)
    dt_crosswalk <- get_crosswalk_version(crosswalk_version_id = crosswalk_version_id)
    crosswalk <- comp_dt(dt_crosswalk)

    #Anti-join
    bundle_v <- anti_join(bundle, version, by = c('nid', 'location_id', 'location_name'))
    comp_bun_output(bundle_v, bundle_id = bundle_id, bundle_version_id = bundle_version_id)
    bundle_c <- anti_join(bundle, crosswalk, by = c('nid', 'location_id', 'location_name'))
    comp_bun_output(bundle_c, bundle_id = bundle_id, crosswalk_version_id = crosswalk_version_id)
  } else if(is.null(bundle_id) == FALSE & is.null(crosswalk_version_id) == FALSE){
    dt_bundle <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
    bundle <- comp_dt(dt_bundle)
    dt_crosswalk <- get_crosswalk_version(crosswalk_version_id = crosswalk_version_id)
    crosswalk <- comp_dt(dt_crosswalk)

    #Anti-join
    bundle_c <- anti_join(bundle, crosswalk, by = c('nid', 'location_id', 'location_name'))
    comp_bun_output(bundle_c, bundle_id = bundle_id, crosswalk_version_id = crosswalk_version_id)
  } else if(is.null(bundle_id) == FALSE & is.null(bundle_version_id) == FALSE){
    dt_bundle <- get_bundle_data(bundle_id = bundle_id, gbd_round_id = gbd_round_id, decomp_step = decomp_step, merge_id_columns = merge_id_columns)
    bundle <- comp_dt(dt_bundle)
    dt_version <- get_bundle_version(bundle_version_id = bundle_version_id)
    version <- comp_dt(dt_version)

    #Anti-join
    bundle_v <- anti_join(bundle, version, by = c('nid', 'location_id', 'location_name'))
    comp_bun_output(bundle_v, bundle_id = bundle_id, bundle_version_id = bundle_version_id)
  } else if(is.null(bundle_version_id) == FALSE & is.null(crosswalk_version_id) == FALSE) {
    dt_version <- get_bundle_version(bundle_version_id = bundle_version_id)
    version <- comp_dt(dt_version)
    dt_crosswalk <- get_crosswalk_version(crosswalk_version_id = crosswalk_version_id)
    crosswalk <- comp_dt(dt_crosswalk)

    #Anti-join
    ver_cross <- anti_join(version, crosswalk, by = c('nid', 'location_id', 'location_name'))
    comp_bun_output(ver_cross, bundle_version_id = bundle_version_id, crosswalk_version_id = crosswalk_version_id)
  } else {
    stop('Please enter a bundle version or crosswalk version id to make the comparison possible!')
  }
}
# The above function will find all the rows of bundle not present in version/crosswalk based on the specified criteria.

comp_dt <- function(dt) {
  as.data.table(dt)
  dt <- dt %>% select(1:9)
  dt <- distinct_at(dt, .vars = c('nid', 'location_id', 'location_name'), .keep_all = FALSE)
  return(dt)
}

comp_bun_output <- function(dt, bundle_id = NULL, bundle_version_id = NULL, crosswalk_version_id = NULL){
  if (is.null(bundle_id) == FALSE & is.null(bundle_version_id) == FALSE){
    as.data.table(dt)
    write.csv(dt ,paste0('comparison(bundle&bversion)_report_','_', bundle_id, '_', bundle_version_id,'.csv'), row.names = FALSE)
  } else if(is.null(bundle_id) == FALSE & is.null(crosswalk_version_id) == FALSE) {
    as.data.table(dt)
    write.csv(dt ,paste0('comparison(bundle&crosswalk)_report_','_', bundle_id, '_', crosswalk_version_id,'.csv'), row.names = FALSE)
  } else if(is.null(bundle_version_id) == FALSE & is.null(crosswalk_version_id) == FALSE){
    as.data.table(dt)
    write.csv(dt ,paste0('comparison(bversion&crosswalk)_report_','_', bundle_version_id, '_', crosswalk_version_id,'.csv'), row.names = FALSE)
  }
}
