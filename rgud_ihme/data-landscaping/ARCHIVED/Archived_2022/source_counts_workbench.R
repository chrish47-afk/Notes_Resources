## Title: Source Counts, 2022
## Authors: Mae Dirac, Helen Ippolito
## Edited and Updated by: Christian Hernandez
## Date Created: Nov 24, 2020
## Date Edited: March 30, 2022

rm(list=ls())

############################################################
### SET UP
############################################################

user <- Sys.getenv("USER")

## Filepaths
lib_path <- "/ihme/cc_resources/libraries/current/r/"
#map_path <- "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R"
out_path <- "/ihme/scratch/users/chrish47/sources_count_testing/" #@ Changing the output path for testing purposes!
#repo_path <- paste0(h, "repos/upper-digestive/")
counts_path <- "/ihme/scratch/users/chrish47/sources_count_testing/source_counts/" #@ 

pacman::p_load(data.table, ggplot2, readr, RMySQL, openxlsx, readxl, stringr, tidyr, plyr, dplyr, dbplyr, gridExtra, labeling, forcats)

## Source central functions
# functions <- c("get_age_metadata", "get_outputs", "get_location_metadata", "get_ids", "get_covariate_estimates", "get_envelope", "get_pct_change", "get_draws", "get_population", "get_elmo_ids", "get_bundle_version", "get_crosswalk_version", "get_cod_data")
# invisible(lapply(functions, function(x) source(paste0(lib_path, x, ".R"))))
# source(paste0(j, map_path))

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))


############################################################
## GLOBAL OBJECTS - For Source Counts!
############################################################
# ## GBD 2019 round and version args
gbd_rnd <- 6
gbd_decomp_step <- "step5"
compare_version <- 7244

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
############################################################
## SOURCE COUNTS
############################################################

## Note: source counts generated at the "country" level here represent counts of
## all sources from a given country at any location level, NOT just
## nationally-representative sources (ie. if a crosswalk version's only Chinese
## sources were 1 NID from Beijing and 1 from China, with one year_start each,
## this would be counted as two "country-level" sources for China, which is
## distinct from counting 1 source at the national level.)
      
## NONFATAL SOURCE COUNTING STRATEGY, PART 1      
## Nonfatal source counts function - MUST BE RUN ALONG WITH merged_nid_count_adjustment() TO ACCOUNT FOR MERGED NIDS!!! See note below. 
source_counts <- function(cw_id, source_type){
  #@ source counts folder, necessary, to avoid doing it manually!
  if(file.exists(paste0(out_path, 'source_counts'))){
    counts_path <- paste0(out_path, 'source_counts')
  } else {
    dir.create(paste0(out_path, 'source_counts'))
    counts_path <- paste0(out_path, 'source_counts')
  }
  
  #@ vetting folder, where vettting report on source counts vs. central comp results live -- THIS IS OPTIONAAL! Delete if necessary!
  if(file.exists(paste0(out_path, 'vetting'))){
    vetting_path <- paste0(out_path, 'vetting')
  } else {
    dir.create(paste0(out_path, 'vetting'))
    vetting_path <- paste0(out_path, 'vetting')
  }
  
  # Get crosswalk version if it hasn't already been written to flat file
  if (file.exists(paste0(counts_path, "cw_version_", cw_id, ".xlsx"))) {
    cw_version <- read.xlsx(paste0(counts_path, "cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA 
    #@ Issue arising from having empty cells -- > Converting all empty cells to NA. #verify if this needs to be string NA value
  } else {
    cw_version <- get_crosswalk_version(cw_id)
    write.xlsx(cw_version, paste0(counts_path, "cw_version_", cw_id, ".xlsx"))
    cw_version[cw_version == ""] <- NA 
    #@ Issue arising from having empty cells -- > Converting all empty cells to NA. #verify if this needs to be string NA value
  }
  
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  
  # Format crosswalk version data based on source type; attribute all data to its corresponding country. 
  #@ The following code simply collapses the elmo table to its desired source type(input) by location criteria
  #@ Important points to address: (1) Every bundle is different, so the below code will only work if it contains the required column criteria. Important to consider. Additionally, important to discuss the measure type included in the cw_locs. As this will differ for every elmo_id and/or cause. (2) The following dat wrangling, creates an important column(country_id), which is constructed by location_id and path_to_top_parent!
  if (source_type == "claims") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "claims") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x) #@ added dplyr:: to every rename call
  } else if (source_type == "inpatient") {
    cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "prev") {
        cw_locs <- cw_version %>% 
          filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
          left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
          mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                              "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
          filter(!country_id=="OTHER") %>% # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
          mutate(country_id = as.integer(country_id)) %>%
          left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
          dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
        #@ Review on how exactly the regex line of code works! That creates the country_id column from the location_id and path_to_top_parent
    } else {
      message("Invalid source_type argument.")
      } 
  # Check to see if there are any sources to count for the given source type
  if (nrow(cw_locs) == 0) {
    message(paste0("cw_id ", cw_id, ", ", source_type, ": no sources of this data type."))
  } else {
  
    # Unique NID-location-year_start combinations [most granular locs]
    count_loc <- cw_locs %>% dplyr::distinct(nid, year_start, location_name) %>%
      group_by(location_name) %>% 
      dplyr::tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # Only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    count_loc_merged <- cw_locs %>% 
      dplyr::distinct(nid, year_start, location_name) %>%
      dplyr::pull(nid) %>%
      unique() %>%
      paste0(collapse=",")
    message(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ", count_loc_merged))
    
    # Unique NID-country-year_start combinations [country-level location counts]
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      group_by(country_name) %>% 
      tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = c('country_name' = 'location_name'))
    
    # Create new xlsx workbook and add source counts (separate sheets for country and most granular counts)
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, ".xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_name")
    writeData(wb, "counts_by_loc_name", count_loc)
    
    addWorksheet(wb, "counts_by_country")
    writeData(wb, "counts_by_country", count_country)
    
    saveWorkbook(wb, paste0(counts_path, "/", out_filename), overwrite = TRUE)
    
    # Compare to Central Comp counts [mainly used as diagnostic before writing merged_nid_count_adjustment() function
    # THE BELOW CODE: Not relevant if CC doesn't provide counts.] README
    cc_counts <- read_csv(paste0("/ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/source_counts/counts_from_CC/GBD 2019 custom PUD prevalence subnational-level counts for ", ifelse(source_type=="prev", "other", source_type), " NIDs.csv"))
    cc_counts %>% 
      left_join(count_loc, by = "location_name") %>% 
      dplyr::rename(cc_count = source_count, rgud_count = n) %>% 
      mutate(cc_minus_rgud = cc_count - rgud_count, factor = cc_count/rgud_count) %>% # difference and proportion between Central Comp and RGUD counts
      
      write.csv(., paste0(vetting_path,"/", source_type, "_cc_rgud_comparison.csv")) #@Important, either change this so that the required folder is in there or just create a folder manually. 
  }
}

# Run source counts function for PUD and GD      
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c('prev', 'inpatient', 'claims')){ #@ cw_id = 9890 doesn't have any prev nids, based on the above criteria.Review. 
    source_counts(id, type)
  }
}

## NONFATAL SOURCE COUNTING STRATEGY, PART 2      
## NOTE: the above source counting strategy does NOT account for merged NIDs - each component NID needs to be counted individually. 
# Strategy to add counts for component NIDs of merged NIDs:
# 1. Enter 'count_loc_merged' NID list (from output message of source_counts() function) into GHDx tool: http://internal-ghdx.healthdata.org/search-by-nids-all-types/
# 2. Download list of NIDs and source types (see /ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/source_counts/merged_NIDs_adjustment)
# 3. Download list of all merged NIDs and their component NIDs from GHDx: http://internal-ghdx.healthdata.org/merged-citations-components (also at /merged_nids_adjustment path from above)
# 4. Run merged_nid_count_adjustment(). 

#@ @@@@@@@@@@@@@@@@@@@@@@@@@@
complete_nid_list <- c(411100,407536,408336,411786,404395,354896,411787,234670,234672,234674,234671,284419,234673,234766,234771,234774,234769,234772,3822,234758,234761,234760,287201,287203,287202,234740,234742,287204,234741,234738,331084,68367,68535,133665,234745,234747,234746,234692,234703,234704,234693,104246,284439,284442,284444,284440,234764,234765,292577,234762,67132,287207,292575,292437,284422,284421,234750,336851,336852,334465,334464,334466,292574,321359,337619,285520,317423,337129,281819,96714,397812,397813,397814,409155,409156,409157,409158,409159,336203,336847,408680,244369,244370,336850,244371,336849,336848,11100,407536,408336,411786,404395,354896,411787,234670,234672,234674,234671,284419,234673,234766,234771,234774,234769,234772,3822,234758,234761,234760,287201,287203,287202,234740,234742,234738,287204,234741,331084,68367,68535,133665,234745,234747,234746,234692,234703,104246,234704,234693,284439,284442,284444,284440,234764,234765,292577,234762,67132,287207,292575,292437,284422,284421,234750,336851,336852,334465,334466,334464,292574,321359,317423,337619,285520,337129,281819,96714,397812,397813,397814,336203,244369,244370,336850,244371,336849,336848,336847,408680)
complete_nid_list <- unique(as.character(complete_nid_list)) 
# The merged 
cat(complete_nid_list, sep = ",")
#@ @@@@@@@@@@@@@@@@@@@@@@@@@
 
merged_nid_count_adjustment <- function(cw_id, source_type){
  counts_path <- paste0(out_path, "source_counts/")
  
  # Get crosswalk version if it hasn't already been written to flat file
  if (file.exists(paste0(counts_path, "cw_version_", cw_id, ".xlsx"))) {
    cw_version <- read.xlsx(paste0(counts_path, "cw_version_", cw_id, ".xlsx"))
  } else {
    cw_version <- get_crosswalk_version(cw_id)
    write.xlsx(cw_version, paste0(counts_path, "cw_version_", cw_id, ".xlsx"))
  }
  if (!"ihme_loc_id" %in% names(cw_version)) {
    cw_version <- cw_version %>% mutate(ihme_loc_id = NA)
  }
  
  # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
  merged_comp_list <- read.csv(paste0(counts_path, "merged_nids_adjustment/merged_citation_component_records.csv")) %>% #@ Change the path accordin.
    group_by(Merged.Citation.NID) %>%
    tally() %>%
    dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
  
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  if(!file.exists(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"))){
    message(paste0("No ", source_type, " NIDs for cw_id ", cw_id))
  } else {
  
    unadj_counts_subnat <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 1) 
    unadj_counts_country <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 2) 
    
    # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
    source_type_nids <- read.csv(paste0(counts_path, "merged_nids_adjustment/", ifelse(cw_id==12221, "pud", "gd"), "_", source_type, "_NID_merge_types.csv")) #@ This will need to be changed, as file names will differ
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>% 
      filter(!is.na(n_comp_nids))
    print(1) 
    # Format crosswalk version data to prep for source counting 
    if (source_type == "claims") {
      cw_locs <- cw_version %>% #mutate(across(everything(), as.character)) %>% #@ adding the as.character line
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "claims") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
        ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(2)
    } else if (source_type == "inpatient") {
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(3)
    } else if (source_type == "prev") {
      cw_locs <- cw_version %>% 
        filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
        left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      print(4)
    } else {message("Invalid source_type argument.")} 
    
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs
    print(5)
    cw_locs <- transform(cw_locs, nid = as.numeric(nid)) #@ These are important to avoid incompability issues!
    key <- transform(key, NID = as.numeric(NID)) #@ These are important to avoid incompability issues!
    count_loc <- cw_locs %>% distinct(nid, year_start, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>% #@ This is the line with the incompatible types problems!!!!! 
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    print(6)
    # Unique NID-country-year_start combinations [country-level counts] accounting for component NIDs
    count_country <- cw_locs %>% distinct(nid, year_start, country_name) %>% 
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(country_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(country_name) %>%
      distinct(country_name, n_adj) %>% 
      left_join(location_data %>% select(location_name, location_id), by = c('country_name'='location_name'))
    
    # Create xlsx file with sheets for most granular and country-level source counts. 
    out_filename <- paste0(source_type, "_counts_cw_version_", cw_id, "_merged_adj.xlsx")
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "counts_by_loc_adj")
    writeData(wb, "counts_by_loc_adj", count_loc)
    
    addWorksheet(wb, "counts_by_country_adj")
    writeData(wb, "counts_by_country_adj", count_country)
    
    saveWorkbook(wb, paste0(counts_path, out_filename), overwrite = TRUE)
  }  
}

# Run source counts adjustment for PUD and GD
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c("prev", "inpatient", "claims")){
    merged_nid_count_adjustment(id, type)
  }
}

merged_nid_count_adjustment(cw_id = 12221, source_type = 'claims')
##----------------------------------------------------------
## SUPPLEMENTARY FIG: Nonfatal source counts maps (national and most granular levels)

## Note that only country-level count maps were used for publication. Maps with
## subnational do not represent country-level counts for locations with national
## and subnational sources.

for(id in c(12221, 9890)){
  for (level in c("Location", "Country")) { # generates maps for counts at most granular level and country level
    source_types <- c("inpatient", "claims")
    if (id==12221) {
      source_types <- source_types %>% append("prev") # only PUD has prevalence sources
    }
    plot_data <- data.frame()
    
    # Read in ALL source counts for a given cause; collapse counts across source types. 
    for (file in paste0(source_types, "_counts_cw_version_", id, "_merged_adj.xlsx")) {
      plot_data <- rbind(plot_data, read.xlsx(paste0(counts_path, file), sheet = if_else(level == "Location", 1, 2)) %>% select(location_id, n_adj))
    }
    plot_data <- plot_data %>%
      group_by(location_id) %>%
      mutate(mapvar = sum(n_adj)) %>% # mapped variable is sum of source counts across all source types
      distinct(location_id, mapvar)
    write.csv(plot_data, paste0(counts_path, if_else(id==12221, "pud", "gd"), "_", level, "_source_counts.csv")) # flat file of collapsed counts
    
    pdf(paste0(out_path, "SUPP_", if_else(id==12221, "pud", "gd"), "_nf_source_counts_map_", level, ".pdf"), height = 4.15, width = 7.5, pointsize = 6.5)
    bin_limits <- c(1, seq(from = 5, to = if_else(id==12221, 30, 25), by = 5), max(plot_data$mapvar))
    bin_labels <- c()
    for (i in 1:(length(bin_limits)-1)) {
      bin_labels <- append(bin_labels, paste0(bin_limits[i], "-", bin_limits[i+1]-1))
    }
    bin_labels <- str_replace(bin_labels, paste0("-", max(plot_data$mapvar)-1), paste0("-", max(plot_data$mapvar)))
    gbd_map(plot_data,
            limits = bin_limits,
            labels = bin_labels, 
            sub_nat = if_else(level == "Location", "capstone", "none"),
            # limits = c(0, quantile(plot_data$mapvar, c(0.2, 0.4, 0.6, 0.8)), max(plot_data$mapvar)) %>%
            #              sprintf("%.1f", .) %>% as.numeric(),
            legend.title = paste0("Sources (", level, "-Year)"))
    dev.off()  
  }
  
}
##----------------------------------------------------------


##----------------------------------------------------------
## SUPPLEMENTARY FIG 2: CoD Source Counts maps based on Central Comp counts
for (cause in c("pud", "gd", "ugi")) {
  pdf(paste0(out_path, "SUPP2_", cause, "_cod_source_counts_map.pdf"), height = 4.15, width = 7.5, pointsize = 6.5)
  
  plot_data <- read_csv(paste0(counts_path, "counts_from_CC/GBD2019 ", cause, " COD counts by location.csv")) %>%
    select(location_id, mapvar = source_count)
  
  bin_limits <- c(1, seq(from = 10, to = 50, by = 10), 100, max(plot_data$mapvar))
  bin_labels <- c()
  for (i in 1:(length(bin_limits)-1)) {
    bin_labels <- append(bin_labels, paste0(bin_limits[i], "-", bin_limits[i+1]-1))
  }
  bin_labels <- str_replace(bin_labels, paste0("-", max(plot_data$mapvar)-1), paste0("-", max(plot_data$mapvar)))
  
  gbd_map(plot_data,
          limits = bin_limits,
          labels = bin_labels, 
          sub_nat = "none",
          legend.title = "Sources (Country-Year)")
  dev.off()  
}
##----------------------------------------------------------

## In-text statement: "Data are most abundant for our mortality estimates, with
## more than 3700 sources, but, nonetheless, they were absent in most of the
## countries in sub-Saharan Africa (33/46) and Oceania (11/18), and 17
## additional countries scattered across the remainder of the globe."

ugi_cod_locs <- location_data %>% 
  select(location_name, location_id, region_name, super_region_name, level) %>% 
  left_join(read_csv(paste0(counts_path, "counts_from_CC/GBD2019 ugi COD counts by location.csv"))) %>% filter(level == 3) #country-level
ugi_cod_locs %>% filter(super_region_name=="Sub-Saharan Africa" & is.na(source_count)) %>% nrow() # 33/46
ugi_cod_locs %>% filter(region_name=="Oceania" & is.na(source_count)) %>% nrow() # 11/18
ugi_cod_locs %>% filter(region_name!="Oceania" & super_region_name!="Sub-Saharan Africa" & is.na(source_count)) %>% nrow() # 17 other countries without CoD sources





#------------------------------------------
## Documentation of PRELIMINARY methods for source-counting. THESE ARE NOT FINAL/CORRECT AND WERE NOT USED FOR PUBLICATION. 
## 21. Altogether, studies from the GHDx and peer-reviewed literature provided XXX location-years of data for a total of XXX locations
get_elmo_ids(gbd_rnd, "step4", bundle_id=6998)
pud_bv <- get_bundle_version(16010) # total PUD with modeled EMR
pud_bv_locs <- pud_bv %>% 
  filter(clinical_data_type == "" & measure == "prevalence") %>% # only counting non-clinical sources with extracted prev
  left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
  mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                TRUE ~ "OTHER")) %>%
  filter(!country_id=="OTHER") %>% # one EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only "IHME" source in this bundle).
  mutate(country_id = as.integer(country_id)) %>%
  left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
  select(names(.), country_name = location_name.y)

pud_bv_locs$country_name %>% unique() %>% length()
pud_bv_locs %>% distinct(nid, year_start, country_name) %>% nrow()
# Bundle 6998, total PUD with EMR, has data for 13 countries (level 3) and 59 unique NID-year_start-country combinations. 

# *** NOTE: source counts change if you run the preceding code based on crosswalk version 12221 rather than the bested bundle version.
# *** Amended statement based on CW version: crosswalk version 12221 has data for 8 countries (level 3) and 40 unique NID-year_start-country combinations.


## 22. A total of 8 years of claims data were available for 50 USA states and
## District of Columbia, 5 years for 83 [subjects, oblasts?] of Russia, 3 years
## for 16 Polish voivodeships, and 1 year was available for Taiwan.
pud_claims <- pud_bv %>% 
  left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
  filter(clinical_data_type== "claims") %>% 
  mutate(country = substr(ihme_loc_id.y, 1, 3)) ## %>% pull(country) %>% unique() %>% sort() # claims data available from Poland, Russia, Taiwan, and USA

# Number of subnational locs with claims data per country
pud_claims %>% distinct(country, location_name) %>%
  group_by(country) %>%
  tally()

# Number of years of claims data per country
pud_claims %>% distinct(year_start, country) %>%
  group_by(country) %>% tally()


## 23. Inpatient discharges with ICDs for PUD as primary diagnosis were identified from 104 country-years of inpatient discharge data from 43 countries.
pud_clin <- pud_bv %>% 
  left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
  filter(clinical_data_type== "inpatient") %>% 
  mutate(country = substr(ihme_loc_id.y, 1, 3))
pud_clin %>% distinct(country, year_start)
pud_clin$country %>% unique() %>% length() 


## COD source counts [NOTE: this method was NOT used to generate/verify CoD source counts for manuscript. Used Central Comp counts from above section instead.]
pud_cod <- get_cod_data(527, gbd_round_id = 6, decomp_step = "step4")
pud_cod %>% distinct(nid, location_id, location_name, year) # returns 20364 counts at most granular loc level. Two-pager reports 2641.
pud_cod$nid %>% unique() %>% length() # returns 703 unique NIDs
pud_cod %>% left_join(location_data %>% select(location_id, path_to_top_parent, level), by = "location_id") %>%
                        mutate(country_id = as.integer(case_when(level == 3 ~ as.character(location_id), 
                                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2]))) %>%
  left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
  rename(., country_name = location_name.y, location_name = location_name.x) %>%
  distinct(nid, country_name, year) # returns 2546 counts at country level. Closer to two-pager (n = 2641).
