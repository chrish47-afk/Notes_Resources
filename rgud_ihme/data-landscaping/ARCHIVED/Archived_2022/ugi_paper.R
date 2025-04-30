## Title: UGI Paper, GBD 2019
## Authors: Mae Dirac, Helen Ippolito
## Date: Nov 24, 2020

## Description: This script creates the tables, figures, in-text analyses, and
## source counts for the GBD 2019 peptic ulcer disease (PUD) and
## gastritis/duodenitis (G/D) topic paper. Code is divided by topic areas
## (mostly measures); code for individual figures is demarcated with commented
## dashed lines within the respective topic areas. Inherited code from previous
## iterations of this paper has been indented 3x to distinguish it from code
## actively being used in figures/analyses. Numeric labels of in-text statements
## correspond to the row numbers in this tracking document:
## https://docs.google.com/spreadsheets/d/1ELdMPEnl2YNEv7HqYUsouONcyIVz9egXBd5m2OvwXig/edit?usp=sharing
## Some figure numbers changed and are labeled "FIGURE X [renumbered to FIGURE
## Y]: description," where X corresponds to the original figure number in the tracker
## and Y is the final figure number in the submission to collaborators.


rm(list=ls())

############################################################
### SET UP
############################################################

user <- Sys.getenv("USER")
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <-paste0("/homes/", user, "/")
  l <- "/ihme/limited_use/"
} else {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
}

## Filepaths
lib_path <- "/ihme/cc_resources/libraries/current/r/"
map_path <- "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/inset_maps/2019GBD_MAP.R"
out_path <- "/ihme/scratch/users/chrish47/sources_count_testing/" #@ Changing the output path for testing purposes!
repo_path <- paste0(h, "repos/upper-digestive/")

## Load packages
pacman::p_load(data.table, ggplot2, readr, RMySQL, openxlsx, readxl, stringr, tidyr, plyr, dplyr, dbplyr, gridExtra, labeling, forcats)
#gpclibPermit() #the map functions uses a package has a restricted license that is disabled by default and needs this line here enables it

## Source central functions
functions <- c("get_age_metadata", "get_outputs", "get_location_metadata", "get_ids", "get_covariate_estimates", "get_envelope", "get_pct_change", "get_draws", "get_population", "get_elmo_ids", "get_bundle_version", "get_crosswalk_version", "get_cod_data")
invisible(lapply(functions, function(x) source(paste0(lib_path, x, ".R"))))
source(paste0(j, map_path))



############################################################
## GLOBAL OBJECTS
############################################################

## Cause IDs, names
fatal_causes <- c("992", "527", "528")
nonfatal_causes <- c("536", "527", "528")
all_causes <- c("992", "527", "528", "536")
pud_gd <- data.frame(cause_id = c("527", "528"), 
                     cause_name_full = c("peptic ulcer disease (PUD)", "gastritis and duodenitis (G/D)"))

## GBD 2019 round and version args
gbd_rnd <- 6
gbd_decomp_step <- "step5"
compare_version <- 7244

## Compare_version_id 7244 parameters - get_pct_change() doesn't accept compare_version_id so params must be specified for each source.
cod_version <- 135
daly_version <- 47
como_version <- 470

year_ids <-c(1990:2019)
all_age_grps <- c(5:20, 30:32, 235)
age_group_set_id <- 23

location_data <- get_location_metadata(location_set_id=35, gbd_round_id=gbd_rnd)
ntl_locations <- location_data[level==3, ]
ntl_loc_id <- ntl_locations$location_id
regions <- location_data[level==2, ]
reg_loc_id <- regions$location_id
super_regions <- location_data[level==1, ]
sr_loc_id <- super_regions$location_id

pop <- get_population(gbd_round_id = gbd_rnd, decomp_step = gbd_decomp_step, status = "best", sex_id = c(1,2), year_id = c(year_ids[1], year_ids[length(year_ids)]))

      order_causes <- c(
            'Gastritis and duodenitis',
            'Peptic ulcer disease',
            'Gastroesophageal reflux disease')

## Colors for plots      
cause_colors <- c(
      'Gastroesophageal reflux disease' = rgb(203, 226, 160, maxColorValue=255),
      'Gastritis and duodenitis' = rgb(103, 181, 107, maxColorValue=255),
      'Peptic ulcer disease' = "#4C0089")



############################################################
## Age-standardized death rate (ASDR)
############################################################

# Pull age-standardized death rate, all locs/years/sexes
asdr <-get_outputs(
  "cause",
  cause_id = fatal_causes,
  sex_id = c(1, 2, 3),
  year_id = year_ids,
  metric_id = 3, #rate
  measure_id = 1, #deaths
  age_group_id = 27,
  location_id = "all",
  gbd_round_id = gbd_rnd,
  decomp_step = gbd_decomp_step,
  compare_version_id = compare_version
)
asdr <- asdr[ , asdr:=val*100000]


      #Report relative fatal burden PUD to gastritis
      asdr_p_g <- asdr[cause_id %in% c(527, 528) & sex_id==3, ]
      
      asdr_p_g <- dcast(asdr_p_g, year_id + location_id ~ cause_id, value.var = c("asdr", "upper", "lower"))
      asdr_p_g[, asdr_ratio:=asdr_527/asdr_528] # PUD / GD
      print(asdr_p_g[location_id %in% c(1, sr_loc_id), ]) # global, superregions
      print(asdr_p_g[asdr_ratio<1, ]) # GD death rate exceeds PUD
      print(asdr_p_g[location_id==1, ]) %>% # global only
        pull(asdr_ratio) %>% range() # returns min/max of PUD/GD ASDR ratio across all years
      #No uncertainty; will have to pull draws to get with uncertainty
      
      
      # Histogram of PUD/GD ASDR ratio (all years, locs)  
      asdr_p_g_hist <- ggplot(asdr_p_g, aes(asdr_ratio)) + geom_histogram(binwidth=1)
      ggsave(paste0("asdr_p_g_ratio_hist.pdf"), device = pdf, path = out_path, width=12, height=10)


##----------------------------------------------------------
## FIGURE 3A,B [renumbered to FIGURE 2]: 2019 ASDR for PUD, G/D
## FIGURE TYPE: Map
      
for (cause in pud_gd$cause_id) {
  plot_data <- asdr %>%
    filter(year_id==year_ids[length(year_ids)] & sex_id==3 & cause_id==cause) %>%
    mutate(mapvar = signif(asdr, digits = 2))
  #bin_limits_raw <- seq(min(plot_data$asdr), max(plot_data$asdr), length.out=10) # raw sequence of bin limits from min to max ASDR. 
  cut_pt <- if_else(cause == 527, 20, 3)
  bin_limits_raw <- c(0, quantile(c(plot_data[plot_data$asdr<cut_pt, ]$asdr), c(0.2, 0.4, 0.6, 0.8)), cut_pt, max(plot_data$asdr))
  #bin_limits <- c(floor(bin_limits_raw[1]), ceiling(bin_limits_raw[2:length(bin_limits_raw)])) # flooring first bin limit, rounding up all other bin limits to include all values of ASDR.
  bin_limits <- sprintf("%.1f", bin_limits_raw) %>% as.numeric()
  
  pdf(paste0(out_path, "FIG2_", cause, "_asdr_2019.pdf"), height = 4.15, width = 7.5, pointsize = 6.5)
  gbd_map(plot_data, bin_limits, 
          #fname=paste0(out_path, cause, "_asdr_2019_FIG3.pdf"), # fname argument overrides pdf() specifications
          col.rev=TRUE, 
          #legend.cex = 0.9, #scaling factor for legend
          legend.shift = c(7,-5),
          legend.columns = 2,
          #title=paste0("Age-standardized death rate (per 100,000) due to ", pud_gd[pud_gd$cause_id==cause,]$cause_name_full, ", both sexes, ", year_ids[length(year_ids)]), 
          legend.title="Age-standardized death rate per 100,000") # person-years")
  dev.off()
}
##----------------------------------------------------------

      
      #Percent change 1990 to 2019 map
      asdr_change <- get_outputs(
          "cause",
          cause_id = 992,
          sex_id = 3,
          year_start_id = 1990,
          year_end_id = year_ids[length(year_ids)],
          metric_id = 3,
          measure_id = 1,
          age_group_id = 27,
          location_id = "all",
          gbd_round_id = gbd_rnd, 
          decomp_step = gbd_decomp_step,
          compare_version_id = compare_version
        )
      
      asdr_change <- asdr_change[, mapvar:=signif(val*100, digits=2)]
      bin_limits<-seq(min(asdr_change$val*100), max(asdr_change$val*100), length.out=10)
      bin_limits<-signif(bin_limits, digits = 2)
      
      asdr_change <- merge(asdr_change, location_data, by="location_id")
      print(asdr_change[val>0 & level==3,]) # countries with increase in upper digestive diseases since 1990
      
      gbd_map(asdr_change, bin_limits, fname=paste0(out_path, "asdr_change.pdf"), col.rev=TRUE, title=paste0("Change in age-standardized death rate due to upper digestive system diseases, both sexes, 1990 to ", year_ids[length(year_ids)]), legend.title="Percent change in age-standardized death rate")
      
      
      #Try to explain exceptions in the above map
      env_change <- get_envelope(location_id=ntl_loc_id, year_id=c(1990,2017), sex_id=3, gbd_round_id=gbd_rnd, with_hiv=1, rates=1, decomp_step = gbd_decomp_step)
      env_change <- dcast(env_change, location_id ~ year_id, value.var = c("mean", "upper", "lower"))
      env_change[ , delta:=mean_2017-mean_1990]
      #@ Added decomp_step & compare_version #Only year_id available, 2019? 
      #@year_start_id = 1990 & year_end_id = 2017 were excluded, as it seems only data for year 2019 was provided!(year_start_id=1990, year_end_id=2017)
      
      cf_change <- get_outputs("cause", cause_id=fatal_causes, sex_id=3, measure_id=1, metric_id=2, gbd_round_id=gbd_rnd, location_id="lvl3", decomp_step = gbd_decomp_step, compare_version_id = compare_version)
      #@year_start_id = 1990 & year_end_id = 2017 were excluded, as it seems only data for year 2019 was provided!(year_start_id=1990, year_end_id=2017)
      #@year_start_id = 1990 & year_end_id = 2017 were excluded, as it seems only data for year 2019 was provided!(year_start_id=1990, year_end_id=2017)
      #@ Review original code for review
      
      inc_env <- env_change[delta>0, ]
      inc_cf <- cf_change[val>0, ]
      explain <- merge(inc_env, inc_cf, by="location_id")
      print(explain[cause_id==992,])
      
      #Report sex-ratios in 1990 and 2017 for UGI, PUD and gastritis
      #asdr_sex <- asdr[sex_id=c(1,2), year_id=c(1990,2017),] #@ This exact same line of code doesn't work, used the line below instead!
      asdr_sex <- subset(asdr, (sex_id == c(1,2) & year_id == c(1990, 2017)))
      asdr_sex_ratio <- dcast(asdr_sex, year_id + cause_id + location_id ~ sex_id, value.var = c("asdr", "upper", "lower"))
      asdr_sex_ratio[, asdr_ratio:=asdr_1/asdr_2]
      print(asdr_sex_ratio[location_id %in% c(1, sr_loc_id), ])
      #No uncertainty; will have to pull draws to get with uncertainty ## UPDATED FOR 2019 - see ASDR in-text statement section below. 

      
##----------------------------------------------------------
## FIGURE 2A-B: Age standardized death rate per 100,000 population of PUD/GD (by GBD region/sex) 2019
## FIGURE TYPE: Bar graph, by sex
      
for(cause in pud_gd$cause_id){
  plot_data <- asdr %>% 
    filter(cause_id == cause & location_type == "region" & year_id == year_ids[length(year_ids)])
  
  # Creating ordered vector of region names in descending ASDR order (based on both-sex mean)
  region_order <- plot_data %>% 
    filter(sex == "Both") %>%
    arrange(val) %>%
    pull(location_name)
    
  plot <- ggplot(data = plot_data %>%
                   filter(sex != "Both") %>%
                   mutate(sex = factor(sex, levels = c("Female", "Male")), #reordering sex
                          location_name = factor(location_name, levels = region_order)), #reordering GBD regions by descending mean ASDR (based on both-sex mean)
                 aes(x = location_name, y = val*100000, fill = sex)) + 
    geom_bar(stat="identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=lower*100000, ymax=upper*100000), 
                  width = 0.5,
                  position = position_dodge(0.9)) +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    scale_fill_discrete(name = "", labels = c("Male (Rate, 95% Uncertainty Limits)", "Female (Rate, 95% Uncertainty Limits)"), limits=c("Male", "Female")) +
    #scale_x_discrete(breaks = c(1, seq(from = 5, to = 95, by = 5))) +
    #scale_y_continuous(breaks = seq(from = 5000, to = 20000, by = 5000), limits = c(0, 20000)) +
    xlab("GBD Region") +
    ylab("Age-standardised death rate per 100,000 population") #+
    #ggtitle(paste0("Age-standardized death rate due to ", pud_gd[pud_gd$cause_id==cause,]$cause_name_full, " by GBD Regions for men and women in ", year_ids[length(year_ids)]))
  
  ggsave(paste0("SUPP3_asdr_by_region_", plot_data$acause %>% unique(), ".pdf"), path = out_path, width=12, height=10)    
}
dev.off()


## FIGURE 2, version 2 [version used in final appendix, renumbered to SUPPLEMENT 3]: ASDR per 100,000 pop of PUD and GD (by GBD region/sex) 2019 - ONE plot with both causes.
## FIGURE TYPE: Bar graph, by sex and cause
plot_data <- asdr %>% 
  filter(cause_id %in% pud_gd$cause_id & location_type == "region" & year_id == year_ids[length(year_ids)] & sex != "Both") %>%
  mutate(asdr = val*100000, lower = lower*100000, upper = upper*100000, 
         sex = factor(sex, levels = c("Male", "Female")),
         location_name = factor(location_name, levels = rev(regions$location_name)), 
         cause_sex = paste0(sex, if_else(cause_id==527, " PUD", " G/D")),
         cause_sex = factor(cause_sex, levels = rev(c("Male PUD", "Male G/D", "Female PUD", "Female G/D")))) # reversed factor order to be compatible with coordinate flip

plot <- ggplot(data = plot_data,
               aes(x = location_name, y = asdr, fill = cause_sex)) + 
  geom_bar(stat="identity",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width = 0.5,
                position = position_dodge(0.9)) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_fill_manual(name = "", 
                      labels = c("Male PUD (Rate, 95% Uncertainty Limits)", "Male G/D (Rate, 95% Uncertainty Limits)", "Female PUD (Rate, 95% Uncertainty Limits)", "Female G/D (Rate, 95% Uncertainty Limits)"), 
                      limits=c("Male PUD", "Male G/D", "Female PUD", "Female G/D"), 
                      values = c("#F9766A", "#FBBAB6", "#00C0C3", "#81DFDE")) +
  #scale_alpha_manual(name = "", labels = c("G/D", "PUD"), values = c(0.5, 1)) + # different approach - shading to distinguish PUD, G/D
  xlab("GBD Region") +
  ylab("Age-standardised death rate per 100,000 population") #+
  #ggtitle(paste0("Age-standardized death rate due to PUD/GD by GBD Regions for men and women in ", year_ids[length(year_ids)]))

ggsave(paste0("SUPP3_asdr_by_region_PUD_GD.pdf"), path = out_path, width=12, height=10)    

dev.off()
##----------------------------------------------------------


##----------------------------------------------------------    
## FIGURE 4A-C [renumbered to FIGURE 3]: Plot ASDR vs SDI or other covariate for all national locations
## FIGURE TYPE: Scatter w/ time trend

asdr_both <- asdr[sex_id==3 & cause_id==992, ]
#asdr_both <- asdr_both[ , xlab:=signif(asdr, digits=2)]
cov_ids <- c("881", "160", "142")

for (x in cov_ids) {
  cov_national <- get_covariate_estimates(covariate_id=x, year_id=year_ids, location_id=ntl_loc_id, gbd_round_id=gbd_rnd, decomp_step = "step4") %>%
    left_join(get_ids("covariate"), by = c('covariate_id', 'covariate_name_short')) %>%
    mutate(cov = mean_value)
  asdr_cov_data <- merge(asdr_both, cov_national, by=c("location_id", "year_id")) %>% 
    filter(year_id %in% year_ids) %>%
    #filter(year_id == year_ids[length(year_ids)]) %>% #2019 only
    left_join(location_data, by = 'location_id')
  asdr_cov_data[year_id == 2019, ]$year_id <- 2020 # a bit of data fudging to make legend go through 2020. Only slightly changes shading (unnoticeable).
  
  asdr_cov_fig <- ggplot(data = asdr_cov_data, 
                         aes(x = cov, y = asdr, color=factor(super_region_name), alpha = year_id)) + 
                          #aes(x = cov, y = asdr, color=factor(super_region_name)), alpha = 0.2) + #2019 only
      geom_point() +
      #ggtitle(paste0("Age-standardized death rate due to ", asdr_cov_data$cause_name, " by ", asdr_cov_data$covariate_name, ", both sexes, 1990-", year_ids[length(year_ids)])) +
      xlab(asdr_cov_data$covariate_name) +
      ylab("Age-standardized death rate per 100,000 person-years") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      scale_color_discrete(name = "GBD Super-region") +
      scale_alpha_continuous(name = "Year")
      #theme(legend.position = "none") # removes legend
  
  # Adding location labels for SDI plot - Cambodia, Laos, Greenland, Bangladesh and Nepal - only for version with 2019 values only
  if(x == "881" & asdr_cov_data$year_id %>% unique() %>% length() == 1){
    asdr_cov_fig <- asdr_cov_fig +
      geom_text(aes(label=ifelse(location_id %in% c(10,12,23, 19, 349), as.character(ihme_loc_id),'')), size=3, nudge_x=0.025, nudge_y=0, show.legend = FALSE) +
      geom_text(aes(label=ifelse(location_id %in% c(164), as.character(ihme_loc_id),'')), size=3, nudge_x=-0.025, nudge_y=0, show.legend = FALSE) +
      geom_text(aes(label=ifelse(location_id %in% c(161), as.character(ihme_loc_id),'')), size=3, nudge_x=-0.025, nudge_y=-0.25, show.legend = FALSE)
  }
  ggsave(paste0("FIG3_asdr_", asdr_cov_data$covariate_name_short %>% unique(), "_timeseries.pdf"), path = out_path, width=12, height=10)
}
 dev.off()

## FIGURE 3 legend insert - generates grid of dots with colors/transparency
## values corresponding to super-regions and years. 
## NOTE: this legend insert was manually edited to condense horizontal dimension
## and generate legend version used for submission to collaborator network. This
## is not an ideal method, as the final legend image could not be properly
## combined with the plot pdf to generate a vectorized image for the Lancet. All
## components of this figure are saved at
## /ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/figs_outstanding_issues
 
legend_data <- data.frame(super_region = rep((location_data$super_region_name %>% unique())[2:8], each = 4), 
                          year = rep(c(1990, 2000, 2010, 2019), times = 7)) %>%
  mutate(super_region = factor(super_region, levels = c("Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))) %>%
  arrange(super_region) %>%
  cbind(., val = rep(c(1:4), times = 7))
legend_fig <- ggplot(data = legend_data,
                       aes(x = fct_rev(super_region), y = val, color=super_region, alpha = year)) + 
  geom_point(size = 7) +
  coord_flip() +
  #scale_x_discrete(expand=c(0,0.6)) +
  #coord_fixed(ratio = 3) +
  #scale_alpha_continuous(limits = c("1990", "2000", "2010", "2019"), expand=c(0.2, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(paste0("FIG3_asdr_legend.pdf"), path = out_path, width=6, height=4)
dev.off()  
  #scale_color_discrete(name = "GBD Super-region") +
  #scale_alpha_continuous(name = "Year")
##----------------------------------------------------------


## IN-TEXT STATEMENTS

## 2. In 2019, the global age-standardized death rate for PUD was 3.0 (95%
## uncertainty interval [UI] 2.7 to 3.3) per 100,000 person-years [TABLE 1A] and
## national age-standardized death rates ranged from 0.5 (0.3 to 0.6) to 22.5
## (17.4 to 29.0)
  asdr %>% filter(year_id == year_ids[length(year_ids)] & location_name == "Global" & sex_id==3)
  min_max <- asdr %>% left_join(location_data) %>% filter(acause=="digest_pud" & level==3 & year_id==2019 & sex_id==3) %>% pull(asdr) %>% range()
  asdr %>% filter(asdr %in% min_max) # Cambodia = 22.5 (17.4, 29.0), Sri Lanka = 0.5 (0.3, 0.6)

## 3. The same year [2019], global age-standardized death rate for G/D was 0.5
## (0.4 to 0.5) [TABLE 1A] and national age-standardized death rates ranged from
## 0.0 (0.0 to 0.0) to 4.3 (2.1 to 7.6).
  asdr %>% filter(year_id == year_ids[length(year_ids)] & location_name == "Global" & sex_id==3)
  min_max <- asdr %>% left_join(location_data) %>% filter(acause=="digest_gastritis" & level==3 & year_id==2019 & sex_id==3) %>% pull(asdr) %>% range()
  asdr %>% filter(asdr %in% min_max) # Uzbekistan = 0.0 (0.0, 0.0), Central African Rep = 4.3 (2.1, 7.6)
  
## 24. Globally, the age-standardized cause-specific mortality rate
## (age-standardized death rate, or ASDR) due to PUD for both sexes combined was
## 6.2 (5.6-8.0) to 9.1 (7.1-11.0) times greater than for G/D in all years.
  
  # Range w/o uncertainty
  asdr %>% 
    filter(cause_id %in% pud_gd$cause_id & sex_id==3 & location_id==1) %>% 
    select(-cause_name, -cause_id) %>% 
    pivot_wider(., names_from = acause, values_from = c(val, upper, lower)) %>% 
    mutate(pud_gd_ratio = val_digest_pud/val_digest_gastritis) %>% 
    pull(pud_gd_ratio) %>% 
    range()
  
  # Range with uncertainty
  draw_cols <- paste0("draw_",0:999)
  pud_gd_asdr_draws <- get_draws(
    gbd_id_type='cause_id',
    gbd_id=pud_gd$cause_id,
    source='codcorrect',
    age_group_id = 27,
    measure_id=1, #deaths
    sex_id=3,
    location_id = 1,
    gbd_round_id=gbd_rnd,
    decomp_step=gbd_decomp_step,
    status = "best",
    version_id = cod_version,
    year_id = year_ids) %>%
    pivot_longer(all_of(draw_cols), names_to = "draw_num", names_prefix = "draw_", values_to = "val")
  
  ratio <- pud_gd_asdr_draws %>% pivot_wider(., names_from = cause_id, values_from = val) %>%
    mutate(pud_gd_ratio = `527`/`528`) %>%
    group_by(year_id) %>%
    summarize(mean = mean(pud_gd_ratio), 
              lower = quantile(pud_gd_ratio, 0.025), 
              upper = quantile(pud_gd_ratio, 0.975))
  ratio %>% filter(mean %in% range(ratio$mean))
  
  
## 33. Globally, the male-to-female sex ratio of the ASDR due to the combination
## of PUD and G/D was 1.73 (95% UI 1.47 to 1.96) in 1990 and 1.45 (1.29 to 1.65)
## in 2019, ranging from 1.19 (0.86 to 1.55) to 3.15 (2.97 to 3.32) in 1990 and
## 0.94 (0.70 to 1.23) to 2.21 (1.92 to 2.54) in 2019 across the GBD
## super-regions.
  
  draw_cols <- paste0("draw_",0:999)
  
  # Pull ASDR draws for global and superregions, by sex and year (1990, 2019)
  ud_draws <- get_draws(
                gbd_id_type='cause_id',
                gbd_id=992,
                source='codcorrect',
                age_group_id = 27,
                measure_id=1, #deaths
                sex_id=c(1,2),
                location_id = c(1, super_regions$location_id),
                gbd_round_id=gbd_rnd,
                decomp_step=gbd_decomp_step,
                status = "best",
                version_id = cod_version,
                year_id = c(year_ids[1], year_ids[length(year_ids)])) %>%
    pivot_longer(all_of(draw_cols), names_to = "draw_num", names_prefix = "draw_", values_to = "val")
  
  # Calculate male/female ASDR ratio with 95% UI for each location/year.
  mf_ratios <- ud_draws %>% 
      pivot_wider(names_from = "sex_id", values_from = "val", names_prefix = "val_sex_") %>%
      mutate(m_to_f = val_sex_1/val_sex_2) %>%
      group_by(location_id, year_id) %>%
      summarize_at(vars(m_to_f), list(mean = mean, 
                                       lower = function(x){quantile(x, 0.025)}, 
                                       upper = function(x){quantile(x, 0.975)})) %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_id")
  
  # Identify superregions with highest and lowest M/F ASDR ratios. 
  # South Asia has lowest ratio in both years; Central Europe/Eastern Europe/Central Asia has highest ratio in both years. 
  for (year in c(year_ids[1], year_ids[length(year_ids)])) {
    print(mf_ratios %>% 
            filter(mean %in% (mf_ratios %>% 
                                filter(location_id!=1 & year_id==year) %>% 
                                pull(mean) %>% 
                                range())) %>% 
            arrange(mean))          
  }
  
  
    
############################################################
## Cause-specific mortality rate (CSMR), death counts
############################################################

# Pull CSMR and deaths for 2019, both sex, all age groups, global
csmr_deaths <-get_outputs(
  "cause",
  cause_id = pud_gd$cause_id,
  sex_id = c(1, 2),
  year_id = year_ids[length(year_ids)],
  metric_id = c(1,3), # number, rate
  measure_id = 1, #deaths
  age_group_id = all_age_grps,
  gbd_round_id = gbd_rnd,
  decomp_step = gbd_decomp_step,
  compare_version_id = compare_version
)    


##----------------------------------------------------------
## FIGURE 1: CSMR and death counts in 2019 for PUD, G/D by age,sex  
## FIGURE TYPE: line and bar graphs by sex

for (cause in pud_gd$cause_id) {
  for(metric in c(1,3)){
    plot_data <- csmr_deaths %>%
      filter(cause_id == cause & metric_id == metric) %>%
      mutate(age_start = as.numeric(gsub("^([0-9]+).*", "\\1", age_group_name)))
    
    #FIGURES 1A,C: CSMR by age and sex, line graphs
    if (metric == 3) {
      plot <- ggplot() +
        geom_line(data = plot_data, aes(x = age_start, y = val*100000, color = sex)) +
        #geom_line(data = plot_data, aes(x = age_start, y = lower*100000, color = sex), linetype = "dotted") +
        #geom_line(data = plot_data, aes(x = age_start, y = upper*100000, color = sex), linetype = "dashed") +
        geom_ribbon(data = plot_data, aes(ymin = lower*100000, ymax = upper*100000, x=age_start, fill=sex), color= NA, alpha = 0.2) +
                #scale_linetype_manual(values = c("Male" = "dashed", "Female" = "dotted")) +
        guides(fill = FALSE) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
        scale_color_discrete(name = "", labels = c("Male (Rate, 95% Uncertainty Limits)", "Female (Rate, 95% Uncertainty Limits)"), limits=c("Male", "Female")) +
        theme(legend.key = element_rect(colour = "transparent", fill = "white")) +
        scale_fill_discrete(name = "", limits = c("Male", "Female")) +
        scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 140)) +
        xlab("Age (years)") +
        ylab("Cause-specific mortality rate per 100,000 person-years") +
        theme(legend.key = element_rect(fill = NA)) #+
        #ggtitle(paste0("Cause-specific mortality rate due to ", pud_gd[pud_gd$cause_id==cause,]$cause_name_full, " by age, for men and women in ", year_ids[length(year_ids)]))
      
      ggsave(paste0("FIG1_csmr_", plot_data$acause %>% unique(), ".pdf"), path = out_path, width=12, height=10)
    }
        
    ## FIGURES 1B, D: total deaths by age and sex, bar graphs
    else {
      plot <- ggplot(data = plot_data %>%
                       mutate(age_start = as.factor(age_start), # converting age_start to factor so ggplot treats it as discrete variable
                              sex = factor(sex, levels = c("Male", "Female"))), #reordering sex
                     aes(x = age_start, y = val, fill = sex)) + 
        #geom_col(aes(fill = sex), position = "dodge") +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), 
                      width = 0.5,
                      position = position_dodge(0.9)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
        scale_fill_discrete(name = "", labels = c("Male (Number, 95% Uncertainty Limits)", "Female (Number, 95% Uncertainty Limits)"), limits=c("Male", "Female")) +
        scale_x_discrete(breaks = c(1, seq(from = 5, to = 95, by = 5))) +
        scale_y_continuous(breaks = seq(from = 5000, to = 20000, by = 5000), limits = c(0, 20000)) +
        xlab("Age (years)") +
        ylab("Total deaths (Number)") #+
        #ggtitle(paste0("Total deaths due to ", pud_gd[pud_gd$cause_id==cause,]$cause_name_full, " by age, for men and women in ", year_ids[length(year_ids)]))
      
      ggsave(paste0("FIG1_deaths_", plot_data$acause %>% unique(), ".pdf"), path = out_path, width=12, height=10)
    }
  }   
}  
dev.off()
##----------------------------------------------------------
    
    
    
############################################################
## DALYs, YLLs
############################################################ 

# Pull DALYs and YLLs for all years, both sexes, all-ages, global
dalys_ylls <-
  get_outputs(
    "cause",
    cause_id = all_causes,
    sex_id = 3,
    year_id = year_ids,
    measure_id = c(2, 4), # DALY, YLL
    metric_id = 1, # number
    age_group_id = 22,
    gbd_round_id = gbd_rnd, 
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version)
dalys_ylls <- dalys_ylls[ , yval:=val/1e6]
dalys_ylls <- dalys_ylls[ , yunits:=paste0(signif(yval, digits = 2), "M")]


      
##----------------------------------------------------------     
## FIGURE 5, FIGURE 9: YLLs/DALYs by cause, 1990-2019. [Figure 5 not included in final manuscript]
## FIGURE TYPE: Stacked bar graphs
      
for (z in c(2,4)) {
  data_z <- dalys_ylls[measure_id==z & cause_id %in% pud_gd$cause_id, ]
  data_z$cause_name <- factor(data_z$cause_name, levels=order_causes) 
  data_z$year_id <-factor(as.character(data_z$year_id))
  
  data_cause_yr_z <- ggplot(data_z) +
      geom_bar(mapping = aes(x=year_id, y = yval, fill = cause_name), stat = "identity", width=1) +
      scale_fill_manual(guide = guide_legend(title = "", nrow=22, byrow = T), values = cause_colors) +
      scale_x_discrete(labels=as.character(unique(data_z$year_id)), expand=c(0,0)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      xlab("Year") +
      ylab(paste0("Million ", data_z$measure_name)) +
      ggtitle(paste0(data_z$measure_name, " due to PUD and G/D, from 1990 to ", year_ids[length(year_ids)])) 

ggsave(paste0(data_z$measure_name %>% unique() %>% word(1), "_by_cause_year.pdf"),  path = paste0(out_path), width=12, height=10)
}
dev.off()

## FIGURE 9, second version [version used in final manuscript, renumbered as FIGURE 4]: YLLs/YLDs by cause, 1990-2019
## FIGURE TYPE: Stacked bar graphs, faceted

ylds <-
  get_outputs(
    "cause",
    cause_id = pud_gd$cause_id,
    sex_id = 3,
    year_id = year_ids,
    measure_id = 3, # YLDs
    metric_id = 1, # number
    age_group_id = 22,
    location_id = 1,
    gbd_round_id = gbd_rnd, 
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version)

plot_data <- rbind(ylds %>% mutate(yval = val/1e6), dalys_ylls[measure_id==4 & cause_id %in% pud_gd$cause_id, ], fill = TRUE) %>%
  mutate(cause_measure = paste0(word(measure_name), " due to ", if_else(cause_id==527, "PUD", "G/D"))) %>%
  mutate(cause_measure = factor(cause_measure, levels = c("YLDs due to G/D", "YLLs due to G/D", "YLDs due to PUD", "YLLs due to PUD")))

plot <- ggplot(plot_data) +
  geom_bar(mapping = aes(x=year_id, y = yval, fill = cause_measure), position = "stack", stat = "identity", width=1) +
  scale_fill_manual(guide = guide_legend(title = ""), values=c("#67B56B", "#95CB97", "#4C0089", "#834DAD")) + #additional colors based on cause_colors and IHME style guide
  #scale_fill_manual(guide = guide_legend(title = "", nrow=22, byrow = T), values = cause_colors) +
  scale_alpha_manual(values = c(1, 0.7)) +
  #scale_x_discrete(labels=as.character(unique(plot_data$year_id)), expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.spacing = unit(1.5, "lines")) +
  xlab("Year") +
  ylab("Million YLDs, YLLs") #+
  #ggtitle(paste0("YLDs, YLLs due to PUD and G/D, from 1990 to ", year_ids[length(year_ids)])) 
ggsave("FIG4_YLD_YLL_by_cause_year_stacked.pdf",  path = paste0(out_path), width=12, height=10)

##----------------------------------------------------------


## IN-TEXT STATEMENTS

## 5. Global age-standardized prevalence and rates of YLLs, YLDs and DALYs also
## decreased for PUD during this time-period [1990:2019]
  yll_rate <-
    get_outputs(
      "cause",
      cause_id = 527,
      sex_id = 3,
      year_id = c(year_ids[1], year_ids[length(year_ids)]),
      measure_id = 4, # DALY, YLL
      metric_id = 3, # rate
      age_group_id = 27, #age standardized
      gbd_round_id = gbd_rnd, 
      decomp_step = gbd_decomp_step,
      compare_version_id = compare_version)

## 8. Overall health loss due to these diseases in 2019 was 6.0 million (5.6 to
## 6.6) DALYs due to PUD and 2.8 million (2.1 to 3.8) DALYs due to G/D [TABLE
## 1C], 0.2% (0.2 to 0.3) and 0.1% (0.1 to 0.1) of all DALYs due to all
## diseases and injuries in GBD.
  daly_pct <- get_outputs(
    "cause",
    cause_id = pud_gd$cause_id,
    sex_id = 3,
    year_id = year_ids[length(year_ids)],
    metric_id = 2, #percent
    measure_id = 2, #DALY
    age_group_id = 22,
    location_id = 1,
    gbd_round_id = gbd_rnd,
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version
  )

## 36. Globally, PUD and G/D were responsible for 8.77 million YLLs (95% UI 8.12
## to 9.59 million) in 1990 and 6.47 million YLLs (95% UI 6.02 to 7.08 million) in
## 2019, representing a decrease in age-standardized rate from 202.3 YLLs per 100,000 population
## (95% UI 187.9 to 219.9) to 80.0 YLLs per 100,000 population (95% UI 74.3 to 87.3) between
## 1990 and 2019, a 60.5% decrease in the rate of YLLs (95% UI 56.8-64.1).
  upper_dig_yll <-
    get_outputs(
      "cause",
      cause_id = 992, #upper digest - combination of PUD/GD for fatal results
      sex_id = 3,
      year_id = c(year_ids[1], year_ids[length(year_ids)]),
      measure_id = 4, # YLL
      metric_id = c(1, 3), # number, rate
      age_group_id = c(22, 27),
      gbd_round_id = gbd_rnd, 
      decomp_step = gbd_decomp_step,
      compare_version_id = compare_version)
  upper_dig_yll %>% filter((metric_name=="Rate" & age_group_id==27) | (metric_name=="Number" & age_group_id==22)) #%>% mutate(val = val*100000, lower = lower*100000, upper = upper*100000)
  
  upper_dig_pct <- get_pct_change(gbd_id_type='cause_id', 
                 gbd_id=fatal_causes,
                 year_start_id=year_ids[1], 
                 year_end_id=year_ids[length(year_ids)],
                 location_id=1, 
                 age_group_id=27, 
                 sex_id=3,
                 gbd_round_id=gbd_rnd, 
                 decomp_step=gbd_decomp_step, 
                 version_id = cod_version, 
                 source='codcorrect', 
                 change_type='pct_change_rate', measure_id = 4)
  
## 37. PUD accounted for 7.85 million of these YLLs in 1990 (95% UI 7.25 to 8.61)
## and for 5.60 million YLLs in 2019 (95% UI 5.20 to 6.22), for age-standardized rates of 181.4 per
## 100,000 population (95% UI 168.2 to 197.9) and 69.1 per 100,000 (95% UI 64.2 to 76.6),
## respectively.
## 38. G/D, on the other hand, accounted for 919 000 YLLs in 1990 (95% CI
## 680 000 to 1 150 000) and for 872 000 YLLs in 2019 (95% UI 690 000 to 999 000), for age-standardized rates
## of 20.9 per 100,000 population (95% UI 15.9 to 26.4) and 10.9 per 100,000 (95% UI 8.6
## to 12.5) in 1990 and 2019, respectively
  dalys_ylls %>% 
    filter(year_id %in% c(year_ids[1], year_ids[length(year_ids)]) & location_name == "Global" & cause_id %in% pud_gd$cause_id & measure_id==4)
  yll_rate <- get_outputs(
    "cause",
    cause_id = pud_gd$cause_id,
    sex_id = 3,
    year_id = c(year_ids[1], year_ids[length(year_ids)]),
    metric_id =3, #rate
    measure_id = 4, #YLL
    age_group_id = 27, #age-standardized
    location_id = 1,
    gbd_round_id = gbd_rnd,
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version) %>%
    mutate(val = val*100000, lower = lower*100000, upper = upper*100000)

## 39. This represents a decrease in the age-standardized rate of YLLs for PUD of 61.9% (95% UI
## 58.1% to 65.4%) and a decrease in the age-standardized rate of YLLs for G/D of 48.2% (95% UI
## 30.4% to 60.5%) and a decrease in the age-standardized PUD-to-G/D ratio from
## 8.8 (6.9 to 11.5) to 6.4 (5.6 to 8.5) over this 29-year period.
  upper_dig_pct %>% filter(cause_id %in% pud_gd$cause_id)
  
  # Pull global YLLs draws (1990, 2019)
  ud_yll_draws <- get_draws(
    gbd_id_type='cause_id',
    gbd_id=pud_gd$cause_id,
    source='codcorrect',
    age_group_id = 27, #age-standardized
    measure_id=4, #YLLs
    sex_id=3,
    location_id = 1,
    gbd_round_id=gbd_rnd,
    decomp_step=gbd_decomp_step,
    status = "best",
    version_id = cod_version,
    year_id = c(year_ids[1], year_ids[length(year_ids)])) %>%
    pivot_longer(all_of(draw_cols), names_to = "draw_num", names_prefix = "draw_", values_to = "val")
  
  # Calculate PUD:G/D YLL ratio with 95% UI for each cause/year.
  pud_gd_ratios <- ud_yll_draws %>% 
    pivot_wider(names_from = "cause_id", values_from = "val", names_prefix = "val_") %>%
    mutate(pud_to_gd = val_527/val_528) %>%
    group_by(year_id) %>%
    summarize_at(vars(pud_to_gd), list(mean = mean, 
                                    lower = function(x){quantile(x, 0.025)}, 
                                    upper = function(x){quantile(x, 0.975)}))


############################################################
## Nonfatal
############################################################

      #Nonfatal data inputs
      map_input_epi <- function(bundle, acause){
        loc_map <- copy(ntl_locations[, .(location_id, ihme_loc_id)])
        dt <- as.data.table(fread(paste0(j, "WORK/12_bundle/", acause, "/", bundle, "/gbd2019_RW_year_count_by_location_", bundle, ".csv")))
        dt <- dt[measure_name %in% c("Prevalence")]
        dt[, years_count := sum(years_count), by = "location_id"]
        dt <- unique(dt, by = "location_id")
        dt <- merge(dt, loc_map, by = "location_id")
        dt <- dt[, .(ihme_loc_id, years_count)]
        setnames(dt, "years_count", "mapvar")
        bottom <- floor(min(dt$mapvar)/2)*2
        top <- ceiling(max(dt$mapvar)/2)*2
        gbd_map(data = dt, limits = seq(bottom, top, by = 2), 
                legend.title = "Data Counts (Location-Years)",
                fname = paste0(h, out_path, acause, "_epi_input_", date, ".pdf"))
      }
      
      nonfatal_data_map <- map_input_epi(3059, "digest_gerd")

      #@ COME BACK TO THIS ONE! There are some issues with gbd_map() that I can't seem to resolve.
      #@ Existing error: data is missing location_id or mapvar variables, which are present, but for reason they are getting processed adequately. 
      
############################################################
## Prevalence
############################################################

# Pull prev rate for all sexes, age groups, locs.
prev <-
  get_outputs(
    "cause",
    cause_id = pud_gd$cause_id,
    sex_id = c(1, 2, 3),
    year_id = year_ids,
    measure_id = 5, #prev
    metric_id = 3, #rate
    age_group_id = c(22, 27, all_age_grps),
    location_id = "all",
    gbd_round_id = gbd_rnd, 
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version)


      case_counts <-
        get_outputs(
          "cause",
          cause_id = nonfatal_causes,
          sex_id = c(1, 2, 3),
          year_id = c(1990, year_ids[length(year_ids)]),
          measure_id = 5, #prev
          metric_id = 1, #count
          age_group_id = 22,
          location_id = "all",
          gbd_round_id = gbd_rnd, 
          decomp_step = gbd_decomp_step,
          compare_version_id = compare_version)

      #Text report 
      print(prev[location_id==1 & sex_id==3 & age_group_id==27, ])
      print(prev[location_id==1 & sex_id==3 & age_group_id==22, ])
      print(case_counts[location_id==1 & sex_id==3, ])
      change_asp <- get_outputs("cause", cause_id=536, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=5, metric_id=3, age_group_id=27, gbd_round_id=5)
      change_AAp <- get_outputs("cause", cause_id=536, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=5, metric_id=3, age_group_id=22, gbd_round_id=5)
      change_count <- get_outputs("cause", cause_id=536, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=5, metric_id=1, age_group_id=22, gbd_round_id=5)

      
# -------------------------------------------------------------------------------------------------------------
## DRAW FILES
## FIGURE 6 [renumbered as FIGURE 5]: Age-standardized prevalence of peptic ulcer disease, gastritis and duodenitis (combined), both sexes, 2017
## FIGURE TYPE: Map

      # Launch array job: writes flat draw files of aggregated PUD+G/D age-standardized prev rates for all GBD locations. 
      loc_id_list <- data.table(location_id = location_data$location_id)
      #loc_id_list[, task_num := 1:.N] #all locations
      loc_id_list <- loc_id_list[1:2] #testing array job on first two locations
      print(loc_id_list) #@
      
      map_path <- paste0(out_path, "location_map.csv")
      print(map_path) #@
      write.csv(loc_id_list, map_path, row.names = F)
      n_jobs <- nrow(loc_id_list) 
      
      rscript <- paste0(repo_path, "ugi_paper_child.R")
      print(rscript) #@
      shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_4059.img"
      mem <- "50G"
      threads <- 2
      
      code_command <- paste(shell, "-s", rscript, map_path, out_path, gbd_rnd, gbd_decomp_step) 
      #@ For the full_command call, please compare it to the orginal UGE full_command for vetting purposes.
      full_command <- paste0("sbatch --mem=", mem, " -c ", threads, " -t 01:30:00 -C archive -p long.q -A proj_rgud ",
                             "-J ", "ugi_draws ",
                             "-a ", paste0("1-", n_jobs), " ", #@ changed this
                             "-o /ihme/scratch/users/chrish47/output/%x.o%j ",
                             "-e /ihme/scratch/users/chrish47/errors/%x.e%j ",
                             code_command)
      print(full_command)
      system(full_command)       
# -------------------------------------------------------------------------------------------------------------
# Assemble rows of average PUD/GD aggregated prevalence for all locations into single df
agg_prev_all_locs <- lapply(location_data$location_id, 
                            function(x){
                              read_csv(paste0(out_path, "prev_draws/", x, ".csv")) %>% 
                                filter(draw_num == 0)
                            }) %>%
  do.call(rbind.data.frame, .) %>%
  select(-X1) %>%
  mutate(mapvar = avg_agg*100) %>%
  left_join(location_data, by = 'location_id') %>%
  filter(!str_detect(ihme_loc_id, "POL_")) # removing high-prev Polish subnats to scale bin labels based on highest loc being mapped (Central African Republic)

bin_limits_raw <- seq(min(agg_prev_all_locs$mapvar), max(agg_prev_all_locs$mapvar), length.out=10)
#bin_limits <- signif(bin_limits_raw, digits = 2)
bin_limits <- sprintf("%.1f", bin_limits_raw) %>% as.numeric()

pdf(paste0(out_path, "FIG5_asp_pudgd_2019.pdf"), height = 4.15, width = 7.5, pointsize = 6.5)
gbd_map(agg_prev_all_locs, bin_limits, 
        #fname=paste0(out_path, "asp_pudgd_2019_FIG6.pdf"), #fname arg overrides pdf() specifications
        col.rev=TRUE, 
        #title=paste0("Age-standardized prevalence of peptic ulcer disease, gastritis and duodenitis, both sexes, ", year_ids[length(year_ids)]), 
        legend.title="Age-standardized prevalence, percent")
dev.off()
##----------------------------------------------------------


##----------------------------------------------------------    
## FIGURE 7A,B [renumbered to FIGURE 6]: Prevalence of peptic ulcer disease (PUD) and gastritis/duodenitis by age, for men and women in 2019
## FIGURE TYPE: Line graph by sex

for (cause in pud_gd$cause_id) {
  plot_data <- prev %>%
    filter(cause_id == cause & age_group_id %in% all_age_grps & location_name == "Global" & sex != "Both" & year_id==year_ids[length(year_ids)]) %>%
    mutate(age_start = as.numeric(gsub("^([0-9]+).*", "\\1", age_group_name)))
    
  plot <- ggplot() +
    geom_line(data = plot_data, aes(x = age_start, y = val*100000, color = sex)) +
    geom_line(data = plot_data, aes(x = age_start, y = lower*100000, color = sex), linetype = "dotted") +
    geom_line(data = plot_data, aes(x = age_start, y = upper*100000, color = sex), linetype = "dashed") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    scale_color_discrete(name = "", labels = c("Male (95% Uncertainty Limits)", "Female (95% Uncertainty Limits)"), limits=c("Male", "Female")) +
    #scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 140)) +
    xlab("Age (years)") +
    ylab("Prevalence per 100,000 population") +
    theme(legend.key = element_rect(fill = NA)) #+
    #ggtitle(paste0("Prevalence of ", pud_gd[pud_gd$cause_id==cause,]$cause_name_full, " by age, for men and women in ", year_ids[length(year_ids)]))
  
  ggsave(paste0("FIG6_prevalence_", plot_data$acause %>% unique(), ".pdf"), path = out_path, width=12, height=10)
}
dev.off()
##----------------------------------------------------------


##----------------------------------------------------------
## FIGURE 8: Prevalence of PUD and G/D, for men and women, all ages, 1990−2017
## FIGURE TYPE: Line graph by sex

plot_data <- prev %>%
  filter(cause_id %in% pud_gd$cause_id & age_group_id==22 & location_name == "Global" & sex == "Both")

plot <- ggplot() +
  geom_line(data = plot_data, aes(x = year_id, y = val*100000, color = cause_name)) +
  geom_line(data = plot_data, aes(x = year_id, y = lower*100000, color = cause_name), linetype = "dotted") +
  geom_line(data = plot_data, aes(x = year_id, y = upper*100000, color = cause_name), linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_color_discrete(name = "", labels = c("PUD (95% Uncertainty Limits)", "G/D (95% Uncertainty Limits)"), limits=c("Peptic ulcer disease", "Gastritis and duodenitis")) +
  scale_y_continuous(limits = c(0, 700)) +
  xlab("Year") +
  ylab("Prevalence per 100,000 population") +
  theme(legend.key = element_rect(fill = NA)) +
  ggtitle(paste0("Prevalence of PUD and G/D by sex, all-ages, 1990-", year_ids[length(year_ids)]))

ggsave("FIG8_prevalence_pudgd.pdf", path = out_path, width=12, height=10)
dev.off()


## FIGURE 8, version 2 [used in final manuscript, renumbered FIGURE 7]: Prevalence of PUD and G/D, for men and women, all ages, 1990−2017
## FIGURE TYPE: Line graph by sex

plot_data <- prev %>%
  filter(cause_id %in% pud_gd$cause_id & age_group_id %in% c(22, 27) & location_name == "Global" & sex == "Both") %>%
  mutate(val= val*100000, lower = lower*100000, upper = upper*100000)

plot <- ggplot(plot_data) +
  #facet_wrap(~age_group_name) +
  geom_line(aes(x = year_id, y = val, color = cause_name, linetype = age_group_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x=year_id, color = cause_name, linetype = age_group_name, fill = cause_name), alpha = 0.2) +
  #geom_line(data = plot_data, aes(x = year_id, y = lower, color = cause_name), linetype = "dotted") +
  #geom_line(data = plot_data, aes(x = year_id, y = upper, color = cause_name), linetype = "dotted") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_color_manual(name = "", values=cause_colors[2:3], labels = c("G/D (95% Uncertainty Limits)", "PUD (95% Uncertainty Limits)"), limits=c("Gastritis and duodenitis", "Peptic ulcer disease")) +
  scale_fill_manual(values=cause_colors[2:3]) +
  scale_linetype(name = "") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 705)) +
  xlab("Year") +
  ylab("Prevalence per 100,000 population") +
  theme(legend.key = element_rect(fill = NA)) #+
  #ggtitle(paste0("Prevalence of PUD and G/D by sex, 1990-", year_ids[length(year_ids)]))

ggsave("FIG7_prevalence_pudgd.pdf", path = out_path, width=12, height=10)
dev.off()
##----------------------------------------------------------


## IN-TEXT STATEMENTS
## 50. Globally, the male-to-female sex ratio of the age-standardized prevalence
## due to PUD was 1.41 (95% UI 1.36-1.46) in 1990 and 1.11 (1.08-1.14) in 2019,
## ranging from 1.01 (0.95-1.06) to 2.10 (2.01-2.19) in 1990 and 0.83 (0.79-0.86) to
## 1.65 (1.60-1.69) in 2019 across GBD super-regions.
## 51. For G/D, the global sex-ratio was 0.85 (95% UI 0.81-0.89) in 1990 and
## 0.80 (0.78-0.83) in 2019, ranging 0.59 (0.56-0.61) to 0.96 (0.90-1.03) across
## super-regions in 1990, and 0.56 (0.54-0.59) to 0.95 (0.88-1.02) in 2019.

  mf_prev_ratio <- function(cause){  
    # Pull prev draws for global and superregions, by sex and year (1990, 2019)
    prev_draws <- get_draws(
      gbd_id_type='cause_id',
      gbd_id=cause,
      source='como',
      age_group_id = 27,
      measure_id=5, #prev
      sex_id=c(1,2),
      location_id = c(1, super_regions$location_id),
      gbd_round_id=gbd_rnd,
      decomp_step=gbd_decomp_step,
      status = "best",
      version_id = como_version,
      year_id = c(year_ids[1], year_ids[length(year_ids)])) %>%
      pivot_longer(all_of(draw_cols), names_to = "draw_num", names_prefix = "draw_", values_to = "val")
    
    # Calculate male/female prev rate ratio with 95% UI for each location/year.
    mf_ratios <- prev_draws %>% 
      pivot_wider(names_from = "sex_id", values_from = "val", names_prefix = "val_sex_") %>%
      mutate(m_to_f = val_sex_1/val_sex_2) %>%
      group_by(location_id, year_id) %>%
      summarize_at(vars(m_to_f), list(mean = mean, 
                                      lower = function(x){quantile(x, 0.025)}, 
                                      upper = function(x){quantile(x, 0.975)})) %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_id")
    print(mf_ratios)
    
    # Identify superregions with highest and lowest M/F prev rate ratios. 
    # Sub-Saharan Africa and South Asia have the lowest ratio in 1990 and 2019, respectively; Central Europe/Eastern Europe/Central Asia has highest ratio in both years. 
    for (year in c(year_ids[1], year_ids[length(year_ids)])) {
      print(mf_ratios %>% 
              filter(mean %in% (mf_ratios %>% 
                                  filter(location_id!=1 & year_id==year) %>% 
                                  pull(mean) %>% 
                                  range())) %>% 
              arrange(mean))          
    }
  }

  for(cause in pud_gd$cause_id){
    mf_prev_ratio(cause)
  }

## 56. Global, all-age PUD prevalence falls from 120.3 (101.0 to 142.6) to 104.6 (87.8 to
## 123.9) per 100,000, a decrease of only 13.1% (11.7 to 14.5), and global, all-age G/D prevalence rises from 502.7 (405.5
## to 614.2) to 546.0 (442.0 to 669.7), an increase of 8.6% (4.5 to 13.1).
prev %>% filter(year_id %in% c(year_ids[1], year_ids[length(year_ids)]) & age_group_id == 22 & location_name == "Global" & sex_id == 3) %>%
  mutate(val = val*100000, lower = lower*100000, upper = upper*100000)

pct_change <- get_pct_change(gbd_id_type='cause_id', 
                             gbd_id=pud_gd$cause_id,
                             year_start_id=year_ids[1], 
                             year_end_id=year_ids[length(year_ids)],
                             location_id=1, 
                             age_group_id=22, 
                             sex_id=3,
                             gbd_round_id=gbd_rnd, 
                             decomp_step=gbd_decomp_step, 
                             version_id = como_version, 
                             source='como', 
                             change_type='pct_change_rate', measure_id = 5) %>%
  mutate(mean = mean*100, lower = lower*100, upper = upper*100)


## 71. Our estimates of PUD prevalence, ranging from 0.1% to 0.1% (for all ages
# combined) and 0.1% to 0.1% (after age-standardization) in the years 1995 to 2000,
# are generally lower than those reported by Sung and colleagues.

for(age in c(22, 27)){
  print(age)
  prev %>% filter(year_id %in% c(1995:2005) & age_group_id==age & location_name == "Global" & sex_id==3 & acause == "digest_pud") %>%
    mutate(pct = val*100) %>% pull(pct) %>% range() %>% print() # this gives value range across 10-yr time period, not explicitly in 1995 and 2005
}

###################################Come back to the cause_id issue, as this is a problem further down the road in the code. 
###################################Delete these comments once you figure it out!

      #Map of GERD, age-standardized
      asprev_gerd <- prev[age_group_id==27 & sex_id==3 & year_id==2017 & cause_id==527, ] #@cause_id:536 has no data. Tried 527. 
      asp_gerd_map <- asprev_gerd[ , mapvar:=val*100000]
      asp_gerd_map <- merge(asp_gerd_map, location_data, by="location_id")
      asp_gerd_map <- asp_gerd_map[ , c("ihme_loc_id", "mapvar")]
      
      bin_limits <- seq(min(asp_gerd_map$mapvar), max(asp_gerd_map$mapvar), length.out=10)
      bin_limits <- signif(bin_limits, digits = 2)
      
      gbd_map(asp_gerd_map, bin_limits, fname=paste0(h, out_path, "asp_gerd_2017.pdf"), col.rev=TRUE, title=paste0("Age-standardized prevalence of gastroesophageal reflux disease, both sexes, 2017"), legend.title="Age-standardized prevalence, percent")
      #@ COME BACK TO THIS ONE! There are some issues with gbd_map() that I can't seem to resolve.
      #@ Existing error: data is missing location_id or mapvar variables, which are present, but for reason they are getting processed adequately. 
      
      #Map of GERD, all-ages
      AAprev_gerd <- prev[age_group_id==22 & sex_id==3 & year_id==2017 & cause_id==527, ] #@cause_id:536 has no data. Tried 527. 
      AAprev_gerd <- AAprev_gerd[ , mapvar:=val*100000]
      AAprev_gerd <- merge(AAprev_gerd, location_data, by="location_id")
      AAprev_gerd <- AAprev_gerd[ , c("ihme_loc_id", "mapvar")]
      
      bin_limits <- seq(min(AAprev_gerd$mapvar), max(AAprev_gerd$mapvar), length.out=10)
      bin_limits <- signif(bin_limits, digits = 2)
      
      gbd_map(AAprev_gerd, bin_limits, fname=paste0(h, out_path, "allage_gerd_2017.pdf"), col.rev=TRUE, title=paste0("All-age prevalence of gastroesophageal reflux disease, both sexes, 2017"), legend.title="All-age prevalence, per 100,000")
      #@ COME BACK TO THIS ONE! There are some issues with gbd_map() that I can't seem to resolve.
      #@ Existing error: data is missing location_id or mapvar variables, which are present, but for reason they are getting processed adequately. 
      
      #Figure: Plot ASP vs SDI or other covariate for all national locations in 2017
      asprev_gerd <- asprev_gerd[ , xlab:=signif(val, digits=2)]
      merge_by <- c("location_id", "year_id")
      
      cov_ids <- c("68", "455", "881")
      
      for (x in cov_ids) {
        
        cov_national <- get_covariate_estimates(covariate_id=x, year_id=year_ids, location_id=ntl_loc_id, gbd_round_id=5)
        cov_national <- cov_national[ , cov:=mean_value]
        asp_cov_data <- merge(asprev_gerd, cov_national, by=merge_by)
        
        asp_x_data <- asp_cov_data[covariate_id==x, ]  
        asp_cov_fig <- ggplot(asp_x_data, aes(cov, val)) + geom_point(show.legend=FALSE, aes(alpha=.2, color=factor(location_id))) +
          ggtitle(paste0("Age-standardized prevalence of gastroesophageal reflux disease by ", asp_x_data$covariate_name_short, ", 2017")) +
          xlab(asp_x_data$covariate_name_short) +
          ylab("Age-standardized prevalence per 100,000") +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        ggsave(paste0("asd_", asp_x_data$covariate_name_short, ".pdf"), device = pdf, path = paste0(out_path), width=12, height=10)
        
      }
      #@ path = paste0(h, out_path) to what's above. 



############################################################
## YLDs
############################################################

ylds <-
  get_outputs(
    "cause",
    cause_id = pud_gd$cause_id,
    sex_id = 3,
    year_id = year_ids,
    measure_id = 3, # YLDs
    metric_id = 1, # number
    age_group_id = 22,
    location_id = 1,
    gbd_round_id = gbd_rnd, 
    decomp_step = gbd_decomp_step,
    compare_version_id = compare_version)
      
      change_yld <- get_outputs("cause", cause_id=nonfatal_causes, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=3, metric_id=1, age_group_id=22, location_id=c(1, sr_loc_id), gbd_round_id=5)
      print(change_yld[cause_id==536, ])
      
      yld_rate <- get_outputs("cause", cause_id=nonfatal_causes, sex_id=3, year_id=c(1990,2017), measure_id=3, metric_id=3, age_group_id=c(22,27), location_id=c(1, sr_loc_id), gbd_round_id=5)
      change_yldAArate <- get_outputs("cause", cause_id=nonfatal_causes, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=3, metric_id=3, age_group_id=22, location_id=c(1, sr_loc_id), gbd_round_id=5)
      print(change_yldAArate[cause_id==536,])
      change_yldASrate <- get_outputs("cause", cause_id=nonfatal_causes, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=3, metric_id=3, age_group_id=27, location_id=c(1, sr_loc_id), gbd_round_id=5)
      print(change_yldASrate[cause_id==536,])
      
      change_relyld <- change_count <- get_outputs("cause", cause_id=536, sex_id=3, year_start_id=1990, year_end_id=2017, measure_id=3, metric_id=2, age_group_id=22, gbd_round_id=5)
      
      #YLDs due to UGI, PUD and gastritis, in 2017 and 1990, give ratio and any change in ratio over time
      yld_cause_ratio <-dcast(ylds, year_id + location_id ~ cause_id, value.var = c("val", "upper", "lower"))
      yld_cause_ratio[, gastritis_ratio:=val_528/val_527]
      yld_cause_ratio[, gerd_ratio:=val_536/val_527]
      yld_cause_ratio[, val_ugi:= val_527+val_528+val_536]
      print(yld_cause_ratio)
      #No uncertainty, need to do with draws
      
      #Report sex ratios for PUD, g/d, GERD in 2017 and 1990
      prev_ratio <- prev[sex_id %in% c(1,2) & age_group_id==27, ]
      prev_ratio <- dcast(prev_ratio, year_id + location_id + cause_id ~ sex_id, value.var = c("val", "upper", "lower"))
      prev_ratio[, prev_ratio:=val_1/val_2]
      prev_ratio <- prev_ratio[location_id %in% c(1, sr_loc_id), ]
      #Without uncertainty, will need draws to get with uncertainty

      
## IN-TEXT STATEMENTS

## 5. Global age-standardized prevalence and rates of YLLs, YLDs and DALYs also
## decreased for PUD during this time-period [1990:2019]
yld_rate <- get_outputs(
          "cause",
          cause_id = pud_gd$cause_id,
          sex_id = 3,
          year_id = c(year_ids[1], year_ids[length(year_ids)]),
          measure_id = 3, # YLDs
          metric_id = 3, # number
          age_group_id = 27, #age standardized
          location_id = 1,
          gbd_round_id = gbd_rnd, 
          decomp_step = gbd_decomp_step,
          compare_version_id = compare_version)  
yld_rate %>% filter(cause_id==527)
      
## 55. Globally, PUD and G/D were responsible for 348 000 (95% UI 227 000 to 486
## 000) and 1.30 million (0.808 to 1.93) YLDs in 1990, respectively.
## 56. By 2019, these [global YLDs] had increased to 428 000 (279 000 to
## 598 000) and 1.94 million (1.22 to 2.91) for PUD and G/D,
## respectively.
ylds %>% filter(year_id %in% c(year_ids[1], year_ids[length(year_ids)]))

## 57. The rise in YLDs for these diseases contrasts the declines in
## age-standardized YLD rates of 7.6 per 100,000 person-years (95% UI 5.0 to
## 10.7) in 1990 and 5.3 (3.4 to 7.4) in 2019 for PUD, (change -31.0%, 95% UI
## -28.7 to -33.1) and 27.2 (17.1 to 40.8) in 1990 and 23.9 (15.0 to 35.8) in
## 2019 for G/D (change -12.0%, -9.6 to -14.8).
yld_rate %>% mutate(val = val*100000, lower = lower*100000, upper = upper*100000)

yld_pct <- get_pct_change(gbd_id_type='cause_id', 
                                gbd_id=pud_gd$cause_id,
                                year_start_id=year_ids[1], 
                                year_end_id=year_ids[length(year_ids)],
                                location_id=1, 
                                age_group_id=27, 
                                sex_id=3,
                                gbd_round_id=gbd_rnd, 
                                decomp_step=gbd_decomp_step, 
                                version_id = como_version, 
                                source='como', 
                                change_type='pct_change_rate', measure_id = 3) %>%
  mutate(mean = mean*100, lower = lower*100, upper = upper*100) %>%
  select(cause_id, mean = mean, lower, upper)



############################################################
## TABLES
############################################################

##----------------------------------------------------------
## TABLE 1A-C
      
pud_gd_table <- function(measureID){
  
  data_raw <-
    get_outputs(
      "cause",
      cause_id = pud_gd$cause_id,
      sex_id = 3,
      year_id = c(year_ids[1], year_ids[length(year_ids)]),
      measure_id = measureID, 
      metric_id = c(1,3),
      age_group_id = c(22,27),
      location_id = c(1, ntl_locations$location_id, regions$location_id, super_regions$location_id),
      gbd_round_id = gbd_rnd, 
      decomp_step = gbd_decomp_step,
      compare_version_id = compare_version) %>% 
    filter(!(metric_id==1 & age_group_id==27) & !(metric_id==3 & age_group_id==22)) # removing age-standardized counts (not estimated, show up as NA) and all-ages rates (only want age-standardized rates)
    
  pct_change <- get_pct_change(gbd_id_type='cause_id', 
                               gbd_id=pud_gd$cause_id,
                               year_start_id=year_ids[1], 
                               year_end_id=year_ids[length(year_ids)],
                               location_id=c(1, ntl_locations$location_id, regions$location_id, super_regions$location_id), 
                               age_group_id=27, 
                               sex_id=3,
                               gbd_round_id=gbd_rnd, 
                               decomp_step=gbd_decomp_step, 
                               version_id = case_when(measureID==1 ~ cod_version,
                                                      measureID==2 ~ daly_version,
                                                      measureID==5 ~ como_version), 
                               source=case_when(measureID==1 ~ 'codcorrect', 
                                                measureID==2 ~ 'dalynator', 
                                                measureID==5 ~ 'como'), 
                               change_type='pct_change_rate', measure_id = measureID)
  
  pct_change_rows <- pct_change %>%
    left_join(location_data, by = "location_id") %>%
    mutate(acause = factor(if_else(cause_id==527, "pud", "gd"), levels = c("pud", "gd"))) %>%
    select(location_name, location_id, acause, mean, upper, lower) %>%
    pivot_longer(cols = c(mean, lower, upper), names_to = "mean_ui_type", values_to = "val") %>%
    mutate(val = sprintf("%.1f", round(val*100, 1))) %>%
    distinct(location_name, acause, mean_ui_type, val, .keep_all = TRUE) %>%
    pivot_wider(names_from = mean_ui_type, values_from = val) %>%
    mutate(val = paste0(mean, " (", lower, ", ", upper, ")")) %>%
    select(-mean, -lower, -upper) %>%
    pivot_wider(names_from = acause, names_prefix = "pct_change_", values_from = val)
    
  table_data <- data_raw %>% 
    select(location_name, location_id, acause, year_id, metric_name, mean = val, lower, upper) %>%
    pivot_longer(cols = c(mean, lower, upper), names_to = "mean_ui_type", values_to = "val") %>%
    mutate(metric_name = factor(metric_name, levels = c("Number", "Rate")),
           val = case_when(metric_name == "Number" ~ round(val, 0) %>% as.character(), 
                           TRUE ~ sprintf("%.1f", round(val*100000, 1))), 
           acause = factor(if_else(acause=="digest_pud", "pud", "gd"), levels = c("pud", "gd"))) %>% 
    arrange(location_name, acause, year_id, metric_name) %>%
    distinct(location_name, acause, year_id, metric_name, mean_ui_type, val) %>% # deduplicating South Asia and North Africa and Middle East rows. 
    pivot_wider(names_from = mean_ui_type, values_from = val) %>%
    mutate(val = paste0(mean, " (", lower, ", ", upper, ")")) %>%
    select(-mean, -lower, -upper) %>%
    pivot_wider(names_from = c(acause, year_id, metric_name), values_from = val) %>%
    left_join(pct_change_rows, by = c("location_name")) %>%
    left_join(location_data %>% select(location_id, sort_order), by = 'location_id') %>%
    arrange(sort_order) %>%
    select(-location_id, -sort_order)

  col_order <- c(names(table_data) %>% str_subset(., "pud"), names(table_data) %>% str_subset(., "gd"))
  write_csv(table_data %>% select(Location = location_name, col_order),
            path = paste0(out_path, "TABLE1_", data_raw$measure_name %>% unique(), ".csv"))
}

# Generate deaths, prevalence, DALYs tables      
for (measureID in c(1,2,5)) {
  pud_gd_table(measureID)
}
##----------------------------------------------------------


      ## OLD CODE to generate tables. Not used in GBD 2019 update. 
      #Massive table (plus text)
      deaths <- get_outputs(
        "cause",
        cause_id = pud_gd,
        sex_id = 3,
        year_id = c(1990, year_ids[length(year_ids)]),
        measure_id = 1,
        metric_id = 1,
        age_group_id = 22,
        location_id = "all",
        gbd_round_id = gbd_rnd, 
        decomp_step = gbd_decomp_step,
        compare_version_id = compare_version)
          
      death_table <- dcast(deaths, location_id ~ cause_id + year_id, value.var = c("val", "lower", "upper"))
          
      prev_table <- prev[age_group_id==22 & sex_id==3 & cause_id %in% pud_gd, ]
      prev_table <- dcast(prev_table, location_id ~ year_id + cause_id, value.var=c("val", "lower", "upper"))
      
      mass_table <- merge(death_table, prev_table, by="location_id")
      mass_table <- merge(mass_table, location_data, by="location_id")
          
      table_col_order <- c("location_name", "location_id", "val_1990", "lower_1990", "upper_1990", "val_2017", "lower_2017", "upper_2017", 
                           "val_1990_527", "lower_1990_527", "upper_1990_527", "val_2017_527", "lower_2017_527", "upper_2017_527", 
                           "val_1990_528", "lower_1990_528", "upper_1990_528", "val_2017_528", "lower_2017_528", "upper_2017_528",
                           "val_1990_536", "lower_1990_536", "upper_1990_536", "val_2017_536", "lower_2017_536", "upper_2017_536")
          
      table_col_keep = names(mass_table)[names(mass_table) %in% table_col_order]
      
      table_col_names = c("Location Name", "Location ID",
                          "Deaths in 1990", "LL 1990", "UL 1990", "Deaths in 2017", "LL 2017", "UL 2017", 
                          "PUD Prevalence 1990", "LL PUD 1990", "UL PUD 1990", "PUD Prevalence 2017", "LL PUD 2017", "UL PUD 2017", 
                          "G/D Prevalence 1990", "LL G/D 1990", "UL G/D 1990", "G/D Prevalence 2017", "LL G/D 2017", "UL G/D 2017",
                          "GERD Prevalence 1990", "LL GERD 1990", "UL GERD 1990", "GERD Prevalence 2017", "LL GERD 2017", "UL GERD 2017")
          
      mass_table <- mass_table[ , table_col_keep, with = FALSE]
      setcolorder(mass_table, table_col_order)
          
      write.table(mass_table, file=paste0(out_path, "ugi_table"), row.names = FALSE, col.names = table_col_names)
      

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
  
  # Format crosswalk version data based on source type; attribute all data to its corresponding country. 
  if (source_type == "claims") {
    cw_locs <- cw_version %>% 
      left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
      filter(clinical_data_type== "claims") %>% 
      mutate(country = substr(ihme_loc_id.y, 1, 3),
             ihme_loc_id = ihme_loc_id.y) %>%
      left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
      rename(., country_name = location_name.y, location_name = location_name.x)
  } else if (source_type == "inpatient") {
    cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
             ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        rename(., country_name = location_name.y, location_name = location_name.x)
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
          rename(., country_name = location_name.y, location_name = location_name.x)
      } else {message("Invalid source_type argument.")} 
  
  # Check to see if there are any sources to count for the given source type
  if (nrow(cw_locs) == 0) {
    message(paste0("cw_id ", cw_id, ", ", source_type, ": no sources of this data type."))
  } else {
  
    # Unique NID-location-year_start combinations [most granular locs]
    count_loc <- cw_locs %>% distinct(nid, year_start, location_name) %>%
      group_by(location_name) %>% 
      tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    count_loc_merged <- cw_locs %>% 
      distinct(nid, year_start, location_name) %>%
      pull(nid) %>%
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
    
    saveWorkbook(wb, paste0(counts_path, out_filename), overwrite = TRUE)
    
    # Compare to Central Comp counts [mainly used as diagnostic before writing merged_nid_count_adjustment() function - not relevant if CC doesn't provide counts.]
    cc_counts <- read_csv(paste0("/ihme/scratch/projects/rgud/upper_digest/pud_gd_topic_paper/source_counts/counts_from_CC/GBD 2019 custom PUD prevalence subnational-level counts for ", ifelse(source_type=="prev", "other", source_type), " NIDs.csv"))
    cc_counts %>% 
      left_join(count_loc, by = "location_name") %>% 
      rename(cc_count = source_count, rgud_count = n) %>% 
      mutate(cc_minus_rgud = cc_count - rgud_count, factor = cc_count/rgud_count) %>% # difference and proportion between Central Comp and RGUD counts
      write.csv(., paste0(counts_path, "vetting/", source_type, "_cc_rgud_comparison.csv"))
  }
}

# Run source counts function for PUD and GD      
for(id in c(12221, 9890)){ # crosswalk version IDs associated with bested model versions for PUD, GD respectively
  for(type in c("prev", "inpatient", "claims")){
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
  merged_comp_list <- read.csv(paste0(counts_path, "merged_nids_adjustment/merged_citation_component_records.csv")) %>%
    group_by(Merged.Citation.NID) %>%
    tally() %>%
    rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
  
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  if(!file.exists(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"))){
    message(paste0("No ", source_type, " NIDs for cw_id ", cw_id))
  } else {
  
    unadj_counts_subnat <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 1) 
    unadj_counts_country <- read.xlsx(paste0(counts_path, source_type, "_counts_cw_version_", cw_id, ".xlsx"), sheet = 2) 
    
    # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
    source_type_nids <- read.csv(paste0(counts_path, "merged_nids_adjustment/", ifelse(cw_id==12221, "pud", "gd"), "_", source_type, "_NID_merge_types.csv"))
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>%
      filter(!is.na(n_comp_nids))
    
    # Format crosswalk version data to prep for source counting 
    if (source_type == "claims") {
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "claims") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "inpatient") {
      cw_locs <- cw_version %>% 
        left_join(location_data %>% select(location_id, ihme_loc_id), by = "location_id") %>% 
        filter(clinical_data_type== "inpatient") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        rename(., country_name = location_name.y, location_name = location_name.x)
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
        rename(., country_name = location_name.y, location_name = location_name.x)
    } else {message("Invalid source_type argument.")} 
    
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs
    count_loc <- cw_locs %>% distinct(nid, year_start, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    
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
    s
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
