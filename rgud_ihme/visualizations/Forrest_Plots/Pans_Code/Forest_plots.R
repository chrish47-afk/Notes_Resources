####forest plots for RO SR
##goal: creat forest plots to compare the effect size across different outcome types


library(readxl)
library(forestplot)

#read data
bmi_obs_bundle_v43219_incl_Urupture <- read_excel("/mnt/team/rgud/priv/Maternal/pan/for_christian/forest_plots/bmi_obs_bundle_v43219_incl_Urupture.xlsx")
bmi_obs_bundle_v43219_incl_Urupture <- as.data.table(bmi_obs_bundle_v43219_incl_Urupture)


#check number of unique source
n_distinct(bmi_obs_bundle_v43219_incl_Urupture$nid)

#check the frequency table for different outcome types
table(bmi_obs_bundle_v43219_incl_Urupture$outcome_cat)
table(bmi_obs_bundle_v43219_incl_Urupture$outcome_cat_short)

#round the confidence interval for labrl creation
bmi_obs_bundle_v43219_incl_Urupture$alt_risk_lower <- round(bmi_obs_bundle_v43219_incl_Urupture$alt_risk_lower, 1)
bmi_obs_bundle_v43219_incl_Urupture$alt_risk_upper <- round(bmi_obs_bundle_v43219_incl_Urupture$alt_risk_upper, 1)

#create summary table for forest plots
summary_df <- bmi_obs_bundle_v43219_incl_Urupture %>%
  group_by(outcome_cat) %>%
  summarise(nid=nid,
            mean=mean, 
            lower=lower, 
            upper=upper, 
            outcome_cat=outcome_cat,
            label = paste(outcome_cat_short, nid, alt_risk_lower,alt_risk_upper,sep = " - ") )
summary_df<-subset(summary_df,nid!=530677)##exclude the source that CI is very wide


#create forest plots
plots<-NULL

##re order the data based on label
summary_df <- summary_df[order(summary_df$label), ]


plots<-forestplot(labeltext = summary_df$label, 
                  mean = summary_df$mean,
                  lower = summary_df$lower,
                  upper = summary_df$upper,
                  slab = summary_df$label,
                  order=order(summary_df$outcome_cat), 
                  #rows=c(3:9,14:34,39:41,46:49), #3-fac,4-com,21-mix,7-NR
                  #rows=c(46:49,39:41,14:34,3:9),
                  #rows=c(1:7,8:48,49:59,60:64, 65:106),
                  txt_gp = fpTxtGp(label=gpar(cex = 0.2)), # for label text size
                  xlog = FALSE, # if the data are log-transformed, change to TRUE
                  xlab = "relative risk of high BMI on obstructed labor",
                  col=fpColors(box="royalblue", line="darkblue", zero = "gray50"),
                  zero = 1) # to set the zero line at x = 1

plots
