
rm(list=ls())
library(openxlsx)
library(meta)
library(metafor)
library(openxlsx)

######################################################################################################################
# BMI
######################################################################################################################

# BMI
# A= Obstructed labor (with sufficient contraction) 
# B= Prolonged labor/Nonprogressive labor (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)
# C= Labor dystocia or dystocia (without specifying whether there were sufficient contraction, no temporal cut-offs specified)
# D= C-section due to dystocia 
# E= C-section due to sub-causes of obstructed labor (following a trial of labor)
# F= Uterine rupture 


#read data
df <- read.xlsx("/mnt/team/rgud/priv/Maternal/pan/for_christian/forest_plots/03082024/bmi_obs_bundle_v43219_incl_Urupture.xlsx")
#bmi_obs_bundle_v43219_incl_Urupture <- read_excel("/mnt/team/rgud/priv/Maternal/pan/for_christian/forest_plots/03082024/height_obs_bundle_v43478_incl_fistula.xlsx")

df <- as.data.table(df)

#check number of unique source
n_distinct(df$nid)

#check the frequency table for different outcome types
table(df$outcome_cat)
table(df$outcome_cat_short)

#round the confidence interval for labrl creation
df$alt_risk_lower <- round(df$alt_risk_lower, 1)
df$alt_risk_upper <- round(df$alt_risk_upper, 1)

#create summary table for forest plots
summary_df <- df %>%
  group_by(outcome_cat) %>%
  summarise(nid=nid,
            location_name=location_name,
            year_start=year_start,
            year_end=year_end,
            mean=mean, 
            lower=lower, 
            upper=upper, 
            outcome_cat=outcome_cat,
            #label = paste(nid," ", alt_risk_lower,"-",alt_risk_upper, ":",ref_risk_lower,"-",ref_risk_upper)
            label = paste(location_name,",", year_start, "to", year_end, ",", alt_risk_lower,"-",alt_risk_upper, ":",ref_risk_lower,"-",ref_risk_upper)
#            label = paste(outcome_cat_short, nid, alt_risk_lower,alt_risk_upper,sep = " - ")
            )
summary_df<-subset(summary_df,nid!=530677)##exclude the source that CI is very wide

unique(summary_df$outcome_cat)
summary_df <- summary_df <- summary_df %>% mutate(outcome_cat = recode(outcome_cat,
                          "obstructed labor reference defintion" = "Obstructed labor (with sufficient contraction)",
                          "FTP" = "Prolonged labor/Nonprogressive labor (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)",                         
                          "dystocia" = "Labor dystocia or dystocia (without specifying whether there were sufficient contraction, no temporal cut-offs specified)",
                          "c-section due to obstructed labor/dystocia" = "C-section due to dystocia",                          
                          "c-section due to subcauses" = "C-section due to sub-causes of obstructed labor (following a trial of labor)",
                          "uterine rupture" = "Uterine Rupture"                       
                          #.default = column1
                          )) # Keeps other values as is

# Convert the 'Category' column to a factor with levels specified by custom_order
custom_order_list <- c("Obstructed labor (with sufficient contraction)",
                       "Prolonged labor/Nonprogressive labor (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)",
                       "Labor dystocia or dystocia (without specifying whether there were sufficient contraction, no temporal cut-offs specified)",
                       "C-section due to dystocia",
                       "C-section due to sub-causes of obstructed labor (following a trial of labor)",
                       "Uterine Rupture"
                       )

summary_df$outcome_cat <- factor(summary_df$outcome_cat, levels = custom_order_list)

# Order the data frame by the 'Category' column
summary_df <- summary_df[order(summary_df$outcome_cat), ]

########################################
# Forest PLot
########################################
data_table <- summary_df
# Calculate the variance for the effect sizes assuming a normal distribution
#data_table$vi <- ((data_table$upper - data_table$mean) / qnorm(0.975)) ^ 2
data_table$vi <- ((data_table$upper - data_table$lower) / (2 * qnorm(0.975))) ^ 2

# Create the forest plot
pdf("bmi_obs_bundle_v43219_incl_Urupture_ForestPlot.pdf", width = 11, height = 16) # Adjust size as needed

# # Calculate data range and buffer
# data_min <- min(data_table$lower)
# data_max <- max(data_table$upper)
# buffer <- (data_max - data_min) * 0.1
# 
# # Adjust xlim dynamically
# xlim_min <- data_min - buffer
# xlim_max <- data_max + buffer


meta::forest(
  x = data_table$mean,
  ci.lb = data_table$lower,
  ci.ub = data_table$upper,
  slab = data_table$label,
  vi = data_table$vi,
  ref = 1, #0 or 1
  rows = rev(c(1:5, 10:16, 21:55, 60:71, 76:115, 120:126)), #5, 7, 35, 12, 40, 7
#  rows = c(1:7, 12:15, 20:32, 37:43, 48:82, 87:126),
#  rows - rev(c(126:87, 82:48, 43:37, 32:20, 15:12, 7:1)),
  xlab = "Effect Size",
  alim = c(0, 15), # Adjust axis limits as needed #c(min(data_table$lower), max(data_table$upper))
  #alim = c(xlim_min, xlim_max),
  #xlim=c(5, 10), # Extend limits to include the labels 
  # forrest() automatically calculates axis limits to fit the data, in some cases, manually setting these limits can help utilize the plot area more efficiently.
  cex=0.5, 
  ylim=c(-1, 130), #y-axis limits
  mlab="", 
  psize=1, 
  #header= "NID  alt_risk_lower-alt_risk_upper : ref_risk_lower-ref_risk_upper", #"Author(s) and Year"
  header = "Location Name, Year range, alt_risk_lower-alt_risk_upper : ref_risk_lower-ref_risk_upper"
  #xaxt = "n" # Suppress the default x-axis
)

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.50, font=4)

### add text for the subgroups
# Assuming that -2 is within the range of your x-axis
text_x_position <- -15.85  # Replace this with the x-coordinate where you want your text to start
text_y_positions <- rev(c(6, 17.5, 56, 73, 117, 127))  # y-coordinates for the text

# Text strings, already reversed
subgroup_labels <- c("Obstructed labor (with sufficient contraction)", #7
                     "Prolonged labor/Nonprogressive labor (without\n specifying whether there were sufficient\n contractions, but specifying temporal cut-offs)", #40
                     "Labor dystocia or dystocia (without specifying\n whether there were sufficient contraction,\n no temporal cut-offs specified)", #12
                     "C-section due to dystocia", #35
                     "C-section due to sub-causes of obstructed\n labor (following a trial of labor)", #7
                     "Uterine Rupture" #5
                     )

# Add the text to the plot
graphics::text(x = text_x_position, y = text_y_positions, labels = subgroup_labels, adj = 0)


#x is the x-coordinate where the text will start.
#y is a vector of y-coordinates where each piece of text will be placed.
#labels are the text labels to be placed at the coordinates.
#adj = 0 specifies that the text should be left-justified at the x coordinate.
#pos = 2 specifies that the text should be placed to the left of the specified x-coordinate (if the text is being cut off, try removing pos = 2 as it may not be necessary with adj = 0).

### switch to bold font
par(font=3)

### set par back to the original settings
par(op)

dev.off()


# rows <- summary_df %>% filter(grepl('Pro', outcome_cat)) %>% nrow()
# print(rows)

######################################################################################################################
# Height
######################################################################################################################
rm(list=ls())
library(openxlsx)
library(meta)
library(metafor)
library(openxlsx)

# Height
# A= Obstructed labor (with sufficient contraction)
# B= Prolonged labor/Nonprogressive labor  (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)
# C= Labor dystocia/dystocia  (without specifying whether there were sufficient contraction, no temporal cut-offs specified)
# D= C-section due to dystocia
# E= C-section dut to subcauses
# F= Fistula

#read data
df <- read.xlsx("/mnt/team/rgud/priv/Maternal/pan/for_christian/forest_plots/03082024/height_obs_bundle_v43478_incl_fistula.xlsx")
#bmi_obs_bundle_v43219_incl_Urupture <- read_excel("/mnt/team/rgud/priv/Maternal/pan/for_christian/forest_plots/03082024/height_obs_bundle_v43478_incl_fistula.xlsx")

df <- as.data.table(df)

#check number of unique source
n_distinct(df$nid)

#check the frequency table for different outcome types
table(df$outcome_cat)
table(df$outcome_cat_short)

#round the confidence interval for labrl creation
df$alt_risk_lower <- round(df$alt_risk_lower, 1)
df$alt_risk_upper <- round(df$alt_risk_upper, 1)

#create summary table for forest plots
summary_df <- df %>%
  group_by(outcome_cat) %>%
  summarise(nid=nid,
            location_name=location_name,
            year_start=year_start,
            year_end=year_end,
            mean=mean, 
            lower=lower, 
            upper=upper, 
            outcome_cat=outcome_cat,
            #label = paste(nid," ", alt_risk_lower,"-",alt_risk_upper, ":",ref_risk_lower,"-",ref_risk_upper)
            label = paste(location_name,",", year_start, "to", year_end, ",", alt_risk_lower,"-",alt_risk_upper, ":",ref_risk_lower,"-",ref_risk_upper)
            #            label = paste(outcome_cat_short, nid, alt_risk_lower,alt_risk_upper,sep = " - ")
  )
summary_df<-subset(summary_df,nid!=530692)##exclude the source that CI is very wide

unique(summary_df$outcome_cat)
summary_df <- summary_df <- summary_df %>% mutate(outcome_cat = recode(outcome_cat,
                                                                       "A-obstructed labor reference defintion" = "Obstructed labor (with sufficient contraction)",
                                                                       "B-FTP (specified temporal cut-offs, no info on contraction)" = "Prolonged labor/Nonprogressive labor (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)",
                                                                       "C-dystocia" = "Labor dystocia/dystocia (without specifying whether there were sufficient contraction, no temporal cut-offs specified)",
                                                                       "D-c-section due to obstructed labor/dystocia" = "C-section due to dystocia",
                                                                       "E-c-section due to subcauses" = "C-section due to subcauses",
                                                                       "F-fistula" = "Fistula",
                                                                       #.default = column1
)) # Keeps other values as is

# Height
# A= Obstructed labor (with sufficient contraction)
# B= Prolonged labor/Nonprogressive labor  (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)
# C= Labor dystocia/dystocia  (without specifying whether there were sufficient contraction, no temporal cut-offs specified)
# D= C-section due to dystocia
# E= C-section dut to subcauses
# F= Fistula

# Convert the 'Category' column to a factor with levels specified by custom_order
custom_order_list <- c("Obstructed labor (with sufficient contraction)",
                       "Prolonged labor/Nonprogressive labor (without specifying whether there were sufficient contractions, but specifying temporal cut-offs)",
                       "Labor dystocia/dystocia (without specifying whether there were sufficient contraction, no temporal cut-offs specified)",
                       "C-section due to dystocia",
                       "C-section due to subcauses",
                       "Fistula"
                       )

summary_df$outcome_cat <- factor(summary_df$outcome_cat, levels = custom_order_list)

# Order the data frame by the 'Category' column
summary_df <- summary_df[order(summary_df$outcome_cat), ]

########################################
# Forest PLot
########################################
data_table <- summary_df
# Calculate the variance for the effect sizes assuming a normal distribution
#data_table$vi <- ((data_table$upper - data_table$mean) / qnorm(0.975)) ^ 2
data_table$vi <- ((data_table$upper - data_table$lower) / (2 * qnorm(0.975))) ^ 2

# Create the forest plot
pdf("height_obs_bundle_v43478_incl_fistula_ForestPlot.pdf", width = 10, height = 7) # Adjust size as needed

# # Calculate data range and buffer
# data_min <- min(data_table$lower)
# data_max <- max(data_table$upper)
# buffer <- (data_max - data_min) * 0.1
# 
# # Adjust xlim dynamically
# xlim_min <- data_min - buffer
# xlim_max <- data_max + buffer


meta::forest(
  x = data_table$mean,
  ci.lb = data_table$lower,
  ci.ub = data_table$upper,
  slab = data_table$label,
  vi = data_table$vi,
  ref = 1, #0 or 1
  rows = rev(c(1:3, 8:11, 16:18, 23:29, 34:39, 44)), #3, 4, 3, 7, 6, 1
  #  rows = c(1:7, 12:15, 20:32, 37:43, 48:82, 87:126),
  #  rows - rev(c(126:87, 82:48, 43:37, 32:20, 15:12, 7:1)),
  xlab = "Effect Size",
  alim = c(0, 40), # Adjust axis limits as needed #c(min(data_table$lower), max(data_table$upper))
  #alim = c(xlim_min, xlim_max),
  #xlim=c(5, 10), # Extend limits to include the labels 
  # forrest() automatically calculates axis limits to fit the data, in some cases, manually setting these limits can help utilize the plot area more efficiently.
  cex=0.5, 
  ylim=c(-1, 48), #y-axis limits
  mlab="", 
  psize=1, 
  header = "Location Name, Year range, alt_risk_lower-alt_risk_upper : ref_risk_lower-ref_risk_upper"
  #xaxt = "n" # Suppress the default x-axis
)

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.50, font=4)

### add text for the subgroups
# Assuming that -2 is within the range of your x-axis
text_x_position <- -39.5  # Replace this with the x-coordinate where you want your text to start
text_y_positions <- rev(c(4, 12, 19, 31, 41, 45))  # y-coordinates for the text

# Text strings, already reversed
subgroup_labels <- c("Obstructed labor (with sufficient contraction)", #1
                       "Prolonged labor/Nonprogressive labor (without\n specifying whether there were sufficient contractions,\n but specifying temporal cut-offs)", #6
                       "Labor dystocia/dystocia (without specifying\n whether there were sufficient contraction,\n no temporal cut-offs specified)", #7
                       "C-section due to dystocia", #3
                       "C-section due to subcauses", #4
                       "Fistula" #3
                     )

# Add the text to the plot
graphics::text(x = text_x_position, y = text_y_positions, labels = subgroup_labels, adj = 0)


#x is the x-coordinate where the text will start.
#y is a vector of y-coordinates where each piece of text will be placed.
#labels are the text labels to be placed at the coordinates.
#adj = 0 specifies that the text should be left-justified at the x coordinate.
#pos = 2 specifies that the text should be placed to the left of the specified x-coordinate (if the text is being cut off, try removing pos = 2 as it may not be necessary with adj = 0).

### switch to bold font
par(font=3)

### set par back to the original settings
par(op)

dev.off()


#rows <- summary_df %>% filter(grepl('Pro', outcome_cat)) %>% nrow()
#print(rows)
