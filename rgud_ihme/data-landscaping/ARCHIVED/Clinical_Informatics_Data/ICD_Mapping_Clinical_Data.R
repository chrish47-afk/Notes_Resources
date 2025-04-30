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

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

dt <- read.csv('/ihme/RTs_and_Projects/GBD/Teams/RGUD/Gyn materials/Spring 2021_Exploring Bundle 409/Other Gyne Bundle 409 ICD9.csv')
dt <- read.csv('')

library(dplyr)
#library(plyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(plyr)
library(RColorBrewer)

# Sourcing in Mohsen ICD Mapping Files
ICD9_condensed_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/Condense Maps/icd9_gbd2020_map.xlsx")
ICD10_condensed_table <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/Condense Maps/icd10_gbd2020_map.xlsx")

# Mutating acause column to lowercase and subsetting to only include icd mappings for maternal subcauses
ICD9_maternal <- ICD9_condensed_table %>% mutate(acause = tolower(acause)) %>% filter(grepl("maternal_", acause) & !grepl("iran", icd_code)) 
ICD10_maternal <- ICD10_condensed_table %>% mutate(acause = tolower(acause)) %>% filter(grepl("maternal_", acause) & !grepl("iran", icd_code))
#Removing period from icd_code column or else it will get counted as a character - we only want to count digits(characters) for icd_length.
ICD9_maternal$icd_code <- gsub("\\.","",as.character(ICD9_maternal$icd_code))
ICD10_maternal$icd_code <- gsub("\\.","",as.character(ICD10_maternal$icd_code))

# Creating digit lenth column
ICD9_maternal <- ICD9_maternal %>% mutate(digits = nchar(icd_code)) %>% select(c(-'package_name',-'package_description'))
ICD10_maternal <- ICD10_maternal %>% mutate(digits = nchar(icd_code)) %>% select(c(-'package_name',-'package_description'))

# Creating a count column for unique digit length and cleaning up the tables for Graphing!
ICD9_maternal_table_graph <- ICD9_maternal %>% select(acause, digits) %>% group_by(acause, digits) %>% tally() %>% dplyr::rename('count' = 'n') %>% dplyr::rename("icd_length" = "digits")
ICD10_maternal_table_graph <- ICD10_maternal %>% select(acause, digits) %>% group_by(acause, digits) %>% tally() %>% dplyr::rename('count' = 'n') %>% dplyr::rename("icd_length" = "digits")

# Graphing - Histograms
ICD9_maternal_table <- ICD9_maternal_table_graph %>% group_by(acause) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
ICD10_maternal_table <- ICD10_maternal_table_graph %>% group_by(acause) %>% summarise_all(funs(paste(na.omit(.), collapse = ","))) #%>% filter(!"maternal_other" %in% acause & !"maternal_indirect" %in% acause & !"maternal_late" %in% acause )
write.xlsx(ICD9_maternal_table, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/ICD9_count_summary.xlsx")
write.xlsx(ICD10_maternal_table, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/ICD10_count_summary.xlsx")


##################### Stacked Histogram #####################
ICD9_maternal_final <- ICD9_maternal_table_graph %>% filter(!"maternal_other" %in% acause & !"maternal_indirect" %in% acause)
h1 <- ggplot(ICD9_maternal_final, 
       aes(x = acause,
           y = count,
           fill = factor(icd_length),
           label = count)) + 
  geom_bar(stat = "identity") +
  #geom_text(size = 1, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  labs(title = "ICD Length for Maternal causes for ICD9", subtitle = "From Mohsen ICD Condensed Tables", x='Cause', y = 'Count(Code Names)', fill = 'ICD length (Digits)') +
  theme(axis.text.x=element_text(size=6))


ICD10_maternal_final <- ICD10_maternal_table_graph %>% filter(!"maternal_other" %in% acause & !"maternal_indirect" %in% acause & !"maternal_late" %in% acause)
h2 <- ggplot(ICD10_maternal_final, 
       aes(x = acause,
           y = count,
           fill = factor(icd_length),
           label = count)) + 
  geom_bar(stat = "identity") +
  #geom_text(size = 1, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  labs(title = "ICD Length for Maternal causes for ICD10", subtitle = "From Mohsen ICD Condensed Tables", x='Cause', y = 'Count(Code names)', fill = 'ICD length (Digits)') +
  theme(axis.text.x=element_text(size=6))


savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/"
file_name <- "ICD Length Stacked Histograms.pdf"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)

grid.arrange(h1, h2, nrow = 1)

ggsave(filename = pdf_path, width = 10, height = 5)
dev.off()
##################################################################################################################################################
### Histogram 2 ##
## This was created for testing purposes ##
kenya_133665 <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/Code_files/133665_codes.xlsx")
armenia_432266 <- read.csv("/home/j/DATA/Incoming Data/ARM/COLLABORATOR_DATA/HOSPITAL_DATA/Armenia_aggregated_2016.csv") %>% filter(grepl("O",Diagnosecode))
mohsen_map_icd10 <- read.xlsx("/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/Condense Maps/icd10_gbd2020_map.xlsx")

# y-axis (Sources)
# x-axis (ICD codes) / counts

# Since we are doing only only one source for testing purposes, then we would expect a single bar?
mohsen_map_icd10_codesonly <- mohsen_map_icd10 %>% select(acause, icd_code)
kenya_133665_edit <- kenya_133665 %>% dplyr::rename("icd_code" = "CODE") %>% select(icd_code) %>% mutate(id = 133665) %>% unique()  %>% group_by(icd_code, id) %>% tally()
armenia_432266_edit <- armenia_432266 %>% dplyr::rename("icd_code" = "Diagnosecode") %>% select(icd_code) %>% mutate(id = 432266) %>% unique() %>% group_by(icd_code, id) %>% tally()

# Join
stacked <- rbind(kenya_133665_edit, armenia_432266_edit)
joined_table <-left_join(stacked, mohsen_map_icd10_codesonly, by = 'icd_code')
joined_table <- joined_table %>% filter(acause != "_gc" & acause != "maternal_indirect" & acause != "maternal_other") 


#colourCount = length(unique(mtcars$hp))
#getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(joined_table,
       aes(x = factor(id),
           y = n,
           fill = icd_code #factor()
           )) +
  geom_bar(stat = 'identity', colour = "white") +
  labs(title = "ICD Code Distribution by Source", subtitle = "Using Mohsen's Map ICD10 Key Codes & Native Source Code Distribution", x='Source, NID', y = 'Counts', fill = 'ICD Codes(Maternal Only)') +
  theme(axis.text.x=element_text(size=8)) +
  scale_fill_discrete(h = c(240, 10))

# Saving PDF
savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/"
file_name <- "ICD code Distribution by Source.pdf"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 10, height = 5)

ggsave(filename = pdf_path, width = 10, height = 7)
dev.off()

##################################################################################################################################################
### Histogram 3 ##
library(fuzzyjoin)
library(readxl) # to read .xls files!
kenya_133665 <- read_excel("/home/j/DATA/KEN/HOSPITAL/KEN_1999_HOSPITAL_INPATIENT_MORBIDITY_MORTALITY.XLS", skip = 3) %>% select(CODE) %>% filter(grepl("O", CODE))
armenia_432266 <- read.csv("/home/j/DATA/Incoming Data/ARM/COLLABORATOR_DATA/HOSPITAL_DATA/Armenia_aggregated_2016.csv") %>% filter(grepl("O",Diagnosecode))
#####################################################
## ICD19 Parent Codes
# O00-O08  Pregnancy with abortive outcome
# O09-O09  Supervision of high risk pregnancy
# O10-O16  Edema, proteinuria and hypertensive disorders in pregnancy, childbirth and the puerperium
# O20-O29  Other maternal disorders predominantly related to pregnancy
# O30-O48  Maternal care related to the fetus and amniotic cavity and possible delivery problems
# O60-O77  Complications of labor and delivery
# O80-O82  Encounter for delivery
# O85-O92  Complications predominantly related to the puerperium
# O94-O9A  Other obstetric conditions, not elsewhere classified
a <- data.table(c('O00', 'O01', 'O02', 'O03', 'O04', 'O05', 'O06', 'O07', 'O08'), 'O00-O08')
b <- data.table('O09', 'O09-O09')
c <- data.table(c('O10', 'O11', 'O12', 'O13', 'O14', 'O15', 'O16'), 'O10-O16')
d <- data.table(c('O20', 'O21', 'O22', 'O23', 'O24', 'O25', 'O26', 'O27', 'O28', 'O29'), 'O20-O29')
e <- data.table(c('O30', 'O31', 'O32', 'O33', 'O34', 'O35', 'O36', 'O37', 'O38', 'O39', 'O40', 'O41', 'O42', 'O43', 'O44', 'O45', 'O46', 'O47', 'O48'), 'O30-O48')
f <- data.table(c('O60', 'O61', 'O62', 'O63', 'O64', 'O65', 'O66', 'O67', 'O68', 'O69', 'O70', 'O71', 'O72', 'O73', 'O74', 'O75', 'O76', 'O77'), 'O60-O77')
g <- data.table(c('O80', 'O81', 'O82'), 'O80-O82')
h <- data.table(c('O85', 'O86', 'O87', 'O88', 'O89', 'O90', 'O91', 'O92'), 'O85-O92')
i <- data.table(c('O94', 'O98', 'O99', 'O9A'), 'O94-O9A')
parent_codes <- rbind(a, b, c, d, e, f, g, h, i) %>% dplyr::rename('icd_code' = 'V1', 'parent_id' = 'V2')
parent_id <- c('O00-O08', 'O09-O09', 'O10-O16', 'O20-O29', 'O30-O48', 'O60-O77', 'O80-O82', 'O85-O92', 'O94-O9A')
key_codes <- data.table(parent_id = parent_id) 
#####################################################

#mohsen_map_icd10_codesonly <- mohsen_map_icd10 %>% select(acause, icd_code)
kenya_133665_edit <- kenya_133665 %>% dplyr::rename("icd_code" = "CODE") %>% unique()
armenia_432266_edit <- armenia_432266 %>% dplyr::rename("icd_code" = "Diagnosecode") %>%  select(icd_code) %>% unique()

armenia_432266_join <- regex_left_join(armenia_432266_edit, parent_codes, by = 'icd_code')
armenia_432266_join <- regex_left_join(key_codes, armenia_432266_join, by = 'parent_id') %>% mutate(count =ifelse(is.na(icd_code.y) == TRUE, 0, 1))

kenya_133665_join <- regex_left_join(kenya_133665_edit, parent_codes, by = 'icd_code')
kenya_133665_join <- regex_left_join(key_codes, kenya_133665_join, by = 'parent_id') %>% mutate(count =ifelse(is.na(icd_code.y) == TRUE, 0, 1))

h1 <- ggplot(armenia_432266_join, 
             aes(x = parent_id.x,
                 y = count,
                 fill = parent_id.x,
                 )) + 
  geom_bar(stat = "identity") + #fill =
  labs(title = "ICD Code Distribution for source NID: 432266 (Armenia)", x='Parent Code Distribution', y = 'Counts') +
  scale_fill_discrete(name = '', labels = c('O00-O08' = "O00-O08: Pregnancy with abortive outcome", 'O09-O09'= "O09-O09: Supervision of high risk pregnancy", 'O10-O16'= "O10-O16: Edema, proteinuria and hypertensive disorders in pregnancy, childbirth and the puerperium", 'O20-O29' = "O20-O29: Other maternal disorders predominantly related to pregnancy", 'O30-O48' = "O30-O48: Maternal care related to the fetus and amniotic cavity and possible delivery problems", 'O60-O77' = "O60-O77: Complications of labor and delivery", 'O80-O82' = "O80-O82: Encounter for delivery", 'O85-O92' = "O85-O92: Complications predominantly related to the puerperium", 'O94-O9A' = "O94-O9A: Other obstetric conditions, not elsewhere classified")) +
  theme(axis.text.x=element_text(size=9),axis.title = element_text(size = 10) , legend.text = element_text(size = 10), legend.position="bottom") + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

h2 <- ggplot(kenya_133665_join, 
             aes(x = parent_id.x,
                 y = count,
                 fill = parent_id.x,
             )) + 
  geom_bar(stat = "identity") + #fill =
  labs(title = "ICD Code Distribution for source NID: 133665 (Kenya)", x='Parent Code Distribution', y = 'Counts') +
  scale_fill_discrete(name = '', labels = c('O00-O08' = "O00-O08: Pregnancy with abortive outcome", 'O09-O09'= "O09-O09: Supervision of high risk pregnancy", 'O10-O16'= "O10-O16: Edema, proteinuria and hypertensive disorders in pregnancy, childbirth and the puerperium", 'O20-O29' = "O20-O29: Other maternal disorders predominantly related to pregnancy", 'O30-O48' = "O30-O48: Maternal care related to the fetus and amniotic cavity and possible delivery problems", 'O60-O77' = "O60-O77: Complications of labor and delivery", 'O80-O82' = "O80-O82: Encounter for delivery", 'O85-O92' = "O85-O92: Complications predominantly related to the puerperium", 'O94-O9A' = "O94-O9A: Other obstetric conditions, not elsewhere classified")) +
  theme(axis.text.x=element_text(size=9),axis.title = element_text(size = 10) , legend.text = element_text(size = 10), legend.position="bottom") +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))


# Putting plots together
savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/"
file_name <- "ICD Code Distribution, ICD10.pdf"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 12)

grid.arrange(h1, h2, nrow = 2)

ggsave(filename = pdf_path, width = 20, height = 12)
dev.off()

##################################################################################################################################################
### Histogram 4 ##
library(fuzzyjoin)
library(readxl) # to read .xls files!
kenya_133665 <- read_excel("/home/j/DATA/KEN/HOSPITAL/KEN_1999_HOSPITAL_INPATIENT_MORBIDITY_MORTALITY.XLS", skip = 3) %>% select(CODE) %>% filter(grepl("O", CODE))
armenia_432266 <- read.csv("/home/j/DATA/Incoming Data/ARM/COLLABORATOR_DATA/HOSPITAL_DATA/Armenia_aggregated_2016.csv") %>% filter(grepl("O",Diagnosecode))
#####################################################
## ICD19 Parent Codes
#kenya_133665_edit <- kenya_133665 %>% dplyr::rename("icd_code" = "CODE") %>% select(icd_code) %>% group_by(icd_code) %>% tally()
kenya_133665_edit <- gsub("\\.","",as.character(kenya_133665$CODE)) %>% as.data.table 
kenya_133665_edit <- kenya_133665_edit %>% mutate(icd_code = tolower(.)) %>% mutate(digits = nchar(.)) %>% select(2:3) 
kenya_133665_edit <- kenya_133665_edit %>% group_by(icd_code,digits) %>% tally()

armenia_432266_edit <- armenia_432266 %>% dplyr::rename("icd_code" = "Diagnosecode") %>% select(icd_code) %>% group_by(icd_code) %>% tally()
armenia_432266_edit <- gsub("\\.","",as.character(armenia_432266$Diagnosecode)) %>% as.data.table 
armenia_432266_edit <- armenia_432266_edit %>% mutate(icd_code = tolower(.)) %>% mutate(digits = nchar(.)) %>% select(2:3)
armenia_432266_edit <- armenia_432266_edit %>% group_by(icd_code, digits) %>% tally()

# Histogram Code that Graphs the ICD code distribution per source at the most detailed level!
h1 <- ggplot(armenia_432266_edit, aes(x = icd_code, y = n, fill = factor(digits))) +
  geom_bar(stat = "identity") +
  labs(title = "Unique ICD Code Distribution by Subject(Inpatients) for source NID: 432266 (Armenia)", x='Unique ICD Codes', y = 'Subjects', fill = 'ICD Code Length') +
  theme(axis.text = element_text(face="bold"))

h2 <- ggplot(kenya_133665_edit, aes(x = icd_code, y = n, fill = factor(digits)))+
  geom_bar(stat = "identity") +
  labs(title = "Unique ICD Code Distribution by Subject(Inpatients) for source NID: 133665 (Kenya)", x = "Unique ICD Codes", y = "Subjects", fill = 'ICD Code Length') +
  theme(axis.text = element_text(face="bold")) + 
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(breaks = 1) # Names of axis can also be included here !



savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/"
file_name <- "Unique ICD Code Distribution by Subject, ICD10.pdf"
pdf_path = paste0(savedir, file_name)
pdf(file = pdf_path, width = 20, height = 12)

grid.arrange(h1, h2, nrow = 2)

ggsave(filename = pdf_path, width = 20, height = 12)
dev.off()


# Function
histogram_ICD <- function(dt, nid){
  # Data Input -- Transforming the ICD10 code column #ICD9 are only numbers!
  # The most common characters to remove are: ".", ",","-", and "X" in some occasions
  names(dt)[1] <- 'CODE' #Changing the dt column name
  dt <- as.data.table(dt)
  
  colname <- colnames(dt) #
  dt_periods <- dt %>% filter(grepl(".", CODE)) #Do colname
  dt_X <- dt %>% filter(grepl("X", CODE)) #Usually X is a placeholder for ICD codes. #Confirm! #This one can be removed for Maternal!
  dt_comma <- dt %>% filter(grepl(",", CODE))
  dt_hyphen <- dt %>% filter(grepl(",", CODE))
  if(nrow(dt_periods) != 0) {
    dt <- gsub("\\.","",as.character(dt$CODE)) %>% as.data.table()
  } else if(nrow(dt_X) != 0) {
    dt <- gsub("\\X","",as.character(dt$CODE)) %>% as.data.table()
  } else if(nrow(dt_comma) != 0) {
    dt <- gsub("\\,","",as.character(dt$CODE)) %>% as.data.table()
  } else if(nrow(dt_hyphen) != 0){
    dt <- gsub("\\-","",as.character(dt$CODE)) %>% as.data.table()
  }

  dt <- dt %>% mutate(icd_code = tolower(.)) %>% mutate(digits = nchar(.)) %>% select(2:3)
  dt <- dt %>% group_by(icd_code, digits) %>% tally() %>% mutate(icd_code = toupper(icd_code))
  
  # Histogram
  h <- ggplot(dt, aes(x = icd_code, y = n, fill = factor(digits))) +
    geom_bar(stat = "identity") +
    labs(title = paste0("Unique ICD Code Distribution by Subjects for source: ", nid), x = "Unique ICD Codes", y = "Subjects", fill = "ICD Code Length") +
    theme(axis.text = element_text(face = "bold"), axis.title = element_text(size = 12), legend.text = element_text(size = 12)) +
    scale_x_discrete(guide = guide_axis(angle = 30)) +
    scale_y_continuous(breaks = 1) #Name of the axix(y/x) can also be included here instead)
  
  savedir <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/ICD_Mappings/ICD_HISTOGRAMS/"
  file_name <- paste0("ICD_Code_Distribution_",nid,".pdf")
  pdf_path = paste0(savedir, file_name)
  grid.arrange(h) #Multiple graphs can be added here if necessary! Review grid.arrange() as necessary.
  ggsave(filename = pdf_path, width = 20, height = 10)
  dev.off()
  
}
# dt_test <- dt %>% gsub("\\.","",as.character(dt$CODE)) %>% gsub("\\-","",as.character(dt$CODE)) %>% as.data.table() %>% as.data.table()
# # Alternative to extracting numbers only?
# #https://stackoverflow.com/questions/38944600/remove-everything-except-period-and-numbers-from-string-regex-in-r
# dt_c <- substr(dt$CODE,1,1) %>% as.data.table() #This extracts only the first character(Code Letter)
# dt_n <- dt %>% as.data.frame() %>% mutate(code_numbers = parse_number(`CODE`))

readLines("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2009/ECU_HOSPITAL_DISCHARGES_2009.DAT", n=500)

dt0 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2000/ECU_HOSPITAL_DISCHARGES_2000.DAT", header=FALSE, skip = 1)
dt0 <- dt1 %>% filter(grepl('O',V27)) %>% select(V27)
dt1 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2001/ECU_HOSPITAL_DISCHARGES_2001.DAT", header=FALSE, skip = 1)
dt1 <- dt2 %>% filter(grepl('O',V27)) %>% select(V27)
dt2 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2002/ECU_HOSPITAL_DISCHARGES_2002.DAT", header=FALSE, skip = 1)
dt2 <- dt3 %>% filter(grepl('O',V27)) %>% select(V27)
dt3 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2003/ECU_HOSPITAL_DISCHARGES_2003.DAT", header=FALSE, skip = 1)
dt3 <- dt4 %>% filter(grepl('O',V27)) %>% select(V27)
dt4 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2004/ECU_HOSPITAL_DISCHARGES_2004.DAT", header=FALSE, skip = 1)
dt4 <- dt5 %>% filter(grepl('O',V27)) %>% select(V27)
dt5 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2005/ECU_HOSPITAL_DISCHARGES_2005.DAT", header=FALSE, skip = 1)
dt5 <- dt6 %>% filter(grepl('O',V27)) %>% select(V27)
dt6 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2006/ECU_HOSPITAL_DISCHARGES_2006.DAT", header=FALSE, skip = 1)
dt6 <- dt7 %>% filter(grepl('O',V27)) %>% select(V27)
dt7 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2007/ECU_HOSPITAL_DISCHARGES_2007.DAT", header=FALSE, skip = 1)
dt7 <- dt8 %>% filter(grepl('O',V27)) %>% select(V27)
dt8 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2008/ECU_HOSPITAL_DISCHARGES_2008.DAT", header=FALSE, skip = 1)
dt8 <- dt9 %>% filter(grepl('O',V27)) %>% select(V27)
dt9 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2009/ECU_HOSPITAL_DISCHARGES_2009.DAT", header=FALSE, skip = 2)
dt9 <- dt10 %>% filter(grepl('O',V27)) %>% select(V27)
dt10 <- read.table("/home/j/DATA/ECU/HOSPITAL_DISCHARGES/2010/ECU_HOSPITAL_DISCHARGES_2010.DAT", header=FALSE, skip = 1)
dt10 <- dt11 %>% filter(grepl('O',V27)) %>% select(V27)

dt <- read.csv("/home/j/DATA/GBR/ENGLAND_HOSPITAL_EPISODE_STATISTICS/GBR_ENGLAND_HES_2001_2014_BY_AGE_SEX_CAUSE_UTLA_Y2016M10D10.CSV")


histogram_ICD(dt = dt0, nid = 87000)
histogram_ICD(dt = dt1, nid = 87001)
histogram_ICD(dt = dt2, nid = 87002)
histogram_ICD(dt = dt3, nid = 87003)
histogram_ICD(dt = dt4, nid = 87004)
histogram_ICD(dt = dt5, nid = 87005)
histogram_ICD(dt = dt6, nid = 87006)
histogram_ICD(dt = dt7, nid = 87007)
histogram_ICD(dt = dt8, nid = 87008)
#histogram_ICD(dt = dt9, nid = 87009)
histogram_ICD(dt = dt10, nid = 87010)
#histogram_ICD(dt = armenia_432266, nid = 432266)
#histogram_ICD(dt = kenya_133665, nid = 133665)
##############################################################################################################################################
##################################################################################################################################
##################################################################################################################################
# Clinical Data Review Analysis
##################################################################################################################################
##################################################################################################################################

dt_19694 <- get_crosswalk_version(19694)
#write.xlsx(dt_19694, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_19694.xlsx")
dt_20036 <- get_crosswalk_version(20036)
#write.xlsx(dt_20036, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20036.xlsx")
dt_20552 <- get_crosswalk_version(20552)
#write.xlsx(dt_20552, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20552.xlsx")
dt_20792 <- get_crosswalk_version(20792)
#write.xlsx(dt_20792, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20792.xlsx")
dt_20033 <- get_crosswalk_version(20033)
#write.xlsx(dt_20033, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20033.xlsx")
dt_20900 <- get_crosswalk_version(20900)
#write.xlsx(dt_20900, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20900.xlsx")
dt_29777 <- get_crosswalk_version(29777)
#write.xlsx(dt_29777, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_29777.xlsx")
dt_17186 <- get_crosswalk_version(17186)
#write.xlsx(dt_17186, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_17186.xlsx")
dt_20465 <- get_crosswalk_version(20465)
#write.xlsx(dt_20465, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20465.xlsx")
dt_20810 <- get_crosswalk_version(20810)
#write.xlsx(dt_20810, "/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/crosswalk_20810.xlsx")

# # Pulling all crosswalk data bins #This reads all the .xlsx from this directory
# setwd("/ihme/scratch/users/chrish47/source_counts_mb/subcauses_JulyMeeting/crosswalk_data_bins/")
# file.list <- list.files(pattern='*.xlsx')
# df.list <- lapply(file.list, read_excel)

# Unique Metrics for Clinical Data Review
dt_19694_edit <- dt_19694 %>% select(nid, field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20036_edit <- dt_20036 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20552_edit <- dt_20552 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20792_edit <- dt_20792 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20033_edit <- dt_20033 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20900_edit <- dt_20900 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_29777_edit <- dt_29777 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
#dt_17186_edit <- dt_17186 %>% filter(!clinical_data_type %in% "") %>% select(nid, source_type, location_id, location_name, year_start, year_end, clinical_data_type)
dt_20465_edit <- dt_20465 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")
dt_20810_edit <- dt_20810 %>% select(nid,field_citation_value, source_type, location_id, location_name, year_start, year_end, clinical_data_type, extractor) %>% filter(clinical_data_type != "") %>% filter(clinical_data_type != "literature")


# Cause Combine
# Maternal Hypertensive Disorders
dt_mh <- dt_19694_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Hemorrhage' = 'nid')
dt_hd <- rbind(dt_20792_edit, dt_20033_edit, dt_20900_edit) %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Hypertensive' = 'nid')
dt_i <- rbind(dt_20036_edit, dt_20552_edit) %>% select(nid) %>% unique() #%>% dplyr::rename('Maternal_Infections' = 'nid')
dt_of <- dt_29777_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Obstruction' = 'nid') # No Fistula Clinical Sources
dt_e <- dt_20810_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Ectopic' = 'nid')
dt_a <- dt_20465_edit %>% select(nid) %>% unique() #%>% dplyr::rename('Materan_Abortion' = 'nid')
###########
output <- "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/"
out_filename <- "unique_nids_bycause.xlsx"
wb <- createWorkbook(title = out_filename)

addWorksheet(wb, "Maternal Hypertensive Disorders")
writeData(wb, "Maternal Hypertensive Disorders", dt_hd)

addWorksheet(wb, "Maternal Hemorrhage")
writeData(wb, "Maternal Hemorrhage", dt_hd)

addWorksheet(wb, "Maternal Infection")
writeData(wb, "Maternal Infection", dt_i)

addWorksheet(wb, "Maternal Obstruction")
writeData(wb, "Maternal Obstruction", dt_of)

addWorksheet(wb, "Maternal Ectopic")
writeData(wb, "Maternal Ectopic", dt_e)

addWorksheet(wb, "Maternal Abortion")
writeData(wb, "Maternal Abortion", dt_a)

saveWorkbook(wb, paste0(output, "/", out_filename), overwrite = TRUE)
###########


# # Whole Combine
# nid <- c(413916,125126,124938,125036,125037,120469,109943,120471,124877,109946,124884,125084,125125,125039,124935,413950)
# table_nids <- data.table(nid)
# dt_combine <- rbind(dt_19694_edit, dt_20036_edit, dt_20552_edit, dt_20792_edit, dt_20033_edit, dt_20900_edit, dt_29777_edit, dt_20465_edit, dt_20810_edit) %>% left_join(location_data %>% dplyr::select(location_id, location_type), by='location_id')
# write.xlsx(dt_combine, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/All_Data_Bins_Clinical_crosswalk.xlsx")
# 
# dt_combine2 <- rbind(dt_20930, dt_20945, dt_20948, dt_20933, dt_20936, dt_21185, dt_29876, dt_20942, dt_20951) %>% left_join(location_data %>% dplyr::select(location_id, location_type), by='location_id')
# write.xlsx(dt_combine2, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/All_Data_Bins_Clinical_bundleversion.xlsx")
# 
# crosswalk_nids <- left_join(table_nids, dt_combine, by = 'nid') %>% unique
# write.xlsx(crosswalk_nids, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nids_of_interest_xwalk.xlsx")
# bundleversion_nids <- left_join(table_nids, dt_combine2, by = 'nid') %>% unique
# write.xlsx(bundleversion_nids, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nids_of_interest_bv.xlsx")
# 
# # Unique Subnationals
# dt_combine_subnationals <- dt_combine %>% select(nid,location_type) %>% unique()
# write.xlsx(dt_combine_subnationals, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nid_locationtypes.xlsx")
# 
# # Unique Years
# dt_combine_years <- dt_combine %>% select(nid, year_start, year_end) %>% unique() %>% setorder(nid, year_start, year_end)
# write.xlsx(dt_combine_years, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique/unique_nid_years.xlsx")


### Unique NIDs ### Generating Component NIDs as well from Merged NIDs
dt_combine_nids <- rbind(dt_19694_edit, dt_20036_edit, dt_20552_edit, dt_20792_edit, dt_20033_edit, dt_20900_edit, dt_29777_edit, dt_20465_edit, dt_20810_edit) %>% select(nid) %>% unique()
write.xlsx(dt_combine_nids,"/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/unique_clinical_nids.xlsx")

location_collapsed <- location_data %>% select(location_id, location_type)
dt_combine_nids <- left_join(dt_combine_nids, location_collapsed, by = 'location_id') 
write.xlsx(dt_combine_nids,'/ihme/scratch/users/chrish47/unique_nids_locations.xlsx')
# nid input for merged nids (THIST LIST OF NIDS INCLUDES RECORDS AND MERGED NIDS)
# The records can be vetted, but merged nids need to be further broken down by component NIDS - STEP BELOW!
nids_input <- c(3822, 234672, 234758, 287201, 234771, 234740, 331084, 68367, 133665, 234745, 234692, 234764, 284439, 67132, 432298, 234761, 234774, 234742, 292575, 284422, 234747, 234703, 234750, 336851, 334465, 234765, 284442, 234670, 234766, 104241, 234671, 234769, 234738, 104246, 432265, 234674, 433061, 287203, 287207, 411100, 234760, 234772, 234741, 284421, 234746, 234693, 334464, 234762, 284440, 292574, 432266, 321359, 407536, 284419, 433059, 285520, 433115, 408336, 430710, 287204, 411786, 317423, 292437, 337619, 404395, 206640, 234704, 337129, 281819, 336852, 334466, 421046, 354896, 292577, 284444, 411787, 234673, 287202, 68535, 299375, 433060, 422874, 422318, 421047, 433114, 431674, 336847, 408680, 406980, 397812, 397813, 397814, 244369, 244370, 336850, 244371, 336849, 336848, 336203, 336846, 336843, 336838, 336833, 336827, 336825, 336824, 336817, 336845, 336844, 336842, 336841, 336840, 336839, 336837, 336836, 336835, 336834, 336832, 336831, 336830, 336829, 336828, 336826, 336822, 336820, 413916, 125126, 124938, 125036, 125037, 120469, 109943, 120471, 124877, 109946, 124884, 439735, 439739, 439738, 439737, 125084, 125125, 125039, 124935, 413950, 96714)

dt_combine_merged <- ghdx_merged_citation_component(nid = nids_input)
dt_combine_merged <- dt_combine_merged %>% dplyr::rename('NID_from_mergednid'='Component NID') %>% dplyr::rename("NID" = "Merged Citation NID")
write.xlsx(dt_combine_merged, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/merged_nids_component_nids.xlsx")

nids_records <- ghdx_search_by_nid(nid = nids_input) 
nids_records <- nids_records %>% transform(ID = as.numeric(factor(NID)))

dt_records_only <- left_join(nids_records, dt_combine_merged, by = 'NID')
write.xlsx(dt_records_only, "/ihme/scratch/users/chrish47/source_counts_mb/Clinical_data_review/nids_records_only.xlsx")



# dt_combine_nids <- dt_combine_nids %>% select(nid) %>% unique()
# dt_combine_merged <- dt_combine_merged %>% select(`Component NID`) %>% unique() %>% dplyr::rename('nid' = 'Component NID')
# full_nids <- rbind(dt_combine_nids, dt_combine_merged) %>% unique()
#full_nids[duplicated(full_nids)] #duplicates
# full_nids_message <- full_nids %>% paste0(collapse=",")
# message(full_nids_message)
# full_nids_input <- c(3822, 234672, 234758, 287201, 234771, 234740, 331084, 68367, 133665, 234745, 234692, 234764, 284439, 67132, 432298, 234761, 234774, 234742, 292575, 284422, 234747, 234703, 234750, 336851, 334465, 234765, 284442, 234670, 234766, 104241, 234671, 234769, 234738, 104246, 432265, 234674, 433061, 287203, 287207, 411100, 234760, 234772, 234741, 284421, 234746, 234693, 334464, 234762, 284440, 292574, 432266, 321359, 407536, 284419, 433059, 285520, 433115, 408336, 430710, 287204, 411786, 317423, 292437, 337619, 404395, 206640, 234704, 337129, 281819, 336852, 334466, 421046, 354896, 292577, 284444, 411787, 234673, 287202, 68535, 299375, 433060, 422874, 422318, 421047, 433114, 431674, 336847, 408680, 406980, 397812, 397813, 397814, 244369, 244370, 336850, 244371, 336849, 336848, 336203, 336846, 336843, 336838, 336833, 336827, 336825, 336824, 336817, 336845, 336844, 336842, 336841, 336840, 336839, 336837, 336836, 336835, 336834, 336832, 336831, 336830, 336829, 336828, 336826, 336822, 336820, 413916, 125126, 124938, 125036, 125037, 120469, 109943, 120471, 124877, 109946, 124884, 439735, 439739, 439738, 439737, 125084, 125125, 125039, 124935, 413950, 96714, 26333, 86886, 86887, 86888, 86889, 86890, 86891, 86892, 86893, 86894, 86895, 86896, 86897, 86898, 86899, 86900, 86910, 86911, 86912, 86913, 86914, 86915, 86916, 86917, 86949, 86950, 86951, 86952, 86953, 86954, 86955, 86956, 86957, 86958, 86993, 86994, 86995, 86996, 86997, 86998, 86999, 87000, 87001, 87002, 87003, 87004, 87005, 87006, 87007, 87008, 87009, 87010, 87011, 87012, 87013, 87014, 90314, 90315, 90316, 90317, 90318, 90319, 90322, 94170, 94171, 104242, 104243, 104244, 104245, 104246, 104247, 104248, 104249, 104250, 104251, 104252, 104253, 104254, 104255, 104256, 104257, 104258, 114624, 114876, 121272, 121273, 121274, 121275, 121276, 121277, 121278, 121279, 121280, 121281, 121282, 121334, 121405, 121407, 121408, 121415, 121416, 121417, 121418, 121419, 121420, 121421, 121422, 121423, 121424, 121425, 121444, 121445, 121446, 121447, 121448, 121449, 121450, 121451, 121452, 121453, 121454, 121831, 121832, 121841, 121842, 121843, 121844, 121845, 121846, 121847, 121848, 121849, 121850, 121851, 121854, 121855, 121856, 121857, 121858, 121859, 121860, 121862, 121863, 121917, 126517, 126518, 128781, 130051, 130054, 134187, 136997, 140085, 149500, 149501, 149502, 149503, 149504, 150449, 160484, 174085, 193857, 205019, 212492, 212493, 220205, 220786, 220787, 220788, 220789, 220790, 220791, 220792, 220793, 220794, 220795, 220796, 220797, 220798, 220799, 220800, 221323, 222560, 222563, 223670, 223672, 224499, 224500, 224501, 224502, 237756, 239353, 250036, 265423, 265424, 265425, 281773, 281819, 282493, 282496, 282497, 283865, 284235, 292435, 292436, 293984, 305653, 305654, 307815, 307817, 307818, 307819, 307820, 307821, 307822, 307823, 319414, 327619, 331137, 331138, 331139, 331140, 331141, 331142, 331143, 331144, 331145, 331146, 331147, 331148, 333358, 333359, 333360, 333361, 333649, 333650, 336193, 336195, 336197, 336198, 336199, 336200, 336802, 336853, 336854, 336855, 336856, 336860, 354240, 354860, 369475, 369564, 369579, 377002, 377003, 409529, 409530, 418019, 418020, 421046, 428363, 432299, 432300, 432301, 432304, 432305, 432306, 432307)
# full_nids_types <- ghdx_search_by_nid(nid = full_nids_input)
