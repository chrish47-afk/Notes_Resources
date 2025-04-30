##################################
# Author: Christian Hernandez
# Date: August, 2023
# Purpose: Using rJava and the tabulizer package to locally pull/extract PDF tables. This allows you to converte a table(s) in a PDF to a data fram or object in R for data processing purposes.

### IMPORTANT STEPS BEFORE YOU USE THIS SCRIPT #########################
########################################################################
# • Please run this LOCALLY, not on the CLUSTER.
# • Before runninng this code, Please make sure that you are able to adequately source in 'rJava' and 'tabulizer'. If you are having trouble with this, please run the following on your command line:

# -- On your command line (Make sure you are on Admin) -- 
# (1) @powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin
#
# (2) choco install jdk8 -y

# • Github: https://github.com/ropensci/tabulizer
# • Help Video: https://www.youtube.com/watch?v=0AU2tSzKZXU&t=1s

######################################################################
######################################################################

# If you already have these packages locally installed, there's no need to install them again.

################################## Setup
install.packages("rJava")
install.packages("remotes")
install.packages("dplyr")
install.packages("tidyr")
install.packages("openxlsx")

library(rJava)
library(remotes)
library(dplyr)
library(tidyr)
library(openxlsx)

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

library(tabulizer)

################################## Sourcing and Reading in PDF Data
################################## Please adapt this to your own PDF input and parameters

pdf_file <- "H:/pdf_input_test.pdf"

pdf_tables_all <- extract_tables(pdf_file, output = "data.frame") # This will output all the tables in the PDF into a matrix, list.
pdf_tables_page <- extract_tables(pdf_file, pages = 3, output = "data.frame") # This will output all the tables in the PDF page into a matrix, list.

characteristics_table <- pdf_tables_all[[1]] # This will pull out the specified data frame from the output matrix.


################################## Data Wrangling & Enhancement
################################## This will heavily depend on your PDF table output. Some will require significant wrangling, some will not.

colnames(characteristics_table)

characteristics_table_final <- characteristics_table %>% rename("Characteristic" = "X", "Women Without Peripartum
Cardiomyopathy" = "Women.Without.Peripartum.Women.With.Peripartum", "Women With Peripartum cardiomyopathy" = "X.1" , "P" = "X.2") %>% slice(-1) %>% separate("Women Without Peripartum
Cardiomyopathy", c("Women Without Peripartum
Cardiomyopathy(number)", "Women Without Peripartum
Cardiomyopathy(percent)", "Women With Peripartum cardiomyopathy(number)", "Women With Peripartum cardiomyopathy(percent)"), sep = " ") %>% select(-6)

write.xlsx(characteristics_table_final, "H:/table_1_ref_3772.xlsx")

# There are several helpful functions in dplyr and tidyr for this, including rename(), separate(), slice(), etc.
#######################################################

# Another Example
pdf_file <- "H:/Ref 3050.pdf"

pdf_tables <- extract_tables(pdf_file, output = "data.frame")
table_1 <- pdf_tables[[3]]
table_2 <- pdf_tables[[4]]

write.xlsx(table_1, "H:/table_1_ref_3050.xlsx")
write.xlsx(table_2, "H:/table_2_ref_3050.xlsx")

