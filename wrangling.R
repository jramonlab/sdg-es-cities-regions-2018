## Donwload and wrangling
library(readxl)
library(dplyr)

options(digits = 3)

url <- "http://reds-sdsn.es/wp-content/uploads/2018/10/Spanish-Cities-Index_para-publicacio%CC%81n-web.xlsx"

# Prepare working directory
# File tree structure. Set the working directory where the file is
wd <- getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd <- getwd()

system("mkdir data")
system("mkdir rda")
system("mkdir figs")
system("mkdir report")

# local temp store
#temp_file <- "./data/data_temp.zip"
#download.file(url, temp_file)
# local temp store
#temp_file <- "./data/data_temp_eu.xlsx"
#download.file(url, temp_file)
# random name
#tmp_filename <- tempfile()
#download.file(url, tmp_filename)


# So many problems with download, so manual download
#local_file <- "./data/Spanish-Cities-Index_para-publicación-web.xlsx"
# error due to file name, so we rename it as follows:
local_file <- "./data/ods-es-cities-2018.xlsx"
#
# Region code has been included manually in this file 
local_file <- "./data/ods-es-cities-by-regions-2018.xlsx"

# Create temp file and store data into a table called dat
dat_sheets <- read_excel(local_file)
# Select sheet to import
dat_database <- read_excel(local_file, sheet = "100 Cities Index")
# Select range from sheeet to import
dat_database_summary <- read_excel(local_file, 
                                   sheet = "100 Cities Index", 
                                   range = "FT1:GJ101")

dat_database_cities <- read_excel(local_file, 
                                   sheet = "100 Cities Index", 
                                   range = "A1:C101")

dat_database_regions <- read_excel(local_file, 
                                  sheet = "Regions", 
                                  range = "B5:C24")

#
# WRANGLING
#

#
# Add region name to sdg2018es
dat_database_cities <- left_join(dat_database_cities, dat_database_regions, by = c("region_id" = "Region_code"))

# bind cities and sdg values
sdg2018es <- cbind(dat_database_cities, dat_database_summary)

#
# Add column with average SDG score
sdg2018es <- sdg2018es %>% mutate(sdg_avg = rowMeans(na.rm = TRUE, .[, c(5:ncol(sdg2018es))]))

class(sdg2018es)
str(sdg2018es)
names(sdg2018es)

#
# replace NA by EU
sdg2019$'Sub-region' <- replace(sdg2019$'Sub-region', is.na(sdg2019$'Sub-region'), "Whole European Union")
# change region as factor
sdg2019$'Sub-region' <- as.factor(sdg2019$'Sub-region')
class(sdg2019)
# from tibble to dataframe
#sdg2019 <- as.data.frame(sdg2019)

## COUNTRY overall data + all 17 goal indexes
#  EUROPE
sdg2019eu <- cbind(sdg2019, goal_index)

class(sdg2019eu)
str(sdg2019eu)
names(sdg2019eu)

save(sdg2018es, file = "rda/sdg2018es.rda")


