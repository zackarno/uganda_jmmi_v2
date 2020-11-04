## Uganda Market Monitoring - Data Cleaning
## Last modified 07/06/2020

## this code cleans your environment
rm(list=ls())

today <- Sys.Date()

# Download clog package from GitHub
#install.packages("devtools")
#devtools::install_github("mabafaba/clog", build_vignettes = T, force = T)


# browseVignettes("clog")  # IF THE VIGNETTES FAIL TO SHOW CHANGE THE CODE "build_vignettes = T" with "build_opts = c()".


# Load necessary packages
require(glue)
require(clog)
require(tidyverse)
require(openxlsx)

# Load sources
source("./cleaning/fixing_clog.R")

# Load cleaning log and raw data
cleaning_log <- read.xlsx("./inputs/REACH_Uganda_Covid market monitoring_safety_resons_recoded_20200609.xlsx")

data_raw <- read.xlsx("./inputs/WFP_Market data_15-31_May2020.xlsx")
names(data_raw)[names(data_raw) == "_uuid"] <- "uuid"


# Proceed to load the cleaning log into the environemnt
clog <- cleaninglog(ids = cleaning_log$uuid, 
                    variables = cleaning_log$indicator, 
                    new_values = cleaning_log$new_value, 
                    data_id_column_name = "uuid", change = cleaning_log$change)

# names(cleaning_log) <- c("ids", "spotted_by", "name", "variables", "current_value","new_values", "new_value_2", "issue")
# attr(cleaning_log, 'data_id_column_name') <- "X_uuid"
# cleaning_log$change <- T

# Check that all the uuids are there - if result is 0 it's ok
cleaning_log$uuid[!(cleaning_log$uuid %in% data_raw$uuid)]

# Clean data, recoding and rename
data_clean <- clog_clean(df = data_raw, cleaninglog = clog)


data_clean$safety_reason_recoding_lesssecure <- rep(NA, nrow(data_clean))
data_clean$safety_reason_recoding_lesssecure[data_clean$safety == "Less secure"] <- data_clean$safety_reason[data_clean$safety == "Less secure"]
data_clean$safety_reason_recoding_moresecure <- rep(NA, nrow(data_clean))
data_clean$safety_reason_recoding_moresecure[data_clean$safety == "More secure"] <- data_clean$safety_reason[data_clean$safety == "More secure"]


data_clean$decrease__pct <- rep(NA, nrow(data_clean))
data_clean$decrease__pct[data_clean$customers_change == "Decreased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Decreased"]
data_clean$increase_pct <- rep(NA, nrow(data_clean))
data_clean$increase_pct[data_clean$customers_change == "Increased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Increased"]

## Save clean output - feel free to rename the data as it most convenient for you, the "today" add the date this script was run to ease validation
write.xlsx(data_clean, paste0("./outputs/clean data/market monitoring_clean_data_",today,".xlsx"))

