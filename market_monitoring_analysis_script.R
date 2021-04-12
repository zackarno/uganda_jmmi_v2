## Uganda Market Monitoring - Update R Script
## Last modified 2021-04-08

#****** for 202103 round -- fix this "meb_items_last_round" to new aggregation scheme. 

today <- Sys.Date()

## Download and install hypegrammaR from IMPACT GitHub
#devtools::install_github("impact-initiatives/hypegrammaR", build_opts = c())


## Load required packaged
library(openxlsx)
library(tidyverse)
library(data.table)
library(hypegrammaR)
library(lubridate)
library(butteR)
# Sources

## Save files
source("./R/locations_list.R")
source("./R/functions.R")
source("R/extra_r11_cleaning.R")
source("R/pct_change_function.R")


## Round names - these need to be changed at every round
fps<-get_files_metadata(folder_path = "inputs/clean_data")
year_of_assessment<- as.numeric(str_sub(fps$file_name[length(fps$file_name)],start = 1,end = 4))
month_number<- as.numeric(str_sub(fps$file_name[length(fps$file_name)],start = 5,end = 6))
month_chr<- ifelse(month_number<10,paste0("0",month_number),month_number)
this_round_vec<-month(month_number,label=T, abbr=F)
output_folder<- paste0(year_of_assessment, 
                       month_chr,
                       "_reach_uga_jimmi_outputs")

if(!dir.exists(paste0("outputs/",output_folder))){
  dir.create(paste0("outputs/",output_folder))
}

yrmo_constructed<- glue::glue("{year_of_assessment}{month_chr}")
date_constructed<-as_date(glue::glue("{year_of_assessment}-{month_number}-01"))


prev1_month_number<- month(floor_date(date_constructed - months(1), "month"))
prev2_month_number<- month(floor_date(date_constructed - months(2), "month"))
# yrmos_to_include<- yr

yrmo<- str_sub(string= fps$file_name, start = 1,end = 6)
yrmo_to_include<- c(yrmo[1], yrmo[length(yrmo)],yrmo[length(yrmo)-1],yrmo[length(yrmo)-2]) %>% sort()

df<-fps$fullpath %>% 
  set_names(fps$file_name) %>% 
  map2_dfr(.y = yrmo,
       function(x,y){
         print(x)
        x<- read_csv(x) %>% 
          mutate(
            # month=as.numeric(month),
            settlement= str_replace(settlement, "rhino", "rhino camp"),
            yrmo= as.numeric(y),
            yr= str_sub(string= yrmo, start = 1,end = 4) %>% as.numeric(),
            mo= str_sub(string= yrmo, start = 5,end = 6) %>% as.numeric()
            
            
      ) %>% select(-any_of("today"),yrmo)

        colnames(x)<- ifelse(str_detect(colnames(x), "uuid"),"uuid",colnames(x))
        x
       }
  )

df<-df %>% 
  filter(yrmo %in% yrmo_to_include)


### ANALYSIS

# Creating one big dataframe with all the values from past rounds

# Add the settlement coordinates to the dataset
df <- left_join(df, settlement_data, by="settlement") 

# Add district shape values to dataset
df <- left_join(df, district_data, by = "district")

# Some house cleaning 
# Remove columns that we don't need and rename our uuid columns 
df <- df %>% select(month:district,F15Regions,DName2019, uuid) %>%  
  select(-contains("X_"),-name, -objectid ) %>% 
  mutate(sub_regions = str_to_sentence(F15Regions))


# this is actually incorrect assignment of regions -- need to keep
# so that we get the same values for previous month, but will fix for january
# and eventually phase out

df$regions <- "south west"
df$regions[df$sub_regions == "Acholi" | df$sub_regions == "West nile" ] <- "west nile"
df$regions[df$district == "Bunyoro" ] <- "west nile"



df<-df %>% 
  mutate(month_lab=lubridate::month(mo, label=T, abbr=F),
         # Add new market column that includes other markets
         market_final = ifelse(market == "Other",market_other,market),
         market= NULL,
         market_other=NULL) %>% 
  filter(!is.na(month_lab))


# Move things around using moveme function and delete column not needed
df <- select(df, -c("day", "F15Regions", "DName2019", "sub_regions"))
df$country <- "uganda"

df <- df %>% select(c("month","country", "district", "regions", "settlement", "market_final"), everything())


# WFP/REACH decided to remove 'Less' from vendors_change data as it should not have been an option 
df<- month_specific_cleaning(df)


## Means Calculation
# Prices columns
item_prices <- df %>%  select(uuid,yrmo,month,
                              regions,district,
                              settlement,market_final,
                              contains("price"), starts_with("weight_"), -starts_with("price_increase"), -starts_with("price_decrease"),
                              -ends_with(".prices"), -starts_with("challenge."))

item_prices[item_prices == 99] <- NA
item_prices[item_prices == "yes"] <- 1

# Because WFP added "no" in the columns we now have to turn them into integers
# item_prices[ , 7:54] <- apply(item_prices[ , 7:54], 2,            
#                              function(x) as.numeric(as.character(x)))


# Recalculate non standard items
item_prices <- item_prices %>% ungroup() %>% mutate(price_dodo = price_dodo/weight_dodo,
                                                    price_cassava = price_cassava/weight_cassava,
                                                    price_fish = price_fish/weight_fish,
                                                    price_firewood = price_firewood/weight_firewood,
                                                    price_charcoal = price_charcoal/weight_charcoal)

item_prices <- item_prices %>% select(-contains("weight"), -contains("Observed"))



# Collection_order
# last_round_vec<- month(prev1_month_number,label=T, abbr=F)

# for meb calculations we need to include last 2 months plus march
# item_prices_last_2_and_march<-item_prices %>% 
#   filter(month %in% c(3, prev1_month_number:month_number))
# 
# # collection order is used to split up settlement data in percent change
# item_prices_last_2_and_march <- item_prices_last_2_and_march %>%
#   mutate(collection_order = ifelse(month == month_number, 4,
#                                    ifelse(month == prev1_month_number,3, 1)),
#          month=month(month, label = T, abbr=F))



# for pct change we only want this month, last month, and march
item_prices_for_pct_change<- item_prices %>% 
  filter(yrmo %in% c(yrmo_to_include[1],yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[length(yrmo_to_include)-1])) %>% 
  mutate(collection_order = ifelse(month == month_number, 4,
                                   ifelse(month == prev1_month_number,3, 1)),
         month=month(month, label = T, abbr=F))

# Mean prices
nan_inf_to_na <- function(x) {
  y <- replace(x, is.infinite(x), NA) 
  z <- replace(y, is.nan(y), NA)
  z
}  ## function to replace Inf and NaN with NAs for a cleaner output


national_items <- item_prices_for_pct_change %>%  
  select(-uuid, -regions, -district, -settlement, -market_final) %>% 
  group_by(month, collection_order) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_at(vars(-group_cols()), nan_inf_to_na)


markets_items <- item_prices_for_pct_change %>%  select(-uuid,-regions,-district) %>% 
  group_by(settlement,market_final,month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate_at(vars(-group_cols()), nan_inf_to_na)


settlement_items <- item_prices_for_pct_change %>%  select(-uuid,-market_final) %>% 
  group_by(regions,district,settlement,month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_at(vars(-group_cols()), nan_inf_to_na)


district_items <- item_prices_for_pct_change %>%  select(-uuid,-settlement,-market_final) %>% 
  group_by(regions,district,month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_at(vars(-group_cols()), nan_inf_to_na)  


region_items <- item_prices_for_pct_change %>%  select(-uuid,-market_final,-district,-settlement) %>% 
  group_by(regions,month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_at(vars(-group_cols()), nan_inf_to_na)


# Counts per area: region and settlements
markets_per_region <- item_prices_for_pct_change %>% 
  select(regions, month, market_final) %>% 
  group_by(regions,month) %>% 
  summarise(num_market_assessed = n_distinct(market_final),
            num_assessed = length(month)) %>% 
  rename("level"= regions) %>% filter(month == this_round_vec) %>% 
  select(level,num_market_assessed,num_assessed)


settlements_per_region <- item_prices_for_pct_change %>% 
  select(regions,settlement,month) %>% 
  group_by(regions,month) %>% 
  summarise(markets_numer = n_distinct(settlement))


# Counts per area: nation wide
markets_nationwide <- item_prices_for_pct_change %>%
  select(regions, month, market_final) %>% 
  group_by(month) %>% 
  summarise(num_market_assessed = n_distinct(market_final),
            num_assessed = length(month),
            level = "national") %>% 
  filter(month == this_round_vec) %>% 
  select(level,num_market_assessed,num_assessed)


data_merge_summary <- bind_rows(markets_nationwide,markets_per_region)


# Calcualte MEBs
source("./R/meb_calc.R")

# Calculate % changes
# source("./R/percent_change_calc.R")
source("./R/percent_change_calc_revised.R")
# al_percent_change_region<-percent_change_region
# al_change_settlement<-change_settlement


## Data exports 
list_of_datasets_med <- list("Market mean price" = markets_items,
                             "Settlement mean price" = settlement_items,
                             "District Mean" = district_items,
                             "Region mean" = region_items,
                             "National level mean" = national_items,
                             "Percent change Settlement" = change_settlement,
                             "Percent change Region" = percent_change_region,
                             "Percent change National" = percent_change_national,
                             "Rank settlements" = rank_settlments
)

list_of_datasets_meb <- list("Settlement MEB" = meb_items,
                             "Regional MEB" = meb_items_regional,
                             "National MEB" = meb_items_national,
                             "Percent change MEB Settlment" = meb_items_pct_change$settlement,
                             "Percent change MEB Regional" = meb_items_pct_change$regions,
                             "Percent change MEB National" = meb_items_pct_change$national
)
write.xlsx(list_of_datasets_med, 
           paste0("./outputs/",
                  output_folder,"/",
                  butteR::date_file_prefix(),"_",
                  this_round_vec,
                  "_UGA_JMMI_Means and percentage change.xlsx")
           )

write.xlsx(list_of_datasets_meb, 
           paste0("./outputs/",
                  output_folder,"/",
                  butteR::date_file_prefix(),"_",
                  this_round_vec,
                  "_UGA_JMMI_MEB and percentage change.xlsx")
           )


## Market Functionality Page

### Load data analysis plan and questionnaire
dap <- load_analysisplan("./inputs/dap/jmmi_dap_v1.csv")
# dap %>% View()


df_analysis <- df %>%
  mutate(mobile_accepted = ifelse(grepl("mobile_money", payment_type), "yes", "no")) %>%
  filter(yrmo == yrmo_constructed)
kobo_tool <- load_questionnaire(df_analysis,
                                questions = read.csv("./inputs/kobo/questions.csv"),
                                choices = read.csv("./inputs/kobo/choices.csv"),
                                choices.label.column.to.use = "label")


## Prepare dataset for analysis

df_analysis$customer_number <- as.numeric(df_analysis$customer_number)
df_analysis$agents_number <- as.numeric(df_analysis$agents_number)


## Launch analysis and isolate analysis results
analysis <- from_analysisplan_map_to_output(data = df_analysis,
                                            analysisplan = dap,
                                            weighting = NULL,
                                            questionnaire = kobo_tool)
## SUMMARY STATS LIST ##

summary.stats.list <- analysis$results %>% 
  lapply(function(x){map_to_labeled(result = x, questionnaire = kobo_tool)})


## Save tabulated analysis file
summary.stats.list %>% 
  resultlist_summary_statistics_as_one_table() %>% 
  select(-se, -min, -max) %>%
  map_to_file(paste0("./outputs/",
                     output_folder,"/",
                     butteR::date_file_prefix(),"_",
                     this_round_vec, "_jmmi_analysis.csv"))
# resultlist_summary_statistics_as_one_table()

## Save html analysis file
hypegrammaR:::map_to_generic_hierarchical_html(resultlist = analysis,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("research.question","indicator"),
                                               by_prefix = c("RQ:", "Indicator: "),
                                               level = 2,
                                               questionnaire = kobo_tool,
                                               label_varnames = TRUE,
                                               dir = paste0("./outputs/", output_folder),
                                               filename = paste0(butteR::date_file_prefix(),
                                                                 "_html_analysis_jmmi",
                                                                 ".html")
                                               )


## TOP 3 Analysis
### Slice result list by areas
summary.stats.list <- analysis$results %>% resultlist_summary_statistics_as_one_table %>% select(-se, -min, -max, -repeat.var, -repeat.var.value)
vec1 <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
vec2 <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)

## Rename based on choices from kobo
# need choice object
choices <- read.csv("./inputs/kobo/choices.csv")
summary.stats.list$dependent.var.value <- choices$label[match(summary.stats.list$dependent.var.value, choices$name)]

## All markets 
top3_uganda <- summary.stats.list %>% filter(dependent.var == "payment_type" |
                                               dependent.var == "safety_reason_less_secure" |
                                               dependent.var == "safety_reason_more_secure" |
                                               dependent.var == "item_scarcity_reason" |
                                               dependent.var == "price_increase_item" |
                                               dependent.var == "price_decrease_item" |
                                               dependent.var == "challenge") %>% 
  filter(independent.var.value == "uganda") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:3)

## Add ranking col and rename the options
top3_uganda$rank <- vec1

## New var for data merge and pivot wider
top3_uganda <- top3_uganda %>% 
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% 
  ungroup() %>%
  select(new_var, numbers, dependent.var.value) %>%
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## South West Region
top3_southwest <- summary.stats.list %>% filter(dependent.var == "payment_type" |
                                                  dependent.var == "safety_reason_less_secure" |
                                                  dependent.var == "safety_reason_more_secure" |
                                                  dependent.var == "item_scarcity_reason" |
                                                  dependent.var == "price_increase_item" |
                                                  dependent.var == "price_decrease_item" |
                                                  dependent.var == "challenge") %>% 
  filter(independent.var.value == "south west") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:3)
## Add ranking col
top3_southwest$rank <- vec1

## New var for datamerge and pivot wider
top3_southwest <- top3_southwest %>% mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>%  ungroup() %>%
  select(new_var, numbers, dependent.var.value) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))




## West Nile regions
top3_westnile<- summary.stats.list %>% filter(dependent.var == "payment_type" |
                                                dependent.var == "safety_reason_less_secure" |
                                                dependent.var == "safety_reason_more_secure" |
                                                dependent.var == "item_scarcity_reason" |
                                                dependent.var == "price_increase_item" |
                                                dependent.var == "price_decrease_item" |
                                                dependent.var == "challenge") %>%
  filter(independent.var.value == "west nile") %>% 
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:3)

## Add ranking col
top3_westnile$rank <- vec1


## New var for datamerge and pivot wider
top3_westnile <- top3_westnile %>% 
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>%
  ungroup() %>%
  select(new_var, numbers, dependent.var.value) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## TOP 2 Analysis - Increase in price

## All markets 
top2_uganda <- summary.stats.list %>% filter(dependent.var == "cereal_increase_reason" |
                                               dependent.var == "cassava_increase_reason" |
                                               dependent.var == "beans_increase_reason" |
                                               dependent.var == "vegetables_increase_reason" |
                                               dependent.var == "milk_increase_reason" |
                                               dependent.var == "fish_increase_reason" |
                                               dependent.var == "oil_increase_reason" |
                                               dependent.var == "salt_increase_reason" |
                                               dependent.var == "wash_increase_reason" |
                                               dependent.var == "energy_increase_reason") %>% 
  filter(independent.var.value == "uganda") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_uganda$rank <- vec2

## New var for datamerge and pivot wider
top2_uganda <- top2_uganda %>% 
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% 
  ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## South West
top2_southwest <- summary.stats.list %>% filter(dependent.var == "cereal_increase_reason" |
                                                  dependent.var == "cassava_increase_reason" |
                                                  dependent.var == "beans_increase_reason" |
                                                  dependent.var == "vegetables_increase_reason" |
                                                  dependent.var == "milk_increase_reason" |
                                                  dependent.var == "fish_increase_reason" |
                                                  dependent.var == "oil_increase_reason" |
                                                  dependent.var == "salt_increase_reason" |
                                                  dependent.var == "wash_increase_reason" |
                                                  dependent.var == "energy_increase_reason") %>% 
  filter(independent.var.value == "south west") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_southwest$rank <- vec2


## New var for datamerge and pivot wider
top2_southwest <- top2_southwest %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% 
  ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>%
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## West Nile
top2_westnile <- summary.stats.list %>% filter(dependent.var == "cereal_increase_reason" |
                                                 dependent.var == "cassava_increase_reason" |
                                                 dependent.var == "beans_increase_reason" |
                                                 dependent.var == "vegetables_increase_reason" |
                                                 dependent.var == "milk_increase_reason" |
                                                 dependent.var == "fish_increase_reason" |
                                                 dependent.var == "oil_increase_reason" |
                                                 dependent.var == "salt_increase_reason" |
                                                 dependent.var == "wash_increase_reason" |
                                                 dependent.var == "energy_increase_reason") %>% 
  filter(independent.var.value == "west nile") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_westnile$rank <- vec2

## New var for datamerge and pivot wider
top2_westnile <- top2_westnile %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% 
  ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))





## TOP 2 Analysis - Decrease in price

## All markets 
top2_uganda_dec <- summary.stats.list %>% filter(dependent.var == "cereal_decrease_reason" |
                                                   dependent.var == "cassava_decrease_reason" |
                                                   dependent.var == "beans_decrease_reason" |
                                                   dependent.var == "vegetables_decrease_reason" |
                                                   dependent.var == "milk_decrease_reason" |
                                                   dependent.var == "fish_decrease_reason" |
                                                   dependent.var == "oil_decrease_reason" |
                                                   dependent.var == "salt_decrease_reason" |
                                                   dependent.var == "wash_decrease_reason" |
                                                   dependent.var == "energy_decrease_reason") %>% 
  filter(independent.var.value == "uganda") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_uganda_dec$rank <- vec2

## New var for datamerge and pivot wider
top2_uganda_dec <- top2_uganda_dec %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>%
  ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>% pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))




## South West 
top2_southwest_dec <- summary.stats.list %>% filter(dependent.var == "cereal_decrease_reason" |
                                                      dependent.var == "cassava_decrease_reason" |
                                                      dependent.var == "beans_decrease_reason" |
                                                      dependent.var == "vegetables_decrease_reason" |
                                                      dependent.var == "milk_decrease_reason" |
                                                      dependent.var == "fish_decrease_reason" |
                                                      dependent.var == "oil_decrease_reason" |
                                                      dependent.var == "salt_decrease_reason" |
                                                      dependent.var == "wash_decrease_reason" |
                                                      dependent.var == "energy_decrease_reason") %>% 
  filter(independent.var.value == "south west") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_southwest_dec$rank <- vec2

## New var for datamerge and pivot wider
top2_southwest_dec <- top2_southwest_dec %>% mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>% pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))



## West Nile 
top2_westnile_dec <- summary.stats.list %>% filter(dependent.var == "cereal_decrease_reason" |
                                                     dependent.var == "cassava_decrease_reason" |
                                                     dependent.var == "beans_decrease_reason" |
                                                     dependent.var == "vegetables_decrease_reason" |
                                                     dependent.var == "milk_decrease_reason" |
                                                     dependent.var == "fish_decrease_reason" |
                                                     dependent.var == "oil_decrease_reason" |
                                                     dependent.var == "salt_decrease_reason" |
                                                     dependent.var == "wash_decrease_reason" |
                                                     dependent.var == "energy_decrease_reason") %>% 
  filter(independent.var.value == "west nile") %>%
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice(1:2)
## Add ranking col
top2_westnile_dec$rank <- vec2

## New var for datamerge and pivot wider
top2_westnile_dec <- top2_westnile_dec %>% 
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",rank)) %>% 
  ungroup() %>%
  select(dependent.var.value,  new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## Bind all together in one data merge-ready file, multiply by 100 and round up
top_analysis <- cbind(top3_uganda, top3_southwest, top3_westnile, top2_uganda, top2_uganda_dec, top2_southwest,
                      top2_southwest_dec, top2_westnile, top2_westnile_dec)

cols <- sapply(top_analysis, is.numeric)
top_analysis[, cols] <- top_analysis[, cols] * 100


## Select one and other analysis spread

## All markets
non_perct_vars <- summary.stats.list %>% filter(dependent.var == "agents_number" |
                                                  dependent.var == "customer_number") %>%
  filter(independent.var.value == "uganda")


## New var for datamerge and pivot wider
non_perct_vars <- non_perct_vars %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var)) %>%
  ungroup() %>%
  select(new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers))


## South West
non_perct_vars_southwest <- summary.stats.list %>% 
  filter(dependent.var == "agents_number" |dependent.var == "customer_number") %>%
  filter(independent.var.value == "south west")


## New var for datamerge and pivot wider
non_perct_vars_southwest <- non_perct_vars_southwest %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var)) %>% 
  ungroup() %>%
  select(new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers))


## West Nile
non_perct_vars_westnile <- summary.stats.list %>% 
  filter(dependent.var == "agents_number" |dependent.var == "customer_number") %>%
  filter(independent.var.value == "west nile")


## New var for datamerge and pivot wider
non_perct_vars_westnile <- non_perct_vars_westnile %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var)) %>% 
  ungroup() %>%
  select(new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers))


## Cbind the non-percent analysis
non_perct_vars_fin <- cbind(non_perct_vars, non_perct_vars_southwest, non_perct_vars_westnile)

## All markets
perct_vars <- summary.stats.list %>% filter(dependent.var == "mobile_accepted" |
                                              dependent.var == "vendor_number" |
                                              dependent.var == "vendors_change" |
                                              dependent.var == "safety" |
                                              dependent.var == "item_scarcity" |
                                              dependent.var == "stock_runout") %>%
  filter(independent.var.value == "uganda")

# perct_vars %>% View()

## New var for datamerge and pivot wider
perct_vars <- perct_vars %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",dependent.var.value)) %>%
  ungroup() %>%
  select(dependent.var.value, new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## South West
perct_vars_southwest <- summary.stats.list %>% filter(dependent.var == "mobile_accepted" |
                                                        dependent.var == "vendor_number" |
                                                        dependent.var == "vendors_change" |
                                                        dependent.var == "safety" |
                                                        dependent.var == "item_scarcity" |
                                                        dependent.var == "stock_runout") %>%
  filter(independent.var.value == "south west")


## New var for datamerge and pivot wider
perct_vars_southwest <- perct_vars_southwest %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",dependent.var.value)) %>%
  ungroup() %>%
  select(dependent.var.value, new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## West Nile
perct_vars_westnile <- summary.stats.list %>% filter(dependent.var == "mobile_accepted" |
                                                       dependent.var == "vendor_number" |
                                                       dependent.var == "vendors_change" |
                                                       dependent.var == "safety" |
                                                       dependent.var == "item_scarcity" |
                                                       dependent.var == "stock_runout") %>%
  filter(independent.var.value == "west nile")


## New var for datamerge and pivot wider
perct_vars_westnile <- perct_vars_westnile %>%
  filter(!is.na(dependent.var.value)) %>%
  mutate(new_var = paste0(independent.var.value,"_",dependent.var,"_",dependent.var.value)) %>%
  ungroup() %>%
  select(dependent.var.value, new_var, numbers) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value))


## Bind all together in one data merge-ready file, multiply by 100 and round up
perct_vars_fin <- cbind(perct_vars, perct_vars_southwest, perct_vars_westnile)

cols <- sapply(perct_vars_fin, is.numeric)
perct_vars_fin[, cols] <- perct_vars_fin[, cols] * 100



## Launch script that pivots all the prices, mebs, and percent changes, and most expensive settlements
source("./R/data_merge.R")


## Cbind everything, round up, and save as a csv file
data_merge_final <- cbind(top_analysis, 
                          non_perct_vars_fin, 
                          perct_vars_fin,
                          data_merge)

cols <- sapply(data_merge_final, is.numeric)
data_merge_final[, cols] <- round(data_merge_final[, cols], 0)

data_merge_final %>% class()
## Save

write_csv(
  data_merge_final,
  paste0("outputs/",
         output_folder,
         "/",
         butteR::date_file_prefix(),"_",
         this_round_vec,
         "_jmmi_data merge.csv"), na= "n/a"
  
  
)
