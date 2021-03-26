## Uganda Market Monitoring - MEB Calculations and Percentage Change Analysis
## Last modified 08/06/2020


################################
# Percentage Change Settlement #
################################

### Split settlement medians dataset and remove settlements that do not match
## Create list of unique ID from last month and march
rename_cols_for_FS<- function(df,suffix){
  colnames(df)[2:length(colnames(df))]<-colnames(df)[2:length(colnames(df))] %>% 
    str_replace_all("price_","") %>% 
    paste0(.,"_perct_",suffix)
  return(df)
  
}


settlements_last_round <- df %>% filter(yrmo==yrmo_to_include[length(yrmo_to_include)-1]) %>% pull(settlement) %>% unique()
settlements_baseline <- df %>% filter(yrmo== yrmo_to_include[1])%>% pull(settlement) %>% unique()
settlements_this_round <- df %>% filter(yrmo==yrmo_to_include[length(yrmo_to_include)]) %>% pull(settlement) %>% unique()


settlement_items<-settlement_items %>% 
  filter(settlement %in% settlements_this_round)
yrmo_current_and_last<- c(yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[length(yrmo_to_include)-1]) %>% sort()
yrmo_current_and_baseline<- c(yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[1]) %>% sort()

current_and_last<-settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_last)
current_and_baseline<-settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_baseline)

pct_change_current_to_last<-current_and_last %>%
  pct_change_by_groups_all_numerics(group_var = settlement, time_id = yrmo)


pct_change_current_and_last<-pct_change_current_to_last %>% rename_cols_for_FS("last_round")




colnames(pct_change_current_to_last)[2:length(colnames(pct_change_current_to_last))] <- 
  colnames(pct_change_current_to_last)[2:length(colnames(pct_change_current_to_last))] %>% 
  str_replace_all("price_","") %>% 
  paste0(.,"_perct_last_round")




colnames(change_settlement_last_round)[2:length(colnames(change_settlement_last_round))] <- 
  colnames(change_settlement_last_round)[2:length(colnames(change_settlement_last_round))] %>% 
  str_replace_all("price_","") %>% 
  paste0(.,"_perct_last_round")


pct_change_current_to_baseline<-current_and_baseline %>%
  pct_change_by_groups_all_numerics(group_var = settlement, time_id = yrmo)

colnames(pct_change_current_to_baseline)[2:length(colnames(pct_change_current_to_baseline))] <- 
  colnames(pct_change_current_to_baseline)[2:length(colnames(pct_change_current_to_baseline))] %>% 
  str_replace_all("price_","") %>% 
  paste0(.,"_perct_march")

## Merge the two dataset
pct_change_settlement <- left_join(pct_change_current_to_last, pct_change_current_to_baseline, by = "settlement")


#############################
# Percentage Change Region #
############################

item_list<-list(region_items,national_items) %>%
  set_names(c("regions","national"))
analysis_level<-c("regions", "national")
region_items$regions
item_list$regions$regions
# Calculate percentage change between rounds
# % Change this month vs last month
pct_change_regional_national<-item_list %>% 
  map2(analysis_level,function(x,y){
    if(y=="national"){
    x<- x %>%
      mutate(national="national")
    }
    current_and_last<-x%>% 
      filter(yrmo %in% yrmo_current_and_last)
    current_and_base<-x %>% 
      filter(yrmo %in% yrmo_current_and_baseline)
      
    pct_change_current_and_last<- current_and_last %>%
      pct_change_by_groups_all_numerics(group_var = !!sym(y), time_id = yrmo)
    pct_change_current_and_base<- current_and_base %>%
      pct_change_by_groups_all_numerics(group_var = !!sym(y), time_id = yrmo)
    pct_change_current_and_last<-pct_change_current_and_last %>% rename_cols_for_FS("last_round")
    pct_change_current_and_base<-pct_change_current_and_base %>% rename_cols_for_FS("march")
    pct_change_region<- pct_change_current_and_last %>% left_join(pct_change_current_and_base)
  }
  )

# percent_change_national$regions <- "national" # this might bite me


##############################
#   Percentage Change MEBs   #
##############################
meb_items_for_pct_change_list<-list(meb_items_regional,meb_items_national,meb_items) %>% set_names(c("regions","national","settlement"))
analysis_level<-c("regions", "national","settlement")
meb_items_pct_change<-meb_items_for_pct_change_list %>% 
  map2(analysis_level,function(x,y){
    if(y=="national"){
      x<- x %>%
        mutate(national="national")
    }
    current_and_last<-x%>% 
      filter(yrmo %in% yrmo_current_and_last)
    current_and_base<-x %>% 
      filter(yrmo %in% yrmo_current_and_baseline)
    
    pct_change_current_and_last<- current_and_last %>%
      pct_change_by_groups_all_numerics(group_var = !!sym(y), time_id = yrmo)
    pct_change_current_and_base<- current_and_base %>%
      pct_change_by_groups_all_numerics(group_var = !!sym(y), time_id = yrmo)
    pct_change_current_and_last<-pct_change_current_and_last %>% rename_cols_for_FS("last_round")
    pct_change_current_and_base<-pct_change_current_and_base %>% rename_cols_for_FS("march")
    pct_change_region<- pct_change_current_and_last %>% 
      left_join(pct_change_current_and_base) %>% 
      select(contains("food"), contains("full"))
  }
  )


# REGIONAL

change_region_last_round<- left_join(pct_change_regional_national$regions,meb_items_pct_change$regions) %>% select(-ends_with("march"))
change_region_march<- left_join(pct_change_regional_national$regions,meb_items_pct_change$regions) %>% select(-ends_with("last_round"))
# Add to relevant df

reg_list <- list(pct_change_regional_national$regions, change_region_last_round, change_region_march)

percent_change_region <- purrr::reduce(reg_list, left_join)

# percent_change_meb_settlement <- left_join(change_meb_set_current_to_last_red, change_meb_set_march_red)

# NATIONAL ... NOT SURE WHERE THIS GOES
change_national_last_round <- left_join(pct_change_regional_national$national,
                                        meb_items_pct_change$national) %>% select(-ends_with("march"))
change_national_march <- left_join(pct_change_regional_national$national,
                                   meb_items_pct_change$national) %>% select(-ends_with("last_round"))

# SETTLEMENT LEVEL
change_settlement_last_round <- left_join(pct_change_settlement, meb_items_pct_change$settlement) %>% select(-ends_with("march"))
change_settlement_march <- left_join(change_settlement_march, meb_items_pct_change$settlement) %>% select(-ends_with("last_round"))

set_list <- list(change_settlement, change_settlement_last_round, change_settlement_march)

change_settlement <- purrr::reduce(set_list, left_join)


