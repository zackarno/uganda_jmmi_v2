## Uganda Market Monitoring - MEB Calculations and Percentage Change Analysis
## Last modified 03/26/2021

################################
# Percentage Change Settlement #
################################

# getting unique settlements in each of the relevant rounds
settlements_last_round <- df %>% filter(yrmo==yrmo_to_include[length(yrmo_to_include)-1]) %>% pull(settlement) %>% unique()
settlements_baseline <- df %>% filter(yrmo== yrmo_to_include[1])%>% pull(settlement) %>% unique()
settlements_this_round <- df %>% filter(yrmo==yrmo_to_include[length(yrmo_to_include)]) %>% pull(settlement) %>% unique()

# tolower(settlements_this_round) [!tolower(settlements_this_round) %in%tolower(settlements_baseline)]
# we need to just consider settlements that match the current round in order calculate % changes in current round
settlement_items<-settlement_items %>% 
  filter(settlement %in% settlements_this_round)

# extract the yrmo combos of interest
yrmo_current_and_last<- c(yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[length(yrmo_to_include)-1]) %>% sort()
yrmo_current_and_baseline<- c(yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[1]) %>% sort()

# create the two datasets necessary for % change calculations
current_and_last<-settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_last)
current_and_baseline<-settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_baseline)

# calculate % change for each data set
# current to last month
# debugonce(pct_change_by_groups_all_numerics)
pct_change_current_to_last<-current_and_last %>%
  pct_change_by_groups_all_numerics(group_var = "settlement", time_id = "yrmo")
# current to baseline (march 2020)
pct_change_current_to_baseline<-current_and_baseline %>%
  pct_change_by_groups_all_numerics(group_var = "settlement", time_id = "yrmo")

# rename columns according to schema from previous rounds
pct_change_current_and_last<-pct_change_current_to_last %>% rename_cols_for_FS("last_round")
pct_change_current_to_baseline<-pct_change_current_to_baseline %>% rename_cols_for_FS("march")

## Merge the two dataset
pct_change_settlement <- left_join(pct_change_current_to_last, pct_change_current_to_baseline, by = "settlement")



# Calculate % change at regional and national -----------------------------

# since the process is the same, put them in a list so we can purrr through
item_list<-list(region_items,national_items) %>%
  set_names(c("regions","national"))
analysis_level<-c("regions", "national")

# calculate % change between this round and base and this round and last round

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
      pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo")
    pct_change_current_and_base<- current_and_base %>%
      pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo")
    pct_change_current_and_last<-pct_change_current_and_last %>% rename_cols_for_FS("last_round")
    pct_change_current_and_base<-pct_change_current_and_base %>% rename_cols_for_FS("march")
    pct_change_region<- pct_change_current_and_last %>% left_join(pct_change_current_and_base)
  }
  )
# Calc % change in mebs at regional, natl, and settlement level -----------
# same process as above, but involves a slightly different selection at the end.
# therefore, I juts copied and pasted, but I should integrate into one map statement with if/else to accou
# for different selections
meb_items_for_pct_change_list<-list(meb_items_regional,meb_items_national,meb_items) %>%
  set_names(c("regions","national","settlement"))
analysis_level<-c("regions", "national","settlement")

meb_items_pct_change<-meb_items_for_pct_change_list %>% 
  map2(analysis_level,function(x,y){
    print(analysis_level)
    if(y=="national"){
      x<- x %>%
        mutate(national="national")
    }
    
    current_and_last<-x%>% 
      filter(yrmo %in% yrmo_current_and_last)
    current_and_base<-x %>% 
      filter(yrmo %in% yrmo_current_and_baseline)
    
    
    pct_change_current_and_last<- current_and_last %>%
      pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo")
    
    
    pct_change_current_and_base<- current_and_base %>%
      pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo")
    pct_change_current_and_last<-pct_change_current_and_last %>% rename_cols_for_FS("last_round")
    pct_change_current_and_base<-pct_change_current_and_base %>% rename_cols_for_FS("march")
    
    pct_change_region<- pct_change_current_and_last %>% 
      left_join(pct_change_current_and_base) %>% 
      select(y,contains("food"), contains("full"))
  }
  )

# extract regional results in format/schema needed for downstream processes
# combine different results, get price changes with meb price changes  (this round to last round)
change_region_last_round<- left_join(pct_change_regional_national$regions,meb_items_pct_change$regions) %>%
  select(-ends_with("march"))
# combine different results, get price changes with meb price changes  (this round to last march 2020)
change_region_march<- left_join(pct_change_regional_national$regions,meb_items_pct_change$regions) %>%
  select(-ends_with("last_round"))
# combine results
reg_list <- list(pct_change_regional_national$regions, change_region_last_round, change_region_march)
percent_change_region <- purrr::reduce(reg_list, left_join)

# SAME AS ABOVE, BUT NATL
change_national_last_round <- left_join(pct_change_regional_national$national,
                                        meb_items_pct_change$national) %>% select(-ends_with("march"))
change_national_march <- left_join(pct_change_regional_national$national,
                                   meb_items_pct_change$national) %>% select(-ends_with("last_round"))
percent_change_national<- left_join(change_national_last_round,change_national_march)

# SAME AS ABOVE, BUT SETTLEMENT LEVEL7

change_settlement_last_round <- left_join(pct_change_settlement, meb_items_pct_change$settlement) %>% select(-ends_with("march"))
change_settlement_march <- left_join(pct_change_settlement, meb_items_pct_change$settlement) %>% select(-ends_with("last_round"))
set_list <- list( change_settlement_last_round, change_settlement_march)
change_settlement <- purrr::reduce(set_list, left_join)


