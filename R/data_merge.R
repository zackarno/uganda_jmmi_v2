## Uganda Market Monitoring - Data Merge Wrangling
## Last modified 08/10/2020


## Number of things assessed
num_assessed_merge <- data_merge_summary %>% gather(num_market_assessed, num_assessed, -level)

num_assessed_merge <- num_assessed_merge %>% mutate(new_var = paste0(num_market_assessed,"_",level)) %>% ungroup() %>%
                                             select(new_var, num_assessed) %>% pivot_wider(names_from = new_var, values_from = c(num_assessed))

                                           
##############################
#       Items Prices         #
##############################

# Extracting relevant data - National
national <- national_items %>% filter(collection_order == 4) %>% ungroup() %>% select(-collection_order, -price_nails, -month)
colnames(national) <- paste0("national_", colnames(national))

# Extracting relevant data - Regional
regional_sw <- region_items %>% filter(month == this_round_vec & regions == "south west") %>% ungroup() %>% 
                                select(-collection_order, -price_nails, -month)

colnames(regional_sw) <- paste0("southwest_", colnames(regional_sw))

regional_wn <- region_items %>% filter(month == this_round_vec & regions == "west nile") %>% ungroup() %>% 
                                select(-collection_order, -price_nails, -month)


colnames(regional_wn) <- paste0("westnile_", colnames(regional_wn))

# Extracting relevant data - Settlement
settlement_dm <- settlement_items %>% filter(month == this_round_vec) %>% ungroup() %>%
                                      select(-collection_order, -district, -regions, -price_nails, -month) %>% gather(var_name, var_value, -settlement) %>%
                                      mutate(new_var = paste0(settlement,"_",var_name)) %>% select(new_var, var_value) %>%
                                      pivot_wider(names_from = new_var, values_from = var_value)



##############################
# Percentage Change National #
##############################

change_national_march$collection_order_perct_march <- NULL
colnames(change_national_march) <- paste0("national_", colnames(change_national_march))


change_national_last_round$collection_order_perct_last_round <- NULL
colnames(change_national_last_round) <- paste0("national_", colnames(change_national_last_round))



##############################
# Percentage Change Regional #
##############################

# Extracting relevant data - Regional level
percent_change_region_sw <- percent_change_region %>% filter(regions == "south west") %>% ungroup() %>% 
                                                  select(-collection_order_perct_last_round, -collection_order_perct_march, -regions)

colnames(percent_change_region_sw) <- paste0("southwest_", colnames(percent_change_region_sw))


percent_change_region_wn <- percent_change_region %>% filter(regions == "west nile") %>% ungroup() %>% 
                                                  select(-collection_order_perct_last_round, -collection_order_perct_march, -regions)

colnames(percent_change_region_wn) <- paste0("westnile_", colnames(percent_change_region_wn))



################################
# Percentage Change Settlement #
################################

# Extracting relevant data - Settlement level
percent_change_set <- change_settlement %>% select(-contains("collection_")) %>% ungroup() %>%
                                            gather(var_name, var_value, -settlement) %>% mutate(new_var = paste0(settlement,"_",var_name)) %>% 
                                            select(new_var, var_value) %>% pivot_wider(names_from = new_var, values_from = var_value)



##############################
#            MEBs            #
##############################

## Settlement data merge
meb_set <- meb_items %>% filter(collection_order == 4) %>% ungroup() %>% select(-collection_order, -month, -regions, -district) %>%
                         gather(var_name, var_value, -settlement) %>% mutate(new_var = paste0(settlement,"_",var_name)) %>% select(new_var, var_value) %>%
                         pivot_wider(names_from = new_var, values_from = var_value)



## Regional data merge
meb_reg_sw <- meb_items_regional %>% filter(collection_order == 4 & regions == "south west") %>% ungroup() %>% select(-regions, -month, -collection_order)

colnames(meb_reg_sw) <- paste0("southwest_", colnames(meb_reg_sw))

meb_reg_wn <- meb_items_regional %>% filter(collection_order == 4 & regions == "west nile") %>% ungroup() %>% select(-regions, -month, -collection_order)

colnames(meb_reg_wn) <- paste0("westnile_", colnames(meb_reg_wn))



## National data merge
meb_nat <- meb_items_national %>% filter(collection_order == 4) %>% select(-month, -collection_order)

colnames(meb_nat) <- paste0("national_", colnames(meb_nat))



##############################
#         MEB Ranks          #
##############################

rank_dm <- rank_settlments %>% mutate(new_var = paste0("expansive_rank_",rank)) %>% ungroup() %>%
                               select(-rank, -meb_full) %>% pivot_wider(names_from = new_var, values_from = settlement)


## Cbind everything
data_merge <- cbind(national,
                    regional_sw,
                    regional_wn,
                    settlement_dm,
                    change_national_march,
                    change_national_last_round,
                    percent_change_region_sw,
                    percent_change_region_wn,
                    percent_change_set,
                    meb_set,
                    meb_reg_sw,
                    meb_reg_wn,
                    meb_nat,
                    num_assessed_merge,
                    rank_dm)

## Round up
cols <- sapply(data_merge, is.numeric)
data_merge[, cols] <- round(data_merge[, cols],0)








