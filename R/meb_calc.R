## Uganda Market Monitoring - Food MEB and MEB Calculations
## Last modified 16/06/2020

## Load March price
march_mebs <- read.xlsx("./inputs/wfp_march_mebs.xlsx")


## Medians Calculation
meb_items <- item_prices %>% select (-uuid, -market_final, -price_maize_g, -price_underwear, -price_charcoal,
                                     -price_pads, -price_DAP, -price_NKP, -price_malathion, -price_millet_f, -contains("_price")) %>% 
                            group_by(settlement, district, regions, month) %>% 
                                                summarise_all(funs(median(., na.rm = TRUE))) %>% filter(month == this_round_vec | month == last_round_vec)



## Calculate proximity: if a price is missing take the mean of the district, otherwise, Region
meb_items <- meb_items %>% group_by(district, month) %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

meb_items <- meb_items %>% group_by(regions, month) %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

## If NA put price from last round
meb_items <- meb_items %>% group_by(regions) %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

## Calcualte MEB for food items
meb_items$meb_maize_f <- meb_items$price_maize_f * 8.7 * 5
meb_items$meb_beans <- meb_items$price_beans * 5.4 * 5
meb_items$meb_sorghum <- meb_items$price_sorghum * 1.5 * 5
meb_items$meb_oil <- meb_items$price_oil * 0.75 * 5
meb_items$meb_salt <- meb_items$price_salt * 0.15 * 5
meb_items$meb_milk <- meb_items$price_milk * 0.3 * 5
meb_items$meb_dodo <- meb_items$price_dodo * 3 * 5
meb_items$meb_fish <- meb_items$price_fish * 0.6 * 5
meb_items$meb_cassava <- meb_items$price_cassava * 0.6 *5


## Calcualte MEB for non-food items
meb_items$meb_soap <- meb_items$price_soap * 0.45 * 5
meb_items$meb_firewood <- meb_items$price_firewood * 1.1 * 30 * 5


## Calcualte MEB for Hygiene items
meb_items$meb1_reusable_pads <- 4667               ## Yearly one-off
meb_items$meb1_jerry_can <- 1090                   ## March
meb_items$meb1_bucket <- 632                       ## Yearly one-off
meb_items$meb1_hand_washing <- 208                 ## Yearly one-off



meb_items <- meb_items %>% mutate(meb_hygiene = meb1_reusable_pads + meb1_jerry_can +
                                                    meb1_bucket + meb1_hand_washing + meb_soap)


## Extra Items
## Add extra columns
meb_items$meb_clothing <- 3806          ## March    
meb_items$meb_water <- 3750             ## March
meb_items$meb_livelihoods <- 37705      ## March
meb_items$meb_education <- 28667        ## March
meb_items$meb_transport <- 11001        ## March
meb_items$meb_health <- 2669            ## March
meb_items$meb_communication <- 4256     ## March
meb_items$meb1_lighting <- 5000         ## March

## One-off Items - Once a year
blanket <- 45000
pans <- 13125
plates <- 4885
spoon <- 3538
cups <- 3985
mingle <- 1000

meb_items <- meb_items %>% mutate(meb_other_hdd = sum(blanket, pans, plates, spoon, cups, mingle)/12)


## MEB Energy
meb_items <- meb_items %>% mutate(meb_energy = meb1_lighting + meb_firewood)


## Food MEB Calcuations
meb_items <- meb_items %>% mutate(meb_food = meb_maize_f + meb_beans + meb_sorghum +
                                                      meb_oil + meb_milk + meb_cassava + meb_salt +
                                                      meb_dodo + meb_fish)


## Full MEB Calcuations
meb_items <- meb_items %>% mutate(meb_full = meb_food + meb_clothing + meb_water +
                              meb_livelihoods + meb_education + meb_transport +
                              meb_health + meb_communication + meb_hygiene +
                              meb_other_hdd + meb_energy
                              )

## Clean Table, round up, and aggregate
## Settlement
meb_items <- meb_items %>% select(-starts_with("price_"), -starts_with("meb1_"))

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

meb_items <- round_df(meb_items, digits = 0)

### Rbind march mebs in
meb_items <- plyr::rbind.fill(march_mebs, meb_items)

meb_items <- meb_items %>% select("month", everything())
  



## regions
meb_items_regional <- meb_items %>%  select(-district,-settlement) %>% 
  group_by(regions,month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


meb_items_regional <- round_df(meb_items_regional, digits = 0)

names(meb_items_regional)[names(meb_items_regional) == "meb_food"] <- "regional_meb_food"
names(meb_items_regional)[names(meb_items_regional) == "meb_full"] <- "regional_meb_full"

## National
meb_items_national <- meb_items %>%  select(-district,-settlement, -regions) %>% 
  group_by(month) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

meb_items_national$regions <- "nationwide"

meb_items_national <- round_df(meb_items_national, digits = 0)

names(meb_items_national)[names(meb_items_national) == "meb_food"] <- "national_meb_food"
names(meb_items_national)[names(meb_items_national) == "meb_full"] <- "national_meb_full"

## Calculate the top three most expansive settlements
rank_settlments <- meb_items %>% filter(month == this_round_vec)

top_settlments <- rank_settlments %>% ungroup () %>% select(settlement, meb_full) %>%
                                 arrange(desc(meb_full))%>% mutate(rank = 1:nrow(rank_settlments)) %>% filter(rank <= 3)




## Calculate the top three least expansive settlemets
bottom_settlments <- rank_settlments %>% ungroup () %>% select(settlement, meb_full) %>%
                                 arrange(desc(meb_full)) %>% mutate(rank = 1:nrow(rank_settlments)) %>% filter(rank >=11)

rank_settlments <- rbind(top_settlments, bottom_settlments)
 
