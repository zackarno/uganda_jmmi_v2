## Uganda Market Monitoring - MEB Calculations and Percentage Change Analysis
## Last modified 08/06/2020


################################
# Percentage Change Settlement #
################################

### Split settlement medians dataset and remove settlements that do not match
## Create list of unique ID from last month and march


last_round_unique <- df %>% filter(month==prev1_month_number) %>% pull(settlement) %>% unique()
march_unique <- df %>% filter(month==3) %>% pull(settlement) %>% unique()


## Split settlement medians list into multiples
settlement_items_split <- settlement_items %>% split.data.frame(.,factor(settlement_items$collection_order))

## Select unique ID from last month to match on this month
settlement_items_split[["4"]] <- subset(settlement_items_split[["4"]], last_round_unique %in% settlement_items_split[["4"]][["settlement"]])

## Create dataset
temp1 <- settlement_items_split[["4"]]
temp2 <- settlement_items_split[["3"]]

pct_change_current_to_last <- rbind(temp1, temp2)


## Select unique ID from march to match on this month
settlement_items_split[["4"]] <- subset(settlement_items_split[["4"]], march_unique %in% settlement_items_split[["4"]][["settlement"]])

## Create dataset
temp1 <- settlement_items_split[["4"]]
temp2 <- settlement_items_split[["1"]]

pct_change_current_to_march <- rbind(temp1, temp2)

rm(temp1)
rm(temp2)


# Calculate percentage change between rounds
# % Change this month vs last month
pct_change_current_to_last <- pct_change_current_to_last %>% split.data.frame(.,factor(pct_change_current_to_last$collection_order))

change_settlement_last_round <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, pct_change_current_to_last[["4"]], pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame

names(change_settlement_last_round) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round") 

change_settlement_last_round$settlement <- pct_change_current_to_last[["3"]]$settlement



# % Change this month vs march
pct_change_current_to_march <- pct_change_current_to_march %>% split.data.frame(.,factor(pct_change_current_to_march$collection_order))

change_settlement_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, pct_change_current_to_march[["4"]], pct_change_current_to_march[["1"]]) %>% do.call(cbind,.) %>% as.data.frame

names(change_settlement_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

change_settlement_march$settlement <- pct_change_current_to_march[["1"]]$settlement

## Merge the two dataset
change_settlement <- merge(change_settlement_last_round, change_settlement_march, by = "settlement")


#############################
# Percentage Change Region #
############################

# Calculate percentage change between rounds
# % Change this month vs last month
region_pct_change_current_to_last <- region_items %>% split.data.frame(.,factor(region_items$collection_order))


change_region_last_round <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, region_pct_change_current_to_last[["4"]], region_pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_region_last_round) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round")

# % Change this month vs march

change_region_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, region_pct_change_current_to_last[["4"]], region_pct_change_current_to_last[["1"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_region_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

percent_change_region <- cbind(change_region_last_round, change_region_march)
percent_change_region$regions <- c("south west", "west nile")



##############################
# Percentage Change National #
##############################

# Calculate percentage change between rounds
national_pct_change_current_to_last <- national_items %>% split.data.frame(.,factor(national_items$collection_order))

change_national_last_round <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, national_pct_change_current_to_last[["4"]], national_pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_national_last_round) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round") 



change_national_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, national_pct_change_current_to_last[["4"]], national_pct_change_current_to_last[["1"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_national_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

percent_change_national <- cbind(change_national_last_round, change_national_march)
percent_change_national$regions <- "national"


##############################
#   Percentage Change MEBs   #
##############################

# meb_sett_pct_change_current_to_last <- meb_items %>% split.data.frame(.,factor(meb_items$month))

# Calculate percentage change between rounds - National
meb_nat_pct_change_current_to_last <- meb_items_national %>% split.data.frame(., factor(meb_items_national$collection_order))

change_meb_nat_current_to_last <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_nat_pct_change_current_to_last[["4"]], meb_nat_pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_nat_current_to_last) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round") 


change_meb_nat_current_to_last_red <- change_meb_nat_current_to_last %>% select(contains("food"), contains("full"))


change_meb_nat_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_nat_pct_change_current_to_last[["4"]], meb_nat_pct_change_current_to_last[["1"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_nat_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

change_meb_nat_march_red <- change_meb_nat_march %>% select(contains("food"), contains("full"))


percent_change_meb_national <- cbind(change_meb_nat_current_to_last, change_meb_nat_march)
percent_change_meb_national$regions <- "national"

percent_change_meb_national <- percent_change_meb_national %>% select(contains("food"), contains("full"), regions)

## Add to relevant df
change_national_march  <- cbind(change_national_march, change_meb_nat_march_red)
change_national_last_round <- cbind(change_national_last_round, change_meb_nat_current_to_last_red)




# Calculate percentage change between rounds - Regional
meb_reg_pct_change_current_to_last <- meb_items_regional %>% split.data.frame(., factor(meb_items_regional$collection_order))

change_meb_reg_current_to_last <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_reg_pct_change_current_to_last[["4"]], meb_reg_pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_reg_current_to_last) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round") 

change_meb_reg_current_to_last_red <- change_meb_reg_current_to_last %>% select(contains("food"), contains("full"))


change_meb_reg_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_reg_pct_change_current_to_last[["4"]], meb_reg_pct_change_current_to_last[["1"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_reg_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

change_meb_reg_march_red <- change_meb_reg_march %>% select(contains("food"), contains("full"))

percent_change_meb_regional <- cbind(change_meb_reg_current_to_last_red, change_meb_reg_march_red)


percent_change_meb_regional$regions <- c("south West", "west Nile")

# Add to relevant df
change_region_last_round <- cbind(change_region_last_round, percent_change_meb_regional) %>% select(-contains("march"))
change_region_march <- cbind(change_region_march, percent_change_meb_regional) %>% select(-contains("last_round"))

reg_list <- list(percent_change_region, change_region_last_round, change_region_march)

percent_change_region <- purrr::reduce(reg_list, left_join)

# Calculate percentage change between rounds - Settlement
meb_set_pct_change_current_to_last <- meb_items %>% select(-district, -regions, -month) %>% split.data.frame(., factor(meb_items$collection_order))

change_meb_set_current_to_last <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_set_pct_change_current_to_last[["4"]], meb_set_pct_change_current_to_last[["3"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_set_current_to_last) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_last_round") 

change_meb_set_current_to_last_red <- change_meb_set_current_to_last %>% select(contains("food"), contains("full"))

change_meb_set_current_to_last_red$settlement <- meb_set_pct_change_current_to_last[["3"]]$settlement


change_meb_set_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, meb_set_pct_change_current_to_last[["4"]], meb_set_pct_change_current_to_last[["1"]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_meb_set_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

change_meb_set_march_red <- change_meb_set_march %>% select(contains("food"), contains("full"))
change_meb_set_march_red$settlement <- meb_set_pct_change_current_to_last[["1"]]$settlement

percent_change_meb_settlement <- left_join(change_meb_set_current_to_last_red, change_meb_set_march_red)

# Add to relevant df
change_settlement_last_round <- left_join(change_settlement_last_round, percent_change_meb_settlement) %>% select(-contains("march"))
change_settlement_march <- left_join(change_settlement_march, percent_change_meb_settlement) %>% select(-contains("last_round"))

set_list <- list(change_settlement, change_settlement_last_round, change_settlement_march)

change_settlement <- purrr::reduce(set_list, left_join)


