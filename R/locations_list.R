## Uganda Market Monitoring - Settlemend location management
## Last modified 07/06/2020


# Load locations data
settlement_data <- read.csv("inputs/settlement_list.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
district_data <- read.csv("inputs/Districts_list.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))


# Create new variable with district name in lower case
district_data <- district_data %>%  mutate(district = str_to_sentence(DName2019) )
district_data$district <- tolower(district_data$district)


# Aggregate coordinates for Adjumani and add the aggregated value to the settlements coordinates list
Adjumani_coordinates <- settlement_data %>% filter(DISTRICT == "adjumani") %>% 
  select(DISTRICT,Longitude,Latitude) %>% 
  group_by(DISTRICT) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(NAME0 = "adjumani")

settlement_data <- bind_rows(settlement_data,Adjumani_coordinates)
names(settlement_data)[4] <- "settlement"

colnames(settlement_data) <- tolower(colnames(settlement_data))

rm(Adjumani_coordinates)
