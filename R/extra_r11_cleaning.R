# nov - round 11 data cleaning

month_specific_cleaning<-function(df){
  south_west<-c("kyaka", "kyangwali", "nakivale", "oruchinga", "rwamwanja")
  west_nile<- c("adjumani", "rhino camp", "bidibidi", "imvepi", "kiryandongo", 
                "lobule", "palabek", "palorinya")
  df %>% 
  mutate(vendors_change= case_when(month==11 &vendors_change=="Less"~NA_character_,
                                   TRUE~vendors_change),
         weight_firewood= case_when(month==11 & 
                                      settlement=="kiryandongo" & 
                                      weight_firewood==2~6,
                                    TRUE~weight_firewood),
         regions = case_when(
           yrmo>202012 & settlement %in% south_west~"south west",
           yrmo>202012 & settlement %in% west_nile~"west nile",
           TRUE ~ regions
           )
         
         )
}

 
