# nov - round 11 data cleaning

extra_round11_cleaning<-function(df){df %>% 
  mutate(vendors_change= case_when(month==11 &vendors_change=="Less"~NA_character_,
                                   TRUE~vendors_change),
         weight_firewood= case_when(month==11 & 
                                      settlement=="kiryandongo" & 
                                      weight_firewood==2~6,
                                    TRUE~weight_firewood))
}


