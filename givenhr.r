allyrs.12vars %>% filter(hr==TRUE) %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table
