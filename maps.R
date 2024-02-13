#create map for display on AGOL
#Keleigh Reynolds
#revamping before handoff 11/6/2023

#read in the final tables from the L drive

year = "2023"

wave_score<-read.csv("L:/DOW/BWAM Share/data/streams/cleaned_files/Final_WAVE_ITS/MASTER_S_WAVE_ASSESSMENT.csv")
site_info<-read.csv("L:/DOW/BWAM Share/data/streams/cleaned_files/Final_WAVE_ITS/MASTER_S_WAVE_SAMPLE_EVENT_INFO.csv")


wave_score<-wave_score %>% 
  distinct()

#create shorter lat/longs to account for slightly different spots between the years
wave_score<-wave_score %>% 
  mutate(tidyr::separate_wider_delim(wave_score,cols = WA_SAMPLE_ID, delim = "_", names = c("lat", "long","date")))

wave_score2<-wave_score %>% 
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))



wave_score3<-wave_score2 %>% 
  mutate(lat = signif(lat, digits = 5),
         long = signif(long, digits = 5)) %>% 
  select(-c(WA_SAMPLE_ID)) %>% 
  distinct() %>% 
  mutate(site_id = paste(lat, long, sep = "_")) %>% 
  mutate( lat = round(lat, digits = 3),
          long = round(long, digits = 3))

wave_score3<-wave_score3 %>% 
  mutate(year = substr(date,1,4)) %>% 
  mutate(year_cat = paste(year, WA_ASSESSMENT,sep = ": "),
         year_cat = stringr::str_to_title(year_cat))

wave_score4<-wave_score3 %>% 
  group_by(site_id) %>%
  slice(which.max(as.Date(date, '%Y%m%d')))

wave_hist<-wave_score3 %>% 
  select(site_id,year,year_cat) %>% 
  arrange(desc(year)) %>% 
  select(-c(year)) %>% 
  group_by(site_id) %>% 
  summarise(text=paste(year_cat, collapse='\n'))

merge<-merge(wave_score4,wave_hist,
             by = "site_id")

merge<-merge %>% 
  select(-c(date,year_cat)) %>% 
  arrange(lat) %>% 
  mutate(pos_long = -(long),
    diff_lat = lat - lag(lat, default = first(lat)),
         diff_long = (pos_long - lag(pos_long, default = first(pos_long)))) %>% 
  mutate(group = case_when(
    (diff_lat < 0.010) & 
      (diff_long < 0.010) & 
      year == lag(year,default = first(year)) &
      WA_ASSESSMENT == lag(WA_ASSESSMENT)
    ~ paste("group", row_number()-1),
          TRUE~ "not_grouped"
  ))

merge2<-merge %>% 
  mutate(collapse = case_when(
    group == "not_grouped"~FALSE,
    TRUE~TRUE),
    row_to_collapse = case_when(
      collapse == TRUE ~ row_number()-1,
      TRUE ~ 0
    )
  )
merge3<-merge2 %>% 
 filter(collapse == FALSE) %>% 
  mutate(Current_WAVE_Score = stringr::str_to_title(WA_ASSESSMENT)) %>% 
  select(-c(WA_ASSESSMENT,pos_long,diff_lat,diff_long,group,collapse, row_to_collapse)) %>% 
  rename(Historical_results = text)

write.csv(merge3, here::here(paste("outputs/",
                                   year,
                                   "_wave_map_dups_removed.csv",
                                   sep = "")))
