#making db tables
library(dplyr)

bugs<-read.csv(here::here("2021/bug_repeat_1.csv"))
sample<-read.csv(here::here("2021/WAVE_bug_id_0.csv"))
field<-read.csv(here::here("2021/S_WSEI_SAMPLE_EVENT_INFO_0.csv"))

#assessment for 2021
assess.2021<-read.csv(here::here("2021/all.with.assessment.csv"),stringsAsFactors = FALSE)


#read in the old ones
#just the colnames

colnames<-read.csv(here::here("lookuptables/db_colnames.csv"),stringsAsFactors = FALSE)

#########################################################################################
#bug id's
bugs.short<-bugs %>% 
  select(ParentGlobalID,Final.ID) %>% 
  distinct()

combo<-merge(sample,bugs.short,by.x="GlobalID",by.y="ParentGlobalID")

combo.short<-combo %>% 
  select(GlobalID,Please.enter.the.Sample.Accession.Number,Final.ID) %>% 
  distinct()
field.short<-field %>% 
  select(S_WSEI_SAMPLE_ID,`Step.4..The.fun.part.`) %>% 
  dplyr::rename(Accession.Number=`Step.4..The.fun.part.`)

bugs.2020<-merge(combo.short,field.short,by.x="Please.enter.the.Sample.Accession.Number",
                 by.y="Accession.Number") %>% 
  distinct()
bugs.2020<-bugs.2020 %>% 
  select(S_WSEI_SAMPLE_ID,Final.ID) %>% 
  dplyr::rename(WMFDH_MACRO_FAMILY=Final.ID,
         WMFDH_SAMPLE_ID=S_WSEI_SAMPLE_ID)
bugs.2020$WMFDH_MACRO_FAMILY<-toupper(bugs.2020$WMFDH_MACRO_FAMILY)

write.csv(bugs.2020,
          "outputs/db_tables/20220208_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY_append.csv",
          row.names = FALSE)


####################################################################
#assessments

assess.2021.short<-assess.2021 %>% 
  select(S_WSEI_SAMPLE_ID,assessment) %>% 
  distinct() %>% 
  dplyr::rename(WA_SAMPLE_ID=S_WSEI_SAMPLE_ID,
         WA_ASSESSMENT=assessment)
assess.2021.short$WA_ASSESSMENT<-tolower(assess.2021.short$WA_ASSESSMENT)

write.csv(assess.2021.short,
          "outputs/db_tables/20220208_S_WAVE_ASSESSMENT_append.csv",
          row.names = FALSE)
######################################################################
#sample event info

field.table<-field %>% 
  select_if(grepl("S_WSEI_",colnames(field)))

colnames(field.table)=gsub("S_","",colnames(field.table))

field.table<-field.table %>% 
  relocate(WSEI_SAMPLE_ID,.before=WSEI_LATITUDE)
#fix dates

field.table$WSEI_COLLECTION_DATE<-format(field.table$WSEI_COLLECTION_DATE,"%m/%d/%Y")

field.names<-names(field.table)
col.db<-unique(colnames$WAVE_SAMPLE_EVENT_INFO)

nonmatch<-setdiff(col.db,field.names)
#looks good!

#write to table
write.csv(field.table,
          "outputs/db_tables/20220208_S_WAVE_SAMPLE_EVENT_INFO_append.csv",
          row.names = FALSE)
#######################################################################################
#combine into one master file
old.wave<-read.csv("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_WAVE_ITS/MASTER_S_WAVE_ASSESSMENT.csv")
old.bugs<-read.csv("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_WAVE_ITS/MASTER_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv")
old.info<-read.csv("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_WAVE_ITS/MASTER_S_WAVE_SAMPLE_EVENT_INFO_JOIN.csv")


#sample event

master.samp<-rbind(field.table,old.info)
master.bugs<-rbind(old.bugs,bugs.2020)
master.assess<-rbind(old.wave,assess.2020.short)

#write to csv
write.csv(master.samp,"outputs/db_tables/MASTER_S_WAVE_SAMPLE_EVENT_INFO.csv",row.names = FALSE)
write.csv(master.bugs,"outputs/db_tables/MASTER_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv",row.names=FALSE)
write.csv(master.assess,"outputs/db_tables/MASTER_S_WAVE_ASSESSEMENT.csv",row.names = FALSE)
