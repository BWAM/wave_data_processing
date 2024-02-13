#making db tables
library(dplyr)
year = 2023


bugs<-read.csv(here::here("2023/bug_repeat_1.csv"))
sample<-read.csv(here::here("2023/WAVE_bug_id_0.csv"))
field<-read.csv(here::here("2023/WAVE_2023_matched.csv"))

#assessment for 2023
assess.2023<-read.csv(here::here("2023/all_with_assessment_for_emails.csv"),stringsAsFactors = FALSE)

#filter for just this year's
bugs<-bugs %>% 
  mutate(date = format(as.Date(CreationDate,"%m/%d/%Y %H:%M:%S"))) %>% 
  filter(date >= year)

sample<-sample %>% 
  mutate(date = format(as.Date(Sample.Date,"%m/%d/%Y %H:%M:%S"))) %>% 
  filter(date >=year)

field<-field %>% 
  mutate(date = format(as.Date(Date,"%m/%d/%Y %H:%M"))) %>% 
  filter(date >= year)

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
  select(S_WSEI_SAMPLE_ID,AC_number) %>%
  dplyr::rename(Accession.Number=AC_number)

bugs.2020<-merge(combo.short,field.short,by.x="Please.enter.the.Sample.Accession.Number",
                 by.y="Accession.Number") %>% 
  distinct()
bugs.2020<-bugs.2020 %>% 
  select(S_WSEI_SAMPLE_ID,Final.ID) %>% 
  dplyr::rename(WMFDH_MACRO_FAMILY=Final.ID,
         WMFDH_SAMPLE_ID=S_WSEI_SAMPLE_ID)
bugs.2020$WMFDH_MACRO_FAMILY<-toupper(bugs.2020$WMFDH_MACRO_FAMILY)

write.csv(bugs.2020,
          here::here("outputs/db_tables/20240209_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY_append.csv"),
          row.names = FALSE)


####################################################################
#assessments

assess.2023.short<-assess.2023 %>% 
  select(S_WSEI_SAMPLE_ID,assessment) %>% 
  distinct() %>% 
  dplyr::rename(WA_SAMPLE_ID=S_WSEI_SAMPLE_ID,
         WA_ASSESSMENT=assessment)
assess.2023.short$WA_ASSESSMENT<-tolower(assess.2023.short$WA_ASSESSMENT)

write.csv(assess.2023.short,
          "outputs/db_tables/20240209_S_WAVE_ASSESSMENT_append.csv",
          row.names = FALSE)
######################################################################
#sample event info

field.table<-field %>% 
  select_if(grepl("S_WSEI_",colnames(field))|
              grepl("Date",colnames(field))|
              grepl("Choose.the.one.answer*",colnames(field))|
              grepl("Previous.24.hrs.Weather",colnames(field))|
              grepl("Weather",colnames(field))|
            grepl("Choose.all.the.variables",colnames(field)))

field.table<-field.table %>% 
  rename(WSEI_PRIMARY_CONTACT="Choose.the.one.answer.which.best.describes.your.ability.and.desire.to.participate.in.primary.contact.recreation..swimming.." ,
         WSEI_CURRENT_WEATHER="Weather", 
         WSEI_SECONDARY_CONTACT ="Choose.the.one.answer.which.best.describes.your.ability.and.desire.to.participate.in.secondary.contact.recreation..boating.fishing..",
         WSEI_PREV_WEATHER="Previous.24.hrs.Weather",
         WSEI_PRIMARY_VARIABLE = "Choose.all.the.variables.that.negatively.affect.your.opinion.of.recreational.use.of.the.waterbody.today.")

colnames(field.table)=gsub("S_","",colnames(field.table))

field.table<-field.table %>% 
  relocate(WSEI_SAMPLE_ID,.before=WSEI_LATITUDE)
#fix dates

field.table$WSEI_COLLECTION_DATE<-format(as.Date(field.table$Date,"%m/%d/%Y"),"%m/%d/%Y")

field.names<-names(field.table)
col.db<-unique(colnames$WAVE_SAMPLE_EVENT_INFO)

nonmatch<-setdiff(col.db,field.names)
#fix missing notes colm
field.table$WSEI_USER_NOTES<-""

#write to table
write.csv(field.table,
          "outputs/db_tables/20240209_S_WAVE_SAMPLE_EVENT_INFO_append.csv",
          row.names = FALSE)
#######################################################################################
#combine into one master file
old.wave<-read.csv("L:/DOW/BWAM Share/data/streams/cleaned_files/Final_WAVE_ITS/MASTER_S_WAVE_ASSESSMENT.csv")
old.bugs<-read.csv("L:/DOW/BWAM Share/data/streams/cleaned_files/Final_WAVE_ITS/MASTER_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv")
old.info<-read.csv("L:/DOW/BWAM Share/data/streams/cleaned_files/Final_WAVE_ITS/MASTER_S_WAVE_SAMPLE_EVENT_INFO.csv")


#sample event
setdiff(names(old.info),names(field.table))
field.table$Date<-NULL
field.table$CreationDate<-NULL
field.table$EditDate<-NULL
master.samp<-rbind(field.table,old.info)
setdiff(names(old.bugs),names(bugs.2020))
master.bugs<-rbind(old.bugs,bugs.2020)
setdiff(names(old.wave),names(assess.2023.short))
master.assess<-rbind(old.wave,assess.2023.short)

#write to csv
write.csv(master.samp,"outputs/db_tables/MASTER_S_WAVE_SAMPLE_EVENT_INFO.csv",row.names = FALSE)
write.csv(master.bugs,"outputs/db_tables/MASTER_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv",row.names=FALSE)
write.csv(master.assess,"outputs/db_tables/MASTER_S_WAVE_ASSESSMENT.csv",row.names = FALSE)
