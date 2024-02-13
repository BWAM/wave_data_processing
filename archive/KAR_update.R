#Keleigh Reynolds
#WAVE workup
#fixing files from survey123 to work with the scripts here
library(dplyr)

bugs<-read.csv("2023/bug_repeat_1.csv",stringsAsFactors = FALSE)
sample<-read.csv("2023/WAVE_bug_id_0.csv",stringsAsFactors = FALSE)
field<-read.csv("2023/S_WSEI_SAMPLE_EVENT_INFO_0.csv")

MACROS <- read.csv("lookuptables/Families.csv")

bugs.short<-bugs %>% 
  select(ParentGlobalID,Final.ID) %>% 
  distinct()
bugs.short$macro_num<-ave(bugs.short$ParentGlobalID, bugs.short$ParentGlobalID, FUN=seq_along)
#this one created a grouping column to be able to spread the macroid's-don't actually have to do that

bugs.short<-bugs.short %>% 
  tidyr::spread(key=macro_num,value=Final.ID)

#this will be useful for the email i think

bugs.short2<-bugs %>% 
  select(ParentGlobalID,Final.ID) %>% 
  distinct()

combo<-merge(sample,bugs.short,by.x="GlobalID",by.y="ParentGlobalID",all.x = TRUE)
#collapse columns into one


combo2<-merge(sample,bugs.short2,by.x="GlobalID",by.y="ParentGlobalID",all.x = TRUE)

#merge macros and the samples

ASSESSMENT<-merge(combo2,MACROS,by.x="Final.ID",by.y="Macroinvertebrate.Family")
#Make numeric columns
ASSESSMENT$Most <- as.numeric(ASSESSMENT$Most)
ASSESSMENT$Least <- as.numeric(ASSESSMENT$Least)
ASSESSMENT$IWLScore <- as.numeric(ASSESSMENT$IWLScore)

ASSESSMENT<-ASSESSMENT %>% 
  group_by(GlobalID,Sample.Date) %>% 
  summarize(Most = sum(Most,na.rm=TRUE),
            Least = sum(Least,na.rm=TRUE),
            IWLScore = sum(IWLScore,na.rm=TRUE)) %>% 
  ungroup()
#Convert scores to assessment categories
ASSESSMENT$assessment <- NA
ASSESSMENT$assessment <- ifelse(ASSESSMENT$Least<(-3),"Possibly Impaired",ASSESSMENT$assessment)
ASSESSMENT$assessment <- ifelse(ASSESSMENT$Most>5,"No Known Impact",ASSESSMENT$assessment)
ASSESSMENT$assessment <- ifelse(ASSESSMENT$Most<6&ASSESSMENT$Least>(-4),"No Conclusion",ASSESSMENT$assessment)
ASSESSMENT$IWL <- NA
ASSESSMENT$IWL <- ifelse(ASSESSMENT$IWLScore>23,"Excellent",ASSESSMENT$IWL)
ASSESSMENT$IWL <- ifelse(ASSESSMENT$IWLScore<24&ASSESSMENT$IWLScore>17,"Good",ASSESSMENT$IWL)
ASSESSMENT$IWL <- ifelse(ASSESSMENT$IWLScore<18&ASSESSMENT$IWLScore>11,"Fair",ASSESSMENT$IWL)
ASSESSMENT$IWL <- ifelse(ASSESSMENT$IWLScore<12,"Poor",ASSESSMENT$IWL)

#merge back with original to get the assessments
combo2<-combo2 %>% 
  select(!c(Final.ID,"ObjectID" )) %>% 
  distinct()

final<-merge(combo2,ASSESSMENT,by="GlobalID",all.x = TRUE)
#merge with the wide bugs to get what was in there
final<-merge(final,bugs.short,by.x = "GlobalID",by.y="ParentGlobalID")


# write the tables
#write.table(final,file="2022/Assessment_check.csv",sep=",",row.names=FALSE)
write.table(field, file ="2023/field_check_match_ACnum.csv",sep = ",",row.names = FALSE)


# Come back here when AC numbers and emails are checked -------------------


combo3<-merge(ASSESSMENT,bugs.short,by.x="GlobalID",by.y="ParentGlobalID",all.x = TRUE)

#get accession# and globalID to merge with field sheets
#read matched file back in
field<-read.csv("2023/field_check_match_ACnum.csv")

final.short<-final %>% 
  select(Please.enter.the.Sample.Accession.Number,GlobalID) %>% 
  distinct()

combo4<-merge(combo3,final.short,by="GlobalID",all.x = TRUE)
combo5<-merge(combo4,field,
              by.x="Please.enter.the.Sample.Accession.Number",
              by.y="AC_number",
              all.x = TRUE)
combo5<-combo5 %>% 
  select(-c("ï..ObjectID","GlobalID.y")) %>% 
  distinct()

bugs.short3<-bugs %>% 
  select(ParentGlobalID,Final.ID) %>% 
  distinct() %>% 
  group_by(ParentGlobalID) %>% 
  across(funs(paste(.,collapse=",")))

# combo5<-combo5 %>% 
#   select(!Final.ID) %>% 
#   distinct()

combo6<-merge(combo5,bugs.short3, by.x="GlobalID.x",by.y="ParentGlobalID",all.x = TRUE)
combo6<-combo6 %>% 
  select(!GlobalID.x) %>% 
  distinct()

write.csv(combo6,file = "2022/all.with.assessment_check.csv",row.names = FALSE)
#bring in the checked file

combo7<-read.csv(here::here("2022/all.with.assessment_check.csv"))

# site.sub<-read.csv("2021/site_submission.csv",stringsAsFactors = FALSE)
# 
# combo5<-site.sub %>% 
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   mutate(latlon=paste(latitude,longitude,sep="_")) %>% 
#   mutate(names=paste(First.Name,Last.Name,sep = " ")) %>% 
#   select(names,Email.Address,latlon,What.is.the.name.of.this.stream.)
# 
# 
# site.sub<-site.sub %>% 
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   mutate(latlon=paste(latitude,longitude,sep="_")) %>% 
#   select(What.is.the.name.of.this.stream.,First.Name,Last.Name,Email.Address,latlon)

# combo4<-merge(combo3,site.sub,by.x="waterbody_name",by.y="What.is.the.name.of.this.stream.",all.x=TRUE)
# combo4<-combo4 %>% 
#   distinct()
# 
# bug.small<-combo4 %>% 
#   select(Please.enter.the.Sample.Accession.Number,Final.ID) %>% 
#   distinct() %>% 
#   group_by(Please.enter.the.Sample.Accession.Number) %>% 
#   summarise_each(funs(paste(.,collapse=",")))
# 
# combo4<-combo4 %>% 
#   select(!c(Final.ID,latlon)) %>% 
#   distinct()
# 
# combo4<-merge(combo4,bug.small,by="Please.enter.the.Sample.Accession.Number",all.x = TRUE)
# 
# 
# #try again with the lat/longs and the sampler's names
# write.csv(combo4,"2020/emails_all.csv")

c2022<-read.csv("2022/all.with.assessment_check.csv")

colnames(c2022) = gsub("S_", "", colnames(c2022))

write.csv(c2022,"2022/all.with.assessment_22_fix.csv")
