#Keleigh Reynolds
#WAVE 2020 workup
#fixing files from survey123 to work with the scripts here
library(dplyr)

bugs<-read.csv("2021/bug_repeat_1.csv",stringsAsFactors = FALSE)
sample<-read.csv("2021/WAVE_bug_id_0.csv",stringsAsFactors = FALSE)
field<-read.csv("2021/S_WSEI_SAMPLE_EVENT_INFO_0.csv")

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

combo<-merge(sample,bugs.short,by.x="GlobalID",by.y="ParentGlobalID")
#collapse columns into one


combo2<-merge(sample,bugs.short2,by.x="GlobalID",by.y="ParentGlobalID")

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
  select(!c(Final.ID)) %>% 
  distinct()

final<-merge(combo2,ASSESSMENT,by="GlobalID",all.x = TRUE)
#merge with the wide bugs to get what was in there
final<-merge(final,bugs.short,by.x = "GlobalID",by.y="ParentGlobalID")


# write the tables
write.table(final,file="2021/Assessment_check.csv",sep=",",row.names=FALSE)

combo3<-merge(ASSESSMENT,bugs.short2,by.x="GlobalID",by.y="ParentGlobalID")

#get accession# and globalID to merge with field sheets
final.short<-final %>% 
  select(Please.enter.the.Sample.Accession.Number,GlobalID)

combo3<-merge(combo3,final.short,by="GlobalID",all.x = TRUE)
combo3<-merge(combo3,field,
              by.x="Please.enter.the.Sample.Accession.Number",
              by.y="Step.4..The.fun.part.",all.x=TRUE)
combo3<-combo3 %>% 
  distinct()

bugs.short3<-bugs %>% 
  select(ParentGlobalID,Final.ID) %>% 
  distinct() %>% 
  group_by(ParentGlobalID) %>% 
  summarise_each(funs(paste(.,collapse=",")))

combo3<-combo3 %>% 
  select(!Final.ID) %>% 
  distinct()

combo3<-merge(combo3,bugs.short3, by.x="GlobalID.x",by.y="ParentGlobalID",all.x = TRUE)
combo3<-combo3 %>% 
  select(!GlobalID.x) %>% 
  distinct()

write.csv(combo3,file = "2021/all.with.assessment_check.csv",row.names = FALSE)
#bring inthe site submission

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

c2021<-read.csv("2021/all.with.assessment_21.csv")

colnames(c2021) = gsub("S_", "", colnames(c2021))

write.csv(c2021,"2021/all.with.assessment_21_fix.csv")
