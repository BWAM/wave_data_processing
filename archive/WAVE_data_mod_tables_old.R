#WAVE data tables for data modernization effort
#9/30/2020
#Keleigh Reynolds

#first grab the data
#it might all be in the 2019 folder....trying to keep it all to one flat file might be the easiest route.
#then try and marry them outside of the db?

library(dplyr)
##########################################################################################
#raw bug data

sample<-read.csv("data/2019/sample.final.csv",stringsAsFactors = FALSE)
#change to date format
sample$Collection.Date<-as.Date(sample$Collection.Date,"%m/%d/%Y")
sample$date<-format(sample$Collection.Date,"%Y%m%d")
#make unique ID
sample$SBUID<-paste(sample$Latitude,sample$Longitude,sample$date,sep = "_")

#see if they are all in there
#sample_18<-read.csv("data/2018/sample.final.csv",stringsAsFactors = FALSE)
#sample_18$Collection.Date<-as.Date(sample_18$Collection.Date,"%m/%d/%Y")
#sample_18$SBUID<-paste(sample_18$Latitude,sample_18$Longitude,sample_18$Collection.Date,sep = "_")


#sample_check<-anti_join(sample,sample_18,by="SBUID")

#sample_17<-read.csv("data/2017/sample.final.csv",stringsAsFactors = FALSE)
#sample_17$Collection.Date<-as.Date(sample_17$Collection.Date,"%m/%d/%Y")
#sample_17$SBUID<-paste(sample_17$Latitude,sample_17$Longitude,sample_17$Collection.Date,sep = "_")

#sample.check.2<-anti_join(sample,sample_17,by="SBUID")


#ok I think that the 2019 file houses everything to date, so that's good.
#rm(sample_17,sample_18,sample.check.2,sample_check)

#check the same things with the other sets of files ############################################
sites<-read.csv("data/2019/Sites.final.csv",stringsAsFactors = FALSE)
#ths might have to wait, thinking that I could join them by GIS? But i coudl leave it blank for now.

sample<-sample %>% 
  rename("W_SAMPLE_DATE"=Collection.Date,
         "W_LATITUDE"=Latitude,
         "W_LONGITUDE"=Longitude,
         "W_MACRO_FAMILY_ID"=Macroinvertebrate.Family,
         "W_SBUID"=SBUID)
#make family all upper case
sample$W_MACRO_FAMILY_ID<-toupper(sample$W_MACRO_FAMILY_ID)


###############################################################################################
#Visual assessment data and final assessment

#Visual
visual<-read.csv("data/2019/Visual.final.csv",stringsAsFactors = FALSE)
visual$date<-as.Date(visual$date,"%m/%d/%Y")
visual$date.1<-format(visual$date,"%Y%m%d")
visual$SBUID<-paste(visual$Latitude,visual$Longitude,visual$date.1,sep = "_")

#check that the files are additive
#visual.18<-read.csv("data/2018/Visual.final.csv",stringsAsFactors = FALSE)
#visual.18$SBUID<-paste(visual.18$Latitude,visual.18$Longitude,visual.18$Collection.Date,sep = "_")
#rm(visual.18,visual.check)

#make all characters into lower case
visual.1<-mutate_all(visual,funs(tolower))

#make teh tables
wave.assess<-visual.1 %>% 
  select(SBUID,assessment) %>% 
  rename("S_WSEI_SAMPLE_ID"=SBUID,
         "S_WA_ASSESSMENT"=assessment) %>% 
  mutate("S_WSEI_EVENT_SAMPLE_ID"="","S_WA_EVENT_SAMPLE_ID"="")

wave.bugs.raw<-sample %>% 
  select(W_SBUID,W_MACRO_FAMILY_ID) %>% 
  rename("S_WSEI_SAMPLE_ID"=W_SBUID,
         "S_WMFDH_MACRO_FAMILY"=W_MACRO_FAMILY_ID) %>% 
  mutate("S_WSEI_EVENT_SAMPLE_ID"="","S_WMFDH_EVENT_SAMPLE_ID"="")

#take out blanks, there should not be any
wave.bugs.raw<-wave.bugs.raw %>% 
  filter(S_WMFDH_MACRO_FAMILY!="")

wave.sample<-visual.1 %>% 
  select(SBUID,Latitude, Longitude,date,X1o,X2o.contact,Water.Clarity,Phytoplankton,
         Periphyton.Cover,Macrophyte,Odor,Trash,Discharges.pipes,primary.deterrant,
         weather..cur.,weather.24hr,Other,substrate.cover,Embeddedness.,Velocity.Depth,
         sediment,channel.flow,Channel.Alteration,riffle.frequency,Bank.Stab..L..,
         Bank.Stab..R.,Bank.Veg..L..,Bank.Veg..R..,Riparian.Veg..L.,Riparian.Veg..R.) %>% 
  rename("S_WSEI_SAMPLE_ID"=SBUID,
         "S_WSEI_LATITUDE"=Latitude, 
         "S_WSEI_LONGITUDE"=Longitude,
         "S_WSEI_COLLECTION_DATE"=date,
         "S_WSEI_PRIMARY_CONTACT"=X1o,
         "S_WSEI_SECONDARY_CONTACT"=X2o.contact,
         "S_WSEI_WATER_CLARITY"=Water.Clarity,
         "S_WSEI_SUSPENDED_PHYTOPLANKTON"=Phytoplankton,
         "S_WSEI_PERIPHYTON"=Periphyton.Cover,
         "S_WSEI_MACROPHYTE"=Macrophyte,
         "S_WSEI_ODOR"=Odor,
         "S_WSEI_TRASH"=Trash,
         "S_WSEI_DISCHARGE_PIPE"=Discharges.pipes,
         "S_WSEI_PRIMARY_VARIABLE"=primary.deterrant,
         "S_WSEI_CURRENT_WEATHER"=weather..cur.,
         "S_WSEI_PREV_WEATHER"=weather.24hr,
        "S_WSEI_USER_NOTES"= Other,
        "S_WSEI_EPIFAUNAL_COVER"=substrate.cover,
         "S_WSEI_EMBEDDEDNESS_POOLING"=Embeddedness.,
         "S_WSEI_VELOCITY_DEPTH_REGIME"=Velocity.Depth,
         "S_WSEI_SEDIMENT_DEPOSITION"=sediment,
         "S_WSEI_FLOW_STATUS"=channel.flow,
         "S_WSEI_CHANNEL_ALTERATION"=Channel.Alteration,
         "S_WSEI_RIFFLE_BEND_FREQUENCY"=riffle.frequency,
        "S_WSEI_LEFT_BANK_STABILITY"= Bank.Stab..L..,
        "S_WSEI_RIGHT_BANK_STABILITY"= Bank.Stab..R.,
         "S_WSEI_LEFT_BANK_VEG"=Bank.Veg..L..,
         "S_WSEI_RIGHT_BANK_VEG"=Bank.Veg..R..,
         "S_WSEI_LEFT_BANK_VEG_ZONE"=Riparian.Veg..L.,
         "S_WSEI_RIGHT_BANK_VEG_ZONE"=Riparian.Veg..R.) %>% 
  mutate("S_WSEI_EVENT_SAMPLE_ID"="") %>% 
  distinct()

wave.sample[wave.sample==NA]<-"" 
  
wave.sample<-wave.sample %>% 
  distinct()

#checking site unique
site<-wave.sample %>% 
  group_by(S_WSEI_SAMPLE_ID) %>% 
  mutate(n=n()) %>% 
  select(S_WSEI_SAMPLE_ID,n) %>% 
  subset(n>1) %>% 
  distinct()

#write to file
write.csv(wave.sample,"outputs/S_WAVE_SAMPLE_EVENT_INFO.csv",row.names = FALSE)
write.csv(wave.bugs.raw,"outputs/S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv",row.names = FALSE)
write.csv(wave.assess,"outputs/S_WAVE_ASSESSEMENT.csv",row.names = FALSE)
