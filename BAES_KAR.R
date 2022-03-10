#KAR Update for converting possibly impaired using BAES methodology

#read in the species/metrics files.

temp = list.files(path = "C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Macro_ITS/",
                  pattern = "*.csv",full.names = TRUE)
names=list.files(path = "C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Macro_ITS/",
                 pattern = "*.csv",full.names = FALSE)
myfiles=lapply(setNames(temp, make.names(gsub("*.csv$", "", names))), 
       read.csv)
sites<-read.csv("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Sites_ITS/Master_S_Site_v2_created_2021_12_07.csv",
                          fileEncoding = "UTF-8-BOM")

#now they're in a nice tidy list
metrics<-myfiles$metrics.with.all.fields

#Convert dates to date values
myfiles$metrics.with.all.fields$DATE <- as.Date(myfiles$metrics.with.all.fields$MSSIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
#Create a new column for sampling month
myfiles$metrics.with.all.fields$month <- format(myfiles$metrics.with.all.fields$DATE,"%m")

#pull records where month is between 7-9
mon = myfiles$metrics.with.all.fields$month == "07" | myfiles$metrics.with.all.fields$month == "08" | myfiles$metrics.with.all.fields$month == "09"
myfiles$metrics.with.all.fields <- myfiles$metrics.with.all.fields[mon,]
rm(mon)

#restricting to only multiplate and kick samples
sample = myfiles$metrics.with.all.fields$MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM=="1"|
  myfiles$metrics.with.all.fields$MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM=="2"|
  myfiles$metrics.with.all.fields$MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM=="5"
  # not 3, this is PONAR,myfiles$metrics.with.all.fields$MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM=="3"
myfiles$metrics.with.all.fields<-myfiles$metrics.with.all.fields[sample,]
#rm(sample)

#restricting to only one replicate -question for Alene, why didn't she summarize it?
myfiles$metrics.with.all.fields<-myfiles$metrics.with.all.fields[myfiles$metrics.with.all.fields$MSSIH_REPLICATE=="1",]

myfiles$metrics.with.all.fields<-myfiles$metrics.with.all.fields %>% 
  dplyr::rename(bap=MMDH_BIO_ASMT_PROFILE_SCORE)

#pull only the most recent date
data1<-aggregate(myfiles$metrics.with.all.fields$DATE,by=list(myfiles$metrics.with.all.fields$MSSIH_EVENT_SMAS_HISTORY_ID),max)
names(data1)[names(data1)=="Group.1"]<-"SITE_ID"
names(data1)[names(data1)=="x"]<-"DATE"
data<-merge(data1,myfiles$metrics.with.all.fields,by.x=c("SITE_ID","DATE"),
            by.y=c("MSSIH_EVENT_SMAS_HISTORY_ID","DATE"),
            all=FALSE)
rm(data1)

#modify species table for merge
species<-merge(myfiles$MASTER_20220128_S_MACRO_SPECIES_DATA_HISTORY,
               myfiles$MASTER_20220128_S_MACRO_SPECIES_SAMP_INF_HIST,
               by.x="MSDH_LINKED_ID_VALIDATOR",
               by.y="MSSIH_LINKED_ID_VALIDATOR")


myfiles$MASTER_20220128_S_MACRO_SPECIES_DATA_HISTORY$sample_id<-paste(species$BASIN,"-",species$LOCATION,"-",species$RIVMILE,sep="")
species$DATE <- as.Date(species$MSSIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
species<-unique(species[c("MSSIH_EVENT_SMAS_HISTORY_ID",
                          "DATE",
                          "MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM",
                          "MSSIH_REPLICATE",
                          "MSTR_MACRO_SPECIES_ID")])
species<-species %>% 
  dplyr::rename(SITE_ID=MSSIH_EVENT_SMAS_HISTORY_ID,
         COLLECT=MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM,
         REPLICATE=MSSIH_REPLICATE,
         genspecies=MSTR_MACRO_SPECIES_ID)
data<-data %>% 
  dplyr::rename(COLLECT=MSSIH_BIOSAMPLE_COLLECT_METHOD_NUM,
         REPLICATE=MSSIH_REPLICATE)

#merge the tables
data1<-merge(data,species,
            by=c("SITE_ID","DATE","COLLECT","REPLICATE"),
              all=FALSE)
rm(species)
data1<-unique(data1[c("SITE_ID","DATE","genspecies","bap")])
data$WAVE<-NA

SBU<-data1
rm(data)


#Pull bug names from the table
WAVE = read.csv("data/All.WAVE.Families.csv")
#convert the names to characters
WAVE$Family <- as.character(WAVE$Family)
WAVE$GENSPECIES <- as.character(WAVE$GENSPECIES)
names(WAVE)[names(WAVE)=="GENSPECIES"]<- "genspecies"
WAVE$genspecies<-gsub(" ","_",WAVE$genspecies)
WAVE$genspecies<-tolower(WAVE$genspecies)

#Create a file with the WAVE Organisms Named
#this will remove organisms that aren't on the WAVE Families list
SBU2 <- merge(SBU,WAVE,by=c("genspecies"))
#remove WAVE column
keep <- c("SITE_ID","DATE","bap","genspecies","Family")
SBU2 <- SBU2[keep]
rm(keep)
#rename Family column WAVE column
names(SBU2)[names(SBU2)=="Family"]<-"WAVE"
#Convert the family names to characters
SBU2$WAVE <- as.character(SBU2$WAVE)
#Produces a SBU file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
SBU2 <- unique(SBU2[c("SITE_ID","DATE","bap","WAVE")])
rm(WAVE)


#Create impact column
SBU2$impact <- "0"
SBU2$impact <- ifelse(SBU2$bap<5,"3.Low",SBU2$impact)
SBU2$impact <- ifelse(SBU2$bap>=7.5,"1.High",SBU2$impact)
SBU2$impact <- ifelse(SBU2$impact==0,"2.Slight",SBU2$impact)

#Create WAVE.Family Frequencies
#first count the frequency of each combination
library(plyr)
SBUFreq <- count(SBU2[c("WAVE","impact")])
#now restructure the table
library(reshape)
SBUFreq <- reshape::cast(SBUFreq,WAVE~impact)

#Get total values for each impact
junk <- unique(SBU2[c("SITE_ID","DATE","impact")])
total <- count(junk,c("impact"))
rm(junk)
high <- total$freq[1]
slight <- total$freq[2]
low <- total$freq[3]
rm(total)

#Create probability tables
SBUProb <- SBUFreq
SBUProb$`1.High` <- SBUFreq$`1.High`/high
SBUProb$`2.Slight` <- SBUFreq$`2.Slight`/slight
SBUProb$`3.Low` <- SBUFreq$`3.Low`/low
#convert NA values to 0
SBUProb[is.na(SBUProb)]<- 0
rm(high)
rm(low)
rm(slight)
rm(SBUFreq)
rm(SBU)

#write file and close program to restart with scripting below
write.csv(SBUProb, file="SBUProb.csv",row.names=FALSE)

#read the raw data back in and merge with the probablilities
########################################################################################
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

bugs.short2<-bugs %>% 
  select(ParentGlobalID,Final.ID,Family) %>% 
  distinct()

combo<-merge(sample,bugs.short,by.x="GlobalID",by.y="ParentGlobalID")

combo2<-merge(sample,bugs.short2,by.x="GlobalID",by.y="ParentGlobalID")

combo3<-combo2 %>% 
  select(GlobalID,Sample.Date,Family)

###################################################################################

#merge Sample and Prob tables
data<-merge(SBUProb,combo3,by.x = c("WAVE"),
            by.y=c("Family"),
            all=FALSE)
#rm(list=c("Sample","SBUProb"))

data<-data %>% 
  dplyr::rename(sample_id=GlobalID)

##############################################################################################################
#Calculate prob for each sample
##############################################################################################################


#calculate the prob of high, slight, low for each sample 
#find out how many samples I have to run
samples = unique(data$sample_id)
nsamples <- length(samples)

#for the first sample
sample = data$sample_id==samples[1]
WAVEsample <- data[sample,]
WAVEsample$`1.High`<- prod(WAVEsample$`1.High`)
WAVEsample$`2.Slight`<- prod(WAVEsample$`2.Slight`)
WAVEsample$`3.Low`<- prod(WAVEsample$`3.Low`)
WAVEsample <- unique(WAVEsample[c("sample_id","Sample.Date","1.High","2.Slight","3.Low")])
highT <- WAVEsample$`1.High`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
slightT <- WAVEsample$`2.Slight`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
lowT <- WAVEsample$`3.Low`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
WAVEsample$`1.High`<- highT
WAVEsample$`2.Slight`<- slightT
WAVEsample$`3.Low`<- lowT
ProbSamp <- WAVEsample
rm(list=c("WAVEsample","sample","slightT","lowT","highT"))



#and now for all subsequent samples
for(i in 2:nsamples){
  sample = data$sample_id==samples[i]
  WAVEsample <- data[sample,]
  WAVEsample$`1.High`<- prod(WAVEsample$`1.High`)
  WAVEsample$`2.Slight`<- prod(WAVEsample$`2.Slight`)
  WAVEsample$`3.Low`<- prod(WAVEsample$`3.Low`)
  WAVEsample <- unique(WAVEsample[c("sample_id","Sample.Date","1.High","2.Slight","3.Low")])
  highT <- WAVEsample$`1.High`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  slightT <- WAVEsample$`2.Slight`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  lowT <- WAVEsample$`3.Low`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  WAVEsample$`1.High`<- highT
  WAVEsample$`2.Slight`<- slightT
  WAVEsample$`3.Low`<- lowT
  ProbSamp <- merge(ProbSamp,WAVEsample,all=TRUE)
  rm(list=c("WAVEsample","sample","slightT","lowT","highT"))
}
rm(list=c("i","nsamples","samples"))

#restrict it to only those sites where Prob Low is >50%
ProbSamp<-unique(ProbSamp[c("sample_id","Sample.Date","3.Low")])
ProbSamp<-ProbSamp[ProbSamp$`3.Low`>.5,]
ProbSamp$PossiblyImpaired <-"yes"

library(dplyr)
#remerge with original to get the lat/longs of the sites
sample.short<-sample %>% 
  dplyr::select(GlobalID,Enter.the.sample.Latitude,Enter.the.sample.Longitude,Stream.Name) %>% 
  dplyr::rename(Latitude=Enter.the.sample.Latitude,
         Longitude=Enter.the.sample.Longitude)

ProbSamp<-merge(ProbSamp,sample.short,by.x="sample_id",by.y="GlobalID")
write.csv(ProbSamp,file="2021/Recommended.WAVE.Sites.csv",row.names=FALSE)


#pull all of the recommended WAVE sites for a round up
r.2019<-read.csv(here::here("2020/Recommended.WAVE.Sites.csv"),stringsAsFactors = FALSE)
combo.rec<-r.2019 %>% 
  dplyr::rename("3.Low"=X3.Low) %>% 
  mutate(sample_id="",
         Stream.Name="")
combo.rec<-rbind.fill(combo.rec,ProbSamp)

combo.rec$Sample.Date<-as.Date(combo.rec$Sample.Date,"%m/%d/%Y")
write.csv(combo.rec,file="2021/Recommended.WAVE.Sites_all.csv",row.names=FALSE)

