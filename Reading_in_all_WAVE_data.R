#Reading WAVE data
#Alene ONion
#2026

ass<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/WAVE.Scripts/wave_data_processing/Outputs/db_tables/MASTER_S_WAVE_ASSESSEMENT.csv")
macros<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/WAVE.Scripts/wave_data_processing/Outputs/db_tables/MASTER_S_WAVE_MACROINVERTEBRATE_FAMILY_DATA_HISTORY.csv")
samples<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/WAVE.Scripts/wave_data_processing/Outputs/db_tables/MASTER_S_WAVE_SAMPLE_EVENT_INFO.csv")

ass<-ass %>% rename(SAMPLE_ID=WA_SAMPLE_ID)
macros<-macros %>% rename(SAMPLE_ID=WMFDH_SAMPLE_ID)
samples<-samples %>% rename(SAMPLE_ID=WSEI_SAMPLE_ID)

wave<-merge(ass,samples,by=c('SAMPLE_ID'),all=TRUE)
wave<-merge(wave,macros,by=c('SAMPLE_ID'),all=TRUE)
