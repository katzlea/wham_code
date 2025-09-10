#
#-------------------------------------------------------------
#           STEP 1 in the WHAM-workflow : SYNC
#                       by LEA KATZ
#-------------------------------------------------------------
# This script is written for syncing images from the BlueROV2 with telemetry.
# It is specifically created for analyzing images on BIIGLE, but can be adapted
# for other purposes.
# The BlueROV2 is equipped with an additional GoPro camera and a Ping-1D sonar.
# This script synchronises each individual image to the ROV telemetry.
# It needs :
# - a sonar file (from PingViewer, converted from .bin to .csv)
# - a telemetry file (as a .csv, from QGroundControl)
# - all pictures from the GoPro in one folder
# - some extra metadata (optional)
# The output is a .csv and .RData file with all info synced :
# => image name, distance to seafloor, yaw-pitch-roll, depth of ROV, temperature, etc.


#install.packages("tidyverse","lubridate")
library(tidyverse)
library(lubridate)

# -------------------------------------------------------------
# DEFINE VARIABLES
# -------------------------------------------------------------
campaign<-"TANGO2"
dive<-"ROV21"
replicate<-"t2"

freq<-10 #extracted 1 images every n second

#start_time_ping<-as.POSIXct("2023-02-23 15:14:33", #see time .bin file was created !! take care of time difference (-4)...
                           # tz="Antarctica/Rothera") 

#load metadata file
meta<-readxl::read_excel(paste0("wham_data/",campaign,"/metadata/",campaign,"_metadata.xlsx"))
dive.meta<-subset(meta,event_ID==dive & transect==replicate)

source("wham_code/functions/time_convert.R")
source("wham_code/functions/date_convert2.R")

time_str<-time_convert(dive.meta$start_GP)
dateonly_str<-date_convert2(dive.meta$date)
date_str<-paste(dateonly_str,time_str)
pingtime_str<-time_convert(dive.meta$start_ping)
ping_str<-paste(dateonly_str,pingtime_str)

start_time_images<-as.POSIXct(date_str,tz="Antarctica/Rothera")
start_time_ping<-as.POSIXct(ping_str,tz="Antarctica/Rothera")
n_images<-as.numeric(dive.meta$n_images)
end_time_images<-start_time_images + freq*seconds(n_images-1)

# -------------------------------------------------------------
# SYNC IMAGES WITH TELEMETRY AND SONAR
# -------------------------------------------------------------

# IMAGES ### !! only works if there are no other files in the directory
path="wham_data/TANGO2/PHOTOS_TEMP"
image_vec<-list.files(paste0(path)) #takes time
time_vec<-seq.POSIXt(start_time_images,end_time_images,by=freq)
image_sync<-data.frame(time_vec,image_vec)

# QGC NAVIGATION 
tlog<-read.csv(paste0("wham_data/",campaign,"/navigation/qgc_logs/",campaign,"_",dive,".csv"))
tlog$Timestamp<-as.POSIXct(tlog$Timestamp,tz="Antarctica/Rothera")

# PING SONAR
ping_raw<-read.csv(paste0("wham_data/",campaign,"/navigation/ping_logs/",campaign,"_",dive,"_sonar.csv"))
#ping<- ping_raw %>% filter(confidence==100) #keep only sonar data of dive (confidence = 100%)
punix=start_time_ping+hms(ping_raw$time)
ping<-cbind(ping_raw,punix)
ping<-ping %>% mutate(punix=substr(as.character(punix),1,19)) %>% group_by(punix) %>% summarise(bathymetry=last(bathymetry))

# SYNC
tlog<- tlog %>% mutate(Timestamp=substr(as.character(Timestamp),1,19))
nav<-inner_join(tlog, ping, by=c("Timestamp" = "punix"))
image_sync$time_vec<-as.character(image_sync$time_vec)
sync<-merge(nav,image_sync,by.x="Timestamp",by.y="time_vec")

# -------------------------------------------------------------
# SAVE RESULTS
# -------------------------------------------------------------

## SAVE
save(nav,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"-nav.RData"))
save(sync,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"-sync.RData"))
write.csv2(nav,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"-nav.csv"))
write.csv2(sync,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"-sync.csv"))
#save(nav,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"_",square,"-nav.RData"))
#save(sync,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"_",square,"-sync.RData"))
#write.csv2(nav,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"_",square,"-nav.csv"))
#write.csv2(sync,file=paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"_",square,"-sync.csv"))

#load(paste0("wham_data/",campaign,"/navigation/sync/",campaign,"_",dive,"-sync.RData"))

# FOR BIIGLE metadata file
biigle_metadata <- sync %>% 
  mutate(gps_altitude=altitudeRelative) %>%
  mutate(distance_to_ground=bathymetry/1000) %>%
  mutate(SUB_heading=heading) %>%
  select(c(image_vec,Timestamp,gps.lon,gps.lat,gps_altitude,distance_to_ground,SUB_heading)) %>% 
  rename(filename = image_vec,
         taken_at = Timestamp,
         lng = gps.lon,
         lat = gps.lat)
write.table(biigle_metadata,file=paste0("wham_data/",campaign,"/for_biigle/",campaign,"_",dive,"_",replicate,"-biigle.csv"),sep=",",quote=FALSE,row.names = FALSE)

# -------------------------------------------------------------
# OTHER APPLICATIONS
# -------------------------------------------------------------
# if already saved, LOAD
#load(paste0("wham_data/",campaign,"/",dive,"/",campaign,"_",dive,"_",square,"-sync.RData"))

# FOR METASHAPE
metashape_metadata <- sync %>%
  mutate(Altitude=bathymetry/10000) %>%
  select(c(image_vec,gps.lon,gps.lat,Altitude,heading,pitch,roll)) %>%
  rename(Label=image_vec,
         Long=gps.lon,
         Lat=gps.lat,
         Yaw=heading,
         Pitch=pitch,
         Roll=roll)
  
write.table(metashape_metadata,file=paste0("wham_data/",campaign,"/",dive,"/",campaign,"_",dive,"_",square,"-metashape.csv"),sep=",",quote=FALSE,row.names = FALSE)

# FOR CHUBACAPP
chubacapp_nav <- sync %>% 
  mutate(depth=((-bathymetry)/1000)-altitudeRelative) %>%
  select(c(clock.currentDate,clock.currentTime,gps.lat,gps.lon,depth,heading,pitch,roll)) %>% 
  rename(date = clock.currentDate,
         time = clock.currentTime,
         latitude = gps.lat,
         longitude = gps.lon) 

write.table(chubacapp_nav,file=paste0("wham_data/",campaign,"/",dive,"/",campaign,"_",dive,"_",square,"-chubacapp.csv"),sep=",",quote=FALSE,row.names = FALSE)

