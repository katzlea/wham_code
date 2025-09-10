#
#-----------------------------------------------------------------------
#           STEP 6 in the WHAM-workflow : MAKE NICE MAPS
#                       by LEA KATZ
#-----------------------------------------------------------------------
# This script was used to make maps of the sampling area
# from the TANGO project in the Western Antarctic Peninsula

library(tidyverse)
theme_set(theme_bw())
library(sf)               
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(prettymapr)
library(paletteer)
library(ggnewscale)
library(ggrepel)

# lOAD DATA ----------------------------------------------------------
world <- ne_countries(scale = "large", returnclass = "sf")
tango2<-read.csv("wham_data/TANGO2/metadata/TANGO2_metadata.csv")
ROV<-read.csv("wham_data/TANGO2/metadata/TANGO2_ROV_GPS_points_final.csv")
manon<-read.csv("wham_data/TANGO2/metadata/MANON_METADATA.csv")

# subset ----
ROV_stations<-ROV[c(5,26,42),]
ROV_stations$station<-c("Melchior Islands","Hovgaard Islands","Foyn Harbor")
ROV_stations$station_abbr<-c("MI","HI","FH")

# antarctica general plot ----
ggplot(data = world) +
  #annotation_map_tile(type="osm") +
  geom_sf() + #basemap
  coord_sf(crs = st_crs(3031),
           ylim = c(-3e6,3e6),  
           xlim = c(-3e6,3e6))

# tango2 general plot ----
ggplot(data = world) +
  #annotation_map_tile(type="osm") +
  geom_sf() + #basemap
  coord_sf(crs = st_crs(4326),
           xlim = c(-70, -52), 
           ylim = c(-70, -60.5), 
           expand = FALSE) + #zoom
  theme(panel.background = element_rect(fill = "aliceblue")) +
  geom_point(data = ROV_stations, aes(x = site_lat_DD, y = site_lon_DD, colour=station,shape=station),size=3) + #stations
  #geom_text(data = ROV_stations, aes(x = site_lat_DD, y = site_lon_DD, label = station_abbr)) +
  scale_color_manual(values=c("cornflowerblue","maroon2","orange")) +
  labs(x = NULL, y = NULL) +     # Removes axis labels
  theme(legend.position = "bottom") +
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) #+ 
  #ggtitle("Map of the Antarctic Peninsula", subtitle = "TANGO2 Sampling stations") 

# tango2 zoomed plot ----
ggplot(data = world) +
  annotation_map_tile(type="osm",zoomin=-1,alpha=0.9) +
  #geom_sf() + #basemap
  coord_sf(xlim = c(-64.5, -61.5), 
           ylim = c(-65.3, -64.1), 
           expand = FALSE) + #zoom
  theme(panel.background = element_rect(fill = "aliceblue")) +
  geom_point(data = ROV_stations, aes(x = site_lat_DD, y = site_lon_DD, colour=station,shape=station),size=3) + #stations
  #geom_text(data = ROV_stations, aes(x = site_lat_DD, y = site_lon_DD, label = station_abbr)) +
  scale_color_manual(values=c("cornflowerblue","maroon2","orange")) +
  labs(x = NULL, y = NULL) +     # Removes axis labels
  theme(legend.position = "none") +
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         #pad_x = unit(0.75, "in"), 
                         #pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering)
  #ggtitle("Map of the Antarctic Peninsula", subtitle = "TANGO2 Sampling stations")
ggsave("map_all.png",width=4.5)


# Foyn harbor zoom -------
fh_iso <- read_sf("wham_data/TANGO2/maps/Foyn/Foyn Hbr_Isobaths.shp")
fh_land <- read_sf("wham_data/TANGO2/maps/Foyn/Foyn Hbr_Land.shp")

transects <- tango2[tango2$station_full_name=="Foyn Harbor",]
transects_start<- transects %>% select(transect,site_lat_DD,site_lon_DD) %>%
  rename(lat=site_lat_DD , lon=site_lon_DD) %>%
  mutate(type = "start")
transects_stop<- transects %>% select(transect,site_lat_stop_DD,site_lon_stop_DD) %>%
  rename(lat=site_lat_stop_DD , lon=site_lon_stop_DD) %>%
  mutate(type = "stop")
transects_FH<-bind_rows(transects_start,transects_stop)

manonfh<-manon[manon$Region=="Foyn Harbor",]

bathypal<-colorRampPalette(c("lightblue", "darkblue"))
ggplot() +
  #annotation_map_tile(type="osm",zoomin=2,alpha=0.9) +
  geom_sf(data = fh_land,fill="grey",color="black") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  #geom_sf(data = hov_iso,fill="aliceblue",color="aliceblue") +
  #geom_sf(data = hov_iso,aes(color=MIN_CAT)) +
  #scale_color_manual(values=bathypal(nlevels(as.factor(hov_iso$MIN_CAT)))) +
  geom_point(data=manonfh,aes(x=Longitude,y=Latitude)) +
  geom_line(data = transects_FH[transects_FH$transect != "" & !is.na(transects_FH$transect), ], aes(x=lat,y=lon,group=transect),color="cornflowerblue") +
  #geom_label_repel(data = transects_FH[transects_FH$transect != "" & !is.na(transects_FH$transect) & transects_FH$type=="stop", ], aes(x=lat,y=lon,label=transect),color="black") +
  xlab("Latitude") + 
  ylab("Longitude") +
  ggtitle("Foyn Harbor") +
  #annotation_map_tile(type="cartodark",zoom=0) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         #pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) 
  ggsave("map_foyn.png",width=4.5)


#Melchior zoom --------

min_iso <- read_sf("wham_data/TANGO2/maps/Melchior/Melchior Is_Isobaths.shp")
min_land <- read_sf("wham_data/TANGO2/maps/Melchior/Melchior Is_Land.shp")
mis_iso <- read_sf("wham_data/TANGO2/maps/Melchior/South Omega_Isobaths.shp")
mis_land <- read_sf("wham_data/TANGO2/maps/Melchior/south Omega_Land.shp")

manonmi<-manon[manon$Sub_region=="North_Omega",]

transects <- tango2[tango2$station_full_name=="Melchior Islands North Omega",]
transects_start<- transects %>% select(transect,site_lat_DD,site_lon_DD) %>%
  rename(lat=site_lat_DD , lon=site_lon_DD) %>%
  mutate(type = "start")
transects_stop<- transects %>% select(transect,site_lat_stop_DD,site_lon_stop_DD) %>%
  rename(lat=site_lat_stop_DD , lon=site_lon_stop_DD) %>%
  mutate(type = "stop")
transects_MIN<-bind_rows(transects_start,transects_stop)

ggplot() +
  geom_sf(data = min_land,fill="grey",color="black") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  geom_line(data = transects_MIN[transects_MIN$transect != "" & !is.na(transects_MIN$transect), ], aes(x=lat,y=lon,group=transect),color="orange") +
  geom_point(data=manonmi,aes(x=Longitude,y=Latitude)) +
  #geom_label_repel(data = transects_MIN[transects_MIN$transect != "" & !is.na(transects_MIN$transect) & transects_MIN$type=="stop", ], aes(x=lat,y=lon,label=transect),color="black") +
  xlab("Latitude") + 
  ylab("Longitude") +
  ggtitle("Melchior Islands - Omega North") +
  annotation_scale(location = "tl") + 
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_y = unit(0.3, "in"), 
                         pad_x = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 
ggsave("map_min.png",width=4.5)

transects <- tango2[tango2$station_full_name=="Melchior Islands South Omega",]
transects_start<- transects %>% select(transect,site_lat_DD,site_lon_DD) %>%
  rename(lat=site_lat_DD , lon=site_lon_DD) %>%
  mutate(type = "start")
transects_stop<- transects %>% select(transect,site_lat_stop_DD,site_lon_stop_DD) %>%
  rename(lat=site_lat_stop_DD , lon=site_lon_stop_DD) %>%
  mutate(type = "stop")
transects_MIS<-bind_rows(transects_start,transects_stop)

ggplot() +
  geom_sf(data = mis_land,fill="grey",color="black") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  geom_line(data = transects_MIS[transects_MIS$transect != "" & !is.na(transects_MIS$transect), ], aes(x=lat,y=lon,group=transect),color="orange") +
  #geom_label_repel(data = transects_MIS[transects_MIS$transect != "" & !is.na(transects_MIS$transect) & transects_MIS$type=="stop", ], aes(x=lat,y=lon,label=transect),color="black") +
  xlab("Latitude") + 
  ylab("Longitude") +
  ggtitle("Melchior Islands - Omega South") +
  annotation_scale(location = "tl") + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         #pad_y = unit(0.3, "in"), 
                         #pad_x = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 
ggsave("map_mis.png",width=4.5)


#Hovgaard zoom ---------

hov_iso <- read_sf("wham_data/TANGO2/maps/Hovgaard/Hovgaard_Isobaths.shp")
hov_land <- read_sf("wham_data/TANGO2/maps/Hovgaard/Hovgaard_Land.shp")

manonhi<-manon[manon$Region=="Hovgaard Island ",]

transects <- tango2[tango2$station_full_name=="Hovgaard Islands",]
transects_start<- transects %>% select(transect,site_lat_DD,site_lon_DD) %>%
  rename(lat=site_lat_DD , lon=site_lon_DD) %>%
  mutate(type = "start")
transects_stop<- transects %>% select(transect,site_lat_stop_DD,site_lon_stop_DD) %>%
  rename(lat=site_lat_stop_DD , lon=site_lon_stop_DD) %>%
  mutate(type = "stop")
transects_HI<-bind_rows(transects_start,transects_stop)

bathypal<-colorRampPalette(c("lightblue", "darkblue"))
ggplot() +
  #annotation_map_tile(type="osm",zoomin=2,alpha=0.9) +
  geom_sf(data = hov_land,fill="grey",color="black") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  geom_point(data=manonhi,aes(x=Longitude,y=Latitude)) +
  #geom_sf(data = hov_iso,fill="aliceblue",color="aliceblue") +
  #geom_sf(data = hov_iso,aes(color=MIN_CAT)) +
  #scale_color_manual(values=bathypal(nlevels(as.factor(hov_iso$MIN_CAT)))) +
  geom_line(data = transects_HI[transects_HI$transect != "" & !is.na(transects_HI$transect), ], aes(x=lat,y=lon,group=transect),color="maroon2") +
  #geom_label_repel(data = transects_HI[transects_HI$transect != "" & !is.na(transects_HI$transect) & transects_HI$type=="stop", ], aes(x=lat,y=lon,label=transect),color="black") +
  xlab("Latitude") + 
  ylab("Longitude") +
  ggtitle("Hovgaard Islands") +
  #annotation_map_tile(type="cartodark",zoom=0) +
  annotation_scale(location = "tr") + 
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) 
ggsave("map_hovgaard.png",width=4.5,height=4)

