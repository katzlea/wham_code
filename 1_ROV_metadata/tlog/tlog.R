library("ggplot2")
library("tidyverse")
library("rgdal")
library("sp")
library("maps")
tlog<-readr::read_csv("seno_otway.csv")

tlog$long<-(tlog$GPS_RAW_INT.lon/10000000)
tlog$lat<-(tlog$GPS_RAW_INT.lat/10000000)

#map
ggplot(tlog,aes(long,lat)) +
  geom_path()
ggplot(tlog,aes(long,lat)) + 
  geom_sf() + 
  coord_sf()

ggplot(tlog, aes(long, lat)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()


