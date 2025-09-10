#
#-----------------------------------------------------------------
#           STEP 3 in the WHAM-workflow : BIODIVERSITY ANALYSIS
#                       by LEA KATZ
#-----------------------------------------------------------------
# This script is written for any abundance dataset.
# It can be used for :
# - making barplots of community composition
# - calculating biodiversity indexes
# - making pie charts of community composition
# Outputs of this script are the plots and should be saved 
# manually if satisfactory.

#install.packages("vegan","labdsv","tidyverse","reshape2","rdiversity","ggpie","viridis")
library(vegan)
library(labdsv)
library(tidyverse)
library(reshape2)
library(rdiversity)
library(ggpie)
library(viridis)
library(pals)

#LOAD DATA ---------------------------------------------------
load(file="wham_data/TANGO/datasets/TANGO_ALL_CLEAN.RData")
load(file="wham_data/TANGO2/datasets/TANGO2_NWAP_cleaned.RData")
labels<-read.csv(file="wham_data/TANGO/metadata/TANGO_labels.csv")
load(file="wham_data/TANGO1/datasets/TANGO1_DI2_biota.RData")
load(file="wham_data/TANGO2/datasets/TANGO2_microhabitats_final.RData")
mydata<-pivot_longer(mydata,cols=colnames(mydata)[1:81])
mydata <- merge(mydata, labels[, c("label_names","group", "sub_group","algaecolor")], by.x = "name", by.y="label_names")

mydataCLEANED->mydata
mydata<-mydata[grep("MI|HI|FH",mydata$station),] #only TANGO2

#OPTIONAL : CLEAN AND HOMOGENIZE DATASET ----------------------
mydata$morphotype<-NA
mydata$group<-NA
mydata$sub_group<-NA
mydata$feeding_strategy<-NA
mydata$mobility<-NA
mydata$algaecolor<-NA
for (i in 1:length(mydata$site)) {
  mydata$label_names[i]->mylabel
  myrow<-labels[labels$label_names == mylabel,]
  myrow$morphotype->mydata$morphotype[i]
  myrow$group->mydata$group[i]
  myrow$sub_group->mydata$sub_group[i]
  myrow$feeding_strat->mydata$feeding_strategy[i]
  myrow$mobility->mydata$mobility[i]
  myrow$algaecolor->mydata$algaecolor[i]
}

# SUMMARIZE BY TRANSECT, SITE AND STATION ---------------------
transect_totals <- mydata %>% group_by(transect) %>% summarise(unique_images = n_distinct(image_filename))
site_totals <- mydata %>% group_by(site) %>% summarise(unique_images = n_distinct(image_filename))
station_totals <- mydata %>% group_by(station) %>% summarise(unique_images = n_distinct(image_filename))

##by images ----
mydataIMAGES<-mydata

##by transects ----
mydataTRANSECTS <- mydata %>%
  group_by(transect, label_names) %>%
  summarise(
    total_n = sum(n, na.rm = TRUE),
    total_cover = sum(cover, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  ) %>%
  left_join(transect_totals, by = "transect") %>%  
  mutate(true_cover = total_cover / unique_images) %>%
  mutate(true_n = total_n / unique_images)

##by site ----
mydataSITES <- mydata %>%
  group_by(site, label_names) %>%
  summarise(
    total_n = sum(n, na.rm = TRUE),
    total_cover = sum(cover, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  ) %>%
  left_join(site_totals, by = "site") %>%  
  mutate(true_cover = total_cover / unique_images) %>%
  mutate(true_n = total_n / unique_images)

##by station ----
mydataSTATIONS <- mydata %>%
  group_by(station, label_names) %>%
  summarise(
    total_n = sum(n, na.rm = TRUE),
    total_cover = sum(cover, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  ) %>%
  left_join(station_totals, by = "station") %>%  
  mutate(true_cover = total_cover / unique_images) %>%
  mutate(true_n = total_n / unique_images)

# GRAPHS ------------------------------------------------------

## community composition ----
mydata<-mydataIMAGES
#mydata<-mydataSITES
#mydata<-mydataTRANSECTS
#mydata<-mydataSTATIONS

mydata<-mydata[-grep("substrate|macroalgae",mydata$group),]

### number of annotations ABSOLUTE ----
ggplot(mydata, aes(x = site, y = n, fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# of Annotations", title = "Community Composition", fill = "Taxa") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = unname(glasbey()))

### number of annotations NORMALIZED ----
ggplot(mydata, aes(x = site, y = true_n, fill = sub_group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# of Annotations (NORMALISED)", title = "Community Composition", fill = "Taxa") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = unname(glasbey()))

### relative cover
ggplot(mydata, aes(x = site, y = true_cover, fill = sub_group)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "site", y = "%cover", title = "Community Composition", fill = "Taxa") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = unname(glasbey()))

## target groups ----

### substrate ----
#mydata<-mydataTRANSECTS
mydata<-mydataSITES
#mydata<-mydataSTATIONS

group<-"substrate"
mydata<-mydata[grep(group,mydata$group),]
custom_order<-c("mud","sand_mud","fine_sand","coarse_sand","gravel","pebble","cobbles","boulder","rock","consolidated","invisible","microalgal_biofilm","dusty")
mydata<- mydata%>%mutate(morphotype=factor(morphotype,levels=custom_order))

ggplot(mydata, aes(x = station, y = cover, fill = morphotype)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "site", y = "Cover", title = "substrate composition", fill = "substrate granulometry") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("seashell","wheat1","wheat2","wheat3","grey47","grey37","grey27","grey17","grey7","grey7","palegreen4","palegreen2","lightgoldenrod"))

### macroalgae ----
mydata<-mydataSITES
#mydata<-mydataSTATIONS

group<-"macroalgae"
mydata<-mydata[grep(group,mydata$group),]

####by color ----
ggplot(mydata, aes(x = site, y = value, fill = algaecolor)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "macroalgae cover", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("peru","green4","pink"))

####by morphotype ----
ggplot(mydata, aes(x = site, y = value, fill = name)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "macroalgae cover", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("magenta","pink","darkgoldenrod","darkgoldenrod4","darkolivegreen4","darkolivegreen2","darkseagreen4","darkorange4","darkorange3","brown"))

## functional groups ----

### trophic ----
mydata<-mydataSITES
ggplot(mydata[-grep("substrate|macroalgae",mydata$group),], aes(x = site, y = true_cover, fill = feeding_strategy)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Site", y = "Cover", title = "trophic diversity", fill = "morphotype") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = unname(glasbey()))


# BIODIVERSITY INDEXES -----------------------------------------
mydata<-mydataSITES
#mydata<-mydataSTATIONS

mydata<-pivot_longer(mydata,cols=colnames(mydata)[1:81])
mydata<-mydata[-grep("substrate",mydata$group),]

##formatting ----
mydata
m<- mydata %>% select(Grouping,name,value)
m<- mydata %>% select(transect,morphotype,n)
mydata_m<- matrify(as.data.frame(m))

##alpha diversity ----

###indexes ----
SN<- specnumber(mydata_m) #Species Richness
shan<- diversity(mydata_m) #Shannon-Wiener
pielou<- shan/log(SN) #Pielou's evenness

mydata_m$SN<- SN
mydata_m$shan<- shan
mydata_m$pielou<- pielou
mydata_m$group_total<- micro_totals$unique_images
mydata_m$mean_per_image<- mydata_m$SN/mydata_m$group_total

###plot ----
mydata_i<- mydata_m %>% select(SN,shan,pielou,mean_per_image)
mydata_i$group<- rownames(mydata_i)
mydata_long <- gather(mydata_i, key="index", value="value", c("SN","shan","pielou"))

####barplot ----
ggplot(mydata_i, aes(x=mean_per_image,y=group)) +
  geom_bar(stat="identity",width=0.5) +
  geom_text(aes(label=mean_per_image), size=3) +
  theme_minimal()

ggplot(mydata_long, aes(x=group, y=value)) +
  geom_bar(stat="identity",width=0.5) +
  geom_text(aes(label=value), size=3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,vjust=0.7)) +
  labs(x=element_blank(),
       y=element_blank()) +
  coord_flip() +
  facet_wrap(~index, scales="free_x")

####bubble plot ----
ggplot(mydata_i,aes(x=shan, y=SN, size=pielou, color=site)) +
  geom_point(alpha=0.7) +
  geom_text(aes(label=site),size=4,vjust=-3) +
  scale_size(range = c(1.4, 19), name="Evenness") +
  scale_color_viridis(discrete=TRUE,guide="none") +
  theme_bw() +
  theme(legend.position="bottom")

#PIE CHARTS ----------------------------------------------------

##transects ----
mydata<-mydataTRANSECTS
mydata<-mydata[-grep("substrate|macroalgae",mydata$group),]
ggplot(mydata, aes(x="", y=true_cover, fill=group)) +
  geom_bar(position="fill",stat="identity") +
  labs (x="transect",y="relative abundance",title="Community composition") +
  theme_void() +
  coord_polar("y",start=0) +
  facet_wrap(vars(transect)) +
  scale_fill_manual(values = unname(glasbey()))

##sites ----
mydata<-mydataSITES
mydata<-mydata[-grep("substrate|macroalgae",mydata$group),]
ggplot(mydata, aes(x="", y=true_cover, fill=group)) +
  geom_bar(position="fill",stat="identity") +
  labs (x="site",y="relative abundance",title="Community composition") +
  theme_void() +
  coord_polar("y",start=0) +
  facet_wrap(vars(site)) +
  scale_fill_manual(values = unname(glasbey()))
