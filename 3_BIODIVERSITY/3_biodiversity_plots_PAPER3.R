#
#-----------------------------------------------------------------
#           STEP 3, adapted for MULTI-SCALE BNI PAPER
#                       by LEA KATZ
# citation...
#-----------------------------------------------------------------
library(tidyverse)

#LOAD DATA -------------------------------------------------------
load(file=here::here("wham_data/TANGO/datasets/TANGO_ALL_CLEAN.RData"))
mydataCLEANED->mydata
mydata<-mydata[grep("MI|HI|FH",mydata$station),] #only TANGO2

#SUMMARIZE -------------------------------------------------------
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

# MACROALGAE ----------------------------------------
mydata<-mydataSITES
group<-"macroalgae"
mydata<-mydata[grep(group,mydata$group),]

#bniNode<-c("Red branching algae","Red branching algae","Red branching algae","Green encrusting algae","Green sheet-like algae","Himantothallus","Coralline algae","Red sheet-like algae","Other","Other","Desmarestia","Other","Other","Desmarestia","Diatom mats","Other","Other","Brown sheet-like algae","Other","Other")
bniNode<-c("Red branching algae","Red branching algae","Desmarestia","Coralline algae","Red sheet-like algae","Other","Desmarestia","Other encrusting", "Himantothallus","Other","Other","Red branching algae","Other","Other","Other","Other","Diatom mats","Other encrusting","Himantothallus","Diatom mats")
labelz<-unique(mydata$label_names)
dat<-cbind(bniNode,labelz)
dat<-as.data.frame(dat)
mydata_macroalgae<-left_join(mydata,dat,join_by(label_names==labelz))
mydata_macroalgae$bniNode<-factor(mydata_macroalgae$bniNode, levels=c("Diatom mats","Coralline algae","Himantothallus","Desmarestia","Red sheet-like algae","Red branching algae"))

## by site ----
ggplot(mydata_macroalgae, aes(x = site, y = true_cover, fill = bniNode)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "macroalgae cover", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
 #scale_fill_manual(values=c("darkolivegreen2","magenta","darkgoldenrod","darkgoldenrod4","brown","red")) +
  scale_fill_manual(values=c("#F1F7B5","#A8D1D1","#DFEBEB","#9EA1D4","#FFCBCB","#FD8A8A"))

mydata<-mydataSTATIONS
group<-"macroalgae"
mydata<-mydata[grep(group,mydata$group),]
mydata_macroalgae<-left_join(mydata,dat,join_by(label_names==labelz))
mydata_macroalgae$bniNode<-factor(mydata_macroalgae$bniNode, levels=c("Diatom mats","Coralline algae","Himantothallus","Desmarestia","Red sheet-like algae","Red branching algae"))

## by station  ----
ggplot(mydata_macroalgae, aes(x = station, y = true_cover, fill = bniNode)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "macroalgae cover", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  #scale_fill_manual(values=c("darkolivegreen2","magenta","darkgoldenrod","darkgoldenrod4","brown","red")) +
  scale_fill_manual(values=c("#F1F7B5","#A8D1D1","#DFEBEB","#9EA1D4","#FFCBCB","#FD8A8A"))

## total macroalgal cover by site
mydata_total <- mydata_macroalgae %>%
  group_by(site) %>%
  summarise(total_cover = sum(true_cover, na.rm = TRUE))

# Plot
ggplot(mydata_total, aes(x = site, y = total_cover)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(x = "Site", y = "Cover", title = "Macroalgae Total Cover") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# GASTROPODS ------------------------------------------
mydata<-mydataSITES
group<-"gastropods"
mydata<-mydata[grep(group,mydata$sub_group),]

bniNode<-c("Other gastropods","Nacella","Margarella","Nudibranchia","Nudibranchia","Nudibranchia","Nudibranchia","Nacella")
labelz<-unique(mydata$label_names)
dat<-cbind(bniNode,labelz)
dat<-as.data.frame(dat)
mydata_gast<-left_join(mydata,dat,join_by(label_names==labelz))

## by site ----
ggplot(mydata_gast, aes(x = site, y = total_n, fill = bniNode)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Gastropods", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#41436A","#F64668","#6AAB9C","grey"))

mydata<-mydataSTATIONS
group<-"gastropods"
mydata<-mydata[grep(group,mydata$sub_group),]
mydata_gast<-left_join(mydata,dat,join_by(label_names==labelz))

## by station ----
ggplot(mydata_gast, aes(x = station, y = total_n, fill = bniNode)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Gastropods", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#41436A","#F64668","#6AAB9C","grey"))


# LATERNULA ------------------------------------------
mydata<-mydataSITES
label<-"laternula"
mydata<-mydata[grep(label,mydata$label_names),]

##by site ----
ggplot(mydata, aes(x = site, y = total_n, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# annotations", title = "Laternula", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#83AF9B"))

mydata<-mydataSTATIONS
label<-"laternula"
mydata<-mydata[grep(label,mydata$label_names),]

##by station ----
ggplot(mydata, aes(x = station, y = total_n, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# annotations", title = "Laternula", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#83AF9B"))

# ANEMONES -------------------------------------
mydata<-mydataSITES
group<-"cnidarians"
mydata<-mydata[grep(group,mydata$group),]
mydata$label_names<-factor(mydata$label_names, levels=c("orange_anemone_msp1","yellow_anemone_msp2","white_anemone_msp3","edwardsiella_msp4","Other anemones"))

## by site ----
ggplot(mydata, aes(x = site, y = total_n, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Anemones", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  #scale_fill_manual(values=c("darkolivegreen2","magenta","darkgoldenrod","darkgoldenrod4","brown","red")) +
  scale_fill_manual(values=c("#FC9D9A","#F9CDAD","#C8C8A9","#83AF9B","grey"))

mydata<-mydataSTATIONS
group<-"cnidarians"
mydata<-mydata[grep(group,mydata$group),]
mydata$label_names<-factor(mydata$label_names, levels=c("orange_anemone_msp1","yellow_anemone_msp2","white_anemone_msp3","edwardsiella_msp4","Other anemones"))

## by station ----
ggplot(mydata, aes(x = station, y = total_n, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Anemones", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  #scale_fill_manual(values=c("darkolivegreen2","magenta","darkgoldenrod","darkgoldenrod4","brown","red")) +
  scale_fill_manual(values=c("#FC9D9A","#F9CDAD","#C8C8A9","#83AF9B","grey"))


# BIOFILM -----------------------------------------------
mydata<-mydataSITES
label<-"biofilm"
mydata<-mydata[grep(label,mydata$label_names),]

##by site ----
ggplot(mydata, aes(x = site, y = true_cover, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "Biofilm", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#FE4365"))

mydata<-mydataSTATIONS
label<-"biofilm"
mydata<-mydata[grep(label,mydata$label_names),]

##by station ----
ggplot(mydata, aes(x = station, y = true_cover, fill = label_names)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Biofilm", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#FE4365"))

# ECHINODERMS ------------------------------------
mydata<-mydataSITES
group<-"echinoderms"
mydata<-mydata[grep(group,mydata$group),]

##by site ----
ggplot(mydata, aes(x = site, y = total_n, fill = sub_group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Echinoderms", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#6ba3d6","#ea6B73","#e9c39b","#ac613c"))

mydata<-mydataSTATIONS
group<-"echinoderms"
mydata<-mydata[grep(group,mydata$group),]

##by site ----
ggplot(mydata, aes(x = station, y = total_n, fill = sub_group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "# individuals", title = "Echinoderms", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#6ba3d6","#ea6B73","#e9c39b","#ac613c"))

# PORIFERA ------------------------------------
mydata<-mydataSITES
group<-"sponges"
mydata<-mydata[grep(group,mydata$group),]
mydata<-subset(mydata, sub_group %in% c("encrusting sponges", "massive sponges", "erect sponges")) 

##by site ----
ggplot(mydata, aes(x = site, y = true_cover, fill = sub_group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title = "Porifera", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#FAA968","#F85525","#01204E"))

mydata<-mydataSTATIONS
group<-"sponges"
mydata<-mydata[grep(group,mydata$group),]
mydata<-subset(mydata, sub_group %in% c("encrusting sponges", "massive sponges", "erect sponges")) 

##by site ----
ggplot(mydata, aes(x = station, y = true_cover, fill = sub_group)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "site", y = "Cover", title ="Porifera", fill = "type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#FAA968","#F85525","#01204E"))

# DIVERSITY METRICS --------------------------------------------------
library(labdsv)
library(vegan)
load(file="wham_data/TANGO2/datasets/microhabitats_site_scale.RData")
SN<- specnumber(mydata_m) #Species Richness
shan<- diversity(mydata_m) #Shannon-Wiener
pielou<- shan/log(SN) #Pielou's evenness

mydata_m$SN<- SN
mydata_m$shan<- shan
mydata_m$pielou<- pielou
mydata_i<-mydata_m%>%select(shan,pielou,SN)
