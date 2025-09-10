#
#-----------------------------------------------------------------
#           STEP 4.1 in the WHAM-workflow : NMDS for MH
#                  for MICROHABITATS PAPER
#                       by LEA KATZ
#
# This script is used to do an NMDS on an abundance dataset.
# It outputs the NMDS graphs from CH3 of the thesis and related paper.
#
# citation...
#-----------------------------------------------------------------

library(vegan)
library(tidyverse)
library(labdsv)

# LOAD DATA ------------------------------------------------------
##images
load(file="wham_data/TANGO2/datasets/TANGO2_microhabitats_final.RData")
mydata[1:81]->mydata_m
as.matrix(mydata_m)->mydata_m
storage.mode(mydata_m) <- "numeric"

##transects
mydata_m<-read.csv(file="wham_data/TANGO2/datasets/microhabitats_transect_scale.csv")
transects<-mydata_m$X
as.matrix(mydata_m)->mydata_m
rownames(mydata_m)<-transects
mydata_m<-mydata_m[,-1]
storage.mode(mydata_m) <- "numeric"

##sites
load(file="wham_data/TANGO2/datasets/microhabitats_site_scale.RData")

# DEFINE COLORS ---------------------------------------------------
site_colors <- c(
  "MIN" = "yellow",
  "MIS-i" = "gold",
  "MIS-m" = "orange",
  "MIS-o" = "darkorange",
  "HI-n" = "lightpink",
  "HI-s" = "hotpink",
  "HI-c" = "maroon2",
  "FH-s" = "lightblue",
  "FH-b" = "steelblue",
  "FH-e" = "dodgerblue",
  "FH-w" = "cornflowerblue"
)
# SUMMARIZE -----------------------------------------------------
## group by transects ----
transect_totals <- mydata %>% group_by(transect) %>% summarise(unique_images = n())
mydataTRANSECTS <- mydata %>%
  group_by(transect) %>%
  summarise(across(1:81,sum,na.rm=TRUE),
            site = first(site),
            station = first(station))
#mydata_m<-mydataTRANSECTS[2:84]
#as.matrix(mydata_m)->mydata_m
#rownames(mydata_m)<-mydataTRANSECTS$transect

## group by sites ----
site_totals <- mydata %>% group_by(site) %>% summarise(unique_images = n())
mydataSITES <- mydata %>%
  group_by(site) %>%
  summarise(across(1:81,sum,na.rm=TRUE),
            station = first(station))
mydata_m<-mydataSITES[2:82]
as.matrix(mydata_m)->mydata_m
rownames(mydata_m)<-mydataSITES$site

# NMDS ---------------------------------------------------------
nmds <- metaMDS(mydata_m)
nmds

#extract scores
data.scores<-as.data.frame(vegan::scores(nmds, "sites"))
data.scores$image_filename<-rownames(data.scores)

##if by images
data.scores <- merge(data.scores, mydata[,82:89], by=0, all=TRUE)

##if summarized by transects
data.scores$transect<-rownames(data.scores)
data.scores$site<-mydataTRANSECTS$site
data.scores$station<-mydataTRANSECTS$station

##if summarized by sites
data.scores$site<-rownames(data.scores)
data.scores$station<-mydataSITES$station


species.scores<-as.data.frame(vegan::scores(nmds,"species"))
species.scores$species <-rownames(species.scores)


# Make hulls -----------------------------------------
HI_chull<-data.scores[data.scores$station=="HI",][-1,][chull(data.scores[data.scores$station=="HI",c("NMDS1", "NMDS2")][-1,]),]
FH_chull<-data.scores[data.scores$station=="FH",][chull(data.scores[data.scores$station=="FH",c("NMDS1", "NMDS2")]),]
MI_chull<-data.scores[data.scores$station=="MI",][-32,][chull(data.scores[data.scores$station=="MI",c("NMDS1", "NMDS2")][-32,]),]

# if summarized by transects
HI_chull<-data.scores[data.scores$station=="HI",][chull(data.scores[data.scores$station=="HI",c("NMDS1", "NMDS2")]),]
FH_chull<-data.scores[data.scores$station=="FH",][chull(data.scores[data.scores$station=="FH",c("NMDS1", "NMDS2")]),]
MI_chull<-data.scores[data.scores$station=="MI",][chull(data.scores[data.scores$station=="MI",c("NMDS1", "NMDS2")]),]

hull.data<-rbind(HI_chull,FH_chull,MI_chull)

# PLOT ---------------------------------------------
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=station,group=station),alpha=0.1) + # add the convex hulls
  #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=station,colour=site),size=2) + # add the point markers
  #scale_colour_manual(values=c("cornflowerblue","maroon2","orange")) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  scale_colour_manual(values = site_colors) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=transect),size=2,vjust=2) +  
  coord_equal() +
  xlim(min(c(data.scores$NMDS1, species.scores$NMDS1)),0.005) +
  xlim(min(c(data.scores$NMDS1, species.scores$NMDS1)),-0.003) +
  theme_bw() #+ 
  #theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
