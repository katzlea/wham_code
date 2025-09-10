#
#-----------------------------------------------------------------
#           STEP 5 in the WHAM-workflow : PREP FOR BNI
#                   Adapted for TANGO1 data
#                       by LEA KATZ
#-----------------------------------------------------------------
# This script is for preparing abundance datasets for Bayesian Network Analysis :
# - checking distribution of binary variables (counting presence/absence),
# - grouping variables that are too zero-heavy
# - adding more discrete levels for variables that are most abundant
# - dropping variables that should not be included in dataset
# The outputs are 2 .txt files. One with the node names, to use in BANJO settings
# and one with the discrete data.

#install.packages("tidyverse","readxl")
library(tidyverse)
library(readxl)

# -------------------------------------------------------------------------------------------
# DEFINE VARIABLES AND LOAD DATASET
# -------------------------------------------------------------------------------------------
campaign="TANGO2"
dataset="NWAP"

#load data 
load(paste0("wham_data/",campaign,"/matrified/",campaign,"_",dataset,"_m.RData"))
#load(paste0("wham_data/",campaign,"/matrified/",campaign,"_",dive,"_",square,"_m.RData"))

#taxa<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_taxa.csv"))
#sub<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_substrate.csv"))
labels<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_labels.csv"))

mydata<-mydata_matrified
mydata<-as.data.frame(sapply(mydata,as.numeric))
rownames(mydata)<-rownames(mydata_matrified)

taxa<-intersect(taxa$taxa,colnames(mydata))
sub<-intersect(sub$label_name,colnames(mydata))

# -------------------------------------------------------------------------------------------
# ADD ENVIRONMENTAL DATA AND MAKE SUBSETS
# -------------------------------------------------------------------------------------------
env<-read.csv(file = "wham_data/TANGO1/metadata/TANGO1_images_metadata.csv")
mydata<-rownames_to_column(mydata,var="image_filename")
temp<-left_join(mydata,env[,c("image_filename","substrate2","dropstones","dist_to_glacier","site","max_depth","slope")])
rownames(temp)<-temp$image_filename
mydata<-temp[,-1]

#subsets : distance to glacer (close/medium/far)
close<-subset(mydata, dist_to_glacier==0)
medium<-subset(mydata, dist_to_glacier==1)
far<-subset(mydata, dist_to_glacier==2)
mydata_biota<-select(mydata, all_of(taxa))
mydata_sub<-select(mydata, all_of(sub))

# CURRENTLY WORKING ON
mydata->discr
close->discr
medium->discr
far->discr

mydata->mod

mydata_biota->discr
mydata_biota->mod

mydata_sub->discr
mydata_sub->mod


#------------------------------------------------------------------------------------
## ABUNDANCE => PRESENCE/ABSENCE (2 levels)
ncolbiota<-111
discr[,1:ncolbiota]<-lapply(discr[,1:ncolbiota],function(x) ifelse(x==0,0,1))

# CHECK DISTRIBUTION OF PRESENCE/ABSENCE
gg_melt <- pivot_longer(discr,cols=1:length(unique(colnames(discr))),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  scale_x_discrete(labels=c("0"="absent","1"="present")) +
  scale_fill_manual(values=c("0"="#FD6467","1"="#5BBCD6"),name="State",labels = c("0" = "absent", "1" = "present"))+
  facet_wrap(~species) +
  theme_classic() +
  ggtitle("Distribution of bins") +
  labs(y="number of images")

# Look at the plot. 
# If taxa has mostly absence : group together with similar taxa.
# If taxa has mostly presence : try adding more levels when discretizing

#_________________________________________
## REMOVE STUFF
discr <- discr %>% select(-c(
  #write here all columns that need to be removed
  "unknown","wtf"
  ))

discr <- discr %>% select(-c(
  "bivalves",
  "filterfeeders",
  "parborlasia","parborlasia_aggr","parborlasia_corrugatus","Worms"
))

#________________________________________
## GROUP STUFF

#stuff_list<-c("thing1","thing2")
#mydata<-mydata %>% mutate(stuff = rowSums(mydata[,stuff_list]))
#discr$stuff<-ifelse(rowSums(mydata[,stuff_list])==0,0,1)

stars_list<-c("pink_star_msp1","Asteroidea","carrot_star_msp4","glitter_star_msp3","light_brown_star_msp15","medium_yellow_star_msp13","skinny_yellow_star_msp10")
#stars_list<-c("carrot_star_msp4","glitter_star_msp3","pink_star_msp1")
mod<-mod%>% mutate(stars = rowSums(mod[,stars_list]))
discr$starfish<-ifelse((mod$stars)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(stars_list))]

limpets_list<-c("limpet_msp1","limpet_msp2")
mod<-mod %>% mutate(limpets = rowSums(mod[,limpets_list]))
discr$limpets<-ifelse((mod$limpets)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(limpets_list))]

macroalgae_list<-c("Macroalgae","himantothallus_msp1","red_or_brown_sheet_algae","red_sheet_msp1","desmarestia_msp1","desmarestia_msp2","branching_red_algae_msp1","branching_red_algae_msp2")
mod<-mod %>% mutate(macroalgae = rowSums(mod[,macroalgae_list]))
discr$macroalgae<-ifelse(rowSums(mod[,macroalgae_list])==0,0,1)
discr<-discr[ , !(names(discr) %in% c(macroalgae_list))]

encrusting_list<-c("pink_encr_algae","green_encr_algae")
mod<-mod %>% mutate(encrusting_algae = rowSums(mod[,encrusting_list]))
discr$encrusting_algae<-ifelse(rowSums(mod[,encrusting_list])==0,0,1)
discr<-discr[ , !(names(discr) %in% c(encrusting_list))]

parborlasia_list<-c("parborlasia_aggr","parborlasia_corrugatus")
mod<-mod %>% mutate(parborlasia = rowSums(mod[,parborlasia_list]))
discr$parborlasia<-ifelse(rowSums(mod[,parborlasia_list])==0,0,1)
discr<-discr[ , !(names(discr) %in% c(encrusting_list))]

filterfeeders_list<-c("fine_orange_msp4","finger_sponge_msp1","ascidian_msp1","ascidian_msp2","ascidian_msp3","orange_blob_msp5","spiky_ball_sponge_msp6","yellow_anemone_msp2","yellow_blob_msp2")
#filterfeeders_list<-c("ascidian_msp1","ascidian_msp2","fine_orange_msp4","finger_sponge_msp1")
#filterfeeders_list<-c("ascidian_msp1","ascidian_msp2","ascidian_msp3","finger_sponge_msp1")
mod<-mod %>% mutate(filterfeeders = rowSums(mod[,filterfeeders_list]))
discr$suspension_feeders<-ifelse(rowSums(mod[,filterfeeders_list])==0,0,1)
discr<-discr[ , !(names(discr) %in% c(filterfeeders_list))]

bivalves_list<-c("beige_bivalve_msp1","clam_msp2","brown_bivalve_msp3")
discr$bivalves<-ifelse(rowSums(mod[,bivalves_list])==0,0,1)
mod<-mod %>% mutate(bivalves = rowSums(mod[,bivalves_list]))
discr<-discr[ , !(names(discr) %in% c(bivalves_list))]

gastropoda_list<-c("Gastropoda","white_snail_msp1","white_bunny_msp1","white_porcupine_msp2","wavy_msp3","nacella","limpet_msp1")
discr$gastropoda<-ifelse(rowSums(mod[,gastropoda_list])==0,0,1)
mod<-mod %>% mutate(gastropoda = rowSums(mod[,gastropoda_list]))
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#branching_algae_list<-c("branching_red_algae_msp2","desmarestia_msp1")
#discr$branching_algae<-ifelse(rowSums(medium[,branching_algae_list])==0,0,1)
#medium<-medium %>% mutate(branching_algae = rowSums(medium[,branching_algae_list]))
#discr<-discr[ , !(names(discr) %in% c(branching_algae_list))]

#_________________________________________
## CHECK DISTRIBTION OF COVER
hist(mydata$macroalgae)

#_________________________________________
## ADD LEVELS => ABSENT/LOW/HIGH (3 levels)
source("wham_code/functions/get.hilo.R")

#discr$thing<-as.factor(get.hilo(mydata$thing))
discr$macroalgae<-get.hilo(mod$macroalgae)

#________________________________________
## LOOK AT ALL OF IT, SATISFACTORY? THEN FINALIZE
final<-discr

gg_melt <- pivot_longer(final,1:ncol(final),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  scale_x_discrete(labels=c("absent","present/few","many")) +
  scale_fill_manual(values=c("0"="#FD6467","1"="#5BBCD6","2"="#F2AD00"),name="State",labels = c("0" = "absent", "1" = "present/few","2"="many"))+
  facet_wrap(~species) +
  theme_classic() +
  ggtitle("Distribution of bins")+
  labs(y="number of images")

#env variables
env_discr<-mydata[,61:66]
gg_melt <- pivot_longer(env_discr,1:ncol(env_discr),names_to="abiotic")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  scale_x_discrete(labels=c("absent","present/few","many")) +
  scale_fill_manual(values=c("0"="#FD6467","1"="#5BBCD6","2"="#F2AD00","3"="#00A08A","4"="#F98400"),name="State")+
  facet_wrap(~abiotic) +
  theme_classic() +
  ggtitle("Distribution of bins") +
  labs(y="number of images")

#---------------------------------------------------------------------
# COMPILE THE FINAL DATASET FOR NEXT STEP
#---------------------------------------------------------------------
dataset_name="DI2-env-only"

save(final,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
final<-rownames_to_column(final,var="image_filename")
temp<-left_join(final,env[,c("image_filename","site","max_depth","slope")])
rownames(temp)<-temp$image_filename
final<-temp[,-1]

nodes<-colnames(final)
write.table(nodes,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_nodes.txt"),
            row.names = FALSE,col.names = FALSE,quote=FALSE)
write.table(final,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,".txt"),
            row.names = FALSE,col.names = FALSE)
