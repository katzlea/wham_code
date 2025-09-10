#
#-----------------------------------------------------------------
#           STEP 5.1 in the WHAM-workflow : PREP FOR BNI
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
campaign="TANGO"
dataset="WAP"


#load data 
load(paste0("wham_data/",campaign,"/matrified/",campaign,"_",dataset,"_m.RData"))
labels<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_labels.csv"))

labels_b<-labels[-grep("substrate",labels$group),]
labels_s<-labels[grep("substrate",labels$group),]

mydata<-mydata_matrified
mydata<-as.data.frame(sapply(mydata,as.numeric))
rownames(mydata)<-rownames(mydata_matrified)

#WAP without BI
mydata<-mydata[-c(150:200),]


biota<-mydata[,(names(mydata)) %in% c(labels_b$label_names)]
for (i in 1:length(colnames(biota))) {
  colnames(biota)[i]->mylabel
  myrow<-labels_b[labels_b$label_names == mylabel,]
  myrow$morphotype->colnames(biota)[i]
}

biota<-as.data.frame(t(rowsum(t(biota), group = colnames(biota), na.rm = T)))

#now we work with 'discr', adapt as necessary
discr<-biota
mod<-biota


#------------------------------------------------------------------------------------
## ABUNDANCE => PRESENCE/ABSENCE (2 levels)
ncolbiota<-99
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
  "thingy","wtf","star_impression","urchin_test","red_ball_in_algae", #random stuff
  "VME_unknown"  #unidentified stuff
))

discr <- discr %>% select(-c("Bryozoa","mollusc","fish","crawling_traces","glypt_ant",
                             "orange_lump_free","white_amphipod_msp1","pycnogonid_msp1"))

#________________________________________
## GROUP STUFF

#asteroidea without odontaster
asteroidea<-labels[grep("asteroidea",labels$sub_group),]
asteroidea_list<-unique(asteroidea$morphotype)
mod<-mod%>% mutate(asteroidea_all = rowSums(across(all_of(intersect(asteroidea_list[-11], colnames(mod))))))
discr$asteroidea_all<-ifelse((mod$asteroidea_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(asteroidea_list[-11]))]

#gastropoda
gastropoda<-labels[grep("gastropods",labels$sub_group),]
gastropoda_list<-unique(gastropoda$morphotype)
mod<-mod%>% mutate(gastropoda_all = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
discr$gastropoda_all<-ifelse((mod$gastropoda_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#bivalves
bivalves<-labels[grep("non-burrowing bivalves|bivalves",labels$sub_group),]
bivalves_list<-unique(bivalves$morphotype)
mod<-mod%>% mutate(bivalves_all = rowSums(across(all_of(intersect(bivalves_list, colnames(mod))))))
discr$bivalves_all<-ifelse((mod$bivalves_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(bivalves_list))]

#macroalgae ALL
macroalgae<-labels[grep("macroalgae",labels$group),]
macroalgae_list<-unique(macroalgae$morphotype)
mod<-mod%>% mutate(macroalgae_all = rowSums(across(all_of(intersect(macroalgae_list, colnames(mod))))))
discr$macroalgae_all<-ifelse((mod$macroalgae_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(macroalgae_list))]

#macroalgae diversity
for (x in 1:nrow(mod)) {
  myrow<-mod[x,]
  myalgae<-intersect(macroalgae_list, colnames(myrow))
  myrowalgae<-myrow[,myalgae]
  snalgae<-rowSums(myrowalgae != 0)
  discr$macroalgae_div[x]<-as.numeric(snalgae)
}


#suspension feeders rare
rare_suspfeed<-labels[grep("anemones|ascidians|crinoids|holothurians|massive sponges|encrusting sponges|erect sponges",
                           labels$sub_group),]
rare_suspfeed_list<-unique(rare_suspfeed$morphotype)
mod<-mod%>% mutate(rare_suspfeed = rowSums(across(all_of(intersect(rare_suspfeed_list, colnames(mod))))))
discr$rare_suspfeed<-ifelse((mod$rare_suspfeed)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(rare_suspfeed_list))]

#worms
worms_all<-labels[grep("worms",labels$group),]
worms_list<-unique(worms_all$morphotype)
mod<-mod%>% mutate(worms_all = rowSums(across(all_of(intersect(worms_list, colnames(mod))))))
discr$worms_all<-ifelse((mod$worms_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(worms_list))]


#_________________________________________
## ADD LEVELS => ABSENT/LOW/HIGH (3 levels)
source("wham_code/functions/get.hilo.R")

discr$macroalgae_all<-as.factor(get.hilo(mod$macroalgae_all))


#________________________________________
## LOOK AT ALL OF IT, SATISFACTORY? THEN FINALIZE
final<-discr

final[sapply(final, is.numeric)]<-lapply(final[sapply(final, is.numeric)], 
                                         as.factor)

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
substrate<-mydata[,(names(mydata)) %in% c(labels_s$label_names)]
for (i in 1:length(colnames(substrate))) {
  colnames(substrate)[i]->mylabel
  myrow<-labels_s[labels_s$label_names == mylabel,]
  myrow$morphotype->colnames(substrate)[i]
}
substrate<-as.data.frame(t(rowsum(t(substrate), group = colnames(substrate), na.rm = T)))

selected_columns <- c("mud","sand_mud","fine_sand","coarse_sand","cobbles","gravel","pebble","consolidated","boulder","rock")  # Define which columns to use
selected_columns <- c("sand_mud","fine_sand","coarse_sand","cobbles","gravel","pebble","consolidated","boulder","rock")
#selected_columns <- c("mud","sand_mud","fine_sand","coarse_sand","rock") #HI
#selected_columns <- c("mud","fine_sand","coarse_sand","rock") #HIchannel
#selected_columns <- c("sand_mud","fine_sand","coarse_sand","cobbles","pebble","boulder","rock")

substrate$dominant_sediment <- selected_columns[max.col(substrate[,selected_columns], ties.method = "first")]

substrate$secondary_sediment <- apply(substrate[, selected_columns], 1, function(row) {
  # Check if more than one value is above 0
  if (sum(row > 0) > 1) {
    sorted_cols <- order(row, decreasing = TRUE)  # Order columns by value
    second_max_idx <- sorted_cols[2]  # Get the second highest value's index
    selected_columns[second_max_idx]  # Return the column name for second max
  } else {
    NA  # If there's only one value > 0, return NA
  }
})

#now give them a category
for (i in 1:nrow(substrate)) {if (substrate$dominant_sediment[i]=="mud") {1 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="sand_mud") {2 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="fine_sand") {3 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="coarse_sand") {4 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="gravel") {5 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="pebble") {5 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="cobbles") {5 -> substrate$substrate[i]} 
  else if (substrate$dominant_sediment[i]=="boulder") {6 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="rock") {6 -> substrate$substrate[i]}
  else if (substrate$dominant_sediment[i]=="consolidated") {6 -> substrate$substrate[i]}
  if (substrate$invisible[i]>0) {1 -> substrate$substrate_invisible[i]}
  if (substrate$invisible[i]==0) {0 -> substrate$substrate_invisible[i]}
  if (substrate$microalgal_biofilm[i]>0) {1 -> substrate$biofilm[i]}
  if (substrate$microalgal_biofilm[i]==0) {0 -> substrate$biofilm[i]}
} 

substrate<-substrate[,15:17] #choose only the newly made columns
#check
gg_melt <- pivot_longer(substrate,1:ncol(substrate),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  facet_wrap(~species) +
  theme_classic() +
  ggtitle("Distribution of bins")+
  labs(y="number of images")

substrate <- substrate %>% select(-c("biofilm"))

#sea ice (+ other evenetual env data)
SIC<-read.csv(file=paste0("wham_data/",campaign,"/datasets/TANGO_env_data.csv"))
#assign
sic<-c()
sic[241:292]<-sic5y[1,2] #MIN
sic[1:30]<-sic5y[2,2] #MIS
sic[293:394]<-sic5y[2,2] #MIS
sic[31:120]<-sic5y[3,2] #HI
sic[121:240]<-sic5y[4,2] #FH

#---------------------------------------------------------------------
# COMPILE THE FINAL DATASET FOR NEXT STEP
#---------------------------------------------------------------------
#merge env with the biota
#final_all<-cbind(final,substrate,sic)
final_all<-cbind(final,substrate)
final_all[sapply(final_all, is.factor)]<-lapply(final_all[sapply(final_all, is.factor)], 
                                                as.numeric)

#define name
dataset_name<-"MIN"

save(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
nodes<-colnames(final_all)
final_all<-rownames_to_column(final_all,var="image_filename")
final_all<-final_all[,-1]


write.table(nodes,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_nodes.txt"),
            row.names = FALSE,col.names = FALSE,quote=FALSE)
write.table(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,".txt"),
            row.names = FALSE,col.names = FALSE)

