#
#-----------------------------------------------------------------
#           STEP 5 in the WHAM-workflow : PREP FOR BNI
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
labels<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_labels.csv"))

labels_b<-labels[-grep("substrate",labels$group),]
labels_s<-labels[grep("substrate",labels$group),]

mydata<-mydata_matrified
mydata<-as.data.frame(sapply(mydata,as.numeric))
rownames(mydata)<-rownames(mydata_matrified)

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
ncolbiota<-94 #94 for all, 69 for MI, 54 for HI, 65 for FH
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
  "mollusc"  #unidentified stuff
))

#________________________________________
## GROUP STUFF

#asteroidea
asteroidea<-labels[grep("asteroidea",labels$sub_group),]
asteroidea_list<-unique(asteroidea$morphotype)
mod<-mod%>% mutate(asteroidea_all = rowSums(across(all_of(intersect(asteroidea_list, colnames(mod))))))
discr$asteroidea_all<-ifelse((mod$asteroidea_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(asteroidea_list))]

#asteroidea diversity
for (x in 1:nrow(mod)) {
  myrow<-mod[x,]
  mystars<-intersect(asteroidea_list, colnames(myrow))
  myrowstars<-myrow[,mystars]
  snstars<-rowSums(myrowstars != 0)
  discr$asteroidea_div[x]<-as.numeric(snstars)
}

#porifera
#porifera<-labels[grep("sponges",labels$group),]
#porifera_list<-unique(porifera$morphotype)
#mod<-mod%>% mutate(porifera_all = rowSums(across(all_of(intersect(porifera_list, colnames(mod))))))
#discr$porifera_all<-ifelse((mod$porifera_all)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(porifera_list))]

#gastropoda
gastropoda<-labels[grep("gastropods",labels$sub_group),]
gastropoda_list<-unique(gastropoda$morphotype)
mod<-mod%>% mutate(gastropoda_all = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
discr$gastropoda_all<-ifelse((mod$gastropoda_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#gastropoda for Foyn harbor
gastropoda<-labels[grep("gastropod|nacella|nudibranch|wavy_msp3|white_bunny_msp1|white_porcupine_msp2",labels$morphotype),]
gastropoda_list<-unique(gastropoda$morphotype)
mod<-mod%>% mutate(gastropoda_all = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
discr$gastropoda_all<-ifelse((mod$gastropoda_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#echinodermata
echinodermata<-labels[grep("holothurians|ophiuroids|crinoids|echinoidea",labels$sub_group),]
echinodermata_list<-unique(echinodermata$morphotype)
mod<-mod%>% mutate(echinodermata_all = rowSums(across(all_of(intersect(echinodermata_list, colnames(mod))))))
discr$echinodermata_all<-ifelse((mod$echinodermata_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(echinodermata_list))]

#anemones
anemones<-labels[grep("anemones",labels$sub_group),]
anemones_list<-unique(anemones$morphotype)
mod<-mod%>% mutate(anemones_all = rowSums(across(all_of(intersect(anemones_list, colnames(mod))))))
discr$anemones_all<-ifelse((mod$anemones_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(anemones_list))]

#anemones for hovgaard
anemones<-labels[grep("orange_anemone_msp1|white_anemone_msp3|yellow_anemone_msp2",labels$morphotype),]
anemones_list<-unique(anemones$morphotype)
mod<-mod%>% mutate(anemones_all = rowSums(across(all_of(intersect(anemones_list, colnames(mod))))))
discr$anemones_all<-ifelse((mod$anemones_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(anemones_list))]

#bivalves
bivalves<-labels[grep("non-burrowing bivalves|bivalves",labels$sub_group),]
bivalves_list<-unique(bivalves$morphotype)
mod<-mod%>% mutate(bivalves_all = rowSums(across(all_of(intersect(bivalves_list, colnames(mod))))))
discr$bivalves_all<-ifelse((mod$bivalves_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(bivalves_list))]

#tunicata
#tunicata<-labels[grep("tunicates",labels$group),]
#tunicata_list<-unique(tunicata$morphotype)
#mod<-mod%>% mutate(tunicata_all = rowSums(across(all_of(intersect(tunicata_list, colnames(mod))))))
#discr$tunicata_all<-ifelse((mod$tunicata_all)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(tunicata_list))]

#rare mobile stuff
rare_mobile<-labels[grep("worm|bryozoa|fish|glypt_ant|white_amphipod_msp1|orange_lump_free|pycnogonid_msp1|small_pink_worm_msp1|small_white_worm_msp2|parborlasia_corrugatus",labels$morphotype),]
rare_mobile_list<-unique(rare_mobile$morphotype)
mod<-mod%>% mutate(rare_mobile = rowSums(across(all_of(intersect(rare_mobile_list, colnames(mod))))))
discr$rare_mobile<-ifelse((mod$rare_mobile)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(rare_mobile_list))]

#brown algae
brown_algae<-labels[grep("brown",labels$algaecolor),]
brown_algae_list<-unique(brown_algae$morphotype)
mod<-mod%>% mutate(brown_algae = rowSums(across(all_of(intersect(brown_algae_list, colnames(mod))))))
discr$brown_algae<-ifelse((mod$brown_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(brown_algae_list))]

#rare sessile stuff
rare_sessile<-labels[grep("encrusting_sponge|erect_sponge|fine_orange_msp4|finger_sponge_msp1|large_yellow_msp3|orange_blob_msp5|spiky_ball_sponge_msp6|spiky_massive_msp9|spiky_yellow_msp7|porifera|white_massive_msp10|yellow_blob_msp2|yellow_finger_msp8|echiura_msp1|VME_unknown|terebellidae|ascidian|pale_yellow_ascidian_msp5|red_brown_ascidian_msp4|translucent_blob|white_ascidian_msp3|yellow_ascidian_msp2|yellow_brown_ascidian_msp1",labels$morphotype),]
rare_sessile_list<-unique(rare_sessile$morphotype)
mod<-mod%>% mutate(rare_sessile = rowSums(across(all_of(intersect(rare_sessile_list, colnames(mod))))))
discr$rare_sessile<-ifelse((mod$rare_sessile)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(rare_sessile_list))]

#other algae
other_algae<-labels[grep("green_sheet_algae_msp1|filamentous_filiform_algae|filamentous_green_algae|erect_coarse_algae|erect_fine_branching|erect_fine_branching_red|red_or_brown_sheet_algae_msp0|green_encr_algae|encrusting_algae|macroalgae",labels$morphotype),]
other_algae_list<-unique(other_algae$morphotype)
mod<-mod%>% mutate(other_algae = rowSums(across(all_of(intersect(other_algae_list, colnames(mod))))))
discr$other_algae<-ifelse((mod$other_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(other_algae_list))]

#red branching algae
red_branching_algae<-labels[grep("branching_red_algae_msp1|branching_red_algae_msp2",labels$morphotype),]
red_branching_list<-unique(red_branching_algae$morphotype)
mod<-mod%>% mutate(red_branching = rowSums(across(all_of(intersect(red_branching_list, colnames(mod))))))
discr$other_algae<-ifelse((mod$other_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(red_branching_list))]

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

#_________________________________________
## CHECK DISTRIBTION OF COVER
#hist(mydata$macroalgae)
#hist(substrate$substrate)

#_________________________________________
## ADD LEVELS => ABSENT/LOW/HIGH (3 levels)
source("wham_code/functions/get.hilo.R")

discr$white_snail<-as.factor(get.hilo(mydata$white_snail))
discr$red_sheet_msp1<-as.factor(get.hilo(mydata$red_sheet_msp1))
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

#selected_columns <- c("mud","sand_mud","fine_sand","coarse_sand","cobbles","gravel","pebble","consolidated","boulder","rock")  # Define which columns to use
#selected_columns <- c("sand_mud","fine_sand","coarse_sand","cobbles","gravel","pebble","consolidated","boulder","rock")
selected_columns <- c("mud","sand_mud","fine_sand","coarse_sand","rock")
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
  if (substrate$rock[i]>0) {1 -> substrate$rock_present[i]}
  if (substrate$rock[i]==0) {0 -> substrate$rock_present[i]}
} 

substrate<-substrate[,11:14] #choose only the newly made columns
#check
gg_melt <- pivot_longer(substrate,1:ncol(substrate),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  facet_wrap(~species) +
  theme_classic() +
  ggtitle("Distribution of bins")+
  labs(y="number of images")


#---------------------------------------------------------------------
# COMPILE THE FINAL DATASET FOR NEXT STEP
#---------------------------------------------------------------------
#merge env with the biota
final_all<-cbind(final,substrate)
final_all[sapply(final_all, is.factor)]<-lapply(final_all[sapply(final_all, is.factor)], 
                                                as.numeric)

#define name
dataset_name<-"HI_cust2"

save(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
nodes<-colnames(final_all)
final_all<-rownames_to_column(final_all,var="image_filename")
final_all<-final_all[,-1]


write.table(nodes,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_nodes.txt"),
            row.names = FALSE,col.names = FALSE,quote=FALSE)
write.table(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,".txt"),
            row.names = FALSE,col.names = FALSE)

