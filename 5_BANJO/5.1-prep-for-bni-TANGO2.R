#
#-----------------------------------------------------------------
#           STEP 5 in the WHAM-workflow : PREP FOR BNI
#                   Adapted for TANGO2 data
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
#load(paste0("wham_data/",campaign,"/matrified/",campaign,"_",dataset,"_sites_m.RData"))

labels<-read.csv(file=paste0("wham_data/",campaign,"/metadata/",campaign,"_labels.csv"))

labels_b<-labels[-grep("substrate",labels$group),]
labels_s<-labels[grep("substrate",labels$group),]

#--------------spatial scale
#by images
mydata<-mydata_matrified

#by transects
mydata_matrified_sites<-mydata_matrified_sites[,-1]
temp <- mydata_matrified_sites %>%
  group_by(site) %>%
  summarise(across(everything(),sum,na.rm=TRUE)) 
mydata_sites<-temp[,-1] 
rownames(mydata_sites)<-temp$site

#by site
mydata_matrified_sites<-mydata_matrified_sites[,-1]
mydata_matrified_sites$site <- str_sub(mydata_matrified_sites$site, 1, -2)
temp <- mydata_matrified_sites %>%
  group_by(site) %>%
  summarise(across(everything(),sum,na.rm=TRUE)) 


mydata_sites<-temp[,-1] 
rownames(mydata_sites)<-temp$site  

mydata<-as.data.frame(sapply(mydata_sites,as.numeric))
rownames(mydata)<-rownames(mydata_sites)


#---------------------------------------
#site subsets
#---------------------------------------
##HI_out
mydata<-mydata[1:30,]
##HI_channel
mydata<-mydata[31:60,]
##HI_south
mydata<-mydata[61:90,]

#MIN
mydata<-mydata[31:82,]
#MIS
mydata<-mydata[-(31:82),]
#MIS_outer
mydata<-mydata[1:30,]
#MIS_mid
mydata<-mydata[118:147,]
#MIS_inner
mydata<-mydata[c(83:117,148:184),]

#FH_boat
mydata<-mydata[c(1:23,34:40),]
#FH_south
mydata<-mydata[c(24:33,41:60),]
#FH_wall
mydata<-mydata[91:120,]
#FH_wreck
mydata<-mydata[61:90,]

#WAP without BI
mydata<-mydata[-c(150:200),]


#homogenize morphotype names
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
ncolbiota<-69 #94 for all, 69 for MI, 54 for HI, 65 for FH
discr[,1:ncolbiota]<-lapply(discr[,1:ncolbiota],function(x) ifelse(x==0,0,1))

# CHECK DISTRIBUTION OF PRESENCE/ABSENCE
gg_melt <- pivot_longer(discr,cols=1:length(unique(colnames(discr))),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  geom_text(stat="count",aes(label=..count..)) +
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
discr <- discr %>% select(-c("")) #write here all nodes that need to be removed

discr <- discr %>% select(-c(  #for NWAP
  "bryozoa","fish","glypt_ant","mollusc","orange_lump_free","pycnogonid_msp1",
  "star_impression","thingy","urchin_test","VME_unknown","white_amphipod_msp1","wtf"))

discr <- discr %>% select(-c("glypt_ant","thingy","mollusc","orange_lump_free","urchin_test","wtf")) #FH_NWAP
discr <- discr %>% select(-c("fish","glypt_ant","mollusc","orange_lump_free","pycnogonid_msp1","star_impression","urchin_test","white_amphipod_msp1","wtf")) #HI_NWAP
discr <- discr %>% select(-c("bryozoa","fish","orange_lump_free","porifera_all","star_impression","thingy","urchin_test","VME_unknown","worms_all","wtf")) #MIN_NWAP
discr <- discr %>% select(-c("bryozoa","fish","orange_lump_free","star_impression","thingy","urchin_test","VME_unknown","wtf")) #MIS_NWAP

discr <- discr %>% select(-c("urchin_test","star_impression","fish","wtf","white_amphipod_msp1","pycnogonid_msp1","glypt_ant","orange_lump_free")) #HI outer

discr <- discr %>% select(-c("brittle_msp1","brown_sheet_algae","encrusting_algae","encrusting_sponge","erect_coarse_algae","erect_fine_branching","erect_fine_branching_brown","erect_fine_branching_red","filamentous_filiform_algae","filamentous_green_algae","green_encr_algae","laminate_brown","macroalgae","porifera","red_or_brown_sheet_algae_msp0","red_ball_in_algae"))
discr <- discr %>% select(-c("erect_fine_branching","mollusc","pink_encr_algae_msp1","urchin_msp1","worms_all"))

discr <- discr %>% select(-c("asteroidea_all","bivalves_all","brown_algae","erect_fine_branching","filamentous_filiform_algae","fish","glypt_ant","green_algae","mollusc","orange_lump_free","pink_encr_algae_msp1","pycnogonid_msp1","star_impression","urchin_msp1","urchin_test","white_amphipod_msp1","worms_all","wtf"))
discr <- discr %>% select(-c("glypt_ant","thingy","mollusc","orange_lump_free","urchin_test","worms_all","wtf"))

discr <- discr %>% select(-c("brown_algae","erect_coarse_algae","erect_fine_branching","green_algae","holoth_all"))
discr <- discr %>% select(-c("bivalves_all","brittle_msp1","erect_coarse_algae","erect_fine_branching","holoth_all","glypt_ant","green_algae","mollusc","orange_lump_free","thingy","urchin_msp1","urchin_test","worms_all","wtf"))
discr <- discr %>% select(-c("macroalgae","anemones_all","filamentous_green_algae","tunicata_all2"))
discr <- discr %>% select(-c("bivalves_all","encrusting_algae","green_algae","holoth_all","red_or_brown_sheet_algae_msp0","urchin_msp1"))
discr <- discr %>% select(-c("erect_fine_branching"))

discr<- discr %>% select(c("branching_red_algae","gastropoda_all2","red_sheet_msp1"))
#_____________________________________
## GROUP STUFF

#asteroidea
asteroidea<-labels[grep("asteroidea",labels$sub_group),]
asteroidea_list<-unique(asteroidea$morphotype) #[-10]
mod<-mod%>% mutate(asteroidea_all = rowSums(across(all_of(intersect(asteroidea_list, colnames(mod))))))
discr$asteroidea_all<-ifelse((mod$asteroidea_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(asteroidea_list))]

#asteroidea2
asteroidea2_list<-c(
  "big_spotted_star_msp14","brown_star_msp9","carrot_star_msp4",       
  "chunky_orange_star_msp16","chunky_star_msp11","glitter_star_msp3",
  "light_brown_star_msp15","medium_yellow_star_msp13",     
  "skinny_white_star_msp12","skinny_yellow_star_msp10",
  "small_yellow_star_msp6","asteroidea","sun_star_msp2","white_star_msp8",   
  "yellow_star_msp5"   
)
mod<-mod%>% mutate(asteroidea2 = rowSums(across(all_of(intersect(asteroidea2_list, colnames(mod))))))
discr$asteroidea2<-ifelse((mod$asteroidea2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(asteroidea2_list))]

#porifera
porifera<-labels[grep("sponges",labels$group),]
porifera_list<-unique(porifera$morphotype)
mod<-mod%>% mutate(porifera_all = rowSums(across(all_of(intersect(porifera_list, colnames(mod))))))
discr$porifera_all<-ifelse((mod$porifera_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(porifera_list))]

#porifera massive
porifera_massive<-labels[grep("massive sponges",labels$sub_group),]
porifera_massive_list<-unique(porifera_massive$morphotype)
mod<-mod%>% mutate(porifera_massive = rowSums(across(all_of(intersect(porifera_massive_list, colnames(mod))))))
discr$porifera_massive<-ifelse((mod$porifera_massive)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(porifera_massive_list))]

#porifera erect
porifera_erect<-labels[grep("erect sponges",labels$sub_group),]
porifera_erect_list<-unique(porifera_erect$morphotype)
mod<-mod%>% mutate(porifera_erect = rowSums(across(all_of(intersect(porifera_erect_list, colnames(mod))))))
discr$porifera_erect<-ifelse((mod$porifera_erect)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(porifera_erect_list))]

#gastropoda
gastropoda<-labels[grep("gastropods",labels$sub_group),]
gastropoda_list<-unique(gastropoda$morphotype) #[-7]
mod<-mod%>% mutate(gastropoda_all = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
discr$gastropoda_all<-ifelse((mod$gastropoda_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#gastropoda2
gastropoda_list<-c("gastropoda_all","red_ball_in_algae")
mod<-mod%>% mutate(gastropoda_all2 = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
discr$gastropoda_all2<-ifelse((mod$gastropoda_all2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#nudibranch
nudibranch_list<-c("nudibranch","white_porcupine_msp2","white_bunny_msp1","wavy_msp3")
mod<-mod%>% mutate(nudibranch_all = rowSums(across(all_of(intersect(nudibranch_list, colnames(mod))))))
discr$nudibranch_all<-ifelse((mod$nudibranch_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(nudibranch_list[-7]))]

#gastropoda for Foyn harbor
#gastropoda<-labels[grep("gastropod|nacella|nudibranch|wavy_msp3|white_bunny_msp1|white_porcupine_msp2",labels$morphotype),]
#gastropoda_list<-unique(gastropoda$morphotype)
#mod<-mod%>% mutate(gastropoda_all = rowSums(across(all_of(intersect(gastropoda_list, colnames(mod))))))
#discr$gastropoda_all<-ifelse((mod$gastropoda_all)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(gastropoda_list))]

#echinodermata
echinodermata<-labels[grep("echinoderms",labels$group),]
echinodermata_list<-unique(echinodermata$morphotype)
mod<-mod%>% mutate(echinodermata_all = rowSums(across(all_of(intersect(echinodermata_list, colnames(mod))))))
discr$echinodermata_all<-ifelse((mod$echinodermata_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(echinodermata_list))]

#holothurians
holoth<-labels[grep("holothurians",labels$sub_group),]
holoth_list<-unique(holoth$morphotype)
mod<-mod%>% mutate(holoth_all = rowSums(across(all_of(intersect(holoth_list, colnames(mod))))))
discr$holoth_all<-ifelse((mod$holoth_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(holoth_list))]

#echinodermata2
echinodermata_list<-c("echinodermata_all","asteroidea_all")
mod<-mod%>% mutate(echinodermata_all2 = rowSums(across(all_of(intersect(echinodermata_list, colnames(mod))))))
discr$echinodermata_all2<-ifelse((mod$echinodermata_all2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(echinodermata_list))]

#echinodermata3 (ast+urchins)
echinodermata_list<-c("urchin_msp1","asteroidea_all")
mod<-mod%>% mutate(echinodermata_all2 = rowSums(across(all_of(intersect(echinodermata_list, colnames(mod))))))
discr$echinodermata_all2<-ifelse((mod$echinodermata_all2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(echinodermata_list))]

#anemones
anemones<-labels[grep("anemones",labels$sub_group),]
anemones_list<-unique(anemones$morphotype) #[-2] #edwardsiella
mod<-mod%>% mutate(anemones_all = rowSums(across(all_of(intersect(anemones_list, colnames(mod))))))
discr$anemones_all<-ifelse((mod$anemones_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(anemones_list))]

#anemones for hovgaard
#anemones<-labels[grep("anemones",labels$sub_group),]
#anemones_list<-unique(anemones$morphotype)
#mod<-mod%>% mutate(anemones_all = rowSums(across(all_of(intersect(anemones_list[-2], colnames(mod))))))
#discr$anemones_all<-ifelse((mod$anemones_all)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(anemones_list[-2]))]

#bivalves
bivalves<-labels[grep("non-burrowing bivalves|bivalves",labels$sub_group),]
bivalves_list<-unique(bivalves$morphotype)
mod<-mod%>% mutate(bivalves_all = rowSums(across(all_of(intersect(bivalves_list, colnames(mod))))))
discr$bivalves_all<-ifelse((mod$bivalves_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(bivalves_list))]

#bivalves
bivalves<-labels[grep("non-burrowing bivalves",labels$sub_group),]
bivalves_list<-c(unique(bivalves$morphotype),"bivalve")
mod<-mod%>% mutate(bivalves_all = rowSums(across(all_of(intersect(bivalves_list, colnames(mod))))))
discr$bivalves_all<-ifelse((mod$bivalves_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(bivalves_list))]

#molluscs
#molluscs<-labels[grep("mollusc",labels$group),]
#molluscs_list<-unique(molluscs$morphotype)
#mod<-mod%>% mutate(molluscs_all = rowSums(across(all_of(intersect(molluscs_list, colnames(mod))))))
#discr$molluscs_all<-ifelse((mod$molluscs_all)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(molluscs_list))]

#tunicata
tunicata<-labels[grep("tunicates",labels$group),]
tunicata_list<-unique(tunicata$morphotype)
mod<-mod%>% mutate(tunicata_all = rowSums(across(all_of(intersect(tunicata_list, colnames(mod))))))
discr$tunicata_all<-ifelse((mod$tunicata_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(tunicata_list))]

#tunicata2
tunicata_list<-c("tunicata_all","translucent_blob")
mod<-mod%>% mutate(tunicata_all2 = rowSums(across(all_of(intersect(tunicata_list, colnames(mod))))))
discr$tunicata_all2<-ifelse((mod$tunicata_all2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(tunicata_list))]

#worms
worms<-labels[grep("worms",labels$group),]
worms_list<-unique(worms$morphotype) #[-6]
mod<-mod%>% mutate(worms_all = rowSums(across(all_of(intersect(worms_list, colnames(mod))))))
discr$worms_all<-ifelse((mod$worms_all)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(worms_list))]

#desmarestia
desmarestia_list<-c("desmarestia_msp1","desmarestia_msp2")
mod<-mod%>% mutate(desmarestia = rowSums(across(all_of(intersect(desmarestia_list, colnames(mod))))))
discr$desmarestia<-ifelse((mod$desmarestia)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(desmarestia_list))]

#brown algae
brown_list<-c("brown_sheet_algae","desmarestia_msp1","desmarestia_msp2","erect_fine_branching_brown","himantothallus_msp1","laminate_brown")
mod<-mod%>% mutate(brown_algae = rowSums(across(all_of(intersect(brown_list, colnames(mod))))))
discr$brown_algae<-ifelse((mod$brown_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(brown_list))]

#red algae
red_list<-c("branching_red_algae_msp1","branching_red_algae_msp2","red_sheet_msp1","erect_fine_branching_red")
mod<-mod%>% mutate(red_algae = rowSums(across(all_of(intersect(red_list, colnames(mod))))))
discr$red_algae<-ifelse((mod$red_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(red_list))]

#branching_red algae
branching_red_list<-c("branching_red_algae_msp1","branching_red_algae_msp2","erect_fine_branching_red")
mod<-mod%>% mutate(branching_red_algae = rowSums(across(all_of(intersect(branching_red_list, colnames(mod))))))
discr$branching_red_algae<-ifelse((mod$branching_red_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(branching_red_list))]

#green algae
green_list<-c("green_encr_algae","green_sheet_algae_msp1")
mod<-mod%>% mutate(green_algae = rowSums(across(all_of(intersect(green_list, colnames(mod))))))
discr$green_algae<-ifelse((mod$green_algae)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(green_list))]

#rare sessile stuff
#rare_sessile<-labels[grep("terebellidae|encrusting_sponge|erect_sponge|fine_orange_msp4|finger_sponge_msp1|large_yellow_msp3|orange_blob_msp5|spiky_ball_sponge_msp6|spiky_massive_msp9|spiky_yellow_msp7|porifera|white_massive_msp10|yellow_blob_msp2|yellow_finger_msp8|echiura_msp1|VME_unknown|ascidian|pale_yellow_ascidian_msp5|red_brown_ascidian_msp4|translucent_blob|white_ascidian_msp3|yellow_ascidian_msp2|yellow_brown_ascidian_msp1",labels$morphotype),]
#rare_sessile_list<-unique(rare_sessile$morphotype)
#<-mod%>% mutate(rare_sessile = rowSums(across(all_of(intersect(rare_sessile_list, colnames(mod))))))
#discr$rare_sessile<-ifelse((mod$rare_sessile)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(rare_sessile_list))]

#rare sessile stuff for HI outer
#rare_sessile<-labels[grep("echiura_msp1|VME_unknown|terebellidae|ascidian|pale_yellow_ascidian_msp5|red_brown_ascidian_msp4|translucent_blob|white_ascidian_msp3|yellow_ascidian_msp2|yellow_brown_ascidian_msp1,anemones_all",labels$morphotype),]
#rare_sessile_list<-unique(rare_sessile$morphotype)
#mod<-mod%>% mutate(rare_sessile = rowSums(across(all_of(intersect(rare_sessile_list, colnames(mod))))))
#discr$rare_sessile<-ifelse((mod$rare_sessile)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(rare_sessile_list))]

#suspension feeders rare (holoth,ascidian) for HI channel
#rare_suspfeed<-labels[grep("blue_holoth_msp1|finger_sponge_msp1|orange_blob_msp5|spiky_yellow_msp7|translucent_blob|white_ascidian_msp3|yellow_ascidian_msp2",labels$morphotype),]
#rare_suspfeed_list<-unique(rare_suspfeed$morphotype)
#mod<-mod%>% mutate(rare_suspfeed = rowSums(across(all_of(intersect(rare_suspfeed_list, colnames(mod))))))
#discr$rare_suspfeed<-ifelse((mod$rare_suspfeed)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(rare_suspfeed_list))]

#suspension feeders rare (holoth,ascidian) for HI south
#rare_suspfeed<-labels[grep("orange_holoth_msp4|yellow_brown_ascidian_msp1",labels$morphotype),]
#rare_suspfeed_list<-unique(rare_suspfeed$morphotype)
#mod<-mod%>% mutate(rare_suspfeed = rowSums(across(all_of(intersect(rare_suspfeed_list, colnames(mod))))))
#discr$rare_suspfeed<-ifelse((mod$rare_suspfeed)==0,0,1)
#discr<-discr[ , !(names(discr) %in% c(rare_suspfeed_list))]

#suspension feeders rare (porifera,anemones,bryozoa) for MI mid/MI
#rare_sessile_list<-c("anemones_all","tunicata_all2")
rare_sessile_list<-c("susp_feed","holoth_all")
mod<-mod%>% mutate(susp_feed2 = rowSums(across(all_of(intersect(rare_sessile_list, colnames(mod))))))
discr$susp_feed2<-ifelse((mod$susp_feed2)==0,0,1)
discr<-discr[ , !(names(discr) %in% c(rare_sessile_list))]

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
discr$pink_star_msp1<-as.factor(get.hilo(mod$pink_star_msp1))
discr$laternula<-as.factor(get.hilo(mod$laternula))
discr$edwardsiella_msp4<-as.factor(get.hilo(mod$edwardsiella_msp4))
discr$porifera_all<-as.factor(get.hilo(mod$porifera_all))
discr$anemones_all<-as.factor(get.hilo(mod$anemones_all))
discr$asteroidea_all<-as.factor(get.hilo(mod$asteroidea_all))
discr$gastropoda_all2<-as.factor(get.hilo(mod$gastropoda_all2))
discr$susp_feed<-as.factor(get.hilo(mod$susp_feed))

discr$branching_red_algae<-as.factor(get.hilo(mod$branching_red_algae))
discr$branching_red_algae_msp2<-as.factor(get.hilo(mod$branching_red_algae_msp2))
discr$pink_encr_algae_msp1<-as.factor(get.hilo(mod$pink_encr_algae_msp1))
discr$porifera_massive<-as.factor(get.hilo(mod$porifera_massive))
discr$himantothallus_msp1<-as.factor(get.hilo(mod$himantothallus_msp1))
discr$asteroidea2<-as.factor(get.hilo(mod$asteroidea2))
discr$worms_all<-as.factor(get.hilo(mod$worms_all))
discr$nacella<-as.factor(get.hilo(mod$nacella))

discr$filamentous_filiform_algae<-as.factor(get.hilo(mod$filamentous_filiform_algae))

discr$pink_star_msp1<-as.factor(get.hilo(mod$pink_star_msp1))
discr$brown_algae<-as.factor(get.hilo(mod$brown_algae))

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

substrate<-substrate[,16:18] #choose only the newly made columns, the 3 last
#check
gg_melt <- pivot_longer(substrate,1:ncol(substrate),names_to="species")
ggplot(gg_melt, aes(x=value,fill=factor(value))) +
  geom_bar(stat="count") +
  facet_wrap(~species) +
  theme_classic() +
  ggtitle("Distribution of bins")+
  labs(y="number of images")

substrate <- substrate %>% select(-c("substrate_invisible","biofilm"))

#sea ice (+ other evenetual env data)
SIC<-read.csv(file=paste0("wham_data/",campaign,"/datasets/TANGO_env_data.csv"))
#assign
sic<-c()
sic[241:292]<-SIC[1,2] #MIN
sic[1:30]<-SIC[2,2] #MIS
sic[293:394]<-SIC[2,2] #MIS
sic[31:120]<-SIC[3,2] #HI
sic[121:240]<-SIC[4,2] #FH

sic[24]<-SIC[1,2] #MIN
sic[25:34]<-SIC[2,2] #MIS
sic[15:23]<-SIC[3,2] #HI
sic[1:14]<-SIC[4,2] #FH

sic[8]<-SIC[1,2] #MIN
sic[9:11]<-SIC[2,2] #MIS
sic[5:7]<-SIC[3,2] #HI
sic[1:4]<-SIC[4,2] #FH

substrate$sic<-as.numeric(sic)
sic<-as.numeric(sic)

#---------------------------------------------------------------------
# COMPILE THE FINAL DATASET FOR NEXT STEP
#---------------------------------------------------------------------
#merge env with the biota

#final_all<-cbind(final,substrate,sic)
final_all<-cbind(final,substrate)
final_all[sapply(final_all, is.factor)] <- lapply(final_all[sapply(final_all, is.factor)], 
                                                  function(x) as.numeric(as.character(x)))


#define name
dataset_name<-"MIS_inner_OK"

save(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
nodes<-colnames(final_all)

final_all<-rownames_to_column(final_all,var="image_filename")
final_all<-final_all[,-1]


write.table(nodes,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_nodes.txt"),
            row.names = FALSE,col.names = FALSE,quote=FALSE)
write.table(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,".txt"),
            row.names = FALSE,col.names = FALSE)

#correction
campaign="TANGO2"
dataset_name<-"MIN_OK"

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
colnames(final_all)

#CHANGE RANGE OF BIOTIC VARIABLES
final_all[, 1:10] <- lapply(final_all[, 1:10], function(x) if (is.numeric(x)) x - 1 else x)

save(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))

nodes<-colnames(final_all)
final_all<-rownames_to_column(final_all,var="image_filename")
final_all<-final_all[,-1]

write.table(nodes,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_nodes.txt"),
            row.names = FALSE,col.names = FALSE,quote=FALSE)
write.table(final_all,file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,".txt"),
            row.names = FALSE,col.names = FALSE)
