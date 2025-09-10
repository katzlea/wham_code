#
#------------------------------------------------------------------------
#           STEP 2.2 in the WHAM-workflow : GROUP BIIGLE ANNOTATIONS
#                       by LEA KATZ
#------------------------------------------------------------------------
# Using the "...abundance.RData" files created in step 2.1
# This script creates a grouped dataset for any group you like.
# You can rename each individual dataset manually.
# You can also export different kind of subsets.
#
# IMPORTANT : if there is already a grouped abundance file with same name, delete it or change dir.

# -------------------------------------------------------------
# DEFINE VARIABLES
# -------------------------------------------------------------
campaign="TANGO"
group="WAP"

metadata<-readxl::read_excel(paste0("wham_data/",campaign,"/metadata/",campaign,"_metadata.xlsx")) 

#get filenames
dir<-paste0("wham_data/",campaign,"/datasets/") 

import<-list.files(dir) 

# -------------------------------------------------------------
# GIVE NEW NAMES AND MERGE
# -------------------------------------------------------------
#new names
print(import) #see what old names are
#then manually rename if you like
sites<-c("FH_boat1","FH_boat2","FH_boat3",
         "FH_south1","FH_boat4","FH_south2","FH_south3",
         "FH_wreck1","FH_wreck2","FH_wreck3","FH_wreck4",
         "FH_wall1","FH_wall2","FH_wall3")
#lon<-c()
#lat<-c()

#merge files
for (i in 1:length(import)) {
  load(paste0(dir,"/",import[i]))
  site<-sites[i]
  x <- mydata
  #x <- abundance_images
  x["site"]=paste0(site)
  if (i == 1) {mydata2<-x} else {mydata2<-bind_rows(mydata2,x)}
}
mydata2->mydata

#save
save(mydata,file=paste0("wham_data/",campaign,"/datasets/",campaign,"_",group,".RData"))

# -------------------------------------------------------------
# MAKE SUBSETS
# -------------------------------------------------------------
campaign="TANGO2"
group="WAP"
load(file=paste0("wham_data/",campaign,"/datasets/",campaign,"_",group,".RData"))

taxa<-read_csv2(paste0("wham_data/",campaign,"/metadata/",campaign,"_taxa.csv"))
substrate<-read_csv(paste0("wham_data/",campaign,"/metadata/",campaign,"_substrate.csv"))
other<-read_csv2(paste0("wham_data/",campaign,"/metadata/",campaign,"_other.csv"))

#BIOTA
mydata<- mydata %>% filter(label_names %in% taxa$taxa)
save(mydata,file=paste0("wham_data/",campaign,"/datasets/",campaign,"_",group,"_biota.RData"))

#SUBSTRATE
mydata<- mydata %>% filter(label_names %in% substrate$label_name)
save(mydata,file=paste0("wham_data/",campaign,"/datasets/",campaign,"_",group,"_substrate.RData"))

