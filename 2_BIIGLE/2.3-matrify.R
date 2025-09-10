#
#------------------------------------------------------------------------
#           STEP 2.3 in the WHAM-workflow : MATRIFY DATASET
#                       by LEA KATZ
#------------------------------------------------------------------------
# This script matrifies the abundance data from the images into a matrix 
# suited for biodiversity and community analysis with vegan or other 
# packages. It also replaces counts with cover for colonial animals, algae
# or other features.
# This script requires : 
# - an abundance dataset (like the one created in step 2.2)
# - a spreadsheet (or a list) with the labels that need to be replaced with cover

library(labdsv)
library(tidyverse)

# -------------------------------------------------------------
# LOAD DATA
# -------------------------------------------------------------
# the abundance data
load(file="wham_data/TANGO/datasets/TANGO_ALL_CLEAN_v2.RData")
mydataCLEANED->mydata
mydata<-mydata[grep("MI|HI|FH",mydata$station),] 
mydata<-mydata[-grep("substrate|macroalgae",mydata$group),]

# the label list
coverlabels<-read_csv(paste0("wham_data/TANGO/metadata/TANGO_labels_cover.csv"))
coverlabelsstring<-colnames(coverlabels)
mydata->cover

# check format
print(mydata)

# store info from site
site_info <- mydata %>%
  select(image_filename, site) %>%
  distinct()

# -------------------------------------------------------------
# MATRIFY
# -------------------------------------------------------------
mydata<- mydata %>% select(image_filename,morphotype,n)
mydata<-as.data.frame(mydata)
mydata_matrified<-matrify(mydata)

# -------------------------------------------------------------
# ADD COVER
# -------------------------------------------------------------
m<-as.matrix(mydata_matrified)
#m <- m[,-1]
#rownames(m) <- mydata_matrified[,1]

cover_wanted<-coverlabelsstring
for (x in 1:nrow(cover)) {
  i <- cover$image_filename[x]
  j <- cover$morphotype[x]
  z<-cover$cover[x]
  if (cover$morphotype[x] %in% cover_wanted) {m[i,j]<-z
  }
}
class(m) <- "numeric"
mydata_matrified<-as.data.frame(m)


# -------------------------------------------------------------
# ADD SITE
# -------------------------------------------------------------
mydata_matrified<-rownames_to_column(mydata_matrified,var="image_filename")
mydata_matrified_sites<- site_info %>% left_join(mydata_matrified,by="image_filename")


# -------------------------------------------------------------
# SAVE
# -------------------------------------------------------------
#save(mydata_matrified,file=paste0("wham_data/",campaign,"/matrified/",campaign,"_",dive,"_",square,"_m.RData"))
save(mydata_matrified,file=paste0("wham_data/",campaign,"/matrified/",campaign,"_",dataset,"_m.RData"))

save(mydata_matrified_sites,file=paste0("wham_data/",campaign,"/matrified/",campaign,"_",dataset,"_sites_m.RData"))

save(mydata_matrified, file="wham_data/TANGO2/matrified/TANGO2_CLEAN_fauna_cover_raw_m.RData")

