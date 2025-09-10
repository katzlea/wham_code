#
#------------------------------------------------------------------------
#           STEP 2.1 in the WHAM-workflow : PROCESS BIIGLE ANNOTATIONS
#                       by LEA KATZ
#------------------------------------------------------------------------
# This script is specially written for BIIGLE area reports. 
# It calculates relative cover of each annotation (in sqpx), for orthomosaics or single images,
# then makes a sum for each label.
# The output is called "campaign_dive_square_abundance.RData"
# It also computes a total abundance per image, then it's called "...abundance_images.RData"

#install.packages("readxl","dplyr")
library(readxl)
library(dplyr)
source("wham_code/functions/headertrue.R")

# -------------------------------------------------------------
# DEFINE VARIABLES
# -------------------------------------------------------------
campaign="TANGO1"
dive="ROV19"
square="t2" #or transect

#load data
area<-read_xlsx(paste0("wham_data/",campaign,"/biigle_outputs/",campaign,"_",dive,"_",square,"_image_annotation_area_report.xlsx"))
area<-header.true(area)

# -------------------------------------------------------------
# FOR TRANSECTS with separate images
# -------------------------------------------------------------
area$annotation_area_sqpx<-as.numeric(area$annotation_area_sqpx)
area$annotation_width_px<-as.numeric(area$annotation_width_px)
abundance_images<- NA

#count and get cover, images seperate
for (i in 1:length(unique(area$image_filename))) {
  unique(area$image_filename)[i]->image
  image_area<-subset(area,image_filename==image)
  #cover
  total=max(image_area$annotation_area_sqpx)
  image_area<-image_area %>% mutate (cover = (annotation_area_sqpx*100)/total)
  for (u in 1:length(unique(image_area$image_filename))) {
    for (s in 1:nrow(image_area)) {
      if (image_area$shape_name[s] == "Polygon" | image_area$shape_name[s] == "Circle") { #for all polygons
        image_area$cover[s]<-(image_area$annotation_area_sqpx[s]*100)/total
      }
      else if (image_area$label_names[s] == "laternula") {
        image_area$cover[s]<-(((((image_area$annotation_width_px[s]*0.37618213)/2)^2*3.14)*2)*100)/total #proxy for area from segment length
      }
      else if (image_area$label_names[s] == "Parborlasia_corrugatus") {
        image_area$cover[s]<-((image_area$annotation_width_px[s]*25)*100)/total #proxy for area from segment length
      }
      else if (image_area$label_names[s] == "Worms") {
        image_area$cover[s]<-((image_area$annotation_width_px[s]*5)*100)/total #proxy for area from segment length
      }
    }
    }
  #abundance
  image_abundance<- image_area %>% count(label_names)
  image_sum<-image_area %>% group_by(image_filename, label_names) %>% summarize(cover = sum(cover))
  image_abundance<- inner_join(image_abundance,image_sum,by="label_names",copy=FALSE)
  abundance_images<-rbind(abundance_images,image_abundance)
  }
abundance_images<-abundance_images[-1,]

#sum for all images
abundance<- abundance_images %>% group_by(label_names) %>% summarize(cover=sum(cover),n=sum(n))
abundance$image_filename<-paste0(campaign,"_",dive,"_",square)

#save
#save(abundance,file=paste0("wham_data/",campaign,"/biigle_outputs/processed/",campaign,"_",dive,"_",square,"_abundance.RData"))
save(abundance_images,file=paste0("wham_data/",campaign,"/biigle_outputs/processed/",campaign,"_",dive,"_",square,"_abundance_images.RData"))

# -------------------------------------------------------------
# FOR ORTHOMOSAICS
# -------------------------------------------------------------
#get relative cover for each annotation
area$annotation_area_sqpx<-as.numeric(area$annotation_area_sqpx)
area$annotation_width_px<-as.numeric(area$annotation_width_px)
total=max(area$annotation_area_sqpx)

area$cover<-NA

for (u in 1:length(unique(area$image_filename))) {
  for (s in 1:nrow(area)) {
    if (area$shape_name[s] == "Polygon" | area$shape_name[s] == "Circle") { #for all polygons
      area$cover[s]<-(area$annotation_area_sqpx[s]*100)/total
    }
    else if (area$label_names[s] == "laternula") {
      area$cover[s]<-(((((area$annotation_width_px[s]*0.37618213)/2)^2*3.14)*2)*100)/total #proxy for area from segment length
    }
    else if (area$label_names[s] == "Parborlasia_corrugatus") {
      area$cover[s]<-((area$annotation_width_px[s]*25)*100)/total #proxy for area from segment length
    }
    else if (area$label_names[s] == "Worms") {
      area$cover[s]<-((area$annotation_width_px[s]*5)*100)/total #proxy for area from segment length
    }
  }
}

#get total abundance/cover per image
abundance<- area %>% count(label_names)
sum<-area %>% group_by(image_filename, label_names) %>% summarize(cover = sum(cover))
abundance<- inner_join(abundance,sum,by="label_names",copy=FALSE)

#save
save(abundance,file=paste0("wham_data/",campaign,"/abundance/",campaign,"_",dive,"_",square,"_abundance.RData"))
