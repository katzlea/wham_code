# This is a extra script for when there are no seperate images, but I needed
# to subsample aligned orthomosaics in order to get non-overlapping images.
#
# by LEA KATZ for project WHAM

library(readxl)
library(tidyverse)
source("wham_code/functions/headertrue.R")

# -------------------------------------------------------------
# DEFINE VARIABLES
# -------------------------------------------------------------
campaign="TANGO1"
dive="ROV13"
square="q2"
outline= 1116709.882
#total area of each subsample in sqpx, check in cameraCalibration file

#from QGIS
pols<-read.csv(paste0("wham_data/",campaign,"/qgis/subsampling/",campaign,"_",dive,"_",square,"_polygons.csv"))
lines<-read.csv(paste0("wham_data/",campaign,"/qgis/subsampling/",campaign,"_",dive,"_",square,"_linestrings.csv"))
pts<-read.csv(paste0("wham_data/",campaign,"/qgis/subsampling/",campaign,"_",dive,"_",square,"_points.csv"))
#combine
subids<-rbind(pols,lines,pts)

#from BIIGLE
biigle<-read.csv(paste0("wham_data/",campaign,"/biigle/Image_annotation_csv/",campaign,"_",dive,"_",square,".csv"))
area<-read_xlsx(paste0("wham_data/",campaign,"/biigle/Image_annotation_area/",campaign,"_",dive,"_",square,"_image_annotation_area_report.xlsx"))
area<-header.true(area)
area$annotation_id<-as.integer(area$annotation_id)

biigle<-select(biigle,c(annotation_label_id,annotation_id))
area<-inner_join(area,biigle,by="annotation_id")
subids<-inner_join(subids,area[,c("annotation_label_id","annotation_area_sqpx","shape_name","annotation_width_px")],by=c("X_id"="annotation_label_id"))

subids<- subids %>% 
  rename(label_names=X_label_name) %>% 
  mutate(image_filename=str_c(X_image_filename,id))

subids$cover<-NA
subids$annotation_area_sqpx<-as.numeric(subids$annotation_area_sqpx)
subids$annotation_width_px<-as.numeric(subids$annotation_width_px)

for (x in 1:length(unique(subids$X_id))) {
  unique(subids$X_id)[x]->annot
  annot_subids<-subset(subids,X_id==annot)
  count<-nrow(annot_subids)
  for (y in 1:nrow(subids)) {
    if (subids$X_id[y]==annot){subids$annotation_area_sqpx[y]<-subids$annotation_area_sqpx[y]/count}
    if (subids$X_id[y]==annot){subids$annotation_width_px[y]<-subids$annotation_width_px[y]/count}
  }
}

abundance_images<- NA

for (i in 1:length(unique(subids$image_filename))) {
  unique(subids$image_filename)[i]->image
  image_subids<-subset(subids,image_filename==image)
  #cover
  for (u in 1:nrow(image_subids)) {
    if (image_subids$label_names[u]=="outline") {image_subids$annotation_area_sqpx[u]<-outline}
    if (image_subids$shape_name[u] == "Polygon" | image_subids$shape_name[u] == "Circle") { #for all polygons
      image_subids$cover[u]<-(image_subids$annotation_area_sqpx[u]*100)/outline
    }
    else if (image_subids$label_names[u] == "laternula") {
      image_subids$cover[u]<-(((((image_subids$annotation_width_px[u]*0.37618213)/2)^2*3.14)*2)*100)/outline #proxy for area from segment length
    }
    else if (image_subids$label_names[u] == "parborlasia_corrugatus") {
      image_subids$cover[u]<-((image_subids$annotation_width_px[u]*25)*100)/outline #proxy for area from segment length
    }
    else if (image_subids$label_names[u] == "Worms") {
      image_subids$cover[u]<-((image_subids$annotation_width_px[u]*5)*100)/outline #proxy for area from segment length
    }
  }
  image_abundance<- image_subids %>% count(label_names)
  image_sum<-image_subids %>% group_by(image_filename, label_names) %>% summarize(cover = sum(cover))
  image_abundance<- inner_join(image_abundance,image_sum,by="label_names",copy=FALSE)
  abundance_images<-rbind(abundance_images,image_abundance)
}
abundance_images<-abundance_images[-1,]

#abundance_subsamples<- NA
#for (i in 1:length(unique(subids$image_filename))) {
 # unique(subids$image_filename)[i]->subsample_id
  #subsample<-subset(subids,image_filename==subsample_id)
  #subsample<- subsample %>% distinct(X_id, .keep_all = TRUE)
  #abundance
  #subsample_abundance<- subsample %>% count(label_names,image_filename)
  #abundance_subsamples<-rbind(abundance_subsamples,subsample_abundance)
#}
#abundance_subsamples<-abundance_subsamples[-1,]

#sum for all subsamples
abundance<- abundance_images %>% group_by(label_names) %>% summarize(n=sum(n))
abundance$image_filename<-paste0(campaign,"_",dive,"_",square)

#save
save(abundance,file=paste0("wham_data/",campaign,"/abundance/",campaign,"_",dive,"_",square,"_abundance_total.RData"))
save(abundance_images,file=paste0("wham_data/",campaign,"/abundance_images/",campaign,"_",dive,"_",square,"_abundance_subsamples.RData"))

