#
#-----------------------------------------------------------------
#           STEP 4.2 in the WHAM-workflow : MH STATISTICS
#                  for MICROHABITATS PAPER
#                       by LEA KATZ
#
# This script is used to do a cluster analysis on an abundance dataset
# It outputs all the graphs from CH3 of the thesis and related paper.
#
# citation...
#-----------------------------------------------------------------
library(colormap)
library(vegan)
library(tidyverse)
library(labdsv)
library(dendextend)
library(viridis)
library(pals)

# LOAD DATA ---------------------------------------------
#load(file="wham_data/TANGO/datasets/TANGO_ALL_CLEAN_v2.RData")
#mydataCLEANED->mydata
#mydata<-mydata[grep("MI|HI|FH",mydata$station),] 
#metadata<- mydata %>% select(image_filename,transect,site,station)
#mydata<-mydata[-grep("substrate",mydata$group),]
#load("wham_data/TANGO2/metadata/TANGO2_substrate_types.RData")
load("wham_data/TANGO2/metadata/TANGO2_metadata_final.RData")
metadata_short<- metadata %>% select(image_filename,station,site, substrate,macroalgal_cover_cat,macroalgal_cover_cat4)
metadata_short<- metadata_short[!duplicated(metadata_short),]
load(file="wham_data/TANGO2/datasets/TANGO2_microhabitats_final_raw.RData")
load(file="wham_data/TANGO2/datasets/TANGO2_microhabitats_final.RData") #with metadata included

#rownames(mydata_matrified)<-mydata_matrified[,1]
#mydata_matrified<-mydata_matrified[,-1]

# REMOVE UNCLEAR LABELS FOR ANALYSIS ---------------------------
mydata_error<- mydata_matrified %>%
  filter(brown_sheet_algae== 0) %>%
  filter(erect_coarse_algae== 0) %>%
  filter(erect_fine_branching== 0) %>%
  filter(erect_fine_branching_brown== 0) %>%
  filter(erect_fine_branching_red== 0)
mydata_matrified<- mydata_error %>%
  select(-c(brown_sheet_algae,erect_coarse_algae,erect_fine_branching,erect_fine_branching_brown,erect_fine_branching_red))

# ADD MORE METADATA LABELS ---------------------------------------
## substrate ----
soft<-c("sand_mud","fine_sand","coarse_sand")
hard<-c("cobbles","gravel","pebble","consolidated","boulder","rock")    
substrate$final<-NA
for (i in 1:394) {
  if (substrate$invisible[i]>70) {final<-"invisible"} else {
    if (is.na(substrate$secondary_sediment[i])) {
      substrate$dominant_sediment[i]->sub_type
      if (sub_type %in% soft) {final<-"soft"}
      if (sub_type %in% hard) {final<-"hard"}
    } else {sub_type<-"mix"}
    if (sub_type=="mix") {
      substrate$dominant_sediment[i]->mix1
      substrate$secondary_sediment[i]->mix2
      if ((mix1 %in% soft & mix2 %in% hard) | (mix1 %in% hard & mix2 %in% soft)) {final <- "mix"}
    } 
  }
  substrate$final[i]<-final
}
substrate <- substrate %>% 
  rownames_to_column(var="image_filename") %>%
  select(image_filename,final)
metadata <- metadata %>% 
  left_join(substrate,by="image_filename") %>%
  rename(substrate=final)

## macroalgae cover ----
macroalgae<-mydata[grep("macroalgae",mydata$group),]
macroalgae_list<-unique(macroalgae$morphotype)
mods<- mydata %>% select(image_filename,morphotype,cover,transect,site,station)
mod<- matrify(as.data.frame(mods[,1:3]))
mod<-mod%>% mutate(macroalgal_cover = rowSums(across(all_of(intersect(macroalgae_list, colnames(mod))))))
mod <- mod %>%
  rownames_to_column(var="image_filename") %>%
  select(image_filename,macroalgal_cover)

source("wham_code/functions/get.hilo.R")
mod$macroalgal_cover_cat<-as.factor(get.hilo(mod$macroalgal_cover))
metadata <- metadata %>% 
  left_join(mod,by="image_filename")
#OR
source("wham_code/functions/get.quantiles.R")
mod$macroalgal_cover_cat4<-as.factor(get.quantiles(mod$macroalgal_cover,4))
mod <- mod %>% select(image_filename,macroalgal_cover_cat4)
metadata <- metadata %>% 
  left_join(mod,by="image_filename")

# TRANSFORMATION? -----------------------------------------------
mydata[,1:81]->mydata_m
#mydata_m0<-mydata_matrified
#mydata_m1<-sqrt(mydata_m0)
#mydata_m2<-sqrt(mydata_m1)
#mydata_m3<-log(1+mydata_m0)

mydata_m<-mydata_m0
#mydata_m<-mydata_m1
#mydata_m<-mydata_m2
#mydata_m<-mydata_m3

# DISTANCE MATRIX -----------------------------------------------
distm<-vegdist(mydata_m,method="bray") 
#distmdf<-as.data.frame(as.matrix(distm))
#simm<-as.dist(1-distm)

# CLUSTERING ----------------------------------------------------
cluster.average<-hclust(distm,"average")
#cluster.average<-hclust(simm,"average")
#cluster.ward<-hclust(distm,"ward.D2")

den <- as.dendrogram(cluster.average)
#den <- as.dendrogram(cluster.ward)

# Add color labels to dendrogram (functions) ----
## per station ----
colLabSTATION<<-function(n){
  if(is.leaf(n)){
    a=attributes(n)
    ligne=match(attributes(n)$label,metadata[,1])
    station=metadata[ligne,4];
    site=metadata[ligne,3];
    if(station=="HI"){col_station="maroon2"};if(station=="MI"){col_station="orange"};if(station=="FH"){col_station="cornflowerblue"}
    #Modification of leaf attribute
    attr(n,"nodePar")<-c(a$nodePar,list(cex=1,lab.cex=1,pch=20,col=col_station,lab.col=col_station,lab.font=1,lab.cex=1))
  }
  return(n)
}

## per site ----
colLabSITE<<-function(n){
  if(is.leaf(n)){
    a=attributes(n)
    ligne=match(attributes(n)$label,metadata[,1])
    site=metadata[ligne,3];
    if(site=="MI_ON_east"){col_station="yellow"};
    if(site=="MI_OS_inner"){col_station="gold"};
    if(site=="MI_OS_mid"){col_station="orange"};
    if(site=="MI_OS_outer"){col_station="darkorange"};
    if(site=="HI_outside"){col_station="lightpink"};
    if(site=="HI_south"){col_station="hotpink"};
    if(site=="HI_channel"){col_station="maroon2"};
    if(site=="FH_south"){col_station="lightblue"};
    if(site=="FH_boat"){col_station="steelblue"};
    if(site=="FH_wreck"){col_station="dodgerblue"};
    if(site=="FH_wall"){col_station="cornflowerblue"}
    #Modification of leaf attribute
    attr(n,"nodePar")<-c(a$nodePar,list(cex=2,lab.cex=1,pch=20,col=col_station,lab.col=col_station,lab.font=1,lab.cex=1))
  }
  return(n)
}

## per substrate type ----
colLabSUB<<-function(n){
  if(is.leaf(n)){
    a=attributes(n)
    ligne=match(attributes(n)$label,metadata[,1])
    substrate=metadata[ligne,5];
    if(substrate=="soft"){col_sub="steelblue3"};if(substrate=="hard"){col_sub="gold2"};if(substrate=="mix"){col_sub="firebrick2"};if(substrate=="invisible"){col_sub="black"};
    #Modification of leaf attribute
    attr(n,"nodePar")<-c(a$nodePar,list(cex=1,lab.cex=1,pch=20,col=col_sub,lab.col=col_sub,lab.font=1,lab.cex=1))
  }
  return(n)
}

## per macroalgae cover category ----
colLabALG<<-function(n){
  if(is.leaf(n)){
    a=attributes(n)
    ligne=match(attributes(n)$label,metadata[,1])
    alg=metadata[ligne,7];
    if(alg=="0"){col_alg="sienna3"};if(alg=="1"){col_alg="khaki"};if(alg=="2"){col_alg="darkseagreen3"};if(alg=="3"){col_alg="seagreen"}
    #Modification of leaf attribute
    attr(n,"nodePar")<-c(a$nodePar,list(cex=1,lab.cex=1,pch=20,col=col_alg,lab.col=col_alg,lab.font=1,lab.cex=1))
  }
  return(n)
}

## per macroalgae cover category 2 ----
colLabALG4<<-function(n){
  if(is.leaf(n)){
    a=attributes(n)
    ligne=match(attributes(n)$label,metadata[,1])
    alg=metadata[ligne,8];
    if(alg=="0"){col_alg="sienna3"};if(alg=="1"){col_alg="khaki"};if(alg=="2"){col_alg="darkseagreen3"};if(alg=="3"){col_alg="seagreen"}
    #Modification of leaf attribute
    attr(n,"nodePar")<-c(a$nodePar,list(cex=1,lab.cex=1,pch=20,col=col_alg,lab.col=col_alg,lab.font=1,lab.cex=1))
  }
  return(n)
}

# MAKE DENDROGRAM -------------------------------------------------
## apply color labels if needed (CHOOSE 1) ---- 
den2<-dendrapply(den,colLabSTATION)
den2<-dendrapply(den_simp,colLabSTATION)
den2<-dendrapply(den,colLabSITE)
den2<-dendrapply(den,colLabSUB)
den2<-dendrapply(den,colLabALG4)
den2<-den

## plot ----
den2 %>%
  #set("branches_k_color", value = leafcolor, k = 10) %>%
  set("labels",rep("")) %>%
  plot()

# SIMPROF ANALYSIS ------------------------------------------------
#simprof result from PRIMER
simprof_res<-read.csv(file="wham_data/TANGO2/datasets/simprof-result.csv")
simprof_groups<-simprof_res$Grouping
names(simprof_groups)<-simprof_res$Image

mydata <- mydata_m %>% rownames_to_column(var = "Image")
mydata<-left_join(mydata, simprof_res)
mydata<-left_join(mydata,metadata_short,join_by(Image==image_filename))

group_colors <- c("a"="grey43",
                  "b"="grey77",
                  "c"="grey43",
                  "d"="grey77",
                  "e"="deeppink",
                  "f"="grey77",
                  "g"="grey43",
                  "h"="firebrick2",
                  "i"="darkorange1",
                  "j"="goldenrod2",
                  "k"="grey77",
                  "l"="grey43",
                  "m"="grey77",
                  "n"="gold1",
                  "o"="grey77",
                  "p"="grey43",
                  "q"="grey77",
                  "r"="grey43",
                  "s"="yellowgreen",
                  "t"="seagreen2",
                  "u"="grey77",
                  "v"="grey43",
                  "w"="grey77",
                  "x"="lightskyblue",
                  "y"="grey43",
                  "z"="grey77",
                  "aa"="grey43",
                  "ab"="steelblue",
                  "ac"="royalblue3",
                  "ad"="mediumblue",
                  "ae"="blue4"
                  )
#group_colors <- c("magenta4","deeppink","lightpink","darkorange1","gold1","yellowgreen","springgreen4","steelblue3","blue3","blue4","purple4","black")
#group_colors <- c("deeppink","firebrick2","darkorange1","goldenrod2","gold1","yellowgreen","seagreen2","lightskyblue","steelblue1","royalblue3","mediumblue","blue4")

dendlabs<-labels(den)
ordered_groups <- simprof_res$Grouping[match(dendlabs, simprof_res$Image)]
#names(ordered_groups) <- dendlabs
#ordered_groups <- as.integer(factor(ordered_groups))

station <- mydata$station[match(dendlabs,mydata$Image)]
barcolor <- c("FH" = "cornflowerblue","HI"="maroon2","MI"="orange")
barcolor <- barcolor[as.numeric(as.factor(station))]

macroalgal_cat <- mydata$macroalgal_cover_cat4[match(dendlabs,mydata$Image)]
barcolor2 <- c("0" = "red","1"="beige","2"="lightgreen","3"="darkgreen")
barcolor2 <- barcolor2[as.numeric(as.factor(macroalgal_cat))]

substrate <- mydata$substrate[match(dendlabs,mydata$Image)]
barcolor3 <- c("soft" = "orange","hard"="black","invisible"="white","mix"="grey")
barcolor3 <- barcolor3[as.numeric(as.factor(substrate))]

den_colored <- den %>%
  color_branches(clusters = ordered_groups, col = group_colors) %>%
  set("labels", rep(""))
plot(den_colored, axes = FALSE)

colored_bars(colors = barcolor, dend = den_colored, rowLabels = "",sort_by_labels_order = FALSE)
colored_bars(colors = barcolor2, dend = den_colored, rowLabels = "",sort_by_labels_order = FALSE)
colored_bars(colors = barcolor3, dend = den_colored, rowLabels = "",sort_by_labels_order = FALSE)


# TEST SIGNIFICANCE OF CLUSTERS ------------------------------------
## PERMANOVA ----
metadata_filtered <- metadata_short[metadata_short$image_filename %in% labels(distm), ]
adonis_res<-adonis2(distm ~ station + macroalgal_cover_cat4 + substrate, 
        data = metadata_filtered,
        method = "bray", 
        permutations = 999)

adonis_res2<-adonis2(distm ~ station * macroalgal_cover_cat4 * substrate, 
                    data = metadata_filtered,
                    method = "bray", 
                    permutations = 999)

results <- lapply(2:15, function(k) {
  groups <- cutree(den2, k = k)
  aov <- adonis2(mydata_m ~ as.factor(groups), method = "bray")
  data.frame(k = k, R2 = aov$R2[1], F = aov$F[1], p = aov$`Pr(>F)`[1])
})

results_df <- do.call(rbind, results)

plot(results_df$k, results_df$R2, type = "b", ylab = "R²", xlab = "Number of clusters (k)")

## ANOSIM ----
all(rownames(distm) == rownames(metadata2)) #just to check
anosim_stations<-anosim(distm,grouping=as.factor(metadata2$station)) 
anosim_substrate<-anosim(distm,grouping=as.factor(metadata2$substrate)) 
anosim_algae<-anosim(distm,grouping=as.factor(metadata2$macroalgal_cover_cat4)) 

# Example result table (after running anosim for each variable)
anosim_results <- data.frame(
  Variable = c("station", "substrate", "macroalgae"),
  R = c(0.344, 0.126, 0.233),
  p = c(0.001, 0.001, 0.001)
)

ggplot(anosim_results, aes(x = reorder(Variable, -R), y = R)) +
  geom_col() +
  geom_text(aes(label = paste0("p=", round(p, 3))), vjust = -0.5) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey")) +
  theme_minimal() +
  labs(title = "ANOSIM", y = "R statistic", x = "Variable")

# BETADISPER ----
disp <- betadisper(distm,group=as.factor(newg5))
plot(disp)

# SIMPER ----
top_groups<-c("x", "t", "h", "ae", "ab", "ad", "s", "n", "e", "j", "i", "ac")
simprof_groups_top<-simprof_groups[simprof_groups %in% top_groups]
mydata_m<-mydata_m[rownames(mydata_m) %in% names(simprof_groups_top),]

sim <- simper(mydata_m[rownames(mydata_m) %in% names(simprof_groups_top), ], simprof_groups_top)
sim_data <- data.frame(species = sim[[1]]$species, 
                       "group_ab_ae" = sim[[1]]$average)
for(i in 2:length(sim)) {
  group_pair <- names(sim)[i]
  new_data <- data.frame(species = sim[[i]]$species, 
                         group_pair = sim[[i]]$average)
  sim_data <- cbind(sim_data, new_data[,2])
  colnames(sim_data)[ncol(sim_data)] <- paste0("group_",group_pair)
}

# simper plot
sim_data_melted <- reshape2::melt(sim_data, id.vars = "species", variable.name = "group_pair", value.name = "average_contribution")
ggplot(sim_data_melted, aes(x = group_pair, y = species, fill = average_contribution)) +
  geom_tile() +
  #scale_fill_viridis_c() +  # Color scale based on contribution value
  theme_minimal() +
  labs(title = "SIMPER Analysis: Species Contributions by Group Pair",
       x = "Group Pair", y = "Species", fill = "Avg Contribution") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "blue") +  # Adjust colors for contribution intensity
  theme(axis.text.y = element_text(size = 6))  # Adjust y-axis text size if species names are long

#top taxa?
group_factor <- as.factor(simprof_groups_top)

top_taxa <- unique(unlist(lapply(sim, function(comp) {
  comp <- as.data.frame(comp)  # Ensure it's treated as a data frame
  top_rows <- order(comp$average, decreasing = TRUE)[1:min(10, nrow(comp))]
  rownames(comp)[top_rows]
})))

mydata_top <- mydata_m[, top_taxa]

group_means_top <- as.data.frame(t(aggregate(mydata_top, by = list(Group = group_factor), FUN = mean)))
colnames(group_means_top) <- group_means_top[1, ]
group_means_top <- group_means_top[-1, ]
rownames(group_means_top) <- colnames(mydata_top)

# Convert all values to numeric properly
group_means_top <- apply(group_means_top, 2, function(x) as.numeric(as.character(x)))
group_means_top <- matrix(group_means_top, 
                          nrow = length(colnames(mydata_top)), 
                          dimnames = list(colnames(mydata_top), colnames(group_means_top)))

desired_order <- c("e","h","i","j","n","s","t","x","ab","ac","ad","ae")

# Reorder columns of matrix
group_means_top <- group_means_top[, desired_order]

pheatmap::pheatmap(group_means_top, 
                   scale = "row",
                   cluster_cols = FALSE,
                   color = colorRampPalette(c("darkblue", "white", "maroon2"))(25),
                   breaks = seq(-3, 3, length.out = 26),
                   main = "")

# INFORMATIVE BARPLOTS ------------------------------------
## how much images of each site/station per group ----
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

bar_data <- mydata %>%
  filter(!is.na(Grouping)) %>%
  group_by(Grouping) %>%
  mutate(total_count = n()) %>%
  select(Grouping,site,total_count,station)

bar_data<- bar_data %>%
  group_by(Grouping, site) %>%
  mutate(count = n()) %>%
  select(Grouping,total_count,site,count,station) %>%
  distinct(Grouping,site,.keep_all = TRUE)

bar_data %>%
  ggplot(aes(x = reorder(Grouping, desc(total_count)), y = count, fill = station)) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = site_colors) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  theme_minimal() +
  labs(y = "number of images", x = "group") +
  theme_bw()

## How much microhabitats in each site? ----
bar_data %>%
  ggplot(aes(x = site, y = count, fill = Grouping)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=group_colors) +
  theme_minimal() +
  labs(title = "Number of Images per Site in Each Group",
       y = "Count", x = "Site")

## How much taxa per image for each group? ----
mydata$ntaxa <- rowSums(mydata[,1:81] > 0)
mydata %>%
  group_by(Grouping) %>%
  summarise(mean_taxa_per_image = mean(ntaxa)) %>%
  ggplot(aes(x=Grouping, y=mean_taxa_per_image)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=mean_taxa_per_image), size=3) +
  theme_minimal() +
  coord_flip() +
  labs(title = "mean taxa per image in Group",
       y = "mean", x = "group")

## how much groups per site? ----
desired_order <- c("e","h","i","j","n","s","t","x","ab","ac","ad","ae")
desired_order2 <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","aa","ab","ac","ad","ae")
mydata2 <- filter(mydata, Grouping %in% desired_order)

#by site
image_count <- mydata %>%
  group_by(site, Grouping) %>%
  summarise(n_images = n()) %>%
  ungroup() %>%
  mutate(Grouping=factor(Grouping,levels=desired_order2))

#color=colorRampPalette(c("#fb00a8","#6fbbff","#0d45ff"))(12)
group_colors <- c("e"="deeppink",
                  "h"="firebrick2",
                  "i"="darkorange1",
                  "j"="goldenrod2",
                  "n"="gold1",
                  "s"="yellowgreen",
                  "t"="seagreen2",
                  "x"="lightskyblue",
                  "ab"="steelblue",
                  "ac"="royalblue3",
                  "ad"="mediumblue",
                  "ae"="blue4"
                  )

group_colors <- c("a"="grey43",
                  "b"="grey77",
                  "c"="grey43",
                  "d"="grey77",
                  "e"="deeppink",
                  "f"="grey77",
                  "g"="grey43",
                  "h"="firebrick2",
                  "i"="darkorange1",
                  "j"="goldenrod2",
                  "k"="grey77",
                  "l"="grey43",
                  "m"="grey77",
                  "n"="gold1",
                  "o"="grey77",
                  "p"="grey43",
                  "q"="grey77",
                  "r"="grey43",
                  "s"="yellowgreen",
                  "t"="seagreen2",
                  "u"="grey77",
                  "v"="grey43",
                  "w"="grey77",
                  "x"="lightskyblue",
                  "y"="grey43",
                  "z"="grey77",
                  "aa"="grey43",
                  "ab"="steelblue",
                  "ac"="royalblue3",
                  "ad"="mediumblue",
                  "ae"="blue4"
)

image_count$Grouping <- factor(image_count$Grouping, levels = desired_order2)
image_count$site <- factor(image_count$site, levels = c("HI-n","HI-c","HI-s","MIN","MIS-i","MIS-m","MIS-o","FH-s","FH-b","FH-w","FH-e"))
named_colors <- setNames(group_colors, desired_order2)

# Step 1: Compute proportion and label_y using separate grouped pipeline
plot_data <- image_count %>%
  group_by(site) %>%
  mutate(
    total = sum(n_images),
    prop = n_images / total
  ) %>%
  arrange(site, desc(Grouping)) %>%  # Only affects label placement
  group_by(site) %>%
  mutate(
    cum_prop = cumsum(prop),
    label_y = cum_prop - prop / 2
  ) %>%
  ungroup()

# Step 2: Plot with correctly placed labels
ggplot(plot_data, aes(x = site, y = prop, fill = Grouping)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = Grouping, y = label_y), size = 3, color = "black") +
  scale_fill_manual(values = named_colors, breaks = desired_order2) +
  labs(x = "Site", y = "Proportion of images") +
  theme_bw()

#by transect
image_count <- mydata2 %>%
  group_by(transect, Grouping) %>%
  summarise(n_images = n()) %>%
  ungroup() %>%
  mutate(Grouping=factor(Grouping,levels=desired_order))

color=colorRampPalette(c("#fb00a8","#6fbbff","#0d45ff"))(12)
named_colors<-setNames(color,desired_order)

ggplot(image_count, aes(x=transect, y=n_images, fill=Grouping)) +
  geom_bar(stat="identity",position= "fill") +
  #scale_fill_manual(values=unname(glasbey())) +
  scale_fill_manual(
    values = named_colors,
    breaks = desired_order) +
  labs(x="transect",
       y="Number of images") +
  theme_bw()
  
# Step 0: Define which groupings are 'grey' (to be combined as "Other")
grey_groups <- names(group_colors)[group_colors %in% c("grey43", "grey77")]

# Step 1: Recode 'Grouping' to combine grey groups into "Other"
plot_data_combined <- image_count %>%
  mutate(Grouping_recode = ifelse(Grouping %in% grey_groups, "Other", as.character(Grouping))) %>%
  group_by(site, Grouping_recode) %>%
  summarise(n_images = sum(n_images), .groups = "drop") %>%
  group_by(site) %>%
  mutate(
    total = sum(n_images),
    prop = n_images / total
  ) %>%
  arrange(site, Grouping_recode != "Other", Grouping_recode) %>%  # ensures 'Other' is on top
  mutate(
    cum_prop = cumsum(prop),
    label_y = cum_prop - prop / 2
  ) %>%
  ungroup()

# Step 2: Define new color palette (only one grey for "Other")
new_colors <- c(
  "Other" = "grey70",  # or any consistent grey
  "e" = "deeppink",
  "h" = "firebrick2",
  "i" = "darkorange1",
  "j" = "goldenrod2",
  "n" = "gold1",
  "s" = "yellowgreen",
  "t" = "seagreen2",
  "x" = "lightskyblue",
  "ab" = "steelblue",
  "ac" = "royalblue3",
  "ad" = "mediumblue",
  "ae" = "blue4"
)

# Optional: reorder Grouping for consistent stacking (non-Other first)
plot_data_combined$Grouping_recode <- factor(
  plot_data_combined$Grouping_recode,
  levels = c(setdiff(names(new_colors), "Other"), "Other")  # stack non-grey first
)

# Step 3: Plot
ggplot(plot_data_combined, aes(x = site, y = prop, fill = Grouping_recode)) +
  geom_bar(stat = "identity", position = "fill") +
  #geom_text(aes(label = Grouping_recode, y = label_y), size = 3, color = "black") +
  scale_fill_manual(values = new_colors) +
  labs(x = "Site", y = "Proportion of images", fill = "Group") +
  theme_bw()

