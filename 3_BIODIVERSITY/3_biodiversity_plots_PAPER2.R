#
#-----------------------------------------------------------------
#           STEP 3, adapted for MICROHABITATS PAPER
#                       by LEA KATZ
# citation...
#-----------------------------------------------------------------

#LOAD DATA -------------------------------------------------------
load(file="wham_data/TANGO2/datasets/TANGO2_microhabitats_final.RData")

#FORMAT ----------------------------------------------------------
mydata<-pivot_longer(mydata,cols=colnames(mydata)[1:81])
m<- mydata %>% select(site,morphotype,n)
mydata_m->mydata

#SUMMARIZE -------------------------------------------------------
transect_totals <- mydata %>% group_by(transect) %>% summarise(unique_images = n_distinct(image_filename))
site_totals <- mydata %>% group_by(site) %>% summarise(unique_images = n_distinct(image_filename))
station_totals <- mydata %>% group_by(station) %>% summarise(unique_images = n_distinct(image_filename))

## by transect ----
mydataTRANSECTS <- mydata %>%
  group_by(transect,name) %>%
  summarise(
    total_n = sum(value, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  )

## by site ----
mydataSITES <- mydata %>%
  group_by(site,name) %>%
  summarise(
    total_n = sum(value, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  )

## by station ----
mydataSTATIONS <- mydata %>%
  group_by(station,name) %>%
  summarise(
    total_n = sum(value, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  )

mydata<-mydataSTATIONS
mydata<-mydataSITES
mydata<-mydataTRANSECTS

## check format ----
mydata
#m<- mydata %>% select(Grouping,name,value)
#m<- mydata %>% select(transect,morphotype,n)
m<-mydata %>% select(site, name,total_n)
m<-mydata %>% select(station, name, total_n)
mydata_m<- matrify(as.data.frame(m))

## by microhabitats ----
mydata_m<-mydata[,c(1:81,89)]
mydataMICRO <- mydata %>%
  group_by(Grouping) %>%
  summarise(
    total= sum(n, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  )
#or
mydataMICRO <- mydata_m %>%
  group_by(Grouping) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# BIODIVERSITY INDEX ----
SN<- specnumber(mydata_m) #Species Richness
shan<- diversity(mydata_m) #Shannon-Wiener
pielou<- shan/log(SN) #Pielou's evenness
mydata_m$SN<- SN
mydata_m$shan<- shan
mydata_m$pielou<- pielou
mydata_m$station<-rownames(mydata_m)
mydata_m$ngroups<-group_count_by_site$n_groupings

mydata_i<- mydata_m %>% select(SN,shan,pielou,station,ngroups)
mydata_i$ngroups<- group_count_by_site$n_groupings

df_info_unique <- mydataSITES %>%
  distinct(site, .keep_all = TRUE)
df_combined <- merge(mydata_i, df_info_unique[, c("site", "station")], by = "site")

df_info_unique <- mydataSTATIONS %>%
  distinct(station, .keep_all = TRUE)
df_combined <- merge(mydata_i, df_info_unique[, c("station")], by = "station")


# PLOTS ----
load(file="wham_data/TANGO2/datasets/biodiv_plots_stations.RData")

## barplots  ----
ggplot(mydata_i, aes(x=station, y=shan, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
  labs(y="Shannon-Wiener diversity") +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) 

ggplot(mydata_i, aes(x=station, y=pielou, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
  labs(y="Pielou's evenness") +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) 

ggplot(mydata_i, aes(x=station, y=SN, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
  labs(y="Number of morphotaxa") +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) 

ggplot(mydata_i, aes(x=station, y=ngroups, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
  labs(y="Number of microhabitats") +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) 

## lollipops ----
ggplot(mydata_i, aes(x=site, y=SN,fill=site)) +
  geom_segment( aes(x=site, xend=site, y=0, yend=SN), color="grey") +
  geom_point(shape=21, color="black", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  xlab("") +
  ylab("n of morphotaxa")

ggplot(mydata_i, aes(x=site, y=shan,fill=site)) +
  geom_segment( aes(x=site, xend=site, y=0, yend=shan), color="grey") +
  geom_point(shape=21, color="black", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  xlab("") +
  ylab("shannon-wiener diversity")

ggplot(mydata_i, aes(x=site, y=pielou,fill=site)) +
  geom_segment( aes(x=site, xend=site, y=0, yend=pielou), color="grey") +
  geom_point(shape=21, color="black", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  xlab("") +
  ylab("pielou's evenness")

ggplot(mydata_i, aes(x=station, y=ngroups ,fill=station)) +
  geom_segment( aes(x=station, xend=station, y=0, yend=ngroups), color="grey") +
  geom_point(shape=21, color="black", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values=c("cornflowerblue","maroon2","orange")) +
  xlab("") +
  ylab("n of microhabitats")


ggsave("alpha.png",height=3,width=5) 

ggplot(mydata_i, aes(x=site, y=SN, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Species Richness") +
  scale_fill_discrete(name="Station") 

ggsave("richness.png",height=3,width=5)

ggplot(mydata_i, aes(x=site, y=pielou, fill=station)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Evenness") +
  scale_fill_discrete(name="Station") 

ggsave("evenness.png",height=3,width=5)
