#
#-----------------------------------------------------------------
#           STEP 5.3 in the WHAM-workflow : PREDICTIVE MODELS
#                       by LEA KATZ
#-----------------------------------------------------------------
# This script computes conditional probability from the Bayesian Network
# 

#install.packages("bnlearn",tidyverse","Rgraphviz")
library(bnlearn)
library(tidyverse)
library(Rgraphviz)

# LOAD DATA -------------------------------------------------------
campaign="TANGO2"
dataset_name="NWAP_sic"
load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
final_all->final
final[,1:ncol(final)]<-lapply(final[,1:ncol(final)],function(x) as.factor(x))
final->data

#from banjo
edges<-read.csv(file=paste0("wham_data/",campaign,"/banjo_outputs/",
                             campaign,"_",dataset_name,"/",
                             campaign,"_",dataset_name,"_EdgesModified.csv"))


# Prep data -------------------------------------------------------
#choose frequency cutoff
edges$TotalFreq
edges<-edges[1:14,] #choose cutoff : here 61%
edges<-separate(edges,AB,into=c("from","to"),sep="->",remove=FALSE)

numvec<-c(as.character(0:8))
namevec<-colnames(data)
matchy<-data.frame(numvec,namevec)

edges2<-inner_join(edges,matchy,join_by(from==numvec))
edges3<-inner_join(edges2,matchy,join_by(to==numvec))

# "arcs"
arc.set <- edges3 %>% select(namevec.x,namevec.y)
arc.strength <- edges3 %>% select(namevec.x,namevec.y,StrAB) %>% rename(from=namevec.x,to=namevec.y,strength=StrAB)

#make an empty network
e = empty.graph(namevec)
arcs(e)<-arc.set
#calculate CPTs
cpt<-bn.fit(e,data,method="mle")
#add edge strength
for (i in 1:length(edges3)) {
  arc <- edges3$AB[i]
  attr(e, "arc.strength")[arc] <- edges3$StrAB[i]
}
#add edge frequency
for (i in 1:length(edges3)) {
  arc <- edges3$AB[i]
  attr(e, "edge.frequency")[arc] <- edges3$TotalFreq[i]
}

#plot
graphviz.plot(e)

#conditional probability table
e.fit<-bn.fit(e,data)
#just one
cpquery(e.fit,(stars=="1"),(filterfeeders=="0"))

# -------------------------------------------------------------
# CHOOSE THE CHANGING NODE ----
# -------------------------------------------------------------

#for all nodes, if filterfeeders change
#----------------------------------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = 2, nrow = 1))
colnames(probs.ff)<-levels(data$stars)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 2, nrow = 1))
    colnames(myrow)<-levels(data$stars)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$stars))) {
      state.evidence=levels(data$stars)[a]
      evidence = paste0("filterfeeders==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
 probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta=probs.ff$'0' - probs.ff$'1')



probs.ff<-data.frame(matrix(ncol = 2, nrow = 1))
colnames(probs.ff)<-levels(data$filterfeeders)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 2, nrow = 1))
    colnames(myrow)<-levels(data$filterfeeders)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$filterfeeders))) {
      state.evidence=levels(data$filterfeeders)[a]
      evidence = paste0("filterfeeders==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta=probs.ff$'0' - probs.ff$'1')
#----------------------------------------------------------------------------------------

#for all nodes, if stars change
#----------------------------------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = 2, nrow = 1))
colnames(probs.ff)<-levels(data$stars)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 2, nrow = 1))
    colnames(myrow)<-levels(data$stars)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$stars))) {
      state.evidence=levels(data$stars)[a]
      evidence = paste0("stars==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta=probs.ff$'0' - probs.ff$'1')
#----------------------------------------------------------------------------------------

#for all nodes, if dist_to_glacier changes
#----------------------------------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = 3, nrow = 1))
colnames(probs.ff)<-levels(data$dist_to_glacier)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 3, nrow = 1))
    colnames(myrow)<-levels(data$dist_to_glacier)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$dist_to_glacier))) {
      state.evidence=levels(data$dist_to_glacier)[a]
      evidence = paste0("dist_to_glacier==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta=probs.ff$'0' - probs.ff$'2')
#----------------------------------------------------------------------------------------

#for all nodes, if granulometry changes
#----------------------------------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = 5, nrow = 1))
colnames(probs.ff)<-levels(data$substrate2)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 5, nrow = 1))
    colnames(myrow)<-levels(data$substrate2)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$substrate2))) {
      state.evidence=levels(data$substrate2)[a]
      evidence = paste0("substrate2==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta=probs.ff$'0' - probs.ff$'2')
#----------------------------------------------------------------------------------------

#for all nodes, if macroalgae change
#----------------------------------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = 3, nrow = 1))
colnames(probs.ff)<-levels(data$macroalgae)

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = 3, nrow = 1))
    colnames(myrow)<-levels(data$macroalgae)
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data$macroalgae))) {
      state.evidence=levels(data$macroalgae)[a]
      evidence = paste0("macroalgae==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}
probs.ff<-probs.ff[-1,]
#----------------------------------------------------------------------------------------


# -------------------------------------------------------------
# WRITE THE FILE
# -------------------------------------------------------------

node="filterfeeders" #change according to chosen node
write.csv(probs.ff,file=paste0("wham_data/",campaign,"/",campaign,"_",dataset_name,"_",node,"_probabilities.csv"),dec=".")

# -------------------------------------------------------------
# EXTRA PLOTS
# -------------------------------------------------------------
#prep data
colnames(probs.ff)<-c("invisible","very fine","fine","pebbly/gravely","consolidated")
probs.ff[3:4,2:5]->urchins
probs.ff[5:6,2:5]->stars

state<-c("absence","presence")

urchins<-cbind(state,urchins)
urchins<-pivot_longer(urchins,cols=c(2:5))
urchins$state<-as.factor(urchins$state)

stars<-cbind(state,stars)
stars<-pivot_longer(stars,cols=c(2:5))
stars$state<-as.factor(stars$state)

ggplot(urchins,aes(x=name,y=value,fill=state)) +
  geom_bar(stat="identity") +
  theme_classic() +
  scale_x_discrete(limit=c("very fine","fine","pebbly/gravely","consolidated")) +
  scale_fill_manual(values=c("plum","royalblue")) +
  labs(x="Granulometry",
       y="Probability",
       title="Conditional probability learned from the Bayesian Network")

ggplot(stars,aes(x=name,y=value,fill=state)) +
  geom_bar(stat="identity") +
  theme_classic() +
  scale_x_discrete(limit=c("very fine","fine","pebbly/gravely","consolidated")) +
  scale_fill_manual(values=c("plum","royalblue")) +
  labs(x="Granulometry",
       y="Probability",
       title="Conditional probability learned from the Bayesian Network")

