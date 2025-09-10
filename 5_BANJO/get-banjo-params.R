#
#-----------------------------------------------------------------
#           STEP 5.3 in the WHAM-workflow : PREDICTIVE MODELS
#                       by LEA KATZ
#-----------------------------------------------------------------
# This script computes conditional probability from the Bayesian Network
# 
#
#
#


#install.packages("bnlearn",tidyverse","Rgraphviz")
library(bnlearn)
library(tidyverse)
library(Rgraphviz)

#load data
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


library(igraph)
#MAX/MEAN CHAIN LENGTH
adj_mat<-amat(e) #convert to adjacency matrix
g <- graph_from_adjacency_matrix(adj_mat, mode = "directed")
max_chain_length <- max(distances(g, mode = "out"), na.rm = TRUE)
all_paths <- distances(g, mode = "out")
mean_chain_length <- mean(all_paths[all_paths > 0], na.rm = TRUE)
cat("Maximum Chain Length:", max_chain_length, "\n")
cat("Mean Chain Length:", mean_chain_length, "\n")
