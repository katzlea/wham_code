#
#-----------------------------------------------------------------------
#           STEP 5.4 in the WHAM-workflow : EXTRACT BANJO PARAMETERS
#                       by LEA KATZ
#-----------------------------------------------------------------------
# This script converts Banjo networks to bnlearn-compatible format
# and extracts parameters from the networks.
#
# Input needed : 
# - bni-prepped data (input for Banjo)
# - the .csv file created by step "5.2-banjo-outputs", but Modified (delete duplicate edges)
# - OPTIONAL : if you have a spreadsheet with network info

#install.packages("bnlearn",tidyverse","Rgraphviz")
library(bnlearn)
library(tidyverse)
library(Rgraphviz)
library(igraph)
library(readxl)

#load data
params<-read_xlsx("wham_data/TANGO2/banjo_outputs/TANGO2_network_parameters.xlsx")

#specify variables
campaign="TANGO2"
dataset_name="MI_general_OK"

# "for-banjo" file
load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
final_all->final
final[,1:ncol(final)]<-lapply(final[,1:ncol(final)],function(x) as.factor(x))
final->data
save(data,file=paste0("wham_data/",campaign,"/bnlearn-networks/",campaign,"_",dataset_name,"_data.RData"))

# "banjo-outputs" file, to get the edges
edges<-read.csv(file=paste0("wham_data/",campaign,"/banjo_outputs/",
                            campaign,"_",dataset_name,"/",
                            campaign,"_",dataset_name,"_EdgesModified.csv"))

# keep only valid egdes 
#from spreadsheet
cutoff<-params$cutOffFreq[which(params$name==paste0(dataset_name))]
idx<-which(edges$TotalFreq==cutoff) #to account for multiple values of cutoff value
if (length(idx)>0) {
  edges <- edges[1:max(idx), ]
}
edges<-separate(edges,AB,into=c("from","to"),sep="->",remove=FALSE)
#or manually
#edges$TotalFreq
#edges<-edges[1:14,] #choose cutoff : here 61%
#edges<-separate(edges,AB,into=c("from","to"),sep="->",remove=FALSE)

# combine all
numvec<-c(as.character(0:(params$nNodes[which(params$name==paste0(dataset_name))]-1)))
namevec<-colnames(data)
matchy<-data.frame(numvec,namevec)
edges2<-inner_join(edges,matchy,join_by(from==numvec))
edges3<-inner_join(edges2,matchy,join_by(to==numvec))
arc.set <- edges3 %>% select(namevec.x,namevec.y)
arc.strength <- edges3 %>% select(namevec.x,namevec.y,StrAB) %>% rename(from=namevec.x,to=namevec.y,strength=StrAB)

# make an empty network
e = empty.graph(namevec)
arcs(e)<-arc.set
# calculate CPTs
cpt<-bn.fit(e,data,method="mle")
# add edge strength
for (i in 1:length(edges3)) {
  arc <- edges3$AB[i]
  attr(e, "arc.strength")[arc] <- edges3$StrAB[i]
}
# add edge frequency
for (i in 1:length(edges3)) {
  arc <- edges3$AB[i]
  attr(e, "edge.frequency")[arc] <- edges3$TotalFreq[i]
}

# plot
graphviz.plot(e)

print(e)

# save
save(e,file=paste0("wham_data/",campaign,"/bnlearn-networks/",campaign,"_",dataset_name,"_network.RData"))

# -------------------------------------------------------------
# PARAMETERS
# -------------------------------------------------------------
#campaign="TANGO2"
#dataset_name="HI_general_OK"
#load(file=paste0("wham_data/",campaign,"/bnlearn-networks/",campaign,"_",dataset_name,"_network.RData"))
source("wham_code/functions/find_all_paths.R")

# get all network chains
adj_matrix <- amat(e) # Convert bn to an adjacency matrix
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE) # Convert to an igraph object

root_nodes <- V(g)[degree(g, mode = "in") == 0]$name
leaf_nodes <- V(g)[degree(g, mode = "out") == 0]$name

chains_list <- list()
chain_lengths <- c()
for (root in root_nodes) {
  for (leaf in leaf_nodes) {
    paths <- find_all_paths(g, root, leaf)
    if (length(paths) > 0) {
      for (path in paths) {
        chains_list <- c(chains_list, paste(path, collapse = " → "))  # Store chain as string
        chain_lengths <- c(chain_lengths, length(path) - 1)  # Chain length = number of edges
      }
    }
  }
}
chains_table <- data.frame(Chain = unlist(chains_list),Chain_Length = chain_lengths)
print(chains_table)

# get max/mean chain length
max(chains_table$Chain_Length)
mean(chains_table$Chain_Length[chains_table$Chain_Length>0])
length(chains_table$Chain_Length[chains_table$Chain_Length>0])
#max(chains_table$Chain_Length)->params$maxChainLength[which(params$name==paste0(dataset_name))]
#mean(chains_table$Chain_Length[chains_table$Chain_Length>0])->params$meanChainLength[which(params$name==paste0(dataset_name))]
#length(chains_table$Chain_Length[chains_table$Chain_Length>0])->params$nChains[which(params$name==paste0(dataset_name))]

# save
#write_csv(params,file="wham_data/TANGO2/bnlearn-networks/TANGO2_network_parameters2.csv")
