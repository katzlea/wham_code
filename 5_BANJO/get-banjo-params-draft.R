#
#-----------------------------------------------------------------
#           STEP X.X in the WHAM-workflow : EXTRACT PARAMETERS
#                       by LEA KATZ
#-----------------------------------------------------------------
# This script extracts parameters from banjo networks


#install.packages("bnlearn",tidyverse","Rgraphviz")
library(bnlearn)
library(tidyverse)
library(Rgraphviz)
library(readxl)

#load data
params<-read_xlsx("wham_data/TANGO2/banjo_outputs/TANGO2_network_parameters.xlsx")

#specify variables
dataset_name="HI_general_OK"
campaign="TANGO2"

load(file=paste0("wham_data/",campaign,"/for_banjo/",campaign,"_",dataset_name,"_bni.RData"))
final_all->final
final[,1:ncol(final)]<-lapply(final[,1:ncol(final)],function(x) as.factor(x))
final->data

#from banjo
edges<-read.csv(file=paste0("wham_data/",campaign,"/banjo_outputs/",
                            campaign,"_",dataset_name,"/",
                            campaign,"_",dataset_name,"_EdgesModified.csv"))
#frequency cutoff
cutoff<-params$cutOffFreq[which(params$name==paste0(dataset_name))]
edges<-edges[1:which(edges$TotalFreq==cutoff),]
edges<-separate(edges,AB,into=c("from","to"),sep="->",remove=FALSE)


numvec<-c(as.character(0:(params$nNodes[which(params$name==paste0(dataset_name))]-1)))
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
adj_matrix <- amat(e) # Convert bn to an adjacency matrix
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE) # Convert to an igraph object
topo_order <- as_ids(topo_sort(g, mode = "out"))  # Get node names properly
longest_path <- rep(0, vcount(g)) # Initialize longest path vector
names(longest_path) <- V(g)$name  # Ensure correct indexing
for (node in topo_order) {
  parents <- neighbors(g, node, mode = "in")  # Get parent nodes
  if (length(parents) > 0) {
    parent_names <- as_ids(parents)  # Convert to proper node names
    longest_path[node] <- max(longest_path[parent_names]) + 1
  }
}
max_chain_length <- max(longest_path, na.rm = TRUE)


root_nodes <- V(g)[degree(g, mode = "in") == 0]$name
leaf_nodes <- V(g)[degree(g, mode = "out") == 0]$name
dist_matrix <- distances(g, mode = "out")
root_leaf_paths <- dist_matrix[root_nodes, leaf_nodes, drop = FALSE]
valid_paths <- root_leaf_paths[root_leaf_paths > 0 & !is.infinite(root_leaf_paths)]
mean_chain_length_root_leaf <- mean(valid_paths, na.rm = TRUE)

# Print result
cat("Mean Root-to-Leaf Chain Length:", mean_chain_length_root_leaf, "\n")
cat("Maximum Chain Length:", max_chain_length, "\n")




# Collect all root-to-leaf paths
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

# Create a data frame with chains and their lengths
chains_table <- data.frame(
  Chain = unlist(chains_list),
  Chain_Length = chain_lengths
)

# Print the table
print(chains_table, row.names = FALSE)
