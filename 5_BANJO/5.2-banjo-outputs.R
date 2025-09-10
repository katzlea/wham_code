#
#-----------------------------------------------------------------
#           STEP 5.2 in the WHAM-workflow : BANJO OUTPUTS
#                       by LEA KATZ
#-----------------------------------------------------------------
# Modified from script by courtesy of Ming Khan.
#
# This script takes BANJO output files to create 
# - a summary BN table 
# - high frequency edge pairs 
# - .dot file plotting code  

campaign="TANGO2"
dataset="NWAP_tr_sic"
#iterations="x1000"

dat <- read.table(file = paste0("wham_data/",campaign,"/banjo_outputs/",campaign,"_",dataset,"/",campaign,"_",dataset,".txt_combine.txt"))

# -------------------------------------------------------------
# CREATE A SUMMARY TABLE OF EDGES
# -------------------------------------------------------------

edges <- as.matrix(dat$V1)
res <- strsplit(edges, "->")
res <- as.data.frame(do.call(rbind, res))

colnames(res) <- c("from", "to")

#Reverse edges 
res2 <- cbind(res$to, res$from)
res2 <- as.data.frame(res2)
colnames(res2) <- c("from", "to")

#create the reverse edges 
rev <- as.matrix(paste(res2$from, res2$to, sep = "->"))

#Find the indices that match 
ind <- match(rev, edges)

indEdge <- ind[!is.na(ind) == TRUE] #index of the edges that have reverses 
indRev <- which(!is.na(ind)) #index of the reverse edges 

Edges <- dat[indEdge,]
Revs <- dat[indRev,]

EdgeSummary <- cbind(Edges, Revs)
colnames(EdgeSummary) <- c("AB", "FreqAB", "StrAB", "BA", "FreqBA", "StrBA")

EdgeNoRev <- dat[-indEdge,]

temp <- matrix(NA, nrow = nrow(EdgeNoRev), ncol = 3)

temp2 <- cbind(EdgeNoRev, temp)
colnames(temp2) <- c("AB", "FreqAB", "StrAB", "BA", "FreqBA", "StrBA")
temp2$FreqBA <- 0
temp2$StrBA <- 0

EdgeSummary <- rbind(EdgeSummary, temp2)

#add the total frequency 
EdgeSummary$TotalFreq <- EdgeSummary$FreqAB + EdgeSummary$FreqBA

#Calculate the Influence Scores 
EdgeSummary$IS <- (EdgeSummary$FreqAB*EdgeSummary$StrAB + 
                     EdgeSummary$FreqBA*EdgeSummary$StrBA)/EdgeSummary$TotalFreq

#order the edges by total frequency 
EdgeSummary <- EdgeSummary[order(EdgeSummary$TotalFreq, decreasing = TRUE),]
EdgeSummary$pen <- EdgeSummary$TotalFreq/10



write.csv(EdgeSummary, row.names = FALSE, 
          file = (paste0("wham_data/",campaign,"/banjo_outputs/",campaign,"_",dataset,"/",campaign,"_",dataset,"_Edges.csv")))

# -------------------------------------------------------------
# HISTOGRAM OF EDGES
# -------------------------------------------------------------
windows()
par(mfrow = c(1, 2))
barplot(EdgeSummary$TotalFreq)
hist(EdgeSummary$TotalFreq, breaks = 50)
