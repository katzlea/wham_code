#
#-----------------------------------------------------------------------
#           STEP 5.5 in the WHAM-workflow : PREDICTIVE MODELS
#                       by LEA KATZ
#-----------------------------------------------------------------------
# This script calculates CPTs for all nodes
# 
# Input needed : Bayesian Network in bnlearn format

#load data
campaign="TANGO2"
dataset_name="HI_general_OK"
load(file=paste0("wham_data/",campaign,"/bnlearn-networks/",campaign,"_",dataset_name,"_network.RData"))
load(file=paste0("wham_data/",campaign,"/bnlearn-networks/",campaign,"_",dataset_name,"_data.RData"))

print(e)
graphviz.plot(e)

# -------------------------------------------------------------
# CONDITIONAL PROBABILITY TABLES
# -------------------------------------------------------------
e.fit<-bn.fit(e,data)

#just one
cpquery(e.fit,(edwardsiella_msp4=="1"),(laternula=="0"))

#for all nodes, if one changes
# -------------------------------------------------------------
mynode="macroalgae_all"
# -------------------------------------------------------------
probs.ff<-data.frame(matrix(ncol = length(levels(data[[mynode]])), nrow = 1))
colnames(probs.ff)<-levels(data[[mynode]])

for (x in 1:length(colnames(data))) {
  node<-colnames(data)[x]
  lvls<-levels(data[,x])
  
  for (y in 1:length(lvls)) {
    state.event=lvls[y]
    event = paste0(node,"==",'"',state.event,'"')
    myrow<-data.frame(matrix(ncol = length(levels(data[[mynode]])), nrow = 1))
    colnames(myrow)<-levels(data[[mynode]])
    rownames(myrow)[1]<-event
    
    for (a in 1:length(levels(data[[mynode]]))) {
      state.evidence=levels(data[[mynode]])[a]
      evidence = paste0(mynode,"==",'"',state.evidence,'"')
      qtxt=paste("cpquery(e.fit,",event,",",evidence,")")
      prob=eval(parse(text=qtxt))
      myrow[a]<-prob
    }
    probs.ff<-rbind(probs.ff,myrow)
  }
}

probs.ff<-probs.ff[-1,]
probs.ff<-probs.ff %>% mutate(delta01=probs.ff$'0' - probs.ff$'1')
probs.ff<-probs.ff %>% mutate(delta12=probs.ff$'1' - probs.ff$'2')
probs.ff<-probs.ff %>% mutate(delta02=probs.ff$'0' - probs.ff$'2')

#save
write.csv(probs.ff,file=paste0("wham_data/",campaign,"/cpt/",campaign,"_",dataset_name,"_",mynode,".csv"))


