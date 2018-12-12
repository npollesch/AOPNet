#libraries
library(igraph)
library(prodlim)
library(RColorBrewer)

#Directories
workingDir<-"C:\\Users\\obrienja\\Documents\\GitHub\\AOPWiki\\R_files_Jason\\"

#load source of functions
source(paste(workingDir,"AOP-Net-Functions.R",sep=""))    #imports custom functions

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data


### User Defined Adj vs NonAdj

#instances where KER is ALWAYS defined by users as direct (adjacent)
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
allAdj<-vector()
for(i in unique(kAdj$ID)){
  tf<-all(kAdj$adjacency[kAdj$ID==i]=="adjacent")
  allAdj<-c(allAdj,tf)
}
sum(allAdj)
# 859 instances where KER is always "adjacent"

#instances where KER is ALWAYS defined by users indirect (non adjacent)
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
allNon<-vector()
for(i in unique(kAdj$ID)){
  tf<-all(kAdj$adjacency[kAdj$ID==i]=="non-adjacent")
  allNon<-c(allNon,tf)
}
sum(allNon)
#180 instances where KER is always "non-adjacent"

#instances where KER adjacency is different across multiple AOPs
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
isDiff<-vector()
for(i in unique(kAdj$ID)){
  tf<-!all(kAdj$adjacency[kAdj$ID==i]==kAdj$adjacency[kAdj$ID==i][1])
  isDiff<-c(isDiff,tf)
}
sum(isDiff)
#11 instances where KER adjacency changes across AOPs


### Algorithm Based Identification of Adj vs NonAdj using add_KER_adjacency() function

g1<-add_KER_adjacency(g1) # may take a few mins

# number adjacent and non-adjacent, based on algorithm
sum(E(g1)$adjacency=="adjacent")
sum(E(g1)$adjacency=="non-adjacent")
# 943 adjacent, 107 non-adjacent


# Subgraph with only adjacent edges
g1_adj<-subgraph.edges(g1, eids=E(g1)[E(g1)$adjacency=="adjacent"] )


