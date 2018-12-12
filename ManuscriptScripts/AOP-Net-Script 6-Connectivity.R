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
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network
###   5) "AOP-Net-5-Linear AOPs.R"              identifies all linear aops


### Degree in/out

V(g1)$degree_in<-degree(g1, mode="in")
V(g1)$degree_out<-degree(g1, mode="out")
V(g1)$degree_total<-degree(g1, mode="all")

max(V(g1)$degree_in) # 10
V(g1)[V(g1)$degree_in==10] # KE 351 (increased, mortality)...bummer :(

max(V(g1)$degree_out) # 10 also
V(g1)[V(g1)$degree_out==10] # 18 and 167 (Activation AhR, and Activation, LXR)

max(V(g1)$degree_total) # 15
V(g1)[V(g1)$degree_total==15] # 55 (cell injury/death)


### betweenness centrality

V(g1)$betweenness<-betweenness(g1, nobigint = FALSE, normalized=FALSE)
V(g1)$n_betweenness<-betweenness(g1, nobigint = FALSE, normalized=TRUE)

max(V(g1)$betweenness) #1967
V(g1)[V(g1)$betweenness==1967] # 1088, increased oxidative stress

max(V(g1)$n_betweenness) #1967
V(g1)[V(g1)$n_betweenness==max(V(g1)$n_betweenness)] # 1088, increased oxidative stress
#normlizing doesnt seem to make much difference



### AOP Occurence

# calculate and add LAOC (linear AOP occurence) attribute to KEs
g1<-add_KE_LAOC(g1) 

mean(V(g1)$KE_LAOC)     # mean = 123
median(V(g1)$KE_LAOC)   # median = 5
max(V(g1)$KE_LAOC)      # max = 6615, KE 341: "Impaired Learing and Memory"

V(g1)$AOP_ID[V(g1)$ID=="341"]
#[1] "12" "13" "17" "48" "54" "77" "78" "87" "88" "89" "90" "99"
# KE 341 has 12 AOP-IDs
# KE 341 is part of the subgraph that has the most LAOPS
# KE 341 is an AO (sometimes KE that is upstream of several other AOs)
# downstream of MIEs 209, 828, 898, and 1486 (three of which of are in MIE/AO pairs with greatest # of laops)
# upstream of AOs 563, 566, 568 and 572 (563 is in MIE/Ao pairs with greatest laops)


### AOP edge connectivity

# full network (takes a few minutes)
edgeCon_g1<-aop_connectivity(g1)
mean(edgeCon_g1$edgeCon)            # mean = 1.08
median(edgeCon_g1$edgeCon)          # median = 1
max(edgeCon_g1$edgeCon)             # max = 5
edgeCon_g1[edgeCon_g1$edgeCon==5,]  # MIE/AO pair 167/459 have edgeCon =5  (part of liver steatosis subGraph)


# Table of LAOPs and Edge connectivity for all MIE/AO pairs
laop_vs_edgeCon<-data.frame(
  MIE=laopTable$MIE,
  AO=laopTable$AO,
  LAOPS=laopTable$LAOPS,
  edgeCon=edgeCon_g1$edgeCon[row.match(edgeCon_g1[,1:2], laopTable[,1:2])],
  stringsAsFactors=FALSE
)


# example of high edgeCon sub graph (MIE:167 AO:459 have 18 LAOPs and edgeCon=5), for plotting later
eList<-sapply(laops_g1[["167 459"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
hi_E<-subgraph.edges(g1, eids=E(g1, P=eList ))

# example of low edgeCon sub graph (MIE:1486 AO:566 have 80 LAOPS and edgeCon=1), for plotting later
eList<-sapply(laops_g1[["1486 566"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
low_E<-subgraph.edges(g1, eids=E(g1, P=eList ))


# adjacent KERs onyl
edgeCon_g1_adj<-aop_connectivity(g1_adj)
mean(edgeCon_g1_adj$edgeCon)            # mean = 1.02
median(edgeCon_g1_adj$edgeCon)          # median = 1
max(edgeCon_g1_adj$edgeCon)             # max = 3
edgeCon_g1_adj[edgeCon_g1_adj$edgeCon==3,]
# Three MIE/AO pairs with max adjacent edgeCon
#  MIE   AO EdgeCon
#   18  455       3   part of liver steatosis network
#   79  675       3   reduced reporductive success from cyclooxygenase inhibition network
# 1317 1344       3   selective serotonin reuptake inhibitors network

sum(edgeCon_g1$edgeCon==1)
# 859 of the possible 913 MIE/AO pairs (94%) had edgeCon=1


