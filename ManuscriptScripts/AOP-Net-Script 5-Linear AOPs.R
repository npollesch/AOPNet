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


### User defined linear AOPs

uLaopCount<-vector()
for(i in 1:nrow(aData)){
  if(!is.null(aData$kers[[i]])){
    
    # edge list for each user-define AOP
    eList<-aData$kers[[i]]
    
    # subgraph by edgelist
    subG<-graph_from_edgelist(as.matrix(eList[,c("KEup", "KEdown")]), directed=TRUE)
    
    # add KE_KED to each KE (MIE, AO, or KE)
    subKed<-data.frame(KE=c(eList$KEup,eList$KEdown), KED=c(eList$KEDup, eList$KEDdown), stringsAsFactors=FALSE) 
    subKed<-unique(subKed)
    V(subG)$KE_KED<-subKed$KED[match(V(subG)$name,subKed$KE)]
    
    # count linear AOPs
    laops_subG<-linear.AOPs(subG)
    if(length(laops_subG)==0){
      uLaopCount<-c(uLaopCount,0)
    }else{
      uLaopCount<-c(uLaopCount,sum(sapply(laops_subG,length)))
    }
  }else{
    uLaopCount<-c(uLaopCount,0)
  }
}
sum(uLaopCount) # 471 user define linear AOPs
mean(uLaopCount) # mean = 2.3 laops per user-defined AOP
median(uLaopCount) # median = 1
max(uLaopCount) 
aData$ID[uLaopCount==max(uLaopCount)] # AOP:58  has max of 32 laops

sum(uLaopCount==0) # 40 (21%)
sum(uLaopCount==1) # 63 (34%)
sum(uLaopCount==2) # 38 (20%)
sum(uLaopCount>2) # 46 (25%)

# result summary
udLaops<-data.frame(aData$ID, LAOPs=uLaopCount)


### Linear AOPs in Full Network
laops_g1<-linear.AOPs(g1, use_KE_PD=FALSE) # takes a few minutes
sum(sapply(laops_g1,length)) 
# 9876  laops total

laopCount<-sapply(laops_g1, length)
mean(laopCount)     # mean = 10.8 (not inlcuding MIE/AO pairs with zero)
median(laopCount)   # meadian = 3 (not inlcuding MIE/AO pairs with zero)
max(laopCount)      # max = 292 laops

length(laopCount) # 913 (number of MIE/AO pairs with LAOPs)

sum(laopCount==1) # 235
sum(laopCount==2) # 179
sum(laopCount>2) # 499
sum(laopCount>50) # 22

#in table form
splitN<-strsplit(names(laopCount), " ")
laopTable<-data.frame(
  MIE=sapply(splitN, FUN=function(x) x[1]),
  AO=sapply(splitN, FUN=function(x) x[2]),
  LAOPS=laopCount,
  stringsAsFactors=FALSE,
  row.names=NULL
)

names(laops_g1)[laopCount==max(laopCount)]
# 3 MIE/AO pairs have the max # (292) of laops
# MIE: 828 to AO:563
# MIE: 898 to AO:563
# MIE:1486 to AO:563

# create subgraphs of these MIE/AO pairs
subList<-list(laops_g1[["828 563"]], laops_g1[["898 563"]], laops_g1[["1486 563"]] )
laopSubGraphs<-list()
for(i in 1:length(subList)){
  eList<-sapply(subList[[i]], edge_from_path)
  eList<-do.call("rbind", eList)
  eList<-unique(eList)
  eList<-as.vector(t(eList))
  laopSubGraphs[[i]]<-subgraph.edges(g1, eids=E(g1, P=eList ))
}

sapply(laopSubGraphs, vcount)
sapply(laopSubGraphs, ecount)
# 2 subgraphs have 28 and 46  V and Es...are they the same?

# same KEs?
all(V(laopSubGraphs[[1]])$ID%in%V(laopSubGraphs[[3]])$ID) #FALSE

# is smaller one contained in larger ones?
all(V(laopSubGraphs[[2]])$ID%in%V(laopSubGraphs[[1]])$ID) #TRUE
all(V(laopSubGraphs[[2]])$ID%in%V(laopSubGraphs[[3]])$ID) #TRUE
# subgraph 2 is contained within 1 and 3...so only really two subgraphs with significant overlap

# combine into a single subgraph
subE<-unique(rbind(as_edgelist(laopSubGraphs[[1]]), as_edgelist(laopSubGraphs[[3]])))
subL<-subgraph.edges(g1, eids=E(g1, P=c(t(subE))))

# has strong componets?
components(subL)
# has one non-trivial strong components (#13)
V(subL)$scc<-components(subL, mode="strong")$membership

# how many laops in this single subgraph?
laops_subL<-linear.AOPs(subL)
sum(sapply(laops_subL, length))
#1785 laops in this subGraph only, nearly 20% of total



# MIE/AO pair with most laops AND no strong components
# subgraph with strong components removed
subNoS<-induced_subgraph(g1, vids= V(g1)[!V(g1)$scc%in%ntcomps])
any(components(subNoS, mode="strong")$csize>1) # FALSE (strong components removed)
laops_NoS<-linear.AOPs(subNoS)
lCountNoS<-sapply(laops_NoS, length)
max(lCountNoS) # max = 119 laops
names(laops_NoS)[lCountNoS==max(lCountNoS)]
# MIE 279 to AO 563 has 119 laops

# create subgrapg of these laops
eList<-laops_NoS[["279 563"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(subNoS, eids=E(subNoS, P=eList ))
components(sub_lNoS, mode="strong") # no strong components 

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # sub graph has 348 laops (has several MIE/AO pairs)
# candidate for topo Sort :)
# verdict: too many laops! makes for sloppy graphs/figures

# MIE/AO pair with fewer laops= 167/549
# create subgrapg of these laops
eList<-laops_g1[["167 459"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(g1, eids=E(g1, P=eList ))
components(sub_lNoS, mode="strong") # no strong components 

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # subgraph has 21 laops
# better for demonstrating topo sort....less crowded figures...but all woe="HIGH"!!!


# MIE/AO pair with fewer laops= 201/341
# create subgrapg of these laops
eList<-laops_g1[["201 341"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(g1, eids=E(g1, P=eList ))
components(sub_lNoS, mode="strong") # no strong components 

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # subgraph has 35 laops and a good mix of WOE/quant...will use for topo sort figures



#longest laop
laopLength<-lapply(laops_g1, function(x) sapply(x, length))
maxLper<-sapply(laopLength, max)  # max laop length PER MIE/AO pair
maxL<-max(maxLper)                # max laop length = 17 KEs
lpairs<-which(maxLper==maxL)
# Seven MIE/AO pairs with laops of length 17 KEs long
# 41 563
# 478 360
# 478 686
# 478 679
# 724 563
# 828 563
# 1486 563 

# identify and save longest paths (for plotting later)
hasMax<-sapply(lpairs, function(x)laops_g1[[x]])
whereMax<-lapply(hasMax, function(x) which(sapply(x, function(y) length(y)==maxL)) )
longPaths<-list()
for(i in 1:length(whereMax)){
  longPaths[[names(whereMax[i])]]<-lapply(whereMax[[i]], function(x) laops_g1[[names(whereMax[i])]][[x]]) 
}

# save long paths as edgelist (for plotting later)
longEdges<-lapply(longPaths, function(x)
  lapply(x, function(y) edge_from_path(y))
)
  
  
  
### linear AOP analysis of adjacent-only graph

#number of linear AOPs
laops_g1_adj<-linear.AOPs(g1_adj)
sum(sapply(laops_g1_adj,length))
# 3097 linear AOPs when only adjacent MIE to AO paths are considered

# MIE pair with largest number of simple paths
mean(sapply(laops_g1_adj,length))     # mean = 3.4 (not inlcuding MIE/AO pairs with zero)
median(sapply(laops_g1_adj,length))   # meadian = 2 (not inlcuding MIE/AO pairs with zero)
max(sapply(laops_g1_adj,length))   # max = 37 laops

names(laops_g1_adj)[sapply(laops_g1_adj,length)==max(sapply(laops_g1_adj,length))]
# 1 MIE/AO pair with max (37) laops is MIE:559 and AO:563



### linear AOPs in contracted network 

laops_g1_contr<-linear.AOPs(g1_contr)
sum(sapply(laops_g1_contr,length))
# 7260 linear AOPs in contracted network ....does not account for as many loaps as non-adj KERs

# MIE pair with largest number of simple paths
mean(sapply(laops_g1_contr,length))     # mean = 8.0 (not inlcuding MIE/AO pairs with zero)
median(sapply(laops_g1_contr,length))   # meadian = 3 (not inlcuding MIE/AO pairs with zero)
max(sapply(laops_g1_contr,length))   # max = 203 laops

names(laops_g1_contr)[sapply(laops_g1_contr,length)==max(sapply(laops_g1_contr,length))]
# 1 MIE/AO pair with max (203) laops is MIE:279 and AO:563



