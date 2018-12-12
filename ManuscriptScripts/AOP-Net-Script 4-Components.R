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


### Weak Components in Full Network

# weak components
g1_wComps<-components(g1, mode="weak") #35 weak components
mean(g1_wComps$csize) # 23.6
median(g1_wComps$csize) # 7
min(g1_wComps$csize) # 2
max(g1_wComps$csize) # 578

#not including largest
max(g1_wComps$csize[2:35]) #17

# assign the attribute wcc to nodes based on their membership 
V(g1)$wcc<-g1_wComps$membership 


### Strong Components in Full Network

# strong components
g1_sComps<-components(g1, mode="strong")
ntcomps<-which(g1_sComps$csize>1) # non-trivial ccs (i.e. with more than 1 node)
length(ntcomps) # 4 non-trivial strong components
g1_sComps$csize[ntcomps] # with size = 2, 4, 7, 4 KEs each

# assign the attribute scc to nodes based on their membership 
V(g1)$scc<-g1_sComps$membership 

# show KEs and AOPs in each component
for(i in ntcomps){
  print(i)
  print(V(g1)[V(g1)$scc==i])
  print(V(g1)$AOP_ID[V(g1)$scc==i])
}



### Contract Strong Components to form "Contracted Network"

#contract strong components
g1_contr<-contract.scc(g1)



### Weak Components of adjacent-KER-only network

# weak components
g1_adj_wComps<-components(g1_adj, mode="weak")
mean(g1_adj_wComps$csize) # 23.6
median(g1_adj_wComps$csize) # 7
max(g1_adj_wComps$csize) # 578
# 35 weak components when adjacent KERs, all #'ssame as full network

#assign wComp membership
V(g1_adj)$wcc<-g1_adj_wComps$membership 


# strong components- AOPg_adj
g1_adj_sComps<-components(g1_adj, mode="strong")
ntcomps_adj<-which(g1_adj_sComps$csize>1)
length(ntcomps_adj)
g1_adj_sComps$csize[ntcomps_adj]
# for nt strong comps, with sizes 2, 4, 7, 4 (did not change compared to g1)

#assign sComp membership
V(g1_adj)$scc<-g1_adj_sComps$membership 

# compare scc membership between g1 and g1_adj
all(sapply(1:length(ntcomps), FUN=function(x) all(V(g1)[V(g1)$scc==ntcomps[x]]==V(g1_adj)[V(g1_adj)$scc==ntcomps_adj[x]])))
# TRUE i.e. components have the same membership in g1 and g1_adj

# compare edges in components for g1 and g1_adj (by plot)
par(mar=c(0,0,0,0), mfrow=c(length(ntcomps),2))
cList<-list()
cList_adj<-list()
for(i in 1:length(ntcomps)){
  cList[[i]]<-induced.subgraph(g1, vids=V(g1)[V(g1)$scc==ntcomps[i]])
  cList_adj[[i]]<-induced.subgraph(g1_adj, vids=V(g1_adj)[V(g1_adj)$scc==ntcomps_adj[i]])
  
  vCol<-rep("white",length(V(cList[[i]])))
  vCol[V(cList[[i]])$KE_KED=="MIE"]<-"green"
  vCol[V(cList[[i]])$KE_KED=="AO"]<-"red"
  eCol<-rep("grey50", length(E(cList[[i]])))
  eCol[E(cList[[i]])$adjacency=="non-adjacent"]<-"orange"
  eWidth<-rep(2.5,length(E(cList[[i]])))
  eWidth[E(cList[[i]])$adjacency=="non-adjacent"]<-3.5
  set.seed(1)
  plot(cList[[i]], 
       vertex.size=20, vertex.color=vCol, vertex.label=NA,
       edge.width=eWidth, edge.arrow.size=0.5, edge.color=eCol)
  
  vCol<-rep("white",length(V(cList_adj[[i]])))
  vCol[V(cList_adj[[i]])$KE_KED=="MIE"]<-"green"
  vCol[V(cList_adj[[i]])$KE_KED=="AO"]<-"red"
  eCol<-rep("grey50", length(E(cList_adj[[i]])))
  eCol[E(cList_adj[[i]])$adjacency=="non-adjacent"]<-"orange"
  eWidth<-rep(2.5,length(E(cList_adj[[i]])))
  eWidth[E(cList_adj[[i]])$adjacency=="non-adjacent"]<-3.5
  set.seed(1)
  plot(cList_adj[[i]],
       vertex.size=20, vertex.color=vCol, vertex.label=NA,
       edge.width=eWidth, edge.arrow.size=0.5, edge.color=eCol)    
}
# The second ntcomp (component #411) has one extra edge (a nonAdj)... all other strong comps are the same
# conclusion: adjacent KER have no impact on component membership

#contract strong components -AOPg_adj
g1_adj_contr<-contract.scc(g1_adj)


