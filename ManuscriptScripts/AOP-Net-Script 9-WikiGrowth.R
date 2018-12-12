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
###   6) "AOP-Net-6-Connectivity.R"             AOP occurence and edge connectivity



### Growth by AOP ID  (NOTE: MAY TAKE A FEW HOURS TO COMPLETE)

#list all AOP IDs in graph and list in order
IDs<-unlist(V(g1)$AOP_ID)
IDs<-unique(IDs)
IDs<-IDs[order(as.numeric(IDs))]

#place to store metrics
vC<-vector()        # vectex count
eC<-vector()        # edge count
mieC<-vector()      # MIE count
aoC<-vector()       # AO count
oC<-vector()        # origin count
tC<-vector()        # terminus count
adjC<-vector()      # adjacent KER count
nonC<-vector()      # non-adjacent KER count
wcC<-vector()       # weak component count
scC<-vector()       # strong component count
borrowC<-vector()   # instances a new AOP borrows a KE that is already in the network
laopC<-vector()     # number of linear AOPs
laopC_adj<-vector() # number of linear AOPs, only considering adjacent MIE to AO paths
maxLaoc<-vector()   # maximum AOP Occurence value in network


# vector to store list of vertices that are currently in the growing network
kerSet<-vector()
vSet<-vector()

# Build network from XML data and measure the network one AOP_ID at a time
for(i in 1:length(IDs)){
  print(paste(i, " of ", length(IDs), sep="")) # to track progress
  
  newData<-aData[match(IDs[i], aData$ID),]
  
  # collect KERs to add to network
  if(!is.null(newData$kers[[1]])){
    kerSet<-unique(rbind(kerSet, newData$kers[[1]][,c("KEup", "KEdown")]))
  }
  
  # collect KE information to add to network
  vTemp<-data.frame(
    KE=c(newData$mies[[1]], newData$aos[[1]], newData$kes[[1]]),
    KED=c( rep("MIE", length(newData$mies[[1]])), rep("AO", length(newData$aos[[1]])), rep("KE", length(newData$kes[[1]])) ),
    stringsAsFactors=FALSE)
  
  # number of times the new AOP uses a KE that is already in the network (borrowed a KE)
  if(length(vSet)==0){
    b<-0  
  }else{
    b<-sum(vTemp$KE%in%vSet$KE)
  }
  borrowC<-c(borrowC, sum(borrowC[length(borrowC)],b))
  
  #add new vertices to vSet and update KED if required
  for(j in 1:nrow(vTemp)){
    if(length(vSet)==0){
      vSet<-rbind(vSet, vTemp[j,])
    }else{
      if(vTemp$KE[j]%in%vSet$KE){
        if(vTemp$KED[j]=="MIE" | vTemp$KED[j]=="AO"){
          vSet$KED[vSet$KE==vTemp$KE[j]]<-vTemp$KED[j]
        }
      }else{
        vSet<-rbind(vSet, vTemp[j,])
      }
    }
  }

  # create subG and add KED
  subG<-graph_from_edgelist(as.matrix(kerSet))
  V(subG)$KE_KED<-vSet$KED[match(V(subG)$name,vSet$KE)]
  
  #number of vertices
  vC<-c(vC,vcount(subG))
  
  #number of edges
  eC<-c(eC,ecount(subG))
  
  #number of MIEs and AOs
  mieC<-c(mieC, sum(V(subG)$KE_KED=="MIE"))
  aoC<-c(aoC, sum(V(subG)$KE_KED=="AO"))
  
  #identify and count origin and terminus
  subG<-add_KE_PD(subG) 
  oC<-c(oC, sum(V(subG)$KE_PD=="origin"))
  tC<-c(tC, sum(V(subG)$KE_PD=="terminus"))
  
  #linear AOPs
  laops<-linear.AOPs(subG, use_KE_PD=FALSE)
  laopC<-c(laopC, sum(sapply(laops,length)))
  
  #identify and count adjacent and non-adjacent KERs
  subG<-add_KER_adjacency(subG)
  adjC<-c(adjC, sum(E(subG)$adjacency=="adjacent"))
  nonC<-c(nonC, sum(E(subG)$adjacency=="non-adjacent"))
  
  # create subgraph of only adjacent edges
  subG_adj<-subgraph.edges(subG, eids=E(subG)[E(subG)$adjacency=="adjacent"])
  
  #adj only linear AOPs
  laops_adj<-linear.AOPs(subG_adj, use_KE_PD=FALSE)
  laopC_adj<-c(laopC_adj, sum(sapply(laops_adj,length)))
  
  #components
  wcC<-c(wcC,components(subG, mode="weak")$no)
  scC<-c(scC,sum(components(subG, mode="strong")$csize>1))
  
  # AOP occurnece
  # NOTE: The AOP Occurnece function uses the linear.AOP() function, which is already run above, and takes a lot of time
  #       There for i've copied the part after the linear.AOP call here, rather than use the full function, in order to reduce processing time
  KEs<-V(subG)$name
  LAOC<-vector()
  for(i in KEs){
    count<-sum(sapply(laops, FUN=function(pairList){
      sum(sapply(pairList, FUN=function(pathlist) i%in%attributes(pathlist)$names))
    }))
    LAOC<-c(LAOC, count)
  }
  maxLaoc<-c(maxLaoc,max(LAOC))

}

wikiGrowth<-data.frame(
  AOP_ID=IDs,
  Vs=vC,
  Es=eC,
  MIEs=mieC,
  AOs=aoC,
  origins=oC,
  termini=tC,
  borrow=borrowC,
  adjE=adjC,
  non_adjE=nonC,
  weakC=wcC,
  strongC=scC,
  LAOPS=laopC,
  ADJ_LAOPS=laopC_adj,
  max_laoc=maxLaoc,
  stringsAsFactors=FALSE)

#write.table(wikiGrowth, paste(workingDir,"wikiGrowth_by_AOP-April 1 2018.txt", sep=""), sep="\t", row.names=FALSE)



