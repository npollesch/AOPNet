
###################################################################
## FUNCTION: add_KE_LAOC
## adds linear AOP occurence counts
###################################################################
#
### Description: counts the number of times a KE occurs in different linear AOP paths
#
### INPUT: AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO")
#   optional: can use KE_PD ("origin" and "terminus") instead of KE_KED
#
### OUTPUT: identical igraph object with new vertex attribute called KE_LAOC (linear AOP Occurence)

add_KE_LAOC<-function(g, use_KE_PD=FALSE){
  laops_g<-linear.AOPs(g, use_KE_PD=use_KE_PD)
  KEs<-V(g)$name
  LAOC<-vector()
  for(i in KEs){
    count<-sum(sapply(laops_g, FUN=function(pairList){
      sum(sapply(pairList, FUN=function(pathlist) i%in%attributes(pathlist)$names))
    }))
    LAOC<-c(LAOC, count)
  }
  newG<-g
  V(newG)$KE_LAOC<-LAOC
  return(newG)
}



#####################################################
## FUNCTION: add_KE_PD
## finds "origin" and "terminus" vertices
#####################################################

# Adds $KE_PD (KE Path Designator) vertex attribute: 
#    "origin"    if formally described as MIE, OR degree in = 0
#    "terminus"  if formally described as AO, or degree out = 0
#    ""          if not origin or terminus

### INPUT: Must be an igraph object
#   all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO" 

### OUTPUT: New igraph object that is identical to input object
#   but now with $KE_PD vertex attribute

add_KE_PD<-function(g){
  V(g)$KE_PD<-""
  V(g)$KE_PD[V(g)$KE_KED=="MIE"]<-"origin"
  V(g)$KE_PD[degree(g, mode="in")==0]<-"origin"
  V(g)$KE_PD[V(g)$KE_KED=="AO"]<-"terminus"
  V(g)$KE_PD[degree(g, mode="out")==0]<-"terminus"
  return(g)
}



##########################################################
## FUNCTION: add_KER_adjacency
## Identify non-adjacent KERs in an AOP network
##########################################################

### Algorithm Description
#     STEP 1: Potenial non-adjacent KERs identified when distance between KEup and KEdown can be greater than two KEs
#     STEP 2: Longest unique paths between "origin" to "terminus" pairs identified.
#             NOTE: "Origin" and "terminus" are used instead of MIE/AO to account for incomplete AOPs that do not have the MIE or AO specified yet
#     STEP 3: A KER is non-adjacent if its KEs have a possible distance >2 (Step 1) AND DOES NOT occur in any longest unique path (Step 2)
#
### Input: an igraph object with:
#     Vertex attribute called KE_KED with values "MIE", "KE", or "AO"
#
### Output: identical igraph object as input, with new edge attribute called "adjacency"

add_KER_adjacency<-function(g){
  
  # libraries
  require(igraph)
  
  ### STEP 1: check all edges for when possible simple path length >2
  
  allE<-as_edgelist(g, names=FALSE)
  maybeNon<-vector()
  for(i in 1:nrow(allE)){
    sPaths<-all_simple_paths(g, from=allE[i,1], to=allE[i,2])
    if(all(sapply(sPaths,length)<3)){
      maybeNon<-c(maybeNon,0)
    }else{
      maybeNon<-c(maybeNon,1)
    }
  }
  maybeNon_E<-allE[maybeNon==1,] 
  maybeNon_E<- matrix(V(g)[maybeNon_E]$name, ncol=2) # Edge list of "potentially" non-adjacent KERs
  
  
  ### Step 2: identify longest unique paths between "origin" to "terminus" pairs
  #     Step A: identify all simple paths between all origin and terminus pairs using linear.AOPs() function
  #     Step B: A path is a "longest unique path" if there is NO OTHER LONGER path that contains all the same KEs
  
  #~~ Step 2a ~~~~
  # add KE_PD vertex attributes (identifies "origin" and "terminus" KEs)
  gOT<-add_KE_PD(g)
  
  # identify all simple paths between Origin/Terminus pairs
  p<-linear.AOPs(gOT, use_KE_PD=TRUE)
  
  #~~ Step 2b ~~~
  # identif all longest unique paths:
  longPaths<-list()
  for(i in names(p)){
    longPaths[[i]]<-list()
    
    # sort all paths for O/T pair i, in order from longest to shortest
    byL<-order(sapply(p[[i]], FUN=length), decreasing=TRUE) 
    testSet<-p[[i]][byL]
    
    # moves through the "testSet" paths until they are all checked against shorter paths
    while(length(testSet)>0){
      
      # if only one path left, it is moved to longPaths list
      if(length(testSet)==1){
        longPaths[[i]][[length(longPaths[[i]])+1]]<-testSet[[1]]
        testSet<-testSet[-1]
        
        # else check if all KEs on shorter paths are contained within path "1"
        # if so, it is NOT a "longest unique path"
      }else{
        hasShort<-vector()
        for(j in 2:length(testSet)){
          if(length(testSet[[j]]) < length(testSet[[1]]) & all(testSet[[j]]%in%testSet[[1]])){  
            hasShort<-c(hasShort,j)
          }
        }
        
        # move top path in testList to longPAths and remove all short paths identifed from testList. Repeat until testList is empty
        longPaths[[i]][[length(longPaths[[i]])+1]]<-testSet[[1]]
        testSet<-testSet[-c(1,hasShort)]
      }
    }
  }
  
  #Convert longPaths into an edge list using edge_from_path() function
  lp_E_temp<-list()
  for(i in 1:length(longPaths)){
    lp_E_temp[[i]]<-lapply(longPaths[[i]], edge_from_path, by.vertex.name=TRUE)  
  }
  lp_E<-lapply(lp_E_temp, function(x) do.call(rbind,x))
  lp_E<-do.call(rbind,lp_E)
  lp_E<-unique(lp_E)  # Edge list of KERs occur longest unique paths
  
  
  ### Step 3
  
  # compare "maybeNon_E" to "lp_E" edgeList
  #   if edge from "maybeNon_E"  DOES NOT also occur in lp_E, then it is NON-ADJACENT
  lp_E<-as.data.frame(lp_E, stringsAsFactors=FALSE)
  maybeNon_E<-as.data.frame(maybeNon_E, stringsAsFactors=FALSE)
  nonAdjE<-maybeNon_E[is.na(row.match(maybeNon_E, lp_E)),]
  nonAdjE<-as.vector(as.character(t(nonAdjE))) #format so that list can be used to call edges using E(g)
  
  # FINAL: add edge attribute $adjacency, with value "adjacent" or "non-adjacent"
  newG<-g
  E(newG)$adjacency<-"adjacent"
  E(newG, P=nonAdjE)$adjacency<-"non-adjacent"
  return(newG)
  
}



###################################################################
## FUNCTION: aop_connectivity
## MIE AO edge connectivity
###################################################################
#
### Description: Edge connectivity is the minimum number of edges that must be removed 
#     in order to disrupt all paths between to vertices. This function computes edge connectivity
#     of all MIE/AO pairs
#
### INPUT: AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO")
#   optional: can use KE_PD ("origin" and "terminus") instead of KE_KED
#
### OUTPUT: data from of MIE AO pairs and their edge connectivity

aop_connectivity<-function(g, use_KE_PD=FALSE){
  
  if(use_KE_PD==TRUE){
    #identify all "origins" and "termini"
    pStart<-V(g)[V(g)$KE_PD=="origin"]
    pEnd<-V(g)[V(g)$KE_PD=="terminus"]
  }else{  
    #identify all "MIEs" and "AOs"
    pStart<-V(g)[V(g)$KE_KED=="MIE"]
    pEnd<-V(g)[V(g)$KE_KED=="AO"]
  }
  
  if(length(pStart)==0|length(pEnd)==0){
    stop("no start/end pairs")
  }
  
  MIE<-vector()
  AO<-vector()
  edgeCon<-vector()
  for(i in 1:length(pStart)){
    for(j in 1:length(pEnd)){
      MIE<-c(MIE, pStart[i]$name)
      AO<-c(AO, pEnd[j]$name)
      ec<-edge.connectivity(g, source=pStart[i], target=pEnd[j])
      edgeCon<-c(edgeCon, ec)
    }
  }
  
  result<-data.frame(
    MIE=MIE[edgeCon>0],
    AO=AO[edgeCon>0],
    edgeCon=edgeCon[edgeCon>0]
  )
  return(result)
}



###################################################################
## FUNCTION: contract.scc
## Condense all strongly connected components in a graph
###################################################################

contract.scc<-function(g){
  sccMap<-components(g, mode="strong")$membership
  ntcomps<-which(components(g, mode="strong")$csize>1)
  
  # identify if the strong components have MIE/AO or origin/terminus attributes
  has_M<-vector()
  has_A<-vector()
  no_MA<-vector()
  has_O<-vector()
  has_T<-vector()
  no_OT<-vector()
  for(i in ntcomps){
    if(any(V(g)$KE_KED[sccMap==i]=="MIE")){
      has_M<-c(has_M,i)
    }else{
      if(any(V(g)$KE_KED[sccMap==i]=="AO")){
        has_A<-c(has_A,i)
      }else{
        no_MA<-c(no_MA,i)
      }
    }
    if(any(V(g)$KE_PD[sccMap==i]=="origin")){
      has_O<-c(has_O,i)
    }else{
      if(any(V(g)$KE_PD[sccMap==i]=="terminus")){
        has_T<-c(has_T,i)
      }else{
        no_OT<-c(no_OT,i)
      }
    }
  }
  
  # tells contract() function that if merged vertices have plotting coordinates, take the mean of them
  vAttComb<-list(
    plotX = "mean",
    plotY = "mean",
    toString
  )
  
  #contract the vertices in strongly connected components
  g2<-contract(g,mapping=sccMap, vertex.attr.comb = vAttComb)
  
  # remove redundant edges and resulting loops
  newG<-simplify(g2, remove.multiple=TRUE, remove.loops=TRUE)
  
  # assign attributes to contracted vertices
  V(newG)$col[ntcomps]<-"purple"    
  V(newG)$size<-3
  V(newG)$size[ntcomps]<-4
  V(newG)$KE_KED[has_M]<-"MIE"
  V(newG)$KE_KED[has_A]<-"AO"
  V(newG)$KE_KED[no_MA]<-"KE"
  V(newG)$KE_PD[has_O]<-"origin"
  V(newG)$KE_PD[has_T]<-"terminus"
  V(newG)$KE_PD[no_OT]<-""
  return(newG)
}



##############################################
## FUNCTION: edge_from_path
## CREATE EDGE LIST FROM PATHS
##############################################

## INPUT  is a vector that lists the sequence of vertices in a path (from start to end if a directed path)
## OUTPUT is a 2-column matrix, each row representing the start and end vetices of an edge

edge_from_path<-function(x, by.vertex.name=TRUE){
  require(igraph)
  
  edgeList<-vector()
  for(i in 1:(length(x)-1)){
    edgeList<-c(edgeList, x[i], x[i+1])
  }
  if(by.vertex.name==TRUE){
    edgeList<-matrix(names(edgeList), ncol=2, byrow=TRUE)
  }else{
    edgeList<-matrix(edgeList, ncol=2, byrow=TRUE)
  }
  return(edgeList)
}



####################################################################
## FUNCTION: linear.AOPs 
## Determines all linear AOPs (ie simple paths) between all
## possible MIE and AO pairs (or "ORIGIN" and "TERMINUS" if desired)
####################################################################

### INPUT must be an igraph object
#   all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"
#   all vertices must have a "names" attribute (this is used to identify vertices)
#   optionally, if use_KE_PD=TRUE, then all verticesmust have an attributes called "KE_PD" (Path Designator), with values of "origin", "", or "terminus" 

### OUTPUT is a "List of a List" of all paths (by vertex) between all MIE/AO pairs (or origin/terminus pairs)
#   List objects are named by the MIE/AO pair involved
#   List objects are Lists of all paths (by vertex) between the MIE and AO for which the List is named
#   if "remove.zero.paths=TRUE", all origin/terminus pairs with ZERO linear AOPs are removed from the summary, if FALSE they are left in

# takes about 3 mins to run on my computer for the full wiki

linear.AOPs<- function(g, use_KE_PD=FALSE){
  #libraries
  require(igraph)
  
  if(use_KE_PD==TRUE){
    #identify all "origins" and "termini"
    pStart<-V(g)[V(g)$KE_PD=="origin"]
    pEnd<-V(g)[V(g)$KE_PD=="terminus"]
  }else{  
    #identify all "MIEs" and "AOs"
    pStart<-V(g)[V(g)$KE_KED=="MIE"]
    pEnd<-V(g)[V(g)$KE_KED=="AO"]
  }
  
  if(length(pStart)==0|length(pEnd)==0){
    return(list(NULL))
  } 
  
  #list and table to store results
  tempList<-list()
  
  #identify all simple paths between all possible MIE and AO pairs
  for(i in 1:length(pStart)){
    for(j in 1:length(pEnd)){
      tempList[[paste(names(pStart)[i],names(pEnd)[j])]]<-all_simple_paths(g, from=pStart[i], to=pEnd[j], mode="out")
    }
  }
  
  #remove instances where #paths=0 from pathList
  pathList<-list()
  for(i in names(tempList)){
    if(length(tempList[[i]])>0){
      pathList[[i]]<-tempList[[i]]
    }
  }
  
  #result
  return(pathList)
}



###################################################################
## FUNCTION: short.path.edge.color
## edge color for all shortest paths between two nodes
###################################################################
#
### Description: provides colors for all edges in a igraph object, where the shortest path is colored, all other edges are transparent
#
### Input: iGrapgh object, from node, to node, LOC (leave original color), path color (default is purple), non-path color (default is "transparent") edge weights, all (color all shortets paths, or only 1/first))
#
### Output: colors to use for edges, where shortest paths are colored as indicated by user

short.path.edge.color<-function(gr,fromnode,tonode,loc=F,clr="purple", nonclr="transparent" , weight=NA , all=T){
  if(nonclr=="transparent"){nonC<-rgb(1,1,1, alpha=0)}else{nonC<-nonclr}
  paths<-all_shortest_paths(gr,from=fromnode,to=tonode,mode="out",weights=weight)
  if(all){
    if(length(paths)==0){return("No simple paths between nodes")}
    else{
      if(loc){
        for(i in 1:length(paths[[1]])){
          E(gr,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(which(!is.na(E(gr)$clrs)))}
      else{
        E(gr)$clrs<-nonC
        for(i in 1:length(paths[[1]])){
          E(gr,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(E(gr)$clrs)}
    }
  }
  else{
    if(length(paths)==0){return("No simple paths between nodes")}
    else{
      if(loc){
        for(i in 1:1){
          E(gr,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(which(!is.na(E(gr)$clrs)))}
      else{
        E(gr)$clrs<-nonC
        for(i in 1:1){
          E(gr,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(E(gr)$clrs)}
    }
  }
}



###################################################################
## FUNCTION: topo.lay
## Topological Sorting
###################################################################
#
### Description: topo.lay() function computes a topological ordering layout for visualizaiton of an igraph object
#
### Input: iGrapgh object with no strongly connected components
#
### Output: layout coordinates for plotting a topologically sorted network (can be used for "layout" argument of the plot function)


topo.lay <- function(gr){
  tsort<-topo_sort(gr, mode = c("out"))
  tpos<-match(V(gr)$name,tsort$name) # finds position of node names in topoloigcal ordering
  tlay<-cbind(tpos,tpos) #define a (x,y) plot layout that orders accoring to topological order
  return(tlay) # return layout coordinates
}
