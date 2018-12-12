#' add.adjacency
#'
#' Adds \code{$adjacency} edge attribute:
#'  Algorithm Description
#'    STEP 1: Potenial non-adjacent KERs identified when distance between KEup and KEdown can be greater than two KEs
#'    STEP 2: Longest unique paths between "origin" to "terminus" pairs identified.
#'             NOTE: "Origin" and "terminus" are used instead of MIE/AO to account for incomplete AOPs that do not have the MIE or AO specified yet
#'    STEP 3: A KER is non-adjacent if its KEs have a possible distance >2 (Step 1) AND DOES NOT occur in any longest unique path (Step 2)

#' @param g Must be an igraph object all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"  [igraph object]
#' @return identical igraph object as input, with new edge attribute called \code{$adjacency} [igraph object]
#' @family KER Attribution
#' @export

##########################################################
## FUNCTION: add.adjacency
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

add.adjacency<-function(g){
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
