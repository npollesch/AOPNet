#' AOPN.edge.connectivity
#'
#' Edge connectivity is the minimum number of edges that must be removed
#'     in order to disrupt all paths between to vertices. This function computes edge connectivity
#'     of all MIE/AO pairs
#' @param g Must be an igraph object all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"  [igraph object]
#' @param use_KE_PD Optional (Default=FALSE): Can use KE_PD ("origin" and "terminus") instead of KE_KED [boolean]
#' @return Edge connectivity of MIE AO pairs [data.frame]
#' @family AOPNet Operations
#' @export

###################################################################
## FUNCTION: AOPN.edge.connectivity
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

AOPN.edge.connectivity<-function(g, use_KE_PD=FALSE){

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
