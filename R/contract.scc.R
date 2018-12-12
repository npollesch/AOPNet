#' contract.scc
#'
#' Condense all strongly connected components in a graph
#' @param g Must be an igraph object all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"  [igraph object]
#' @return New condensed igraph object where each strongly connected component has been contracted to a single node [igraph object]
#' @family AOPNet Analyses
#' @export

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
