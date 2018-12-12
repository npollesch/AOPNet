#' add.KE_PD
#'
#' Adds \code{$KE_PD} (KE Path Designator) vertex attribute:
#'    "origin"    if formally described as MIE, OR degree in = 0
#'    "terminus"  if formally described as AO, or degree out = 0
#'    ""          if not origin or terminus
#' @param g Must be an igraph object all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"  [igraph object]
#' @return New igraph object that is identical to input object but now with \code{$KE_PD} vertex attribute [igraph object]
#' @family KE Attribution
#' @export

#####################################################
## FUNCTION: add.KE_PD
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

add.KE_PD<-function(g){
  V(g)$KE_PD<-""
  V(g)$KE_PD[V(g)$KE_KED=="MIE"]<-"origin"
  V(g)$KE_PD[degree(g, mode="in")==0]<-"origin"
  V(g)$KE_PD[V(g)$KE_KED=="AO"]<-"terminus"
  V(g)$KE_PD[degree(g, mode="out")==0]<-"terminus"
  return(g)
}
