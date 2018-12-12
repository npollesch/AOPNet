#' topo.layout
#'
#' Computes a topological ordering layout for visualization of an igraph object, as long as the igraph object is acyclic
#' @param g An igraph object with no strongly connected components [igraph object]
#' @return Layout coordinates for plotting a topologically sorted network (can be used for "layout" argument of the plot function) [array]
#' @family AOPNet Operations
#' @export

###################################################################
## FUNCTION: topo.layout
## Topological Sorting
###################################################################
#
### Description: AOPN.topo.layout function computes a topological ordering layout for visualizaiton of an igraph object
#
### Input: iGrapgh object with no strongly connected components
#
### Output: layout coordinates for plotting a topologically sorted network (can be used for "layout" argument of the plot function)


topo.layout<- function(g){
  tsort<-topo_sort(g, mode = c("out"))
  tpos<-match(V(g)$name,tsort$name) # finds position of node names in topoloigcal ordering
  tlay<-cbind(tpos,tpos) #define a (x,y) plot layout that orders accoring to topological order
  return(tlay) # return layout coordinates
}
