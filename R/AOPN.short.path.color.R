#' AOPN.short.path.color
#'
#' Provides colors for all edges in a igraph object, where the shortest path is colored, all other edges are transparent
#' @param g [igraph object]
#' @param fromnode The first KE in the path [vertex of igraph object]
#' @param tonode The final KE in the path [vertex of igraph object]
#' @param loc Leave originial color? TRUE leaves as orginial color, Default=FALSE [boolean]
#' @param clr Specified color for path, default="purple" [R color]
#' @param nonclr Non-path color, default="transparent" [R Color]
#' @param weight Edge weights for shortest path algorithm, default=NA [vector]
#' @param all Color all shortest paths or just the first detected? TRUE colors all, default=TRUE [boolean]
#' @return A vector of colors for edges [vector]
#' @family Path Operations
#' @export

###################################################################
## FUNCTION: AOPN.short.path.color
## edge color for all shortest paths between two nodes
###################################################################
#
### Description: provides colors for all edges in a igraph object, where the shortest path is colored, all other edges are transparent
#
### Input: iGrapgh object, from node, to node, LOC (leave original color), path color (default is purple), non-path color (default is "transparent") edge weights, all (color all shortets paths, or only 1/first))
#
### Output: colors to use for edges, where shortest paths are colored as indicated by user

AOPN.short.path.color<-function(g,fromnode,tonode,loc=F,clr="purple", nonclr="transparent" , weight=NA , all=T){
  if(nonclr=="transparent"){nonC<-rgb(1,1,1, alpha=0)}else{nonC<-nonclr}
  paths<-all_shortest_paths(g,from=fromnode,to=tonode,mode="out",weights=weight)
  if(all){
    if(length(paths)==0){return("No simple paths between nodes")}
    else{
      if(loc){
        for(i in 1:length(paths[[1]])){
          E(g,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(which(!is.na(E(g)$clrs)))}
      else{
        E(g)$clrs<-nonC
        for(i in 1:length(paths[[1]])){
          E(g,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(E(g)$clrs)}
    }
  }
  else{
    if(length(paths)==0){return("No simple paths between nodes")}
    else{
      if(loc){
        for(i in 1:1){
          E(g,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(which(!is.na(E(g)$clrs)))}
      else{
        E(g)$clrs<-nonC
        for(i in 1:1){
          E(g,path=paths[[1]][[i]],dir=T)$clrs<-clr}
        return(E(g)$clrs)}
    }
  }
}
