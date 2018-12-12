#' edges.from.path
#'
#' Creates an edge list from input path
#' @param x A vector that lists the sequence of vertices in a path (from start to end if a directed path)  [vector]
#' @param by.vertex.name Optional (Default=TRUE) [boolean]
#' @return a 2-column matrix, each row representing the start and end vetices of an edge [array]
#' @family Path Operations
#' @export

##############################################
## FUNCTION: edges.from.path
## CREATE EDGE LIST FROM PATHS
##############################################

## INPUT  is a vector that lists the sequence of vertices in a path (from start to end if a directed path)
## OUTPUT is a 2-column matrix, each row representing the start and end vetices of an edge

edges.from.path<-function(x, by.vertex.name=TRUE){
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
