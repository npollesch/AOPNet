#' add.KE_LAOC
#'
#' Counts the number of times a KE occurs in different linear AOP paths
#' @param g AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO") [igraph object]
#' @param use_KE_PD Optional (Default=FALSE): Can use KE_PD ("origin" and "terminus") instead of KE_KED [boolean]
#' @return Identical igraph object with new vertex attribute called KE_LAOC (linear AOP Occurence) [igraph object]
#' @family KE Attribution
#' @export

###################################################################
## FUNCTION: add.KE_LAOC
## adds linear AOP occurence counts
###################################################################
#
### Description: counts the number of times a KE occurs in different linear AOP paths
#
### INPUT: AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO")
#   optional: can use KE_PD ("origin" and "terminus") instead of KE_KED
#
### OUTPUT: identical igraph object with new vertex attribute called KE_LAOC (linear AOP Occurence)

add.KE_LAOC<-function(g, use_KE_PD=FALSE){
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
