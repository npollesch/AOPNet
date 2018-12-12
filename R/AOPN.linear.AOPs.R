#' AOPN.linear.AOPs
#'
#' Determines all linear AOPs (ie simple paths) between all
#' possible MIE and AO pairs (or "ORIGIN" and "TERMINUS" if desired)
#' @param g AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO"), must also have "names" attribute [igraph object]
#' @param use_KE_PD Optional (Default=FALSE): Can use KE_PD ("origin" and "terminus") instead of KE_KED [boolean]
#' @return OUTPUT is a "List of a List" of all paths (by vertex) between all MIE/AO pairs (or origin/terminus pairs)
#'   List objects are named by the MIE/AO pair involved
#'   List objects are Lists of all paths (by vertex) between the MIE and AO for which the List is named [list]
#' @family AOPNet Operations
#' @export

####################################################################
## FUNCTION: AOPN.linear.AOPs
## Determines all linear AOPs (ie simple paths) between all
## possible MIE and AO pairs (or "ORIGIN" and "TERMINUS" if desired)
####################################################################

### INPUT must be an igraph object
#   all vertices must have an attributes called "KE_KED" (Key Event Designator), with values of "MIE", "KE", or "AO"
#   all vertices must have a "names" attribute (this is used to identify vertices)
#   optionally, if use_KE_PD=TRUE, then all vertices must have an attributes called "KE_PD" (Path Designator), with values of "origin", "", or "terminus"

### OUTPUT is a "List of a List" of all paths (by vertex) between all MIE/AO pairs (or origin/terminus pairs)
#   List objects are named by the MIE/AO pair involved
#   List objects are Lists of all paths (by vertex) between the MIE and AO for which the List is named
#   if "remove.zero.paths=TRUE", all origin/terminus pairs with ZERO linear AOPs are removed from the summary, if FALSE they are left in

# takes about 3 mins to run on my computer for the full wiki

AOPN.linear.AOPs<- function(g, use_KE_PD=FALSE){

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

