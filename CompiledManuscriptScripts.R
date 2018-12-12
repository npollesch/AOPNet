#### Initialize ####

#Load packages
library(xml2)
library(igraph)
library(prodlim)
library(RColorBrewer)
library(plotrix)
library(autoimage)

#Set working directory
workingDir<-paste(getwd(),'/R/',sep="")

#Load source of functions
source(paste(workingDir,"AOP-Net-Functions.R",sep=""))    #imports Jason's custom functions




#### AOP-Net-Script-1-Parse XML.R ####

#Provide AOP-Wiki XML file location
fName<-"data/aop-wiki-xml-2018-10-01.xml"

xData<-read_xml(fName)
xData<-xml_ns_strip(xData)

### Ref ID to AOPwiki ID

keID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

kerID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

aopID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)


### Key event (KE) Data

keData<-data.frame(
  ID=keID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event"), "id"),keID$ref)],
  title=xml_text(xml_find_all(xData, "/data/key-event/title")),
  LOBO=xml_text(xml_find_all(xData, "/data/key-event/biological-organization-level")),
  stringsAsFactors=FALSE
)


### Key event relationship (KER) Data

kerData<-data.frame(
  ID=kerID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event-relationship"), "id"),kerID$ref)],
  KEup=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/upstream-id")),keID$ref)],
  KEdown=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/downstream-id")),keID$ref)],
  stringsAsFactors=FALSE
)


### AOP data

# OECD status: not all aops have an "oecd-status" xml tag, so must us "if" to return NA when missing
oecdStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("oecd-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"oecd-status")))
  }else{
    return("not specified")
  }
})

# SAAOP status: not all aops have an "saaop-status" xml tag, so must us "if" to return NA when missing
saaopStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("saaop-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"saaop-status")))
  }else{
    return("not specified")
  }
})

# MIEs: more than one MIE possible per aop, so must return list
mies<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("molecular-initiating-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})

# AOs: more than one AO possible per aop, so must return list
aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("adverse-outcome"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})


# KEs: more than one KE possible per aop, so must return list
kes<-lapply(xml_find_all(xData, "/data/aop/key-events"),FUN=function(x){
  if("key-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])
  }else{
    return(NULL)
  }
})

# KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
# So must return data frame of KERs
kers<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
  if("relationship"%in%xml_name(xml_children(x))){
    return(data.frame(
      ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
      adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
      quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
      woe=xml_text(xml_find_all(x, "relationship/evidence")),
      stringsAsFactors=FALSE
    ))
  }else{
    return(NULL)
  }
})

# add kes and MIE/AO designation (which is AOP-specific) for each KER in kers data.frame
for(i in 1:length(kers)){
  if(length(kers[[i]])>0){
    KEup<-kerData$KEup[match(kers[[i]]$ID,kerData$ID)]
    KEDup<-sapply(KEup, FUN=function(x){
      if(x%in%mies[[i]]){
        return("MIE")
      }else{
        if(x%in%aos[[i]]){
          return("AO")
        }else{
          return("KE")
        }
      }
    })

    KEdown<-kerData$KEdown[match(kers[[i]]$ID,kerData$ID)]
    KEDdown<-sapply(KEdown, FUN=function(x){
      if(x%in%mies[[i]]){
        return("MIE")
      }else{
        if(x%in%aos[[i]]){
          return("AO")
        }else{
          return("KE")
        }
      }
    })

    kers[[i]]<-data.frame(
      ID=kers[[i]]$ID,
      KEup=KEup,
      KEDup=KEDup,
      KEdown=KEdown,
      KEDdown=KEDdown,
      adjacency=kers[[i]]$adjacency,
      quant=kers[[i]]$quant,
      woe=kers[[i]]$woe,
      row.names=NULL,
      stringsAsFactors = FALSE
    )
  }
}


aopData<-data.frame(
  ID=aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"),aopID$ref)],
  oecdStatus=oecdStatus,
  saaopStatus=saaopStatus,
  mies=I(mies),
  aos=I(aos),
  kes=I(kes),
  kers=I(kers),
  stringsAsFactors=FALSE
)




#### AOP-Net-Script 2-Build Network ####

### NOTE
### AOP Wiki data is from parsed XML file downloaded from aopwiki.org
### MUST run "AOP-XML Parse.R" script to create the following data frames:
###   1) keData
###   2) kerData
###   1) aopData


### Currate AOPs that can be used in network

# total number of AOPs
nrow(aopData) # 219 total AOPs

# remove all "archived" aops based on SAAOP status (and not NA)
sum(aopData$saaopStatus=="Archived") # 6 archived aops
aData<-aopData[!aopData$saaopStatus=="Archived",]

# remove all "empty" aops (i.e. aops that have ZERO mies, aos, kes, and/or ZERO kers)
mieNull<-sapply(aData$mies, is.null)
aoNull<-sapply(aData$aos, is.null)
keNull<-sapply(aData$kes, is.null)
kerNull<-sapply(aData$kers, is.null)
emptyAOP<-(mieNull & aoNull & keNull) | kerNull
sum(emptyAOP) # 26 empty aops
aData<-aData[!emptyAOP,]


### Create AOP wiki igraph object

# Combine all KER tables from all AOPs
kerSet<-do.call("rbind", aData$kers)

# Create edge list from unique "KEup" and "KEdown" pairs
edgeList<-unique(kerSet[,c("ID","KEup","KEdown")])

# Create iGraph object from edgeList (edgelist must be a matrix)
g1<-graph_from_edgelist(as.matrix(edgeList[,c("KEup","KEdown")]), directed=TRUE)



### Summarize number of AOPs, KEs, and KERs that were used in the network

# aops
nrow(aData) # 187 AOPs in network

#kes
allKEs<-unique(c(unique(do.call(c, aData$mies)), unique(do.call(c, aData$aos)), unique(do.call(c, aData$kes))))
length(allKEs) # 840 total unique KEs in aData (mie, ke and ao), however, some are not associated with any KER and could not be included in the network
sum(!allKEs%in%V(g1)$name) # 15 KEs not icluded in network
length(V(g1)) #825 KEs total in network

#kers
nrow(edgeList) # 1050 KERs

# number of KEs and KERs per AOP
KEperAOP<-vector()
KERperAOP<-vector()
for(i in 1:nrow(aData)){
  KEperAOP<-c(KEperAOP,length(unique(c(aData[i,]$mies[[1]], aData[i,]$aos[[1]], aData[i,]$kes[[1]]))))
  if(is.null(aData[i,]$kers[[1]])){
    KERperAOP<-c(KERperAOP, 0)
  }else{
    KERperAOP<-c(KERperAOP, nrow(aData[i,]$kers[[1]]))
  }
}

mean(KEperAOP) # 7.1 KEs per AOP
median(KEperAOP) # 7
min(KEperAOP) # 1
max(KEperAOP) # 29

mean(KERperAOP) # 6.5 KERs per AOP
median(KERperAOP) # 6
min(KERperAOP) # 0
max(KERperAOP) # 26

### Map and summarize vertex (KE) attributes

# add "ID" attribute (is same as "name")
V(g1)$ID<-V(g1)$name

# not all KEs from "keData" are included in network (many of them are not connected to any KERs yet). How many are in network
sum(keData$ID%in%V(g1)$ID) # only 825 out of 840 KEs are in the network

# Add KE_KED ("MIE", "KE", or "AO") based on "once an MIE/AO always an MIE/AO"

# combine ke/ked data form each aop into one table
kedDat<-do.call("rbind", aData$kers)

# combine all unique KE/ KEDs  into a single two column table
kedDat<-data.frame(KE=c(kedDat$KEup,kedDat$KEdown), KED=c(kedDat$KEDup, kedDat$KEDdown), stringsAsFactors=FALSE)
kedDat<-unique(kedDat[order(as.numeric(kedDat$KE)),])

#assign KED to V(g1) based on table ("once an MIE/AO always an MIE/AO")
V(g1)$KE_KED<-sapply(V(g1)$ID, FUN=function(x){
  if("MIE"%in%kedDat$KED[kedDat$KE==x]){
    return("MIE")
  }else{
    if("AO"%in%kedDat$KED[kedDat$KE==x]){
      return("AO")
    }else{
      return("KE")
    }
  }
})

sum(V(g1)$KE_KED=="MIE") # 126 MIEs
sum(V(g1)$KE_KED=="KE") # 586 KEs
sum(V(g1)$KE_KED=="AO") # 113 AOs


# Add KE_PD (origin or terminus)
g1<-add_KE_PD(g1)
sum(V(g1)$KE_PD=="origin") # 155 origin
sum(V(g1)$KE_PD=="origin" & V(g1)$KE_KED!="MIE") # 29 non MIE origins
sum(V(g1)$KE_PD=="terminus") # 136 terminus
sum(V(g1)$KE_PD=="terminus" & V(g1)$KE_KED!="AO") # 23 non AO termini


# add colour attribute (MIE=green, KE=white, AOP=red)
V(g1)$col<-"white"
V(g1)$col[V(g1)$KE_KED=="MIE"]<-"green"
V(g1)$col[V(g1)$KE_KED=="AO"]<-"red"


# Add AOP IDs. Note: Each KE may belong to more than one AOP, so the AOP_IDs object is a list not just a single AOP ID
V(g1)$AOP_ID<-list(vector())
for(i in 1:nrow(aData)){
  keAll<-c(aData$mies[[i]], aData$aos[[i]], aData$kes[[i]])
  if(length(keAll)>0){
    for(j in keAll){
      if(j%in%V(g1)$ID){
        V(g1)$AOP_ID[[match(j,V(g1)$ID)]]<-c(V(g1)$AOP_ID[[match(j,V(g1)$ID)]],aData$ID[i])
      }
    }
  }
}

# number of unique AOP IDs included in network
any(is.na(unlist(V(g1)$AOP_ID))) # FALSE (i.e.no KEs with no aop-id)
inAops<-unique(unlist(V(g1)$AOP_ID))
length(inAops) # 187 AOPs

# number of AOP IDs per KE
KEnumIDs<-sapply(V(g1)$AOP_ID, FUN=function(x) length(x))
mean(KEnumIDs) # 1.6
median(KEnumIDs) # 1
max(KEnumIDs) # 21


# map level of biological organization (LOBO)
V(g1)$LOBO<-keData$LOBO[match(V(g1)$ID, keData$ID)]
loboSum<-data.frame(
  LOBO=c("Molecular", "Cellular", "Tissue", "Organ", "Individual", "Population"),
  counts=c(
    sum(V(g1)$LOBO=="Molecular"),
    sum(V(g1)$LOBO=="Cellular"),
    sum(V(g1)$LOBO=="Tissue"),
    sum(V(g1)$LOBO=="Organ"),
    sum(V(g1)$LOBO=="Individual"),
    sum(V(g1)$LOBO=="Population")
  ),
  stringsAsFactors=FALSE
)
#         LOBO counts
# 1  Molecular    226
# 2   Cellular    270
# 3     Tissue    138
# 4      Organ     78
# 5 Individual     88
# 6 Population     25


# map titles as KE attribute
V(g1)$title<-keData$title[match(V(g1)$ID, keData$ID)]



### Map and summarize edge (KER) attributes

# add KER ID as edge attribute
E(g1)$ID<-edgeList$ID

# Add AOP IDs. Note: Each KER may belong to more than one AOP, so the AOP_IDs object is a list not just a single AOP ID
E(g1)$AOP_ID<-list(vector())
for(i in 1:nrow(aData)){
  if(!is.null(aData$kers[[i]])){
    for(j in aData$kers[[i]]$ID){
      if(j%in%E(g1)$ID){
        E(g1)$AOP_ID[[match(j,E(g1)$ID)]]<-c(E(g1)$AOP_ID[[match(j,E(g1)$ID)]],aData$ID[i])
      }
    }
  }
}

# number of AOP IDs in KERs
length(unique(unlist(E(g1)$AOP_ID))) # 187

# number of AOP IDs per KE
KERnumIDs<-sapply(E(g1)$AOP_ID, FUN=function(x) length(unlist(x)))
mean(KERnumIDs) # 1.3
median(KERnumIDs) # 1
max(KERnumIDs) # 13


# WoE and quatitative understanding

# currently WOE and Quant are AOP-SPECIFIC
# For this study, we will use the LOWEST WOE and Quant assigned to a KER, if it has multiple values

#woe
woeList<-unique(kerSet[,c("ID","KEup","KEdown", "woe")])
dupIDs<-unique(woeList$ID[duplicated(woeList$ID)])
w<-vector()
for(i in dupIDs){
  kerW<-woeList$woe[woeList$ID==i]
  if("Low"%in%kerW){
    w<-c(w, "Low")
  }else{
    if("Moderate"%in%kerW){
      w<-c(w, "Moderate")
    }else{
      if("High"%in%kerW){
        w<-c(w,"High")
      }else{
        w<-c(w,"Not Specified")
      }
    }
  }
}
# remove duplicates
woeList<-woeList[!duplicated(woeList$ID),]
# add woes for duplicates
woeList$woe[match(dupIDs, woeList$ID)]<-w
# assign as edge attribute
E(g1)$woe[match(woeList$ID, E(g1)$ID)]<-woeList$woe

#convert to numeric score
wScores<-data.frame(w=c("High","Moderate","Low","Not Specified"), score=c(1, 2, 3, 3))
E(g1)$woe_score<-wScores$score[match(E(g1)$woe, wScores$w)]
mean(E(g1)$woe_score) # 2.1


# quant
quantList<-unique(kerSet[,c("ID","KEup","KEdown", "quant")])
dupIDs<-unique(quantList$ID[duplicated(quantList$ID)])
q<-vector()
for(i in dupIDs){
  kerQ<-quantList$quant[quantList$ID==i]
  if("Low"%in%kerQ){
    q<-c(q, "Low")
  }else{
    if("Moderate"%in%kerQ){
      q<-c(q, "Moderate")
    }else{
      if("High"%in%kerQ){
        q<-c(q,"High")
      }else{
        q<-c(q,"Not Specified")
      }
    }
  }
}
# remove duplicates
quantList<-quantList[!duplicated(quantList$ID),]
# add quants for duplicates
quantList$quant[match(dupIDs, quantList$ID)]<-q
# assign as edge attribute
E(g1)$quant[match(quantList$ID, E(g1)$ID)]<-quantList$quant

#convert to numeric score
qScores<-data.frame(w=c("High","Moderate","Low","Not Specified"), score=c(1, 2, 3, 3))
E(g1)$quant_score<-wScores$score[match(E(g1)$quant, wScores$w)]
mean(E(g1)$quant_score)# 2.7




#### AOP-Net-Script 3-Adjacent vs NonAdjacent ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data


### User Defined Adj vs NonAdj

#instances where KER is ALWAYS defined by users as direct (adjacent)
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
allAdj<-vector()
for(i in unique(kAdj$ID)){
  tf<-all(kAdj$adjacency[kAdj$ID==i]=="adjacent")
  allAdj<-c(allAdj,tf)
}
sum(allAdj)
# 859 instances where KER is always "adjacent"

#instances where KER is ALWAYS defined by users indirect (non adjacent)
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
allNon<-vector()
for(i in unique(kAdj$ID)){
  tf<-all(kAdj$adjacency[kAdj$ID==i]=="non-adjacent")
  allNon<-c(allNon,tf)
}
sum(allNon)
#180 instances where KER is always "non-adjacent"

#instances where KER adjacency is different across multiple AOPs
kAdj<-unique(do.call("rbind", aData$kers)[,c("ID","adjacency")])
isDiff<-vector()
for(i in unique(kAdj$ID)){
  tf<-!all(kAdj$adjacency[kAdj$ID==i]==kAdj$adjacency[kAdj$ID==i][1])
  isDiff<-c(isDiff,tf)
}
sum(isDiff)
#11 instances where KER adjacency changes across AOPs


### Algorithm Based Identification of Adj vs NonAdj using add_KER_adjacency() function

g1<-add_KER_adjacency(g1) # may take a few mins

# number adjacent and non-adjacent, based on algorithm
sum(E(g1)$adjacency=="adjacent")
sum(E(g1)$adjacency=="non-adjacent")
# 943 adjacent, 107 non-adjacent


# Subgraph with only adjacent edges
g1_adj<-subgraph.edges(g1, eids=E(g1)[E(g1)$adjacency=="adjacent"] )




#### AOP-Net-Script 4-Components ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network


### Weak Components in Full Network

# weak components
g1_wComps<-components(g1, mode="weak") #35 weak components
mean(g1_wComps$csize) # 23.6
median(g1_wComps$csize) # 7
min(g1_wComps$csize) # 2
max(g1_wComps$csize) # 578

#not including largest
max(g1_wComps$csize[2:35]) #17

# assign the attribute wcc to nodes based on their membership
V(g1)$wcc<-g1_wComps$membership


### Strong Components in Full Network

# strong components
g1_sComps<-components(g1, mode="strong")
ntcomps<-which(g1_sComps$csize>1) # non-trivial ccs (i.e. with more than 1 node)
length(ntcomps) # 4 non-trivial strong components
g1_sComps$csize[ntcomps] # with size = 2, 4, 7, 4 KEs each

# assign the attribute scc to nodes based on their membership
V(g1)$scc<-g1_sComps$membership

# show KEs and AOPs in each component
for(i in ntcomps){
  print(i)
  print(V(g1)[V(g1)$scc==i])
  print(V(g1)$AOP_ID[V(g1)$scc==i])
}



### Contract Strong Components to form "Contracted Network"

#contract strong components
g1_contr<-contract.scc(g1)



### Weak Components of adjacent-KER-only network

# weak components
g1_adj_wComps<-components(g1_adj, mode="weak")
mean(g1_adj_wComps$csize) # 23.6
median(g1_adj_wComps$csize) # 7
max(g1_adj_wComps$csize) # 578
# 35 weak components when adjacent KERs, all #'ssame as full network

#assign wComp membership
V(g1_adj)$wcc<-g1_adj_wComps$membership


# strong components- AOPg_adj
g1_adj_sComps<-components(g1_adj, mode="strong")
ntcomps_adj<-which(g1_adj_sComps$csize>1)
length(ntcomps_adj)
g1_adj_sComps$csize[ntcomps_adj]
# for nt strong comps, with sizes 2, 4, 7, 4 (did not change compared to g1)

#assign sComp membership
V(g1_adj)$scc<-g1_adj_sComps$membership

# compare scc membership between g1 and g1_adj
all(sapply(1:length(ntcomps), FUN=function(x) all(V(g1)[V(g1)$scc==ntcomps[x]]==V(g1_adj)[V(g1_adj)$scc==ntcomps_adj[x]])))
# TRUE i.e. components have the same membership in g1 and g1_adj

# compare edges in components for g1 and g1_adj (by plot)
par(mar=c(0,0,0,0), mfrow=c(length(ntcomps),2))
cList<-list()
cList_adj<-list()
for(i in 1:length(ntcomps)){
  cList[[i]]<-induced.subgraph(g1, vids=V(g1)[V(g1)$scc==ntcomps[i]])
  cList_adj[[i]]<-induced.subgraph(g1_adj, vids=V(g1_adj)[V(g1_adj)$scc==ntcomps_adj[i]])

  vCol<-rep("white",length(V(cList[[i]])))
  vCol[V(cList[[i]])$KE_KED=="MIE"]<-"green"
  vCol[V(cList[[i]])$KE_KED=="AO"]<-"red"
  eCol<-rep("grey50", length(E(cList[[i]])))
  eCol[E(cList[[i]])$adjacency=="non-adjacent"]<-"orange"
  eWidth<-rep(2.5,length(E(cList[[i]])))
  eWidth[E(cList[[i]])$adjacency=="non-adjacent"]<-3.5
  set.seed(1)
  plot(cList[[i]],
       vertex.size=20, vertex.color=vCol, vertex.label=NA,
       edge.width=eWidth, edge.arrow.size=0.5, edge.color=eCol)

  vCol<-rep("white",length(V(cList_adj[[i]])))
  vCol[V(cList_adj[[i]])$KE_KED=="MIE"]<-"green"
  vCol[V(cList_adj[[i]])$KE_KED=="AO"]<-"red"
  eCol<-rep("grey50", length(E(cList_adj[[i]])))
  eCol[E(cList_adj[[i]])$adjacency=="non-adjacent"]<-"orange"
  eWidth<-rep(2.5,length(E(cList_adj[[i]])))
  eWidth[E(cList_adj[[i]])$adjacency=="non-adjacent"]<-3.5
  set.seed(1)
  plot(cList_adj[[i]],
       vertex.size=20, vertex.color=vCol, vertex.label=NA,
       edge.width=eWidth, edge.arrow.size=0.5, edge.color=eCol)
}
# The second ntcomp (component #411) has one extra edge (a nonAdj)... all other strong comps are the same
# conclusion: adjacent KER have no impact on component membership

#contract strong components -AOPg_adj
g1_adj_contr<-contract.scc(g1_adj)




#### AOP-Net-Script 5-Linear AOPs ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network


### User defined linear AOPs

uLaopCount<-vector()
for(i in 1:nrow(aData)){
  if(!is.null(aData$kers[[i]])){

    # edge list for each user-define AOP
    eList<-aData$kers[[i]]

    # subgraph by edgelist
    subG<-graph_from_edgelist(as.matrix(eList[,c("KEup", "KEdown")]), directed=TRUE)

    # add KE_KED to each KE (MIE, AO, or KE)
    subKed<-data.frame(KE=c(eList$KEup,eList$KEdown), KED=c(eList$KEDup, eList$KEDdown), stringsAsFactors=FALSE)
    subKed<-unique(subKed)
    V(subG)$KE_KED<-subKed$KED[match(V(subG)$name,subKed$KE)]

    # count linear AOPs
    laops_subG<-linear.AOPs(subG)
    if(length(laops_subG)==0){
      uLaopCount<-c(uLaopCount,0)
    }else{
      uLaopCount<-c(uLaopCount,sum(sapply(laops_subG,length)))
    }
  }else{
    uLaopCount<-c(uLaopCount,0)
  }
}
sum(uLaopCount) # 471 user define linear AOPs
mean(uLaopCount) # mean = 2.3 laops per user-defined AOP
median(uLaopCount) # median = 1
max(uLaopCount)
aData$ID[uLaopCount==max(uLaopCount)] # AOP:58  has max of 32 laops

sum(uLaopCount==0) # 40 (21%)
sum(uLaopCount==1) # 63 (34%)
sum(uLaopCount==2) # 38 (20%)
sum(uLaopCount>2) # 46 (25%)

# result summary
udLaops<-data.frame(aData$ID, LAOPs=uLaopCount)


### Linear AOPs in Full Network
laops_g1<-linear.AOPs(g1, use_KE_PD=FALSE) # takes a few minutes
sum(sapply(laops_g1,length))
# 9876  laops total

laopCount<-sapply(laops_g1, length)
mean(laopCount)     # mean = 10.8 (not inlcuding MIE/AO pairs with zero)
median(laopCount)   # meadian = 3 (not inlcuding MIE/AO pairs with zero)
max(laopCount)      # max = 292 laops

length(laopCount) # 913 (number of MIE/AO pairs with LAOPs)

sum(laopCount==1) # 235
sum(laopCount==2) # 179
sum(laopCount>2) # 499
sum(laopCount>50) # 22

#in table form
splitN<-strsplit(names(laopCount), " ")
laopTable<-data.frame(
  MIE=sapply(splitN, FUN=function(x) x[1]),
  AO=sapply(splitN, FUN=function(x) x[2]),
  LAOPS=laopCount,
  stringsAsFactors=FALSE,
  row.names=NULL
)

names(laops_g1)[laopCount==max(laopCount)]
# 3 MIE/AO pairs have the max # (292) of laops
# MIE: 828 to AO:563
# MIE: 898 to AO:563
# MIE:1486 to AO:563

# create subgraphs of these MIE/AO pairs
subList<-list(laops_g1[["828 563"]], laops_g1[["898 563"]], laops_g1[["1486 563"]] )
laopSubGraphs<-list()
for(i in 1:length(subList)){
  eList<-sapply(subList[[i]], edge_from_path)
  eList<-do.call("rbind", eList)
  eList<-unique(eList)
  eList<-as.vector(t(eList))
  laopSubGraphs[[i]]<-subgraph.edges(g1, eids=E(g1, P=eList ))
}

sapply(laopSubGraphs, vcount)
sapply(laopSubGraphs, ecount)
# 2 subgraphs have 28 and 46  V and Es...are they the same?

# same KEs?
all(V(laopSubGraphs[[1]])$ID%in%V(laopSubGraphs[[3]])$ID) #FALSE

# is smaller one contained in larger ones?
all(V(laopSubGraphs[[2]])$ID%in%V(laopSubGraphs[[1]])$ID) #TRUE
all(V(laopSubGraphs[[2]])$ID%in%V(laopSubGraphs[[3]])$ID) #TRUE
# subgraph 2 is contained within 1 and 3...so only really two subgraphs with significant overlap

# combine into a single subgraph
subE<-unique(rbind(as_edgelist(laopSubGraphs[[1]]), as_edgelist(laopSubGraphs[[3]])))
subL<-subgraph.edges(g1, eids=E(g1, P=c(t(subE))))

# has strong componets?
components(subL)
# has one non-trivial strong components (#13)
V(subL)$scc<-components(subL, mode="strong")$membership

# how many laops in this single subgraph?
laops_subL<-linear.AOPs(subL)
sum(sapply(laops_subL, length))
#1785 laops in this subGraph only, nearly 20% of total



# MIE/AO pair with most laops AND no strong components
# subgraph with strong components removed
subNoS<-induced_subgraph(g1, vids= V(g1)[!V(g1)$scc%in%ntcomps])
any(components(subNoS, mode="strong")$csize>1) # FALSE (strong components removed)
laops_NoS<-linear.AOPs(subNoS)
lCountNoS<-sapply(laops_NoS, length)
max(lCountNoS) # max = 119 laops
names(laops_NoS)[lCountNoS==max(lCountNoS)]
# MIE 279 to AO 563 has 119 laops

# create subgrapg of these laops
eList<-laops_NoS[["279 563"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(subNoS, eids=E(subNoS, P=eList ))
components(sub_lNoS, mode="strong") # no strong components

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # sub graph has 348 laops (has several MIE/AO pairs)
# candidate for topo Sort :)
# verdict: too many laops! makes for sloppy graphs/figures

# MIE/AO pair with fewer laops= 167/549
# create subgrapg of these laops
eList<-laops_g1[["167 459"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(g1, eids=E(g1, P=eList ))
components(sub_lNoS, mode="strong") # no strong components

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # subgraph has 21 laops
# better for demonstrating topo sort....less crowded figures...but all woe="HIGH"!!!


# MIE/AO pair with fewer laops= 201/341
# create subgrapg of these laops
eList<-laops_g1[["201 341"]]
eList<-sapply(eList, edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
sub_lNoS<-subgraph.edges(g1, eids=E(g1, P=eList ))
components(sub_lNoS, mode="strong") # no strong components

laops_lNoS<-linear.AOPs(sub_lNoS)
sum(sapply(laops_lNoS, length)) # subgraph has 35 laops and a good mix of WOE/quant...will use for topo sort figures



#longest laop
laopLength<-lapply(laops_g1, function(x) sapply(x, length))
maxLper<-sapply(laopLength, max)  # max laop length PER MIE/AO pair
maxL<-max(maxLper)                # max laop length = 17 KEs
lpairs<-which(maxLper==maxL)
# Seven MIE/AO pairs with laops of length 17 KEs long
# 41 563
# 478 360
# 478 686
# 478 679
# 724 563
# 828 563
# 1486 563

# identify and save longest paths (for plotting later)
hasMax<-sapply(lpairs, function(x)laops_g1[[x]])
whereMax<-lapply(hasMax, function(x) which(sapply(x, function(y) length(y)==maxL)) )
longPaths<-list()
for(i in 1:length(whereMax)){
  longPaths[[names(whereMax[i])]]<-lapply(whereMax[[i]], function(x) laops_g1[[names(whereMax[i])]][[x]])
}

# save long paths as edgelist (for plotting later)
longEdges<-lapply(longPaths, function(x)
  lapply(x, function(y) edge_from_path(y))
)



### linear AOP analysis of adjacent-only graph

#number of linear AOPs
laops_g1_adj<-linear.AOPs(g1_adj)
sum(sapply(laops_g1_adj,length))
# 3097 linear AOPs when only adjacent MIE to AO paths are considered

# MIE pair with largest number of simple paths
mean(sapply(laops_g1_adj,length))     # mean = 3.4 (not inlcuding MIE/AO pairs with zero)
median(sapply(laops_g1_adj,length))   # meadian = 2 (not inlcuding MIE/AO pairs with zero)
max(sapply(laops_g1_adj,length))   # max = 37 laops

names(laops_g1_adj)[sapply(laops_g1_adj,length)==max(sapply(laops_g1_adj,length))]
# 1 MIE/AO pair with max (37) laops is MIE:559 and AO:563



### linear AOPs in contracted network

laops_g1_contr<-linear.AOPs(g1_contr)
sum(sapply(laops_g1_contr,length))
# 7260 linear AOPs in contracted network ....does not account for as many loaps as non-adj KERs

# MIE pair with largest number of simple paths
mean(sapply(laops_g1_contr,length))     # mean = 8.0 (not inlcuding MIE/AO pairs with zero)
median(sapply(laops_g1_contr,length))   # meadian = 3 (not inlcuding MIE/AO pairs with zero)
max(sapply(laops_g1_contr,length))   # max = 203 laops

names(laops_g1_contr)[sapply(laops_g1_contr,length)==max(sapply(laops_g1_contr,length))]
# 1 MIE/AO pair with max (203) laops is MIE:279 and AO:563




#### AOP-Net-Script 6-Connectivity ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network
###   5) "AOP-Net-5-Linear AOPs.R"              identifies all linear aops


### Degree in/out

V(g1)$degree_in<-degree(g1, mode="in")
V(g1)$degree_out<-degree(g1, mode="out")
V(g1)$degree_total<-degree(g1, mode="all")

max(V(g1)$degree_in) # 10
V(g1)[V(g1)$degree_in==10] # KE 351 (increased, mortality)...bummer :(

max(V(g1)$degree_out) # 10 also
V(g1)[V(g1)$degree_out==10] # 18 and 167 (Activation AhR, and Activation, LXR)

max(V(g1)$degree_total) # 15
V(g1)[V(g1)$degree_total==15] # 55 (cell injury/death)


### betweenness centrality

V(g1)$betweenness<-betweenness(g1, nobigint = FALSE, normalized=FALSE)
V(g1)$n_betweenness<-betweenness(g1, nobigint = FALSE, normalized=TRUE)

max(V(g1)$betweenness) #1967
V(g1)[V(g1)$betweenness==1967] # 1088, increased oxidative stress

max(V(g1)$n_betweenness) #1967
V(g1)[V(g1)$n_betweenness==max(V(g1)$n_betweenness)] # 1088, increased oxidative stress
#normlizing doesnt seem to make much difference



### AOP Occurence

# calculate and add LAOC (linear AOP occurence) attribute to KEs
g1<-add_KE_LAOC(g1)

mean(V(g1)$KE_LAOC)     # mean = 123
median(V(g1)$KE_LAOC)   # median = 5
max(V(g1)$KE_LAOC)      # max = 6615, KE 341: "Impaired Learing and Memory"

V(g1)$AOP_ID[V(g1)$ID=="341"]
#[1] "12" "13" "17" "48" "54" "77" "78" "87" "88" "89" "90" "99"
# KE 341 has 12 AOP-IDs
# KE 341 is part of the subgraph that has the most LAOPS
# KE 341 is an AO (sometimes KE that is upstream of several other AOs)
# downstream of MIEs 209, 828, 898, and 1486 (three of which of are in MIE/AO pairs with greatest # of laops)
# upstream of AOs 563, 566, 568 and 572 (563 is in MIE/Ao pairs with greatest laops)


### AOP edge connectivity

# full network (takes a few minutes)
edgeCon_g1<-aop_connectivity(g1)
mean(edgeCon_g1$edgeCon)            # mean = 1.08
median(edgeCon_g1$edgeCon)          # median = 1
max(edgeCon_g1$edgeCon)             # max = 5
edgeCon_g1[edgeCon_g1$edgeCon==5,]  # MIE/AO pair 167/459 have edgeCon =5  (part of liver steatosis subGraph)


# Table of LAOPs and Edge connectivity for all MIE/AO pairs
laop_vs_edgeCon<-data.frame(
  MIE=laopTable$MIE,
  AO=laopTable$AO,
  LAOPS=laopTable$LAOPS,
  edgeCon=edgeCon_g1$edgeCon[row.match(edgeCon_g1[,1:2], laopTable[,1:2])],
  stringsAsFactors=FALSE
)


# example of high edgeCon sub graph (MIE:167 AO:459 have 18 LAOPs and edgeCon=5), for plotting later
eList<-sapply(laops_g1[["167 459"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
hi_E<-subgraph.edges(g1, eids=E(g1, P=eList ))

# example of low edgeCon sub graph (MIE:1486 AO:566 have 80 LAOPS and edgeCon=1), for plotting later
eList<-sapply(laops_g1[["1486 566"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
low_E<-subgraph.edges(g1, eids=E(g1, P=eList ))


# adjacent KERs onyl
edgeCon_g1_adj<-aop_connectivity(g1_adj)
mean(edgeCon_g1_adj$edgeCon)            # mean = 1.02
median(edgeCon_g1_adj$edgeCon)          # median = 1
max(edgeCon_g1_adj$edgeCon)             # max = 3
edgeCon_g1_adj[edgeCon_g1_adj$edgeCon==3,]
# Three MIE/AO pairs with max adjacent edgeCon
#  MIE   AO EdgeCon
#   18  455       3   part of liver steatosis network
#   79  675       3   reduced reporductive success from cyclooxygenase inhibition network
# 1317 1344       3   selective serotonin reuptake inhibitors network

sum(edgeCon_g1$edgeCon==1)
# 859 of the possible 913 MIE/AO pairs (94%) had edgeCon=1





#### AOP-Net-Script 7-Plots and Figures ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network
###   5) "AOP-Net-5-Linear AOPs.R"              identifies all linear aops
###   6) "AOP-Net-6-Connectivity.R"             AOP Occurence and edge connectivity


### HISTOGRAMS

# Histogram of KEs and KERs per AOP
par(mar=c(4,4,1,1), mfrow=c(2,1))
hist(KEperAOP, breaks=seq(0, 30, 1), main=NULL, xlab="KEs per AOP", col="grey", xaxt="n")
axis(1, at=seq(0,30, 1)-0.5, labels=seq(0, 30, 1))
mtext("A", 2, outer=TRUE, line=-0.75, adj=0, padj=-10, cex=2, las=1 )
hist(KERperAOP, breaks=seq(0, 30, 1), main=NULL, xlab="KERs per AOP", col= "grey", xaxt="n")
axis(1, at=seq(0,30, 1)-0.5, labels=seq(0, 30, 1))
mtext("B", 2, outer=TRUE, line=-0.75, adj=0, padj=2,  cex=2, las=1)
reset.par()


# Histogram of AOP IDs per KE and KER
par(mar=c(4,4,1,1), mfrow=c(2,1))
hist(KEnumIDs, breaks=seq(0, 22, 1), main=NULL, xlab="AOP-IDs per KE", col="grey", xaxt="n")
axis(1, at=seq(0,22, 1)-0.5, labels=seq(0, 22, 1))
mtext("A", 2, outer=TRUE, line=-0.75, adj=0, padj=-10, cex=2, las=1 )
hist(KERnumIDs, breaks=seq(0, 22, 1), main=NULL, xlab="AOP-IDs per KER", col= "grey", xaxt="n")
axis(1, at=seq(0,22, 1)-0.5, labels=seq(0, 22, 1))
mtext("B", 2, outer=TRUE, line=-0.75, adj=0, padj=2,  cex=2, las=1)
reset.par()


# Histogram of linear aops per user-defined AOP
hist(uLaopCount, breaks=-1:32, main=NULL, xlab="linear AOPs per user-defined AOP", col="grey", xaxt="n")
axis(1, at=seq(0,32, 1)-0.5, labels=seq(0, 32, 1))


# Histogram of linear aops per MIE/AO pair (broken in <50, and >50)
par(mar=c(4,4,1,1), mfrow=c(2,1))
hist(laopCount[laopCount<50], breaks=seq(0,50,1), main=NULL, xlab=NULL, col="grey")
title("A) LAOPs < 50", adj=0)
title(xlab="LAOPs per MIE/AO pair", line=2)
hist(laopCount[laopCount>50],breaks=seq(50,300,10), main=NULL, xlab=NULL, col="grey")
title("B) LAOPs > 50", adj=0)
title(xlab="LAOPs per MIE/AO pair", line=2)
reset.par()


# Histogram of AOP occurrence
hist(log(V(g1)$KE_LAOC), main=NULL, xlab="log(AOP Occurence)", col="grey")


# Histogram of Edge Connectivity
par(mar=c(5,8,2,2))
ecHist<-hist(edgeCon_g1$edgeCon, breaks=0:5, plot=FALSE)
gap.barplot(y=ecHist$counts, gap=c(51,831), xlim=c(0,6), col=rep("grey",length(ecHist$counts)),
            yaxlab=c(seq(0,50,10),seq(840,860,10)), ytics=c(seq(0,50,10),seq(840,860,10)),
            xlab="Edge Connectivity of MIE/AO Pairs", ylab="frequency", bty="n")
reset.par()


### NETWORK PLOTS

# Generate plot coordinates and map to vertices so same layout can always be used for all network graph figures
# NOTE: these plot coordinates will not be assinged to any subgraphs that were created before these coordinates were generated
# In order to preserve the plot coordinates between the "master graph" and the subgraphs, all subgraphs should be re-created after these coordinates are generated
set.seed(1)
toPlot.layout<-layout.fruchterman.reingold(g1, weight=rep(0.375,ecount(g1)))
V(g1)$plotX<-toPlot.layout[,1]
V(g1)$plotY<-toPlot.layout[,2]



### plot: aopWiki network with MIEs and AOs indicated

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY) #defining before plot to ensure same coordinates are always used
par(mar=c(0,0,0,0))

vCol<-rep("white",length(V(toPlot)))
vCol[V(toPlot)$KE_KED=="MIE"]<-"green"
vCol[V(toPlot)$KE_KED=="AO"]<-"red"
eCol<-rep("grey50", length(E(toPlot)))

plot(toPlot, vertex.size=3.5, vertex.color=vCol, vertex.label=NA,
     edge.width=2.5, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
reset.par()



### plot: aopWiki network with LOBO indicated

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

loboPal<-rainbow(length(unique(V(toPlot)$LOBO)), s=0.6, v=0.9)
loboCol<-loboPal[match(V(toPlot)$LOBO, unique(V(toPlot)$LOBO))]
eCol<-rep("grey50", length(E(toPlot)))

plot(toPlot, vertex.size=3.5, vertex.color=loboCol, vertex.label=NA,
     edge.width=2.5, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
reset.par()



### plot: bar graph of LOBO

toPlot<-g1

plt<-barplot(loboSum$counts, col=loboPal[match(loboSum$LOBO, unique(V(toPlot)$LOBO))], xaxt="n", ylab="number of KEs")
text(plt,par("usr")[3], srt = 60, adj=c(1.2, 1.2), xpd = TRUE, labels = loboSum$LOBO)



### plot: network with non-adjancent edges

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

eCol[E(toPlot)$adjacency=="non-adjacent"]<-"orange"
eWidth<-rep(2.5,length(E(toPlot)))
eWidth[E(toPlot)$adjacency=="non-adjacent"]<-5

plot(toPlot, vertex.size=3.5, vertex.color=vCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
reset.par()



### Plot: only adjacent edges

toPlot<-g1

plotLay<-cbind(V(toPlot_adj)$plotX,V(toPlot_adj)$plotY)
par(mar=c(0,0,0,0))

toPlot_adj<-subgraph.edges(toPlot, eids=E(toPlot)[E(toPlot)$adjacency=="adjacent"] )
vCol<-rep("white",length(V(toPlot_adj)))
vCol[V(toPlot_adj)$KE_KED=="MIE"]<-"green"
vCol[V(toPlot_adj)$KE_KED=="AO"]<-"red"
eCol<-rep("grey50", length(E(toPlot_adj)))

plot(toPlot_adj, vertex.size=3.5, vertex.color=vCol, vertex.label=NA,
     edge.width=2.5, edge.arrow.size=0.15, edge.arrow.width=3, edge.color=eCol,
     layout=plotLay)
reset.par()



### MULTI-LAYER PLOT: MIE/AO, non-adj KERs, with weak and strong components

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

#weak comps (as brown coloured regions, with dark outlines)
weakPal<-colorRampPalette(c(hsv(0.08,0.09,0.95), hsv(0.08,0.4,0.6)))(length(unique(V(toPlot)$wcc)))
weakPal[1]<-rgb(1,1,1,alpha=0)
weakColV<-weakPal[match(V(toPlot)$wcc, unique(V(toPlot)$wcc))]

uniqueWCC<-unique(V(toPlot)$wcc)[-1]

clearCol<-rgb(1,1,1,alpha=0)
borderCol<-rep(hsv(0.08,0.5,0.4),length(weakColV))

for(i in 1:length(uniqueWCC)){
  tempBorder<-borderCol
  tempBorder[which(!V(toPlot)$wcc==uniqueWCC[i])]<-clearCol
  tempCol<-weakColV
  tempCol[which(!V(toPlot)$wcc==uniqueWCC[i])]<-clearCol
  if(i==1){
    toAdd<-FALSE
  }else{
    toAdd<-TRUE
  }
  plot(toPlot, vertex.size=11, vertex.color=tempBorder, vertex.frame.color=clearCol,  vertex.label=NA,
       edge.width=2.5, edge.color=rgb(1,1,1,alpha=0), edge.arrow.size=0.15, edge.arrow.width=3,
       layout=plotLay, add=toAdd)
  plot(toPlot, vertex.size=10, vertex.color=tempCol, vertex.frame.color=clearCol,  vertex.label=NA,
       edge.width=2.5, edge.color=rgb(1,1,1,alpha=0), edge.arrow.size=0.15, edge.arrow.width=3,
       layout=plotLay, add=TRUE)
}

#strong comps (dark blue background)
V(toPlot)$scc_col<-rgb(1,1,1,alpha=0)
E(toPlot)$scc_col<-rgb(1,1,1,alpha=0)
sccPal<-hsv(0.59, 0.85, 0.95)
for(i in 1:length(ntcomps)){
  V(toPlot)$scc_col[V(toPlot)$scc==ntcomps[i]]<-sccPal
  subG<-induced_subgraph(toPlot, V(toPlot)[V(toPlot)$scc==ntcomps[i]])
  E<-as.character(as.vector(t(as_edgelist(subG))))
  E(toPlot, P=E)$scc_col<-sccPal
}

plot(toPlot, vertex.size=7, vertex.color=V(toPlot)$scc_col, vertex.frame.color=V(toPlot)$scc_col, vertex.label=NA,
     edge.width=7, edge.color=E(toPlot)$scc_col, edge.arrow.size=0,
     layout=plotLay, add=TRUE)

# mie, Ao adj on top
E(toPlot)$adj_col<-"grey32"
E(toPlot)$adj_col[E(toPlot)$adjacency=="non-adjacent"]<-hsv(0.085, 1, 0.95)
E(toPlot)$adj_width<-0.5
E(toPlot)$adj_width[E(toPlot)$adjacency=="non-adjacent"]<-2.5

plot(toPlot, vertex.size=3.5, vertex.color=V(toPlot)$col, vertex.label=NA,
     edge.width=E(toPlot)$adj_width, edge.color=E(toPlot)$adj_col, edge.arrow.size=0.15, edge.arrow.width=2,
     layout=plotLay, add=TRUE)
reset.par()



### MULTI-LAYER PLOT: showing longest laops (seven MIE/AO pairs have max laop length of 17 KEs)

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

vCol<-rep("white",length(V(toPlot)))
vCol[V(toPlot)$KE_KED=="MIE"]<-"green"
vCol[V(toPlot)$KE_KED=="AO"]<-"red"
eCol<-rep("grey50", length(E(toPlot)))

# identify longest LAOPS and assign unique random colour
set.seed(1)
subPal<-sapply(1:length(lpairs)/length(lpairs), function(x) hsv(x, s=0.8, v=1))[order(runif(length(lpairs)))]
counter<-1
# plot each longest Path
for(i in 1:length(longEdges)){
  for(j in 1:length(longEdges[[i]])){
    lpSub<-subgraph.edges(toPlot, eids=E(toPlot, P=c(t(longEdges[[i]][[j]]))))
    vSubCol<-rep(rgb(1,1,1,alpha=0), length(V(toPlot)))
    vSubCol[match(V(lpSub)$ID, V(toPlot)$ID)]<-subPal[counter]
    eSubCol<-rep(rgb(1,1,1,alpha=0), length(E(toPlot)))
    eSubCol[match(E(lpSub)$ID, E(toPlot)$ID)]<-subPal[counter]

    plot(toPlot, vertex.size=13-counter, vertex.color=vSubCol, vertex.frame.color=vSubCol, vertex.label=NA,
         edge.width=11-counter, edge.color=eSubCol, edge.arrow.size=0,
         layout=plotLay, add=if(i>1|j>1){TRUE}else{FALSE})
    counter<-counter+1
  }
}

plot(toPlot, vertex.size=3.5, vertex.color=vCol, vertex.label=NA,
     edge.width=1.5, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


### MULTI-LAYER PLOT showing subgraph with MOST laops (MIE 828, 898 and 1486 to AO 563) within full network

toPlot<-g1

vCol<-rep("white",length(V(toPlot)))
vCol[V(toPlot)$KE_KED=="MIE"]<-hsv(0.3,0.35,1) # light green
vCol[V(toPlot)$KE_KED=="AO"]<-hsv(1,0.35,1) # light red
vCol[match(c("828","898","1486"), V(toPlot)$ID)]<-hsv(0.3,1, 0.9) # dark green
vCol[match(c("563"), V(toPlot)$ID)]<-hsv(1,1, 0.9) # dark red

vSize<-rep(3,  length(V(toPlot)))
vSize[match(c("828","898","1486","563"), V(toPlot)$ID)]<-5

eCol<-rep("grey50", length(E(toPlot)))

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

# only plot subgraphs #1 and #3 (since #2 in contained within them)
subs<-c(1,3)
subCol<-hsv(0.6, 1, 1)
for(i in 1:length(subs)){
  vSubCol<-rep(rgb(1,1,1,alpha=0), length(V(toPlot)))
  vSubCol[match(V(laopSubGraphs[[subs[i]]])$ID, V(toPlot)$ID)]<-subCol
  eSubCol<-rep(rgb(1,1,1,alpha=0), length(E(toPlot)))
  eSubCol[match(E(laopSubGraphs[[subs[i]]])$ID, E(toPlot)$ID)]<-subCol
  eCol[match(E(laopSubGraphs[[subs[i]]])$ID, E(toPlot)$ID)]<-"grey20"

  plot(toPlot, vertex.size=7, vertex.color=vSubCol, vertex.frame.color=vSubCol, vertex.label=NA,
       edge.width=9, edge.color=eSubCol, edge.arrow.size=0,
       layout=plotLay, add=if(i>1){TRUE}else{FALSE})
}

plot(toPlot, vertex.size=vSize, vertex.color=vCol, vertex.label=NA,
     edge.width=1.5, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()



### plot subgraph with MOST laops (MIE 828, 898 and 1486 to AO 563) ON THEIR OWN

# merge subgraphs 1  and 3
vCol<-rep("white",length(V(subL)))
vCol[V(subL)$KE_KED=="MIE"]<-hsv(0.3,0.2,1) # light green
vCol[V(subL)$KE_KED=="AO"]<-hsv(1,0.35,1) # light red
vCol[match(c("828","898","1486"), V(subL)$ID)]<-hsv(0.3,1, 0.9) # dark green
vCol[match(c("563"), V(subL)$ID)]<-hsv(1,1, 0.9) # dark red
eCol<-rep("grey50", length(E(subL)))
eCol[E(subL)$adjacency=="non-adjacent"]<-"orange"
eWidth<-rep(3, length(E(subL)))
# one strong comp (#13) is present in subL
sccPal<-"blue"
vCol[V(subL)$scc==13]<-sccPal
subG<-induced_subgraph(subL, V(subL)[V(subL)$scc==13])
eCol[match(E(subG)$ID, E(subL)$ID)]<-sccPal
eWidth[match(E(subG)$ID, E(g1)$ID)]<-4

plotLay<-piecewise.layout(subL)
par(mar=c(0,0,0,0))

plot(subL, vertex.size=7, vertex.color=vCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.5, edge.arrow.width=3, layout=plotLay)
reset.par()



### 4X4 plot of networks with Vsize proportional to degree in, out betweenness and aop occurnce

outCol<-hsv(0.5,1,1)
inCol<-hsv(0.1,1,1)
betCol<-hsv(1,1,1)
ocCol<-hsv(0.25,1,1)

sFac<-1.5

outSize<-(V(g1)$degree_out+1)*sFac
inSize<-(V(g1)$degree_in+1)*sFac
betSize<-(log(V(g1)$betweenness+1)+1)*sFac
ocSize<-(log(V(g1)$KE_LAOC+1)+1)*sFac

eCol<-rep("grey50", length(E(g1)))
eWidth<-rep(0.5,length(E(g1)))

plotLay<-cbind(V(g1)$plotX,V(g1)$plotY)
par(mar=c(0,0,0,0), mfrow=c(2,2))

plot(g1, vertex.size=outSize, vertex.color=outCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
text(-0.9,0.9,"A", cex=1.5)


plot(g1, vertex.size=inSize, vertex.color=inCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
text(-0.9,0.9,"B", cex=1.5)

plot(g1, vertex.size=betSize, vertex.color=betCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
text(-0.9,0.9,"C", cex=1.5)

plot(g1, vertex.size=ocSize, vertex.color=ocCol, vertex.label=NA,
     edge.width=eWidth, edge.color=eCol, edge.arrow.size=0.15, edge.arrow.width=3,
     layout=plotLay)
text(-0.9,0.9,"D", cex=1.5)
reset.par()



### Plot example of low edgeConnectivity: MIE/AO = 1486/566

#create subgraph
eList<-sapply(laops_g1[["1486 566"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
low_E<-subgraph.edges(g1, eids=E(g1, P=eList ))

#plot
toPlot<-low_E

set.seed(2)
plotLay<-layout.fruchterman.reingold(toPlot, weight=rep(0.5,ecount(toPlot)))
par(mar=c(0,0,0,0))

vCol<-rep("white",length(V(toPlot)))
vCol[V(toPlot)$ID=="1486"]<-"green"
vCol[V(toPlot)$ID=="566"]<-"red"
eCol<-rep("grey50", length(E(toPlot)))
eCol[E(toPlot)$adjacency=="non-adjacent"]<-"orange"

plot(toPlot, vertex.size=15, vertex.color=vCol, vertex.label=NA,
     edge.width=2, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay)
reset.par()


# Plot example of high edgeConnectivity: MIE/AO = 167/459

eList<-sapply(laops_g1[["167 459"]], edge_from_path)
eList<-do.call("rbind", eList)
eList<-unique(eList)
eList<-as.vector(t(eList))
hi_E<-subgraph.edges(g1, eids=E(g1, P=eList ))

toPlot<-hi_E

set.seed(13)
plotLay<-layout.fruchterman.reingold(toPlot, weight=rep(0.7,ecount(toPlot)))
par(mar=c(0,0,0,0))

vCol<-rep("white",length(V(toPlot)))
vCol[V(toPlot)$ID=="167"]<-"green"
vCol[V(toPlot)$ID=="459"]<-"red"
eCol<-rep("grey50", length(E(toPlot)))
eCol[E(toPlot)$adjacency=="non-adjacent"]<-"orange"

plot(toPlot, vertex.size=15, vertex.color=vCol, vertex.label=NA,
     edge.width=2, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay)
reset.par()



### MULTI-LAYER PLOT: MIE/AO, non-adj KERs, W+S components, radius by AOP occurence

toPlot<-g1

plotLay<-cbind(V(toPlot)$plotX,V(toPlot)$plotY)
par(mar=c(0,0,0,0))

#weak comps (as brown coloured regions, with dark outlines)
weakPal<-colorRampPalette(c(hsv(0.08,0.09,0.95), hsv(0.08,0.4,0.6)))(length(unique(V(toPlot)$wcc)))
weakPal[1]<-rgb(1,1,1,alpha=0)
weakColV<-weakPal[match(V(toPlot)$wcc, unique(V(toPlot)$wcc))]

uniqueWCC<-unique(V(toPlot)$wcc)[-1]

clearCol<-rgb(1,1,1,alpha=0)
borderCol<-rep(hsv(0.08,0.5,0.4),length(weakColV))

for(i in 1:length(uniqueWCC)){
  tempBorder<-borderCol
  tempBorder[which(!V(toPlot)$wcc==uniqueWCC[i])]<-clearCol
  tempCol<-weakColV
  tempCol[which(!V(toPlot)$wcc==uniqueWCC[i])]<-clearCol
  if(i==1){
    toAdd<-FALSE
  }else{
    toAdd<-TRUE
  }
  plot(toPlot, vertex.size=11, vertex.color=tempBorder, vertex.frame.color=clearCol,  vertex.label=NA,
       edge.width=2.5, edge.color=rgb(1,1,1,alpha=0), edge.arrow.size=0.15, edge.arrow.width=3,
       layout=plotLay, add=toAdd)
  plot(toPlot, vertex.size=10, vertex.color=tempCol, vertex.frame.color=clearCol,  vertex.label=NA,
       edge.width=2.5, edge.color=rgb(1,1,1,alpha=0), edge.arrow.size=0.15, edge.arrow.width=3,
       layout=plotLay, add=TRUE)
}

#strong comps (dark blue background)
V(toPlot)$scc_col<-rgb(1,1,1,alpha=0)
E(toPlot)$scc_col<-rgb(1,1,1,alpha=0)
sccPal<-hsv(0.59, 0.85, 0.95)
for(i in 1:length(ntcomps)){
  V(toPlot)$scc_col[V(toPlot)$scc==ntcomps[i]]<-sccPal
  subG<-induced_subgraph(toPlot, V(toPlot)[V(toPlot)$scc==ntcomps[i]])
  E<-as.character(as.vector(t(as_edgelist(subG))))
  E(toPlot, P=E)$scc_col<-sccPal
}

plot(toPlot, vertex.size=7, vertex.color=V(toPlot)$scc_col, vertex.frame.color=V(toPlot)$scc_col, vertex.label=NA,
     edge.width=7, edge.color=E(toPlot)$scc_col, edge.arrow.size=0,
     layout=plotLay, add=TRUE)

# mie, Ao adj on top
E(toPlot)$adj_col<-"grey32"
E(toPlot)$adj_col[E(toPlot)$adjacency=="non-adjacent"]<-hsv(0.085, 1, 0.95)
E(toPlot)$adj_width<-1
E(toPlot)$adj_width[E(toPlot)$adjacency=="non-adjacent"]<-2.5

# v size based on log of AOP occurence
vSize<-log(V(g1)$KE_LAOC+3)

plot(toPlot, vertex.size=vSize, vertex.color=V(toPlot)$col, vertex.label=NA,
     edge.width=E(toPlot)$adj_width, edge.color=E(toPlot)$adj_col, edge.arrow.size=0.1, edge.arrow.width=2,
     layout=plotLay, add=TRUE)
reset.par()




#### AOP-Net-Script 8-Topological Sorting ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network
###   5) "AOP-Net-5-Linear AOPs.R"              identifies all linear aops
###   6) "AOP-Net-6-Connectivity.R"             AOP occurence and edge connectivity


# Toplogical sorting of subgraph made from MIE/AO pair high number
# of laops and WITHOUT strong components (MIE/AO pair 201/341)
# subgraph called sub_lNoS created in AOP-Net-5-Linear AOPs.R script

g<-sub_lNoS


### Plot unsorted

# reusbale plot layout
set.seed(3)
layout.g<-layout_with_graphopt(g, charge=0.07)
V(g)$plotX<-layout.g[,1]
V(g)$plotY<-layout.g[,2]

# plot options
vCol<-rep("white",length(V(g)))
vCol[V(g)$KE_KED=="MIE"]<-"green"
vCol[V(g)$KE_KED=="AO"]<-"red"
eCol<-rep("grey40", length(E(g)))
eCol[E(g)$adjacency=="non-adjacent"]<-hsv(0.085, 1, 0.95)

# plot
plotLay<-cbind(V(g)$plotX,V(g)$plotY)
par(mar=c(0,0,0,0))

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay)
reset.par()



### Topo sorted plot

# topo sort and generate plot layout
topoLay<-topo.lay(g)
V(g)$topoX<-topoLay[,1]
V(g)$topoY<-topoLay[,2]

# plot
plotLay<-cbind(V(g)$topoX,V(g)$topoY)
textLay<-plotLay
textLay[,1]<-textLay[,1]+1

par(mar=c(0,0,0,0))

plot(g, vertex.size=12, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.5, edge.arrow.width=2, edge.curved=1,
     layout=plotLay)
reset.par()



### Shortest Path (regardlesss of any other attribute)

sCol<-short.path.edge.color(g,
                            fromnode=V(g)[V(g)$ID=="201"],
                            tonode=V(g)[V(g)$ID=="341"],
                            loc=F,
                            clr=hsv(0.6,0.4,1),
                            nonclr="transparent",
                            weight=NA,
                            all=T)

# unsorted
plotLay<-cbind(V(g)$plotX,V(g)$plotY)
par(mar=c(0,0,0,0))

plot(g, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0,
     layout=plotLay)

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


# sorted
plotLay<-cbind(V(g)$topoX,V(g)$topoY)
par(mar=c(0,0,0,0))

plot(g, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0), vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0, edge.curved=1,
     layout=plotLay)

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()



### Shortest path for adjacent edges only
subAdj<-subgraph.edges(g, E(g)[E(g)$adjacency=="adjacent"])

spAdj<-all_shortest_paths(subAdj, from= V(subAdj)[V(subAdj)$ID=="201"], to=V(subAdj)[V(subAdj)$ID=="341"], mode="out")
# There are 5 shortest paths of equal length

# generate different colours for each of the 5 paths
spCol<-c(hsv(0.5,0.4,0.2),
         hsv(0.5,0.4,0.4),
         hsv(0.5,0.4,0.6),
         hsv(0.5,0.4,0.8),
         hsv(0.5,0.4,1))
spSize<-c(26,23,19,16,13)


# plot unsorted
plotLay<-cbind(V(subAdj)$plotX,V(subAdj)$plotY)
par(mar=c(0,0,0,0))

for(i in 1: length(spAdj[[1]])){
  ssG<-subgraph.edges(subAdj, eids=E(subAdj, path=spAdj[[1]][[i]]))
  seCol<-rep(hsv(1,1,1,alpha=0), length(E(subAdj)))
  seCol[E(subAdj)$ID%in%E(ssG)$ID]<-spCol[i]
  plot(subAdj, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
       edge.width=spSize[i], edge.color=seCol, edge.arrow.size=0,
       layout=plotLay, add=if(i>1){TRUE}else{FALSE})
}

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


#plot  sorted
plotLay<-cbind(V(subAdj)$topoX,V(subAdj)$topoY)
par(mar=c(0,0,0,0))

for(i in 1: length(spAdj[[1]])){
  ssG<-subgraph.edges(subAdj, eids=E(subAdj, path=spAdj[[1]][[i]]))
  seCol<-rep(hsv(1,1,1,alpha=0), length(E(subAdj)))
  seCol[E(subAdj)$ID%in%E(ssG)$ID]<-spCol[i]
  plot(subAdj, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
       edge.width=spSize[i], edge.color=seCol, edge.arrow.size=0, edge.curved=1,
       layout=plotLay, add=if(i>1){TRUE}else{FALSE})
}

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()



### Shortest path analysis based on WOE
wScores<-data.frame(w=c("High","Moderate","Low","Not Specified"), score=c(1, 2, 3, 3))
wWeight<-wScores$score[match(E(g)$woe, wScores$w)]

sCol<-short.path.edge.color(g,
                            fromnode=V(g)[V(g)$ID=="201"],
                            tonode=V(g)[V(g)$ID=="341"],
                            loc=F,
                            clr=hsv(0.5,0.4,1),
                            nonclr="transparent",
                            weight=wWeight,
                            all=T)

# unsorted
plotLay<-cbind(V(g)$plotX,V(g)$plotY)
par(mar=c(0,0,0,0))

plot(g, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0,
     layout=plotLay)

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


# plot sorted
plotLay<-cbind(V(g)$topoX,V(g)$topoY)
par(mar=c(0,0,0,0))

plot(g, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0), vertex.label=NA,
     edge.width=17, edge.color=sCol, edge.arrow.size=0, edge.curved=1,
     layout=plotLay)

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()


### Shortest path analysis based on quantitave understanding ONLY
wScores<-data.frame(w=c("High","Moderate","Low","Not Specified"), score=c(1, 2, 3, 3))
qWeight<-wScores$score[match(E(g)$quant, wScores$w)]

sCol<-short.path.edge.color(g,
                            fromnode=V(g)[V(g)$ID=="201"],
                            tonode=V(g)[V(g)$ID=="341"],
                            loc=F,
                            clr=hsv(0.5,0.4,1),
                            nonclr="transparent",
                            weight=qWeight,
                            all=T)

# plot unsorted
plotLay<-cbind(V(g)$plotX,V(g)$plotY)
par(mar=c(0,0,0,0))

plot(g, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0,
     layout=plotLay)

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


# plot sorted
plotLay<-cbind(V(g)$topoX,V(g)$topoY)
par(mar=c(0,0,0,0))

plot(g, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0), vertex.label=NA,
     edge.width=12, edge.color=sCol, edge.arrow.size=0, edge.curved=1,
     layout=plotLay)

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()



### Shortest path analysis based on BOTH quantitative understanding AND adjacent KERs
subAdj<-subgraph.edges(g, E(g)[E(g)$adjacency=="adjacent"])
qWeight<-wScores$score[match(E(subAdj)$quant, wScores$w)]

sCol<-short.path.edge.color(subAdj,
                            fromnode=V(subAdj)[V(subAdj)$ID=="201"],
                            tonode=V(subAdj)[V(subAdj)$ID=="341"],
                            loc=F,
                            clr=hsv(0.25,0.4,0.7),
                            nonclr="transparent",
                            weight=qWeight,
                            all=T)

# plot unsorted
plotLay<-cbind(V(subAdj)$plotX,V(subAdj)$plotY)
par(mar=c(0,0,0,0))

plot(subAdj, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0,
     layout=plotLay)

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


# plot sorted
plotLay<-cbind(V(subAdj)$topoX,V(subAdj)$topoY)
par(mar=c(0,0,0,0))

plot(subAdj, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0), vertex.label=NA,
     edge.width=12, edge.color=sCol, edge.arrow.size=0, edge.curved=1,
     layout=plotLay)

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()
#conclusion: there are many paths with equal weight



### Shortest path analysis based on BOTH WOE AND adjacent KERs
subAdj<-subgraph.edges(g, E(g)[E(g)$adjacency=="adjacent"])
wWeight<-wScores$score[match(E(subAdj)$woe, wScores$w)]

sCol<-short.path.edge.color(subAdj,
                            fromnode=V(subAdj)[V(subAdj)$ID=="201"],
                            tonode=V(subAdj)[V(subAdj)$ID=="341"],
                            loc=F,
                            clr=hsv(0.75,0.5,1),
                            nonclr="transparent",
                            weight=wWeight,
                            all=T)

# plot unsorted
plotLay<-cbind(V(subAdj)$plotX,V(subAdj)$plotY)
par(mar=c(0,0,0,0))

plot(subAdj, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0),vertex.label=NA,
     edge.width=20, edge.color=sCol, edge.arrow.size=0,
     layout=plotLay)

plot(g, vertex.size=15, vertex.color=vCol,
     edge.width=4, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3,
     layout=plotLay, add=TRUE)
reset.par()


# plot sorted
plotLay<-cbind(V(subAdj)$topoX,V(subAdj)$topoY)
par(mar=c(0,0,0,0))

plot(subAdj, vertex.size=10, vertex.color= rgb(1,1,1,alpha=0), vertex.frame.color= rgb(1,1,1,alpha=0), vertex.label=NA,
     edge.width=17, edge.color=sCol, edge.arrow.size=0, edge.curved=1,
     layout=plotLay)

plot(g, vertex.size=10, vertex.color=vCol, vertex.label.cex=0.8,
     edge.width=3, edge.color=eCol, edge.arrow.size=0.4, edge.arrow.width=3, edge.curved=1,
     layout=plotLay, add=TRUE)
reset.par()
#conclusion: one unique shortest path with best WOE using adj KERs only



### Shortest path analysis based on BOTH WOE AND adjacent KERs
### AND NORMALIZING FOR LENGTH
subAdj<-subgraph.edges(g, E(g)[E(g)$adjacency=="adjacent"])
laopsAdj<-all_simple_paths(subAdj,
                           from=V(subAdj)[V(subAdj)$ID=="201"],
                           to=V(subAdj)[V(subAdj)$ID=="341"],
                           mode="out")
# 6 Laops

#determine average WoE for each path
wWeight<-wScores$score[match(E(subAdj)$woe, wScores$w)]
avgWoe<-vector()
for(i in 1: length(laopsAdj)){
  mW<-mean(wScores$score[match(E(subAdj, path=laopsAdj[[i]])$woe,wScores$w)])
  avgWoe<-c(avgWoe, mW)
}

# path with lowest (best) average WoE score
laopsAdj[which(avgWoe==min(avgWoe))]
# Results: same as shortest path based on un-normlaized WoE




#### AOP-Net-Scripts 9-WikiGrowth ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files
###   2) "AOP-Net-2-Build Network.R"            to create iGraph object from AOPwiki data
###   3) "AOP-Net-3-Adjacent vs NonAdjacent.R"  identifies non-adjacent KERs and creates adjacent-only network
###   4) "AOP-Net-4-Components.R"               identifies strong and weak components and created "contracted" network
###   5) "AOP-Net-5-Linear AOPs.R"              identifies all linear aops
###   6) "AOP-Net-6-Connectivity.R"             AOP occurence and edge connectivity



### Growth by AOP ID  (NOTE: MAY TAKE A FEW HOURS TO COMPLETE)

#list all AOP IDs in graph and list in order
IDs<-unlist(V(g1)$AOP_ID)
IDs<-unique(IDs)
IDs<-IDs[order(as.numeric(IDs))]

#place to store metrics
vC<-vector()        # vectex count
eC<-vector()        # edge count
mieC<-vector()      # MIE count
aoC<-vector()       # AO count
oC<-vector()        # origin count
tC<-vector()        # terminus count
adjC<-vector()      # adjacent KER count
nonC<-vector()      # non-adjacent KER count
wcC<-vector()       # weak component count
scC<-vector()       # strong component count
borrowC<-vector()   # instances a new AOP borrows a KE that is already in the network
laopC<-vector()     # number of linear AOPs
laopC_adj<-vector() # number of linear AOPs, only considering adjacent MIE to AO paths
maxLaoc<-vector()   # maximum AOP Occurence value in network


# vector to store list of vertices that are currently in the growing network
kerSet<-vector()
vSet<-vector()

# Build network from XML data and measure the network one AOP_ID at a time
for(i in 1:length(IDs)){
  print(paste(i, " of ", length(IDs), sep="")) # to track progress

  newData<-aData[match(IDs[i], aData$ID),]

  # collect KERs to add to network
  if(!is.null(newData$kers[[1]])){
    kerSet<-unique(rbind(kerSet, newData$kers[[1]][,c("KEup", "KEdown")]))
  }

  # collect KE information to add to network
  vTemp<-data.frame(
    KE=c(newData$mies[[1]], newData$aos[[1]], newData$kes[[1]]),
    KED=c( rep("MIE", length(newData$mies[[1]])), rep("AO", length(newData$aos[[1]])), rep("KE", length(newData$kes[[1]])) ),
    stringsAsFactors=FALSE)

  # number of times the new AOP uses a KE that is already in the network (borrowed a KE)
  if(length(vSet)==0){
    b<-0
  }else{
    b<-sum(vTemp$KE%in%vSet$KE)
  }
  borrowC<-c(borrowC, sum(borrowC[length(borrowC)],b))

  #add new vertices to vSet and update KED if required
  for(j in 1:nrow(vTemp)){
    if(length(vSet)==0){
      vSet<-rbind(vSet, vTemp[j,])
    }else{
      if(vTemp$KE[j]%in%vSet$KE){
        if(vTemp$KED[j]=="MIE" | vTemp$KED[j]=="AO"){
          vSet$KED[vSet$KE==vTemp$KE[j]]<-vTemp$KED[j]
        }
      }else{
        vSet<-rbind(vSet, vTemp[j,])
      }
    }
  }

  # create subG and add KED
  subG<-graph_from_edgelist(as.matrix(kerSet))
  V(subG)$KE_KED<-vSet$KED[match(V(subG)$name,vSet$KE)]

  #number of vertices
  vC<-c(vC,vcount(subG))

  #number of edges
  eC<-c(eC,ecount(subG))

  #number of MIEs and AOs
  mieC<-c(mieC, sum(V(subG)$KE_KED=="MIE"))
  aoC<-c(aoC, sum(V(subG)$KE_KED=="AO"))

  #identify and count origin and terminus
  subG<-add_KE_PD(subG)
  oC<-c(oC, sum(V(subG)$KE_PD=="origin"))
  tC<-c(tC, sum(V(subG)$KE_PD=="terminus"))

  #linear AOPs
  laops<-linear.AOPs(subG, use_KE_PD=FALSE)
  laopC<-c(laopC, sum(sapply(laops,length)))

  #identify and count adjacent and non-adjacent KERs
  subG<-add_KER_adjacency(subG)
  adjC<-c(adjC, sum(E(subG)$adjacency=="adjacent"))
  nonC<-c(nonC, sum(E(subG)$adjacency=="non-adjacent"))

  # create subgraph of only adjacent edges
  subG_adj<-subgraph.edges(subG, eids=E(subG)[E(subG)$adjacency=="adjacent"])

  #adj only linear AOPs
  laops_adj<-linear.AOPs(subG_adj, use_KE_PD=FALSE)
  laopC_adj<-c(laopC_adj, sum(sapply(laops_adj,length)))

  #components
  wcC<-c(wcC,components(subG, mode="weak")$no)
  scC<-c(scC,sum(components(subG, mode="strong")$csize>1))

  # AOP occurnece
  # NOTE: The AOP Occurnece function uses the linear.AOP() function, which is already run above, and takes a lot of time
  #       There for i've copied the part after the linear.AOP call here, rather than use the full function, in order to reduce processing time
  KEs<-V(subG)$name
  LAOC<-vector()
  for(i in KEs){
    count<-sum(sapply(laops, FUN=function(pairList){
      sum(sapply(pairList, FUN=function(pathlist) i%in%attributes(pathlist)$names))
    }))
    LAOC<-c(LAOC, count)
  }
  maxLaoc<-c(maxLaoc,max(LAOC))

}

wikiGrowth<-data.frame(
  AOP_ID=IDs,
  Vs=vC,
  Es=eC,
  MIEs=mieC,
  AOs=aoC,
  origins=oC,
  termini=tC,
  borrow=borrowC,
  adjE=adjC,
  non_adjE=nonC,
  weakC=wcC,
  strongC=scC,
  LAOPS=laopC,
  ADJ_LAOPS=laopC_adj,
  max_laoc=maxLaoc,
  stringsAsFactors=FALSE)

#write.table(wikiGrowth, paste(workingDir,"wikiGrowth_by_AOP-April 1 2018.txt", sep=""), sep="\t", row.names=FALSE)




#### AOP-Net-Script 10-User Defined AOP Field Stats ####

### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files

# Convert XML data to an R list for easier manipulation
xList<-as_list(xData)


# identify locations of KE, KER and AOP data in the list
KE_loc<-which(names(xList$data)=="key-event")
KER_loc<-which(names(xList$data)=="key-event-relationship")
AOP_loc<-which(names(xList$data)=="aop")


### Statistics on Key Event fields

# List all KE fields
keFields<-vector()
for(i in 1:length(KE_loc)){
  keFields<-c(keFields, names(xList$data[KE_loc[i]][[1]]))
  keFields<-unique(keFields)
}

keFields
# [1] "title"                                       "short-name"
# [3] "biological-organization-level"               "description"
# [5] "measurement-methodology"                     "evidence-supporting-taxonomic-applicability"
# [7] "organ-term"                                  "applicability"
# [9] "biological-events"                           "references"
# [11] "source"                                      "cell-term"
# [13] "key-event-stressors"

# Total number of KEs
numKEs<-length(KE_loc)

# KE ids
KE_ref_id<-vector()
for(i in 1:length(KE_loc)){
  KE_ref_id<-c(KE_ref_id, attr(xList$data[KE_loc[i]][[1]],"id"))
}
KE_id<-keID$ID[match(KE_ref_id,keID$ref)]


# KEs with titles
KE_title<-vector()
for(i in KE_loc){
  KE_title<-c(KE_title,length(xList$data[i][[1]]$title[[1]]))
}
nKE_title<-sum(KE_titles) # 911
pKE_title<-nKE_title/numKEs*100 # 100%


# KEs with short-name
KE_sn<-vector()
for(i in KE_loc){
  KE_sn<-c(KE_sn,length(xList$data[i][[1]]$"short-name"[[1]]))
}
nKE_sn<-sum(KE_sn) # 911
pKE_sn<-nKE_sn/numKEs*100 # 100%


# KEs with biological organization level
KE_bol<-vector()
for(i in KE_loc){
  KE_bol<-c(KE_bol,xList$data[i][[1]]$"biological-organization-level"[[1]])
}
KE_bol<-KE_bol=="Molecular"|KE_bol=="Cellular"|KE_bol=="Tissue"|KE_bol=="Organ"|KE_bol=="Individual"|KE_bol=="Population"
KE_bol<-as.numeric(KE_bol)
nKE_bol<-sum(KE_bol)
pKE_bol<-nKE_bol/numKEs*100


# KEs with description
KE_des<-vector()
for(i in KE_loc){
  KE_des<-c(KE_des,length(xList$data[i][[1]]$"description"))
}
nKE_des<-sum(KE_des)
pKE_des<-nKE_des/numKEs*100


# KEs with measurement-methodology
KE_mm<-vector()
for(i in KE_loc){
  KE_mm<-c(KE_mm,length(xList$data[i][[1]]$"measurement-methodology"))
}
nKE_mm<-sum(KE_mesLen)
pKE_mm<-nKE_mm/numKEs*100


# KEs with evidence-supporting-taxonomic-applicability
KE_esta<-vector()
for(i in KE_loc){
  KE_esta<-c(KE_esta,length(xList$data[i][[1]]$"evidence-supporting-taxonomic-applicability"))
}
nKE_esta<-sum(KE_esta)
pKE_esta<-nKE_esta/numKEs*100


# KEs with organ term
KE_ot<-vector()
for(i in KE_loc){
  KE_ot<-c(KE_ot,length(xList$data[i][[1]]$"organ-term"))
}
KE_ot<-as.numeric(KE_ot==3)
nKE_ot<-sum(KE_ot)
pKE_ot<-nKE_ot/numKEs*100


# KEs with cell term
KE_ct<-vector()
for(i in KE_loc){
  KE_ct<-c(KE_ct,length(xList$data[i][[1]]$"cell-term"))
}
KE_ct<-as.numeric(KE_ct==3)
nKE_ct<-sum(KE_ct)
pKE_ct<-nKE_ct/numKEs*100


# KEs with cell term OR organ term
KE_otct<-as.numeric(KE_ot|KE_ct)
nKE_otct<-sum(KE_otct)
pKE_otct<-nKE_otct/numKEs*100


# KEs with taxonomic applicability data
KE_tax<-vector()
for(i in 1:length(KE_loc)){
  KE_tax<-c(KE_tax, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="taxonomy"))
}
KE_tax<-as.numeric(KE_tax)
nKE_tax<-sum(KE_tax)
pKE_tax<-nKE_tax/numKEs*100


# KEs with sex applicability data
KE_sex<-vector()
for(i in 1:length(KE_loc)){
  KE_sex<-c(KE_sex, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="sex"))
}
KE_sex<-as.numeric(KE_sex)
nKE_sex<-sum(KE_sex)
pKE_sex<-nKE_sex/numKEs*100


# KEs with life-stage applicability data
KE_ls<-vector()
for(i in 1:length(KE_loc)){
  KE_ls<-c(KE_ls, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="life-stage"))
}
KE_ls<-as.numeric(KE_ls)
nKE_ls<-sum(KE_ls)
pKE_ls<-nKE_ls/numKEs*100


# All KE fields
KE_field_stats<-cbind(KE_title, KE_sn, KE_bol, KE_otct, KE_des, KE_mm, KE_tax, KE_sex, KE_ls, KE_esta)
row.names(KE_field_stats)<-KE_id

# Save results
# write.table(KE_field_stats, file=paste(workingDir, "KE_field_stats.txt"), sep="\t")


### Statistics Key Event Relationship fields

# List all KER fields
kerFields<-vector()
for(i in 1:length(KER_loc)){
  kerFields<-c(kerFields, names(xList$data[KER_loc[i]][[1]]))
  kerFields<-unique(kerFields)
}
kerFields
# [1] "title"                                       "description"
# [3] "weight-of-evidence"                          "quantitative-understanding"
# [5] "taxonomic-applicability"                     "evidence-supporting-taxonomic-applicability"
# [7] "references"                                  "source"
# [9] "creation-timestamp"                          "last-modification-timestamp"


# total number of KERs
numKERs<-length(KER_loc)

# KER ids
KER_ref_id<-vector()
for(i in 1:length(KER_loc)){
  KER_ref_id<-c(KER_ref_id, attr(xList$data[KER_loc[i]][[1]],"id"))
}
KER_id<-kerID$ID[match(KER_ref_id,kerID$ref)]


# KERs with titles
KER_title<-vector()
for(i in 1:length(KER_loc)){
  KER_title<-c(KER_title,length(xList$data[KER_loc[i]][[1]]$title))
}
KER_title<-as.numeric(KER_title==2)
nKER_title<-sum(KER_title)
pKER_title<-nKER_title/numKERs*100


# KERs with descriptions
KER_des<-vector()
for(i in 1:length(KER_loc)){
  KER_des<-c(KER_des,length(xList$data[KER_loc[i]][[1]]$"description"))
}
nKER_des<-sum(KER_des)
pKER_des<-nKER_des/numKERs*100


# KERs with biological plausibility
KER_bp<-vector()
for(i in 1:length(KER_loc)){
  KER_bp<-c(KER_bp,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"biological-plausibility"))
}
nKER_bp<-sum(KER_bp)
pKER_bp<-nKER_bp/numKERs*100


# KERs with emperical-support
KER_es<-vector()
for(i in 1:length(KER_loc)){
  KER_es<-c(KER_es,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"emperical-support-linkage"))
}
nKER_es<-sum(KER_es)
pKER_es<-nKER_es/numKERs*100


# KERs with uncertainties and inconsistencies
KER_ui<-vector()
for(i in 1:length(KER_loc)){
  KER_ui<-c(KER_ui,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"uncertainties-or-inconsistencies"))
}
nKER_ui<-sum(KER_ui)
pKER_ui<-nKER_ui/numKERs*100


# KERs with quantitative understanding
KER_qu<-vector()
for(i in 1:length(KER_loc)){
  KER_qu<-c(KER_qu,length(xList$data[KER_loc[i]][[1]]$"quantitative-understanding"$description))
}
nKER_qu<-sum(KER_qu)
pKER_qu<-nKER_qu/numKERs*100


# KERs with taxonomic applicability data
KER_tax<-vector()
for(i in 1:length(KER_loc)){
  KER_tax<-c(KER_tax, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="taxonomy"))
}
KER_tax<-as.numeric(KER_tax)
nKER_tax<-sum(KER_tax)
pKER_tax<-nKER_tax/numKERs*100


# KERs with sex applicability data
KER_sex<-vector()
for(i in 1:length(KER_loc)){
  KER_sex<-c(KER_sex, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="sex"))
}
KER_sex<-as.numeric(KER_sex)
nKER_sex<-sum(KER_sex)
pKER_sex<-nKER_sex/numKERs*100


# KERs with life-stage applicability data
KER_ls<-vector()
for(i in 1:length(KER_loc)){
  KER_ls<-c(KER_ls, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="life-stage"))
}
KER_ls<-as.numeric(KER_ls)
nKER_ls<-sum(KER_ls)
pKER_ls<-nKER_ls/numKERs*100


# KERs with evidence-supporting-taxonomic-applicability
KER_esta<-vector()
for(i in 1:length(KER_loc)){
  KER_esta<-c(KER_esta,length(xList$data[KER_loc[i]][[1]]$"evidence-supporting-taxonomic-applicability"))
}
nKER_esta<-sum(KER_esta)
pKER_esta<-nKER_esta/numKERs*100


# All KE fields
KER_field_stats<-cbind(KER_title,KER_des,KER_bp,KER_es,KER_qu,KER_ui,KER_tax,KER_sex,KER_ls,KER_esta)
row.names(KER_field_stats)<-KER_id

# Save results
# write.table(KER_field_stats, file=paste(workingDir, "KER_field_stats.txt"), sep="\t")




### Statistics on AOP fields

# list aop fields
aopFields<-vector()
for(i in 1:length(AOP_loc)){
  aopFields<-c(aopFields, names(xList$data[AOP_loc[i]][[1]]))
  aopFields<-unique(aopFields)
}
aopFields
# [1] "title"                       "short-name"                  "authors"                     "status"
# [5] "oecd-project"                "abstract"                    "molecular-initiating-event"  "adverse-outcome"
# [9] "key-event-relationships"     "essentiality-support"        "key-events"                  "applicability"
# [13] "overall-assessment"          "potential-applications"      "references"                  "source"
# [17] "creation-timestamp"          "last-modification-timestamp" "aop-stressors"               "background"

# Total number of AOPs
numAOPs<-length(AOP_loc)

# AOP ids
AOP_ref_id<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ref_id<-c(AOP_ref_id, attr(xList$data[AOP_loc[i]][[1]],"id"))
}
AOP_id<-aopID$ID[match(AOP_ref_id,aopID$ref)]


# AOPs with title
AOP_title<-vector()
for(i in 1:length(AOP_loc)){
  AOP_title<-c(AOP_title, length(xList$data[AOP_loc[i]][[1]]$title[[1]]))
}
nAOP_title<-sum(AOP_title)
pAOP_title<-nAOP_title/numAOPs*100


# AOPs with short name
AOP_sn<-vector()
for(i in 1:length(AOP_loc)){
  AOP_sn<-c(AOP_sn,length(xList$data[AOP_loc[i]][[1]]$"short-name"[[1]]))
}
nAOP_sn<-sum(AOP_sn)
pAOP_sn<-nAOP_sn/numAOPs*100


# AOP authors
AOP_au<-vector()
for(i in 1:length(AOP_loc)){
  AOP_au<-c(AOP_au,length(xList$data[AOP_loc[i]][[1]]$"authors"))
}
nAOP_au<-sum(AOP_au)
pAOP_au<-nAOP_au/numAOPs*100


#AOP abstract
AOP_ab<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ab<-c(AOP_ab,length(xList$data[AOP_loc[i]][[1]]$"abstract"))
}
nAOP_ab<-sum(AOP_ab)
pAOP_ab<-nAOP_ab/numAOPs*100


#AOP MIE
AOP_mie<-vector()
for(i in 1:length(AOP_loc)){
  AOP_mie<-c(AOP_mie,length(xList$data[AOP_loc[i]][[1]]$"molecular-initiating-event"))
}
nAOP_mie<-sum(AOP_mie)
pAOP_mie<-nAOP_mie/numAOPs*100


#AOP evidence supporting mie
AOP_esm<-vector()
for(i in 1:length(AOP_loc)){
  AOP_esm<-c(AOP_esm,length(xList$data[AOP_loc[i]][[1]]$"molecular-initiating-event"$`evidence-supporting-chemical-initiation`))
}
nAOP_esm<-sum(AOP_esm)
pAOP_esm<-nAOP_esm/numAOPs*100


#AOP adverse outcome
AOP_ao<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ao<-c(AOP_ao,length(xList$data[AOP_loc[i]][[1]]$'adverse-outcome'))
}
nAOP_ao<-sum(AOP_ao)
pAOP_ao<-nAOP_ao/numAOPs*100


#AOP adverse outcome examples
AOP_aoe<-vector()
for(i in 1:length(AOP_loc)){
  AOP_aoe<-c(AOP_aoe,length(xList$data[AOP_loc[i]][[1]]$'adverse-outcome'$'examples'))
}
nAOP_aoe<-sum(AOP_aoe)
pAOP_aoe<-nAOP_aoe/numAOPs*100


#AOP KERs
AOP_ker<-vector()
for(i in 1:length(AOP_loc)){
  if(length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships")==1){
    if(length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"[[1]])==1){
      AOP_ker<-c(AOP_ker,0)
    }else{
      AOP_ker<-c(AOP_ker,length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"))
    }
  }else{
    AOP_ker<-c(AOP_ker,length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"))
  }
}
AOP_hasker<-as.numeric(AOP_ker>0)
nAOP_ker<-sum(AOP_hasker)
pAOP_ker<-nAOP_ker/numAOPs*100


#AOP KEs
AOP_ke<-vector()
for(i in 1:length(AOP_loc)){
  if(any(names(xList$data[AOP_loc[i]][[1]]$"key-events")=="key-event")){
    AOP_ke<-c(AOP_ke,length(xList$data[AOP_loc[i]][[1]]$"key-events"))
  }else{
    AOP_ke<-c(AOP_ke,0)
  }
}
AOP_haske<-as.numeric(AOP_ke>0)
nAOP_ke<-sum(AOP_haske)
pAOP_ke<-nAOP_ke/numAOPs*100


#AOP essentiality support
AOP_es<-vector()
for(i in 1:length(AOP_loc)){
  AOP_es<-c(AOP_es,length(xList$data[AOP_loc[i]][[1]]$"essentiality-support"))
}
nAOP_es<-sum(AOP_es>0)
pAOP_es<-nAOP_es/numAOPs*100


#AOP domain of applicability, taxonomic
## the XML file seems to mbe missing all taxonomic applicability data for all AOPs


#AOP domain of applicability, sex
AOP_sex<-vector()
for(i in 1:length(AOP_loc)){
  AOP_sex<-c(AOP_sex,any(names(xList$data[AOP_loc[i]][[1]]$"applicability")=="sex"))
}
AOP_sex<-as.numeric(AOP_sex)
nAOP_sex<-sum(AOP_sex)
pAOP_sex<-nAOP_sex/numAOPs*100


#AOP domain of applicability, life-stage
AOP_ls<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ls<-c(AOP_ls,any(names(xList$data[AOP_loc[i]][[1]]$"applicability")=="life-stage"))
}
AOP_ls<-as.numeric(AOP_ls)
nAOP_ls<-sum(AOP_ls)
pAOP_ls<-nAOP_ls/numAOPs*100


#AOP overall assessment, description
AOP_oad<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oad<-c(AOP_oad,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$description))
}
nAOP_oad<-sum(AOP_oad)
pAOP_oad<-nAOP_oad/numAOPs*100


#AOP overall assessment, applicability
AOP_oaa<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaa<-c(AOP_oaa,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$applicability))
}
nAOP_oaa<-sum(AOP_oaa)
pAOP_oaa<-nAOP_oaa/numAOPs*100


#AOP overall assessment, essentiality summary
AOP_oae<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oae<-c(AOP_oae,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`key-event-essentiality-summary`))
}
nAOP_oae<-sum(AOP_oae)
pAOP_oae<-nAOP_oae/numAOPs*100


#AOP overall assessment, WOE summary
AOP_oaw<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaw<-c(AOP_oaw,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`weight-of-evidence-summary`))
}
nAOP_oaw<-sum(AOP_oaw)
pAOP_oaw<-nAOP_oaw/numAOPs*100


#AOP overall assessment, quantitative summary
AOP_oaq<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaq<-c(AOP_oaq,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`quantitative-considerations`))
}
nAOP_oaq<-sum(AOP_oaq)
pAOP_oaq<-nAOP_oaq/numAOPs*100


#AOP potential applications
AOP_pa<-vector()
for(i in 1:length(AOP_loc)){
  AOP_pa<-c(AOP_pa,length(xList$data[AOP_loc[i]][[1]]$`potential-applications`))
}
nAOP_pa<-sum(AOP_pa)
pAOP_pa<-nAOP_pa/numAOPs*100


#AOP stressors
AOP_st<-vector()
for(i in 1:length(AOP_loc)){
  AOP_st<-c(AOP_st,length(xList$data[AOP_loc[i]][[1]]$"aop-stressors"))
}
AOP_st<-as.numeric(AOP_st>0)
nAOP_st<-sum(AOP_st)
pAOP_st<-nAOP_st/numAOPs*100


#AOP background
AOP_bg<-vector()
for(i in 1:length(AOP_loc)){
  AOP_bg<-c(AOP_bg,length(xList$data[AOP_loc[i]][[1]]$"background"))
}
nAOP_bg<-sum(AOP_bg)
pAOP_bg<-nAOP_bg/numAOPs*100


# All AOP fields
AOP_field_stats<-cbind(AOP_title,AOP_sn,AOP_au,AOP_ab,AOP_bg,AOP_haske,AOP_hasker,AOP_mie,AOP_esm,AOP_ao,AOP_aoe,AOP_es,AOP_sex,AOP_ls,AOP_pa,AOP_st,AOP_oad,AOP_oaa,AOP_oae,AOP_oaw,AOP_oaq)
row.names(AOP_field_stats)<-AOP_id

# Save results
# write.table(AOP_field_stats, file=paste(workingDir, "AOP_field_stats.txt"), sep="\t")



