#libraries
library(igraph)
library(prodlim)
library(RColorBrewer)

#Directories
workingDir<-"C:\\Users\\obrienja\\Documents\\GitHub\\AOPWiki\\R_files_Jason\\"

#load source of functions
source(paste(workingDir,"AOP-Net-Functions.R",sep=""))    #imports Jason's custom functions

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


