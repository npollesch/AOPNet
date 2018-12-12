#libraries
library(igraph)
library(prodlim)
library(RColorBrewer)
library(autoimage)

#Directories
workingDir<-"C:\\Users\\obrienja\\Documents\\GitHub\\AOPWiki\\R_files_Jason\\"

#load source of functions
source(paste(workingDir,"AOP-Net-Functions.R",sep=""))    #imports custom functions

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


