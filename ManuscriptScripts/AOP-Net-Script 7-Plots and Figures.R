#libraries
library(igraph)
library(prodlim)
library(RColorBrewer)
library(plotrix)
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
