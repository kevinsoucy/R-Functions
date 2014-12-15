##================================
## CHAPTER 20: NETWORK DATA 
##================================
library(igraph)

# m is an adjacent matrix (indicating row->column conections)
m=matrix(nrow=3,ncol=3)
m[1,1]=0
m[1,2]=1
m[1,3]=1
m[2,1]=1
m[2,2]=0
m[2,3]=0
m[3,1]=0
m[3,2]=1
m[3,3]=0
m

lab=c(1,2,3)
object <- graph.adjacency(m,mode="directed") 
#set.seed(1)
plot(object, vertex.label=lab)

##---------------------------------------------------------
## Example 1: Marriage and Power in 15th Century Florence
##---------------------------------------------------------
## read the data
florence <- as.matrix(read.csv("C:/EDU/Courses/Analytics4BuzIntelligence/Fall2014/Data/firenze.csv"))
florence

## use the help function to understand the options for the graph  
marriage <- graph.adjacency(florence, mode="undirected", diag=FALSE)
set.seed(1)
# what is layout.fruchterman.reingold
plot(marriage, layout=layout.fruchterman.reingold, vertex.label=V(marriage)$name, vertex.color="red", vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)

data.frame(V(marriage)$name, degree(marriage))

## calculate and plot the shortest paths
V(marriage)$color <- 8
E(marriage)$color <- 8
PtoA <- get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
E(marriage, path=PtoA$vpath[[1]])$color <- "magenta"
V(marriage)[PtoA$vpath[[1]] ]$color <- "magenta"

GtoS <- get.shortest.paths(marriage, from="Ginori", to="Strozzi")
E(marriage, path=GtoS$vpath[[1]])$color <- "green"
V(marriage)[ GtoS$vpath[[1]] ]$color <- "green"
V(marriage)[ "Medici" ]$color <- "cyan"

set.seed(1)
plot(marriage,  layout=layout.fruchterman.reingold, vertex.label=V(marriage)$name,vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)

data.frame(V(marriage)$name, betweenness(marriage))



