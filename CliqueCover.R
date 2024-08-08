library(rapportools)
library(igraph)
library(visNetwork)


gorjinal<- graph(edges <- c(frb50.23.1$V1,frb50.23.1$V2),directed = FALSE) 


gorjinal <- graph(edges <- c("1","1","4","7","4","8","7","8","8","9","6","9","6","10","9","10"),directed = FALSE )

gorjinal <- graph(edges <- c(),directed = FALSE )


########### Clique Cover ###########

vector = c()
MalatyaCentrality <- function(g){
  vertexList <- c(V(g))
  for (i in vertexList) { 
    Vdegree <-degree(g,v = V(g)[i])
    KomsuDegree <- degree(g,v = neighbors(g,v = V(g)[i]))
    Value <- Vdegree/KomsuDegree
    vector <- c(vector, sum(Value))
  }
  return(vector)
}


FindMinimum <- function(graph){
  data <- data.frame(MalatyaCentrality(graph))
  return (order(data$MalatyaCentrality.graph.,decreasing = FALSE)[1])
}


PendandtNodes <- function(x) {
  PendandtNodes <- V(x)[degree(x)== 1]
  return(PendandtNodes)
}

CliqueCover <- function(){
  Sayac <- 0
while (vcount(gorjinal) > 0) {
   g<-gorjinal
  if(is.empty(PendandtNodes(g))[1] == FALSE){
    Pendant = c()
    silinecekNodes =c()
    komsu <- NULL
    Pendant <- append(Pendant,PendandtNodes(g)[1]$name)
    komsu <- neighbors(g,v = PendandtNodes(g)[1]$name)
    silinecekNodes <- append(silinecekNodes,Pendant)
    silinecekNodes <- append(silinecekNodes,komsu$name)
    ##print(paste('pendant clique',silinecekNodes))
    print(silinecekNodes)
    g <- delete_vertices(g,silinecekNodes )
    Sayac <- Sayac+1
    gorjinal<-g
  }else{
  g<- complementer(gorjinal)
  IndependentSet = c()
  while(vcount(g) > 0 ){
    silinecekNodes =c()
    komsu <- NULL 
    IndependentSet <- append(IndependentSet,V(g)[FindMinimum(g)]$name)
    minNode <- V(g)[FindMinimum(g)]$name
    komsu <- neighbors(g,v = minNode)$name
    silinecekNodes <- append(silinecekNodes,minNode)
    silinecekNodes <- append(silinecekNodes,komsu)
    g <- delete_vertices(g,silinecekNodes )
  }
 print(IndependentSet) 
# V(gorjinal)$color <- ifelse(V(gorjinal)$name == IndependentSet, "green", "orange")
 
 # plot(gorjinal, edge.arrow.size=.2, vertex.label.cex = 1.5,vertex.size=20,edge.color="grey",
 #      vertex.color="orange", layout= layout.kamada.kawai(gorjinal) ,vertex.frame.color="#ffffff",
 #      vertex.label=V(gorjinal)$media, vertex.label.color="black")
 Sayac <- Sayac+1
 gorjinal <- delete_vertices(gorjinal,IndependentSet )
  }
}
  print(paste(Sayac,'adet Clique bulundu') )
}



plot(gorjinal, edge.arrow.size=.2, vertex.label.cex = 1.5,vertex.size=20,edge.color="grey",
     vertex.color="orange", layout= layout.kamada.kawai(gorjinal) ,vertex.frame.color="#ffffff",
     vertex.label=V(gorjinal)$media, vertex.label.color="black")
plot(gc, edge.arrow.size=.2, vertex.label.cex = 1.5,vertex.size=20,edge.color="grey",
     vertex.color="orange",  layout= layout.circle(gc)  ,vertex.frame.color="#ffffff",
     vertex.label=V(gc)$media, vertex.label.color="black")


gc<-complementer(gorjinal)

E(gorjinal)$width <- 2
V(gorjinal)$color[V(gorjinal)$name == "14"] = "green"
V(gorjinal)$color[V(gorjinal)$name == "15"] = "green"
V(gorjinal)$color[V(gorjinal)$name == "13"] = "green"
plot(gorjinal, edge.arrow.size=.2, vertex.label.cex = 1.5,vertex.size=20,edge.color="grey",
        layout= layout.kamada.kawai(gorjinal) ,vertex.frame.color="#ffffff",
     vertex.label=V(gorjinal)$media, vertex.label.color="black")




E(gc)$width <- 2
V(gc)$color[V(gc)$name == "9"] = "green"
V(gc)$color[V(gc)$name == "7"] = "green"
V(gc)$color[V(gc)$name == "8"] = "green"
plot(gc, edge.arrow.size=.2, vertex.label.cex = 1.5,vertex.size=20,edge.color="grey",
      layout= layout.circle(gc) ,vertex.frame.color="#ffffff",
     vertex.label=V(gc)$media, vertex.label.color="black")



