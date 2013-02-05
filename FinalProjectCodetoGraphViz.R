#Read in the data

Matrix <- read.csv("InteractionMatrix_with27.csv")
Nodes <- Matrix[1:2]
Matrix1 <- Matrix[,-2]

#subset data to exclude first column
Matrix2 <- subset( Matrix1, select = -Code )

#Make a copy
Matrix3 <- Matrix2

#Remove all values that are not reciprocal
for(i in 1:nrow(Matrix3)){
  for(j in 1:ncol(Matrix3)){
    sp1 = Matrix3[i,j]
    sp2 = t(Matrix3)[i,j]
    Sum = sum(sp1,sp2)
    if(Sum!=0){ 
      Matrix3[i,j]=0
      Matrix3[j,i]=0}
  }
}

Matrix4 <- Matrix3
#Remove negative values to isolate predation interactions
for(i in 1:nrow(Matrix4)){
  for(j in 1:ncol(Matrix4)){
    sp1 <- Matrix4[i,j]
    if(sp1 == -1){
      Matrix4[i,j] = 0}
  }
}

Matrix5 <- Matrix4

#create vectors to store node ID's from interactions 
ConsumerNodeID <- c()
ResourceNodeID <- c()

#Populate two vectors with the node ID's for the consumer and the
#resource wherever there is an interaction
for(i in 1:nrow(Matrix5)){
  for(j in 1:ncol(Matrix5)){
    sp1 <- Matrix5[i,j]
    if(sp1 == 1){
      ConsumerNodeID <- c(ConsumerNodeID, j)
      ResourceNodeID <- c(ResourceNodeID, i)
    }
  }
}

#Merge the two vectors into a data frame
Edges <- data.frame(ConsumerNodeID, ResourceNodeID)
names(Edges) <- c("Consumer", "Resource")

Edges

#Create a network object

require(network)

Web2 <- as.network(Edges, vertex.attr=NULL, vertex.attrnames=Nodes)

plot.network.default(Web2)

network.vertex.names <- Nodes


#Write file of Node names with digraph format 
Codes <- Nodes[,1]
Names <- Nodes[,2]
NamesViz <- toString(' [label="')
QuoteViz <- toString('"]')
    
Viz <- c()
      
for(i in 1:nrow(Nodes)){
  moon <- Names[i]
  star <- paste(i, NamesViz, moon, QuoteViz, sep = "", collapse = NULL)
  Viz <- c(Viz, star)
}
      
write(Viz, file = "Viz")

#Format the linkages in digraph format. And write to file. 

Consumer2 <- Edges[,1]
Resource2 <- Edges[,2]
Hip <- toString(' -> { ')
Hop <- toString(' }')
Arrows2 <- c()

for(i in 1:112){
  FoodsFori <- c()
  for(j in 1:nrow(Edges)){
    if(Consumer2[j] == i){
      FoodsFori <- c(FoodsFori, Resource2[j])
      String <- toString(FoodsFori)
    }
  }
  Awesome <- paste(i, Hip, String, Hop, sep = "", collapse = NULL)
  Arrows2 <- c(Arrows2, Awesome)
}

write(Arrows2, file = "Arrows2")
