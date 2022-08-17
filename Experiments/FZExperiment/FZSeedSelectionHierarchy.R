library(igraph)
library(readxl)
#library(GGally)

setwd("C:/Users/Others/Dropbox/MS-ThesisSpring2022/Rcode")

file=read_excel("mqp-Data2010-2019.xlsx", sheet = 2)

yearVar="2010"

df=file[file[,"YEAR"]==yearVar,]
colnames(df) <- c("V1","V2","weight","year")


g=simplify(graph_from_data_frame(df, directed = F))

weight2=max(E(g)$weight)-E(g)$weight+1   # reverse edge weights for minimum spanning tree

gt=set_edge_attr(g,"weight2",E(g),weight2)

tree=mst(gt,weights = E(gt)$weight2 )

is.directed(tree)


sort(degree(tree),decreasing = T)


totalWeight=strength(g,V(g),weights = E(g)$"weight" )
gtemp=set_vertex_attr(g, "totalWeight", index = V(g), totalWeight)



rootNode=V(gtemp)[which.max(V(gtemp)$totalWeight)]


#rootNode=V(tree)[V(tree)$name=="LGW"]

#write_graph(tree,"MSTree.graphml",format = "graphml")

#ggnet2(tree, label = TRUE, label.size = 3)

#plot(degree(tree))

#plot(tree)


#level1=neighbors(tree, rootNode, mode = "all")

#level2=neighbors(tree, level1, mode = "all")



res=shortest_paths(tree,rootNode, to = V(tree), mode = "all", weights = NULL, output = "vpath",  predecessors = FALSE, inbound.edges = FALSE, algorithm = "automatic")

sizeList=as.numeric(lengths(res))[1]

# create loop here to iterate over all the paths

for(i in 1:sizeList){

  x=unlist(res$vpath[i])

  t=as.data.frame(cbind(row.names(x),x[length(x)]))



  #fix this
  nodeName=V(gtemp)[which.max(V(gtemp)$totalWeight)]


  #nodeName=row.names(t)

  #fix this code
  ##V[tree][nodeName]$level=length(x)-1

  level=length(x)-1

  tree=set_vertex_attr(tree, "level", index = V(nodeName), level)
}


# height = function(v) {
#   D = distances(tree, to=v, mode="all")
#   max(D[D != Inf])
# }

## Apply it to all nodes
#res2=sapply(rootNode, height)
#A Z B C D E F G H I
#3 4 0 1 2 0 0 1 0 0


#nodes=as.data.frame(V(tree)$name)
#edges=as.data.frame(as_edgelist(tree, names = TRUE))

#visNetwork(nodes, edges, width = "100%")



# CentralCities=C("JFK","CDG","LGW","NRT","HKG")
#
# strength(g10,V(g10)["JFK"],weights = E(g10)$"weight" )
#
# strength(g10,V(g10)["JFK"],weights = E(g10)$"LINKS"  )
#
# x=neighbors(g10,"JFK")
# ei <- get.edge.ids(g10, c("JFK","CDG"))
# E(g)[ei]
# E(g10)[9553]$"weight"
# E(g10)$"weights"



#totalWeight=strength(g10,V(g10),weights = E(g10)$"weight" )

#gt=set_vertex_attr(g10, "totalWeight", index = V(g10), totalWeight)

#tkplot(tree)