library(igraph)
library(readxl)
library(influence.mining)

graph <- city2016
# Some stats
#print(graph_summary(graph))
V(graph)$name = V(graph)
# Assign weights
#E(graph)$weight <- 1
weight <- max(E(graph)$weight) - E(graph)$weight + 1 # reverse edge weights for minimum spanning tree
gt <- set_edge_attr(graph, "weight", E(graph), weight)
tree <- mst(gt, weights = E(gt)$weight)
fit_power_law(city2016)
fit_power_law(tree)
#plot(tree)

sort(degree(graph), decreasing = TRUE)
sort(degree(tree), decreasing = TRUE)
u <- mean(degree(tree))
sd <- sd(degree(tree))
n <- 2
cutoff <- u + (n * sd)
cutoff
seed <- V(tree)[degree(tree) > cutoff]
k <- length(seed)

# 2nd degree outliers
u2 <- mean(degree(tree, seed))
sd2 <- sd(degree(tree, seed))
cutoff2 <- u2 + (n * sd2)
cutoff2
seed <- seed[degree(tree, seed) > cutoff2]
k <- length(seed)


influence <- get_influence(graph, seed$name, test_method="INFLUENCE_IC")
influence
inf_ad_degree <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_IC", heuristic = "ADAPTIVE_CENTRALITY", centrality_method = "DEGREE", logging = FALSE)
inf_ad_degree
inf_pagerank <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_IC", heuristic = "PAGERANK", logging = FALSE)
inf_pagerank
inf_coreness <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_IC", heuristic = "CORENESS", logging = FALSE)
inf_coreness

influence <- get_influence(graph, seed$name, test_method="INFLUENCE_LT")
influence
inf_ad_degree <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_LT", heuristic = "ADAPTIVE_CENTRALITY", centrality_method = "DEGREE", logging = FALSE)
inf_ad_degree
inf_pagerank <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_LT", heuristic = "PAGERANK", logging = FALSE)
inf_pagerank
inf_coreness <- influence(graph, budget = k, prob = 0.5, test_method = "INFLUENCE_LT", heuristic = "CORENESS", logging = FALSE)
inf_coreness

influence <- get_influence(graph, seed$name, test_method="RESILIENCE")
influence
inf_ad_degree <- influence(graph, budget = k, prob = 0.5, test_method = "RESILIENCE", heuristic = "ADAPTIVE_CENTRALITY", centrality_method = "DEGREE", logging = FALSE)
inf_ad_degree
inf_pagerank <- influence(graph, budget = k, prob = 0.5, test_method = "RESILIENCE", heuristic = "PAGERANK", logging = FALSE)
inf_pagerank
inf_coreness <- influence(graph, budget = k, prob = 0.5, test_method = "RESILIENCE", heuristic = "CORENESS", logging = FALSE)
inf_coreness



#library(GGally)

#setwd("C:/Users/Others/Dropbox/MS-ThesisSpring2022/Rcode")

#file=read_excel("mqp-Data2010-2019.xlsx", sheet = 2)

#yearVar="2010"

#df=file[file[,"YEAR"]==yearVar,]
#colnames(df) <- c("V1","V2","weight","year")


#g=simplify(graph_from_data_frame(df, directed = F))


# totalWeight=strength(g,V(g),weights = E(g)$"weight" )
#
# gtemp=set_vertex_attr(g, "totalWeight", index = V(g), totalWeight)
#
# rootNode=V(gtemp)[which.max(V(gtemp)$totalWeight)]


#rootNode=V(tree)[V(tree)$name=="LGW"]

#write_graph(tree,"MSTree.graphml",format = "graphml")

#ggnet2(tree, label = TRUE, label.size = 3)

#plot(degree(tree))

#plot(tree)


#level1=neighbors(tree, rootNode, mode = "all")

#level2=neighbors(tree, level1, mode = "all")



# res=shortest_paths(tree,rootNode, to = V(tree), mode = "all", weights = NULL, output = "vpath",  predecessors = FALSE, inbound.edges = FALSE, algorithm = "automatic")
#
# sizeList=as.numeric(lengths(res))[1]

# create loop here to iterate over all the paths

#for(i in 1:sizeList){

#x=unlist(res$vpath[i])

#t=as.data.frame(cbind(row.names(x),x[length(x)]))



#fix this
#nodeName=V(gtemp)[which.max(V(gtemp)$totalWeight)]


#nodeName=row.names(t)

#fix this code
##V[tree][nodeName]$level=length(x)-1

#   level=length(x)-1
#
#   tree=set_vertex_attr(tree, "level", index = V(nodeName), level)
# }


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