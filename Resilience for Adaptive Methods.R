library(igraph)

# Resillience of network: how fast a network goes down when nodes go down
resilience <- function(graph, metrics=c("nodes", "edges", "apl", "transitivity", "avg-degree", "density"), seed_method=c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
  graph <- largest_component(graph)
  nodes <- NULL
  edges <- NULL
  apl <- NULL
  transitivity <- NULL
  avg_degree <- NULL
  density <- NULL
  while (TRUE) {
    param <- NULL
    if (seed_method == "a-degree")
      param <- degree(graph, V(graph), mode="all")
    else if (seed_method == "a-closeness")
      param <- closeness(graph, V(graph), mode="all")
    else if(seed_method == "a-betweenness")
      param <- betweenness(graph, V(graph))
    else if (seed_method == "a-coreness")
      param <- graph.coreness(graph, mode="all")
    else if (seed_method == "a-eigenvector") {
      eigen <- evcent(graph, directed=FALSE)
      param <- eigen$vector
    }
    # Try to remove 1% nodes instead of 1 (not working a.t.m)
    # n_high <- head(sort(param, decreasing=TRUE), n=ceiling(length(param)*0.01))
    graph <- delete.vertices(graph, which.max(param))
    graph <- largest_component(graph)
    if ("nodes" %in% metrics) {
      nodes <- c(nodes, vcount(graph))
    }
    if ("edges" %in% metrics) {
      edges <- c(edges, ecount(graph))
    }
    if ("apl" %in% metrics) {
      this <- average.path.length(graph)
      apl <- c(apl, ifelse(is.nan(this), 0, this))
    }
    if ("transitivity" %in% metrics) {
      this <- transitivity(graph)
      transitivity <- c(transitivity, ifelse(is.nan(this), 0, this))
    }
    if ("avg-degree" %in% metrics) {
      this <- mean(degree(graph))
      avg_degree <- c(avg_degree, ifelse(is.nan(this), 0, this))
    }
    if ("density" %in% metrics) {
      this <- graph.density(graph)
      density <- c(density, ifelse(is.nan(this), 0, this))
    }
    size <- vcount(graph)
    print(size)
    if (size <= 1)
      break
  }
  df <- data.frame(nodes=nodes, edges=edges, apl=apl, transitivity=transitivity, avg_degree=avg_degree, density=density)
  df
}

save_graph <- function(file, xlabel, ylabel, params, param_labels) {
  lengths <- unlist(lapply(params, length))
  peaks <- unlist(lapply(params, max))
  xmax <- max(lengths)
  ymax <- max(peaks)
  png(filename=file, width=6, height=6, units="in", res=200)
  plot((params[[1]]), xlim=c(0, xmax), ylim=c(0, ymax), xlab=xlabel, ylab=ylabel, pch=1, col=1, type="b")
  for (i in 2:length(params)) {
    points((params[[i]]), pch=i, col=i, type="b")
  }
  legend("topright", inset=.05, title="Parameters", param_labels, fill=seq(1:length(param_labels)), horiz=FALSE)
  dev.off()
}


#########################################################
########## Facebook Network from Stanford SNAP ##########
#########################################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/Snap-facebook/")
file_name <- "facebook_edges.txt"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

# Plot resilience by no. of nodes
param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="facebook_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
# Plot resilience by no. of edges
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="facebook_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
# Plot resilience by average path length
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="facebook_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
# Plot resilience by average degree
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="facebook_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
# Plot resilience by density
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="facebook_density.png", xlabel="Steps", ylabel="Density", params, param_labels)

############################################################
########## Twitter Network from Snowball Sampling ##########
############################################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/Twitter/")
file_name <- "my_twitter_network.csv"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="twitter_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="twitter_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="twitter_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="twitter_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="twitter_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


#################################################
########## Actor network from Barabasi ##########
#################################################
# This graph is in form of node list
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/Actor_Barabasi/")
file_name <- "actor.txt"
lines <- scan(file_name, what="character", sep="\n", skip=1) # read the csv file (skipping the header), line-by-line as character string.
lines <- gsub(","," ",lines) # replace commas with spaces
lines <- gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
adjlist <- strsplit(lines," ") # splits the character strings into list with different vector for each line
col1 <- unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
col2 <- unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
el <- cbind(col1, col2) # creates the edgelist by combining column 1 and 2.
g <- graph.edgelist(el)
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

# Plot resilience by no. of nodes
param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="actor_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
# Plot resilience by no. of edges
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="actor_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
# Plot resilience by average path length
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="actor_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
# Plot resilience by average degree
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="actor_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
# Plot resilience by density
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="actor_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


#########################################################
########## Co-authorship Network from the DBLP ##########
#########################################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/Author_DBLP2008/")
file_name <- "author_DBLP_edges.csv"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

# Plot resilience by no. of nodes
param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="DBLP_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="DBLP_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="DBLP_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="DBLP_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="DBLP_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


###########################################
########## CAIDA Project Network ##########
###########################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/as-caida20071105/")
file_name <- "as-caida20071105.txt"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="CAIDA_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="CAIDA_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="CAIDA_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="CAIDA_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="CAIDA_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


###############################################
########## Linux Source Code Network ##########
###############################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/linux/")
file_name <- "linux_edgelist.txt"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="linux_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="linux_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="linux_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="linux_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="linux_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


#################################################
########## Protein Network by Barabasi ##########
#################################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/Protein_Barabasi/")
file_name <- "yeast_barabasi2.txt"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="yeast_health_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="yeast_health_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="yeast_health_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="yeast_health_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="yeast_health_density.png", xlabel="Steps", ylabel="Density", params, param_labels)


#############################################################
########## WordNet Network from Koblenz Collection ##########
#############################################################
setwd("C:/Users/Owais/OneDrive/Knowledge/PhD/Projects/Influence Mining/Datasets/wordnet-words/")
file_name <- "wordnet-words.txt"
ds <- read.csv(file_name, sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=FALSE)  
metrics <- c("nodes", "edges", "apl", "transitivity", "avg-degree", "density")
degree <- resilience(g, metrics, "a-degree")
betweenness <- resilience(g, metrics, "a-betweenness")
closeness <- resilience(g, metrics, "a-closeness")
coreness <- resilience(g, metrics, "a-coreness")
eigenvector <- resilience(g, metrics, "a-eigenvector")

param_labels <- c("coreness", "betweenness", "degree", "closeness", "eigenvector")
params <- list(coreness$nodes, betweenness$nodes, degree$nodes, closeness$nodes, eigenvector$nodes)
save_graph(file="wordnet_nodes.png", xlabel="Steps", ylabel="Nodes", params, param_labels)
params <- list(coreness$edges, betweenness$edges, degree$edges, closeness$edges, eigenvector$edges)
save_graph(file="wordnet_edges.png", xlabel="Steps", ylabel="Edges", params, param_labels)
params <- list(coreness$apl, betweenness$apl, degree$apl, closeness$apl, eigenvector$apl)
save_graph(file="wordnet_apl.png", xlabel="Steps", ylabel="APL", params, param_labels)
params <- list(coreness$avg_degree, betweenness$avg_degree, degree$avg_degree, closeness$avg_degree, eigenvector$avg_degree)
save_graph(file="wordnet_avg_degree.png", xlabel="Steps", ylabel="Avg. Degree", params, param_labels)
params <- list(coreness$density, betweenness$density, degree$density, closeness$density, eigenvector$density)
save_graph(file="wordnet_density.png", xlabel="Steps", ylabel="Density", params, param_labels)
