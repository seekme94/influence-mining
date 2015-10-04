# Author: owaishussain@outlook.com

library(igraph)

# This function is a wrapper for influence_IC and influence_LT functions
# "graph" is the graph object
# "seed" (optional) is a set of seed (initial nodes). If NULL, then seed_method parameter should be given
# "budget" defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
# "steps" is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
# "model" is influence model to run the dataset on. Value MUST either be "LT" or "IC"
# "maximize" should be TRUE if influential nodes are to be derived using Greedy algorithm
# "seed_method" is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness" or "eigenvector"
# "prob" is the probability of activation of a neighbour node. This is applicable only to IC model currently
influence <- function (graph, seed=NULL, budget=1, steps=1, model=c("IC", "LT"), maximize=FALSE, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"), prob=0.5) {
  # In case of influence maximization
  if (maximize) {
    influence_max(graph, budget, steps, model, prob=prob)
  }
  else {
    # Initially, select budget% seed nodes if not provided
    if (is.null(seed)) {
      seed <- select_seed(graph, budget, seed_method)
    }
    # Independent cascade model
    if (model == "IC") {
      influence_IC(graph, seed, steps, prob)
    }
    # Linear threshold model
    else if (model == "LT") {
      influence_LT(graph, seed, steps, threshold=prob)
    }
  }
}

# This function implements Greedy algorithm for Influence Maximization
# "G" is the graph object
# "budget" defines what percentage of most influential nodes out of all nodes is required as output
# "steps" is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100)
# "model" is influence model to run the dataset on. Value MUST either be "LT" or "IC"
# "prob" is the probability of activation of a neighbour node. This is applicable only to IC model currently
influence_max <- function(G, budget, steps, model, prob) {
  start <- as.numeric(Sys.time())
  # Save list of nodes
  nodes <- V(G)
  # Save list of edges
  edges <- E(G)
  size <- length(nodes) * budget / 100
  influence <- 0
  seed <- NULL
  while (length(seed) < size) {
    max_influence <- 0
    most_influential <- NULL
    # For all nodes except seed
    for (node in setdiff(nodes, seed)) {
      # Find infuence of node with existing nodes in seed
      output <- NULL
      if (model == "IC") {
        output <- influence_IC(graph=G, seed=c(seed, node), steps=steps, prob=prob)
      }
      else if (model == "LT") {
        output <- influence_LT(graph=G, seed=c(seed, node), steps=steps, threshold=prob)
      }
      # If current node causes more influence than maximum so far, then swap
      if (output$influence > max_influence) {
        #print (paste("More influential node found:", node, "current influence", output$influence, "previous", max_influence))
        most_influential <- node
        max_influence <- output$influence
      }
    }
    # At the end, we should have node with maximum influence to add to seed
    seed <- c(seed, most_influential)
  }
  end <- as.numeric(Sys.time())
  out <- NULL
  out$nodes <- length(nodes)
  out$edges <- length(edges)
  out$seed_size <- size
  out$influential_nodes <- seed
  out$time <- end - start
  out
}

# This function calculates influence of k nodes under Independent Cascade model
# "graph" is the graph object
# "seed" is the initial seed passed
# "steps" is the number of time steps the function should repeat till
# "prob" is the probability of activation of a neighbour node. This is applicable only to IC model currently
influence_IC <- function(graph, seed, steps, prob) {
  # Algorithm: Independent Cascade model takes a network (G) as input and some budget (k).
  # From G, k fraction of nodes are initially activated by some method. Next, we attempt to activate more nodes in the neighbourhood of these nodes.
  # Each active node attempts to activate each of its neighbour nodes with a global probability p (this is 0.5 for coin toss method)
  # Whether an attempt succeeds or fails, a node cannot be attempted twice by any of the active neighbours.
  
  # Save the start time
  start <- as.numeric(Sys.time())
  # Read graph from file
  G <- graph
  # Save list of nodes
  nodes <- V(G)
  # Save list of edges
  edges <- E(G)
  influence <- 0
  output <- NULL
  output$initial_seed <- c(seed)
  attempted <- seed
  for (t in 1:steps) {
    # If all nodes have been attempted, then break
    if (length(attempted) >= length(nodes) - length(seed)) {
      break
    }
    active <- NULL
    for (v in seed) {
      # Select all neighbours of v, exempting nodes that have already been attempted
      neighbours <- setdiff(neighbors(G, v), attempted)
      if(length(neighbours) == 0) {
        next
      }
      # Store all nodes in active that had successful trial
      activated <- unlist(lapply(neighbours, function(neighbours) 
        if (runif(1) >= (1 - prob)) {neighbours}))
      attempted <- unique(c(attempted, neighbours))
      active <- c(active, activated)
    }
    seed <- active
    #print(c("Active in step", t, "=", length(active)))
    influence <- influence + length(seed)
  }
  end <- as.numeric (Sys.time())
  # Summary
  output$nodes <- length(nodes)
  output$edges <- length(edges)
  output$influence <- influence
  output$time <- (end - start)
  output
}

# This function calculates influence of k nodes under Linear Threshold model
# "graph" is the graph object of igraph library
# "seed" is the initial seed
# "steps" is the number of time steps the function should repeat till
# "threshold" is minimum threshold required to activate a node under observation
influence_LT <- function(graph, seed, steps, threshold) {
  # Algorithm: Linear Threshold model takes a network (G) as input and some budget (k).
  # From G, k fraction of nodes are initially activated randomly. Then we attempt to activate more nodes in the neighbourhood of these nodes.
  # A node v actiates only if sum of weights of its active neighbour nodes equals or exceeds its threshold (assigned randomly here).
  # In the given function, if the fraction of active nodes in neighbourhood equals or exceeds the threshold, the inactive node becomes active
  # The process continues for t steps, in each step, the nodes activated in step t-1 also take part in diffusion process
  
  # Save the start time
  start <- as.numeric(Sys.time())
  # Read graph from file
  G <- graph
  # Save list of nodes
  nodes <- V(G)
  # Save list of edges
  edges <- E(G)
  influence <- 0
  output <- NULL
  output$initial_seed <- c(seed)
  attempted <- seed
  activated <- NULL
  for (t in 1:steps) {
    # If all nodes have been attempted, then break
    if (length(attempted) >= length(nodes) - length(seed)) {
      break
    }
    active <- NULL
    # Select all nodes having at least one neighbour in seed nodes
    inactive <- unlist(lapply(seed, function(seed) {neighbors(G, seed)}))
    # Remove nodes that have already been attempted
    inactive <- setdiff(inactive, attempted)
    # Filter out duplicates
    inactive <- unique(inactive)
    for (u in inactive) {
      # Every seed node in the neighbourhood will attempt to activate u with probability p
      neighbours <- neighbors(G, u)
      active_neighbours <- intersect(neighbours, seed)
      if (length(neighbours) == 0) {
        next
      }
      # If ratio of active nodes in neighbourhood of u is greater than or equal to threshold, then activate u
      if ((length(active_neighbours) / length(neighbours)) <= threshold) {
        active <- c(active, u)
      }
      # Active or not, this node has been attempted
      attempted <- c(attempted, u)
    }
    #print (paste("Attempted on in this step:", length(inactive), "Activated:", length(active)))
    activated <- c(activated, active)
    seed <- active
  }
  end <- as.numeric (Sys.time())
  # Summary
  output$nodes <- length(nodes)
  output$edges <- length(edges)
  output$influence <- length(activated)
  output$time <- (end - start)
  output
}

# This function inputs a graph object G, k as percentage and a seed method and returns k% nodes as seed using the method given
select_seed <- function (G, k, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
  nodes <- V(G)
  seed <- NULL
  # Calculate the actual number of nodes to select as seed
  size <- length(nodes) * k / 100
  if (seed_method == "random") {
    # Select random nodes
    seed <- sample(x=nodes, size)
  }
  else if (seed_method == "degree") {
    # Calculate in/out degrees of all nodes
    degrees <- degree(G, V(G), mode="all", loops=FALSE, normalized=FALSE)
    data <- data.frame(node = c(nodes), degree = c(degrees))
    seed <- tail(data[order(data$degree),], size)$node
  }
  else if (seed_method == "closeness") {
    # Calculate in/out closeness of all nodes, normalized between 0 and 1
    closenesses <- closeness(G, V(G), mode="all", normalized=TRUE)
    data <- data.frame(node = c(nodes), closeness = c(closenesses))
    seed <- tail(data[order(data$closeness),], size)$node
  }
  else if(seed_method == "betweenness") {
    # Calculate betweenness centrality (for huge data sets, use betweenness.estimate() and give some max value of path length as cutoff)
    betweennesses <- betweenness(G, V(G), directed=FALSE)
    data <- data.frame(node = c(nodes), betweenness = c(betweennesses))
    seed <- tail(data[order(data$betweenness),], size)$node
  }
  else if (seed_method == "coreness") {
    # Calculate in/out closeness of all nodes
    coreness <- graph.coreness(G, mode="all")
    data <- data.frame(node = c(nodes), coreness = c(coreness))
    seed <- tail(data[order(data$coreness),], size)$node
  }
  else if (seed_method == "eigenvector") {
    # Calculate eigenvectors of the graph
    eigen <- evcent(G, directed=FALSE)
    eigenvectors <- eigen$vector
    eigenvalue <- eigen$value
    data <- data.frame(node = c(nodes), eigenvector = c(eigenvectors))
    # Select nodes with highest eigenvector centralities
    seed <- tail(data[order(data$eigenvector),], size)$node
  }
  else if (seed_method %in% c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
    seed <- select_adaptive_seed(G, k, seed_method)
  }
  seed
}

## Trying to improve maximization. NOT TESTED
new_influence_max <- function(G, budget, steps, model, prob) {
  start <- as.numeric(Sys.time())
  # Save list of nodes
  nodes <- V(G)
  # Save list of edges
  edges <- E(G)
  size <- length(nodes) * budget / 100
  influence <- 0
  seed <- NULL
  while (length(seed) < size) {
    max_influence <- 0
    most_influential <- NULL
    # For all nodes except seed
    except <- setdiff(nodes, seed)
    if (model == "IC") {
      influences <- sapply(except, function(x) { influence_IC(graph=G, seed=c(seed, x), steps=steps, prob=prob) } )
    }
    else if (model == "LT") {
      influences <- sapply(except, function(x) { influence_LT(graph=G, seed=c(seed, x), steps=steps, threshold=prob) } )
    }
    # Since sapply returns in swapped dimensions, thus a workaround
    df <- data.frame(initial_seed=unlist(influences["initial_seed",]), influence=unlist(influences["influence",]))
    df <- df[!df$initial_seed %in% seed,]
    max_influence_seed <- df$initial_seed[df$influence == max(df$influence)]
    # Remove seed nodes and pick first node (in case there are multiple with same influence)
    seed <- c(seed, max_influence_seed)
  }
  end <- as.numeric(Sys.time())
  out <- NULL
  out$nodes <- length(nodes)
  out$edges <- length(edges)
  out$seed_size <- size
  out$influential_nodes <- seed
  out$time <- end - start
  out
}

# This function returns largest connected component in a network
largest_component <- function(G) {
  gclust = clusters(G)
  lcc = induced.subgraph(G, V(G)[which(gclust$membership == which.max(gclust$csize))])
  lcc
}

# This function finds influential nodes in communities in given graph
community_influence <- function() {
  # TODO
}

# This method finds communities in the given graph and returns the graph after adding a vector "group" to its vertices
# "G" is the graph object of igraph library
# "method" is the algorithm for finding communities
find_communities <- function(G, plot=TRUE, method=c("multilevel", "edgebetweenness", "fastgreedy", "eigenvector", "spinglass", "walktrap", "labelpropagation", "clique", "largescale")) {
  # Based on Louvaine's algorithm; better at scaling and avoids formation of super communities
  if (method == "multilevel") {
    communities <- multilevel.community(G)
    V(G)$group <- communities$membership
  }
  else if (method == "edgebetweenness") {
    communities <- edge.betweenness.community(G)
    V(G)$group <- communities$membership
  }
  else if (method == "eigenvector") {
    communities <- leading.eigenvector.community(G)
    V(G)$group <- communities$membership
  }
  else if (method == "fastgreedy") {
    communities <- fastgreedy.community(G)
    members <- community.to.membership(G, communities$merges, steps=which.max(communities$modularity) - 1)
    V(G)$group <- members$membership
  }
  else if (method == "spinglass") {
    communities <- spinglass.community(G, spins=10)
    V(G)$group <- communities$membership
  }
  else if (method == "walktrap") {
    communities <- walktrap.community(G)
    members <- community.to.membership(G, communities$merges, steps=which.max(communities$modularity) - 1)
    V(G)$group <- members$membership
  }
  else if (method == "labelpropagation") {
    V(G)$group <- label.propagation.community(G)$membership
  }
  else if (method == "clique") {
    
  }
  else if (method == "largescale") {
    
  }
  # Plot the graph, showing communities
  if (plot) {
    G$layout <- layout.kamada.kawai
    size <- length(unique(V(G)$group))
    V(G)$color <- rainbow(size)[V(G)$group]
    plot(G)
  }
  G
}

# Community detection algorithm by Palla et al.
# Palla, Gergely, et al. "Uncovering the overlapping community structure of complex networks in nature and society." Nature 435.7043 (2005): 814-818.
clique.community <- function(graph, k) {
  clq <- cliques(graph, min=k, max=k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges <- c(edges, c(i,j))
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  lapply(comps, function(x) { unique(unlist(clq[ V(x)$name ])) })
}

# Community detection 
# Raghavan, Usha Nandini, Réka Albert, and Soundar Kumara. "Near linear time algorithm to detect community structures in large-scale networks." Physical Review E 76.3 (2007): 036106.
large.scale.community <- function(graph, mode="all") {
  V(graph)$group <- as.character(V(graph))
  thisOrder <- sample(vcount(graph), vcount(graph))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t + 1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups              
    for(v in thisOrder){
      ## get the neighbor group frequencies:
      groupFreq <- table(V(graph)[neighbors(graph, v, mode=mode)]$group)
      ## pick one of the most frequent:
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done) {
        done <- newGroup == V(graph)[v]$group
      }
      V(graph)[v]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:                              
  for(i in unique(V(graph)$group)) {
    ## only bother for connected groups                                         
    if(!is.connected(subgraph(graph, V(graph)[group==i]))) {
      theseNodes <- V(graph)[group==i]
      theseClusters <- clusters(subgraph(graph, theseNodes))
      ## iterate through the clusters and append their names                    
      for(j in unique(theseClusters$membership)) {
        V(graph)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  graph
}

# This function performs a Wilcoxon rank-sum test on the "internal" and "external" degrees of a community in order to quantify its significance. 
# The edges within a community are "internal" and the edges connecting the vertices of a community with the rest of the graph are "external". 
# More internal than external edges show that the community is significant; the otherwise suggests that the community is in fact an "anti-community".
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) {
    stop("This method requires an undirected graph")
  }
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

# This function inputs a graph object G, k as percentage and a seed method and returns k% nodes as seed using the adaptive method provided
# If the network is disconnected, then the function picks only the largest component in each iteration
select_adaptive_seed <- function (G, k, seed_method=c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
  seed <- NULL
  # Calculate the actual number of nodes to select as seed
  G <- largest_component(G)
  size <- length(V(G)) * k / 100
  if (seed_method == "a-degree") {
    while (length(seed) < size) {
      degree <- degree(G, V(G), mode="all")
      max_node <- which.max(degree)
      seed <- c(seed, max_node)
      G <- delete.vertices(G, max_node)
      G <- largest_component(G)
    }
  }
  else if (seed_method == "a-closeness") {
    while (length(seed) < size) {
      closeness <- closeness(G, V(G), mode="all")
      max_node <- which.max(closeness)
      seed <- c(seed, max_node)
      G <- delete.vertices(G, max_node)
      G <- largest_component(G)
    }
  }
  else if(seed_method == "a-betweenness") {
    while (length(seed) < size) {
      betweenness <- betweenness(G, V(G))
      max_node <- which.max(betweenness)
      seed <- c(seed, max_node)
      G <- delete.vertices(G, max_node)
      G <- largest_component(G)
    }
  }
  else if (seed_method == "a-coreness") {
    while (length(seed) < size) {
      coreness <- graph.coreness(G, mode="all")
      max_node <- which.max(coreness)
      seed <- c(seed, max_node)
      G <- delete.vertices(G, max_node)
      G <- largest_component(G)
    }
  }
  else if (seed_method == "a-eigenvector") {
    while (length(seed) < size) {
      # Calculate eigenvectors of the graph
      eigen <- evcent(G, directed=FALSE)
      eigenvectors <- eigen$vector
      eigenvalue <- eigen$value
      max_node <- which.max(eigenvectors)
      seed <- c(seed, max_node)
      G <- delete.vertices(G, max_node)
      G <- largest_component(G)
    }
  }
  seed
}