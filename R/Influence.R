#' This functions gives a summary of common metrics of given graph
#' 
#' @param graph is the igraph object
#' @param plot uses tkplot to plot the graph. Default is FALSE
#' @return object containing summary
graph_summary <- function(graph, plot=FALSE) {
  o <- NULL
  o$edges <- ecount(graph)
  o$vertices <- vcount(graph)
  o$vertex_edge_ratio <- o$vertices / o$edges
  o$connected <- is.connected(graph)
  o$average_degree <- mean(degree(bg))
  o$average_path_length <- average.path.length(bg)
  o$highest_degree <- max(degree(graph))
  o$density <- graph.density(graph)
  o$diameter <- diameter(graph)
  o$radius <- radius(bg)
  o$transitivity <- transitivity(graph)
  o$assortativity <- assortativity.degree(bg)
  if (plot) {
    tkplot(graph)
  }
  o
}

#' This function is a wrapper for influence_IC and influence_LT functions
#' 
#' @param graph is the igraph object
#' @param seed (optional) is a set of seed (initial nodes). If NULL, then seed_method parameter should be given
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param maximize should be TRUE if influential nodes are to be derived using Greedy algorithm
#' @param seed_method is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
#' @examples
#' influence(G, budget=5, seed=NULL, 5, "LT", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=NULL, 5, "IC", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=c(2,5,9,23), 5, "IC", maximize=FALSE, prob=0.5)
influence <- function (graph, seed=NULL, budget=1, steps=1, model=c("IC", "LT"), maximize=FALSE, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"), prob=0.5) {
  require(igraph)
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

#' This function implements Greedy algorithm for Influence Maximization
#' 
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
#' @examples
#' influence_max(graph, budget=2, steps=5, "LT", prob=0.5)
#' influence_max(graph, budget=5, steps=99, "IC", prob=0.5)
influence_max <- function(graph, budget, steps, model, prob) {
  start <- as.numeric(Sys.time())
  # Save list of nodes
  nodes <- V(graph)
  # Save list of edges
  edges <- E(graph)
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
        output <- influence_IC(graph=graph, seed=c(seed, node), steps=steps, prob=prob)
      }
      else if (model == "LT") {
        output <- influence_LT(graph=graph, seed=c(seed, node), steps=steps, threshold=prob)
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
#' 
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
influence_IC <- function(graph, seed, steps, prob) {
  # Algorithm: Independent Cascade model takes a network (graph) as input and some budget (k).
  # From G, k fraction of nodes are initially activated by some method. Next, we attempt to activate more nodes in the neighbourhood of these nodes.
  # Each active node attempts to activate each of its neighbour nodes with a global probability p (this is 0.5 for coin toss method)
  # Whether an attempt succeeds or fails, a node cannot be attempted twice by any of the active neighbours.
  
  # Save the start time
  start <- as.numeric(Sys.time())
  # Read graph from file
  G <- graph
  # Save list of nodes
  nodes <- V(graph)
  # Save list of edges
  edges <- E(graph)
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
#' 
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param threshold is minimum threshold required to activate a node under observation
#' @return output containing summary
influence_LT <- function(graph, seed, steps, threshold) {
  # Algorithm: Linear Threshold model takes a network (graph) as input and some budget (k).
  # From G, k fraction of nodes are initially activated randomly. Then we attempt to activate more nodes in the neighbourhood of these nodes.
  # A node v actiates only if sum of weights of its active neighbour nodes equals or exceeds its threshold (assigned randomly here).
  # In the given function, if the fraction of active nodes in neighbourhood equals or exceeds the threshold, the inactive node becomes active
  # The process continues for t steps, in each step, the nodes activated in step t-1 also take part in diffusion process
  
  # Save the start time
  start <- as.numeric(Sys.time())
  # Read graph from file
  G <- graph
  # Save list of nodes
  nodes <- V(graph)
  # Save list of edges
  edges <- E(graph)
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

#' This function inputs a graph object, percentage and a seed method and returns k% nodes as seed using given method
#' 
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param seed_method is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"
#' @return set of nodes
select_seed <- function (graph, budget, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
  nodes <- V(graph)
  seed <- NULL
  # Calculate the actual number of nodes to select as seed
  size <- length(nodes) * budget / 100
  if (seed_method == "random") {
    # Select random nodes
    seed <- sample(x=nodes, size)
  }
  else if (seed_method == "degree") {
    # Calculate in/out degrees of all nodes
    degrees <- degree(graph, V(graph), mode="all", loops=FALSE, normalized=FALSE)
    data <- data.frame(node = c(nodes), degree = c(degrees))
    seed <- tail(data[order(data$degree),], size)$node
  }
  else if (seed_method == "closeness") {
    # Calculate in/out closeness of all nodes, normalized between 0 and 1
    closenesses <- closeness(graph, V(graph), mode="all", normalized=TRUE)
    data <- data.frame(node = c(nodes), closeness = c(closenesses))
    seed <- tail(data[order(data$closeness),], size)$node
  }
  else if(seed_method == "betweenness") {
    # Calculate betweenness centrality (for huge data sets, use betweenness.estimate() and give some max value of path length as cutoff)
    betweennesses <- betweenness(graph, V(graph), directed=FALSE)
    data <- data.frame(node = c(nodes), betweenness = c(betweennesses))
    seed <- tail(data[order(data$betweenness),], size)$node
  }
  else if (seed_method == "coreness") {
    # Calculate in/out closeness of all nodes
    coreness <- graph.coreness(graph, mode="all")
    data <- data.frame(node = c(nodes), coreness = c(coreness))
    seed <- tail(data[order(data$coreness),], size)$node
  }
  else if (seed_method == "eigenvector") {
    # Calculate eigenvectors of the graph
    eigen <- evcent(graph, directed=FALSE)
    eigenvectors <- eigen$vector
    eigenvalue <- eigen$value
    data <- data.frame(node = c(nodes), eigenvector = c(eigenvectors))
    # Select nodes with highest eigenvector centralities
    seed <- tail(data[order(data$eigenvector),], size)$node
  }
  else if (seed_method %in% c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
    seed <- select_adaptive_seed(graph, budget, seed_method)
  }
  seed
}

#' Try to improve maximization. NOT TESTED
#' 
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output summary
new_influence_max <- function(graph, budget, steps, model, prob) {
  start <- as.numeric(Sys.time())
  # Save list of nodes
  nodes <- V(graph)
  # Save list of edges
  edges <- E(graph)
  size <- length(nodes) * budget / 100
  influence <- 0
  seed <- NULL
  while (length(seed) < size) {
    max_influence <- 0
    most_influential <- NULL
    # For all nodes except seed
    except <- setdiff(nodes, seed)
    if (model == "IC") {
      influences <- sapply(except, function(x) { influence_IC(graph=graph, seed=c(seed, x), steps=steps, prob=prob) } )
    }
    else if (model == "LT") {
      influences <- sapply(except, function(x) { influence_LT(graph=graph, seed=c(seed, x), steps=steps, threshold=prob) } )
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

#' This function returns largest connected component in a network
#' 
#' @param graph is the igraph object
#' @return largest component igraph object
largest_component <- function(graph) {
  gclust = clusters(graph)
  lcc = induced.subgraph(graph, V(graph)[which(gclust$membership == which.max(gclust$csize))])
  lcc
}

#' This function finds influential nodes in communities in given graph
#' 
community_influence <- function() {
  # TODO
}

#' This method finds communities in the given graph and returns the graph after adding a vector "group" to its vertices
#' 
#' @param graph is the igraph object
#' @param plot whether to plot the graph using tkplot. Default is TRUE
#' @param method is the method to find communities. Value can be "multilevel", "edgebetweenness", "fastgreedy", "eigenvector", "spinglass", "walktrap", "labelpropagation", "clique", "largescale"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently

# "G" is the graph object of igraph library
# "method" is the algorithm for finding communities
find_communities <- function(graph, plot=TRUE, method=c("multilevel", "edgebetweenness", "fastgreedy", "eigenvector", "spinglass", "walktrap", "labelpropagation", "clique", "largescale")) {
  # Based on Louvaine's algorithm; better at scaling and avoids formation of super communities
  if (method == "multilevel") {
    communities <- multilevel.community(graph)
    V(graph)$group <- communities$membership
  }
  else if (method == "edgebetweenness") {
    communities <- edge.betweenness.community(graph)
    V(graph)$group <- communities$membership
  }
  else if (method == "eigenvector") {
    communities <- leading.eigenvector.community(graph)
    V(graph)$group <- communities$membership
  }
  else if (method == "fastgreedy") {
    communities <- fastgreedy.community(graph)
    members <- community.to.membership(graph, communities$merges, steps=which.max(communities$modularity) - 1)
    V(graph)$group <- members$membership
  }
  else if (method == "spinglass") {
    communities <- spinglass.community(graph, spins=10)
    V(graph)$group <- communities$membership
  }
  else if (method == "walktrap") {
    communities <- walktrap.community(graph)
    members <- community.to.membership(graph, communities$merges, steps=which.max(communities$modularity) - 1)
    V(graph)$group <- members$membership
  }
  else if (method == "labelpropagation") {
    V(graph)$group <- label.propagation.community(graph)$membership
  }
  else if (method == "clique") {
    # TODO
  }
  else if (method == "largescale") {
    # TODO
  }
  # Plot the graph, showing communities
  if (plot) {
    graph$layout <- layout.kamada.kawai
    size <- length(unique(V(graph)$group))
    V(graph)$color <- rainbow(size)[V(graph)$group]
    plot(graph)
  }
  graph
}

#' This function detects communities using cliques
#' 
#' @param graph is igraph object
#' @param k is the number of communitites to find
#' @references Palla, Gergely, et al. "Uncovering the overlapping community structure of complex networks in nature and society." Nature 435.7043 (2005): 814-818.
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

#' This function detects communities in large-scale graphs
#' 
#' @param graph is igraph object
#' @param mode is detection mode. Values can be "all", "in" or "out". Default is "all"
#' @references Raghavan, Usha Nandini, Réka Albert, and Soundar Kumara. "Near linear time algorithm to detect community structures in large-scale networks." Physical Review E 76.3 (2007): 036106.
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

#' This function performs a Wilcoxon rank-sum test on the "internal" and "external" degrees of a community in order to quantify its significance. 
#' @description The edges within a community are "internal" and the edges connecting the vertices of a community with the rest of the graph are "external". More internal than external edges show that the community is significant; the otherwise suggests that the community is in fact an "anti-community".
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) {
    stop("This method requires an undirected graph")
  }
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

#' This function inputs an igraph object and returns budget% nodes as seed using the adaptive method provided
#' 
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param seed_method is the selection method for seed (initial nodes). Value can be a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"
#' @return seed object containing set of vertices
select_adaptive_seed <- function (graph, budget, seed_method=c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
  # Add node labels (or the graph resets)
  V(graph)$name <- V(graph)
  seed <- NULL
  # Calculate the actual number of nodes to select as seed
  graph <- largest_component(graph)
  size <- ceiling(length(V(graph)) * budget / 100)
  for (i in 1:size) {
    param <- NULL
    if (seed_method == "a-degree") {
      param <- degree(graph, V(graph), mode="all")
    }
    else if (seed_method == "a-closeness") {
      param <- closeness(graph, V(graph), mode="all")
    }
    else if(seed_method == "a-betweenness") {
      param <- betweenness(graph, V(graph))
    }
    else if (seed_method == "a-coreness") {
      param <- graph.coreness(graph, mode="all")
    }
    else if (seed_method == "a-eigenvector") {
      # Calculate eigenvectors of the graph
      eigen <- evcent(graph, directed=FALSE)
      param <- eigen$vector
    }
    max_node <- which.max(param)
    seed <- c(seed, V(graph)[max_node]$name)
    graph <- delete.vertices(graph, max_node)
  }
  seed
}