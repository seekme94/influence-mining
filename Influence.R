#' This function generates a tree-structured graph
#' @name generate_tree
#' @param size is the number of nodes
#' @param children is the number of children each node has (in addition to a parent node)
#' @param direction defines whether the edges are directed inwards, outwards or undirected. Possible values can be 'in', 'out' and 'undirected' (default)
#' @return igraph object
generate_tree <- function(size, children=2, direction='undirected') {
  require(igraph)
  graph.tree(size, children, mode=direction)
}

#' This function generates a ring-structured graph, in which nodes are connected with neighbours within given distance
#' @name generate_ring
#' @param size is the number of nodes
#' @param distance defines maximum distance each node has to its farthest direct neighbour
#' @return igraph object
generate_ring <- function(size, distance) {
  require(igraph)
  connect.neighborhood(graph.ring(size), distance)
}

#' This function generates a fully connected undirected graph
#' @name generate_clique
#' @param size is the number of nodes
#' @return igraph object
generate_clique <- function(size) {
  require(igraph)
  graph.full(size)
}

#' This function generates a Erdos Renyi random graph
#' @name generate_random
#' @param size is the number of nodes
#' @param probability is the probability of edge formation between nodes
#' @param directed generates directed graph when TRUE. Default value is FALSE
#' @param allow_cycles produces loops in the graph when TRUE. Default value is FALSE
#' @return igraph object
generate_random <- function(size, probability=0.2, directed=FALSE, allow_cycles=FALSE) {
  require(igraph)
  erdos.renyi.game(size, probability, directed=directed, loops=allow_cycles)
}

#' This function generates a Watts & Strogatz small-world graph by rewiring a random graph, while keeping the degree distribution consistent
#' @name generate_small_world
#' @param size is the number of nodes
#' @param probability is the probability of edge formation between nodes
#' @param directed generates directed graph when TRUE. Default value is FALSE
#' @param allow_cycles produces loops in the graph when TRUE. Default value is FALSE
#' @return igraph object
generate_small_world <- function(size, probability=0.1, directed=FALSE, allow_cycles=FALSE) {
  require(igraph)
  graph <- generate_random(size, probability, directed, allow_cycles)
  iterations <- size * 10
  rewire(graph, with=keeping_degseq(allow_cycles, niter=iterations))
}

#' This function generates a Barabasi scale-free graph
#' @name generate_ring
#' @param size is the number of nodes
#' @param preference is the power of preferencial attachment. Default is linear, i.e. 1
#' @param directed generates directed graph when TRUE. Default value is FALSE
#' @param allow_cycles produces loops in the graph when TRUE. Default value is FALSE
#' @return igraph object
generate_scale_free <- function(size, preference=1, directed=FALSE, allow_cycles=FALSE) {
  require(igraph)
  barabasi.game(size, power=preference, directed=directed)
}

#' This function generates a Barabasi scale-free graph
#' @name generate_ring
#' @param size is the number of nodes
#' @param preference is the power of preferencial attachment. Default is linear, i.e. 1
#' @param directed generates directed graph when TRUE. Default value is FALSE
#' @param allow_cycles produces loops in the graph when TRUE. Default value is FALSE
#' @return igraph object
generate_watts_strgatz <- function(size, preference=1, directed=FALSE, allow_cycles=FALSE) {
  require(igraph)
  
  barabasi.game(size, power=preference, directed=directed)
}

#' Holme-Kim Network
#'
#' @description Simulate a scale-free network with relatively high clustering, comparing to B-A networks (Holme and Kim, 1999).
#' @param size is the number of nodes of the network
#' @param m is the number of nodes to which a new node connects at each iteration
#' @param triad_prob is Triad formation probability after each preferential attachment mechanism
#' @details The Holme-Kim network model is a simple extension of B-A model. It adds an additional step, called "Triad formation", with the probability \emph{pt} that compensates the low clustering in B-A networks.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @examples x <- generate_holme_kim (1000, 20, 0.1)}
#' @references Holme, Petter, and Beom Jun Kim. "Growing scale-free networks with tunable clustering."Physical review E65, no. 2 (2002): 026107.
#' @export
generate_holme_kim <- function(size, m, triad_prob=0.1) {
  
  neilist <- list()
  neilist[size] <- list(NULL)
  neilist[[m + 1]] <- seq(m)
  for ( k in seq(m)) {
    neilist[[k]] <- m + 1
  }
  df <- c( rep(1, m), m, rep(0, size-m-1))
  for (i in (m + 2) : size) {
    pa.neighbor <- sample(seq(size), 1, prob = df)
    neilist[[i]] <- pa.neighbor
    neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]], i)
    df[pa.neighbor] <- df[pa.neighbor] + 1
    for (j in seq(2, m) ) {
      pool <- setdiff( neilist[[pa.neighbor]], c(i, neilist[[i]]) )
      if (stats::runif(1) <= triad_prob && length(pool) != 0) {
        tf.neighbor <- sample(pool, 1)
        neilist[[i]] <- c(neilist[[i]], tf.neighbor)
        neilist[[tf.neighbor]] = c(neilist[[tf.neighbor]], i)
        df[tf.neighbor] <- df[tf.neighbor] + 1
      } else {
        pa.neighbor <- sample(seq(size)[-neilist[[i]]], 1, prob = df[-neilist[[i]]] )
        neilist[[i]] <- c(neilist[[i]], pa.neighbor)
        neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]], i)
        df[pa.neighbor] <- df[pa.neighbor] + 1
      }
    }
    df[i] <- m
  }
  neilist
}

#' This function plots degree distribution of given graph
plot_degree_distribution <- function(graph) {
  degree = degree(graph, mode="all")
  distribution = degree.distribution(graph, mode="all", cumulative=FALSE)
  degree = 1:max(degree)
  probability = distribution[-1]
  # Remove blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log="xy", xlab="Degree (log)", ylab="Probability (log)", col=1, main="Degree Distribution")
}

fit_power_law = function(graph) {
  degree = degree(graph, mode="all")
  distribution = degree.distribution(graph, mode="all", cumulative=FALSE)
  degree = 1:max(degree)
  probability = distribution[-1]
  # Remove blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # Logistic regression model
  reg = lm(log(probability) ~ log(degree))
  coef = coef(reg)
  power.law.fit = function(x) exp(coef[[1]] + coef[[2]] * log(x))
  alpha = -coef[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log="xy", xlab="Degree (log)", ylab="Probability (log)", col=1, main="Degree Distribution")
  curve(power.law.fit, col="red", add=T, n=length(degree))
}

#' This functions gives a summary of common metrics of given graph
#' @name graph_summary
#' @param graph is the igraph object
#' @param plot uses tkplot to plot the graph. Default is FALSE
#' @return object containing summary
graph_summary <- function(graph, plot=FALSE) {
  o <- NULL
  o$edges <- ecount(graph)
  o$vertices <- vcount(graph)
  o$vertex_edge_ratio <- o$vertices / o$edges
  o$connected <- is.connected(graph)
  o$average_degree <- mean(degree(graph))
  o$average_path_length <- average.path.length(graph)
  o$highest_degree <- max(degree(graph))
  o$density <- graph.density(graph)
  o$diameter <- diameter(graph)
  o$transitivity <- transitivity(graph)
  o$assortativity <- assortativity.degree(graph)
  if (plot) {
    tkplot(graph)
    hist(degree(graph))
  }
  o
}

#' This function is a wrapper for influence_IC and influence_LT functions
#' @name influence
#' @param graph is the igraph object
#' @param seed (optional) is a set of seed (initial nodes). If NULL, then seed_method parameter should be given
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param maximize should be TRUE if influential nodes are to be derived using Greedy algorithm
#' @param seed_method is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' influence(G, budget=5, seed=NULL, 5, "LT", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=NULL, 5, "IC", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=c(2,5,9,23), 5, "IC", maximize=FALSE, prob=0.5)
influence <- function (graph, seed=NULL, budget=1, steps=1, model=c("IC", "LT"), maximize=FALSE, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"), prob=0.5, parallel=FALSE) {
  require(igraph)
  if (parallel) {
    require(snow)
    require(doSNOW)
    require(parallel)
    require(foreach)
    require(igraph)
  }
  # In case of influence maximization
  if (maximize) {
    influence_max_greedy(graph, budget, steps, model, prob=prob, parallel=parallel)
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
#' @name influence_max_greedy
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' influence_max_greedy(graph, budget=2, steps=5, "LT", prob=0.5)
#' influence_max_greedy(graph, budget=5, steps=99, "IC", prob=0.5)
influence_maximization <- function(graph, seed_size, runs=3, parallel=FALSE) {
  start <- as.numeric(Sys.time())
  combinations <- combn(x=V(graph), m=seed_size)
  # Transpose
  combinations <- t(combinations)
  # Add another column to store total spread
  combinations <- cbind(combinations, 0)
  if (parallel) {
    require(parallel)
    cores <- detectCores() - 1
    require(foreach)
    cl <- makeCluster(cores)
    require(doSNOW)
    registerDoSNOW(cl)
    # foreach requires us to define each packages and function name used within it
    foreach (i = 1:nrow(combinations), .packages=c("igraph"), .export=c("ic_spread","simulate_ic")) %dopar% {
      seed <- combinations[i,1:seed_size]
      # Compute average spread under IC model in multiple runs
      spread <- ic_spread(graph, seed, candidate=NULL, runs)
      # Save spread to last column
      combinations[i,(seed_size + 1)] <- spread
    }
    # Unregister cluster
    registerDoSEQ()
    stopCluster(cl)
  } else {
    for (i in 1:nrow(combinations)) {
      seed <- combinations[i,1:seed_size]
      # Compute average spread under IC model in multiple runs
      spread <- ic_spread(graph, seed, candidate=NULL, runs)
      # Save spread to last column
      combinations[i,(seed_size + 1)] <- spread
    }
  }
  end <- as.numeric(Sys.time())
  print(paste("Time:", (end - start)))
  combinations
}

#' This function implements Greedy algorithm for Influence Maximization
#' @name influence_max_greedy
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
#' @examples
#' influence_max_greedy(graph, budget=2, steps=5, "LT", prob=0.5)
#' influence_max_greedy(graph, budget=5, steps=99, "IC", prob=0.5)
influence_max_greedy <- function(graph, budget, steps, model, prob) {
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
    output <- NULL
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
    }
    # If current node causes more influence than maximum so far, then swap
    if (output$influence > max_influence) {
      #print (paste("More influential node found:", node, "current influence", output$influence, "previous", max_influence))
      most_influential <- node
      max_influence <- output$influence
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

#' This function implements Greedy algorithm for Influence Maximization
#' @name influence_max_greedy
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' influence_max_greedy(graph, budget=2, steps=5, "LT", prob=0.5)
#' influence_max_greedy(graph, budget=5, steps=99, "IC", prob=0.5)
influence_max_greedy_parallel <- function(graph, budget, steps, model, prob, parallel=FALSE) {
  require(parallel)
  cores <- detectCores() - 1
  require(foreach)
  cl <- makeCluster(cores)
  require(doSNOW)
  registerDoSNOW(cl)
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
    output <- NULL
    # foreach requires us to define each packages and function name used within it
    foreach (node = setdiff(nodes, seed), .packages=c("igraph"), .export=c("influence_IC","influence_LT")) %dopar% {
      # Find infuence of node with existing nodes in seed
      if (model == "IC") {
        output <- influence_IC(graph=graph, seed=c(seed, node), steps=steps, prob=prob)
      }
      else if (model == "LT") {
        output <- influence_LT(graph=graph, seed=c(seed, node), steps=steps, threshold=prob)
      }
    }
    print(output)
    # If current node causes more influence than maximum so far, then swap
    if (output$influence > max_influence) {
      #print (paste("More influential node found:", node, "current influence", output$influence, "previous", max_influence))
      most_influential <- node
      max_influence <- output$influence
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
  # Unregister cluster
  registerDoSEQ()
  stopCluster(cl)
  out
}

# This function calculates influence of k nodes under Independent Cascade model
#' @name influence_IC
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
influence_IC <- function(graph, seed, steps, prob) {
  # Algorithm: Independent Cascade model takes a network (graph) as input and some budget (k).
  # From G, k fraction of nodes are initially activated by some method. Next, we attempt to activate more nodes in the neighbourhood of these nodes.
  # Each active node attempts to activate each of its neighbour nodes with a global probability p (this is 0.5 for coin toss method)
  # Whether an attempt succeeds or fails, a node cannot be attempted for activation twice by any of the active neighbours.
  
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
#' @name influence_LT
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
      ratio <- (length(active_neighbours) / length(neighbours))
      if (ratio >= threshold) {
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

#' This function calculates influence of k nodes under Weighted Threshold model
#' @name influence_WT
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @return output containing summary
influence_WT <- function(graph, seed, steps) {
}

#' This function inputs a graph object, percentage and a seed method and returns k% nodes as seed using given method
#' @name select_seed
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
    seed <- as.vector(sample(x=nodes, size))
  }
  else if (seed_method == "degree") {
    # Calculate in/out degrees of all nodes
    degrees <- degree(graph, V(graph), mode="all", loops=FALSE, normalized=FALSE)
    data <- as.data.frame(degrees)
    data$nodes <- rownames(data)
    seed <- tail(data[order(data$degrees),], size)$nodes
  }
  else if (seed_method == "closeness") {
    # Calculate in/out closeness of all nodes, normalized between 0 and 1
    closenesses <- closeness(graph, V(graph), mode="all", normalized=TRUE)
    data <- as.data.frame(closenesses)
    data$nodes <- rownames(data)
    seed <- tail(data[order(data$closenesses),], size)$nodes
  }
  else if(seed_method == "betweenness") {
    # Calculate betweenness centrality (for huge data sets, use betweenness.estimate() and give some max value of path length as cutoff)
    betweennesses <- betweenness(graph, V(graph), directed=FALSE)
    data <- as.data.frame(betweennesses)
    data$nodes <- rownames(data)
    seed <- tail(data[order(data$betweennesses),], size)$nodes
  }
  else if (seed_method == "coreness") {
    # Calculate in/out closeness of all nodes
    coreness <- graph.coreness(graph, mode="all")
    data <- as.data.frame(coreness)
    data$nodes <- rownames(data)
    seed <- tail(data[order(data$coreness),], size)$nodes
  }
  else if (seed_method == "eigenvector") {
    # Calculate eigenvectors of the graph
    eigen <- evcent(graph, directed=FALSE)
    eigenvectors <- eigen$vector
    eigenvalue <- eigen$value
    data <- as.data.frame(eigenvectors)
    data$nodes <- rownames(data)
    # Select nodes with highest eigenvector centralities
    seed <- tail(data[order(data$eigenvectors),], size)$nodes
  }
  else if (seed_method %in% c("a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")) {
    seed <- select_adaptive_seed(graph, budget, seed_method)
  }
  as.numeric(seed)
}

#' This function simulates influence spread under Independent Cascade model
#' @name simulate_ic
#' @param graph is the weighted igraph object
#' @param active represents number of active nodes in the graph
#' @return number of nodes activated during simulation
simulate_ic <- function(graph, active) {
  # Algorithm: given a weighted graph G and a set of active nodes V,
  # each node u in V attempts to activate its neighbours with probability equal to the weight on its edge.
  # If a coin toss with this probability is successful, the the inactive neighbour gets activated.
  # Once active, a node does not deactivate
  count <- 0
  for (i in 1:length(active)) {
    # Get first node
    node <- active[1]
    # Fetch neighbours of node
    neighbour_nodes <- neighbors(graph, node)
    # Remove this node from active list
    active <- active[2:length(active)]
    # Remove already activated nodes from neighbours
    neighbour_nodes <- neighbour_nodes[!neighbour_nodes$name %in% active]
    # Try to activate inactive neighbours according to the weight on edge
    for (neighbour in neighbour_nodes) {
      weight <- E(graph, P=c(node, neighbour))$weight
      if (runif(1) <= weight) {
        count <- count + 1
      }
    }
  }
  count
}

#' This function calculates spread under IC model
#' @name ic_spread
#' @param graph is the weighted igraph object
#' @param seed is a set of seed (initial nodes)
#' @param candidate (optional) is seed candidate initially activated
#' @param runs is the number of times the loop should run
#' @return output average spread
#' @examples
#' ic_spread(graph, seed=c(2,5,9,23), 5, 10)
ic_spread <- function (graph, seed, candidate=NULL, runs=100) {
  total <- 0
  for (i in 1:runs) {
    active <- NULL
    count <- 0
    if (!is.null(candidate)) {
      active <- candidate
      count <- count + 1
    }
    # Activate seed nodes
    for (node in seed) {
      count <- count + 1
      active <- c(active, node)
    }
    count <- count + simulate_ic(graph, active);
    total <- total + count;
    #print(paste('Spread for run #', i, count))
  }
  round(total / runs, 5)
}

#' This function calculates spread under IC model
#' @name ic_spread_plus
#' @param graph is the igraph object
#' @param seed is a set of seed (initial nodes)
#' @param candidate (optional) is seed candidate initially activated
#' @param runs is the number of times the loop should run
#' @param current_best is the number of times the loop should run
#' @return output average spread
#' @examples
#' ic_spread(graph, seed=c(2,5,9,23), 5, 10, 2)
ic_spread_plus <- function (graph, seed, candidate=NULL, runs=100, best_node=0) {
  total <- 0
  for (i in 1:runs) {
    active <- NULL
    count <- 0
    if (!is.null(candidate)) {
      active <- candidate
      count <- count + 1
    }
    # Activate seed nodes
    for (node in seed) {
      count <- count + 1
      active <- c(active, node)
    }
    count <- count + simulate_ic(graph, active);
    total <- total + count
    #print(paste('Spread for run #', i, count))
    # Compute next step based on previous best
    if (best_node > 0 & (!best_node %in% active)) {
      active <- c(active, best_node)
      count <- 1
      count <- count + simulate_ic(graph, active);
      total <- total + count
    }
  }
  round(total / runs, 5)
}

#' Try to improve maximization. NOT TESTED
#' @name new_influence_max_greedy
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output summary
new_influence_max_greedy <- function(graph, budget, steps, model, prob) {
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
#' @name largest_component
#' @param graph is the igraph object
#' @return largest component igraph object
largest_component <- function(graph) {
  gclust = clusters(graph)
  lcc = induced.subgraph(graph, V(graph)[which(gclust$membership == which.max(gclust$csize))])
  lcc
}

#' This function finds influential nodes in communities in given graph
#' @name community_influence
community_influence <- function() {
  # TODO
}

#' This method finds communities in the given graph and returns the graph after adding a vector "group" to its vertices
#' @name find_communities
#' @param graph is the igraph object
#' @param plot whether to plot the graph using tkplot. Default is TRUE
#' @param method is the method to find communities. Value can be "multilevel", "edgebetweenness", "fastgreedy", "eigenvector", "spinglass", "walktrap", "labelpropagation", "clique", "largescale"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently

# "G" is the graph object of igraph library
# "method" is the algorithm for finding communities
find_communities <- function(graph, plot=TRUE, method=c("multilevel", "edgebetweenness", "fastgreedy", "eigenvector", "spinglass", "walktrap", "labelpropagation", "clique", "largescale")) {
  # multilevel.community: based on Louvaine's algorithm, it is better at scaling and avoids formation of super communities. In this method, instead of merging communities, nodes are moved between communities such that each node makes a local decision that maximizes its own contribution to the modularity score. 
  # When this procedure gets stuck (i.e. none of the nodes change their membership), then all the communities are collapsed into single nodes and the process continues (thus the name multilevel).
  if (method == "multilevel") {
    communities <- multilevel.community(graph)
    V(graph)$group <- communities$membership
  }
  # edge.betweenness.community: a Hierarchical decomposition process where edges are removed in the decreasing order of their edge betweenness scores. This is motivated by the fact that edges connecting different groups are more likely to be contained in multiple shortest paths simply because in many cases they are the only option to go from one group to another. 
  # This method yields good results but is very slow because of the computational complexity of edge betweenness calculations and because the betweenness scores have to be re-calculated after every edge removal. 
  # Another disadvantage is that it builds a full dendrogram and does not tell where to cut the dendrogram to obtain the final groups, so we use some other measure (e.g. modularity score of the partitions) to decide that
  else if (method == "edgebetweenness") {
    communities <- edge.betweenness.community(graph)
    V(graph)$group <- communities$membership
  }
  # leading.eigenvector.community: a top-down hierarchical approach that optimizes the modularity function. In each step, the graph is split into two parts in a way that the separation itself yields a significant increase in the modularity. The split is determined by evaluating the leading eigenvector of modularity matrix, and there is also a stopping condition which prevents tightly connected groups to be split further. 
  # Due to the eigenvector calculations involved, it might not work on degenerate graphs where the ARPACK eigenvector solver is unstable. On non-degenerate graphs, it is likely to yield a higher modularity score than the fast greedy method, although it is a bit slower.
  else if (method == "eigenvector") {
    communities <- leading.eigenvector.community(graph)
    V(graph)$group <- communities$membership
  }
  # fastgreedy.community: a bottom-up hierarchical approach. It tries to optimize function modularity function in greedy manner. Initially, every vertex belongs to a separate community, and communities are merged iteratively such that each merge is locally optimal (i.e. has high increase in modularity value). 
  # The algorithm stops when it is not possible to increase the modularity any more, so it gives you a grouping as well as a dendrogram. The method is fast and it is the method that is usually tried as a first approximation because it has no parameters to tune. 
  # However, it has a limitation that communities below a given size threshold will always be merged with neighboring communities
  else if (method == "fastgreedy") {
    communities <- fastgreedy.community(graph)
    members <- community.to.membership(graph, communities$merges, steps=which.max(communities$modularity) - 1)
    V(graph)$group <- members$membership
  }
  # spinglass.community: an approach based on Potts model. Each node can be in one of 'c' spin states, and the edges specify which pairs of vertices would prefer to stay in the same spin state and which ones prefer to have different spin states. 
  # The model is then simulated for a given number of steps, and the spin states of the nodes in the end define the communities. 
  # The consequences are that 1) There will never be more than 'c' communities in the end, although you can set c to as high as 200; 2) There may be less than 'c' communities in the end as some of the spin states may become empty; 3) In disconnected networks, it is not guaranteed that nodes in disconencted parts of the networks have different spin states. 
  # The method is not particularly fast and not deterministic, but has a tunable resolution parameter that determines the cluster sizes.
  else if (method == "spinglass") {
    communities <- spinglass.community(graph, spins=10)
    V(graph)$group <- communities$membership
  }
  # walktrap.community: the general idea is that if you perform random walks on the graph, then the walks are more likely to stay within the same community because there are only a few edges that lead outside a given community. 
  # Walktrap runs short random walks of 3-4-5 steps (depending on parameters) and uses the results of these random walks to merge separate communities in a bottom-up manner like fastgreedy.community. We can use the modularity score to select where to cut the dendrogram. 
  # It is a bit slower than the fast greedy approach but also a bit more accurate.
  else if (method == "walktrap") {
    communities <- walktrap.community(graph)
    members <- community.to.membership(graph, communities$merges, steps=which.max(communities$modularity) - 1)
    V(graph)$group <- members$membership
  }
  # label.propagation.community: a simple approach in which every node is assigned one of 'k' labels. The method then proceeds iteratively and re-assigns labels to nodes in a way that each node takes the most frequent label of its neighbors in a synchronous manner. The method stops when the label of each node is one of the most frequent labels in its neighborhood. 
  # It is very fast but yields different results based on the initial configuration (which is decided randomly), therefore it should be run a large number of times before labeling.
  else if (method == "labelpropagation") {
    V(graph)$group <- label.propagation.community(graph)$membership
  }
  else if (method == "clique") {
    # TODO
  }
  else if (method == "largescale") {
    large.scale.community(graph)
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
#' @name clique.community
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
#' @name large.scale.community
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
#' @name community.significance.test
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
#' @name select_adaptive_seed
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
    graph <- largest_component(graph)
  }
  seed
}