#' This function is a wrapper for influence_ic and influence_lt functions
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
    greedy_influence(graph, budget, steps, model, prob=prob, parallel=parallel)
  }
  else {
    # Initially, select budget% seed nodes if not provided
    if (is.null(seed)) {
      seed <- select_seed(graph, budget, seed_method)
    }
    # Independent cascade model
    if (model == "IC") {
      influence_ic(graph, seed, steps, prob)
    }
    # Linear threshold model
    else if (model == "LT") {
      influence_lt(graph, seed, steps, threshold=prob)
    }
  }
}

#' This function implements Greedy algorithm for Influence Maximization
#' @name optimal_maximization
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' greedy_influence(graph, budget=2, steps=5, "LT", prob=0.5)
#' greedy_influence(graph, budget=5, steps=99, "IC", prob=0.5)
optimal_maximization <- function(graph, seed_size, runs=3, model=c("IC", "LT"), parallel=FALSE) {
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
#' @name greedy_influence
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary
#' @examples
#' greedy_influence(graph, budget=2, steps=5, "LT", prob=0.5)
#' greedy_influence(graph, budget=5, steps=99, "IC", prob=0.5)
greedy_influence <- function(graph, budget, steps, model, prob) {
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
        output <- influence_ic(graph=graph, seed=c(seed, node), steps=steps, prob=prob)
      }
      else if (model == "LT") {
        output <- influence_lt(graph=graph, seed=c(seed, node), steps=steps, threshold=prob)
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
#' @name greedy_influence
#' @param graph is the igraph object
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' greedy_influence(graph, budget=2, steps=5, "LT", prob=0.5)
#' greedy_influence(graph, budget=5, steps=99, "IC", prob=0.5)
greedy_influence_parallel <- function(graph, budget, steps, model, prob, parallel=FALSE) {
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
    foreach (node = setdiff(nodes, seed), .packages=c("igraph"), .export=c("influence_ic","influence_lt")) %dopar% {
      # Find infuence of node with existing nodes in seed
      if (model == "IC") {
        output <- influence_ic(graph=graph, seed=c(seed, node), steps=steps, prob=prob)
      }
      else if (model == "LT") {
        output <- influence_lt(graph=graph, seed=c(seed, node), steps=steps, threshold=prob)
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
#' @name influence_ic
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @return output containing summary, including no. of nodes activated and time taken
influence_ic <- function(graph, seed, steps, prob) {
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
  output$influence <- influence
  output$time <- (end - start)
  output
}

# This function calculates influence of k nodes under Linear Threshold model
#' @name influence_lt
#' @param graph is the igraph object
#' @param seed is the initial seed nodes passed
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param threshold is minimum threshold required to activate a node under observation
#' @return output containing summary, including no. of nodes activated and time taken
influence_lt <- function(graph, seed, steps, threshold) {
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
  output$influence <- length(activated)
  output$time <- (end - start)
  output
}

#' This function calculates spread under IC model
#' @name ic_spread
#' @param graph is the weighted igraph object
#' @param seed is a set of seed (initial nodes)
#' @param runs is the number of times the loop should run
#' @return output average spread
#' @examples
#' ic_spread(graph, seed=c(2,5,9,23), 5, 10)
ic_spread <- function (graph, seed, runs=100) {
  total <- 0
  for (i in 1:runs) {
    active <- NULL
    count <- 0
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
#' @param runs is the number of times the loop should run
#' @param current_best is the number of times the loop should run
#' @return output average spread
#' @examples
#' ic_spread_plus(graph, seed=c(2,5,9,23), 5, 10, 2)
ic_spread_plus <- function (graph, seed, runs=100, best_node=0) {
  total <- 0
  for (i in 1:runs) {
    active <- NULL
    count <- 0
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

#' This function calculates spread under LT model
#' @name lt_spread
#' @param graph is the weighted igraph object
#' @param seed is a set of seed (initial nodes)
#' @param runs is the number of times the loop should run
#' @return output average spread
#' @examples
#' lt_spread(graph, seed=c(2,5,9,23), 5, 10)
lt_spread <- function (graph, seed, runs=100) {
  total <- 0
  for (i in 1:runs) {
    active <- NULL
    count <- 0
    # Activate seed nodes
    for (node in seed) {
      count <- count + 1
      active <- c(active, node)
    }
    count <- count + simulate_lt(graph, active);
    total <- total + count;
    #print(paste('Spread for run #', i, count))
  }
  round(total / runs, 5)
}

#' This function simulates influence spread under Linear Threshold model
#' @name simulate_lt
#' @param graph is the weighted igraph object
#' @param active represents number of active nodes in the graph
#' @return number of nodes activated during simulation
simulate_lt <- function(graph, active) {
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

#' This function computes resilience of network
#' @name resilience
#' @param graph is the weighted igraph object
#' @param nodes is a set of nodes to check resilience of
#' @return number of remaining nodes in largest connected component
#' @examples
#' resilience(graph, nodes=c(2,5,9,23))
resilience <- function (graph, nodes) {
  graph <- delete.vertices(graph, nodes)
  graph <- largest_component(graph)
  vcount(graph)
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
