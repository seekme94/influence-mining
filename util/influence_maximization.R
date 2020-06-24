#' This function should be called before any other
setup <- function(logging=TRUE) {
  if (logging) {
    require(logging)
    basicConfig()
    addHandler(writeToFile, logger="influence_maximization", file="output.log")
  }
}

#' This function is a wrapper for influence_ic and influence_lt functions
#' @name influence
#' @param graph is the igraph object
#' @param seed (optional) is a set of seed (initial nodes). If NULL, then seed_method parameter should be given
#' @param budget defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
#' @param steps is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
#' @param model is influence model to run the dataset on. Value MUST either be "LT" or "IC"
#' @param maximize should be TRUE if influential nodes are to be derived using Greedy algorithm
#' @param optimal_solution should be TRUE if influential nodes are to be derived using optimal algorithm. Caution! This is the slowest apporach
#' @param seed_method is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"
#' @param prob is the probability of activation of a neighbour node. This is applicable only to IC model currently
#' @param parallel when true, executes the funtion using multiple CPU cores. Default value is FALSE
#' @return output containing summary
#' @examples
#' influence(G, budget=5, seed=NULL, 5, "LT", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=NULL, 5, "IC", maximize=TRUE, seed_method="degree", prob=0.5)
#' influence(G, budget=5, seed=c(2,5,9,23), 5, "IC", maximize=FALSE, prob=0.5)
influence <- function (graph, seed=NULL, budget=1, steps=1, model=c("IC", "LT"), maximize=FALSE, optimal_solution=FALSE, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector"), prob=0.5, parallel=FALSE) {
  loginfo(paste("influence function parameters: seed=", seed, ", budget=", budget, ", steps=", steps, ",model=", model, ", maximize=", maximize, ", optimal_solution=", optimal_solution, ", seed_method=", seed_method, ", prob=", prob, ", parallel=", parallel, sep=''))
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
    if (optimal_solution) {
      optimal_maximization(graph, seed_size=budget, runs=steps, model=model, parallel=parallel)
    }
    else {
      if (parallel) {
        greedy_influence(graph, budget, steps, model, prob=prob)
      }
      else {
        greedy_influence_parallel(graph, budget, steps, model, prob=prob)
      }
    }
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

#' This function implements optimal algorithm for Influence Maximization
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
  require(iterpc)
  start <- as.numeric(Sys.time())
  # Get all combinations
  loginfo("Creating combinations")
  combinations <- getall(iterpc(vcount(graph), seed_size))
  # Add another column to store total spread
  combinations <- cbind(combinations, 0)
  if (parallel) {
    loginfo("Executing optimal_maximization in parallel")
    cores <- detectCores() - 1
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
    loginfo(paste("Calculating spread under", model))
    # foreach requires us to define each packages and function name used within it
    foreach (i = 1:nrow(combinations), .packages=c("igraph"), .export=c("ic_spread","simulate_ic")) %dopar% {
      seed <- combinations[i,1:seed_size]
      if (model == "IC") {
        # Compute spread under IC model in multiple runs
        spread <- ic_spread(graph, seed, candidate=NULL, runs)
      }
      else if (model == "LT") {
        # Compute spread under LT model in multiple runs
        spread <- lt_spread(graph, seed, candidate=NULL, runs)
      }
      # Save spread to last column
      combinations[i,(seed_size + 1)] <- spread
    }
    # Unregister cluster
    registerDoSEQ()
    stopCluster(cl)
  }
  else {
    for (i in 1:nrow(combinations)) {
      seed <- combinations[i,1:seed_size]
      if (model == "IC") {
        # Compute spread under IC model in multiple runs
        spread <- ic_spread(graph, seed, candidate=NULL, runs)
      }
      else if (model == "LT") {
        # Compute spread under LT model in multiple runs
        spread <- lt_spread(graph, seed, candidate=NULL, runs)
      }
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
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
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
    seed <- c(seed, active)
    #print(c("Active in step", t, "=", length(active)))
    influence <- influence + length(active)
  }
  end <- as.numeric (Sys.time())
  # Summary
  output$influence <- influence
  output$time <- (end - start)
  output$activated <- seed
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
#' ic_spread(graph, seed=c(2,5,9,23), 10)
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
  }
  round(total / runs, 5)
}

#' This function calculates spread under IC model
#' @name ic_spread_plus
#' @param graph is the igraph object
#' @param seed is a set of seed (initial nodes)
#' @param runs is the number of times the loop should run
#' @param best_node is the best known node
#' @return output average spread
#' @examples
#' ic_spread_plus(graph, seed=c(2,5,9,23), 10, 2)
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
  # If a coin toss with this probability is successful, then the inactive neighbour gets activated.
  # Once active, a node does not deactivate

  count <- 0
  # If the graph is unweighted, then default the weights to 1
  if (!is_weighted(graph)) {
    E(graph)$weight <- 0.5
  } else {
    E(graph)$weight <- normalize_trait(E(graph)$weight)
  }
  tried <- NULL
  for (i in 1:length(active)) {
    # Get first node
    node <- active[1]
    # Remove this node from active list
    active <- active[-1]
    # Fetch neighbours of node
    neighbour_nodes <- neighbors(graph, node)
    # Remove already activated nodes from neighbours
    neighbour_nodes <- neighbour_nodes[!neighbour_nodes %in% active]
    neighbour_nodes <- neighbour_nodes[!neighbour_nodes %in% tried]
    # Try to activate inactive neighbours according to the weight on edge
    for (j in 1:length(neighbour_nodes)) {
      weight <- E(graph, P=c(node, neighbour_nodes[j]))$weight
      if (runif(1) <= weight) {
        count <- count + 1
      }
      tried <- c(tried, neighbour_nodes[j])
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
#' lt_spread(graph, seed=c(2,5,9,23), 10)
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
simulate_lt <- function(graph, active, threshold=0.5) {
  # Algorithm: given a weighted graph G and a set of active nodes V,
  # each inactive node in the graph gets a chance to be activated with the probability being the collective weights on its edges with active nodes.
  # If a coin toss with probability as the sum of weights of active neighbours is greater than given threshold, then the inactive node gets activated.
  # Once active, a node does not deactivate

  count <- 0
  # If the graph is unweighted, then default the weights to 1
  if (!is_weighted(graph)) {
    E(graph)$weight <- 0.5
  }
  inactive <- unlist(lapply(active, function(active) {neighbors(graph, active)}))
  inactive <- setdiff(inactive, active)
  for (u in inactive) {
    neighbours <- neighbors(graph, u)
    active_neighbours <- intersect(neighbours, active)
    if (length(neighbours) == 0) {
      next
    }
    # If ratio of active nodes in neighbourhood of u is greater than or equal to threshold, then activate u
    ratio <- (length(active_neighbours) / length(neighbours))
    if (ratio >= threshold) {
      count <- count + 1
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

#' This method quantifies the influence of a set of nodes in a graph
#' @name get_influence
#' @param graph is the igraph object
#' @param nodes the set of nodes to calculate influence for
#' @param measure specifies the method to measure influence. Value "RESILIENCE" (see resilience method); "INFLUENCE_IC" (see simulate_ic method); "INFLUENCE_LT" (see simulate_lt method). Default is "RESILIENCE"
#' @return vector of resiliences of provided combinations
get_influence <- function(graph, nodes, measure="RESILIENCE") {
  if (measure == "RESILIENCE") {
    resilience(graph, nodes)
  } else if (measure == "INFLUENCE_IC") {
    simulate_lt(graph, nodes)
  } else if (measure == "INFLUENCE_LT") {
    simulate_ic(graph, nodes)
  }
}

#' This method returns resiliences of all combinations of sets of budget size from given graph
#' @name get_influential_nodes
#' @param graph is the igraph object
#' @param budget defines size of combinations of nodes. Value must be between 0 and 1
#' @param measure specifies the method to measure influence. Value MUST be "RESILIENCE", "INFLUENCE_IC" or "INFLUENCE_LT". Default is "RESILIENCE"
#' @param parallel flag defines whether the execution will use parallel processing or not. Default is FALSE
#' @return vector of resiliences of provided combinations
get_influential_nodes <- function(graph, budget, measure="RESILIENCE", parallel=TRUE) {
  size <- length(V(graph))
  # Fetch all combinations of given budget
  combinations <- getall(iterpc(vcount(graph), round(budget)))
  samples <- 1:nrow(combinations)
  influence_measures <- NULL
  if (parallel) {
    # Initiate parallel processing
    cores <- detectCores() - 2
    cl <- makeCluster(cores)
    # For linux
    if (Sys.info()[[1]] == "Linux") {
      registerDoMC(cores)
      # Loop for each combination in the sample
      influence_measures <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience","largest_component")) %dopar% {
        # Pick a random sample
        seed <- combinations[i, 1:budget]
        # Calculte the resilience after removal of nodes seed
        get_influence(graph, V(graph)[seed], measure=measure)
      }
      stopCluster(cl)
    } else {
      registerDoSNOW(cl)
      # Loop for each combination in the sample
      influence_measures <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience","largest_component")) %dopar% {
        # Pick a random sample
        seed <- combinations[i, 1:budget]
        # Calculte the resilience after removal of nodes seed
        get_influence(graph, V(graph)[seed], measure=measure)
      }
      # Stop parallel processing cluster
      stopCluster(cl)
    }
  } else {
    for(i in samples) {
      # Pick sample
      seed <- combinations[i, 1:budget]
      # Calculte the resilience after removal of nodes seed
      influence_measures <- c(influence_measures, get_influence(graph, V(graph)[seed], measure=measure))
    }
  }
  combinations <- cbind(combinations, unlist(influence_measures))
  top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
  top_nodes
}

#' This method returns resiliences from given graph using greedy approach
#' @name get_influential_nodes_greedy
#' @param graph is the igraph object
#' @param budget defines size of combinations of nodes. Value must be between 0 and 1
#' @param measure specifies the method to measure influence. Value MUST be "RESILIENCE", "INFLUENCE_IC" or "INFLUENCE_LT". Default is "RESILIENCE"
#' @param parallel flag defines whether the execution will use parallel processing or not. Default is FALSE
#' @return vector of resiliences of provided graph
get_influential_nodes_greedy <- function(graph, budget, measure="RESILIENCE") {
  size <- length(V(graph))
  if (budget < 1) {
    budget <- budget * size
  }
  nodes <- V(graph)
  # Run Greedy method
  top_nodes <- NULL
  # While seed < budget
  while (length(top_nodes) < budget) {
    max_influence <- size
    most_influential <- NULL
    output <- NULL
    # For all nodes except seed
    for (node in setdiff(nodes, top_nodes)) {
      # Find influence of node with existing nodes in seed
      output <- get_influence(graph, c(top_nodes, node), measure=measure)
      # If current node causes more influence than maximum so far, then swap
      if ((measure == "RESILIENCE" && output < max_influence) || (measure != "RESILIENCE" && output > max_influence)) {
        most_influential <- node
        max_influence <- output
      }
    }
    # At the end, we should have node with maximum influence to add to seed
    top_nodes <- c(top_nodes, most_influential)
  }
  V(graph)[top_nodes]
}
