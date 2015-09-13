# Author: owaishussain@outlook.com

library(igraph)

# This function is a wrapper for influence_IC and influence_LT functions
# "graph" is the graph object
# "budget" defines what percentage of most influential nodes out of all nodes is required as output. Default value is 1
# "steps" is the time steps for which, the diffusion process should run. If exhaustive run is required, provide a high value (like 100). Default value is 1
# "model" is influence model to run the dataset on. Value MUST either be "LT" or "IC"
# "maximize" should be TRUE if influential nodes are to be derived using Greedy algorithm
# "seed_method" is the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness" or "eigenvector"
# "prob" is the probability of activation of a neighbour node. This is applicable only to IC model currently
influence <- function (graph, budget=1, steps=1, model=c("IC", "LT"), maximize=FALSE, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector"), prob=0.5) {
    # In case of influence maximization
    if (maximize) {
        influence_max(graph, budget, steps, model, prob=prob)
    }
    else {
        seed <- NULL
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
select_seed <- function (G, k, seed_method=c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector")) {
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