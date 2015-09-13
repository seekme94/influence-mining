# Influence Maximization using Pareto principle

# 1. Find communities
# 2. Pick 20% nodes with highest degree from each community
# 3. Calculate the number of nodes to be picked from each community such that they all sum up to k
# 4. For each community, run influence function for random groups for max_iteration
	# If possible combinations is less than max_iterations, then run for all possible combinations (easter egg)
# 5. Select the group that performed the best in single iteration from each community
# 6. Calculate total influence using that set of groups

pareto_maximization <- function(edgesFile, k = 0.1) {
	# Load the required libraries
	library(igraph)
	library(sqldf)
	# Save the start time
	start <- as.numeric(Sys.time())
	# Read graph from file
	graph <- read.graph(edgesFile, directed=FALSE)
	# Save list of nodes
	nodes <- V(graph)
	# Save list of edges
	edges <- E(graph)
	# Declare required objects
	data <- NULL
	seed <- NULL
	active <- NULL
	influence <- 0
	iterations <- 0
	if(k > 0.2) {
		print ("Value of k cannot be greater than 0.2 since this is the limit for Pareto Principle rule")
		return (-1)
	}
	# Save original value of k
	K <- k
	# Choose percentage of nodes as seed if k < 1
	k <- round(length(nodes) * k)
	# Calculate degrees
	degrees <- degree(graph, V(graph), mode="all", loops=FALSE, normalized=FALSE)
	data <- data.frame(node = c(nodes), degree = c(degrees))
	# Find communities
	lec <- leading.eigenvector.community(graph)
	# Create a data set containing nodes, their respective clusters and degree
	clusters <- data.frame(node = c(nodes), cluster = c(lec$membership), degree = c(degrees))
	# Find out number of clusters
	num <- max(lec$membership)
	seed <- NULL
	# For every cluster, select k% nodes so that the total is k% of all nodes
	for (cnt in 1:num) {
		# Get the number of nodes to pick from current cluster
		limit <- round(length(clusters[which(clusters$cluster == cnt),]$node) * K, 0)
		# Create data set of current clusters
		subcluster <- clusters[which(clusters$cluster == cnt),]
		
		# Select nodes with the highest degree
		group <- tail(subcluster[order(subcluster$degree),],limit)$node
		seed <- c(seed, group)
	}
	# Fill non-seed nodes in inactive list
	inactive <- setdiff(nodes, seed)
	# Repeat until no new nodes are being activated
	repeat {
		tried <- NULL
		neighbours <- NULL
		active <- NULL
		for(j in 1:length(inactive)) {
			u <- inactive[[j]]
			neighbours <- neighbors(graph, u)
			neighbours <- which(neighbours %in% seed)
			count <- 0
			if (length(neighbours) == 0) {
				next
			}
			for(l in 1:length(neighbours)) {
				if (l == u) {
					next
				}
				tried <- c(tried, u)
				toss <- sample(c(0,1), 1, replace=TRUE)
				if (toss == 1) {
					count <- count + 1
				}
			}
			x <- 1:degrees[u]
			threshold <- sample(x, 1, replace=TRUE)
			if (count > 0 && count >= threshold) {
				active <- c (active, u)
			}
		}
		seed <- c (seed, active)
		inactive <- setdiff (inactive, tried)
		inactive <- setdiff (inactive, seed)
		influence <- influence + length(active)
		iterations <- iterations + 1
		if(iterations == max_iterations) {
			print(length(inactive))
			break
		}
		if(length(active) == 0 || length(inactive) == 0)
			break
	}
	end <- as.numeric (Sys.time())
	# Summary
	output <- paste("Method:", method, "N:", length(nodes), "E:", length(edges), "Seed Set:", K, "(", k, ")", "Influence:", influence, "Iterations:", iterations, "Time:", (end - start))
	results <- data.frame(edgesFile, method, nodes = length(nodes), edges = length(edges), initial_seed = k, influence, total = (influence + round(k ,0)), iterations, time = (end - start))
	# Write resutls to a CSV file
	write.table(results, outputFile, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
	return (output)
}