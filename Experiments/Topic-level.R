# This function calculates influence of k nodes chosen by some method
# "edgesFile" is full path of the file containing data as edgelist separated by single space
# "dataFile" is full path of the file containing data about the user events, seperated by comma
# "k" is the percentage of nodes to be chosen as initial seed
# "max_iterations" is the maximum number of iterations or steps for which the algorithm will run
# "topic" is the topic of action about which, the influence will be computed

topic_level_func <- function(edgesFile, dataFile, topic, max_iterations, k) {
	# Load the required libraries
	library(igraph)
	library(sqldf)
	start <- as.numeric(Sys.time())
	# Read the graph
	graph <- read.graph(edgesFile, directed=FALSE)
	# Read the user action data
	dataset <- read.table(dataFile, header=TRUE, sep=",")
	# Selecting categories of given topic. (A topic could fall into multiple categories, you know!)
	categories <- sqldf (paste("select distinct category_id from dataset where topic_id=", topic))
	# Create data frame of influence per user per category
	user_influence <- sqldf ("select user_id, category_id, count(topic_id) as influence from dataset group by user_id, category_id")
	nodes <- V(graph)
	edges <- E(graph)
	seed <- NULL
	active <- NULL
	influence <- 0
	iterations <- 0
	K <- k
	k <- round(length(nodes) * k)
	# Select list of users who have performed at least one action of the given category with their influence
	category_users <- sqldf (paste("select distinct user_id, influence from user_influence where category_id in (select distinct category_id from dataset where topic_id=", topic, ")"))
	# Choose k highest influence users
	seed <- unique(tail(category_users[order(category_users$influence),], k)$user_id)
	inactive <- setdiff(nodes, seed)
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
				inf <- user_influence[which(user_influence$user_id == neighbours[l]),]$influence
				count <- count + inf
			}
			if (count > 0) {
				toss <- sample(c(0,1), 1, replace=TRUE)
				if (toss == 1) {
					active <- c (active, u)
				}
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
	output <- paste("N:", length(nodes), "E:", length(edges), "Seed Set:", K, "(", k, ")", "Influence:", influence, "Iterations:", iterations, "Time:", (end - start))
	results <- data.frame(edgesFile, nodes = length(nodes), edges = length(edges), initial_seed = k, influence, total = (influence + round(k ,0)), iterations, time = (end - start))
	return (output)
}

topic_level_func (edgesFile, dataFile, 4, 1, 0.00625);topic_level_func (edgesFile, dataFile, 4, 1, 0.0125);topic_level_func (edgesFile, dataFile, 4, 1, 0.025);
topic_level_func (edgesFile, dataFile, 13, 1, 0.00625);topic_level_func (edgesFile, dataFile, 13, 1, 0.0125);topic_level_func (edgesFile, dataFile, 85, 1, 0.025);
topic_level_func (edgesFile, dataFile, 14, 1, 0.00625);topic_level_func (edgesFile, dataFile, 14, 1, 0.0125);topic_level_func (edgesFile, dataFile, 85, 1, 0.025);
topic_level_func (edgesFile, dataFile, 4, 10, 0.00625);topic_level_func (edgesFile, dataFile, 4, 10, 0.0125);topic_level_func (edgesFile, dataFile, 4, 10, 0.025);
topic_level_func (edgesFile, dataFile, 13, 10, 0.00625);topic_level_func (edgesFile, dataFile, 13, 10, 0.0125);topic_level_func (edgesFile, dataFile, 85, 10, 0.025);
topic_level_func (edgesFile, dataFile, 14, 10, 0.00625);topic_level_func (edgesFile, dataFile, 14, 10, 0.0125);topic_level_func (edgesFile, dataFile, 85, 10, 0.025);
