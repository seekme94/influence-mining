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
  # TODO
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

#' This function returns largest connected component in a network
#' @name largest_component
#' @param graph is the igraph object
#' @return largest component igraph object
largest_component <- function(graph) {
  gclust = clusters(graph)
  lcc = induced.subgraph(graph, V(graph)[which(gclust$membership == which.max(gclust$csize))])
  lcc
}

