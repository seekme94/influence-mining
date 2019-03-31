#' This functions gives a summary of common metrics of given graph
#' @name graph_summary
#' @param graph is the igraph object
#' @param plot uses tkplot to plot the graph. Default is FALSE
#' @return object containing summary
graph_summary <- function(graph, plot=FALSE) {
  o <- NULL
  degrees <- degree(graph)
  o$edges <- ecount(graph)
  o$vertices <- vcount(graph)
  o$vertex_edge_ratio <- o$vertices / o$edges
  o$connected <- is.connected(graph)
  o$average_degree <- mean(degrees)
  o$average_path_length <- average.path.length(graph)
  o$highest_degree <- max(degrees)
  o$density <- graph.density(graph)
  o$diameter <- diameter(graph)
  o$transitivity <- transitivity(graph)
  o$assortativity <- assortativity.degree(graph)
  o$average_distance <- mean_distance(graph)
  o$graph_triads <- length(triangles(graph))
  o$girth <- girth(graph)$girth
  if (plot) {
    tkplot(graph)
    hist(degree(graph))
  }
  o
}

#' Calculates several traits from given graph and returns as data frame
#' @name get_graph_traits
#' @param graph is the igraph object
#' @param normalize uses pnorm function to normalize the traits. Default is FALSE
#' @return data frame containing graph and its traits
get_graph_traits <- function(graph, normalize=FALSE) {
  degrees <- degree(graph)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  data <- data.frame(node=V(graph)$name,
                     degree=degrees,
                     closeness=closeness(graph),
                     betweenness=betweenness(graph),
                     eigenvalue=eigen_centrality(graph)$vector,
                     eccentricity=eccentricity(graph),
                     pagerank=page_rank(graph)$vector,
                     graph_size=vcount(graph),
                     graph_edges=ecount(graph),
                     graph_avg_degree=mean(degrees),
                     graph_max_degree=max(degrees),
                     graph_apl=average.path.length(graph),
                     graph_clust_coef=transitivity(graph),
                     graph_diameter=diameter(graph),
                     graph_density=graph.density(graph),
                     graph_assortativity=assortativity.degree(graph),
                     avg_distance=mean_distance(graph),
                     graph_triads=length(triangles(graph)),
                     graph_girth=girth(graph)$girth)
  if (normalize) {
    for (trait in c("degree","closeness","betweenness","eigenvalue","eccentricity","pagerank")) {
      data[,trait] <- normalize_trait(data[,trait])
    }
  }
  data
}

#' Calculates several node influence traits from given graph and returns as a list
#' @name get_node_influence_traits
#' @param graph is the igraph object
#' @param traits is the vector of several influential metrices (traits). Available metrices are: betweenness, closeness, eigenvalue, coreness, pagerank, ci, a-degree, a-betweenness, a-closeness, a-eigenvalue, a-coreness, a-pagerank, a-ci (a-xxx representing adaptive ranking variant)
#' @param normalize uses pnorm function to normalize the traits. Default is FALSE
#' @return data frame containing graph and its traits
get_node_influence_traits <- function(graph, normalize=FALSE, traits=c("betweenness", "closeness", "eigenvalue", "coreness", "pagerank", "ci", "a-degree", "a-betweenness", "a-closeness", "a-eigenvalue", "a-coreness", "a-pagerank", "a-ci")) {
  data <- NULL
  data$name <- 1:vcount(graph)
  data$degree <- degree(graph)
  if ("betweenness" %in% traits) {
    data$betweenness <- betweenness(graph)
  }
  if ("closeness" %in% traits) {
    data$closeness <- closeness(graph)
  }
  if ("eigenvalue" %in% traits) {
    data$eigenvalue <- evcent(graph)$vector
  }
  if ("coreness" %in% traits) {
    data$coreness <- coreness(graph)
  }
  if ("pagerank" %in% traits) {
    data$pagerank <- page_rank(graph)$vector
  }
  if ("ci" %in% traits) {
    data$ci <- sapply(V(graph), function(x) { collective_influence(graph, neighborhood_distance=2, x) })
  }
  # Normalize existing traits so far, if required
  if (normalize) {
    for (trait in names(data)) {
      data[,trait] <- normalize_trait(data[,trait])
    }
  }
  # Adaptive ranks will not be normalized
  if ("a-degree" %in% traits) {
    data$a_degree <- get_adaptive_ranking(graph, "degree")
  }
  if ("a-betweenness" %in% traits) {
    data$a_betweenness <- get_adaptive_ranking(graph, "betweenness")
  }
  if ("a-closeness" %in% traits) {
    data$a_closeness <- get_adaptive_ranking(graph, "closeness")
  }
  if ("a-eigenvalue" %in% traits) {
    data$a_eigenvalue <- get_adaptive_ranking(graph, "eigenvalue")
  }
  if ("a-coreness" %in% traits) {
    data$a_coreness <- get_adaptive_ranking(graph, "coreness")
  }
  if ("a-pagerank" %in% traits) {
    data$a_pagerank <- get_adaptive_ranking(graph, "pagerank")
  }
  if ("a-ci" %in% traits) {
    data$a_ci <- get_adaptive_ranking(graph, "collective_influence")
  }
  data
}

normalize_trait <- function(x) {
  pnorm(x, mean(x), sd(x))  
}

#' This function can be used to normalize a set of numeric variables in a dataset between 0 and 1. Non-numeric data will be skipped
#' @name normalize_data
#' @param data is data frame to be normalized
#' @param columns is list of columns to be normalized 
normalize_data <- function(data, columns) {
  for (column in columns) {
    # Skip non-numeric data
    if (mode(data[, column]) != "numeric") {
      next
    }
    x <- data[, column]
    x <- pnorm(x, mean(x), sd(x))
    data[, column] <- x
  }
  data
}

#' This function plots degree distribution of given graph
#' @name plot_degree_distribution
#' @param graph is the igraph object
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

#' This function plots degree distribution and returns power-law exponent of given graph
#' @name fit_power_law
#' @param graph is the igraph object
fit_power_law = function(graph) {
  distribution = degree.distribution(graph, mode="all", cumulative=FALSE)
  degree = 1:max(degree(graph, mode="all"))
  probability = distribution[-1]
  # Remove blank values
  nonzero = which(probability != 0)
  probability = probability[nonzero]
  degree = degree[nonzero]
  # plot
  plot(probability ~ degree, log="xy", xlab="Degree (log)", ylab="Probability (log)", col=1, main="Degree Distribution")
  # Return alpha, the exponent of fitted power-law 
  igraph::fit_power_law(degree(graph))[2]
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
#' @name generate_scale_free
#' @param size is the number of nodes
#' @param preference is the power of preferencial attachment. Default is linear, i.e. 1
#' @param directed generates directed graph when TRUE. Default value is FALSE
#' @param allow_cycles produces loops in the graph when TRUE. Default value is FALSE
#' @return igraph object
generate_scale_free <- function(size, preference=1, directed=FALSE, allow_cycles=FALSE) {
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
generate_holme_kim <- function(size, m, triad_prob=0.1, directed=FALSE) {
  hk <- net.holme.kim(size, m, triad_prob)
  mode <- ifelse(directed, "in", "total")
  graph.adjlist(hk, mode="total")
}

net.holme.kim <- function( n, m, pt ){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer", call. = FALSE)
  if (m<1 | m%%1!=0) stop("Parameter 'm' must be integer  greater than 1", call. = FALSE)
  if (pt<0 | pt>1) stop("Parameter 'pt' must be in (0,1)", call. = FALSE)
  neilist <- list()
  neilist[n] <- list(NULL)
  ##Create the m0 graph + (m+1)node
  neilist[[m+1]] <- seq(m)
  for ( k in seq(m)) {
    neilist[[k]] <- m+1
  }
  df <- c( rep(1,m),m,rep(0,n-m-1))
  for (i in (m+2):n){
    pa.neighbor <- sample(seq(n),1,prob = df)
    neilist[[i]] <- pa.neighbor
    neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]],i)
    df[pa.neighbor] <- df[pa.neighbor] + 1
    for (j in seq(2,m) ) {
      pool <- setdiff( neilist[[pa.neighbor]], c(i,neilist[[i]]) )
      if ( stats::runif(1) <= pt && length( pool ) !=0 ) {
        tf.neighbor <- sample(pool, 1)
        neilist[[i]] <- c(neilist[[i]], tf.neighbor)
        neilist[[tf.neighbor]] = c(neilist[[tf.neighbor]],i)
        df[tf.neighbor] <- df[tf.neighbor] + 1
      } else {
        pa.neighbor <- sample( seq(n)[-neilist[[i]]],1,prob = df[-neilist[[i]]] )
        neilist[[i]] <- c(neilist[[i]], pa.neighbor)
        neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]],i)
        df[pa.neighbor] <- df[pa.neighbor] + 1
      }
    }
    df[i] <- m
  }
  neilist
}

#' This function returns largest connected component in a network
#' @name largest_component
#' @param graph is the igraph object
#' @return largest component igraph object
largest_component <- function(graph) {
  gclust = igraph::clusters(graph)
  lcc = induced.subgraph(graph, V(graph)[which(gclust$membership == which.max(gclust$csize))])
  lcc
}

