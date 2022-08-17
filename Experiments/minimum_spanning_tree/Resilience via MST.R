library(igraph)
library(readxl)
library(influence.mining)

root_dir <- "Experiments/minimum_spanning_tree/"

author <- largest_component(read.graph("dataset/author_netscience.txt", directed=FALSE))
protein <- largest_component(read.graph("dataset/protein_barabasi.txt", directed=FALSE))
influence_citation <- largest_component(read.graph("dataset/influence_citation_network.txt", directed=FALSE, format = "ncol"))
hamster <- largest_component(read.graph("dataset/petster-hamster-friend.txt", directed=FALSE, format = "ncol"))
moreno <- largest_component(read.graph("dataset/moreno_health.txt", directed=FALSE, format = "ncol"))
political <- largest_component(read.graph("dataset/political_blog.txt", directed=FALSE, format = "ncol"))
usairport <- largest_component(read.graph("dataset/opsahl_usairport.txt", directed=FALSE, format = "ncol"))
city2010 <- largest_component(read.graph("dataset/city2010.txt", directed=FALSE, format = "ncol"))
city2013 <- largest_component(read.graph("dataset/city2013.txt", directed=FALSE, format = "ncol"))
city2016 <- largest_component(read.graph("dataset/city2016.txt", directed=FALSE, format = "ncol"))
city2019 <- largest_component(read.graph("dataset/city2019.txt", directed=FALSE, format = "ncol"))
spam <- largest_component(read.graph("dataset/web-spam.txt", directed=FALSE, format = "ncol"))
caida <- largest_component(read.graph("dataset/as-caida.txt", directed=FALSE))
arxiv <- largest_component(read.graph("dataset/arxiv_collaboration.txt", directed=FALSE, format = "ncol"))
ita2000 <- largest_component(read.graph("dataset/ita2000.txt", directed=FALSE))
jdk6 <- largest_component(read.graph("dataset/jdk6_dependencies.txt", directed=FALSE))
johnshopkins <- largest_component(read.graph("dataset/socfb-JohnsHopkins55.txt", directed=FALSE))
linux <- largest_component(read.graph("dataset/linux_code.txt", directed=FALSE))
youtube <- largest_component(read.graph("dataset/youtube_groupmembers.txt", directed=FALSE))
epinions <- largest_component(read.graph("dataset/epinions.txt", directed=FALSE))
wordnet <- largest_component(read.graph("dataset/wordnet.txt", directed=FALSE))

author$name <- "author"
protein$name <- "protein"
influence_citation$name <- "influence_citation"
hamster$name <- "hamster"
moreno$name <- "moreno"
political$name <- "political_blog"
usairport$name <- "usairport"
city2010$name <- "city2010"
city2013$name <- "city2013"
city2016$name <- "city2016"
city2019$name <- "city2019"
spam$name <- "web_spam"
caida$name <- "caida"
arxiv$name <- "arxiv"
ita2000$name <- "ita2000"
jdk6$name <- "jdk6_dependencies"
johnshopkins$name <- "fb_johnshopkins"
linux$name <- "linux_code"
youtube$name <- "youtube_group"
epinions$name <- "epinions"
wordnet$name <- "wordnet"

graphs <- list(author, protein, influence_citation, hamster, moreno, political, usairport, spam, city2010, city2013, city2016, city2019, caida,
               arxiv, ita2000, jdk6, johnshopkins, linux, youtube, epinions)

test_methods <- c('INFLUENCE_IC', 'INFLUENCE_LT', 'RESILIENCE')

output_file <- paste0(root_dir, "mst_results.csv")

#' Function to assign synthetic weights to graphs based on highest degree between two nodes of an edge
get_synthetic_weights <- function(graph, reverse_weights=TRUE) {
  df <- get.data.frame(graph, what="edges")
  df$from_degree <- degree(graph, df$from)
  df$to_degree <- degree(graph, df$to)
  df$weight <- as.integer(apply(df, 1, function(x) { max(x[3], x[4]) } ))
  if (reverse_weights) {
    max_weight <- max(df$weight)
    df$weight <- df$weight - max_weight
  }
  df$weight
}


for (test_method in test_methods) {
  results <- data.frame(graph=c(), method=c(), test=c(), budget=c(), influence=c(), time=c(), weighting=c())
  for (graph in graphs) {
    print(paste0('Running for ', graph$name))
    V(graph)$name = V(graph)
    if (!is.weighted(graph)) {
      E(graph)$weight <- get_synthetic_weights(graph, reverse_weights=TRUE)  # reverse edge weights for minimum spanning tree
    }

    start <- as.numeric(Sys.time())
    tree <- mst(graph, weights = E(graph)$weight)
    u <- mean(degree(tree))
    sd <- sd(degree(tree))
    n <- 2
    cutoff <- u + (n * sd)
    seed <- V(tree)[degree(tree) > cutoff]
    k <- length(seed)

    # 2nd degree outliers
    # u2 <- mean(degree(tree, seed))
    # sd2 <- sd(degree(tree, seed))
    # cutoff2 <- u2 + (n * sd2)
    # seed <- seed[degree(tree, seed) > cutoff2]
    # k <- length(seed)

    influence <- get_influence(graph, seed$name, test_method=test_method)
    end <- as.numeric (Sys.time())
    result <- data.frame(graph=graph$name, method='mst', test=test_method, budget=k, influence=influence, time=(end - start))
    results <- rbind(results, result)

    start <- as.numeric (Sys.time())
    inf_ad_degree <- influence(graph, budget = k, test_method = test_method, heuristic = "ADAPTIVE_CENTRALITY", centrality_method = "DEGREE", logging = FALSE)
    end <- as.numeric (Sys.time())
    result <- data.frame(graph=graph$name, method='adaptive_degree', test=test_method, budget=k, influence=inf_ad_degree$influence, time=(end - start))
    results <- rbind(results, result)

    start <- as.numeric (Sys.time())
    inf_pagerank <- influence(graph, budget = k, test_method = test_method, heuristic = "PAGERANK", logging = FALSE)
    end <- as.numeric (Sys.time())
    result <- data.frame(graph=graph$name, method='pagerank', test=test_method, budget=k, influence=inf_pagerank$influence, time=(end - start))
    results <- rbind(results, result)

    start <- as.numeric (Sys.time())
    inf_coreness <- influence(graph, budget = k, test_method = test_method, heuristic = "CORENESS", logging = FALSE)
    end <- as.numeric (Sys.time())
    result <- data.frame(graph=graph$name, method='coreness', test=test_method, budget=k, influence=inf_coreness$influence, time=(end - start))
    results <- rbind(results, result)
  }
  print(results)
  write.table(results, output_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=',')
}
