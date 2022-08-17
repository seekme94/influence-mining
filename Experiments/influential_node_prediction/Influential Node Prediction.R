###################################
#### Load required resources
###################################

# THIS CODE IS TESTED ON R-3.5

library(igraph)
#devtools::install_github("seekme94/influence.mining")
library(jsonlite)
library(uuid)
library(influence.mining)

library(parallel)
#library(doMC) # For linux
library(doSNOW) # For Windows
# Ubuntu requires apt install build-essential libssl-dev libcurl4-openssl-dev libgmp-dev libcairo2-dev liblzma-dev libblas-dev
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc
library(future.apply)
library(dplyr)
library(centiserve)
library(expm)
library(linkcomm)
library(xgboost)
library(beepr)

source("util/classification_util.R")
root_dir <- "Experiments/influential_node_prediction/"

###################################
#### Experiment functions
###################################
# Get results in a consolidated way
compile_results <- function(uuid, graph, experiment, size, seed, influence_output, test_method) {
  print(influence_output)
  results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', influence_output$time, '","date":"', date(), '","influence":"', influence_output$influence, '","test_method":"', test_method, '","nodes":', toJSON(influence_output$influential_nodes$name), '}', sep='')
  data <- get_graph_traits(graph, normalize=TRUE)
  write.graph(graph, paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".el", sep=''), format='edgelist')
  filename <- paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".csv", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=FALSE, sep=',')
  write.table(results, file=paste(root_dir, "optimal_nodes.json", sep=''), quote=FALSE, row.names=FALSE, append=TRUE)
}

#' This function extends the original experiment by adding more centrality methods for each graph in data directory
compile_extended_results <- function() {
  files <- Sys.glob(paste0(root_dir, "data/graph_1900_93d598ae-a48a-4be5-8622-eef9b4a02436*.el"))
  for (filename in files) {
    print(filename)
    # Open existing CSV file
    csv_file <- sub(".el", ".csv", filename)
    data <- read.csv(csv_file)
    graph <- read.graph(file = filename, format='edgelist', directed=FALSE)
    node_traits=c("AVERAGE_DISTANCE", "BARYCENTER", "CLUSTERRANK", "CROSS_CLIQUE", "CURRENTFLOW_CLOSENESS", "DECAY", "ENTROPY", "FREEMAN_CLOSENESS", "GEODESIC_K_PATH", "KATZ", "LAPLACIAN", "LATORA_CLOSENESS", "LEVERAGE", "LINCENT", "LOBBY", "MARKOV", "MAX_NEIGHBORHOOD_COMPONENT", "MAX_NEIGHBORHOOD_DENSITY", "RADIALITY", "RESIDUAL_CLOSENESS", "TOPOLOGICAL_COEFFICIENT")
    print("Merging")
    for (trait in node_traits) {
      print(trait)
      if (vcount(graph) < nrow(data)) {
        data[, tolower(trait)] <- 0
        next
      }
      set_zero <- (trait == "AVERAGE_DISTANCE" && any(is.loop(graph))) ||
                  (vcount(graph) > 1000 && trait %in% c("CURRENTFLOW_CLOSENESS", "ENTROPY", "CROSS_CLIQUE")) ||
                  (!is.connected(graph, mode="strong") && trait %in% c("AVERAGE_DISTANCE", "BARYCENTER", "CURRENTFLOW_CLOSENESS", "DECAY", "FREEMAN_CLOSENESS"))
      if (set_zero) {
        data[, tolower(trait)] <- 0
      } else {
        scores <- get_centrality_scores(graph, trait, normalize=TRUE)
        scores[is.na(scores)] <- 0 # Deal with NA's
        data[, tolower(trait)] <- scores
      }
    }
    write.table(data, file=csv_file, quote=FALSE, row.names=FALSE, append=FALSE, sep=',')
  }
}


###################################
#### Experiment settings
###################################

# Define parameters
# Repeat experiement 4 times
# 3 for training; 3 for test
seeds <- c(2, 3, 5, 8, 13, 21)
prob <- 0.1

test_method <- "RESILIENCE"
# Repeat experiement for multiple sizes
sizes <- c()#seq(from=15, to=45, by=5)
for (size in sizes) {
  budget_size <- size * prob
  # SCALE FREE
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Influence experiment on Scale free graph ", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=TRUE, test_method=test_method)
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  }
  # SMALL WORLD
  for (seed in seeds) {
    experiment <- paste("Influence experiment on Small world graph ", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=TRUE, test_method=test_method)
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  }
  # HOLME AND KIM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Influence experiment on Holme and Kim graph ", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=TRUE, test_method=test_method)
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  }
}


###################################
#### Greedy approach for large size
###################################

sizes <- c()# seq(from=100, to=2000, by=100)
prob <- 0.025

#** Note: Using future.apply functions to execute in parallel
plan(multiprocess)

for (size in sizes) {
  budget_size <- ceiling(size * prob)
  # SCALE FREE
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Influence experiment on Scale-free graph_", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=FALSE, test_method=test_method, heuristic="GREEDY")
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  })

  # SMALL WORLD
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Influence experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=FALSE, test_method=test_method, heuristic="GREEDY")
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  })

  # HOLME AND KIM
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Influence experiment on Holme and Kim graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph) - 1
    out <- influence(graph, budget_size, optimal_solution=FALSE, test_method=test_method, heuristic="GREEDY")
    compile_results(uuid=UUIDgenerate(), graph, experiment, size, seed, out, test_method)
  })
}


###################################
#### Greedy on Real Datasets
###################################

karate <- largest_component(read.graph("dataset/karate_club.txt", directed=FALSE))
karate$name <- "karate_club"
trade <- largest_component(read.graph("dataset/world_trade.txt", directed=FALSE))
trade$name <- "world_trade"
nematode <- largest_component(read.graph("dataset/nematode_neural_network.txt", directed=FALSE))
nematode$name <- "nematode_neural_network"
football <- largest_component(read.graph("dataset/american_football.txt", directed=FALSE))
football$name <- "american_football"
twitter <- largest_component(read.graph("dataset/twitter_network.txt", directed=FALSE))
twitter$name <- "my_twitter_network"
automata <- largest_component(read.graph("dataset/automata_nlp.txt", directed=FALSE))
automata$name <- "automata_nlp"
blog <- largest_component(read.graph("dataset/political_blog.txt", directed=FALSE))
blog$name <- "political_blog"
citation <- largest_component(read.graph("dataset/influence_citation_network.txt", directed=FALSE, format="ncol"))
citation$name <- "citation_network_influence"
protein <- largest_component(read.graph("dataset/protein_barabasi.txt", directed=FALSE))
protein$name <- "protein_barabasi"
opsahl <- largest_component(read.graph("dataset/opsahl_usairport.txt", directed=FALSE))
opsahl$name <- "opsahl_usairport"
moreno <- largest_component(read.graph("dataset/moreno_health.txt", directed=FALSE))
moreno$name <- "moreno_health"
minnesota <- largest_component(read.graph("dataset/road-minnesota.txt", directed=FALSE))
minnesota$name <- "road-minnesota"
fbmessages <- largest_component(read.graph("dataset/ia-fb-messages.txt", directed=FALSE))
fbmessages$name <- "ia-fb-messages"
maayan <- largest_component(read.graph("dataset/maayan-vidal.txt", directed=FALSE))
maayan$name <- "maayan-vidal"
routers <- largest_component(read.graph("dataset/tech-routers-rf.txt", directed=FALSE))
routers$name <- "tech-routers-rf"
dd6 <- largest_component(read.graph("dataset/DD6.txt", directed=FALSE))
dd6$name <- "dd6"
petster <- largest_component(read.graph("dataset/petster-hamster-friend.txt", directed=FALSE))
petster$name <- "petster-hamster-friend"
delaunay <- largest_component(read.graph("dataset/delaunay_n12.txt", directed=FALSE))
delaunay$name <- "delaunay_n12"
bcspwr10 <- largest_component(read.graph("dataset/power-bcspwr10.txt", directed=FALSE))
bcspwr10$name <- "power-bcspwr10"
fbpages <- largest_component(read.graph("dataset/fb-pages-tvshow-edges.txt", directed=FALSE))
fbpages$name <- "fb-pages-tvshow-edges"
drosophila <- largest_component(read.graph("dataset/bn-fly-drosophila_medulla_1.txt", directed=FALSE))
drosophila$name <- "bn-fly-drosophila_medulla_1"
openflights <- largest_component(read.graph("dataset/inf-openflights.txt", directed=FALSE))
openflights$name <- "inf-openflights"
webspam <- largest_component(read.graph("dataset/web-spam.txt", directed=FALSE))
webspam$name <- "web-spam"
cswikibooks <- largest_component(read.graph("dataset/edit-cswikibooks.txt", directed=FALSE))
cswikibooks$name <- "edit-cswikibooks"
circuit2 <- largest_component(read.graph("dataset/circuit_2.txt", directed=FALSE))
circuit2$name <- "circuit_2"
socfb <- largest_component(read.graph("dataset/socfb-JohnsHopkins55.txt", directed=FALSE))
socfb$name <- "socfb-JohnsHopkins55"
biohs <- largest_component(read.graph("dataset/bio-HS-CX.txt", directed=FALSE))
biohs$name <- "bio-HS-CX"

# graphs <- list(karate, trade, football, nematode, twitter, automata, blog, citation, protein, opsahl, moreno, fbmessages, petster, drosophila, minnesota, maayan, routers, openflights, fbpages, delaunay, cswikibooks, circuit2, dd6, biohs, webspam, socfb, bcspwr10)
graphs <- list()

seed <- 2
budget <- 0.025
for (graph in graphs) {
  print(as.data.frame(graph_summary(graph)))
  nodes <- vcount(graph)
  budget_size <- nodes * budget
  set.seed(seed)
  experiment <- paste("Influence experiment on graph_", graph$name, "(size=", nodes, ",", "probability=", prob, ")", sep='')
  print(experiment)
  V(graph)$name <- 1:vcount(graph) - 1
  out <- influence(graph, budget_size, optimal_solution=FALSE, test_method=test_method, heuristic="GREEDY")
  compile_results(uuid=UUIDgenerate(), graph, experiment, nodes, seed, out, test_method)
}

# Caution! This line below  will compute 30 additional features for each graph in data directory. THIS WILL TAKE LONG TIME
### compile_extended_results()



#################################
#### Read results from all graphs
#################################
# CAUTION! optimal_nodes.json file is not all hunky dory. You'll have to do some manual cleanup to convert into proper JSON array because each row was saved in x = {JSON} per line format
results <- fromJSON(paste(readLines(paste(root_dir, "optimal_nodes.json", sep=''))))

data <- NULL
cores <- 4

# For all rows in results
for (i in 1:nrow(results)) {
  # Graph ID
  graph_id <- paste(root_dir, "data/graph_", results[i, "size"], "_", results[i, "uuid"], sep='')
  filename <- paste(graph_id, ".csv", sep='')
  if (file.exists(filename) == FALSE) {
    print(paste(filename, "does not exist or is unreadable!"))
    next
  }
  # Read the respective graph data
  graph <- read.csv(filename)
  # Add a graph ID column
  graph$graph_id <- graph_id
  # Add a seed column
  graph$seed <- results[i, "seed"]
  # Append a column to label influential nodes
  graph$influential <- 0
  # Extract IDs of influential nodes from results
  influential <- unlist(results[i, "nodes"])
  # Store the influence measure
  graph$influence <- results[i, "influence"]
  # The method used to test influence
  graph$test_method <- results[i, "test_method"]
  # Label these as influential
  graph[influential, "influential"] <- 1
  data <- rbind(data, graph)
}

# View a brief summary of data
length(unique(data$graph_id))

# Prediction formula considering both node and graph traits
node_traits <- c("degree", "betweenness", "closeness", "eigenvector", "eccentricity", "coreness", "pagerank", "ci", "average_distance", "barycenter", "clusterrank", "cross_clique", "currentflow_closeness", "decay", "entropy",
                 "freeman_closeness", "geodesic_k_path", "katz", "laplacian", "latora_closeness", "leverage", "lincent", "lobby", "markov", "max_neighborhood_component", "max_neighborhood_density", "residual_closeness", "topological_coefficient")
graph_traits <- c("graph_size", "graph_edges", "graph_avg_degree", "graph_max_degree", "graph_apl", "graph_clust_coef", "graph_diameter", "graph_density", "graph_assortativity", "graph_avg_distance", "graph_triads", "graph_girth")
columns <- c("name", "graph_id", "seed", "influential", "influence", "test_method", node_traits, graph_traits)

summary(data)
# Split into 50/50 training and test sets
train <- data[data$seed <= 5, ]
test <- data[data$seed > 5, ]

# Influence by network traits
newtest <- NULL
for (graph_id in unique(test$graph_id)) {
  graph <- test[test$graph_id == graph_id,]
  influential_size <- nrow(graph) * 0.1
  # Using dplyr
  { # Label top n high-degree nodes as influential
    inf_by_degree <- head(select(arrange(graph, desc(degree)), name), influential_size)
    graph$inf_by_degree <- 0
    graph$inf_by_degree[graph$name %in% inf_by_degree[[1]]] <- 1
  }
  { # Label top n high-betweenness nodes as influential
    inf_by_betweenness <- head(select(arrange(graph, desc(betweenness)), name), influential_size)
    graph$inf_by_betweenness <- 0
    graph$inf_by_betweenness[graph$name %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-closeness nodes as influential
    inf_by_closeness <- head(select(arrange(graph, desc(closeness)), name), influential_size)
    graph$inf_by_closeness <- 0
    graph$inf_by_closeness[graph$name %in% inf_by_closeness[[1]]] <- 1
  }
  { # Label top n high-eigenvector nodes as influential
    inf_by_eigenvector <- head(select(arrange(graph, desc(eigenvector)), name), influential_size)
    graph$inf_by_eigenvector <- 0
    graph$inf_by_eigenvector[graph$name %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-eccentricity nodes as influential
    inf_by_eccentricity <- head(select(arrange(graph, desc(eccentricity)), name), influential_size)
    graph$inf_by_eccentricity <- 0
    graph$inf_by_eccentricity[graph$name %in% inf_by_eccentricity[[1]]] <- 1
  }
  { # Label top n high-coreness nodes as influential
    inf_by_coreness <- head(select(arrange(graph, desc(coreness)), name), influential_size)
    graph$inf_by_coreness <- 0
    graph$inf_by_coreness[graph$name %in% inf_by_coreness[[1]]] <- 1
  }
  { # Label top n high-pagerank nodes as influential
    inf_by_pagerank <- head(select(arrange(graph, desc(pagerank)), name), influential_size)
    graph$inf_by_pagerank <- 0
    graph$inf_by_pagerank[graph$name %in% inf_by_pagerank[[1]]] <- 1
  }
  { # Label top n high-ci nodes as influential
    inf_by_ci <- head(select(arrange(graph, desc(ci)), name), influential_size)
    graph$inf_by_ci <- 0
    graph$inf_by_ci[graph$name %in% inf_by_ci[[1]]] <- 1
  }
  { # Label top n high-average_distance nodes as influential
    inf_by_average_distance <- head(select(arrange(graph, desc(average_distance)), name), influential_size)
    graph$inf_by_average_distance <- 0
    graph$inf_by_average_distance[graph$name %in% inf_by_average_distance[[1]]] <- 1
  }
  { # Label top n high-barycenter nodes as influential
    inf_by_barycenter <- head(select(arrange(graph, desc(barycenter)), name), influential_size)
    graph$inf_by_barycenter <- 0
    graph$inf_by_barycenter[graph$name %in% inf_by_barycenter[[1]]] <- 1
  }
  { # Label top n high-clusterrank nodes as influential
    inf_by_clusterrank <- head(select(arrange(graph, desc(clusterrank)), name), influential_size)
    graph$inf_by_clusterrank <- 0
    graph$inf_by_clusterrank[graph$name %in% inf_by_clusterrank[[1]]] <- 1
  }
  { # Label top n high-cross_clique nodes as influential
    inf_by_cross_clique <- head(select(arrange(graph, desc(cross_clique)), name), influential_size)
    graph$inf_by_cross_clique <- 0
    graph$inf_by_cross_clique[graph$name %in% inf_by_cross_clique[[1]]] <- 1
  }
  { # Label top n high-currentflow_closeness nodes as influential
    inf_by_currentflow_closeness <- head(select(arrange(graph, desc(currentflow_closeness)), name), influential_size)
    graph$inf_by_currentflow_closeness <- 0
    graph$inf_by_currentflow_closeness[graph$name %in% inf_by_currentflow_closeness[[1]]] <- 1
  }
  { # Label top n high-decay nodes as influential
    inf_by_decay <- head(select(arrange(graph, desc(decay)), name), influential_size)
    graph$inf_by_decay <- 0
    graph$inf_by_decay[graph$name %in% inf_by_decay[[1]]] <- 1
  }
  { # Label top n high-entropy nodes as influential
    inf_by_entropy <- head(select(arrange(graph, desc(entropy)), name), influential_size)
    graph$inf_by_entropy <- 0
    graph$inf_by_entropy[graph$name %in% inf_by_entropy[[1]]] <- 1
  }
  { # Label top n high-freeman_closeness nodes as influential
    inf_by_freeman_closeness <- head(select(arrange(graph, desc(freeman_closeness)), name), influential_size)
    graph$inf_by_freeman_closeness <- 0
    graph$inf_by_freeman_closeness[graph$name %in% inf_by_freeman_closeness[[1]]] <- 1
  }
  { # Label top n high-geodesic_k_path nodes as influential
    inf_by_geodesic_k_path <- head(select(arrange(graph, desc(geodesic_k_path)), name), influential_size)
    graph$inf_by_geodesic_k_path <- 0
    graph$inf_by_geodesic_k_path[graph$name %in% inf_by_geodesic_k_path[[1]]] <- 1
  }
  { # Label top n high-katz nodes as influential
    inf_by_katz <- head(select(arrange(graph, desc(katz)), name), influential_size)
    graph$inf_by_katz <- 0
    graph$inf_by_katz[graph$name %in% inf_by_katz[[1]]] <- 1
  }
  { # Label top n high-laplacian nodes as influential
    inf_by_laplacian <- head(select(arrange(graph, desc(laplacian)), name), influential_size)
    graph$inf_by_laplacian <- 0
    graph$inf_by_laplacian[graph$name %in% inf_by_laplacian[[1]]] <- 1
  }
  { # Label top n high-latora_closeness nodes as influential
    inf_by_latora_closeness <- head(select(arrange(graph, desc(latora_closeness)), name), influential_size)
    graph$inf_by_latora_closeness <- 0
    graph$inf_by_latora_closeness[graph$name %in% inf_by_latora_closeness[[1]]] <- 1
  }
  { # Label top n high-leverage nodes as influential
    inf_by_leverage <- head(select(arrange(graph, desc(leverage)), name), influential_size)
    graph$inf_by_leverage <- 0
    graph$inf_by_leverage[graph$name %in% inf_by_leverage[[1]]] <- 1
  }
  { # Label top n high-lincent nodes as influential
    inf_by_lincent <- head(select(arrange(graph, desc(lincent)), name), influential_size)
    graph$inf_by_lincent <- 0
    graph$inf_by_lincent[graph$name %in% inf_by_lincent[[1]]] <- 1
  }
  { # Label top n high-lobby nodes as influential
    inf_by_lobby <- head(select(arrange(graph, desc(lobby)), name), influential_size)
    graph$inf_by_lobby <- 0
    graph$inf_by_lobby[graph$name %in% inf_by_lobby[[1]]] <- 1
  }
  { # Label top n high-markov nodes as influential
    inf_by_markov <- head(select(arrange(graph, desc(markov)), name), influential_size)
    graph$inf_by_markov <- 0
    graph$inf_by_markov[graph$name %in% inf_by_markov[[1]]] <- 1
  }
  { # Label top n high-max_neighborhood_component nodes as influential
    inf_by_max_neighborhood_component <- head(select(arrange(graph, desc(max_neighborhood_component)), name), influential_size)
    graph$inf_by_max_neighborhood_component <- 0
    graph$inf_by_max_neighborhood_component[graph$name %in% inf_by_max_neighborhood_component[[1]]] <- 1
  }
  { # Label top n high-max_neighborhood_density nodes as influential
    inf_by_max_neighborhood_density <- head(select(arrange(graph, desc(max_neighborhood_density)), name), influential_size)
    graph$inf_by_max_neighborhood_density <- 0
    graph$inf_by_max_neighborhood_density[graph$name %in% inf_by_max_neighborhood_density[[1]]] <- 1
  }
  { # Label top n high-residual_closeness nodes as influential
    inf_by_residual_closeness <- head(select(arrange(graph, desc(residual_closeness)), name), influential_size)
    graph$inf_by_residual_closeness <- 0
    graph$inf_by_residual_closeness[graph$name %in% inf_by_residual_closeness[[1]]] <- 1
  }
  { # Label top n high-topological_coefficient nodes as influential
    inf_by_topological_coefficient <- head(select(arrange(graph, desc(topological_coefficient)), name), influential_size)
    graph$inf_by_topological_coefficient <- 0
    graph$inf_by_topological_coefficient[graph$name %in% inf_by_topological_coefficient[[1]]] <- 1
  }
  newtest <- rbind(newtest, graph)
}


# Returns resutls on test data
get_test_data_results <- function(test, prediction_column) {
  graph_sizes <- unique(test$graph_size[test$graph_size >= 30])
  # Empty data set to contain results
  resultset <- data.frame(size=c(), method=c(), accuracy=c(), pos_pred_value=c(), sensitivity=c(), specificity=c(), f1_score=c())
  for (i in graph_sizes) {
    # Machine learning model
    results <- get_prediction_results(test$influential[test$graph_size == i], test[test$graph_size == i, prediction_column], '1')
    row <- data.frame(size=i, method='ML', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High degree
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_degree[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Degree', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High betweenness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_betweenness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Betweenness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High closeness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_closeness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Closeness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High eigenvector
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_eigenvector[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Eigenvector', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High eccentricity
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_eccentricity[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Eccentricity', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High coreness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_coreness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Coreness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High pagerank
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_pagerank[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Pagerank', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High collective influence
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_ci[test$graph_size == i], '1')
    row <- data.frame(size=i, method='CI', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
  }
  resultset
}


# Calculate traits for test data
selective_node_traits <- c("DEGREE", "BETWEENNESS", "CLOSENESS", "EIGENVECTOR", "ECCENTRICITY", "CROSS_CLIQUE", "CURRENTFLOW_CLOSENESS", "FREEMAN_CLOSENESS", "LAPLACIAN", "LEVERAGE", "LOBBY", "TOPOLOGICAL_COEFFICIENT")
selective_graph_traits <- c("SIZE", "EDGES", "AVERAGE_DEGREE", "MAX_DEGREE", "AVERAGE_PATH_LENGTH", "CLUSTERING_COEFFICIENT", "DIAMETER", "DENSITY", "ASSORTATIVITY", "AVERAGE_DISTANCE", "TRIADS", "GIRTH")

author <- largest_component(read.graph("dataset/author_netscience.txt", directed=FALSE))
ita2000 <- largest_component(read.graph("dataset/ita2000.txt", directed=FALSE))
caida <- largest_component(read.graph("dataset/as-caida.txt", directed=FALSE))
jdk <- largest_component(read.graph("dataset/jdk6_dependencies.txt", directed=FALSE))
#wordnet <- largest_component(read.graph("dataset/wordnet.txt", directed=FALSE))
city2010 <- largest_component(read.graph("dataset/citycity_weighted_2010.csv", directed=FALSE, format = "ncol"))
city2010$name <- "city2010"
V(city2010)$name <- 1:vcount(city2010)
city2013 <- largest_component(read.graph("dataset/citycity_weighted_2013.csv", directed=FALSE, format = "ncol"))
city2013$name <- "city2013"
V(city2013)$name <- 1:vcount(city2013)
city2016 <- largest_component(read.graph("dataset/citycity_weighted_2016.csv", directed=FALSE, format = "ncol"))
city2016$name <- "city2016"
V(city2016)$name <- 1:vcount(city2016)
city2019 <- largest_component(read.graph("dataset/citycity_weighted_2019.csv", directed=FALSE, format = "ncol"))
city2019$name <- "city2019"
V(city2019)$name <- 1:vcount(city2019)

graph_names <- c("author", "ita2000", "caida", "jdk", "city2010", "city2013", "city2016", "city2019")
graphs <- list(author, ita2000, caida, jdk, city2010, city2013, city2016, city2019)

# Calculate traits and save to CSV files
i <- 0
for (graph in graphs) {
  i <- i + 1
  print(graph_names[i])
  data <- get_graph_traits(graph, graph_traits=selective_graph_traits, node_traits=c("DEGREE", "CORENESS", "PAGERANK", "COLLECTIVE_INFLUENCE"), normalize=TRUE)
  for (trait in selective_node_traits) {
    print(trait)
    if (vcount(graph) < nrow(data)) {
      data[, tolower(trait)] <- 0
      next
    }
    set_zero <- (vcount(graph) > 1000 && trait %in% c("CURRENTFLOW_CLOSENESS", "ENTROPY", "CROSS_CLIQUE")) ||
      (!is.connected(graph, mode="strong") && trait %in% c("BARYCENTER", "CURRENTFLOW_CLOSENESS", "DECAY", "FREEMAN_CLOSENESS"))
    if (set_zero) {
      data[, tolower(trait)] <- 0
    } else {
      scores <- get_centrality_scores(graph, trait, normalize=TRUE)
      scores[is.na(scores)] <- 0 # Deal with NA's
      data[, tolower(trait)] <- scores
    }
  }
  filename <- paste(root_dir, "data/", graph_names[i], ".csv", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=FALSE, sep=',')
}


###############################
# Model Learning and Evaluation
###############################

# Normalize data
selected_features <- c("graph_size", "graph_avg_degree", "graph_apl", "graph_clust_coef", "graph_density", "graph_triads",
                       "coreness", "pagerank", "ci", "degree", "betweenness", "closeness", "eigenvector", "cross_clique", "currentflow_closeness", "freeman_closeness", "laplacian", "leverage", "lobby", "topological_coefficient")
selected_nodal_features <- selected_features[sapply(selected_features, function(x) {!startsWith(x, "graph_")})]
test <- newtest
train <- normalize_data(train, columns=selected_nodal_features)
test <- normalize_data(test, columns=selected_nodal_features)

selected_features <- c("graph_size", "graph_avg_degree", "graph_apl", "graph_clust_coef", "graph_density", "graph_triads",
                       "coreness", "pagerank", "ci", "degree", "betweenness", "closeness", "eigenvector", "cross_clique", "currentflow_closeness", "freeman_closeness", "laplacian", "leverage", "lobby", "topological_coefficient")

## Learn prediction model
start <- Sys.time()
# Exclude non-numeric and label data and keep only the feature names in the model
formula <- influential ~ .

xgboost_model <- learn_xgboost_classifier(formula=formula, train=train, label="influential", learning_features=selected_features,
      nrounds=100, nthread=cores, learn_hyperparameters=TRUE, default_params=list(booster="gbtree", objective="binary:logistic",
      eta=0.3, gamma=0, max_depth=3, min_child_weight=1, subsample=1, colsample_bytree=0.5))
test$xgboost_prediction <- round(predict(xgboost_model, as.matrix(test[, xgboost_model$feature_names])))
performance = get_prediction_results(test$influential, test$xgboost_prediction, '1')
print(Sys.time() - start)
print(performance)

importance <- xgb.importance(xgboost_model$feature_names, xgboost_model)
xgb.plot.importance (importance_matrix=importance[1:20])
write.csv(performance, file=paste(root_dir, "xgboost_accuracy.txt", sep=''))
resultset <- get_test_data_results(test, "xgboost_prediction")
write.csv(resultset, file=paste(root_dir, "xgboost_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
xgb.save(xgboost_model, paste(root_dir, "xgboost_model.dat", sep=''))
# Check the number of instances where ML performed better than the rest in terms of accuracy
print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
model <- xgboost_model

################
# Test on graphs
################

# Test the ML model
test_methods <- c("RESILIENCE", "INFLUENCE_IC")
i <- 0
for(graph in graphs) {
  start <- Sys.time()
  i <- i + 1
  print(paste("Graph:", graph_names[i], "Power law coefficient (alpha):", round(fit_power_law(graph)$alpha, 3)))
  test <- read.csv(paste(root_dir, "data/", graph_names[i], ".csv", sep=''))
  # Make predictions using model
  test$prediction_prob <- predict(model, newdata=as.matrix(test[, model$feature_names]))
  test$graph_id <- UUIDgenerate()
  # Influential nodes by all traits
  results <- NULL
  size <- nrow(test) * 0.05
  for (test_method in test_methods) {
    print(test_method)
    i <- 0
    # Random nodes
    results$random <- get_influence(graph, sample(V(graph), size), test_method=test_method)
    # By degree
    inf <- arrange(test, desc(degree))[1:size, "name"]
    results$degree <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By pagerank
    inf <- arrange(test, desc(pagerank))[1:size, "name"]
    results$pagerank <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By coreness
    inf <- arrange(test, desc(coreness))[1:size, "name"]
    results$coreness <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By collective influence
    inf <- arrange(test, desc(ci))[1:size, "name"]
    results$ci <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # Resilience by model. Pick top n by probability
    inf <- arrange(test, desc(prediction_prob))[1:size, "name"]
    results$model <- get_influence(graph, V(graph)[inf], test_method=test_method)
    print(unlist(results))
    print(Sys.time() - start)
  }
}

