###################################
#### Load required resources
###################################

# THIS CODE IS TESTED ON R-3.4.4

library(igraph)
library(parallel)
#library(doMC) # For linux
library(doSNOW) # For Windows
library(foreach)
library(jsonlite)
library(uuid)
# Ubuntu requires apt install build-essential libssl-dev libcurl4-openssl-dev libgmp-dev libcairo2-dev liblzma-dev libblas-dev
library(iterpc)
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc
library(future.apply)

source('util/graph_util.R')
source('util/heuristics.R')
source('util/influence_maximization.R')

root_dir <- "Experiments/influential_node_prediction/"

###################################
#### Experiment functions
###################################

# Get results in a consolidated way
write_results <- function(uuid, graph, results) {
  data <- get_graph_traits(graph)
  write.graph(graph, paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".el", sep=''), format='edgelist')
  filename <- paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".csv", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=FALSE, sep=',')
  write.table(results, file=paste(root_dir, "optimal_nodes.json", sep=''), quote=FALSE, row.names=FALSE, append=TRUE)
}

###################################
#### Experiment settings
###################################

# Define parameters
# Repeat experiement 4 times
# 3 for training; 3 for test
seeds <- c(2, 3, 5, 8, 13, 21)
prob <- 0.1

# Repeat experiement for multiple sizes
sizes <- c()
for (size in sizes) {
  budget <- size * prob
  # SCALE FREE
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale free graph ", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }

  # SMALL WORLD
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph ", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }

  # HOLME AND KIM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Holme and Kim graph ", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
}


###################################
#### Greedy approach for large size
###################################

sizes <- c()
prob <- 0.025

#** Note: Using future.apply functions to execute in parallel
plan(multiprocess)

for (size in sizes) {
  budget <- size * prob
  # SCALE FREE
  #  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })

  # SMALL WORLD
#  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })

  # HOLME AND KIM
#  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Holme and Kim graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })
}


###################################
#### Greedy on Real Datasets
###################################

karate <- largest_component(read.graph("dataset/karate_club.txt", directed=FALSE))
karate$name <- "karate_club"
trade <- largest_component(read.graph("dataset/world_trade.txt", directed=FALSE))
trade$name <- "world_trade"
football <- largest_component(read.graph("dataset/american_football.txt", directed=FALSE))
football$name <- "american_football"
nematode <- largest_component(read.graph("dataset/nematode_neural_network.txt", directed=FALSE))
nematode$name <- "nematode_neural_network"
twitter <- largest_component(read.graph("dataset/my_twitter_network.txt", directed=FALSE))
twitter$name <- "my_twitter_network"
automata <- largest_component(read.graph("dataset/automata_nlp.txt", directed=FALSE))
automata$name <- "automata_nlp"
blog <- largest_component(read.graph("dataset/political_blog.txt", directed=FALSE))
blog$name <- "political_blog"
citation <- largest_component(read.graph("dataset/citation_network_influence.txt", directed=FALSE, format="ncol"))
citation$name <- "citation_network_influence"
protein <- largest_component(read.graph("dataset/protein_barabasi.txt", directed=FALSE))
protein$name <- "protein_barabasi"
opsahl <- largest_component(read.graph("dataset/opsahl_usairport.txt", directed=FALSE))
opsahl$name <- "opsahl_usairport"
moreno <- largest_component(read.graph("dataset/moreno_health.txt", directed=FALSE))
moreno$name <- "moreno_health"
arxiv <- largest_component(read.graph("dataset/arxiv_collaboration.txt", directed=FALSE))
arxiv$name <- "arxiv_collaboration"
linux <- largest_component(read.graph("dataset/linux_code.txt", directed=FALSE))
linux$name <- "linux_code"

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

graphs <- list(karate, trade, football, nematode, twitter, automata, blog, citation, protein, opsahl, moreno, arxiv, minnesota, fbmessages, maayan,
               routers, dd6, petster, delaunay, bcspwr10, fbpages, drosophila, openflights, cswikibooks, webspam, circuit2, socfb, biohs)

seed <- 2
for (graph in graphs) {
  #print(as.data.frame(graph_summary(graph)))
  size <- vcount(graph)
  prob <- 0.1
  if (size > 50) {
    prob <- 0.025
  }
  budget <- size * prob
  set.seed(seed)
  experiment <- paste("Resilience experiment on graph_", graph$name, "(size=", size, ",", "probability=", prob, ")", sep='')
  print(experiment)
  V(graph)$name <- 1:vcount(graph) - 1
  start <- as.numeric(Sys.time())
  top_nodes <- get_influential_nodes_greedy(graph, budget)
  end <- as.numeric(Sys.time())
  min_resilience <- resilience(graph, top_nodes)
  uuid <- UUIDgenerate()
  results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(as.numeric(top_nodes$name)), '}', sep='')
  write_results(uuid=uuid, graph=graph, results=results)
}
