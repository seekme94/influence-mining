require(iterpc)
require(foreach)
require(doMC)

source('./graph_util.R')
source('./db_util.R')
source('./heuristics.R')
source('./community_detection.R')
source('./graph_util.R')
source('./influence_maximization.R')

size <- 50
budget <- size * 0.1
prob <- 0.25

# Generate several graphs of various types
random <- generate_random(size, prob)
ring <- generate_ring(size, budget)
clique <- generate_clique(size)
tree <- generate_tree(size, budget)
sf <- generate_scale_free(size, preference=1)
sw <- generate_small_world(size, prob)
hk <- generate_holme_kim(size, budget, prob)

# GREEDY ALGORITHM
greedy <- greedy_influence(random, budget, 99, "LT", prob)


# On random networks
combinations <- getall(iterpc(vcount(random), budget))
# Bind another column to store influence spread
combinations <- cbind(combinations, 0)
nrow(combinations)
graph <- random
max_spread <- 0
for (i in sample(1:nrow(combinations), 100) ) {
  seed <- combinations[i, 1:budget]
  #  spread <- ic_spread(graph, V(graph)[seed], length(seed))
  spread <- influence_ic(graph, seed, budget, prob)$influence
  combinations[i,(budget + 1)] <- spread
  if (spread > max_spread) {
    max_spread <- spread
    print(max_spread)
  }
}

#################################

db <- get_connection()

# Store graphs and nodes
random_uuid <- save_graph(db, random, "random", TRUE, paste("generate_random(size=", size, ",", "probability=", prob, ")", sep=''))
random_node_uuids <- sapply(V(random), function(x) save_node(db, get_graph_id(db, random_uuid), random, x))
ring_uuid <- save_graph(db, ring, "ring", TRUE, "generate_ring(size=100, distance=5)")
ring_node_uuids <- sapply(V(ring), function(x) save_node(db, get_graph_id(db, ring_uuid), ring, x))
clique_uuid <- save_graph(db, clique, "clique", TRUE, "generate_clique(size=100)")
clique_node_uuids <- sapply(V(clique), function(x) save_node(db, get_graph_id(db, clique_uuid), clique, x))
tree_uuid <- save_graph(db, tree, "tree", TRUE, "generate_tree(size=100, children=5)")
tree_node_uuids <- sapply(V(tree), function(x) save_node(db, get_graph_id(db, tree_uuid), tree, x))
sf_uuid <- save_graph(db, sf, "scale-free", TRUE, "generate_scale_free(size=100, preference=1)")
sf_node_uuids <- sapply(V(sf), function(x) save_node(db, get_graph_id(db, sf_uuid), sf, x))
sw_uuid <- save_graph(db, sw, "small-world", TRUE, "generate_small_world(size=100, probability=0.1)")
sw_node_uuids <- sapply(V(sw), function(x) save_node(db, get_graph_id(db, sw_uuid), sw, x))
hk_uuid <- save_graph(db, hk, "holme-kim", TRUE, "generate_holme_kim(size=100, children=5, probability=1)")
hk_node_uuids <- sapply(V(hk), function(x) save_node(db, get_graph_id(db, hk_uuid), hk, x))

dbDisconnect(db)
dbListConnections(MySQL())


start <- as.numeric(Sys.time())
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoMC(cl)
max_spread <- 0
# foreach requires us to define each packages and function name used within it
foreach (i = 1:800000, .packages=c("igraph"), .export=c("ic_spread","simulate_ic")) %dopar% {
  seed <- combinations[i, 1:budget]
  # Compute average spread under IC model in multiple runs
  spread <- ic_spread(graph, V(graph)[seed], length(seed))
  # Save spread to last column
  combinations[i,(budget + 1)] <- spread
  if (spread > max_spread) {
    max_spread <- spread
  }
}
# Unregister cluster
registerDoSEQ()
stopCluster(cl)
end <- as.numeric(Sys.time())
print(end - start)

max_spread
