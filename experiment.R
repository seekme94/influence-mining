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
prob <- 1/budget
steps <- round(sqrt(size)) # Since influence drops exponentially, therefore below this range, it will be negligible

# Generate several graphs of various types
random <- generate_random(size, prob)
ring <- generate_ring(size, budget)
clique <- generate_clique(size)
tree <- generate_tree(size, budget)
sf <- generate_scale_free(size, preference=1)
sw <- generate_small_world(size, prob)
hk <- generate_holme_kim(size, budget, prob)

# On random networks
graph <- random
combinations <- getall(iterpc(vcount(graph), budget))
# Bind another column to store influence spread under IC
combinations <- cbind(combinations, 0)
# Bind another column to store influence spread under LT
combinations <- cbind(combinations, 0)
max_spread_ic <- 0
max_spread_lt <- 0
samples <- sample(1:nrow(combinations), 500) # This is to limit the number of trials
# Loop for each combination in the sample
for (i in samples) {
  seed <- combinations[i, 1:budget]
  # Calculte the spread under IC model
  spread_ic <- influence_ic(graph, seed, budget, 0.5)$influence
  combinations[i,(budget + 1)] <- spread_ic
  if (spread_ic > max_spread_ic) {
    max_spread_ic <- spread_ic
  }
  
  # Calculte the spread under LT model
  spread_lt <- influence_lt(graph, seed, steps, 0.5)$influence
  combinations[i,(budget + 2)] <- spread_lt
  if (spread_lt > max_spread_lt) {
    max_spread_lt <- spread_lt
  }
  print(paste("Max IC spread:", max_spread_ic, "Max LT spread:", max_spread_lt))
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

