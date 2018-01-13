source('./graph_util.R')
source('./heuristics.R')
source('./community_detection.R')
source('./graph_util.R')
source('./influence_maximization.R')
require(parallel)
require(foreach)
require(doMC)
library(iterpc)

size <- 100
prob <- 0.1
budget <- 5

# Generate several graphs of various types
random <- generate_random(size, prob)
ring <- generate_ring(size, budget)
clique <- generate_clique(size)
tree <- generate_tree(size, budget)
sf <- generate_scale_free(size, preference=1)
sw <- generate_small_world(size, prob)
ws <- generate_watts_strgatz(size, preference=1)
hk <- generate_holme_kim(size, budget, prob)

# On random networks
combination <- iterpc(vcount(random), budget)
combinations <- getall(comb)
combinations <- cbind(combinations, 0)
start <- as.numeric(Sys.time())
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoMC(cl)
# foreach requires us to define each packages and function name used within it
foreach (i = 1:nrow(combinations), .packages=c("igraph"), .export=c("ic_spread","simulate_ic")) %dopar% {
  seed <- combinations[i, 1:budget]
  # Compute average spread under IC model in multiple runs
  spread <- ic_spread(graph, seed, sqrt(length(seed)))
  # Save spread to last column
  combinations[i,(budget + 1)] <- spread
}
# Unregister cluster
registerDoSEQ()
stopCluster(cl)
end <- as.numeric(Sys.time())
end - start
