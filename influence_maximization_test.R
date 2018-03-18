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
set.seed(100)
graph <- generate_random(size, prob)
combinations <- getall(iterpc(vcount(graph), budget))
combinations <- cbind(combinations, 0)
combinations <- cbind(combinations, 0)
max_spread_ic <- 0
max_spread_lt <- 0
samples <- sample(1:nrow(combinations), 1000)
for (i in samples) {
  seed <- combinations[i, 1:budget]
  spread_ic <- influence_ic(graph, seed, budget, 0.5)$influence
  combinations[i,(budget + 1)] <- spread_ic
  if (spread_ic > max_spread_ic) {
    max_spread_ic <- spread_ic
    print(paste("Max IC spread:", max_spread_ic))
  }
  spread_lt <- influence_lt(graph, seed, steps, 0.5)$influence
  combinations[i,(budget + 2)] <- spread_lt
  if (spread_lt > max_spread_lt) {
    max_spread_lt <- spread_lt
    print(paste("Max LT spread:", max_spread_lt))
  }
}

set.seed(100)
sample(x = 1:10, 4)
