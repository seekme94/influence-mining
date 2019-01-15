library(igraph)
source('util/graph_util.R')
source('util/heuristics.R')
source('util/influence_maximization.R')
source('util/community_detection.R')

### Test Influence spread by multiple trials ###
graph <- read.graph("data/sample_edgelist.txt", format="ncol", directed=TRUE)
# Set seed nodes
seed <- c(2,4,8,9)
# Compute spread of influence under IC model for given seed set
ic_spread(graph, seed, runs=10)
# Compute spread of influence, providing best node under IC model for given seed set
ic_spread_plus(graph, seed, runs=10, best_node=9)

### Influence maximization (all combinations)
setup()
combinations <- influence(graph, budget=5, seed=NULL, 5, "LT", maximize=TRUE, seed_method="degree", prob=0.5, parallel=FALSE)

combinations <- influence_maximization(graph, seed_size=2, runs=2, parallel=FALSE)

greedy <- influence_max_greedy(graph=graph, budget=10, steps=10, model="IC", prob=0.3)
greedy

greedy <- influence_max_greedy_parallel(graph=graph, budget=10, steps=10, model="IC", prob=0.3)
greedy

datasets <- c("data/sample_edgelist.txt")
models <- c("IC") # c("IC", "LT")
methods <- c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")
methods <- c("random", "degree", "closeness", "betweenness")
budgets <- c(10, 20)
steps <- 3
probs <- c(0.3)

# Iterate for all models
for (model in models) {
  for (method in methods) {
    summary <- NULL
    filename <- paste(method, "results.csv", sep="_")
    header <- paste("model", "dataset", "nodes", "edges", "budget", "prob", "time", "max_influence", "method", "method_influence", sep=",")
    # Write header
    write(header, filename, append=TRUE)
    
    # Iterate for all datasets
    for (dataset in datasets) {
      # Read graph
      graph <- read.graph(dataset, format="ncol", directed=FALSE)
      # Set seet for random selection
      set.seed(100)
      nodes <- V(graph)
      
      # (Optional) Limit the graph by removing nodes
      # remove <- length(nodes) - 1000
      # graph <- delete.vertices(graph, sample(nodes, remove))
      # print (graph)
      
      # Iterate for all values of budget (k)
      for (budget in budgets) {
        for (prob in probs) {
          # Influence maximization
          max <- influence(graph, budget, seed=NULL, steps, model, maximize=TRUE, seed_method=method, prob=prob)
          # Influence by nodes found by maximization function
          max_inf <- influence(graph, seed=max$influential_nodes, budget, steps, model, maximize=FALSE, seed_method=method, prob=prob)
          # Influence through degree method
          method_inf <- influence(graph, budget, seed=NULL, steps, model, maximize=FALSE, seed_method=method, prob=prob)
          # Prepare summary
          summary[1] <- paste(model, dataset, length(V(graph)), length(E(graph)), budget, prob, max$time, max_inf$influence, method, method_inf$influence, sep=",")
          # Write summary to file
          write(summary, paste(method, "results/influence_results.csv", sep="_"), append=TRUE)
        }
      }
    }
  }
}