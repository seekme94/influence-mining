library(igraph)

setwd("D:/Datasets/Twitter")
datasets <- c("obama_edgelist_txt.csv")
models <- c("LT", "IC")
budgets <- c(0.125, 0.25, 0.5, 1)
steps <- 1
probs <- c(0.5)

summary <- NULL
header <- paste("model", "dataset", "nodes", "edges", "budget", "prob", "time", "max_influence", "deg_influence", sep=",")
# Write header
write(header, "results.csv", append=TRUE)

# Iterate for all datasets
for (dataset in datasets) {
    # Read graph
    G <- read.graph(dataset, directed=FALSE)
    # Set seet for random selection
    set.seed(100)
    nodes <- V(G)
    
    # (Optional) Limit the graph by removing nodes
     remove <- length(nodes) - 1000
     G <- delete.vertices(G, sample(nodes, remove))
     print (G)
    
    # Iterate for all models
    for (model in models) {
        # Iterate for all values of budget (k)
        for (budget in budgets) {
            for (prob in probs) {
                # Influence maximization
                max <- influence(graph=G, budget, seed=NULL, steps, model, maximize=TRUE, seed_method="degree", prob=prob)
                # Influence by nodes found by maximization function
                max_inf <- influence(graph=G, seed=max$influential_nodes, budget, steps, model, maximize=FALSE, seed_method="degree", prob=prob)
                # Influence through degree method
                deg <- influence(graph=G, budget, seed=NULL, steps, model, maximize=FALSE, seed_method="degree", prob=prob)
                # Prepare summary
                summary[1] <- paste(model, dataset, length(V(G)), length(E(G)), budget, prob, max$time, max_inf$influence, deg$influence, sep=",")
                # Write summary to file
                write(summary, "results.csv", append=TRUE)
            }
        }
    }
}
