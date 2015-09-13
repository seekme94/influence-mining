setwd("D:\\Datasets\\Twitter")
datasets <- c("obama_edgelist.csv")
models <- c("LT", "IC")
budgets <- c(0.125, 0.25, 0.5, 1)
steps <- 1

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
#     remove <- length(nodes) - 1000
#     G <- delete.vertices(G, sample(nodes, remove))
#     print (G)
    
    # Iterate for all models
    for (model in models) {
        # Iterate for all values of budget (k)
        for (budget in budgets) {
            # Influence maximization
            max1 <- influence(graph=G, seed=NULL, budget, steps, model, maximize=TRUE, seed_method="degree", prob=0.2)
            max2 <- influence(graph=G, seed=NULL, budget, steps, model, maximize=TRUE, seed_method="degree", prob=0.5)
            # Influence by nodes found by maximization function
            max_inf1 <- influence(graph=G, seed=max1$influential_nodes, budget, steps, model, maximize=FALSE, seed_method="degree", prob=0.2)
            max_inf2 <- influence(graph=G, seed=max2$influential_nodes, budget, steps, model, maximize=FALSE, seed_method="degree", prob=0.5)
            # Influence through degree method
            deg1 <- influence(graph=G, seed=NULL, budget, steps, model, maximize=FALSE, seed_method="degree", prob=0.2)
            deg2 <- influence(graph=G, seed=NULL, budget, steps, model, maximize=FALSE, seed_method="degree", prob=0.5)
            # Prepare summary
            summary[1] <- paste(model, dataset, length(V(G)), length(E(G)), budget, "0.2", max1$time, max_inf1$influence, deg1$influence, sep=",")
            summary[2] <- paste(model, dataset, length(V(G)), length(E(G)), budget, "0.5", max2$time, max_inf2$influence, deg2$influence, sep=",")
            # Write summary to file
            write(summary, "results.csv", append=TRUE)
        }
    }
}
