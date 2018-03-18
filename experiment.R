###################################
#### Load required resources
###################################

require(iterpc)
require(foreach)
require(doMC)
require(jsonlite)

source('./graph_util.R')
source('./db_util.R')
source('./heuristics.R')
source('./community_detection.R')
source('./graph_util.R')
source('./influence_maximization.R')

###################################
#### Experiment settings
###################################

# Define parameters
size <- 50
budget <- size * 0.1
prob <- 0.1
set.seed(100)
experiment <- 'Resilience experiment on Random graph'

# Generate synthetic graph
graph <- generate_random(size, prob)

# Save generated graph in DB
db <- get_connection()
graph_uuid <- save_graph(db, graph, "random", TRUE, paste("generate_random(size=", size, ",", "probability=", prob, ")", sep=''))
graph_node_uuids <- sapply(V(graph), function(x) save_node(db, get_graph_id(db, graph_uuid), graph, x))
V(graph)$names <- graph_node_uuids

###################################
#### Execute resilience experiment
###################################

combinations <- getall(iterpc(vcount(graph), budget))
# Loop for each combination in the sample
min_resilience <- size
top_nodes <- NULL
start <- as.numeric(Sys.time())
for (i in samples) {
  seed <- combinations[i, 1:budget]
  # Calculte the resilience after removal of nodes seed
  nodes <- V(graph)[seed]
  current <- resilience(graph, nodes)
  if (current < min_resilience) {
    min_resilience <- current
    top_nodes <- nodes
    print(paste("Resilience:", min_resilience))
  }
}
end <- as.numeric(Sys.time())

# Get ID of the graph
graph_id <- get_graph_id(db, graph_uuid)
results <- paste('{"time":"', (end - start), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$names), '}', sep='')
description <- date()
uuid <-  UUIDgenerate()

results <- data.frame(graph_id=graph_id, experiment=experiment, results=results, description=description, uuid=uuid)
write.table(results, "output.out")

query <- paste("insert into experiment (experiment_id, experiment, results, description, uuid) ", 
               "values (0,", graph_id, ",'", experiment, "','", results, "','", description, "','", uuid, "')", sep='')

# save_experiment(db, graph_id, experiment, results, description)

dbDisconnect(db)
dbListConnections(MySQL())

