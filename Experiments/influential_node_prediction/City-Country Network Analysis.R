#' This script analyzes dataset of city-city and country-country weighted networks in terms of their influence over the years
#' PREREQUISITE: Run "Influential Node Prediction.R" and learn an XGBoost model. The object xgboost_model.dat should exist in file system

model <- xgboost_model
#model <- xgb.load(paste(root_dir, "xgboost_model.dat", sep=''))

# Read graphs
# country2010 <- largest_component(read.graph("dataset/country_weighted_2010.csv", directed=FALSE, format="ncol"))
# country2013 <- largest_component(read.graph("dataset/country_weighted_2013.csv", directed=FALSE, format="ncol"))
# country2016 <- largest_component(read.graph("dataset/country_weighted_2016.csv", directed=FALSE, format="ncol"))
# country2019 <- largest_component(read.graph("dataset/country_weighted_2019.csv", directed=FALSE, format="ncol"))
# graphs <- list(country2010, country2013, country2016, country2019)
# graph_names <- c("country2010", "country2013", "country2016", "country2019")

city2010 <- largest_component(read.graph("dataset/citycity_weighted_2010.csv", directed=FALSE, format="ncol"))
city2013 <- largest_component(read.graph("dataset/citycity_weighted_2013.csv", directed=FALSE, format="ncol"))
city2016 <- largest_component(read.graph("dataset/citycity_weighted_2016.csv", directed=FALSE, format="ncol"))
city2019 <- largest_component(read.graph("dataset/citycity_weighted_2019.csv", directed=FALSE, format="ncol"))

graphs <- list(city2010, city2013, city2016, city2019)
graph_names <- c("city2010", "city2013", "city2016", "city2019")

test_methods <- c() # c("RESILIENCE", "INFLUENCE_LT", "INFLUENCE_IC")
node_traits <- c("DEGREE", "BETWEENNESS", "CLOSENESS", "EIGENVECTOR", "ECCENTRICITY", "CORENESS", "PAGERANK", "COLLECTIVE_INFLUENCE")
graph_traits <- c("SIZE", "EDGES", "AVERAGE_DEGREE", "MAX_DEGREE", "AVERAGE_PATH_LENGTH", "CLUSTERING_COEFFICIENT", "DIAMETER", "DENSITY", "ASSORTATIVITY", "AVERAGE_DISTANCE", "TRIADS", "GIRTH")
resultset <- data.frame(graph=c(), test_method=c(), random=c(), degree=c(), betweenness=c(), closeness=c(),
                        eigenvector=c(), eccentricity=c(), pagerank=c(), coreness=c(), ci=c(), model=c(), time=c(), influential_nodes=c())
for (test_method in test_methods) {
  print(test_method)
  i <- 0
  for(graph in graphs) {
    start <- Sys.time()
    i <- i + 1
    test <- get_graph_traits(graph=graph, node_traits=node_traits, graph_traits=graph_traits, verbose=FALSE)
    test <- as.data.frame(test)
    # Make predictions using model
    test$prediction_prob <- predict(model, newdata=as.matrix(test[, model$feature_names]))
    test$graph_id <- UUIDgenerate()
    # Influential nodes by all traits
    results <- NULL
    size <- nrow(test) * 0.05
    # Random nodes
    results$random <- get_influence(graph, sample(V(graph), size), test_method=test_method)
    # By degree
    inf <- arrange(test, desc(degree))[1:size, "name"]
    results$degree <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By betweenness
    inf <- arrange(test, desc(betweenness))[1:size, "name"]
    results$betweenness <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By closeness
    inf <- arrange(test, desc(closeness))[1:size, "name"]
    results$closeness <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By eigenvector
    inf <- arrange(test, desc(eigenvector))[1:size, "name"]
    results$eigenvector <- get_influence(graph, V(graph)[inf], test_method=test_method)
    # By eccentricity
    inf <- arrange(test, desc(eccentricity))[1:size, "name"]
    results$eccentricity <- get_influence(graph, V(graph)[inf], test_method=test_method)
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
    row <- data.frame(graph=graph_names[i], test_method=test_method,
      random=results$random, degree=results$degree, betweenness=results$betweenness, closeness=results$closeness, eigenvector=results$eigenvector, eccentricity=results$eccentricity, pagerank=results$pagerank, coreness=results$coreness, ci=results$ci, model=results$model,
      time=(Sys.time() - start), influential_nodes=paste0(V(graph)[inf]$name, collapse=","))
    resultset <- rbind(resultset, row)
  }
  print(resultset)
  file_name <- paste(root_dir, paste("city_comparative_results_", tolower(test_method), ".csv", sep=''), sep='')
  write.csv2(resultset, file_name, quote=FALSE, sep=';', row.names=FALSE)
}

###########################
# Read results
###########################

resilience <- read.csv2(paste(root_dir, "city_comparative_results_resilience.csv", sep=''))
influence_lt <- read.csv2(paste(root_dir, "city_comparative_results_influence_lt.csv", sep=''))
influence_ic <- read.csv2(paste(root_dir, "city_comparative_results_influence_ic.csv", sep=''))
results <- rbind(resilience, influence_ic, influence_lt)

# Print to see whether the model actually outperformed other heuristics
print(results[,-14])
# Print the column name with max value in each row
columns <- c("random", "degree", "betweenness", "closeness", "eigenvector", "eccentricity", "pagerank", "coreness", "ci", "model")
max_cols <- colnames(results[, columns])[max.col(results[, columns], ties.method="last")]
print(max_cols)
print(length(max_cols[max_cols == "model"]) / length(max_cols))

# The above table shows that the model outperformed other methods 75% of the times

###########################
# Influential Node Analysis
###########################



