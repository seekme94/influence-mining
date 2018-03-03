get_connection <- function() {
  require(RMySQL)
  db_user <- 'root'
  db_password <- 'jingle94'
  db_host <- 'localhost'
  db_port <- 3306
  db_name <- 'influence'
  dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, host=db_host, port=db_port)
}

get_graph_id <- function(db, uuid) {
  query <- paste("select graph_id from graph where uuid = '", uuid, "'", sep='')
  rs <- dbSendQuery(db, query)
  id <- fetch(rs, n=1)
  dbClearResult(rs)
  max(id)
}

save_graph <- function(db, graph, graph_type, synthetic=FALSE, source_code) {
  require(igraph)
  require(uuid)
  nodes <- vcount(graph)
  edges <- ecount(graph)
  degrees <- degree(graph)
  average_degree <- mean(degrees)
  highest_degree <- max(degrees)
  average_path_length <- average.path.length(graph)
  clustering_coefficient <- transitivity(graph)
  diameter <- diameter(graph)
  density <- graph.density(graph)
  assortativity <- assortativity.degree(graph)
  if (is.nan(assortativity)) {
    assortativity <- 0
  }
  average_distance <- mean_distance(graph)
  triads <- length(triangles(graph))
  girth <- girth(graph)$girth
  uuid <-  UUIDgenerate()
  query <- paste("insert into graph (graph_id, graph_type, synthetic, nodes, edges, average_degree, highest_degree, average_path_length, clustering_coefficient, diameter, density, assortativity, average_distance, triads, girth, source_code, uuid) ", "values (0,'", graph_type, "',", synthetic, ",", nodes, ",", edges, ",", average_degree, ",", highest_degree, ",", average_path_length, ",", clustering_coefficient, ",", diameter, ",", density, ",", assortativity, ",", average_distance, ",", triads, ",", girth, ",'", source_code, "','", uuid, "')", sep='')
  dbGetQuery(db, query)
  uuid
}

save_node <- function(db, graph_id, graph, node) {
  require(igraph)
  require(uuid)
  degree <- degree(graph, v=node)
  closeness <- closeness(graph, vids=node)
  betweenness <- betweenness(graph, v=node)
  eigen_value <- eigen_centrality(graph)$vector[which(V(graph)==node)]
  eccentricity <- eccentricity(graph, vids=node)
  page_rank <- page_rank(graph, vids=node)$vector
  uuid <-  UUIDgenerate()
  query <- paste("insert into node (node_id, graph_id, degree, closeness, betweenness, eigen_value, eccentricity, page_rank, uuid) ", "values (0,", graph_id, ",", degree, ",", closeness, ",", betweenness, ",", eigen_value, ",", eccentricity, ",", page_rank, ",'", uuid, "')", sep='')
  dbGetQuery(db, query)
  uuid
}

save_experiment <- function(db, graph_id, experiment, results, description) {
  require(igraph)
  require(uuid)
  uuid <-  UUIDgenerate()
  query <- paste("insert into experiment (experiment_id, experiment, results, description, uuid) ", "values (0,", graph_id, ",'", experiment, "','", results, "','", description, "','", uuid, "')", sep='')
  dbGetQuery(db, query)
  uuid
}

