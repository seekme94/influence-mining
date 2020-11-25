#' This script analyzes dataset of city-city and country-country weighted networks in terms of their influence over the years
#' PREREQUISITE: Run "Influential Node Prediction.R" and learn an XGBoost model. The object xgboost_model.dat should exist in file system

library(influence.mining)
library(dplyr)
library(corrplot)
library(uuid)
library(clipr)
library(VennDiagram)
library(RColorBrewer)
library(psych)
library(devEMF)


root_dir <- "Experiments/influential_node_prediction/"
RESET <- FALSE
budget <- 0.05

# Read the dataset in Excel file
file <- "../../Datasets/City Networks/CityNetworks2010-2019.csv"
df <- read.csv2(file, header=TRUE, sep=',')

# Read graphs to form city networks
city2010 <- graph_from_data_frame(df[df$YEAR == 2010, c("CITY1_NAME", "CITY2_NAME", "LINKS")], directed=FALSE, vertices=NULL)
city2013 <- graph_from_data_frame(df[df$YEAR == 2013, c("CITY1_NAME", "CITY2_NAME", "LINKS")], directed=FALSE, vertices=NULL)
city2016 <- graph_from_data_frame(df[df$YEAR == 2016, c("CITY1_NAME", "CITY2_NAME", "LINKS")], directed=FALSE, vertices=NULL)
city2019 <- graph_from_data_frame(df[df$YEAR == 2019, c("CITY1_NAME", "CITY2_NAME", "LINKS")], directed=FALSE, vertices=NULL)

# Extract last two characters as country code
df_country <- NULL
df_country$YEAR <- df$YEAR
df_country$COUNTRY1_CODE <- substr(df$CITY1_NAME, nchar(as.character(df$CITY1_NAME)) - 1, nchar(as.character(df$CITY1_NAME)))
df_country$COUNTRY2_CODE <- substr(df$CITY2_NAME, nchar(as.character(df$CITY2_NAME)) - 1, nchar(as.character(df$CITY2_NAME)))
df_country$LINKS <- df$LINKS
df_country <- data.frame(df_country)
# Remove self directing edges
df_country <- rbind(df_country[df_country$YEAR == 2010 & as.character(df_country$COUNTRY1_CODE) != as.character(df_country$COUNTRY2_CODE),],
                    df_country[df_country$YEAR == 2013 & as.character(df_country$COUNTRY1_CODE) != as.character(df_country$COUNTRY2_CODE),],
                    df_country[df_country$YEAR == 2016 & as.character(df_country$COUNTRY1_CODE) != as.character(df_country$COUNTRY2_CODE),],
                    df_country[df_country$YEAR == 2019 & as.character(df_country$COUNTRY1_CODE) != as.character(df_country$COUNTRY2_CODE),])
# Sum the links on countries
df_country <- as.data.frame(df_country %>% group_by(YEAR, COUNTRY1_CODE, COUNTRY2_CODE) %>% summarize_at(vars(LINKS), list(LINKS = sum)))
head(df_country)

# Construct country networks
country2010 <- graph_from_data_frame(df_country[df_country$YEAR == 2010, c("COUNTRY1_CODE", "COUNTRY2_CODE", "LINKS")], directed=FALSE, vertices=NULL)
country2013 <- graph_from_data_frame(df_country[df_country$YEAR == 2013, c("COUNTRY1_CODE", "COUNTRY2_CODE", "LINKS")], directed=FALSE, vertices=NULL)
country2016 <- graph_from_data_frame(df_country[df_country$YEAR == 2016, c("COUNTRY1_CODE", "COUNTRY2_CODE", "LINKS")], directed=FALSE, vertices=NULL)
country2019 <- graph_from_data_frame(df_country[df_country$YEAR == 2019, c("COUNTRY1_CODE", "COUNTRY2_CODE", "LINKS")], directed=FALSE, vertices=NULL)

graphs <- list(city2010, city2013, city2016, city2019)
graph_names <- c("city2010", "city2013", "city2016", "city2019")
country_graphs <- list(country2010, country2013, country2016, country2019)
country_graph_names <- c("country2010", "country2013", "country2016", "country2019")
years <- c(2019, 2016, 2013, 2010)

test_methods <- c("RESILIENCE", "INFLUENCE_LT", "INFLUENCE_IC")
node_traits <- c("DEGREE", "BETWEENNESS", "CLOSENESS", "EIGENVECTOR", "CORENESS", "PAGERANK", "COLLECTIVE_INFLUENCE", "ADAPTIVE_DEGREE", "ADAPTIVE_BETWEENNESS", "ADAPTIVE_CLOSENESS", "ADAPTIVE_EIGENVECTOR", "ADAPTIVE_CORENESS", "ADAPTIVE_PAGERANK", "ADAPTIVE_COLLECTIVE_INFLUENCE")
graph_traits <- c("SIZE", "EDGES", "AVERAGE_DEGREE", "MAX_DEGREE", "AVERAGE_PATH_LENGTH", "CLUSTERING_COEFFICIENT", "DIAMETER", "DENSITY", "ASSORTATIVITY", "AVERAGE_DISTANCE", "TRIADS", "GIRTH")
lt_threshold <- 0.25


###########################
# Influence Method Analysis
###########################

get_influential_nodes_from_dataframe <- function(df, budget) {
  # Extract common nodes
  df$node_name <- row.names(df)
  limit <- nrow(df) * budget # Pick only top k nodes
  h_degree <- arrange(df, desc(degree))[1:limit, "node_name"]
  h_betweenness <- arrange(df, desc(betweenness))[1:limit, "node_name"]
  h_closeness <- arrange(df, desc(closeness))[1:limit, "node_name"]
  h_eigenvector <- arrange(df, desc(eigenvector))[1:limit, "node_name"]
  h_coreness <- arrange(df, desc(coreness))[1:limit, "node_name"]
  h_pagerank <- arrange(df, desc(pagerank))[1:limit, "node_name"]
  h_ci <- arrange(df, desc(ci))[1:limit, "node_name"]
  h_a_degree <- arrange(df, a_degree)[1:limit, "node_name"]
  h_a_betweenness <- arrange(df, a_betweenness)[1:limit, "node_name"]
  h_a_closeness <- arrange(df, a_closeness)[1:limit, "node_name"]
  h_a_eigenvector <- arrange(df, a_eigenvector)[1:limit, "node_name"]
  h_a_coreness <- arrange(df, a_coreness)[1:limit, "node_name"]
  h_a_pagerank <- arrange(df, a_pagerank)[1:limit, "node_name"]
  h_a_ci <- arrange(df, a_ci)[1:limit, "node_name"]
  list(h_degree, h_betweenness, h_closeness, h_eigenvector, h_coreness, h_pagerank, h_ci,
       h_a_degree, h_a_betweenness, h_a_closeness, h_a_eigenvector, h_a_coreness, h_a_pagerank, h_a_ci)
}

generate_commonality_matrix <- function(influentials_list) {
  commonality_matrix <- matrix(rep(0, length(influentials_list)^2), ncol=length(influentials_list), nrow=length(influentials_list))
  for (i in 1:length(influentials_list)) {
    for (j in 1:length(influentials_list)) {
      a <- unlist(influentials_list[i])
      b <- unlist(influentials_list[j])
      total <- union(a, b)
      common <- intersect(a, b)
      commonality_matrix[i, j] <- round(length(common) / length(total), 2)
    }
  }
  names <- c("deg", "btwn", "close", "eigenv", "core", "pgrnk", "ci", "a_deg", "a_btwn", "a_close", "a_eigenv", "a_core", "a_pgrnk", "a_ci")
  dimnames(commonality_matrix) <- list(names, names)
  commonality_matrix
}

compute_influences <- function(result_list) {
  results <- data.frame(metric=c(), resilience=c(), influence_ic=c(), influence_lt=c())
  for (name in names(result_list)) {
    print(name)
    indices <- V(g)$name %in% result_list[,name]
    resilience <- get_influence(g, nodes=V(g)[indices], test_method="RESILIENCE") + 1
    influence_ic <- get_influence(g, nodes=V(g)[indices], test_method="INFLUENCE_IC") + 1
    influence_lt <- get_influence(g, nodes=V(g)[indices], test_method="INFLUENCE_LT") + 1
    current <- data.frame(metric=name, resilience=resilience, influence_ic=influence_ic, influence_lt=influence_lt,
                          har_mean=harmonic.mean(c(resilience, influence_ic, influence_lt)))
    results <- rbind(results, current)
  }
  results
}

######################################################################################################
g <- city2010
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2010 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2010 <- as.data.frame(df2010)
df2010$year <- 2010
df2010$name <- rownames(df2010)
influentials2010 <- get_influential_nodes_from_dataframe(df2010, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2010 <- data.frame(influentials2010)
names(influential_list2010) <- node_traits

# List the nodes present in all sets of influential nodes
all2010 <- Reduce(intersect, influential_list2010)
all2010

# Create a commonality_matrix and plot the sparse matrix to see how similar any two metrices are. The plot shows proportion of nodes which match
commonality_matrix2010 <- generate_commonality_matrix(influentials2010)
corrplot.mixed(commonality_matrix2010, order="hclust", tl.col="black", title="Common nodes between methods - 2010", mar=c(0,0,1,0))

# Calculate influence under all methods
vitality_results2010 <- compute_influences(influential_list2010)
vitality_results2010
######################################################################################################


######################################################################################################
g <- city2013
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2013 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2013 <- as.data.frame(df2013)
df2013$year <- 2013
df2013$name <- rownames(df2013)
influentials2013 <- get_influential_nodes_from_dataframe(df2013, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2013 <- data.frame(influentials2013)
names(influential_list2013) <- node_traits

# List the nodes present in all sets of influential nodes
all2013 <- Reduce(intersect, influential_list2013)
all2013

# Create a commonality_matrix and plot the sparse matrix to see how similar any two metrices are. The plot shows proportion of nodes which match
commonality_matrix2013 <- generate_commonality_matrix(influentials2013)
corrplot.mixed(commonality_matrix2013, order="hclust", tl.col="black", title="Common nodes between methods - 2013", mar=c(0,0,1,0))

# Calculate influence under all methods
vitality_results2013 <- compute_influences(influential_list2013)
vitality_results2013
######################################################################################################


######################################################################################################
g <- city2016
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2016 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2016 <- as.data.frame(df2016)
df2016$year <- 2016
df2016$name <- rownames(df2016)
influentials2016 <- get_influential_nodes_from_dataframe(df2016, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2016 <- data.frame(influentials2016)
names(influential_list2016) <- node_traits

# List the nodes present in all sets of influential nodes
all2016 <- Reduce(intersect, influential_list2016)
all2016

# Create a commonality_matrix and plot the sparse matrix to see how similar any two metrices are. The plot shows proportion of nodes which match
commonality_matrix2016 <- generate_commonality_matrix(influentials2016)
corrplot.mixed(commonality_matrix2016, order="hclust", tl.col="black", title="Common nodes between methods - 2016", mar=c(0,0,1,0))

# Calculate influence under all methods
vitality_results2016 <- compute_influences(influential_list2016)
vitality_results2016
######################################################################################################


######################################################################################################
g <- city2019
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2019 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2019 <- as.data.frame(df2019)
df2019$year <- 2019
df2019$name <- rownames(df2019)
influentials2019 <- get_influential_nodes_from_dataframe(df2019, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2019 <- data.frame(influentials2019)
names(influential_list2019) <- node_traits

# List the nodes present in all sets of influential nodes
all2019 <- Reduce(intersect, influential_list2019)
all2019

# Create a commonality_matrix and plot the sparse matrix to see how similar any two metrices are. The plot shows proportion of nodes which match
commonality_matrix2019 <- generate_commonality_matrix(influentials2019)
corrplot.mixed(commonality_matrix2019, order="hclust", tl.col="black", title="Common nodes between methods - 2019", mar=c(0,0,1,0))

# Calculate influence under all methods
vitality_results2019 <- compute_influences(influential_list2019)
vitality_results2019
######################################################################################################


######################################################################################################
# Plot commonality between all methods
mean_commonality_matrix <- (commonality_matrix2019 + commonality_matrix2016 + commonality_matrix2013 + commonality_matrix2010) / length(years)
corrplot.mixed(mean_commonality_matrix, order="hclust", tl.col="black", title="Average common nodes between methods", mar=c(0,0,1,0))
write_clip(mean_commonality_matrix)
######################################################################################################



###########################
# Influence Impact Analysis
###########################

# Compare the networks with and without the influential nodes
compare_graphs <- function(graph1, graph2, plot=FALSE) {
  if (plot) {
    # par(mfrow=c(2,1))
    plot(graph1, vertex.label.cex=0.1, vertex.size=log(degree(graph1, V(graph1))), layout=layout.fruchterman.reingold)
    plot(graph2, vertex.label.cex=0.1, vertex.size=log(degree(graph2, V(graph2))), layout=layout.fruchterman.reingold)
  }
  compare <- data.frame(graph=c("graph1", "graph2"))
  summary1 <- graph_summary(graph1, plot=plot)
  summary2 <- graph_summary(graph2, plot=plot)
  compare$vertices <- c(summary1$vertices, summary2$vertices)
  compare$edges <- c(summary1$edges, summary2$edges)
  compare$vertex_edge_ratio <- c(summary1$vertex_edge_ratio, summary2$vertex_edge_ratio)
  compare$average_degree <- c(summary1$average_degree, summary2$average_degree)
  compare$average_path_length <- c(summary1$average_path_length, summary2$average_path_length)
  compare$highest_degree <- c(summary1$highest_degree, summary2$highest_degree)
  compare$density <- c(summary1$density, summary2$density)
  compare$diameter <- c(summary1$diameter, summary2$diameter)
  compare$transitivity <- c(summary1$transitivity, summary2$transitivity)
  compare$assortativity <- c(summary1$assortativity, summary2$assortativity)
  compare$average_distance <- c(summary1$average_distance, summary2$average_distance)
  compare$graph_triads <- c(summary1$graph_triads, summary2$graph_triads)
  compare$power_law <- c(summary1$power_law, summary2$power_law)
  compare
}

# Plot graph of only community memberships (groups) in order to visualize large graphs
community_membership_plot <- function(graph) {
  comm_graph <- walktrap.community(graph)
  graph <- simplify(contract(graph, membership(comm_graph)))
  plot(graph, vertex.label="", vertex.size=5, layout=layout.fruchterman.reingold, margin=-0.1)
  size <- length(unique(membership(comm_graph)))
  size
}

# Visualize induced subgraph and full graph with and without the induced subgraph
analyze_influential_node_impact <- function(graph, influentials) {
  subg_reduced <- largest_component(delete.vertices(graph, v=V(graph)[influentials]))
  # Plot the original graph
  print(community_membership_plot(graph))
  # Plot the graph withouth the influential nodes
  print(community_membership_plot(subg_reduced))
  compare_graphs(graph, subg_reduced, plot=FALSE)
}

# Returns name of the colour associated with given country code
pick_colour <- function(country) {
  countries <- c("CR", "MT", "FJ", "MX", "LK", "GU", "IE", "KE", "GD", "LU", "DM", "CF", "ET", "CI", "MM", "BD", "GR", "FI", "LV", "MO", "GY", "IT", "AT", "MG", "CA", "MZ", "CO", "EG", "CM", "GW", "MK", "MQ", "BZ", "AG", "BY", "FR", "GN", "KG", "CN", "MR", "DE", "HT", "CZ", "GM", "BM", "MY", "AN", "GA", "AR", "DK", "MN", "LY", "KZ", "AF", "LC", "MV", "LR", "DO", "BI", "GQ", "HU", "GT", "AL", "CV", "BN", "CH", "LA", "MD", "DJ", "KI", "ER", "DZ", "AD", "BO", "KH", "BW", "AZ", "BS", "MW", "BH", "ES", "HK", "HR", "EE", "BR", "IL", "NA", "EC", "AO", "BE", "JM", "MH", "BJ", "MU", "FM", "HN", "BB", "ML", "MA", "CY", "KR", "IQ", "KN", "IN", "JO", "GE", "AI", "BF", "GH", "LB", "AE", "JP", "GB", "BA", "AU", "CD", "IS", "BG", "KM", "CU", "KY", "LT", "AM", "CG", "CL", "IR", "ID", "KW", "GF", "GP")
  colours <- c("azure4", "bisque4", "black", "blue", "blue4", "blueviolet", "brown", "brown4", "burlywood", "burlywood4", "cadetblue", "cadetblue4", "chartreuse", "chartreuse4", "chocolate", "chocolate4", "coral4", "cornflowerblue", "cornsilk4", "cyan4", "darkblue", "darkcyan", "darkgoldenrod", "darkgoldenrod4", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkolivegreen4", "darkorange", "darkorange4", "darkorchid", "darkorchid4", "darkred", "darksalmon", "darkseagreen", "darkseagreen4", "darkslateblue", "darkslategray", "darkslategray4", "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deeppink4", "deepskyblue4", "dimgray", "dimgrey", "dodgerblue", "dodgerblue4", "firebrick", "firebrick4", "forestgreen", "gold4", "goldenrod", "goldenrod4", "green4", "honeydew4", "hotpink4", "indianred", "indianred4", "ivory4", "khaki4", "lavenderblush4", "lawngreen", "lemonchiffon4", "limegreen", "magenta4", "maroon", "maroon4", "midnightblue", "mistyrose4", "moccasin", "navy", "navyblue", "olivedrab", "olivedrab4", "orange", "orange4", "orangered", "orangered4", "orchid", "orchid4", "palegoldenrod", "palegreen4", "paleturquoise4", "palevioletred", "palevioletred4", "peachpuff", "peachpuff4", "peru", "pink4", "plum", "plum4", "purple", "purple4", "red", "red4", "rosybrown", "rosybrown4", "royalblue", "royalblue4", "saddlebrown", "salmon", "salmon4", "sandybrown", "seagreen", "seagreen4", "seashell4", "sienna", "sienna4", "skyblue4", "slateblue", "slateblue4", "slategray", "slategray4", "slategrey", "snow4", "springgreen", "springgreen4", "steelblue", "steelblue4", "tan", "tan4", "thistle", "thistle4", "tomato", "tomato4", "turquoise", "turquoise4", "violet", "violetred", "violetred4", "wheat4")
  colours[which(countries == country)]
}


### Betweenness ###
### 2010 ###
influentials <- as.character(influential_list2010$BETWEENNESS)
compare_betweenness_2010 <- analyze_influential_node_impact(city2010, influentials)

### 2013 ###
influentials <- as.character(influential_list2013$BETWEENNESS)
compare_betweenness_2013 <- analyze_influential_node_impact(city2013, influentials)

### 2016 ###
influentials <- as.character(influential_list2016$BETWEENNESS)
compare_betweenness_2016 <- analyze_influential_node_impact(city2016, influentials)

### 2019 ###
influentials <- as.character(influential_list2019$BETWEENNESS)
compare_betweenness_2019 <- analyze_influential_node_impact(city2019, influentials)

rbind(compare_betweenness_2010, compare_betweenness_2013, compare_betweenness_2016, compare_betweenness_2019)


### Adaptive Coreness ###
### 2010 ###
influentials <- as.character(influential_list2010$ADAPTIVE_CORENESS)
compare_a_coreness_2010 <- analyze_influential_node_impact(city2010, influentials)

### 2013 ###
influentials <- as.character(influential_list2013$ADAPTIVE_CORENESS)
compare_a_coreness_2013 <- analyze_influential_node_impact(city2013, influentials)

### 2016 ###
influentials <- as.character(influential_list2016$ADAPTIVE_CORENESS)
compare_a_coreness_2016 <- analyze_influential_node_impact(city2016, influentials)

### 2019 ###
influentials <- as.character(influential_list2019$ADAPTIVE_CORENESS)
compare_a_coreness_2019 <- analyze_influential_node_impact(city2019, influentials)

rbind(compare_a_coreness_2010, compare_a_coreness_2013, compare_a_coreness_2016, compare_a_coreness_2019)


###########################
# Pair-wise Annual Analysis
###########################
# We now compare the datasets in pairs of years

# To draw a Venn diagram to visualize the number of common nodes between all datasets
colors <- brewer.pal(4, "Set2")
venn.diagram(x=list(rownames(df2019), rownames(df2016), rownames(df2013), rownames(df2010)),
             category.names=c("2019", "2016", "2013", "2010"),
             filename=paste(root_dir, "common_nodes_venn_diagram.png", sep=''),
             output=TRUE, imagetype="png",
             height=800, width=800, resolution=100, scale=FALSE,
             col="black", fill=colors, cat.col=colors, cat.cex=2, margin=0.15,
             fontface="bold", fontfamily="sans")

# Merge all into a dataset
dt <- data.frame()
dt <- rbind(dt, df2010, df2013, df2016, df2019)
# Add country code
dt$country <- unlist(lapply(as.character(rownames(dt)), function(x) { substr(x, start=nchar(x) - 1, stop=nchar(x)) }))

############################
# Pairwise yearly comparison
############################

# Convert scores to ranks
dt$betweenness_rank <- NaN
for (year in years) {
  x <- dt$betweenness[dt$year == year]
  x <- max(x) - x # swap the order
  dt$betweenness_rank[dt$year == year] <- rank(x, ties.method='min')
}
head(arrange(dt, betweenness_rank)[,c("name", "betweenness_rank")], 20)

y1 <- 2010
y2 <- 2019
measures <- c("betweenness_rank")
for (measure in measures) {
  ds <- data.frame(name=unique(dt$name))
  y1_column <- paste("year", y1, sep='_')
  y2_column <- paste("year", y2, sep='_')
  # dt[dt$year==y1,c("name", measure)]

  lookup <- dt[dt$year == y1, c("name", measure)]
  names(lookup) <- c("name", y1_column)
  ds <- merge(lookup, ds, by="name")
  lookup <- dt[dt$year == y2, c("name", measure)]
  names(lookup) <- c("name", y2_column)
  ds <- merge(lookup, ds, by="name")
  # Add country code
  ds$country <- unlist(lapply(as.character(ds$name), function(x) { substr(x, start=nchar(x) - 1, stop=nchar(x)) }))
  # Calculate difference in ranking. +Ve value means progress; -ve means decline
  ds$difference <- ds[, y1_column] - ds[, y2_column]

  # Limit to only top relevant cities
  limit <- 100
  ds <- ds[ds[, y1_column] < sort(ds[, y1_column])[limit] & ds[, y2_column] < sort(ds[, y2_column])[limit],]

  colour_lookup <- sort(unique(ds$country))
  # Prepare PNG to save plot
  png(paste(root_dir, y1, "-", y2, "_", measure, ".png", sep=""), width=1500, height=1500, res=200)
  par(oma=rep(0, times=4)) # all sides have equal lines of space
  plot(range(ds[, y1_column]), range(ds[, y2_column]), type="p", xlab=y1_column, ylab=y2_column, cex=0)
  title(paste("Distribution of node ranks", measure))
  # Associate a colour with each country
  for (i in 1:nrow(ds)) {
    text(ds[i, y1_column], ds[i, y2_column], labels=tolower(ds$name[i]), cex= 0.6, col=pick_colour(ds$country[i]), font=2)
  }
  # Plot mid lines
  xmid <- mean(range(ds[, y1_column]))
  xmax <- range(ds[, y1_column])[2]
  ymid <- mean(range(ds[, y2_column]))
  ymax <- range(ds[, y2_column])[2]
  segments(x0=xmid, y0=0, x1=xmid, y=ymax, col="red")
  segments(x0=0, y0=ymid, x1=xmax, y=ymid, col="red")
  # Save plot
  dev.off()
}

# All city rankings over the years
all_ranks <- data.frame(name=c(), country=c(), r_2010=c(), r_2013=c(), r_2016=c(), r_2019=c())
cities <- unique(dt$name)
cities <- cities[cities %in% dt$name[dt$year==2010] & cities %in% dt$name[dt$year==2013] & cities %in% dt$name[dt$year==2016] & cities %in% dt$name[dt$year==2019]]
for (city in cities) {
  row <- data.frame(name=city, country=substr(city, start=nchar(city) - 1, stop=nchar(city)),
                    r_2010=dt$betweenness_rank[dt$year==2010 & dt$name==city],
                    r_2013=dt$betweenness_rank[dt$year==2013 & dt$name==city],
                    r_2016=dt$betweenness_rank[dt$year==2016 & dt$name==city],
                    r_2019=dt$betweenness_rank[dt$year==2019 & dt$name==city])
  all_ranks <- rbind(all_ranks, row)
}
all_ranks





######################################################################################################
######################################################################################################
####################################### COUNTRY-LEVEL ANALYSIS #######################################
######################################################################################################
######################################################################################################

get_country_name <- function(country_code) {
  codes <- c("AW", "AF", "AO", "AI", "AL", "AD", "AN", "AE", "AR", "AM", "AS", "AQ", "TF", "AG", "AU", "AT", "AZ", "BI", "BE", "BJ", "BF", "BD", "BG", "BH", "BS", "BA", "BY", "BZ", "BM", "BO", "BR", "BB", "BN", "BT", "BV", "BW", "CF", "CA", "CC", "CH", "CL", "CN", "CI", "CM", "CD", "CG", "CK", "CO", "KM", "CV", "CR", "CU", "CX", "KY", "CY", "CZ", "DE", "DJ", "DM", "DK", "DO", "DZ", "EC", "EG", "ER", "EH", "ES", "EE", "ET", "FI", "FJ", "FK", "FR", "FO", "FM", "GA", "GB", "GE", "GH", "GI", "GN", "GP", "GM", "GW", "GQ", "GR", "GD", "GL", "GT", "GF", "GU", "GY", "HK", "HM", "HN", "HR", "HT", "HU", "ID", "IN", "IO", "IE", "IR", "IQ", "IS", "IL", "IT", "JM", "JO", "JP", "KZ", "KE", "KG", "KH", "KI", "KN", "KR", "KW", "LA", "LB", "LR", "LY", "LC", "LI", "LK", "LS", "LT", "LU", "LV", "MO", "MA", "MC", "MD", "MG", "MV", "MX", "MH", "MK", "ML", "MT", "MM", "MN", "MP", "MZ", "MR", "MS", "MQ", "MU", "MW", "MY", "YT", "NA", "NC", "NE", "NF", "NG", "NI", "NU", "NL", "NO", "NP", "NR", "NZ", "OM", "PK", "PA", "PN", "PE", "PH", "PW", "PG", "PL", "PR", "KP", "PT", "PY", "PS", "PF", "QA", "RE", "RO", "RU", "RW", "SA", "SD", "SN", "SG", "GS", "SH", "SJ", "SB", "SL", "SV", "SM", "SO", "PM", "ST", "SR", "SK", "SI", "SE", "SZ", "SC", "SY", "TC", "TD", "TG", "TH", "TJ", "TK", "TM", "TP", "TO", "TT", "TN", "TR", "TV", "TW", "TZ", "UG", "UA", "UM", "UY", "US", "UZ", "VA", "VC", "VE", "VG", "VI", "VN", "VU", "WF", "WS", "YE", "YU", "ZA", "ZM", "ZW")
  names <- c("Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "Andorra", "Netherlands Antilles", "United Arab Emirates", "Argentina", "Armenia", "American Samoa", "Antarctica", "French Southern territories", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", "Bahrain", "Bahamas", "Bosnia and Herzegovina", "Belarus", "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei", "Bhutan", "Bouvet Island", "Botswana", "Central African Republic", "Canada", "Cocos (Keeling) Islands", "Switzerland", "Chile", "China", "CÃ´te dÂ’Ivoire", "Cameroon", "Congo, The Democratic Republic of the", "Congo", "Cook Islands", "Colombia", "Comoros", "Cape Verde", "Costa Rica", "Cuba", "Christmas Island", "Cayman Islands", "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", "Algeria", "Ecuador", "Egypt", "Eritrea", "Western Sahara", "Spain", "Estonia", "Ethiopia", "Finland", "Fiji Islands", "Falkland Islands", "France", "Faroe Islands", "Micronesia, Federated States of", "Gabon", "United Kingdom", "Georgia", "Ghana", "Gibraltar", "Guinea", "Guadeloupe", "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Greece", "Grenada", "Greenland", "Guatemala", "French Guiana", "Guam", "Guyana", "Hong Kong", "Heard Island and McDonald Islands", "Honduras", "Croatia", "Haiti", "Hungary", "Indonesia", "India", "British Indian Ocean Territory", "Ireland", "Iran", "Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", "Japan", "Kazakstan", "Kenya", "Kyrgyzstan", "Cambodia", "Kiribati", "Saint Kitts and Nevis", "South Korea", "Kuwait", "Laos", "Lebanon", "Liberia", "Libyan Arab Jamahiriya", "Saint Lucia", "Liechtenstein", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", "Macao", "Morocco", "Monaco", "Moldova", "Madagascar", "Maldives", "Mexico", "Marshall Islands", "Macedonia", "Mali", "Malta", "Myanmar", "Mongolia", "Northern Mariana Islands", "Mozambique", "Mauritania", "Montserrat", "Martinique", "Mauritius", "Malawi", "Malaysia", "Mayotte", "Namibia", "New Caledonia", "Niger", "Norfolk Island", "Nigeria", "Nicaragua", "Niue", "Netherlands", "Norway", "Nepal", "Nauru", "New Zealand", "Oman", "Pakistan", "Panama", "Pitcairn", "Peru", "Philippines", "Palau", "Papua New Guinea", "Poland", "Puerto Rico", "North Korea", "Portugal", "Paraguay", "Palestine", "French Polynesia", "Qatar", "RÃ©union", "Romania", "Russian Federation", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore", "South Georgia and the South Sandwich Islands", "Saint Helena", "Svalbard and Jan Mayen", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", "Somalia", "Saint Pierre and Miquelon", "Sao Tome and Principe", "Suriname", "Slovakia", "Slovenia", "Sweden", "Swaziland", "Seychelles", "Syria", "Turks and Caicos Islands", "Chad", "Togo", "Thailand", "Tajikistan", "Tokelau", "Turkmenistan", "East Timor", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Tuvalu", "Taiwan", "Tanzania", "Uganda", "Ukraine", "United States Minor Outlying Islands", "Uruguay", "United States", "Uzbekistan", "Holy See (Vatican City State)", "Saint Vincent and the Grenadines", "Venezuela", "Virgin Islands, British", "Virgin Islands, U.S.", "Vietnam", "Vanuatu", "Wallis and Futuna", "Samoa", "Yemen", "Yugoslavia", "South Africa", "Zambia", "Zimbabwe")
  names[which(codes == country_code)]
}

######################################################################################################
g <- country2010
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2010 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2010 <- as.data.frame(df2010)
df2010$year <- 2010
df2010$name <- rownames(df2010)
influentials2010 <- get_influential_nodes_from_dataframe(df2010, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2010 <- data.frame(influentials2010)
names(influential_list2010) <- node_traits

# Calculate influence under all methods
vitality_results2010 <- compute_influences(influential_list2010)
vitality_results2010
######################################################################################################


######################################################################################################
g <- country2013
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2013 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2013 <- as.data.frame(df2013)
df2013$year <- 2013
df2013$name <- rownames(df2013)
influentials2013 <- get_influential_nodes_from_dataframe(df2013, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2013 <- data.frame(influentials2013)
names(influential_list2013) <- node_traits

# Calculate influence under all methods
vitality_results2013 <- compute_influences(influential_list2013)
vitality_results2013
######################################################################################################


######################################################################################################
g <- country2016
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2016 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2016 <- as.data.frame(df2016)
df2016$year <- 2016
df2016$name <- rownames(df2016)
influentials2016 <- get_influential_nodes_from_dataframe(df2016, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2016 <- data.frame(influentials2016)
names(influential_list2016) <- node_traits

# Calculate influence under all methods
vitality_results2016 <- compute_influences(influential_list2016)
vitality_results2016
######################################################################################################


######################################################################################################
g <- country2019
E(g)$LINKS <- round(log(E(g)$LINKS + min(E(g)$LINKS) + 1), 2) # Normalize the link distribution
df2019 <- get_graph_traits(g, verbose=TRUE, node_traits=node_traits, graph_traits=graph_traits)
df2019 <- as.data.frame(df2019)
df2019$year <- 2019
df2019$name <- rownames(df2019)
influentials2019 <- get_influential_nodes_from_dataframe(df2019, budget)

# Measure resilience of each metric and compare with each other. Also see if two metrics have similar resilience while having different set of nodes
influential_list2019 <- data.frame(influentials2019)
names(influential_list2019) <- node_traits

# Calculate influence under all methods
vitality_results2019 <- compute_influences(influential_list2019)
vitality_results2019
######################################################################################################

### Betweenness ###
### 2010 ###
influentials <- as.character(influential_list2010$BETWEENNESS)
compare_betweenness_2010 <- analyze_influential_node_impact(country2010, influentials)

### 2013 ###
influentials <- as.character(influential_list2013$BETWEENNESS)
compare_betweenness_2013 <- analyze_influential_node_impact(country2013, influentials)

### 2016 ###
influentials <- as.character(influential_list2016$BETWEENNESS)
compare_betweenness_2016 <- analyze_influential_node_impact(country2016, influentials)

### 2019 ###
influentials <- as.character(influential_list2019$BETWEENNESS)
compare_betweenness_2019 <- analyze_influential_node_impact(country2019, influentials)
rbind(compare_betweenness_2010, compare_betweenness_2013, compare_betweenness_2016, compare_betweenness_2019)

# Merge all into a dataset
dt <- data.frame()
dt <- rbind(dt, df2010, df2013, df2016, df2019)
dt$betweenness_rank <- NaN
for (year in years) {
  x <- dt$betweenness[dt$year == year]
  x <- max(x) - x # swap the order
  dt$betweenness_rank[dt$year == year] <- rank(x, ties.method='min')
}
head(arrange(dt, betweenness_rank)[,c("name", "betweenness_rank")], 20)

y1 <- 2016
y2 <- 2019
measures <- "betweenness_rank"
ds <- data.frame(name=unique(dt$name))
y1_column <- paste("year", y1, sep='_')
y2_column <- paste("year", y2, sep='_')

lookup <- dt[dt$year == y1, c("name", measure)]
names(lookup) <- c("name", y1_column)
ds <- merge(lookup, ds, by="name")
lookup <- dt[dt$year == y2, c("name", measure)]
names(lookup) <- c("name", y2_column)
ds <- merge(lookup, ds, by="name")
# Calculate difference in ranking. +Ve value means progress; -ve means decline
ds$difference <- ds[, y1_column] - ds[, y2_column]

# Limit to only top relevant cities
limit <- 100
ds <- ds[ds[, y1_column] < sort(ds[, y1_column])[limit] & ds[, y2_column] < sort(ds[, y2_column])[limit],]

colour_lookup <- sort(unique(ds$name))
# Prepare PNG to save plot
png(paste(root_dir, "country_", y1, "-", y2, "_", measure, ".png", sep=""), width=1500, height=1500, res=200)
par(oma=rep(0, times=4)) # all sides have equal lines of space
plot(range(ds[, y1_column]), range(ds[, y2_column]), type="p", xlab=y1_column, ylab=y2_column, cex=0)
title(paste("Distribution of node ranks", measure))
# Associate a colour with each country
for (i in 1:nrow(ds)) {
  text(ds[i, y1_column], ds[i, y2_column], labels=tolower(get_country_name(ds$name[i])), cex= 0.6, col=pick_colour(ds$name[i]), font=2)
}
# Plot mid lines
xmid <- mean(range(ds[, y1_column]))
xmax <- range(ds[, y1_column])[2]
ymid <- mean(range(ds[, y2_column]))
ymax <- range(ds[, y2_column])[2]
segments(x0=xmid, y0=0, x1=xmid, y=ymax, col="red")
segments(x0=0, y0=ymid, x1=xmax, y=ymid, col="red")
# Save plot
dev.off()


# All country rankings over the years
all_country_ranks <- data.frame(name=c(), country=c(), r_2010=c(), r_2013=c(), r_2016=c(), r_2019=c())
countries <- unique(dt$name)
countries <- countries[countries %in% dt$name[dt$year==2010] & countries %in% dt$name[dt$year==2013] & countries %in% dt$name[dt$year==2016] & countries %in% dt$name[dt$year==2019]]
for (country in countries) {
  row <- data.frame(name=country,
                    r_2010=dt$betweenness_rank[dt$year==2010 & dt$name==country],
                    r_2013=dt$betweenness_rank[dt$year==2013 & dt$name==country],
                    r_2016=dt$betweenness_rank[dt$year==2016 & dt$name==country],
                    r_2019=dt$betweenness_rank[dt$year==2019 & dt$name==country])
  all_country_ranks <- rbind(all_country_ranks, row)
}
all_country_ranks <- unique(all_country_ranks)
all_country_ranks
write_clip(all_country_ranks)
