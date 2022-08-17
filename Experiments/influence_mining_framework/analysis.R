# Load in Workbook
file <- "C:/Users/owais/Dropbox/Knowledge/PhD/Projects/influence-mining/Experiments/influence_mining_framework/consolidated_results.csv"
results <- read.csv2(file, header=TRUE, sep=',', stringsAsFactors=FALSE)
results$threshold <- as.numeric(results$threshold)
results$time <- round(as.numeric(results$time), 6)

budget <- results[results$criteria == "BUDGET", ]
networks <- unique(budget$graph)
methods <- unique(budget$influence_function)

# Identify which method performs best in influence on minimum budget
columns <- c("graph", "test_method", "criteria", "size", "threshold", "influence_function", "time", "influence")
karate <- budget[budget$graph == 'karate' & budget$threshold == 0.01, columns]

test_methods <- factor(budget$test_method)
levels(test_methods) <- c("RESILIENCE", "INFLUENCE_LT", "INFLUENCE_IC")
karate_table <- xtabs(~karate$influence + karate$test_method, data=karate)
rownames(karate_table) <- methods

barplot(karate_table,
        main="karate",
        xlab="Method",
        ylab="Influence",
        beside=TRUE,
        legend.text=TRUE,
        args.legend=list(x="topright", inset=c(-0.15, -0.15))
)

###########################################
### Analyze results from BUDGET results ###
###########################################

criteria <- "BUDGET"
x <- results[results$criteria == criteria, ]
summarized <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), max_inf=c(), max_inf_function=c())
thresholds <- c(0.01, 0.02, 0.03, 0.04, 0.05)

for (test_method in c("RESILIENCE", "INFLUENCE_IC", "INFLUENCE_LT")) {
  # In each graph
  for (graph in unique(x$graph)) {
    # And threshold
    for (threshold in thresholds) {
      # Print the algorithm with highest influence
      y <- x[x$criteria == criteria & x$test_method == test_method & x$threshold == threshold & x$graph == graph,]
      if (nrow(y) == 0) {
        threshold <- 0.00125
        y <- x[x$criteria == criteria & x$test_method == test_method & x$threshold == threshold & x$graph == graph,]
      }
      max_inf <- max(y$influence)
      max_inf_function <- y$influence_function[y$influence == max_inf]
      print(paste(criteria, test_method, threshold, graph))
      df <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), max_inf=c(), max_inf_function=c())
      for (mif in max_inf_function) {
        df <- rbind(df, data.frame(criteria, test_method, threshold, graph, max_inf, mif))
      }
      print(graph)
      write.table(df, output_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=',')
      if (!is.na(max_inf))
        summarized <- rbind(summarized, df)
    }
  }
}

summarized
# Copy to clipboard to move to Excel
clipr::write_clip(summarized)


##############################################
### Analyze results from INFLUENCE results ###
##############################################

criteria <- "INFLUENCE"
test_method <- "RESILIENCE"
x <- results[results$criteria == criteria & !results$graph %in% c('fb_johnshopkins', 'caida', 'linux_code'), ]
summarized <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), min_time=c(), min_time_function=c())
thresholds <- c(0.005, 0.01, 0.033, 0.067, 0.1)

for (test_method in c("RESILIENCE", "INFLUENCE_IC", "INFLUENCE_LT")) {
  # In each graph
  for (graph in unique(x$graph)) {
    # And threshold
    for (threshold in thresholds) {
      print(paste(graph, test_method))
      print(threshold)
      # Print the algorithm with highest influence
      y <- x[x$criteria == criteria & x$test_method == test_method & x$threshold == threshold & x$graph == graph,]
      if(nrow(y) == 0) {
        print("FAILED! FAILED! FAILED!")
        next
      }
      min_time <- min(y$time)
      min_time_function <- y$influence_function[y$time == min_time]
      print(paste(criteria, test_method, threshold, graph))
      df <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), min_time=c(), min_time_function=c())
      for (mtf in min_time_function) {
        df <- rbind(df, data.frame(criteria, test_method, threshold, graph, min_time, mtf))
      }
      write.table(df, output_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=',')
      if (!is.na(max_inf))
        summarized <- rbind(summarized, df)
    }
  }
}

summarized
# Copy to clipboard to move to Excel
clipr::write_clip(summarized)



x <- results[results$criteria == criteria & results$graph %in% c('fb_johnshopkins', 'caida', 'linux_code'), ]
summarized <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), min_time=c(), min_time_function=c())
thresholds <- c(0.005, 0.01, 0.025)

for (test_method in c("RESILIENCE", "INFLUENCE_IC", "INFLUENCE_LT")) {
  # In each graph
  for (graph in unique(x$graph)) {
    # And threshold
    for (threshold in thresholds) {
      print(paste(graph, test_method))
      print(threshold)
      # Print the algorithm with highest influence
      y <- x[x$criteria == criteria & x$test_method == test_method & x$threshold == threshold & x$graph == graph,]
      if(nrow(y) == 0) {
        print("FAILED! FAILED! FAILED!")
        next
      }
      min_time <- min(y$time)
      min_time_function <- y$influence_function[y$time == min_time]
      print(paste(criteria, test_method, threshold, graph))
      df <- data.frame(criteria=c(), test_method=c(), threshold=c(), graph=c(), min_time=c(), min_time_function=c())
      for (mtf in min_time_function) {
        df <- rbind(df, data.frame(criteria, test_method, threshold, graph, min_time, mtf))
      }
      write.table(df, output_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=',')
      if (!is.na(max_inf))
        summarized <- rbind(summarized, df)
    }
  }
}

summarized
# Copy to clipboard to move to Excel
clipr::write_clip(summarized)
