library(tm)
library(igraph)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(slam)
library(Matrix)
library(wordcloud)
library(psych)

# Hypotheses:
# 1. The influential users on the basis of frequency of tweets to a topic is comparable to the influence spread by any traditional influence algorithm
# 2. The influential content can determine whether a tweet will be retweeted or not. Why is this not happening?
# First check whether most retweeted tweets are present in the largest component or not
# Secondly, check if the influential node is not retweeted, are its neighbours retweeted?
# 3. In a content-network, influential nodes through maximization are either high-centrality nodes, or nodes in the 'n'th neighbourhood of these nodes

## Print output to results.txt file in current working directory
out <- function(x) {
  cat(date(), ": ", x, "\n", file="content_results.txt", append=TRUE)
}

## Function to calculate distance between two sentences
sentence_distance <- function (a, b, split=" ") {
  a_arr <- unlist(strsplit(a, split))
  b_arr <- unlist(strsplit(b, split))
  matches <- NULL
  if (length(a_arr) > length(b_arr))
    matches <- which (b_arr %in% a_arr)
  else
    matches <- which (a_arr %in% b_arr) 
  diff <- length(matches) / max(length(a_arr), length(b_arr))
  diff
}

## Function to limit twitter dataset using different filters
limit_tweets <- function(data, no_hashtags=FALSE, no_links=TRUE, english_only=TRUE, no_duplicates=TRUE, limit=0) {
  # Remove hashtags from existing text
  if (no_hashtags)
    data$text <- gsub(pattern="[#\\S]+", replacement="", x=data[,4])
  # Remove links from existing text
  if (no_links) {
    data$text <- gsub(pattern="(http://)+", replacement="", x=data[,4])
    data$text <- gsub(pattern="(https://)+", replacement="", x=data[,4])
  }
  # Limit to english tweets
  if (english_only)
    data <- data[data$lang=="en",]
  # Remove retweets
  #data <- data[data$retweet==FALSE,]
  # Remove non retweeted
  #data <- data[data$rt_count==0,]
  # Limit data size by picking n number of nodes randomly
  if (limit != 0) {
    data$tweet_id <- 1:nrow(data)
    data <- data[data$tweet_id %in% sample(data$tweet_id, limit, replace=FALSE),]    
  }
  
  # Remove too similar tweets
  if (no_duplicates) {
    dupes <- NULL
    for (i in 1:nrow(data)) {
      for (j in i:nrow(data)) {
        if (i == j)
          next
        dist <- sentence_distance(data$text[i], data$text[j])
        if (dist >= 0.9) # Tune this number to control thickness of graph
        #if (data$text[i] == data$text[j]) # Exact match
          dupes <- append(dupes, j)
      }
      print(i)
    }
    data <- data[!rownames(data) %in% unique(dupes),]
  }
  data
}

# Using text mining functions, convert the data into a a term-frequency x inverse document-frequency matrix
create_tfidf_matrix <- function (data, no_punctuations=TRUE, no_numbers=TRUE, no_whitespaces=TRUE, stemming=TRUE, additional_stopwords=c()) {
  ## Turn all text and hashtags to lower case to avoid case disruption
  data$content <- tolower(data$content)    
  # IMPORTANT: in order to retrieve the tweet Id back, we need to create a mapping
  tweetReader <- readTabular(mapping=list(content="text", id="tweet_id"))
  ## Read tweets text and save as a corpus; reader control writes respective tweet_id in timension names
  corpus = Corpus(DataframeSource(data), readerControl=list(reader=tweetReader))
  if (no_punctuations)
    corpus <- tm_map(corpus, removePunctuation)
  if (no_numbers)
    corpus <- tm_map(corpus, removeNumbers)
  if (no_whitespaces)
    corpus <- tm_map(corpus, stripWhitespace)
  ## Apply stemming with the snowball stemmers (requires SnowballC, RWeka, rJava, RWekajars)
  if (stemming) {
    corpus <- tm_map(corpus, stemDocument)
    corpus_dict <- corpus
    ## WARNING! Takes some time
    stemmed <- tm_map(corpus, function(x) stemCompletion(x, type="shortest", dictionary=corpus_dict))
  }
  ## Build term-document matrix, remove stop words
  stops <- c(additional_stopwords, stopwords("SMART"))
  # Convert into weighted TF-IDF
  tdm = TermDocumentMatrix(corpus, control=list(stopwords=stops, weighting=weightTfIdf))
  ## Print top frequent terms
  frequent <- findFreqTerms(tdm, lowfreq=sqrt(tdm$ncol))
  print(frequent)
  # Find the sum of words in each Document
  row_totals <- row_sums(tdm, na.rm=TRUE, dims=2)
  ## Restrict the term-document matrix to only non-empty documents
  tdm <- tdm[, row_totals > 0]

  # The TDM can be larger than integer limit, thus using sparseMatrix function
  tdm_matrix <- as.matrix(tdm)
  #tdm_matrix <- sparseMatrix(tdm$i, tdm$j, x=tdm$v)
  tdm_matrix
}

# Create word cloud from term-document matrix
create_wordcloud <- function(tfidf_matrix, min.freq=30, words=50) {
  word_freqs = sort(rowSums(tfidf_matrix), decreasing=TRUE)
  frequencies <- sort(rowSums(tfidf_matrix), decreasing=TRUE)
  avg <- mean(frequencies)
  frequencies <- frequencies[frequencies > avg]
  names <- names(frequencies)
  cloud <- data.frame(word=names, freq=frequencies)
  wordcloud(words=cloud$word, freq=cloud$freq, scale=c(5,0.5), max.words=words, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
}

# Create a Network (edge list) from a tfidf_matrix
create_network <- function (tfidf_matrix, initial_threshold=0.5, progressive_threshold=TRUE, threshold_method=c('mean','hm')) {
  edgelist <- data.frame(u=c(), v=c())
  threshold <- initial_threshold
  all_similarities <- c()
  similarities <- c(threshold)
  for (i in 1:ncol(tfidf_matrix)) {
    for (j in i:ncol(tfidf_matrix)) {
      if (i == j)
        next
      x <- tfidf_matrix[,i]
      y <- tfidf_matrix[,j]
      cosine_sim <- round(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)), 5)
      if (is.na(cosine_sim[1]))
        next
      # Cosine similarity should not be too high, to avoid potential duplicates after preprocessing
      else if (cosine_sim[1] < 0.05 | cosine_sim[1] > 0.95)
        next
      #print(cosine_sim)
      similarities <- rbind(similarities, cosine_sim[1])
      if (cosine_sim[1] >= threshold)
        edgelist <- rbind(edgelist, data.frame(colnames(tfidf_matrix)[i], colnames(tfidf_matrix)[j]))
      # Progressive threshold: learn by averaging through given fucntion in each iteration and adding previous similarities to next
      if (progressive_threshold) {
        if (threshold_method == 'mean')
          threshold <- mean(similarities)
        else if (threshold_method == 'hm')
          threshold <- harmonic.mean(c(similarities))
      }
    }
    all_similarities <- rbind(all_similarities, similarities)
    if (i %% 100 == 0)
      out(paste(i, "out of", ncol(tfidf_matrix), "current threshold is:", threshold, sep=" "))
  }
  edgelist
}

# Read the edgelist and replace node names with original text using tweets data
create_names_edgelist <- function (data, edgelist) {
  data$text <- gsub(" ", "_", data$text)
  for (i in 1:nrow(edgelist)) {
    a <- data[data$tweet_id == edgelist[i,1],c(1,3,4)]
    b <- data[data$tweet_id == edgelist[i,2],c(1,3,4)]
    edgelist[i,1] <- paste(a[1],a[2],a[3], sep="-")
    edgelist[i,2] <- paste(b[1],b[2],b[3], sep="-")
  }
  edgelist
}

# Assigns a rank to each row, based on ordering on column number provided
rank <- function(data, rank_by_colnum) {
  data <- data[with(data, order(data[,rank_by_colnum])),]
  data[,rank_by_colnum] <- as.factor(data[,rank_by_colnum])
  data$rank <- NULL
  data$rank <- abs(as.numeric(data[,rank_by_colnum]))
  data$rank <- abs(1 + data$rank - max(data$rank))
  data <- data[with(data, order(rank)),]
  data
}

#######################################################################################################################

# DATA PREPROCESSING USING TEXT MINING
query <- "../data/my_twitter_network"
input <- paste(query, "_tweets.csv", collapse="", sep="")
output <- paste(query, "_tweets_clean.csv", collapse="", sep="")
graph_output <- paste(query, "_edgelist_txt.csv", collapse="", sep="")
edge_names <- paste(query, "_edgenames.csv", collapse="", sep="")
results <- "results/content_network_results.txt"

out("Process started")
out("Experiment finds the influential nodes in a content-network using High-degree influence mining and influence maximization under Linear Threshold model")

## Read all english tweets from Obama network. We choose English, because of available stop words, lemmatization and stemming techniques as well as Obama
data <- read.csv(input, as.is=TRUE)
data <- limit_tweets (data, no_hashtags=FALSE, no_links=TRUE, english_only=TRUE, no_duplicates=FALSE, limit=1000)
# Save to a different file
write.csv(x=data, file=output, append=FALSE, row.names=FALSE)
out("Removed links, non-english and duplicate tweets")

data$content <- data$text # Use text only
#data$content <- paste(data$text, data$hashtags)
#data <- data[data$hashtags != "",] # Limit tweets to those containing hashtags only

tfidf_matrix <- create_tfidf_matrix (data, no_punctuations=TRUE, no_numbers=TRUE, no_whitespaces=TRUE, stemming=FALSE, additional_stopwords=query)
out("Created TF-IDF matrix from tweets, removing punctuations, numbers, whitespaces and stopwords")

wordcloud <- create_wordcloud(tfidf_matrix, words=100)
out("Created Wordcloud from Term-Document matrix")

edgelist <- create_network(tfidf_matrix, initial_threshold=1, progressive_threshold=TRUE, threshold_method='mean')
out("Created a network from tweets using cosine similarity of content; progressive threshold calculated using harmonic mean")

## Write space-separated edge list to file without using quotes, row/column names
write.table(x=edgelist, file=graph_output, quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE, append=FALSE)
out("Wrote edgelist to file.")

# edgenames <- create_names_edgelist (data, edgelist)
# write.table(x=edgelist, file=edge_names, quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE)
# write(paste("Saved tweet_id,username,text-based edge list in:", edge_names), file=results, append=TRUE)


#########################################################################################################
## INFLUENCE MINING ##
edgesFile <- graph_output
steps=999
k=10
p=0.5   # Should this be probability of retweets?
model <- "LT" # "IC"
cent_methods <- c("random", "degree", "closeness", "betweenness", "coreness", "eigenvector", "a-degree", "a-closeness", "a-betweenness", "a-coreness", "a-eigenvector")
out("Influence mining parameters: k=50, steps=999, probability=0.5")

# IMPORTANT! Read as a table first, or igraph will try to store in integers and will throw error
edgedata <- read.table(edgesFile)
graph <- graph.data.frame(edgedata, directed=FALSE)
# Find all clusters in the graph
components <- clusters(graph)
# Restrict graph to largest connected component
graph = induced.subgraph(graph, V(graph)[which(components$membership==which.max(components$csize))])
nodes <- V(graph)
edges <- E(graph)
centralities <- NULL

length(edges)/length(nodes)
average.path.length(graph)
diameter(graph)
mean(degree(graph))
transitivity(graph, type="global")


print(graph)
out(paste("Nodes:", length(nodes), "Edges:", length(edges)))

for (method in cent_methods) {
    print(method)
    #inf_cent <- influence(graph, seed=NULL, budget=k, steps, model="IC", seed_method="degree", maximize=FALSE, prob=p)
    inf_cent <- influence(graph, seed=NULL, budget=k, steps, model=model, seed_method=method, maximize=FALSE, prob=p)

    # Retrieve tweet text from influential node ids
    inf_tweets <- nodes[inf_cent$initial_seed]$name
    tweets <- read.csv(output)
    tweets[tweets$tweet_id %in% inf_tweets,c(1:3,13)]
    
    rt_tweets <- tweets[tweets$rt_count > 0,c(1:4,13)]
    write(paste("Ratio of retweeted tweets:", nrow(rt_tweets)/nrow(tweets)), file=results, append=TRUE)
    hd_tweets <- tweets[tweets$tweet_id %in% inf_tweets,c(1:4,13)]
    rt_hd_tweets <- tweets[tweets$tweet_id %in% inf_tweets & tweets$rt_count > 0,c(1:4,13)]
    out(paste("Ratio of retweeted nodes in high", method, "nodes:", nrow(rt_hd_tweets)/nrow(hd_tweets)))
}


# Calculate all centralities for each node
degrees <- degree(graph)
closenesses <- closeness(graph)
betweennesses <- betweenness(graph)
eigenvectors <- evcent(graph)$vector
corenesses <- graph.coreness(graph, mode="all")
centralities <- data.frame(tweet_id=nodes$name, degree=(degrees), close=(closenesses), between=(betweennesses), eigen=(eigenvectors), core=(corenesses))

# Sort by degree
sorted_degree <- centralities[with(centralities, order(-degree)),]
sorted_close <- centralities[with(centralities, order(-close)),]
sorted_between <- centralities[with(centralities, order(-between)),]
sorted_eigen <- centralities[with(centralities, order(-eigen)),]
sorted_core <- centralities[with(centralities, order(-core)),]

ranked_degree <- rank(sorted_degree, 2)
ranked_close <- rank(sorted_close, 3)
ranked_between <- rank(sorted_between, 4)
ranked_eigen <- rank(sorted_eigen, 5)
ranked_core <- rank(sorted_core, 6)

# Rank all tweets by retweet count
sorted_tweets <- tweets[with(tweets, order(-rt_count)),]
ranked_tweets <- rank(sorted_tweets, 13)
head(ranked_tweets, n=5)

# Merge all the data frames by ranks for each centrality into main ranked data frame
library(sqldf)
ranked_data <- sqldf("select r.tweet_id, r.user_id, r.screen_name, r.rt_count, r.rank as rt_rank, d.rank as degree_rank, cl.rank as close_rank, b.rank as between_rank, e.rank as eigen_rank, co.rank as core_rank from ranked_tweets as r inner join ranked_degree as d on d.tweet_id = r.tweet_id inner join ranked_close as cl on cl.tweet_id = r.tweet_id inner join ranked_between as b on b.tweet_id = r.tweet_id inner join ranked_eigen as e on e.tweet_id = r.tweet_id inner join ranked_core as co on co.tweet_id = r.tweet_id where r.rt_count > 0 order by r.tweet_id")
write.csv(x=ranked_data, file="cricket_tweets_ranked.csv")
out("Wrote CSVs of Ids, ranked by all centrality methods used.")

# Influence functions
#maxim <- influence(graph, seed=NULL, budget=k, steps, model=model, maximize=TRUE, prob=p)

# Retrieve tweet text from influential node ids
# maxim_tweets <- nodes[maxim$influential_nodes]$name
# tweets <- read.csv(output)
# tweets[tweets$tweet_id %in% maxim_tweets,c(1:3,13)]
# 
# rt_tweets <- tweets[tweets$rt_count > 0,c(1:4,13)]
# hd_tweets <- tweets[tweets$tweet_id %in% maxim_tweets,c(1:4,13)]
# rt_hd_tweets <- tweets[tweets$tweet_id %in% maxim_tweets & tweets$rt_count > 0,c(1:4,13)]
# write(paste("Ratio of retweeted nodes in Greedy method", "nodes:", nrow(rt_hd_tweets)/nrow(hd_tweets)), file=results, append=TRUE)


# # Compare both the sets of nodes
# deg_nodes <- nodes[inf_cent$initial_seed]
# max_nodes <- nodes[max_lt$influential_nodes]
# common <- intersect(deg_nodes, max_nodes)
# commonality <- length(common) / length(deg_nodes)
# commonality
# write(paste("Commonality between both sets of nodes:", commonality), file=results, append=TRUE)
# 
# # Check if the rest of the nodes are in immediate neighbourhood
# lt_neighbours <- neighborhood(graph, order=1, nodes=deg_nodes)
# neighbours <- nodes[unique(sort(unlist(lt_neighbours)))]
# write(paste("First neighbours of HD:", neighbours), file=results, append=TRUE)
# # Generate subgraph of these neighbours
# sub <- induced.subgraph(graph, unlist(lt_neighbours))
# # Calculate commonality again
# common <- intersect(neighbours, max_nodes)
# commonality <- length(common) / length(max_nodes)
# commonality
# write(paste("Commonality between maximizing nodes and immediate neigbours of high-degree nodes:", commonality), file=results, append=TRUE)
# 
# write(paste("Finished on:", date()), file=results, append=TRUE)
# 
