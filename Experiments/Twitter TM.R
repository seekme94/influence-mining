setwd("D:\\Datasets\\Twitter")

## Function to calculate Euclidean distance between documents in term-document or document-term matrix
distance <- function(y, method=c("td", "dt")) {
    if (method=="td")
        y / apply(y, 2, function(x) sqrt(sum(x^2)))
    else
        y / apply(y, 1, function(x) sqrt(sum(x^2)))
}

## Function to calculate difference in words between two sentences
sentence_distance <- function (a, b, split=" ")
{
    a_arr <- unlist(strsplit(a, split))
    b_arr <- unlist(strsplit(b, split))
    matches <- NULL
    if (length(a_arr) > length(b_arr))
        matches <- which (b_arr %in% a_arr)
    else matches <- which (a_arr %in% b_arr) 
    diff <- length(matches) / max(length(a_arr), length(b_arr))
    diff
}

## Read all english tweets from Obama network. English, because of available stop words, lemmatization and stemming techniques as well as Obama
data <- read.csv("obama_tweets.csv", as.is=TRUE)
# Remove hashtags and links from existing text
data$text <- gsub(pattern="[#\\S]+|[http://]+", replacement="", x=data[,4])

########### OPTIONAL: Limit dataset ###################
# Limit to english tweets
data <- data[data$lang=="en",]
# Remove retweets
data <- data[data$retweet==FALSE,]
# Remove retweeted
#data <- data[data$rt_count==0,]
# Remove non-retweeted
# Limit data size
data <- data[data$tweet_id < 5000,]
#############################################################################

# Remove too similar tweets
dupes <- NULL
for (i in 1:nrow(data))
{
    for (j in i:nrow(data))
    {
        if (i == j)
            next
        dist <- sentence_distance(data$text[i], data$text[j])
        if (dist < 0.05)
            dupes <- append(dupes, j)
    }
    print(i)
}
data <- data[!rownames(data) %in% unique(dupes),]
# Save to a different file
write.csv(x=data, file="obama_tweets_clean.csv", append=FALSE, row.names=FALSE)
print ("Wrote clean dataset to file...")

########### CHOOSE HOW TO CREATE (HASHTAG/TEXT-BASED) NETWORK ###############
# Merge hashtags with text in order to make content more meaningful
data$content <- NULL
#data$content <- paste(data$text, data$hashtags)
#data <- data[data$hashtags != "",] # Limit to hashtags only
#data$content <- data$hashtags
data$content <- data$text
print("Data read...")
#############################################################################

## Turn all text and hashtags to lower case to avoid case disruption
data$content <- tolower(data$content)
print("Text changed to lower cases...")

# Text mining techniques (require tm)
library(tm)
# IMPORTANT: in order to retrieve the tweet Id back, we need to create a mapping
tweetReader <- readTabular(mapping=list(content="text", id="tweet_id"))
## Read tweets text and save as a corpus; reader control writes respective tweet_id in timension names
corpus = Corpus(DataframeSource(data), readerControl=list(reader=tweetReader))
print("Corpus created...")

## Apply stemming with the snowball stemmers (requires SnowballC, RWeka, rJava, RWekajars)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
#corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus_dict <- corpus
## WARNING! Takes some time
# stemmed <- tm_map(corpus, function(x) stemCompletion(x, type="shortest", dictionary=corpus_dict))
# print("Corpus stemmed...")
# inspect(stemmed[1:3])

## Build term-document matrix, remove stop words
stops <- c("obama", "barack", "barak", stopwords("SMART")) # source http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
tdm = TermDocumentMatrix(corpus, control=list(stopwords=stops))
## Find terms that do not repeat
unique <- findFreqTerms(tdm, highfreq=1)
## Find terms that exist in more than two-third of total documents
frequent <- findFreqTerms(tdm, lowfreq=sqrt(tdm$ncol))
stops <- c(stops, unique, frequent)
## Regenrate term-document matrix
tdm = TermDocumentMatrix(corpus, control=list(stopwords=stops))
print("Term-document matrix created...")
## Inspect matrix slice of 10 random terms in 20 random documents
# inspect(tdm[sample(tdm$i, size=10),sample(tdm$j, size=20)])
#frequent <- findFreqTerms(tdm, lowfreq=100) # Find frequent terms

row_totals <- apply(tdm, 2, sum) # Find the sum of words in each Document
## Restrict the term-document matrix to only non-empty documents
tdm <- tdm[, row_totals > 0]
## Build weighted term-frequency x inverse document-frequency matrix
tfidf <- weightTfIdf(tdm, normalize=TRUE)
print("Computed term-frequency x inverse document-frequency matrix")

## Convert into matrix
tfidf_matrix <- as.matrix(tfidf)
print("Conversion from TFxIDF into matrix")
r <- nrow(tfidf_matrix)
c <- ncol(tfidf_matrix)
#rownames(tfidf_matrix) <- 1:r
## Have a look at some random terms and documents
# tfidf_matrix[sample(1:r, size=10),sample(1:c, size=20)]

## Generate Word cloud from term-document matrix for term "love"
# library(wordcloud)
# matrix = as.matrix(tdm)
# word_freqs = sort(rowSums(matrix), decreasing = TRUE)
# frequencies <- sort(rowSums(matrix), decreasing=TRUE)
# print("Word frequencies calculated...")
# names <- names(frequencies)
# k <- which(names(frequencies)=="love")
# names[k] <- "love"
# cloud <- data.frame(word=names, freq=frequencies)
# print("Generated word cloud for term \"love\"...")
# wordcloud(cloud$word, cloud$freq, min.freq=3)

# Create edgelist by matching words
edgelist <- data.frame(u=c(), v=c())
for (i in 1:c-1) # For each column index
{
    for (j in i:c) # For each column index
    {
        if (i == j)
            next
        diff <- sum(tfidf_matrix[,i] * tfidf_matrix[,j])
        if (diff > 3)
        {
            edgelist <- rbind(edgelist, data.frame(colnames(tfidf_matrix)[i], colnames(tfidf_matrix)[j]))
        }
    }
    print(i)
}
## Write space-separated edge list to file without using quotes, row/column names
write.table(x=edgelist, file="obama_edgelist_txt.csv", quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE, append=FALSE)
print ("Wrote edgelist to file...")

## Read the edgelist file and replace node names with original text
# edgedata <- read.csv("obama_edgelist_ht.csv", sep=" ")
# data$text <- gsub(" ", "_", data$text)
# for (i in 1:nrow(edgedata))
# {
#     edgedata[i,1] <- paste(edgedata[i,1], data$text[data$tweet_id == edgedata[i,1]], sep="-")
#     edgedata[i,2] <- paste(edgedata[i,2], data$text[data$tweet_id == edgedata[i,2]], sep="-")
# }
# write.table(x=edgedata, file="obama_edgenames_ht.csv", quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE)

## The rest of the experiments are in Influence Mining project
