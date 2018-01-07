library(flexclust)
library(igraph)
#Load Iris
k <- 3
tiris=read.table("c:\\Datasets\\iris.data")
irisgd=tiris[5]
tiris=tiris[1:4]
dataset=tiris
datasetGD=irisgd
clusterK=3
sdDataset=sd(dataset)
maxCol <- names(which.max(sdDataset))
sortedData <- dataset[with(dataset,order(dataset[maxCol])),] #tail(dataset[order(dataset[maxCol]),],)
sortedData
incr = NULL
index = NULL
incr = length(dataset[,1])/k
start = 1
end = incr
data <- NULL
for(i in 1:k) {
	centroids <- colMeans(sortedData[start:end,])
	print(centroids)
	data = rbind(data, centroids)
	start <- start + incr
	end <- end + incr
}
distance <- NULL
clusters <- NULL
for(i in 1:length(dataset[,1])) {
	min <- 99
	index <- 1
	for (j in 1:length(data[,1])) {
		distance <- dist(rbind(dataset[i,], data[j,]))
		new <- distance[1]
		if (new < min) {
			index <- j
			min <- new
		}
	}
	#newRow <- c(i, index)
	clusters <- c(clusters, index)
}
tab <- table(clusters, c(irisgd))