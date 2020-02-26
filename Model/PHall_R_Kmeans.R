library(corrplot)
library(cluster) 
library(factoextra)
require(fastcluster)
require(graphics)
library(dendextend)
library(gplots)

#import the data
customer <- read.table("C:/Users/607791/Desktop/DS/Wholesale customers data.csv", header = TRUE, sep = ",")
head(customer)
customer <- na.omit(customer)
summary(customer)
#corrplot of the data to see how the variables relate
corrmatrix <- cor(customer)
corrplot(corrmatrix, method = 'number')
#removig the channel and region data as it has a different scale
df <- customer[-c(1,2)]
head(df,3)

#silhouette method
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
#plot of silhouette scores and number of clusters
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#factoextra package to better visualize the optimal k clusters
#silhouette average width clusters
fviz_nbclust(df, kmeans, method='silhouette')
#total within sum of square clusters
fviz_nbclust(df, kmeans, method='wss')
#gap statistics clustering
fviz_nbclust(df, kmeans, method='gap_stat')

#factoextra visual of cluster 2
km_2 <- kmeans(df, 2)
fviz_cluster(km_2, data=df)
points(km_2$centers[,c("Fresh", "Frozen")], col=1:4, pch=8, cex=2)
#cluster including explaination percent for cluster 2
data$cluster2 <- km_2$cluster
clusplot(data, data$cluster2, color=TRUE, shade = TRUE, label=2)

#factoextra visual of cluster 5
km_5 <- kmeans(df, 5)
fviz_cluster(km_5, data=df)
#cluster including explaination percent for cluster 5
data$cluster5 <- km_5$cluster
clusplot(data, data$cluster5, color=TRUE, shade = TRUE, label=2)

#factoextra visual of cluster 7
km_7 <- kmeans(df, 7)
fviz_cluster(km_7, data=df)
#cluster including explaination percent for cluster 7
data$cluster7 <- km_7$cluster
clusplot(data, data$cluster7, color=TRUE, shade = TRUE, label=2)

#hierarchical clustering
hcl <- dist(df[,-c(1,2)])
#complete method
hclust <- hclust(hcl, method = "complete")
par(mfrow=c(1,3))
plot(hclust)
#single method
hclust_single <- hclust(hcl, method = "single")
#average method
hclust_average <- hclust(hcl, method = "average")
#centroid method
hclust_centroid <- hclust(hcl, method = "centroid")
#plot method comparisons side by side
par(mfrow=c(1,2))
plot(hclust_single)
plot(hclust_average)
par(mfrow=c(1,2))
plot(hclust_single)
plot(hclust_centroid)