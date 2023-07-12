# codes are written in  R studio
```r
# reading the data
wine <- read.csv(file.choose())
summary(wine)
wine1 <- wine[,2:14] # removing 1st column
```
```r
# executing and plotting PCA
wine_pca <- princomp(wine1, cor = TRUE, scores = TRUE, covmat=NULL)
summary(wine_pca)

wine_pca$loadings
wine_pca$scores
scores <- wine_pca$scores

plot(wine_pca$scores[,1:2], col = "white", pch = 19, cex = 1)
```
```r
# plotting different scores
plot(wine_pca$scores[,2:3], col = "blue", cex=1)
plot(wine_pca$scores[,5:6], col = "purple", cex = 1)
# plotting all scores
pairs(wine_pca$scores)
```
```r
# installing packages
install.packages(c("FactoMineR", "factoextra"))
library(factoextra)
library(FactoMineR)
```
```r
# executing hierarchical clustering using 3 PCA scores
wine_3_pca <-  PCA(wine1,ncp = 3, graph = FALSE) 
wine_3_pca_heirarchical_clust <- HCPC(wine_3_pca,nb.clust = 3, graph = TRUE) 
```
```r
# plotting the clusters
fviz_cluster(wine_3_pca_heirarchical_clust,repel = TRUE,show.clust.cent = TRUE, cex = 0.8, pallete = "jco", rect = TRUE, rect_fill= TRUE, rect_border = "jco", labels_track_size = 0.8)
```
```r
# checking the accuracy
wine_heirarchical_final <- data.frame(wine[,1], wine_3_pca_heirarchical_clust$data.clust$clust)
conf <- table(wine_heirarchical_final)
conf
```
```r
# calculating the K-score for K-means clustering
scores <- as.data.frame(scores)

wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(scores[,1:3], centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")
```
```r
# executing K-means clustering
wine_kmeans <- kmeans(scores[,1:3],3)
kmeans_cluster <-wine_kmeans$cluster
kmeans_cluster
wine_kmeans_final_data <- data.frame(wine[,1],kmeans_cluster)
```
```r
# checking the accuracy
kmean_table <- table(wine_kmeans_final_data)
kmean_table
```






