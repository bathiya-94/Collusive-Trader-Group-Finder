# Finding  the outliers  using  KMeans Clustering 


# Reading the dataset
df_trades = read.csv("Trades.csv", header = TRUE)


# Filtering the dataset according  using the selected dataset
selected_stock<- 'ES0158252033'
selected_trades<-subset(df_trades, df_trades$Stock == selected_stock)
selected_trades$ID<- seq.int(nrow(selected_trades))


# Using the Elbow method by considering Within Cluster Sum Squares to find the optimum no of Clusters 
set.seed(10)
wcss <- vector()
for (i in 1:10) wcss[i]<-sum(kmeans(selected_trades$Executed.Qty,i)$withinss)
plot(1:10,wcss, type= "b", main = paste('The Elbow Method'), xlab = "Number of Clusters", ylab = "WCSS")

#Using the elbow method we get 6 as optimal clusters

kmeans <- kmeans(selected_trades$Executed.Qty,3)


# Plot the clusters
plot(selected_trades$Executed.Qty, col=kmeans$cluster , main = paste('Stock Quantity Clusters'),
     xlab = "Index" , ylab = 'Executed Stock Quantity' )



# Plot Cluster Centers
points(kmeans$centers[, 1], col = 1:2, pch = 8, cex = 2) 

# Cluster centers
kmeans$centers[, 1]


# Calculating the  mean
mean.result = mean(selected_trades$Executed.Qty)

#Calculating the Standard deviation
sd.result = sqrt(var(selected_trades$Executed.Qty))

#Upper bound for the outliers
upper = mean.result + 3 *sd.result


library(plyr)

#Separating outliers
outliers<- vector()

for (i in 1:nrow(kmeans$centers)){
  if (kmeans$centers[i] > upper){
    outliers <- c(outliers,i)
  }
}

