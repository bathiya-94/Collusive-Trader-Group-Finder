library("dplyr")
library(plyr)

#Read the dataset
df_trades = read.csv("Trades.csv", header = TRUE)

# Filter the selected trades
selected_stock<- 'ES0158252033'
selected_trades<-subset(df_trades, df_trades$Stock == selected_stock)


# summarize data by buy broker and compute sum, mean, and std of traded quantities
df_trades_sumby_buybroker <- ddply(selected_trades, c("Buy.Broker.ID"),
                                   summarise, sum = sum(Executed.Qty), mean = mean(Executed.Qty), sd = sd(Executed.Qty))


# Using the Elbow method by considering Within Cluster Sum Squares to find the optimum no of Clusters 
set.seed(10)
wcss <- vector()
for (i in 1:10) wcss[i]<-sum(kmeans(df_trades_sumby_buybroker$sum,i)$withinss)
plot(1:10,wcss, type= "b", main = paste('The Elbow Method for Buyer Outliers'), xlab = "Number of Clusters", ylab = "WCSS")

#The Optimum number of clusters is 4


# Applying K means
kmeans <- kmeans(df_trades_sumby_buybroker$sum, 4)

# Plot the Kmeans Results
plot(df_trades_sumby_buybroker$sum, col = kmeans$cluster,main = paste('Clusters of Buyers'),
     xlab = "Index",ylab = "Sum of Stocks bought by each buyer")

kmeans$centers[, 1]

# plot cluster centers
points(kmeans$centers[, 1], col = 1:2, pch = 8, cex = 2) 

# Calulating standarad deviations
mean.result <- mean(df_trades_sumby_buybroker$sum)
std.result<- sqrt(var(df_trades_sumby_buybroker$sum))
upper_limit<-3*std.result + mean.result



# Finding the outliers 
outliers <- vector()
for (i in 1:nrow(kmeans$centers)){
  if (kmeans$centers[i] > upper_limit){
    outliers <- c(outliers,i)
  }
}

kmeans$cluster
outliers