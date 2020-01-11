# imporing libraries
library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(dplyr) 
library(arules)         # Apriori Algorithm library

# Reading the dataset
df_trades = read.csv("Trades.csv", header = TRUE)

# Filtering the dataset for the required stock
selected_stock<- 'ES0158252033'
selected_trades<-subset(df_trades, df_trades$Stock == selected_stock)
selected_trades2 <- selected_trades

#####################################################################################################################################
# Using Trailing Moving Averages  to detect  Price Changges
#####################################################################################################################################s

# Calculating trailing moving average with 50 data points
selected_trades <-selected_trades %>%
  select(Trade.Date,Executed.Price)%>%
  mutate(trailingAverage = rollmean(Executed.Price, k=50, fill =NA,align ="right"))

# Removing rows for which there is no moving average.
selected_trades <- selected_trades[50:1980,]

# Adding an index to the data rows
selected_trades$ID<- seq.int(nrow(selected_trades))

# Plot the index vs Stock Price graph
plot(selected_trades$ID, selected_trades$Executed.Price, type = 'l', xlab = "Index",
     ylab = "Stock Price",col="blue", main = "Variation of Prices for Stock ES0158252033 " ,lwd=1.5)

# Plot the Trailing moving average curve
lines(selected_trades$ID,selected_trades$trailingAverage, col="red" ,lwd=2)


# Calculating the percentage change stock price with respect Moving Trailing Average
perecentage_change <- vector()

for ( i in 1:nrow(selected_trades)){
  value <- abs(selected_trades$Executed.Price[i] - selected_trades$trailingAverage[i])/selected_trades$trailingAverage[i]
  perecentage_change <- c(perecentage_change,value*100)
  
}
selected_trades <- cbind(perecentage_change,selected_trades)

#Selecting the  points where prices have drop and rose
min_max_points <- vector()
max_index <-vector()

for(i in 1:1931){
  if(selected_trades$perecentage_change[i]>2){
    min_max_points <- c(min_max_points, selected_trades$Executed.Price[i])
    max_index <-c(max_index,i)
  } 
}

# Ploting the price changes points
points(max_index,min_max_points,col="yellow" ,pch =1)

#Legend
legend(x="topright",y=0.92, legend=c("Executed Price","Trailing Moving Average","Price Changes"),
       col=c("blue", "red","yellow"),lty=c(1,1,NA),pch =c(NA, NA, 1) ,cex=0.8 ,lwd = 2)

#Finding the relevants  buyers and related to above points
selected_trades2 <-selected_trades2[50:1980,]
selected_trades2$ID <- seq.int(nrow(selected_trades2))

suspected_transactions <- selected_trades2[match(max_index,selected_trades2$ID),]

#####################################################################################################################################
# Applying Apriori Algorithm Detect Collusive Traders
#####################################################################################################################################

#Getting the unique dataset
suspected_transactions2 <- suspected_transactions[c(7,8)]
suspected_transactions2$const = TRUE
suspected_transactions2 <- unique(suspected_transactions2)

#Forming a  sparse matrix for relevant transactions to fits in Apriori

orders_mat_prep <- reshape(data = suspected_transactions2,
                           idvar = "Sell.Broker.ID",
                           timevar = "Buy.Broker.ID",
                           direction = "wide")

order_matrix <- as.matrix(orders_mat_prep[,-1])
order_matrix[is.na(order_matrix)] <- FALSE

colnames(order_matrix) <- gsub(x=colnames(order_matrix),
                               pattern="const\\.", replacement="")

order_trans2 <- as(order_matrix,"transactions")

# Summary and Frequency
summary(order_trans2)
itemFrequencyPlot(order_trans2)

#Applying  Apriori
rules = apriori(data = order_trans2 , parameter = list(support = 0.5, confidence = 0.8 ,maxlen =5 ,minlen =2))
inspect(sort(rules,by ='lift')[1:10])
