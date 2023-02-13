############################### DATA PREPARATION ###############################

# Load data from CSV file and check its structure
rawSalesData <- read.csv("SuperstoreSalesTraining.csv", na.strings = "", stringsAsFactors = TRUE)
str(rawSalesData)
summary(rawSalesData)

# Remove unnecessary variables (rawSalesData$Row)
salesData <- subset(rawSalesData, select=-c(Row))
str(salesData)
summary(salesData)

# Filter NA
colnames(salesData)  # All columns
colnames(salesData[, colSums(is.na(salesData)) == 0])  # Non NA columns
colnames(salesData[, colSums(is.na(salesData)) > 0]) # NA columns ("Postal.Code", "SubRegion")
sum(is.na(salesData$Postal.Code))  # 6985 NA values
sum(is.na(salesData$SubRegion))    # 7316 NA values

salesData <- salesData[, colSums(is.na(salesData)) == 0]
str(salesData)
summary(salesData)


# Numeric and factor variables
isNumericColArr <- unlist(lapply(salesData, is.numeric), use.names = FALSE)
isFactorColArr <- unlist(lapply(salesData, is.factor), use.names = FALSE)

# Numeric: "Order" "Unit.Price" "Order.Quantity" "Sales" "Profit" "Shipping.Cost" "Customer_ID"
colnames(salesData[, isNumericColArr])

# Factor:
# "Order.Priority" "Order.Date" "Discount" "Product.Base.Margin" "Department" "Container"
# "Category" "Item" "Customer.Segment" "Customer.Name" "Region" "State"
# "Country...Region" "City" "Ship.Date" "Ship.Mode"
colnames(salesData[, isFactorColArr])



# Cast the data to the appropriate variable types
salesData$Order               <- as.character(salesData$Order)
salesData$Order.Date          <- as.Date(salesData$Order.Date, format = "%d/%m/%Y")
salesData$Discount            <- as.numeric(sub("%", "", salesData$Discount)) / 100
salesData$Product.Base.Margin <- as.numeric(sub("%", "", salesData$Product.Base.Margin)) / 100
salesData$Item                <- as.character(salesData$Item)
salesData$Customer.Name       <- as.character(salesData$Customer.Name)
salesData$Ship.Date           <- as.Date(salesData$Ship.Date, format = "%d/%m/%Y")

# sapply(salesData, class)
# salesDf <- salesData[, c('Order', 'Order.Date', 'Discount', 'Product.Base.Margin', 'Item', 'Customer.Name', 'Ship.Date')]
# str(salesDf)

boxplot(salesData$Unit.Price, salesData$Order.Quantity)
boxplot(salesData$Order.Quantity)
boxplot(salesData$Sales)
boxplot(salesData$Profit)
boxplot(salesData$Shipping.Cost)

# Remove outliers

# Detect outlier function
hasOutlier <- function(x) {
   quantile1 <- quantile(x, probs = 1/4)
   quantile3 <- quantile(x, probs = 3/4)
   IQR = quantile3 - quantile1  # Inter quartile range
   return(x > quantile3 + (IQR * 1.5) | x < quantile1 - (IQR * 1.5))
}

# Create remove outlier function
removeOutlier <- function(dataframe, columns = colnames(dataframe)) {
   for (col in columns) {
       # Keep observation if it doesnt have an outlier
       dataframe <- dataframe[!hasOutlier(dataframe[[col]]), ] 
   }
   return(dataframe)
}

# removeOutlier(salesDf)

# Outliers are (not) removed due to .

# Standardize data



############################ PRODUCT CLASSIFICATION ############################

# data selection and preparation

# install.packages(c("ggplot2", "rpart", "rpart.plot"))
library("ggplot2")
library("rpart")
library("rpart.plot")

head(salesData)
str(salesData)
classificationData <- salesData[, c("Order.Priority", "Discount", "Unit.Price",
                                    "Shipping.Cost", "Department", "Category",
                                    "Customer.Segment", "Region", "Ship.Mode",
                                    "Profit")]
# , "Order.Quantity", "Sales", "Product.Base.Margin"
str(classificationData)

# Selected data includes variables which can help select products to be marketed
# For example, it can be decided to market and advertise in specific regions,
# market and advertise specific categories of products...


mean(classificationData$Profit)   # 882.1462
median(classificationData$Profit) # 133.645

modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
modes(classificationData$Profit)  # 6.2

plot(classificationData$Profit)
abline(h = mean(classificationData$Profit), col = "#FF0000")
abline(h = median(classificationData$Profit), col = "#0000FF")
abline(h = 2000, col = "#00FF00")
# ggplot(as.data.frame(classificationData$Profit))

limit <- 2000 # low - high profit limit

classificationData$ProfitFactor <- factor(ifelse(classificationData$Profit < limit, "Low", "High"))
str(classificationData)
head(classificationData[, c("Profit", "ProfitFactor")], 40)
# 1 = High, 2 = Low

classificationData <- classificationData[, -grep("^Profit$", colnames(classificationData))]
str(classificationData)

# Profit variable is changed to ProfitFactor variable, which


# divide data to train and test datasets
RNGkind(sample.kind = "Rounding")
set.seed(2)
indices <- sample(nrow(classificationData), 0.7 * nrow(classificationData))
train <- classificationData[indices, ]
test <- classificationData[-indices, ]

str(train)
str(test)
nrow(train) / (nrow(train) + nrow(test))  # 0.6999643
nrow(test) / (nrow(train) + nrow(test))   # 0.3000357


# create classification tree based on training data
RNGkind(sample.kind = "Rounding")
set.seed(2)
tree <- rpart(ProfitFactor~., data = train, method = "class")
print(tree)
prp(tree)

# variable importance
print(tree$variable.importance)
barplot(tree$variable.importance)

# test classification tree on testing / validation data
prediction <- predict(tree, newdata = test, type = "class")
head(prediction)

accuracy <- sum(prediction == test$ProfitFactor) / nrow(test) * 100
print(accuracy) # 92.5

# pruning tree and performance advantages
print(tree$cptable)
#           CP nsplit rel error    xerror       xstd
# 1 0.12734082      0 1.0000000 1.0000000 0.02576849
# 2 0.01498127      2 0.7453184 0.7453184 0.02260634
# 3 0.01000000      4 0.7153558 0.7393258 0.02252364
min(tree$cptable[, "xerror"]) # 0.7393258
# xstd = 0.02252364
0.7393258 - 0.02252364  # 0.7168022
0.7393258 + 0.02252364  # 0.7618494
min(2, 4)  # nsplit = 2
# CP = 0.01498127

prunedTree <- prune(tree, cp = 0.01498127)
print(prunedTree)
prp(prunedTree)
# The tree is the same.


# INTERPRETATION



############################## GROUPING CUSTOMERS ##############################

# data selection and preparation

# install.packages(c("NbClust", "factoextra"))
library("NbClust")
library("factoextra")

head(salesData)
str(salesData)
groupingData <- salesData[, c("Discount", "Unit.Price", "Order.Quantity",
                              "Department", "Container", "Category",
                              "Customer.Segment", "Customer_ID", "Region",
                              "Country...Region", "City", "Ship.Mode")]
str(groupingData)


# remove outliers
groupingData <- removeOutlier(groupingData,
                              columns = c("Discount", "Unit.Price",
                                          "Order.Quantity", "Customer_ID"))
boxplot(groupingData)
head(groupingData)
str(groupingData)
nrow(groupingData)

# standardize
groupingData$Department <- as.numeric(groupingData$Department)
groupingData$Container <- as.numeric(groupingData$Container)
groupingData$Category <- as.numeric(groupingData$Category)
groupingData$Customer.Segment <- as.numeric(groupingData$Customer.Segment)
groupingData$Region <- as.numeric(groupingData$Region)
groupingData$Country...Region <- as.numeric(groupingData$Country...Region)
groupingData$City <- as.numeric(groupingData$City)
groupingData$Ship.Mode <- as.numeric(groupingData$Ship.Mode)
str(groupingData)

groupingDataScaled <- scale(groupingData)
head(groupingDataScaled)
str(groupingDataScaled)

# find optimal number of clusters (partitional, kmeans method)
numberOfClusters <- fviz_nbclust(groupingDataScaled, kmeans, method = "wss")
names(numberOfClusters)
# numberOfClusters$data$y
n_clust<-numberOfClusters$data
maxCluster<-as.numeric(n_clust$clusters[which.max(n_clust$y)])
print(maxCluster)

# grouping votes



# group data



# groups comparison




# INTERPRETATION



############################ TIME SERIES PREDICTION ############################

# data selection and preparation

# describing data

# clean data

# show moving averages

# data decomposition

# stationarity

# ARIMA model

# residuals

# forecasting by ARIMA model


# INTERPRETATION



############################ ADVANCED VISUALIZATION ############################

# data selection and preparation

# ggplot2 visualizations


# INTERPRETATION



######################## LINEAR MODEL PROFIT PREDICTION ########################
# install.packages(c('corrplot', 'PerformanceAnalytics', 'vcd'))
library('corrplot')
library('PerformanceAnalytics')
library('vcd')


# Data selection and preparation
numericSalesData = salesData[, sapply(salesData, is.numeric)]
factorSalesData = salesData[, sapply(salesData, is.factor)]

# Correlation between variables + corrplot
cor(numericSalesData)
corMatrix <- cor(numericSalesData)

# Numeric cor
corrplot(corMatrix)
chart.Correlation(corMatrix)

# Factor cor
summary(factorSalesData)
mosaicplot(Container ~ Ship.Mode, data=factorSalesData, shade=TRUE, legend=TRUE)
mosaicplot(Department ~ Ship.Mode, data=factorSalesData, shade=TRUE, legend=TRUE)
mosaicplot(Ship.Mode ~ Container + Department, data=factorSalesData, shade=TRUE, legend=TRUE)

# Linear model (automatic variable selection)

# Model performance

# Plotting model


# INTERPRETATION



