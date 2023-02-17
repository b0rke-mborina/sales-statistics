############################### DATA PREPARATION ###############################
# install.packages(c("ggplot2", "dplyr", "tidyr", "RColorBrewer"))
library("ggplot2")
library("dplyr")
library("tidyr")
library("RColorBrewer")


# Load data from CSV file and check its structure
rawSalesData <- read.csv("SuperstoreSalesTraining.csv", na.strings = "", stringsAsFactors = TRUE)
str(rawSalesData)
summary(rawSalesData)


# Remove unnecessary variables (rawSalesData$Row)
salesData <- subset(rawSalesData, select=-c(Row))
str(salesData)
summary(salesData)


# Filter NA values
colnames(salesData) # All columns
colnames(salesData[, colSums(is.na(salesData)) == 0]) # Non NA columns
colnames(salesData[, colSums(is.na(salesData)) > 0])  # NA columns ("Postal.Code", "SubRegion")
sum(is.na(salesData$Postal.Code)) # 6985 NA values
sum(is.na(salesData$SubRegion))   # 7316 NA values

lapply(salesData, function(l) sum(is.na(l))) %>%
  data.frame() %>%
  pivot_longer(names_to = "columns", cols = names(.), values_to = "value") %>%
  ggplot(aes(x = columns, y = value)) +
  geom_bar(stat = "identity", fill = "#6FD3FC") +
  coord_flip() +
  labs(x = "Variable", y = "Number of missing values",
       title = "Number of missing values for dataframe variables")

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
str(salesData)

ggplot(data.frame(table(sapply(salesData, class)))) +
  geom_bar(aes(x = reorder(Var1, -Freq), y = Freq,
               fill = reorder(Var1, -Freq)),
           stat = "identity") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "Variable type", y = "Number of variables",
       title = "Number of variables of each type in dataframe",
       fill = "Variable types") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Blues", direction = -1)


# Outliers and impossible data
str(salesData)

min(salesData$Discount)             # 0 >= 0      OK
max(salesData$Discount)             # 0.95 <= 1   OK

min(salesData$Unit.Price)           # 1 >= 0      OK

min(salesData$Order.Quantity)       # 1 >= 1      OK

min(salesData$Sales)                # 0.9 >= 0    OK

min(salesData$Shipping.Cost)        # 0 >= 0      OK

min(salesData$Product.Base.Margin)  # 0.036 >= 0  OK
max(salesData$Product.Base.Margin)  # 0.85 <= 1   OK

ggplot(salesData) +
  geom_histogram(aes(Order.Date), binwidth = 40, fill = "#6FD3FC") +
  xlab("Order Date value") + ylab("Frequency (count)") +
  ggtitle("Order Date histogram") +
  theme(plot.title = element_text(hjust = 0.5))
# Order Date values are OK.

ggplot(salesData) +
  geom_histogram(aes(Ship.Date), binwidth = 40, fill = "#6FD3FC") +
  xlab("Ship Date value") + ylab("Frequency (count)") +
  ggtitle("Ship Date histogram") +
  theme(plot.title = element_text(hjust = 0.5))
# Ship Date values are OK.

# All values are in allowed ranges.

# Remove outliers helper functions

# Detect outlier function
hasOutlier <- function(x) {
   quantile1 <- quantile(x, probs = 1/4)
   quantile3 <- quantile(x, probs = 3/4)
   IQR = quantile3 - quantile1  # Inter quartile range
   return(x > quantile3 + (IQR * 1.5) | x < quantile1 - (IQR * 1.5))
}

removeOutlier <- function(dataframe, columns = colnames(dataframe)) {
   for (col in columns) {
       # Keep observation if it doesnt have an outlier
       dataframe <- dataframe[!hasOutlier(dataframe[[col]]), ] 
   }
   return(dataframe)
}

# Remove outliers
# removeOutlier(salesData, columns = c("Discount", "Unit.Price", "Order.Quantity",
#                                      "Sales", "Profit", "Shipping.Cost",
#                                      "Product.Base.Margin"))
# Outliers are not removed due to all values being real.


# saving data to CSV file
# write.csv(salesData, file = "data.csv", row.names = TRUE)



############################ PRODUCT CLASSIFICATION ############################
# install.packages(c("rpart", "rpart.plot"))
library("rpart")
library("rpart.plot")


# data selection and preparation
head(salesData)
str(salesData)
classificationData <- salesData[, c("Order.Priority", "Discount", "Unit.Price",
                                    "Shipping.Cost", "Department", "Category",
                                    "Customer.Segment", "Region", "Ship.Mode",
                                    "Profit")]
str(classificationData)

# Selected data includes variables which can help select products to be
# marketed. For example, it can be decided to market and advertise in specific
# regions, market and advertise specific categories of products...


# decide limit for profit (low and high)
mean(classificationData$Profit)   # 882.1462
median(classificationData$Profit) # 133.645

limit <- 2000 # low - high profit limit

ggplot(classificationData) +
  geom_point(aes(x = seq_along(Profit), y = Profit)) +
  geom_hline(aes(yintercept = limit, linetype = "High-low limit"), col = "#FD5602") +
  geom_hline(aes(yintercept = mean(Profit), linetype = "Mean"), col = "#FFAF42") +
  geom_hline(aes(yintercept = median(Profit), linetype = "Median"), col = "#FEDEBE") +
  labs(x = "Dataframe record index", y = "Profit",
       title = "Profit scatterplot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_linetype_manual(name = "Lines", values = c(1, 1, 1),
                        guide = guide_legend(override.aes = list(color = c("#FD5602", "#FFAF42", "#FEDEBE"))))


# create new factor variable which says if profit is high or low
classificationData$ProfitFactor <- factor(ifelse(classificationData$Profit < limit, "Low", "High"))
str(classificationData)
head(classificationData[, c("Profit", "ProfitFactor")], 40)

classificationData <- classificationData[, -grep("^Profit$", colnames(classificationData))]
str(classificationData)
levels(classificationData$ProfitFactor) # ProfitFactor levels: 1 = High, 2 = Low


# divide data to train and test datasets (ratio 70:30)
RNGkind(sample.kind = "Rounding")
set.seed(2)
indices <- sample(nrow(classificationData), 0.7 * nrow(classificationData))
train <- classificationData[indices, ]
test <- classificationData[-indices, ]

str(train)
str(test)
nrow(train) / (nrow(train) + nrow(test))  # 0.6999643
nrow(test) / (nrow(train) + nrow(test))   # 0.3000357

ggplot(data.frame(datasets = c("Train", "Test"),
                  percentages = c(nrow(train) / (nrow(train) + nrow(test)),
                                  nrow(test) / (nrow(train) + nrow(test)))),
       aes(x = "", y = percentages, fill = datasets)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Oranges") +
  geom_text(aes(y = c(0.35), label = "69.99643%"), size = 6) +
  geom_text(aes(y = c(0.83), label = "30.00357%"), size = 6) +
  labs(title = "Division of dataset", fill = "Datasets") +
  theme(plot.title = element_text(hjust = 0.5))

# Dataset is divided to 2 parts in ratio approximate to 70:30.


# create classification tree based on training data
RNGkind(sample.kind = "Rounding")
set.seed(2)
tree <- rpart(ProfitFactor~., data = train, method = "class")
print(tree)
prp(tree, extra = 4, nn = TRUE, yesno = 2, varlen = 0,
    box.col = ifelse(tree$frame$yval == 1, "#FF8303", "#FEDEBE"))
# Tree shows classification of data. Using the tree, profit levels (high / low)
# can be predicted based on variable values of the records. Furthermore,
# expectations of profit from different products can be read from the tree.
# Consequently, decisions can be made for which products will be marketed /
# advertised.


# variable importance
print(tree$variable.importance)

ggplot(data = data.frame(tree$variable.importance,
                         variable = names(tree$variable.importance))) +
  geom_bar(aes(x = reorder(variable, -tree.variable.importance),
               y = tree.variable.importance,
               fill = reorder(variable, tree.variable.importance)),
           stat = "identity") +
  scale_fill_brewer(palette = "Oranges") +
  theme(legend.position = "none") +
  labs(x = "Variable", y = "Importance",
       title = "Variable importance barplot") +
  theme(plot.title = element_text(hjust = 0.5))

# Prices of products are the most important, which is expected. Product
# categories and regions are important variables which can be used. Also,
# interesting result is low importance of discount.


# test classification tree on testing / validation data
prediction <- predict(tree, newdata = test, type = "class")
head(prediction)


# calculate accuracy of the tree model
accuracy <- sum(prediction == test$ProfitFactor) / nrow(test) * 100
print(accuracy) # 92.5
# Accuracy of the model is reasonably high. That means model is rather reliable
# and can be used for marketing purposes.


# pruning tree and performance advantages
print(tree$cptable)
#           CP nsplit rel error    xerror       xstd
# 1 0.12734082      0 1.0000000 1.0000000 0.02576849
# 2 0.01498127      2 0.7453184 0.7453184 0.02260634
# 3 0.01000000      4 0.7153558 0.7393258 0.02252364
min(tree$cptable[, "xerror"]) # 0.7393258
# xstd = 0.02252364 (from cptable)
0.7393258 - 0.02252364  # 0.7168022
0.7393258 + 0.02252364  # 0.7618494
# interval between 0.7168022 and 0.7618494
# Only xerror values from the cptable in the interval are the ones with 2 and 4
# splits (value of nsplit variable).
min(2, 4)  # nsplit = 2
# CP = 0.01498127 (from table)

prunedTree <- prune(tree, cp = 0.01498127)
print(prunedTree)
prp(prunedTree, extra = 4, nn = TRUE, yesno = 2, varlen = 0,
    box.col = ifelse(prunedTree$frame$yval == 1, "#FF8303", "#FEDEBE"))
# The tree is the same. There cannot be any performance advantages.



############################# CLUSTERING CUSTOMERS #############################
# install.packages(c("NbClust", "factoextra"))
library("NbClust")
library("factoextra")


# data selection and preparation
head(salesData)
str(salesData)

# select data needed for clustering
clusteringData <- salesData[, c("Discount", "Unit.Price", "Order.Quantity",
                                "Department", "Customer_ID")]
str(clusteringData)

# Discount, Unit.Price and Order.Quantity variables will be used to calculate
# total money spent by each customer. Customers will be represented by its IDs.
# Total money spent by each customer will be calculated for each product
# department (Technology, Office.Supplies, Furniture).


# aggragate and change data to desired shape
clusteringData$TotalSpent <- clusteringData$Order.Quantity * clusteringData$Unit.Price * (1 - clusteringData$Discount)
clusteringData$Order.Quantity <- NULL
clusteringData$Unit.Price     <- NULL
clusteringData$Discount       <- NULL
clusteringData <- pivot_wider(clusteringData, names_from = Department, values_from = TotalSpent, values_fn = sum, values_fill = 0)
clusteringData <- data.frame(clusteringData)
head(clusteringData)
nrow(clusteringData)  # 3403
str(clusteringData)

# boxplot(clusteringData[, -c(2, 3)])
ggplot(clusteringData) +
  geom_boxplot(aes(y = Furniture, x = "Furniture",
                   fill = "Furniture")) +
  geom_boxplot(aes(y = Office.Supplies, x = "Office.Supplies",
                   fill = "Office.Supplies")) +
  geom_boxplot(aes(y = Technology, x = "Technology",
                   fill = "Technology")) +
  labs(x = "", y = "Value",
       title = "Boxplot of data for clustering") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none") +
  scale_fill_brewer(palette = "Greens")

# Total spending values are grouped by department instead of by category to
# avoid many variables and very large amount of zeroes.


# standardize data
colnames(clusteringData)[1] # "Customer_ID"
clusteringDataScaled <- scale(clusteringData[, -1])
head(clusteringDataScaled)
str(clusteringDataScaled)

# Customer_ID variable is left out of standardized data because it is not
# important for finding number of clusters and grouping data.


# Partitional clustering is selected to enable iterative relocation. Data will
# be clustered using Kmeans method because it is good for large amounts of data.
# Euclidean distance will be used due to regular distance between two values
# being important.

# find optimal number of clusters
# (partitional clustering, euclidean distance, kmeans method)
numberOfClusters <- NbClust(clusteringDataScaled,
                            distance = "euclidean", method = "kmeans",
                            min.nc = 2, max.nc = 15)
print(numberOfClusters)
# The optimal number of clusters is 2.

# clustering votes
table(numberOfClusters$Best.nc[1,]) # 2
# Clustering votes also show, with significant difference, that the optimal
# number of clusters is 2.

# group data
RNGkind(sample.kind = "Rounding")
set.seed(2)
groups <- kmeans(clusteringDataScaled, 2, nstart = 25)
print(groups)


# groups of customers comparison
groups$size             # 303, 3100
groups$withinss         # 4728.549, 1421.516
length(groups$cluster)  # 3403

# First group is much smaller than the second group. Sum of squares of elements
# in first group seems to be much greater that the one in the second group.


# create datasets of data from groups
clusteredData <- data.frame(clusteringData, groups$cluster)
str(clusteredData)

cluster1Data <- clusteredData[clusteredData$groups.cluster == 1,]
str(cluster1Data)

cluster2Data <- clusteredData[clusteredData$groups.cluster == 2,]
str(cluster2Data)


# plot clustered data
colnames(clusteredData)

ggplot() +
  geom_point(data = cluster2Data,
             aes(x = Customer_ID, y = Technology, colour = "2")) + 
  geom_point(data = cluster1Data,
            aes(x = Customer_ID, y = Technology, colour = "1")) +
  xlim(c(1, 3403)) +
  scale_colour_manual(values = rev(brewer.pal(name = "Greens", n = 8)[c(6, 8)])) +
  labs(x = "Customer ID", y = "Total spent on technology",
      title = "Total spent on technology by groups of customers",
      colour = "Groups") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(data = cluster2Data,
             aes(x = Customer_ID, y = Office.Supplies, colour = "2")) + 
  geom_point(data = cluster1Data,
             aes(x = Customer_ID, y = Office.Supplies, colour = "1")) +
  xlim(c(1, 3403)) +
  scale_colour_manual(values = rev(brewer.pal(name = "Greens", n = 8)[c(6, 8)])) +
  labs(x = "Customer ID", y = "Total spent on office supplies",
       title = "Total spent on office supplies by groups of customers",
       colour = "Groups") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(data = cluster2Data,
             aes(x = Customer_ID, y = Furniture, colour = "2")) + 
  geom_point(data = cluster1Data,
             aes(x = Customer_ID, y = Furniture, colour = "1")) +
  xlim(c(1, 3403)) +
  scale_colour_manual(values = rev(brewer.pal(name = "Greens", n = 8)[c(6, 8)])) +
  labs(x = "Customer ID", y = "Total spent on furniture",
       title = "Total spent on furniture by groups of customers",
       colour = "Groups") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(data = data.frame(ID = cluster2Data$Customer_ID,
                               TotalSpent = cluster2Data$Office.Supplies + cluster2Data$Furniture + cluster2Data$Technology),
             aes(x = ID, y = TotalSpent, colour = "2")) +
  geom_point(data = data.frame(ID = cluster1Data$Customer_ID,
                               TotalSpent = cluster1Data$Office.Supplies + cluster1Data$Furniture + cluster1Data$Technology),
             aes(x = ID, y = TotalSpent, colour = "1")) +
  geom_hline(aes(yintercept = 30000), col = "#FD5602") +
  geom_text(aes(x = c(3700), y = c(30000), label = "30000", vjust = -0.5), size = 4, col = "red") +
  xlim(c(1, 3700)) +
  scale_colour_manual(values = rev(brewer.pal(name = "Greens", n = 8)[c(6, 8)])) +
  labs(x = "Customer ID", y = "Total spent",
       title = "Total spent by groups of customers",
       colour = "Groups") +
  theme(plot.title = element_text(hjust = 0.5))

# As it can be seen from the plots, two groups of customers are are very
# different.
# First group consists of customers who spend large amounts of money
# buying products. They can be described as loyal customers and marketing
# products to them is not a priority. Customers from the first group usually
# spend more than 30000 on products.
# Second group consists of customers who spend smaller amounts of money on
# products. They are customers to whom we are not main suppliers and marketing
# products to them is a priority. Customers from the first group usually spend
# less than 30000 on products.



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



######################## LINEAR MODEL PROFIT PREDICTION ########################
# install.packages(c('corrplot', 'PerformanceAnalytics', 'vcd', 'MASS', 'leaps', 'caret', 'bootstrap'))
library('corrplot')
library('PerformanceAnalytics')
library('vcd')
library('MASS')
library('leaps')
library('caret')

# Data selection and preparation
numericSalesData = subset(salesData[, sapply(salesData, is.numeric)], select = -Customer_ID)
factorSalesData = salesData[, sapply(salesData, is.factor)]



# Simple one variable linear model for Profit prediction
chart.Correlation(cor(numericSalesData))  # Profit ~ Sales -> 0.96***

# Train 80%, Test 20%
split_percentage = 0.8
split <- sample(nrow(numericSalesData), split_percentage * nrow(numericSalesData))
train <- numericSalesData[split, ]
test <- numericSalesData[-split, ]

fit <- lm(Profit ~ Sales, data=train)
summary(fit)  # Profit = 71.72 + 0.45 * Sales

plot(train$Sales, train$Profit)
abline(fit, col="Blue")



# Multiple variable regression, variable selection
leaps <- regsubsets(Sales ~ ., data=numericSalesData, nbest=1)
plot(leaps, scale="adjr2")

summary(leaps)
#          Discount Unit.Price Order.Quantity Profit Shipping.Cost Product.Base.Margin
# 1  ( 1 ) " "      " "        " "            "*"    " "           " "
# 2  ( 1 ) " "      "*"        " "            "*"    " "           " "
# 3  ( 1 ) " "      "*"        " "            "*"    " "           "*"
# 4  ( 1 ) " "      "*"        "*"            "*"    " "           "*"
# 5  ( 1 ) "*"      "*"        "*"            "*"    " "           "*"
# 6  ( 1 ) "*"      "*"        "*"            "*"    "*"           "*"

# Single variable cor = Sales ~ Profit
# Two variable cor = Sales ~ Profit + Unit.Price
# ...
# All variables included = Sales ~ .


# K-fold cross-validated R-square
shrinkage <- function(fit, k=10){
    require(bootstrap)
    
    # Fit and predict functions
    theta.fit <- function(x, y){lsfit(x, y)}
    theta.predict <- function(fit, x){cbind(1, x) %*% fit$coef} 
    
    x <- fit$model[, 2:ncol(fit$model)]
    y <- fit$model[, 1]
    
    results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
    r2 <- cor(y, fit$fitted.values)**2  # Normal R2 
    r2cv <- cor(y, results$cv.fit)**2   # Cross-validated R2
    
    cat("R-square =", r2, "\n")
    cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
    cat("Change =", r2 - r2cv, "\n")
}


# R-square = 0.7971082; acceptable R-square (~0.8)
# 10 Fold Cross-Validated R-square = 0.7945547 
# Change = 0.002553464; small change 
shrinkage(lm(Profit ~ Sales, data=numericSalesData))

# R-square = 0.8151863 
# 10 Fold Cross-Validated R-square = 0.8113877 
# Change = 0.003798527 
shrinkage(lm(Profit ~ Sales + Unit.Price, data=numericSalesData))

# R-square = 0.8396185 
# 10 Fold Cross-Validated R-square = 0.8357494 
# Change = 0.003869127 
shrinkage(lm(Profit ~ Sales + Unit.Price + Product.Base.Margin, data=numericSalesData))

# R-square = 0.8401814 
# 10 Fold Cross-Validated R-square = 0.836934 
# Change = 0.003247305 
shrinkage(lm(Profit ~ Sales + Unit.Price + Product.Base.Margin + Order.Quantity, data=numericSalesData))

# R-square = 0.8421473 
# 10 Fold Cross-Validated R-square = 0.838756 
# Change = 0.003391272 
shrinkage(lm(Profit ~ Sales + Unit.Price + Product.Base.Margin + Order.Quantity + Discount, data=numericSalesData))

# R-square = 0.8447364; good R-square (~0.84)
# 10 Fold Cross-Validated R-square = 0.8416904 
# Change = 0.003045998; small change
shrinkage(lm(Profit ~ ., data=numericSalesData))


# Factor correlation
summary(factorSalesData)
mosaicplot(Container ~ Ship.Mode, data=factorSalesData, shade=TRUE, legend=TRUE)
mosaicplot(Department ~ Ship.Mode, data=factorSalesData, shade=TRUE, legend=TRUE)
table(factorSalesData$Department, factorSalesData$Ship.Mode)
table(factorSalesData$Container, factorSalesData$Ship.Mode)

# X-squared = 4912, df = 4, p-value < 2.2e-16
chisq.test(table(factorSalesData$Department, factorSalesData$Ship.Mode))

# X-squared = 16636, df = 12, p-value < 2.2e-16
chisq.test(table(factorSalesData$Container, factorSalesData$Ship.Mode))
