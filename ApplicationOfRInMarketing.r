############################### DATA PREPARATION ###############################
# install.packages(c("ggplot2", "dplyr", "tidyr"))
library("ggplot2")
library("dplyr")
library("tidyr")


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
  geom_bar(stat = "identity", fill = "deepskyblue") +
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
  geom_bar(aes(x = reorder(Var1, -Freq), y = Freq),
           fill = "deepskyblue", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "Variable type", y = "Number of variables",
       title = "Number of variables of each type in dataframe") +
  theme(plot.title = element_text(hjust = 0.5))


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
  geom_histogram(aes(Order.Date), binwidth = 40, fill = "deepskyblue") +
  xlab("Order Date value") + ylab("Frequency (count)") +
  ggtitle("Order Date histogram") +
  theme(plot.title = element_text(hjust = 0.5))
# Order Date values are OK.

ggplot(salesData) +
  geom_histogram(aes(Ship.Date), binwidth = 40, fill = "deepskyblue") +
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
  # theme(legend.position = "none") +
  geom_text(aes(y = c(0.35), label = "69.99643%"), size = 6) +
  geom_text(aes(y = c(0.83), label = "30.00357%"), size = 6)

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
       title = "Variable importance barplot")

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

# data selection and preparation

# install.packages(c("NbClust", "factoextra", "tidyr"))
library("NbClust")
library("factoextra")
library("tidyr")

head(salesData)
str(salesData)

# select data needed for clustering
groupingData <- salesData[, c("Discount", "Unit.Price", "Order.Quantity",
                              "Category", "Customer_ID",
                              "Region")]
str(groupingData)

# aggragate and change data to desired shape
groupingData$MoneySpent <- groupingData$Order.Quantity * groupingData$Unit.Price * (1 - groupingData$Discount)
groupingData$Order.Quantity <- NULL
groupingData$Unit.Price     <- NULL
groupingData$Discount       <- NULL
groupingData <- pivot_wider(groupingData, names_from = Category, values_from = MoneySpent, values_fn = sum, values_fill = 0)
groupingData <- data.frame(groupingData)
head(groupingData)

# boxplot(groupingData[, -c(2, 3)])
str(groupingData)
nrow(groupingData)  # 3403
colnames(groupingData)[1:2] # "Customer_ID" "Region"

# standardize data
groupingDataScaled <- scale(groupingData[, -c(1, 2)])
head(groupingDataScaled)
str(groupingDataScaled)

# find optimal number of clusters (partitional, euclidean, kmeans)
numberOfClusters <- NbClust(groupingDataScaled,
                            distance = "euclidean", method = "kmeans",
                            min.nc = 2, max.nc = 15)
print(numberOfClusters)

# grouping votes
table(numberOfClusters$Best.nc[1,]) # 2

# group data
RNGkind(sample.kind = "Rounding")
set.seed(2)
groups <- kmeans(groupingDataScaled, 2, nstart = 25)
print(groups)

# groups comparison
groups$size # 371, 3032
groups$withinss # 40777.615, 9356.963
length(groups$cluster) # 3403

# create groups of data
clusteredData <- data.frame(groupingData, groups$cluster)
str(clusteredData)

cluster1Data <- clusteredData[clusteredData$groups.cluster == 1,]
str(cluster1Data)

cluster2Data <- clusteredData[clusteredData$groups.cluster == 2,]
str(cluster2Data)


# plot clustered data
colnames(clusteredData)

plot(cluster1Data$Storage...Organization)
plot(cluster2Data$Storage...Organization)

plot(cluster1Data$Binders.and.Binder.Accessories)
plot(cluster2Data$Binders.and.Binder.Accessories)

plot(cluster1Data$Chairs...Chairmats)
plot(cluster2Data$Chairs...Chairmats)

plot(cluster1Data$Paper)
plot(cluster2Data$Paper)

plot(cluster1Data$Pens...Art.Supplies)
plot(cluster2Data$Pens...Art.Supplies)

plot(cluster1Data$Office.Machines)
plot(cluster2Data$Office.Machines)

plot(cluster1Data$Office.Furnishings)
plot(cluster2Data$Office.Furnishings)

plot(cluster1Data$Tables)
plot(cluster2Data$Tables)

plot(cluster1Data$Computer.Peripherals)
plot(cluster2Data$Computer.Peripherals)

plot(cluster1Data$Telephones.and.Communication)
plot(cluster2Data$Telephones.and.Communication)

ggplot(clusteredData, aes(x = Customer_ID, y = Computer.Peripherals, color = Region)) +
  geom_point()

ggplot(cluster1Data, aes(x = Customer_ID, y = Computer.Peripherals, color = Region)) +
  geom_point()

ggplot(cluster2Data, aes(x = Customer_ID, y = Computer.Peripherals, color = Region)) +
  geom_point()


ggplot(clusteredData, aes(x = Paper, y = Pens...Art.Supplies, color = Region)) +
  geom_point()

ggplot(cluster1Data, aes(x = Region, y = Paper, color = Region)) +
  geom_point()

# clusteredData[clusteredData$groups.cluster != 0,]
ggplot(clusteredData, aes(x = Office.Furnishings, y = Tables, color = as.factor(groups.cluster))) +
  geom_point() + xlim(0, 10000) + ylim(0, 20000)
cluster2Data
str(clusteredData)

# INTERPRETATION

ggplot(clusteredData, aes(x = Paper, y = Pens...Art.Supplies, color = as.factor(groups.cluster))) +
  geom_point()

ggplot(cluster1Data, aes(x = Paper, y = Pens...Art.Supplies, color = as.factor(groups.cluster))) +
  geom_point()

ggplot(cluster2Data, aes(x = Paper, y = Pens...Art.Supplies, color = as.factor(groups.cluster))) +
  geom_point()


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
