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

sapply(salesData, class)
salesDf <- salesData[, c('Order', 'Order.Date', 'Discount', 'Product.Base.Margin', 'Item', 'Customer.Name', 'Ship.Date')]
str(salesDf)



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

removeOutlier(salesDf)

# Standardize data



####################### CLIENT-TO-PRODUCT CLASSIFICATION #######################

# data selection and preparation

# divide data to train and test dataset

# create classification tree based on training data

# variable importance

# test classification tree on testing / validation data

# pruning tree and performance advantages


# INTERPRETATION



############################### GROUPING CLIENTS ###############################

# data selection and preparation

# find optimal number of clusters (euclidean distance, kmeans method)

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
install.packages(c('corrplot', 'PerformanceAnalytics', 'vcd'))
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



