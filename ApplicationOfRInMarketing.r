############################### DATA PREPARATION ###############################

# load data from CSV file and check its structure
rawSalesData <- read.csv("SuperstoreSalesTraining.csv",
                         na.strings = "",
                         stringsAsFactors = TRUE)
str(rawSalesData)
summary(rawSalesData)

# remove unnecessary variables
salesData <- subset(rawSalesData, select=-c(Row))
str(salesData)
summary(salesData)

# remove columns with any missing data
colnames(rawSalesData)
colnames(rawSalesData[, colSums(is.na(rawSalesData)) == 0])
colnames(rawSalesData[, colSums(is.na(rawSalesData)) > 0]) # "Postal.Code" "SubRegion"
sum(is.na(rawSalesData$Postal.Code))  # 6985
sum(is.na(rawSalesData$SubRegion))    # 7316

salesData <- salesData[, colSums(is.na(salesData)) == 0]
str(salesData)
summary(salesData)

# make sure variable types are OK

# check numeric variables
nums <- unlist(lapply(salesData, is.numeric), use.names = FALSE)
colnames(salesData[ , nums]) # "Order" "Unit.Price" "Order.Quantity" "Sales" "Profit" "Shipping.Cost" "Customer_ID"
# convert order number to character
salesData$Order <- as.character(salesData$Order)
class(salesData$Order)

# check factor variables
factors <- unlist(lapply(salesData, is.factor), use.names = FALSE)
colnames(salesData[ , factors]) # "Order.Priority" "Order.Date" "Discount" "Product.Base.Margin" "Department" "Container"
                                # "Category" "Item" "Customer.Segment" "Customer.Name" "Region" "State"
                                # "Country...Region" "City" "Ship.Date" "Ship.Mode"

# convert date strings to dates
salesData$Order.Date <- as.Date(salesData$Order.Date, format = "%d/%m/%Y")
class(salesData$Order.Date)
head(salesData$Order.Date)

# convert discount to numeric
salesData$Discount <- as.numeric(sub("%", "", salesData$Discount))/100
class(salesData$Discount)
head(salesData$Discount)

# convert product base margin to numeric
salesData$Product.Base.Margin <- as.numeric(sub("%", "", salesData$Product.Base.Margin))/100
class(salesData$Product.Base.Margin)
head(salesData$Product.Base.Margin)

# convert item to character
salesData$Item <- as.character(salesData$Item)
class(salesData$Item)

# convert customer name to character
salesData$Customer.Name <- as.character(salesData$Customer.Name)
class(salesData$Customer.Name)

# convert ship date strings to dates
salesData$Ship.Date <- as.Date(salesData$Ship.Date, format = "%d/%m/%Y")
class(salesData$Ship.Date)
head(salesData$Ship.Date)

str(salesData)
summary(salesData)



# remove outliers

# detect outlier function
detectOutlier <- function(x) {
  quantile1 <- quantile(x, probs = .25)                     # calculate first quantile
  quantile3 <- quantile(x, probs = .75)                     # calculate third quantile
  IQR = quantile3 - quantile1                               # calculate inter quartile range
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5) # return true or false
}

# create remove outlier function
removeOutlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {                                        # for loop to traverse in columns vector
    dataframe <- dataframe[!detectOutlier(dataframe[[col]]), ]  # remove observation if it satisfies outlier function
  }
  # print("Remove outliers")
  # print(dataframe)
  return(dataframe)                                             # return dataframe
}


# standardize data



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


# data selection and preparation

# correlation between variables + corrplot

# linear model (automatic variable selection)

# model performance

# plotting model


# INTERPRETATION



