############################### DATA PREPARATION ###############################

raw_sales = read.csv("SuperstoreSalesTraining.csv", na.strings = "")
colnames(raw_sales)

# Not NA Columns
sales = raw_sales[,colSums(is.na(raw_sales)) == 0]
colnames(sales)

# NA Columns
# sales = raw_sales[,colSums(is.na(raw_sales)) > 0]
# colnames(sales)


############################### GROUPING CLIENTS ###############################



############################ TIME SERIES PREDICTION ############################



############################ ADVANCED VISUALIZATION ############################



######################## LINEAR MODEL PROFIT PREDICTION ########################




