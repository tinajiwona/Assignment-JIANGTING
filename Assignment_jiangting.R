################################assgiment1-use 4 different libraries##################3

###read my dataset(csv) from my computer
data <- read.csv('E:/仁荷大学/F-S/r语言/DATA/food-price-index-September-2021-index-numbers-csv-tables.csv')
head(data)
summary(data)
data[which(data$Data_value>3.0),]#selectdatavalue>3.0
data[order(data$Data_value),]#order by datavalue

####using arrange()-- ordering database by certain subsetting
library(plyr)
arrange(data,data$Data_value)
arrange(data,desc(data$Data_value))#descending order
tail(data)#find the end of the data,and  total number of the dataset
###add data
data$price <- rnorm(51926)#add a subset(price) in the database
tail(data)#check whether the subset(price) is added in database
data1 <- rbind(data,rnorm(1))#add a row in the database
data1
tail(data1)#check if it is successful
str(data1)#check the structure of each of the subset

quantile(data1$Data_value,na.rm = TRUE)#quantiles
table1 <- table(data1$price,data1$Data_value)#make table 

##check for missing value
sum(is.na(data1))#in database, NA value have 52008
any(is.na(data1))
sum(is.na(data1$Data_value))#in subset(data_value),it have 26045 NA value
colSums(is.na(data1))# find out the location of the NA value in database
## %in%----find out the numebers of the value in a certain subset
table(data1$Data_value %in% c(3.27))#in data_value, there are 76 values equal to 3.27
table(data1$Data_value %in% c(3.27,4.13))
table(data1$Period %in% c('2006.10'))
table(data1$Period %in% c('2006.10','2006.08'))
###subsetting  varibles
data$fine = data$Data_value %in% c('3.27','4.13')
table(data$Data_value %in% c('3.27','4.13'))
head(data)
table(data$fine)
###creating binay varibles
data1$binary=ifelse(data1$price<0,TRUE,FALSE)
head(data1)
table(data1$binary)
table(data1$binary,data1$price<0)

###creating categorical varibles
data1$cata= cut(data1$price,breaks = quantile(data1$price))
table(data1$cata)###cut the value of price in quantile varibles
head(data1)
###cut one of the subset by setting certain value range
library(Hmisc)
data1$cata2= cut2(data1$price,g=3)
table(data1$cata2)
data1$cata3=cut2(data1$Data_value,g=4)###cuting the data_value in four range and find out the number in each of the range
table(data1$cata3)

####creating factor varibles
data$factor <- factor(data$Data_value)##alter the character of the data_value as factor
?factor()
head(data)
data$factor[1:10]
class(data$factor)

####cutting factor varibles
library(Hmisc)
data$factor2=cut2(data$Data_value,g=4)
table(data$factor2)

###reshaping
library(reshape2)
head(data)
data$value <- rownames(data)##add a colume 
data$value
head(data)
datamelt <- melt(data,id=c('value','Data_value','Group'),measure.vars = c('Period'))
head(datamelt)
tail(datamelt)

####convert dataframe to JSON
library(jsonlite)
jsondata <- toJSON(data1,pretty = T)
jsondata
##read data from JSON
newd <- fromJSON(jsondata)
head(newd)

##########3#assignment2--sapply function. ########################
#Use the factor function for column "Series_title_1"
data$factor3 <- factor(data$Series_title_1)### convert the charactor of Series_title_1  to factor and add in database
data$factor3[1:5]##check the factor
data$number <- rownames(data)
head(data)
#creat a new database
newdata <- melt(data,id=c('number','factor3','Data_value'),measure.vars = c('factor3'))
head(newdata)
#using split and sapply function to get the average for each product 
sapply(split(newdata$Data_value,newdata$factor3),mean)

