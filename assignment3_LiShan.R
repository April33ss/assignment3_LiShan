###################read.csv()#######################
mydata <- read.csv("D:/1inha/1course/2021-2/Tues.R/food-price-index-September-2021-index-numbers-csv-tables.csv")
mydata

##################pre-processing##############################
library(data.table)
DT=data.table(mydata)#creat a data table
DT

tables()#See all the data tables in memory
summary(DT)#make summary for the whole data
str(DT)#depth information

setkey(DT,Series_reference)
DT['CPIM.SAP0100']#Simply browse the data for CPIM.SAP0100 to get an overview
DT
DT[,a:=Data_value^2]#add a new columns
DT

is.na(DT) #Judge the missing value
sum(is.na(DT))# Counts the number of missing values
DT1<-na.omit(DT)#deal with missing values
any(is.na(DT1))#check for missing values


library(dplyr)
data1 <- filter(DT1,STATUS=="FINAL")#Filter the subjects in the "FINAL" status
data1 

data2<-select(data1,Series_reference,Period,Data_value,Group,Series_title_1,a)#keep columns that the processing need
data2

sort(data2$Series_title_1)#sort by 'Series_title_1'
data2[order(data2$Period),]#order by time
table(data2$Series_title_1 %in%  c("Oranges, 1kg"))#values with specific characterisitics


library(reshape2)
head(data2)# Browse the first six rows
data2melt <- melt(data2, id=c("Series_reference","Group","Series_title_1"), mearsure.vars=c("Data_value"))#melt the data frame
head(data2melt)# Browse the first six rows
tail(data2melt)# Browse the last six rows

data3 <- dcast(data2melt,Series_title_1 ~ variable, mean)#cast the data frame
data3

library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
data4 <-data2
data4$a = cut2(data4$a, g=2)#Cut a Numeric Variable into Intervals
table(data4$a)

###################creat a factor variable##################################
data2$fac <- factor(data2$Series_title_1)#Use the factor function for column "Series_title_1" as a new column 'fac'
data2$fac[1:22222]
class(data2$fac)
levels(data2$fac)
#####################get the average########################################
spIns = split(data2$Data_value,data2$Series_reference)#get the price values for each product
spIns

sapply(spIns,mean)# get the average for each product using the price values

