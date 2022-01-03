#####
## 2. Delivery Optimization
# 1. time of first delivery, average of first 3 delivery, average of last 3 delivery, average of all delivery - vs - total paid, total number of purchases, frequency of purchase (total purchase date difference from first divided by total number of purchases) - all for above 5 lac paid and below 5 lac total paid: 4*3*2 relation determination pair and graphs
# 1.1 load merged system data (invoice, customer ID, shop name, purchase date, order status, total paid, last update time)
# 1.2 make the data unique against invoice number
# 1.3 calculate date difference for all (use Last update date if status is D/S, otherwise current date)
# 
#
# 2. data groups for calculation of relation
# 2.1 separate data for first delivery, for first 3 delivery, last 3 delivery, for all delivery - for all, for above and below 5 lacs (resulting in 4*3=12 sets of data)
# 2.2 calculate average delivery time for all (independent)
# 2.3 calculate average of total customer purchase for the 9 sets (dependent 1)
# 2.4 calculate number of purchases for the 9 sets (dependent 2)
# 2.5 calculate frequency of purchase for the 9 sets (dependent 3)
# 
# 
# 3. calculate relation among the 3*3*2 pairs
# 3.1 take the pairs one by one, use cor() function
# 
# 
# 4. mean and standard dev of delivery time of different shops, overall average delivery time of evaly (it'll act as report, and help with modeling of delivery)
# 4.1 melt data
# 4.2 cast data - group by shops and mean of date difference ()
# 
# 
# 5. profit impact of the entire idea
# 5.1 based on the relation of different slabs (first delivery, first 3, all), impacts (total purchase, total number of purchase, frequency), across groups (all, above and below 5 lac), what are the possibilities of optimization (if every one got in average time, if deliveries are faster and then slower with same average)
#
# 
# 6. given the relation among delivery time and purchase, and shop delivery time info of shops, optimize product delivery for maximising sales
# 6.1 test case: given 5 shops, 5*2=10 products, 5, 5+5, 5+5+5 customers in 3 weeks, 3 weeks, all orders, similar stock input frequency, make delivery of new customers faster and old slower while keeping average delivery time same
# 6.2 results of average delivery time can be considered as performance indicator
# 
# 7. Possibility of auto bill generation from sourcing info

library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(tidyr)

#####
# 1. time of first delivery, average of first 3 delivery, average of last 3 delivery, average of all delivery - vs - total paid, total number of purchases, frequency of purchase (total purchase date difference from first divided by total number of purchases) - all for above 5 lac paid and below 5 lac total paid: 4*3*2 relation determination pair and graphs
# load merged system data (invoice, customer ID, shop name, purchase date, order status, total paid, last update time)
salesData <- read.csv("Data/Output/Merged20_21SalesData210320.csv",
                      stringsAsFactors = F, header = T, comment.char = "",
                      colClasses = c("Customer.Address"= "NULL", "Order.Time" = "NULL", "Shop.Number" = "NULL",
                                     "Payment.Method" = "NULL", "Last.Update" = "NULL", "Last.Update.Time" = "NULL",
                                     "Order.Items" = "NULL", "Order.Quantity" = "NULL", "Order.Price" = "NULL"))

# make the data unique against invoice number
unqInvSalesData <- data.frame(salesData[!duplicated(salesData$Invoice.No), ])
names(unqInvSalesData)

#adding commercial info (kam, bdm, vm, category, shop type, campaign type, mother shop) with unique sales data
commercialInfo <- read.csv("Data/commercialInfo290421.csv", stringsAsFactors = F, header = T, comment.char = "")
names(commercialInfo)

unqInvSalesDataWComInfo <- merge(unqInvSalesData, commercialInfo, by.x = "Shop.Name", by.y = "Shop.Name", all = FALSE)


# calculate date difference for all (use Last update date if status is D/S/Cancel, otherwise current date (before 90 days))
shipDeliSalesData <- dplyr::filter(unqInvSalesDataWComInfo, grepl('shipped|delivered|cancel', Order.Status))
NshipDeliSalesData <- dplyr::filter(unqInvSalesDataWComInfo, !grepl('shipped|delivered|cancel', Order.Status))

shipDeliDateDiff <- mutate(shipDeliSalesData, dateDiff = dmy(LastUpdatedate) - mdy(Order.Date))
NshipDeliDateDiff <- mutate(NshipDeliSalesData, dateDiff = ymd(Sys.Date()) - mdy(Order.Date))
NshipDeliDateDiff <- filter(NshipDeliDateDiff, dateDiff > 90)

allDateDiff <- rbind(shipDeliDateDiff, NshipDeliDateDiff)

#export the unique sales data with commercial info and date difference
write.csv(allDateDiff, "Data/Output/unqSalesDataWdateDiff210320.csv")

#####
# 2. data groups for calculation of relation

#import the unique sales data with commercial info and date ifference
unqSalesDataWdateDiff210320 <- read.csv("Data/Output/unqSalesDataWdateDiff210320.csv", stringsAsFactors = F, header = T, comment.char = "")

names(unqSalesDataWdateDiff210320)

#filter cyclone data
unqCycloneDataWdateDiff210320 <- dplyr::filter(unqSalesDataWdateDiff210320, grepl('Cyclone', Campaign.Type))


# separate data for first delivery, for first 3 delivery, last 3 delivery, for all delivery - for all, for above and below 5 lacs (resulting in 4*3=12 sets of data)

#all delivery
allDeli <- unqCycloneDataWdateDiff210320 %>% group_by(Customer.Id) %>% mutate(customerPurchaseCount=row_number())
allDeli <- cbind(Temp = 1, allDeli)

# first 3 delivery
first3Deli <- filter(allDeli, customerPurchaseCount < 3)

#first delivery
firstDeli <- data.frame(first3Deli[!duplicated(first3Deli$Customer.Id), ])

#last 3 delivery
descUnqCycloneDataWdateDiff210320 <- unqCycloneDataWdateDiff210320[order(-unqCycloneDataWdateDiff210320$X),]

descAllDeli <- descUnqCycloneDataWdateDiff210320 %>% group_by(Customer.Id) %>% mutate(descCustomerPurchaseCount=row_number())

last3Deli <- filter(descAllDeli, descCustomerPurchaseCount < 3)

# calculate average delivery time against each customer for all 4 classes (independent variable)

allDeliMelt <- melt(allDeli, id = c("Invoice.No", "Order.Date", "Customer.Id",
                                    "Order.Status", "Shop.Code", "LastUpdatedate"),
                    measure.vars = c("Total.Price", "dateDiff", "customerPurchaseCount", "Temp"))
allDeliMelt$value <- as.integer(allDeliMelt$value)

first3DeliMelt <- melt(first3Deli, id = c("Invoice.No", "Order.Date", "Customer.Id",
                                          "Order.Status", "Shop.Code", "LastUpdatedate"),
                       measure.vars = c("Total.Price", "dateDiff", "customerPurchaseCount"))

last3DeliMelt <- melt(last3Deli, id = c("Invoice.No", "Order.Date", "Customer.Id",
                                        "Order.Status", "Shop.Code", "LastUpdatedate"),
                      measure.vars = c("Total.Price", "dateDiff", "descCustomerPurchaseCount"))

#avg date diff for all delivery
allDeliCast1 <- dcast(allDeliMelt, Customer.Id ~ variable, mean)
allDeliCast1 <- select(allDeliCast1, -2, -4, -5)
allDeliCast1$dateDiff <- as.integer(allDeliCast1$dateDiff)
allDeliCast1 <- rename(allDeliCast1, allDeliDateDiffAvg =dateDiff)
summary(allDeliCast1)

#total sum and count of customer purchase
allDeliCast2 <- dcast(allDeliMelt, Customer.Id ~ variable, sum)
allDeliCast2 <- select(allDeliCast2, -3, -4)
allDeliCast2 <- rename(allDeliCast2, countOfPurchase = Temp)
allDeliCast2$Total.Price <- as.integer(allDeliCast2$Total.Price)
summary(allDeliCast2)

#average date diff for first 3 delivery
first3DeliCast <- dcast(first3DeliMelt, Customer.Id ~  variable, mean)
first3DeliCast <- select(first3DeliCast, -2, -4)
first3DeliCast$dateDiff <- as.integer(first3DeliCast$dateDiff)
first3DeliCast <- rename(first3DeliCast, first3DeliDateDiffAvg = first3DateDiff)
summary(first3DeliCast)

#date diff for first delivery
names(firstDeli)
firstDeliCast <- select(firstDeli, 6, 21)
firstDeliCast <- rename(firstDeliCast, firstDeliDateDiff = firstDateDiff)
summary(firstDeliCast)

#average date diff for last 3 delivery
last3DeliCast <- dcast(last3DeliMelt, Customer.Id ~  variable, mean)
last3DeliCast <- select(last3DeliCast, -2, -4)
last3DeliCast$dateDiff <- as.integer(last3DeliCast$dateDiff)
last3DeliCast <- rename(last3DeliCast, last3DeliDateDiffAvg = dateDiff)

#Merge the data frames
CustomerDateDiff <- merge(allDeliCast1,
                          merge(allDeliCast2,
                                merge(first3DeliCast,
                                      merge(firstDeliCast, last3DeliCast,
                                            all = FALSE), all = FALSE), all = FALSE), all = FALSE)

#export data
write.csv(CustomerDateDiff, "Data/Output/customerDateDiffWPurchase210320.csv")


#####
# 3. calculate relation among the 3*3*2 pairs

customerDateDiffWPurchase <- read.csv("Data/Output/customerDateDiffWPurchase210320.csv", stringsAsFactors = F, header = T, comment.char = "")
summary(customerDateDiffWPurchase)

#customers with purchase below 1,00,000
filtCustomerDateDiffWPurchase <- filter(customerDateDiffWPurchase, Total.Price < 100000)

#correlation among the groups
cor <- data.frame(round(cor(filtCustomerDateDiffWPurchase, method = c("pearson")), digits = 2))
correlation <- cor[c(3, 6:8), (4:5)]

#correlation among those who have more than 3 orders vs not, against first order delivery time
purchase13testAll <- mutate(customerDateDiffWPurchase, morethan3purchase = factor((countOfPurchase > 3), labels = c("1", "0")))
purchase13test <- select(purchase13testAll, 7, 9)
purchase13test$firstDeliDateDiff <- as.numeric(purchase13test$firstDeliDateDiff)
purchase13test$morethan3purchase <- as.numeric(purchase13test$morethan3purchase)

purchase13testCor <- data.frame(round(cor(purchase13test, method = c("pearson")), digits = 2))
purchase13testCor <- purchase13testCor[2,2]

#correlation among those who have more than 5 orders vs not, against first order delivery time
purchase15testAll <- mutate(customerDateDiffWPurchase, morethan5purchase = factor((countOfPurchase > 5), labels = c("1", "0")))
purchase15test <- select(purchase15testAll, 7, 9)
purchase15test$firstDeliDateDiff <- as.numeric(purchase15test$firstDeliDateDiff)
purchase15test$morethan5purchase <- as.numeric(purchase15test$morethan5purchase)

purchase15testCor <- data.frame(round(cor(purchase15test, method = c("pearson")), digits = 2))

#correlation among those who have more than 5 orders vs not, against first 3 order delivery time
purchase35testAll <- mutate(customerDateDiffWPurchase, morethan5purchase = factor((countOfPurchase > 5), labels = c("1", "0")))
purchase35test <- select(purchase35testAll, 6, 9)
names(purchase35test)
purchase35test$first3DeliDateDiffAvg <- as.numeric(purchase35test$first3DeliDateDiffAvg)
purchase35test$morethan5purchase <- as.numeric(purchase35test$morethan5purchase)

purchase35testCor <- data.frame(round(cor(purchase35test, method = c("pearson")), digits = 2))

#correlation among those who have more than 10 orders vs not, against first 3 order delivery time
purchase310testAll <- mutate(customerDateDiffWPurchase, morethan10purchase = factor((countOfPurchase > 10), labels = c("1", "0")))
purchase310test <- select(purchase310testAll, 6, 9)
purchase310test <- rename(purchase310test, morethan10purchase = morethan5purchase)
names(purchase310test)
purchase310test$first3DeliDateDiffAvg <- as.numeric(purchase310test$first3DeliDateDiffAvg)
purchase310test$morethan10purchase <- as.numeric(purchase310test$morethan10purchase)

purchase50testCor <- data.frame(round(cor(purchase310test, method = c("pearson")), digits = 2))


#####
# 4. mean and standard dev of delivery time of different shops, overall average delivery time of evaly (it'll act as report, and help with modeling of delivery)
# 4.1 melt data


# 4.2 cast data - group by shops and mean of date difference ()


# 
# 
#####
# 5. profit impact of the entire idea
# 5.1 based on the relation of different slabs (first delivery, first 3, all)
#+, impacts (total purchase, total number of purchase, frequency)
#+, across groups (all, above and below 5 lac), what are the possibilities of optimization
#+ (if every one got in average time, if deliveries are faster and then slower with same average)


#####
# 6. given the relation among delivery time and purchase, and shop delivery time info of shops, optimize product delivery for maximising sales
# 6.1 test case: given 5 shops, 5*2=10 products, 5, 5+5, 5+5+5 customers in 3 weeks, 3 weeks, all orders, similar stock input frequency, make delivery of new customers faster and old slower while keeping average delivery time same

