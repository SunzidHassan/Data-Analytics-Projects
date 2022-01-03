#Test data processing####
##load libraries
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)

##sales data
#4 customers, 8 weeks, 2 shops
testOrder <- read.csv("Data/Input Data/testOrder.csv")


#against unique customer-invoice: date diff of each invoice
testOrder <- mutate(testOrder, orderDateDiff = as.integer(Sys.Date() - mdy(testOrder$Date)))


#against unique customer-item: sum of orders of the item. Score = 1/n.
custItem <- testOrder %>% group_by(Customer, Item) %>%
                    mutate(custItem = row_number())


#against unique customer-category: count of category purchased. Score = 1/n.
custCat <- testOrder %>% group_by(Customer, Category) %>%
                    mutate(custCat = row_number())

testOrder <- merge(merge(testOrder, custItem, all.x = T, all.y = F),
                   custCat, all.x = T, all.y = F)


#against unique customer: date difference since last delivery
#if last delivery exists, calculate date diff from it.
delivered <- filter(testOrder, Order.Status=='Delivered')
delivered <- delivered[order(delivered$Customer, mdy(delivered$Last.Update.Date), decreasing = c(F,T)), ]
delivered <- delivered[!duplicated(delivered$Customer), ]
delivered <- mutate(delivered, lastDeliDateDiff = as.integer(Sys.Date() - mdy(delivered$Last.Update.Date))) %>%
                    dplyr::select(c("Customer", "lastDeliDateDiff"))
deliveredCust <- delivered[!duplicated(delivered$Customer), c("Customer")]

#If there are no delivered product, take the earliest ordered item, and calculate date difference from it.
notDelivered <- filter(testOrder, Customer!=deliveredCust)
notDelivered <- notDelivered[order(notDelivered$Customer, notDelivered$Date), ]
notDelivered <- notDelivered[!duplicated(notDelivered$Customer), ]
notDelivered <- mutate(notDelivered, lastDeliDateDiff = as.integer(Sys.Date() - mdy(notDelivered$Date))) %>% select(c("Customer", "lastDeliDateDiff"))

testOrder <- merge(testOrder, merge(delivered, notDelivered, all = T), all.x = T, all.y = F)

#Regret function####
# = Date diff since last delivery (25%) * sum over units
# (Order importance value (25%) + quantity of same item (20%) + date diff (30%))

#score calculation
testOrder <-  merge(testOrder,
                    (filter(testOrder, !Order.Status=='Delivered') %>%
                                         ddply(.(Customer), summarise, sumOrderDateDiff = sum(orderDateDiff))),
                    all.x = T, all.y = F)

minOrderDate <- min(testOrder$orderDateDiff, na.rm = T)
maxOrderDate <- max(testOrder$orderDateDiff, na.rm = T)
minLastDeliDate <- min(testOrder$lastDeliDateDiff, na.rm = T)
maxLastDeliDate <- max(testOrder$lastDeliDateDiff, na.rm = T)
minSumOrderDateDiff <- min(testOrder$sumOrderDateDiff, na.rm = T)
maxSumOrderDateDiff <- max(testOrder$sumOrderDateDiff, na.rm = T)

testScore <- mutate(testOrder, regretVal = ((1/testOrder$custItem)*.1) + (1/testOrder$custCat*.25) +
                                        (((testOrder$orderDateDiff-minOrderDate)/(maxOrderDate-minOrderDate))*.3) +
                                        (((testOrder$lastDeliDateDiff-minLastDeliDate)/(maxLastDeliDate-minLastDeliDate))*.2) +
                                        (((testOrder$sumOrderDateDiff-minSumOrderDateDiff)/(maxSumOrderDateDiff-minSumOrderDateDiff))*.15)) %>%
                    dplyr::select(c("Customer", "Date", "Invoice", "Shop", "Item",
                                    "Category", "Quantity", "Monetary.Value",
                                    "Order.Status", "Last.Update.Date", "regretVal"))
write.csv(testScore,"Data/Input Data/testOrderScore.csv")


#delivery optimization based on supply####
##load sales and supply data
#order data
testOrderScore <- read.csv("Data/Input Data/testOrderScore.csv")

#supply data
testSupply <- read.csv("Data/Input Data/testSupply.csv")

delivery <- matrix(0, ncol = length(testSupply$Shop), nrow = max(testSupply$Quantity, na.rm = T))
selectedCust <- data.frame(Customer = NA)

for (i in 1:length(testSupply$Shop)) {
                    #filter orders based on the shop
                    shopOrder <- filter(testOrderScore, grepl(testSupply[i,c("Shop")], Shop, ignore.case = T))
                    
                    # sort regret value - customer
                    shopOrder <- shopOrder[order(shopOrder$regretVal, shopOrder$Customer),]
                    
                    #omit previously selected customers
                    shopOrder <- shopOrder[shopOrder$Customer %in% !selectedCust$Customer]
                    
                    # take one order of each customer of that shop
                    shopOrder <- shopOrder[!duplicated(shopOrder$Customer), ]
                    
                    #keep record of selected customers, so that they can be omitted from subsequent lists
                    selectedCust <- rbind(selectedCust, shopOrder[!duplicated(shopOrder$Customer), c("Customer")])
                    
                    # select the first n customers
                    for (j in 1:max(testSupply$Quantity)) {
                                        delivery[j, i] <- shopOrder[j, c("Invoice")]
                    }
}