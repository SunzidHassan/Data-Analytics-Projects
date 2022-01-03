#load libraries####
library(readxl)
library(googlesheets4)
library(lubridate)
#library(reshape2)
#library(plyr)
library(dplyr)
library(stringr)
library(data.table)

#commercial info
commercialInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1JrT1sy5QGvSR6GaYiVEn2WrCS6y72ke0sJnYaw-VYAg",
                             range = "Updated Sellers List!A:K", col_names = T, na = "") %>%
                    select(c("Shop Name", "Mother Company", "KAM Name", "BDM Name",
                             "Category Head /Team Lead Name", "Category")) %>%
                    dplyr::rename(ShopName = 'Shop Name', MotherCompany = 'Mother Company',
                                  KAM = 'KAM Name', BDM = 'BDM Name',
                                  CatHead = "Category Head /Team Lead Name")

#Select up to this point#

commercialInfo <- data.table(commercialInfo)
#commercialInfo <- commercialInfo[grepl("T10|Priority", ShopName, ignore.case = T), ]
commercialInfo$ShopName <- tolower(commercialInfo$ShopName)
commercialInfo$ShopName <- str_squish(commercialInfo$ShopName)


##Sales data####

file.list <- list.files(path = "Data/Input Data/SystemReport/SystemReportWithProd", pattern="*.xlsx", full.names = T)

#load the excel files, rename the last column as shopCode. Takes about 5 minutes for data since November.
sales <- lapply(file.list, read_excel) %>%
                    bind_rows(.id = "id")
rm(file.list)

#sales and product data
sales <-  mutate(sales, Month = month(mdy(sales$`Order Date`)))%>%
                    cbind("LastUpdatedate" = gsub(pattern = ",.*", "", sales$`Last Update`)) %>%
                    dplyr::select(c("Invoice No", "Order Date", "Month", "Shop Name", "Order Status", "Payment Status", "Note",
                                    "LastUpdatedate", "Order Items", "Order Quantity", "Order Price", "Seller Price", '...25')) %>%
                    dplyr::rename(Invoice = 'Invoice No', OrderDate = 'Order Date', ShopName = 'Shop Name',
                                  OrderStatus = 'Order Status', OrderItem = 'Order Items',
                                  OrderQuantity = 'Order Quantity', unit.OrderPrice = 'Order Price', LastUpdate = 'Note',
                                  PaymentStatus = 'Payment Status', unit.mrp = 'Seller Price', unit.seller.price = '...25') %>%
                    dplyr::filter(PaymentStatus == "paid")

sales$OrderItem <- gsub(pattern= "\r|\n.*" , "", sales$OrderItem)
sales$OrderItem <- str_squish(sales$OrderItem)
sales$OrderItem <- tolower(sales$OrderItem)
sales$ShopName <- tolower(sales$ShopName)
sales$ShopName <- str_squish(sales$ShopName)

sales$unit.OrderPrice <- as.numeric(sales$unit.OrderPrice)
sales$OrderQuantity <- as.numeric(sales$OrderQuantity)
sales$unit.mrp <- as.numeric(sales$unit.mrp)
sales$unit.seller.price <- as.numeric(sales$unit.seller.price)

sales <- filter(sales, sales$unit.mrp > 0)
sales <- filter(sales, sales$unit.seller.price > 0)
sales <- filter(sales, sales$unit.OrderPrice > 0)
sales <- filter(sales, sales$unit.mrp >= sales$unit.seller.price)

#unique invoice sales data
unqSales <- data.frame(sales[!duplicated(sales$`Invoice`), ]) %>%
                    cbind(count = 1)

#unique shops in sales data
unqShop <- data.frame(unqSales[!duplicated(unqSales$ShopName), ]) %>%
                    select(c("ShopName", "OrderDate")) %>%
                    rename(CampaignDate = OrderDate)

#add campaign date in the sales data
sales <- merge(sales, unqShop, all = T)

sales <- data.table(sales)
unqSales <- data.table(unqSales)

##seller payment####

corporateTrackerUrl <- read.csv("Data/Input Data/PaymentTrackers/CorporateTrackers.csv",
                                stringsAsFactors = F, header = T, comment.char = "")


corporateTracker <- read_sheet(corporateTrackerUrl[1,3],
                               range = "Bill Submission Summary!A4:AR4", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')


for (i in 1:length(corporateTrackerUrl$URL)) {
                    temp <- read_sheet(corporateTrackerUrl[i,3],
                                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                                        select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                                                 "Amount", "Total Due for this bill")) %>%
                                        dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                                      PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                                      Paid = 'Amount', Due = 'Total Due for this bill')
                    temp <- temp[!is.na(temp$Submitted), ]
                    corporateTracker <- rbind(corporateTracker, temp)}

corporateTracker$Serial <- as.character(corporateTracker$Serial)
corporateTracker$ShopName <- as.character(corporateTracker$ShopName)
corporateTracker$Submitted <- as.numeric(gsub(",","",corporateTracker$Submitted))
corporateTracker$Approved <- as.numeric(gsub(",","",corporateTracker$Approved))
corporateTracker$Paid <- as.numeric(gsub(",","",corporateTracker$Paid))
corporateTracker$Due <- as.numeric(gsub(",","",corporateTracker$Due))

#Add shop specific data with corporate tracker data
corporateTracker <- merge(corporateTracker, unqShop, all.x = T, all.y = F)
corporateTracker <- data.table(corporateTracker)

#seller payment

CorporateSellerPayment <- corporateTracker[, .(CorporateSubmitted = sum(Submitted, na.rm = T),
                                               CorporateApproved = sum(Approved, na.rm = T),
                                               CorporatePaid = sum(Paid, na.rm = T),
                                               CorporateDue = sum(Due, na.rm = T)), by = .(ShopName)]

CorporateSellerPayment$ShopName <- tolower(CorporateSellerPayment$ShopName)
CorporateSellerPayment$ShopName <- str_squish(CorporateSellerPayment$ShopName)

rm(corporateTrackerUrl, corporateTracker, i, temp)


#Part 2: PROCESSING####


##Invoice Status####

totalInv <- unqSales[OrderStatus != "cancel", .(Total.Invoice = sum(count, na.rm = T)), by = .(ShopName)]
processingInv <- unqSales[OrderStatus == "processing",
                          .(Processing.Invoice = sum(count, na.rm = T)), by = .(ShopName)]
pickedInv <- unqSales[OrderStatus == "picked",
                      .(Picked.Invoice = sum(count, na.rm = T)), by = .(ShopName)]
refundedInv <- unqSales[grepl("refund", LastUpdate, ignore.case = T) & (OrderStatus == "shipped" | OrderStatus == "delivered"),
                        .(Refunded.Invoice = sum(count, na.rm = T)), by = .(ShopName)]
deliveredInv <- unqSales[!grepl("refund", LastUpdate, ignore.case = T) & (OrderStatus == "shipped" | OrderStatus == "delivered"),
                         .(Delivered.And.Shipped.Inv = sum(count, na.rm = T)), by = .(ShopName)]
undeliveredInv <- unqSales[OrderStatus == "picked" | OrderStatus == "processing", .(Undelivered.Invoice = sum(count, na.rm = T)),
                           by = .(ShopName)]

InvoiceStatus <- merge(merge(merge(merge(merge(totalInv, processingInv, all = T),
                                         pickedInv, all = T),
                                   deliveredInv, all = T),
                             refundedInv, all = T),
                       undeliveredInv, all = T) %>%
                    mutate(refundRatio = Refunded.Invoice/Total.Invoice)


#merge InvoiceStatus report with commercial info
shopInvoiceReport <- merge(merge(merge(unqShop, InvoiceStatus, all = T, by = c("ShopName")),
                                 commercialInfo, all = F, by = c("ShopName")),
                           CorporateSellerPayment, all.x = T, all.y = F, by = c("ShopName")) %>%
                    mutate(Month = month(mdy(CampaignDate)))

shopInvoiceReport <- shopInvoiceReport[!is.na(shopInvoiceReport$ShopName), ]
shopInvoiceReport <- shopInvoiceReport[!is.na(shopInvoiceReport$Total.Invoice), ]

#export shop report
write.csv(shopInvoiceReport, "Data/Output Data/CommercialReport/marginShopInvoiceReport.csv")

#stop auto running here

rm(totalInv, processingInv, pickedInv, deliveredInv, refundedInv, undeliveredInv)


##shop-item report####

#Order status, month, shop, item wise item count. All can take about 20 minutes.
total <- sales[OrderStatus != "cancel", .(Total.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem)]
processing <- sales[OrderStatus == "processing", .(Processing.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem)]
picked <- sales[OrderStatus == "picked", .(Picked.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem)]
refunded <- sales[grepl("refund", LastUpdate, ignore.case = T) & (OrderStatus == "shipped" | OrderStatus == "delivered"), .(Refunded.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem)]
delivered <- sales[!grepl("refund", LastUpdate, ignore.case = T) & (OrderStatus == "shipped" | OrderStatus == "delivered"),
                   .(Delivered.And.Shipped.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem)]
undelivered <- sales[OrderStatus == "picked" | OrderStatus == "processing", .(Undelivered.Quantity = sum(OrderQuantity, na.rm = T)),
                     by = .(ShopName, OrderItem)]

ItemStatus <- merge(merge(merge(merge(merge(total, processing, all = T),
                                      picked, all = T),
                                delivered, all = T),
                          refunded, all = T),
                    undelivered, all = T)

#last date difference
deliveredInv <- unqSales[!grepl("refund", LastUpdate, ignore.case = T) & (OrderStatus == "shipped" | OrderStatus == "delivered"),
                         .(Delivered.And.Shipped.Inv = sum(count, na.rm = T)), by = .(ShopName, OrderItem)]

shopDelivery <- dplyr::filter(sales, OrderStatus == 'shipped' | OrderStatus == 'delivered') %>%
                    dplyr::filter(!grepl("refund", LastUpdate, ignore.case = T)) %>%
                    mutate(deliDays = dmy(LastUpdatedate) - mdy(CampaignDate))
shopDelivery <- shopDelivery[, .(shopItemDeliTime = sum(deliDays, na.rm = T)), by = .(ShopName, OrderItem)]

shopDelivery$shopItemDeliTime <- as.numeric(shopDelivery$shopItemDeliTime, units="days")

#item evaly price
evalyPrice <- sales %>% group_by(ShopName, OrderItem) %>%
                    mutate(itemCount = row_number()) %>%
                    filter(itemCount == 1) %>%
                    select(c("ShopName", "OrderItem", "unit.OrderPrice", "unit.seller.price",
                             "unit.mrp"))
evalyPrice$unit.OrderPrice <- as.numeric(evalyPrice$unit.OrderPrice)
evalyPrice$unit.seller.price <- as.numeric(evalyPrice$unit.seller.price)
evalyPrice$unit.mrp <- as.numeric(evalyPrice$unit.mrp)

shopItemReport <- merge(merge(merge(merge(merge(unqShop, ItemStatus, all = F, by = c("ShopName")),
                                          evalyPrice, all = F, by = c("ShopName", "OrderItem")),
                                    commercialInfo, all = F, by = c("ShopName")),
                              shopDelivery, all.x = T, all.y = F, by = c("ShopName", "OrderItem")),
                        deliveredInv, all.x = T, all.y = F, by = c("ShopName", "OrderItem")) %>%
                    mutate(TotalOrderValue = Total.Quantity * unit.OrderPrice,
                           ProcessingValue = Processing.Quantity * unit.OrderPrice,
                           PickedValue = Picked.Quantity * unit.OrderPrice,
                           Delivered.And.Shipped.Value = Delivered.And.Shipped.Quantity * unit.OrderPrice,
                           Undelivered.Value = Undelivered.Quantity * unit.OrderPrice,
                           refundValue = Refunded.Quantity*unit.seller.price,
                           
                           TotalSellerPrice = Total.Quantity * unit.seller.price,
                           ProcessingSellerPrice = Processing.Quantity * unit.seller.price,
                           PickedSellerPrice = Picked.Quantity * unit.seller.price,
                           Delivered.And.Shipped.SellerPrice = Delivered.And.Shipped.Quantity * unit.seller.price,
                           Undelivered.SellerPrice = Undelivered.Quantity * unit.seller.price,
                           
                           comissionRate = (unit.mrp - unit.seller.price)/unit.mrp,
                           burnRate = (unit.seller.price - unit.OrderPrice)/unit.mrp,
                           discountRate = (unit.mrp- unit.OrderPrice)/unit.mrp,
                           
                           comissionValue = (unit.mrp-unit.seller.price)*Total.Quantity, 
                           discountValue = (unit.mrp - unit.OrderPrice)*Total.Quantity,
                           burnValue = (unit.seller.price - unit.OrderPrice)*Total.Quantity) %>%
                    cbind(count = 1)

shopItemReport <- data.table(shopItemReport)
cost <- shopItemReport[, .(Cost = sum(Delivered.And.Shipped.SellerPrice, Undelivered.SellerPrice, refundValue, na.rm = T)),
                       by = .(ShopName, OrderItem)]

shopItemReport <- merge(shopItemReport, cost, by = c("ShopName", "OrderItem")) %>%
                    mutate(Month = month(mdy(CampaignDate)),
                           costRatio = Cost/TotalOrderValue,
                           avgDeliTime = shopItemDeliTime/Delivered.And.Shipped.Inv)

shopItemReport <- shopItemReport[!is.na(shopItemReport$ShopName), ]
shopItemReport <- shopItemReport[!is.na(shopItemReport$Total.Quantity), ]

write.csv(shopItemReport, "Data/Output Data/CommercialReport/marginShopItemReport.csv")

rm(total, processing, picked, delivered, refunded, undelivered, shopDeliTime, shopDelivery, evalyPrice, cost, deliveredInv)


#Part 3: Export####

shopInvoiceReport <- read.csv("Data/Output Data/CommercialReport/marginShopInvoiceReport.csv",
                              stringsAsFactors = F, header = T, comment.char = "")
shopItemReport <- read.csv("Data/Output Data/CommercialReport/marginShopItemReport.csv",
                           stringsAsFactors = F, header = T, comment.char = "")

shopInvoiceReport <- data.table(shopInvoiceReport)
shopItemReport <- data.table(shopItemReport)


shopInvoiceReport <- shopInvoiceReport[, c("Category", "CatHead", "BDM", "KAM",
                                           "MotherCompany", "ShopName", "CampaignDate", "Month", "Week",
                                           "Total.Invoice", "Processing.Invoice", "Picked.Invoice", "Delivered.And.Shipped.Inv",
                                           "Refunded.Invoice", "Undelivered.Invoice", "refundRatio",
                                           "CorporateSubmitted", "CorporateApproved", "CorporatePaid", "CorporateDue")]

shopItemReport <- shopItemReport[, c("MotherCompany", "KAM", "BDM", "Category", "CatHead",
                                     "ShopName", "CampaignDate", "Month", "Week",
                                     "OrderItem", "unit.OrderPrice", "unit.seller.price", "unit.mrp",
                                     "Total.Quantity", "Processing.Quantity", "Picked.Quantity", "Delivered.And.Shipped.Quantity",
                                     "Refunded.Quantity", "Undelivered.Quantity",
                                     "TotalSellerPrice", "ProcessingSellerPrice", "PickedSellerPrice", "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                                     "TotalOrderValue", "ProcessingValue", "PickedValue", "Delivered.And.Shipped.Value", "Undelivered.Value", "refundValue",
                                     "comissionRate", "burnRate", "discountRate", "comissionValue", "discountValue", "burnValue",
                                     "Cost", "costRatio", "avgDeliTime", "shopItemDeliTime", "count")]

shopInvoiceReport <- mutate(shopInvoiceReport, Week = week(mdy(shopInvoiceReport$CampaignDate)))
shopItemReport <- mutate(shopItemReport, Week = week(mdy(shopItemReport$CampaignDate)))

##Management report####
# MonthTotalValue <- shopItemReport[,.(MonthTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Month)]
# MCTotalValue <- shopItemReport[,.(MCTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, BDM, KAM, MotherCompany, Month)]
# MLMCTotalValue <- shopItemReport[,.(MLMCTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, BDM, KAM, MotherCompany)]
# KAMTotalValue <- shopItemReport[,.(KAMTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, BDM, KAM, Month)]
# BDMTotalValue <- shopItemReport[,.(BDMTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, BDM, Month)]
# CatTotalValue <- shopItemReport[,.(CatHeadTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, Month)]
# CompanyTotalValue <- shopItemReport[,.(CompanyTotalValue = sum(TotalOrderValue, na.rm = T))]

# shopItemReport <- merge(merge(merge(merge(merge(merge(shopItemReport, MCTotalValue, all = T, by = c("Category", "BDM", "KAM", "MotherCompany", "Month")),
#                                                 MonthTotalValue, all = T, by = c("Month")),
#                                           KAMTotalValue, all = T, by = c("Category", "BDM", "KAM", "Month")),
#                                     BDMTotalValue, all = T, by = c("Category", "BDM", "Month")),
#                               CatTotalValue, all = T, by = c("Category", "Month")),
#                         MLMCTotalValue, all = T, by = c("Category", "BDM", "KAM", "MotherCompany"))
# 
# shopItemReport <- cbind(shopItemReport, CompanyTotalValue)
# 
# rm(MCTotalValue, MLMCTotalValue, KAMTotalValue, BDMTotalValue, CatTotalValue, MonthTotalValue, CompanyTotalValue)


#week
MCTotalValue <- shopItemReport[,.(MCTotalValue = sum(TotalOrderValue, na.rm = T)), by = .(Category, BDM, KAM, MotherCompany, Week)]
CompanyTotalValue <- shopItemReport[,.(CompanyTotalValue = sum(TotalOrderValue, na.rm = T))]

names(MCTotalValue)
shopItemReport <- merge(shopItemReport, MCTotalValue, all = F, by = c("Category", "BDM", "KAM", "MotherCompany", "Week"))

shopItemReport <- cbind(shopItemReport, CompanyTotalValue)

rm(MCTotalValue, MLMCTotalValue, KAMTotalValue, BDMTotalValue, CatTotalValue, MonthTotalValue, CompanyTotalValue)

###Mother company report####
#margin analysis####
MCInvoiceReport <- shopInvoiceReport[ , .(TotalInvoice = sum(Total.Invoice, na.rm = T),
                                          ProcessingInvoice = sum(Processing.Invoice, na.rm = T),
                                          PickedInvoice = sum(Picked.Invoice, na.rm = T),
                                          DeliveredAndShippedInvoice = sum(Delivered.And.Shipped.Inv, na.rm = T),
                                          RefundedInvoice = sum(Refunded.Invoice, na.rm = T),
                                          Submitted = sum(CorporateSubmitted, na.rm = T),
                                          Approved = sum(CorporateApproved, na.rm = T),
                                          Paid = sum(CorporatePaid, na.rm = T),
                                          Due = sum(CorporateDue, na.rm = T)),
                                      by = .(Category, BDM, KAM, MotherCompany, Month)]

MCReport <- shopItemReport[, .(Revenue = sum(TotalOrderValue, na.rm = T),
                               Delivered.And.Shipped.Value = sum(Delivered.And.Shipped.Value, na.rm = T),
                               UndeliveredValue = sum(Undelivered.Value, na.rm = T),
                               Delivered.And.Shipped.SellerPrice = sum(Delivered.And.Shipped.SellerPrice, na.rm = T),
                               Undelivered.SellerPrice = sum(Undelivered.SellerPrice, na.rm = T),
                               shopItemDeliTime = sum(shopItemDeliTime, na.rm = T),
                               Cost = sum(Cost, na.rm = T),
                               avgComissionRate = sum(comissionRate*(TotalOrderValue/MCTotalValue), na.rm = T),
                               avgBurnRate = sum(burnRate*(TotalOrderValue/MCTotalValue), na.rm = T),
                               avgDiscountRate = sum(discountRate*(TotalOrderValue/MCTotalValue), na.rm = T)),
                           by = .(Category, BDM, KAM, MotherCompany, Month)]

MCReport <- merge(MCInvoiceReport, MCReport, all = T, by = c("Category", "BDM", "KAM", "MotherCompany", "Month")) %>%
                    mutate(creditPerformance = Delivered.And.Shipped.SellerPrice/Paid,
                           avgRefundRatio = RefundedInvoice/TotalInvoice,
                           avgDeliTime = shopItemDeliTime/DeliveredAndShippedInvoice,
                           avgCostRatio = Cost/Revenue)

MCReport[sapply(MCReport, is.infinite)] <- NA
rm(MCInvoiceReport)

MCReport <- MCReport[, c("Category", "BDM", "KAM", "MotherCompany", "Month",
                         "TotalInvoice", "ProcessingInvoice", "PickedInvoice", "DeliveredAndShippedInvoice", "RefundedInvoice",
                         "Revenue", "Cost", "Delivered.And.Shipped.Value", "UndeliveredValue",
                         "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                         "avgComissionRate", "avgBurnRate", "avgDiscountRate",
                         "avgCostRatio", "avgRefundRatio", "avgDeliTime", "creditPerformance",
                         "Submitted", "Approved", "Paid", "Due")]

write.csv(MCReport, "Data/Output Data/CommercialReport/MCReport.csv")

#Monthless MC Report
MLMCInvoiceReport <- shopInvoiceReport[ , .(TotalInvoice = sum(Total.Invoice, na.rm = T),
                                            ProcessingInvoice = sum(Processing.Invoice, na.rm = T),
                                            PickedInvoice = sum(Picked.Invoice, na.rm = T),
                                            DeliveredAndShippedInvoice = sum(Delivered.And.Shipped.Inv, na.rm = T),
                                            RefundedInvoice = sum(Refunded.Invoice, na.rm = T),
                                            Submitted = sum(CorporateSubmitted, na.rm = T),
                                            Approved = sum(CorporateApproved, na.rm = T),
                                            Paid = sum(CorporatePaid, na.rm = T),
                                            Due = sum(CorporateDue, na.rm = T)),
                                        by = .(Category, BDM, KAM, MotherCompany)]

MLMCReport <- shopItemReport[, .(Revenue = sum(TotalOrderValue, na.rm = T),
                                 Delivered.And.Shipped.Value = sum(Delivered.And.Shipped.Value, na.rm = T),
                                 UndeliveredValue = sum(Undelivered.Value, na.rm = T),
                                 Delivered.And.Shipped.SellerPrice = sum(Delivered.And.Shipped.SellerPrice, na.rm = T),
                                 Undelivered.SellerPrice = sum(Undelivered.SellerPrice, na.rm = T),
                                 shopItemDeliTime = sum(shopItemDeliTime, na.rm = T),
                                 Cost = sum(Cost, na.rm = T),
                                 avgComissionRate = sum(comissionRate*(TotalOrderValue/MLMCTotalValue), na.rm = T),
                                 avgBurnRate = sum(burnRate*(TotalOrderValue/MLMCTotalValue), na.rm = T),
                                 avgDiscountRate = sum(discountRate*(TotalOrderValue/MLMCTotalValue), na.rm = T)),
                             by = .(Category, BDM, KAM, MotherCompany)]

MLMCReport <- merge(MLMCInvoiceReport, MLMCReport, all = T, by = c("Category", "BDM", "KAM", "MotherCompany")) %>%
                    mutate(creditPerformance = Delivered.And.Shipped.SellerPrice/Paid,
                           avgRefundRatio = RefundedInvoice/TotalInvoice,
                           avgDeliTime = shopItemDeliTime/DeliveredAndShippedInvoice,
                           avgCostRatio = Cost/Revenue)

rm(MLMCInvoiceReport)

MLMCReport <- MLMCReport[, c("Category", "BDM", "KAM", "MotherCompany",
                             "TotalInvoice", "ProcessingInvoice", "PickedInvoice", "DeliveredAndShippedInvoice", "RefundedInvoice",
                             "Revenue", "Cost", "Delivered.And.Shipped.Value", "UndeliveredValue",
                             "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                             "avgComissionRate", "avgBurnRate", "avgDiscountRate",
                             "avgCostRatio", "avgRefundRatio", "avgDeliTime", "creditPerformance",
                             "Submitted", "Approved", "Paid", "Due")]

write.csv(MLMCReport, "Data/Output Data/CommercialReport/MLMCReport.csv")

###Month company report####

MonthInvoiceReport <- shopInvoiceReport[ , .(TotalInvoice = sum(Total.Invoice, na.rm = T),
                                             ProcessingInvoice = sum(Processing.Invoice, na.rm = T),
                                             PickedInvoice = sum(Picked.Invoice, na.rm = T),
                                             DeliveredAndShippedInvoice = sum(Delivered.And.Shipped.Inv, na.rm = T),
                                             RefundedInvoice = sum(Refunded.Invoice, na.rm = T),
                                             Submitted = sum(CorporateSubmitted, na.rm = T),
                                             Approved = sum(CorporateApproved, na.rm = T),
                                             Paid = sum(CorporatePaid, na.rm = T),
                                             Due = sum(CorporateDue, na.rm = T)),
                                         by = .(Month)]


MonthReport <- shopItemReport[, .(Revenue = sum(TotalOrderValue, na.rm = T),
                                  Delivered.And.Shipped.Value = sum(Delivered.And.Shipped.Value, na.rm = T),
                                  UndeliveredValue = sum(Undelivered.Value, na.rm = T),
                                  Delivered.And.Shipped.SellerPrice = sum(Delivered.And.Shipped.SellerPrice, na.rm = T),
                                  Undelivered.SellerPrice = sum(Undelivered.SellerPrice, na.rm = T),
                                  shopItemDeliTime = sum(shopItemDeliTime, na.rm = T),
                                  Cost = sum(Cost, na.rm = T),
                                  avgComissionRate = sum(comissionRate*(TotalOrderValue/MonthTotalValue), na.rm = T),
                                  avgBurnRate = sum(burnRate*(TotalOrderValue/MonthTotalValue), na.rm = T),
                                  avgDiscountRate = sum(discountRate*(TotalOrderValue/MonthTotalValue), na.rm = T)),
                              by = .(Month)]

MonthReport <- merge(MonthInvoiceReport, MonthReport, all = T) %>%
                    mutate(creditPerformance = Delivered.And.Shipped.SellerPrice/Paid,
                           avgRefundRatio = RefundedInvoice/TotalInvoice,
                           avgDeliTime = shopItemDeliTime/DeliveredAndShippedInvoice,
                           avgCostRatio = Cost/Revenue)

MonthReport[sapply(MonthReport, is.infinite)] <- NA
rm(MonthInvoiceReport)

MonthReport <- MonthReport[, c("Month",
                               "TotalInvoice", "ProcessingInvoice", "PickedInvoice", "DeliveredAndShippedInvoice", "RefundedInvoice",
                               "Revenue", "Cost", "Delivered.And.Shipped.Value", "UndeliveredValue",
                               "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                               "avgComissionRate", "avgBurnRate", "avgDiscountRate",
                               "avgCostRatio", "avgRefundRatio", "avgDeliTime", "creditPerformance",
                               "Submitted", "Approved", "Paid", "Due")]

write.csv(MonthReport, "Data/Output Data/CommercialReport/MonthReport.csv")

#overall

CompanyInvoiceReport <- shopInvoiceReport[ , .(TotalInvoice = sum(Total.Invoice, na.rm = T),
                                               ProcessingInvoice = sum(Processing.Invoice, na.rm = T),
                                               PickedInvoice = sum(Picked.Invoice, na.rm = T),
                                               DeliveredAndShippedInvoice = sum(Delivered.And.Shipped.Inv, na.rm = T),
                                               RefundedInvoice = sum(Refunded.Invoice, na.rm = T),
                                               Submitted = sum(CorporateSubmitted, na.rm = T),
                                               Approved = sum(CorporateApproved, na.rm = T),
                                               Paid = sum(CorporatePaid, na.rm = T),
                                               Due = sum(CorporateDue, na.rm = T))]


CompanyReport <- shopItemReport[, .(Revenue = sum(TotalOrderValue, na.rm = T),
                                    Delivered.And.Shipped.Value = sum(Delivered.And.Shipped.Value, na.rm = T),
                                    UndeliveredValue = sum(Undelivered.Value, na.rm = T),
                                    Delivered.And.Shipped.SellerPrice = sum(Delivered.And.Shipped.SellerPrice, na.rm = T),
                                    Undelivered.SellerPrice = sum(Undelivered.SellerPrice, na.rm = T),
                                    shopItemDeliTime = sum(shopItemDeliTime, na.rm = T),
                                    Cost = sum(Cost, na.rm = T),
                                    avgComissionRate = sum(comissionRate*(TotalOrderValue/CompanyTotalValue), na.rm = T),
                                    avgBurnRate = sum(burnRate*(TotalOrderValue/CompanyTotalValue), na.rm = T),
                                    avgDiscountRate = sum(discountRate*(TotalOrderValue/CompanyTotalValue), na.rm = T))]

CompanyReport <- cbind(CompanyInvoiceReport, CompanyReport) %>%
                    mutate(creditPerformance = Delivered.And.Shipped.SellerPrice/Paid,
                           avgRefundRatio = RefundedInvoice/TotalInvoice,
                           avgDeliTime = shopItemDeliTime/DeliveredAndShippedInvoice,
                           avgCostRatio = Cost/Revenue)

rm(CompanyInvoiceReport)

CompanyReport <- CompanyReport[, c("TotalInvoice", "ProcessingInvoice", "PickedInvoice", "DeliveredAndShippedInvoice", "RefundedInvoice",
                                   "Revenue", "Cost", "Delivered.And.Shipped.Value", "UndeliveredValue",
                                   "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                                   "avgComissionRate", "avgBurnRate", "avgDiscountRate",
                                   "avgCostRatio", "avgRefundRatio", "avgDeliTime", "creditPerformance",
                                   "Submitted", "Approved", "Paid", "Due")]

write.csv(CompanyReport, "Data/Output Data/CommercialReport/CompanyReport.csv")

#MC-week report
MCInvoiceReport <- shopInvoiceReport[ , .(TotalInvoice = sum(Total.Invoice, na.rm = T),
                                          ProcessingInvoice = sum(Processing.Invoice, na.rm = T),
                                          PickedInvoice = sum(Picked.Invoice, na.rm = T),
                                          DeliveredAndShippedInvoice = sum(Delivered.And.Shipped.Inv, na.rm = T),
                                          RefundedInvoice = sum(Refunded.Invoice, na.rm = T),
                                          Submitted = sum(CorporateSubmitted, na.rm = T),
                                          Approved = sum(CorporateApproved, na.rm = T),
                                          Paid = sum(CorporatePaid, na.rm = T),
                                          Due = sum(CorporateDue, na.rm = T)),
                                      by = .(Category, BDM, KAM, MotherCompany, Week)]

MCReport <- shopItemReport[, .(Revenue = sum(TotalOrderValue, na.rm = T),
                               Delivered.And.Shipped.Value = sum(Delivered.And.Shipped.Value, na.rm = T),
                               UndeliveredValue = sum(Undelivered.Value, na.rm = T),
                               Delivered.And.Shipped.SellerPrice = sum(Delivered.And.Shipped.SellerPrice, na.rm = T),
                               Undelivered.SellerPrice = sum(Undelivered.SellerPrice, na.rm = T),
                               shopItemDeliTime = sum(shopItemDeliTime, na.rm = T),
                               Cost = sum(Cost, na.rm = T),
                               avgComissionRate = sum(comissionRate*(TotalOrderValue/MCTotalValue), na.rm = T),
                               avgBurnRate = sum(burnRate*(TotalOrderValue/MCTotalValue), na.rm = T),
                               avgDiscountRate = sum(discountRate*(TotalOrderValue/MCTotalValue), na.rm = T)),
                           by = .(Category, BDM, KAM, MotherCompany, Week)]

MCReport <- merge(MCInvoiceReport, MCReport, all = T, by = c("Category", "BDM", "KAM", "MotherCompany", "Week")) %>%
                    mutate(creditPerformance = Delivered.And.Shipped.SellerPrice/Paid,
                           avgRefundRatio = RefundedInvoice/TotalInvoice,
                           avgDeliTime = shopItemDeliTime/DeliveredAndShippedInvoice,
                           avgCostRatio = Cost/Revenue)

MCReport[sapply(MCReport, is.infinite)] <- NA
rm(MCInvoiceReport)

MCReport <- MCReport[, c("Category", "BDM", "KAM", "MotherCompany", "Week",
                         "TotalInvoice", "ProcessingInvoice", "PickedInvoice", "DeliveredAndShippedInvoice", "RefundedInvoice",
                         "Revenue", "Cost", "Delivered.And.Shipped.Value", "UndeliveredValue",
                         "Delivered.And.Shipped.SellerPrice", "Undelivered.SellerPrice",
                         "avgComissionRate", "avgBurnRate", "avgDiscountRate",
                         "avgCostRatio", "avgRefundRatio", "avgDeliTime", "creditPerformance",
                         "Submitted", "Approved", "Paid", "Due")]

write.csv(MCReport, "Data/Output Data/CommercialReport/MCweekReport.csv")
