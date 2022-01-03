#Load libraries#####
library(readxl)
library(plyr)
library(dplyr)
library(stringr)


#Load and process system sales data####

#take the folder path
file.list <- list.files(path = "Data/Input Data/System Report", pattern="*.xlsx", full.names = T)

#load the excel files, rename columns, select specific columns - takes about 4 minutes.
system.time(
  sales <- lapply(file.list, read_excel) %>%
    bind_rows(.id = "id") %>%
    dplyr::rename(Invoice = 'Invoice No', OrderDate = 'Order Date', ShopName = 'Shop Name',
                  TotalPrice = 'Total Price', OrderItem = 'Order Items',
                  OrderQuantity = 'Order Quantity', OrderPrice = 'Order Price', shopCode = ...22) %>%
  dplyr::select(c("Invoice", "OrderDate", "ShopName", "TotalPrice", "OrderItem",
                                              "OrderQuantity", "OrderPrice", "shopCode")))

#load and process product info####

#Product data loading - takes about 3 minutes
system.time(productData <- read.csv("Data/Input Data/allProductData.csv",
                                    stringsAsFactors = F, header = T, comment.char = "",
                                    colClasses = c("product.name"="character","id"="character",
                                                   "product.brand"="character", "product.category"="character",
                                                   "enlisted.shop.code"="character","mrp"="numeric",
                                                   "seller.price"="numeric","evaly.price"="numeric")) %>%
              rename(OrderItem = 'product.name', shopCode = 'enlisted.shop.code') %>%
              dplyr::filter(!grepl("express", shopCode, ignore.case = T)))



#calculate total seller price of invoice
salesWprod <- merge(sales, productData, by=c("OrderItem", "shopCode"),all.x = F, all.y = F)
salesWprod$OrderQuantity <- as.numeric(salesWprod$OrderQuantity)
salesWprod$seller.price <- as.numeric(salesWprod$seller.price)

#calculate order seller price
salesWprod <- mutate(salesWprod, OrderSP = (OrderQuantity * seller.price))

#calculate invoice seller price - takes 1.5 hour
system.time(salesWprod <- ddply(salesWprod, .(Invoice), summarise, InvoiceSP = sum(OrderSP)))
salesWprod <- rename(salesWprod, Invoice = 'salesWprod$Invoice')

#one time full merge - sales and salesWproduct data####
salesWprodSP <- merge(sales, salesWprod, by = c("Invoice"), all.x = T, all.y = F)



#export data
write.csv(salesWprodSP, "Data/Output Data/salesWprodSP.csv")


#further new inv check. #for the new invoices, calculate salesWprodSP, and rbind them to existing salesWprodSP####

#load sales, product and product with SP data

#first - load just the system sales data from code above
#second - load product data from code above
#third - load merged salesWprodSP data
salesWprodSP <- read.csv("Data/Output Data/salesWprodSP.csv",
                      stringsAsFactors = F, header = T, comment.char = "")


#get the unique invoices of sales and salesWprodSP data
unqInv.salesWprodSP <- data.frame(InvoiceSP[!duplicated(InvoiceSP$Invoice), ]) %>%
  select(c("Invoice"))

unqInv.sales <- data.frame(sales[!duplicated(sales$Invoice), ]) %>%
  select(c("Invoice"))

#find new invoices in new sales data
newInv <- data.frame(Invoice = unqInv.sales$Invoice[!unqInv.sales$Invoice %in% unqInv.salesWprodSP$Invoice])

#get sales information of new invoices
newSales <- merge(newInv, sales, all.x = T, all.y = F)

#calculate total seller price of invoice
newSalesWprod <- merge(newSales, productData, by=c("OrderItem", "shopCode"),all.x = T, all.y = F)
newSalesWprod$OrderQuantity <- as.numeric(newSalesWprod$OrderQuantity)
newSalesWprod$seller.price <- as.numeric(newSalesWprod$seller.price)

#calculate order seller price
newSalesWprod <- mutate(newSalesWprod, OrderSP = (OrderQuantity * seller.price))

#calculate invoice seller price
system.time(newSalesWprod <- ddply(newSalesWprod, .(newSalesWprod$Invoice), summarise, InvoiceSP = sum(OrderSP)))

#one time full merge - sales and salesWproduct data
newSalesWprodSP <- merge(newSales, newSalesWprod, by = c("Invoice"), all.x = T, all.y = F)

#combine the newSalesWprodSP with existing salesWprodSP
salesWprodSP <- rbind(salesWprodSP, newSalesWprodSP)

#export the data
write.csv(salesWprodSP, "Data/Output Data/salesWprodSP.csv")
