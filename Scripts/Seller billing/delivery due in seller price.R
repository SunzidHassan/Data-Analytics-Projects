#Load libraries#####
library(readxl)
library(googlesheets4)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)

#load and process commercial info####
commercialInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1JrT1sy5QGvSR6GaYiVEn2WrCS6y72ke0sJnYaw-VYAg",
                             range = "Updated Sellers List!A:K", col_names = T, na = "") %>%
  select(c("Shop Name", "Mother Company", "KAM Name", "BDM Name", "Campaign Type", "Category"))

commercialInfo <- dplyr::rename(commercialInfo, ShopName = 'Shop Name', MotherCompany = 'Mother Company',
                                KAM = 'KAM Name', BDM = 'BDM Name', Campaign = 'Campaign Type')


#Load and process system sales data####
#11 full, 12 till 25, 12 till 31, 1 till 15, 1 till 31, 2 full, 3 till 15, 3 till 31, 4 till
#take the folder path
file.list <- list.files(path = "Data/Input Data/System Report", pattern="*.xlsx", full.names = T)

#load the excel files, rename the last column as shopCode. Takes about 5 minutes for data since November.
system.time(
  sales <- lapply(file.list, read_excel) %>%
    bind_rows(.id = "id") %>%
    dplyr::rename(shopCode = ...22))

# add month column, remove comma from last update time and add last update date column,
# select these columns: "Invoice No", "Order Date", "Shop Name", "Order Status", "Total Price",
# "Order Items", "Order Quantity", "Order Price", "Last Update", "LastUpdatedate", "shopCode".

sales <-  mutate(sales, month = month(mdy(sales$`Order Date`), label = T, abbr = T))%>% 
  cbind("LastUpdatedate" = gsub(pattern = ",.*", "", sales$`Last Update Time`)) %>%
  dplyr::select(c("Invoice No", "Order Date", "Shop Name", "Order Status", "Total Price", "Order Items",
                  "Order Quantity", "Order Price", "Last Update", "LastUpdatedate", "shopCode")) %>%
  dplyr::rename(Invoice = 'Invoice No', OrderDate = 'Order Date', ShopName = 'Shop Name',
                OrderStatus = 'Order Status', TotalPrice = 'Total Price', OrderItem = 'Order Items',
                OrderQuantity = 'Order Quantity', OrderPrice = 'Order Price', LastUpdate = 'Last Update')

#add LastdateDiff column
sales <-  mutate(sales, lastDateDiff = Sys.Date() - dmy(sales$LastUpdatedate))

#make the data unique against invoice
unqSales <- data.frame(sales[!duplicated(sales$`Invoice`), ])


#load sales with product seller price data####
salesWprodSP <- read.csv("Data/Output Data/salesWprodSP.csv",
                         stringsAsFactors = F, header = T, comment.char = "")

#merge sales, commercial and product data####
system.time(
salesInfo <- merge(merge(unqSales, commercialInfo, by = c("ShopName"), all.x = T, all.y = F),
                   salesWprodSP, all.x = T, all.y = F)) %>%
  dplyr::select(-c("X"))

#delivered and backlog
delivered <- dplyr::filter(salesInfo, grepl("shipped|delivered", OrderStatus)) %>%
  dplyr::filter(!grepl("refund", LastUpdate, ignore.case = T)) %>%
  dplyr::filter(!grepl("Express", Campaign, ignore.case = T)) %>%
  select(c("ShopName", "TotalPrice", "lastDateDiff", "KAM", "InvoiceSP"))

backlog <-  dplyr::filter(salesInfo, grepl("processing|picked", OrderStatus)) %>%
  dplyr::filter(!grepl("refund", LastUpdate, ignore.case = T)) %>%
  dplyr::filter(!grepl("Express", Campaign, ignore.case = T)) %>%
  select(c("ShopName", "TotalPrice", "lastDateDiff", "KAM", "InvoiceSP"))

#unique shops
unqShopNames <- data.frame(salesInfo[!duplicated(salesInfo$ShopName), ]) %>%
  select(c("ShopName"))


#add PDC info####
PDCinfo <- read_sheet("https://docs.google.com/spreadsheets/d/1yLzK1UodPZSAx3pcXBDB8rGPtdeMdqbf8WYLrfpiGoc",
                      range = "Summary!A3:W", col_names = T, na = "") %>%
  select(c("Primary Shop Name", "KAM", "Days Since Cheque Issuance",
           "Days Since Target Delivery to Clear till Date")) %>%
  dplyr::rename(Primary.Shop.Name = 'Primary Shop Name', Days.Since.Cheque.Issuance = 'Days Since Cheque Issuance',
                Days.Since.Target.Delivery.to.Clear.till.Date = 'Days Since Target Delivery to Clear till Date')

PDCinfo$Primary.Shop.Name <- str_squish(PDCinfo$Primary.Shop.Name)
PDCinfo$KAM <- str_squish(PDCinfo$KAM)
PDCinfo$Days.Since.Cheque.Issuance <- as.numeric(PDCinfo$Days.Since.Cheque.Issuance)
PDCinfo$Days.Since.Target.Delivery.to.Clear.till.Date <- as.numeric(PDCinfo$Days.Since.Target.Delivery.to.Clear.till.Date)

#loop to get pdc shop data####
#create an empty data frame for storing data
shopValue <- data.frame(Shop.Name = NA, postComitDelivered = NA, postComitDeliveredSP = NA,
                        SPDeliInvoice = NA, NASPDeliInvoice = NA, backlog = NA, backlogSP = NA,
                        backlogTillTargetDate = NA, backlogTillTargetDateSP = NA,
                        SPBackInvoice = NA, NASPBackInvoice = NA)

#PDC shops post commitment delivery completion - takes about 10 minutes
system.time(
  for (i in 1:length(PDCinfo$Primary.Shop.Name)){
    name <- dplyr::filter(unqShopNames, grepl(PDCinfo[i,1], ShopName, ignore.case = T))
    if(length(name$ShopName) > 0){
      #shop name in the first column
      shopValue[i, 1] <- PDCinfo[i, 1]
      
      #filter delivered value of given shop and time
      deliFilter <- dplyr::filter(delivered, grepl(PDCinfo[i,1], ShopName, ignore.case = T)) %>%
        #dplyr::filter(grepl(PDCinfo[i,2], KAM, ignore.case = T)) %>%
        filter(lastDateDiff < PDCinfo[i, 3])
      
      #in the ith row of second column, insert the total sales of rows filtered above
      shopValue[i, 2] <- sum(deliFilter$TotalPrice, na.rm = T)
      shopValue[i, 3] <- sum(deliFilter$InvoiceSP)
      shopValue[i, 4] <- length(deliFilter[complete.cases(deliFilter$InvoiceSP), 1])
      shopValue[i, 5] <- length(deliFilter[!complete.cases(deliFilter$InvoiceSP), 1])
      
      
      backFilter <- dplyr::filter(backlog, grepl(PDCinfo[i,1], ShopName, ignore.case = T)) #%>%
        #dplyr::filter(grepl(PDCinfo[i,2], KAM, ignore.case = T))
      
      #in the ith row of third column, write total backlog order value of the shop upto the target date
      shopValue[i, 6] <- sum(backFilter$TotalPrice, na.rm = T)
      shopValue[i, 7] <- sum(backFilter$InvoiceSP, na.rm = T)
      shopValue[i, 10] <- length(backFilter[complete.cases(backFilter$InvoiceSP), 1])
      shopValue[i, 11] <- length(backFilter[!complete.cases(backFilter$InvoiceSP), 1])
      
      #if a time frame is given
      if(!is.na(PDCinfo[i,4])){
        # filter the shop name of pdcinfo from salesinfo, filter out shipped and delivered,
        # take orders with date diff greater than target date diff (turn it on if data is available)
        
        backFilter <- filter(backFilter, lastDateDiff > PDCinfo[i, 4])
        
        #in the ith row of third column, write total backlog order value of the shop upto the target date
        shopValue[i, 8] <- sum(backFilter$TotalPrice, na.rm = T)
        shopValue[i, 9] <- sum(backFilter$InvoiceSP, na.rm = T)
      }
      else{
        shopValue[i, 8] <- c("No Time frame given")
      }
    } else{
      shopValue[i, 1] <- PDCinfo[i, 1]
      shopValue[i, 2] <- c("Name doesn't match with System Data")
    }
  }
)

#write the data in PDC tracker####
sheet_write(shopValue, ss = "https://docs.google.com/spreadsheets/d/1yLzK1UodPZSAx3pcXBDB8rGPtdeMdqbf8WYLrfpiGoc",
            sheet = "DeliveryData")
