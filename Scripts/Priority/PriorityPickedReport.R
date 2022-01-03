library(readxl)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)


# Load and process system PrioritySales data####

file.list <- list.files(path = "Data/Input Data/PriorityPicked", pattern="*.xlsx", full.names = T)

#load the excel files, rename the last column as shopCode. Takes about 5 minutes for data since November.
PrioritySales <- lapply(file.list, read_excel) %>%
                    bind_rows(.id = "id")

PrioritySales <- cbind(PrioritySales, "LastUpdateDate" = gsub(pattern = ",.*", "", PrioritySales$`Last Update`)) %>%
                    dplyr::select(c("Invoice No", "Shop Name", 'Order Status', "Order Items", "Customer Id",
                                    "Order Date", "LastUpdateDate")) %>%
                    dplyr::rename(Invoice = 'Invoice No', ShopName = 'Shop Name',
                                  OrderStatus = 'Order Status', OrderItem = 'Order Items',
                                  CustomerID = "Customer Id", OrderDate = "Order Date")

priorityPicked <- filter(PrioritySales, grepl("Priority Store", PrioritySales$ShopName)) %>%
                    filter(dmy(LastUpdateDate) < Sys.Date())

priorityPicked <- priorityPicked[!duplicated(priorityPicked$Invoice), ]

priorityPicked$LastUpdateDate <- format(as.Date(priorityPicked$LastUpdateDate, '%d/%m/%Y'), "%m/%d/%Y")

write.csv(priorityPicked, "Data/Output Data/PriorityPicked/priorityPicked.csv")
