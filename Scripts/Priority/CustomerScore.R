#libraries####
library(readxl)
library(reshape2)
library(plyr)
library(dplyr)
library(googlesheets4)

file.list <- list.files(path = "Data/Input Data/System Report", pattern="*.xlsx", full.names = T)

#load the excel files, rename the last column as shopCode. Takes about 5 minutes for data since November.
system.time(sales <- lapply(file.list, read_excel) %>%
                                bind_rows(.id = "id") %>%
                                select(c("Customer Id", "Invoice No", "Shop Name", "User Score", "Order Status")) %>%
                                rename(Invoice = "Invoice No", CustomerID = 'Customer Id', ShopName = 'Shop Name',
                                       Score = 'User Score', OrderStatus = 'Order Status') %>%
                                dplyr::filter(grepl('processing|picked', OrderStatus)) %>%
                                filter(Score > 0))

score <- sales[!duplicated(sales$CustomerID), ] %>%
                    select(-c("ShopName", "OrderStatus", "Invoice"))

customer <- sales %>% group_by(CustomerID, ShopName)

write.csv(customer, "Data/Output Data/customerScore.csv")
write.csv(score, "Data/Output Data/Score.csv")
