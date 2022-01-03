#libraries####

library(readxl)
library(googlesheets4)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)

#load commercial info

commercialInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1JrT1sy5QGvSR6GaYiVEn2WrCS6y72ke0sJnYaw-VYAg",
                             range = "Updated Sellers List!A:K", col_names = T, na = "") %>%
                    select(c("Shop Name", "Mother Company", "KAM Name", "BDM Name",
                             "Category Head Name", "Category")) %>%
                    dplyr::rename(ShopName = 'Shop Name', MotherCompany = 'Mother Company',
                                  KAM = 'KAM Name', BDM = 'BDM Name')

sheet_write(commercialInfo, ss = "https://docs.google.com/spreadsheets/d/1XcbcdPlO63v5qaATkL10uhy63EPyqAi3-5bCnqGllUg/",
            sheet = "CommercialInfo")

#load seller payment data####

Tania_01 <- read_sheet("https://docs.google.com/spreadsheets/d/1hU85H6c5GUTwuGzewlaa07Moj5RXAKjvRq8wy0Ahi_s/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Saddam_02 <- read_sheet("https://docs.google.com/spreadsheets/d/1q8H-I-bdlxfLu5kQWLvc2d9zrTw1PPpxYqeS3JU8R8Q/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Yen_03 <- read_sheet("https://docs.google.com/spreadsheets/d/1cKob9jUD1fEmVfgGTI8pTdpWl8AI6t5_5jPzxZnGUK4/",
                     range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Himoy_04 <- read_sheet("https://docs.google.com/spreadsheets/d/1qm3LmoV8gxgxRAJF0IS9p4nGgdV2A18P2SJRhyRCXzY/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Sabbir_05 <- read_sheet("https://docs.google.com/spreadsheets/d/1x98jYNzvXcI4SU-0pW_pCaRqPL18RWubIkb6uG8r3Ks/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

RaiyanShakil_06 <- read_sheet("https://docs.google.com/spreadsheets/d/1vNcXXMZODtaZEiPbMv8KmakXWs_hd3pSv927pfGTu6A/",
                              range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Moshiur_07 <- read_sheet("https://docs.google.com/spreadsheets/d/1yVSBwXYvqpP-YKpDQMKRCQ3PP-T1hRO_4obBpgDursg/",
                         range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Najia_08 <- read_sheet("https://docs.google.com/spreadsheets/d/1H5f7kUNw_Hhwub2fZZ9H7yI9HMbdtvddhAP5LdaBLt0/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Rakib_09 <- read_sheet("https://docs.google.com/spreadsheets/d/1N3bBo01oIPmn0Gkwfsc_PoZUyWLduNPvvXL8dG-Dbtk/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Amitav_11 <- read_sheet("https://docs.google.com/spreadsheets/d/1ohc5w4L_eY89xOFvWunLz7QWxF5CtqQEv_7NF3DkJKM/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Jayed_12 <- read_sheet("https://docs.google.com/spreadsheets/d/1-V1RrbOE0mF7L8AmtyUD7xu4BSjthFVlTzMVM5PrzJo/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Eshita_13 <- read_sheet("https://docs.google.com/spreadsheets/d/16W8etCZvsz_1TD514LcNGRqwST4eTvrLW5tdDxYE0Eg/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Nazia_14 <- read_sheet("https://docs.google.com/spreadsheets/d/19B-yFQwqxGbArhBaAUHD-i_7IMXI-ThGubwRgjVdKPk/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Mashuk_15 <- read_sheet("https://docs.google.com/spreadsheets/d/1dey92uyNrNuxHChcgUS0a8ZTUCkT_QvA8bIT5rfMrzg/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Jaheed_16 <- read_sheet("https://docs.google.com/spreadsheets/d/1MuWJrcE1mjPrvMr68KpKGEfq1xzMDj7ofLYi_48fQkg/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Monjurul_17 <- read_sheet("https://docs.google.com/spreadsheets/d/19aYp6FVfKAxC1xo4KTOkDsw8ZG_B6a55KX7g9bwVOoc/",
                          range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

SpecialReq_18 <- read_sheet("https://docs.google.com/spreadsheets/d/1cB57xzB6-HlYaC9sneVpJVFbvXIAriac7ilZHqtOvIc/",
                            range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Emon_19 <- read_sheet("https://docs.google.com/spreadsheets/d/1XSFviPllz4uM4867d5-_7fbYKF-mbGOToGEYRMYE3R8/",
                      range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Shish_21 <- read_sheet("https://docs.google.com/spreadsheets/d/1G9rQWCMulumbMpvm9JGHPFgtonwxi6g-Jr1FauOWThc/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Noor_22 <- read_sheet("https://docs.google.com/spreadsheets/d/1m50q_BMtolfc3LXMXkzPphwgb4o3KyEh6Fns4Lb_klE/",
                      range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Mozahedul_23 <- read_sheet("https://docs.google.com/spreadsheets/d/1shQHi_KyeDv1DhjOAAhExqJ-FKv79z0MZQm8WIAxNSU/",
                           range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

Zilani_24 <- read_sheet("https://docs.google.com/spreadsheets/d/1unBIFv_dQUTHz_YV9Itu0fjEemdWoB3Nxk-vux9MI0M/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Campaign Name", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Total Due for this bill")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Campaign Name',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Total Due for this bill')

superShop <- read_sheet("https://docs.google.com/spreadsheets/d/1WEy1Qk1qfqj3PZICqOCSMfHnHO7ayvHRGbuajD-IHV8/",
                        range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Mother Company / Payment To", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Due Amount")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Mother Company / Payment To',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Due Amount')


corporateTracker <- rbind(Tania_01, Saddam_02, Yen_03, Himoy_04, Sabbir_05, RaiyanShakil_06, Moshiur_07, 
                          Najia_08, Rakib_09, Amitav_11, Jayed_12, Eshita_13, Nazia_14, Mashuk_15,
                          Jaheed_16, Monjurul_17, SpecialReq_18, Emon_19,
                          Shish_21, Noor_22, Mozahedul_23 , Zilani_24, superShop) %>%
                    select(-c("PaymentDate"))

corporateTracker <- corporateTracker[!is.na(corporateTracker$Submitted), ]
corporateTracker$Serial <- as.character(corporateTracker$Serial)
corporateTracker$ShopName <- as.character(corporateTracker$ShopName)
corporateTracker$Submitted <- as.numeric(gsub(",","",corporateTracker$Submitted))
corporateTracker$Approved <- as.numeric(gsub(",","",corporateTracker$Approved))
corporateTracker$Paid <- as.numeric(gsub(",","",corporateTracker$Paid))
corporateTracker$Due <- as.numeric(gsub(",","",corporateTracker$Due))

#Payment date calculations:
#Going forward, split the data to columns, take average over different date columns


corporateTracker <- filter(corporateTracker, Submitted > 0)

rm(Tania_01, Saddam_02, Yen_03, Himoy_04, Sabbir_05, RaiyanShakil_06,
   Moshiur_07, Najia_08, Rakib_09, Amitav_11, Jayed_12, Eshita_13, Nazia_14,
   Mashuk_15, Jaheed_16, Monjurul_17, SpecialReq_18, Emon_19,
   Shish_21, Noor_22, Mozahedul_23, Zilani_24, superShop)


#seller payment
shopSubmitted <- ddply(corporateTracker, .(ShopName), summarise, shopSubmitted = sum(Submitted, na.rm = T))
shopApproved <- ddply(corporateTracker, .(ShopName), summarise, shopApproved = sum(Approved, na.rm = T))
shopPaid <- ddply(corporateTracker, .(ShopName), summarise, shopPaid = sum(Paid, na.rm = T))
shopDue <- ddply(corporateTracker, .(ShopName), summarise, shopDue = sum(Due, na.rm = T))


#merge seller payment data - merge avgPaymentDateDiff here, once completed
CorporateSellerPayment <- merge(merge(merge(merge(shopSubmitted, shopApproved, all = T),
                                      shopPaid, all = T),
                                shopDue, all = T),
                    commercialInfo, all.x = T, all.y = F)%>%
                    rename(corporateSubmitted = "shopSubmitted", corporateApproved = "shopApproved",
                           corporatePaid = "shopPaid", corporateDue = "shopDue")


#CorporateSellerPayment$avgPaymentDateDiff <- as.numeric(CorporateSellerPayment$avgPaymentDateDiff, units = "days")
rm(shopSubmitted, shopApproved, shopPaid, shopDue, corporateTracker)


#retail Tracker####

retail20 <- read_sheet("https://docs.google.com/spreadsheets/d/1WEy1Qk1qfqj3PZICqOCSMfHnHO7ayvHRGbuajD-IHV8/",
                       range = "Bill Submission Summary!A4:AR", col_names = T, na = "") %>%
                    select(c("Bill Serial No", "Mother Company / Payment To", "Payment Date", "Submitted Bill",
                             "Approved Amount", "Amount", "Due Amount")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Mother Company / Payment To',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Due Amount')


retail21 <- read_sheet("https://docs.google.com/spreadsheets/d/1KtFrZmBlkJ5moY5OHAtpqpZPb1bG8y0nXd4O6Vz-HNU/",
                       range = "Bill Submission!A5:AM", col_names = T, na = "") %>%
                    select(c("Bill Serial No",	"Shop Name...3", "Payment Date", "Submitted Bill", "Approved Amount",
                             "Amount", "Due of Requred Payment")) %>%
                    dplyr::rename(Serial = 'Bill Serial No', ShopName = 'Shop Name...3',
                                  PaymentDate = 'Payment Date', Submitted = 'Submitted Bill', Approved = 'Approved Amount',
                                  Paid = 'Amount', Due = 'Due of Requred Payment')

retailTracker <- merge(retail20, retail21, all = T) %>%
                    select(-c("PaymentDate"))

retailTracker$Submitted <- as.numeric(gsub(",","",retailTracker$Submitted))
retailTracker$Approved <- as.numeric(gsub(",","",retailTracker$Approved))
retailTracker$Paid <- as.numeric(gsub(",","",retailTracker$Paid))
retailTracker$Due <- as.numeric(gsub(",","",retailTracker$Due))

retailTracker$Submitted <- as.integer(unlist(retailTracker$Submitted))
retailTracker$Approved <- as.integer(retailTracker$Approved)
retailTracker$Paid <- as.integer(retailTracker$Paid)
retailTracker$Due <- as.integer(retailTracker$Due)

retailTracker <- filter(retailTracker, Submitted > 0)


retailSubmitted <- ddply(retailTracker, .(ShopName), summarise, Submitted = sum(Submitted, na.rm = T))
retailApproved <- ddply(retailTracker, .(ShopName), summarise, Approved = sum(Approved, na.rm = T))
retailPaid <- ddply(retailTracker, .(ShopName), summarise, Paid = sum(Paid, na.rm = T))
retailDue <- ddply(retailTracker, .(ShopName), summarise, Due = sum(Due, na.rm = T))


#retail tracker date processing
retailSellerPayment <- merge(merge(merge(merge(retailSubmitted, retailApproved, all = T),
                                   retailPaid, all = T),
                             retailDue, all = T),
                       commercialInfo, all.x = T, all.y = F) %>%
                    rename(retailSubmitted = "Submitted",
                              retailApproved = "Approved", retailPaid = "Paid", retailDue ="Due")

rm(retail20, retail21, retailTracker, retailSubmitted, retailApproved, retailPaid, retailDue)

#seller payment
sellerPayment <- merge(CorporateSellerPayment, retailSellerPayment, all = T)

sheet_write(CorporateSellerPayment, ss = "https://docs.google.com/spreadsheets/d/1XcbcdPlO63v5qaATkL10uhy63EPyqAi3-5bCnqGllUg/",
            sheet = "PaymentInfo")
