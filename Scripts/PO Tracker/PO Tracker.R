#libraries####
library(readxl)
library(googlesheets4)
library(dplyr)
library(stringr)

#PO URLs
poUrl <- read.csv("Data/Input Data/POTrackers/URLs.csv",
                  stringsAsFactors = F, header = T, comment.char = "")

#PO summary

POSummary <- read_sheet("https://docs.google.com/spreadsheets/d/1TSoL_ojHi_bp-bFdv61VqKlb5STeu_WieBTiY_156Ko/",
                        range = "PO Summary!A4:M4", col_names = T, na = "")


for (i in 1:length(poUrl$URL)) {
   temp <- read_sheet(poUrl[i,2],
                      range = "PO Summary!A4:M", col_names = T, na = "")
   POSummary <- rbind(POSummary, temp)
}
POSummary <- POSummary[!is.na(POSummary$POID), ] %>%
   select(-c("Number of Invoices in PO"))


#PO item Summary

POItemSummary <- read_sheet("https://docs.google.com/spreadsheets/d/1TSoL_ojHi_bp-bFdv61VqKlb5STeu_WieBTiY_156Ko/",
                            range = "Item Summary!A5:H5", col_names = T, na = "")

for (i in 1:length(poUrl$URL)) {
   temp <- read_sheet(poUrl[i,2],
                      range = "Item Summary!A5:H", col_names = T, na = "")
   POItemSummary <- rbind(POItemSummary, temp)
}
POItemSummary <- POItemSummary[!is.na(POItemSummary$`Item Name`), ] %>%
   select(-c("Number of Invoice"))


#Invoice list

POInvoice <- read_sheet("https://docs.google.com/spreadsheets/d/1TSoL_ojHi_bp-bFdv61VqKlb5STeu_WieBTiY_156Ko/",
                            range = "Invoice Check List!A1:D1", col_names = T, na = "")

for (i in 1:length(poUrl$URL)) {
   temp <- read_sheet(poUrl[i,2],
                      range = "Invoice Check List!A1:D", col_names = T, na = "")
   POInvoice <- rbind(POInvoice, temp)
}
POInvoice <- POInvoice[!is.na(POInvoice$`Invoice Number`), ]


#export data####

sheet_write(POSummary, ss = "https://docs.google.com/spreadsheets/d/1KZC_-dYH9HkIneoi9JLyv3U_zl3vRSmOEQ8sNDIKTYA/",
            sheet = "Summary")

sheet_write(POItemSummary, ss = "https://docs.google.com/spreadsheets/d/1KZC_-dYH9HkIneoi9JLyv3U_zl3vRSmOEQ8sNDIKTYA/",
            sheet = "ItemSummary")

write.csv(POSummary, "Data/Output Data/POTracker/070821_POSummary.csv")
write.csv(POItemSummary, "Data/Output Data/POTracker/070821_POitemSummary.csv")
write.csv(POInvoice, "Data/Output Data/POTracker/070821_POinvoice.csv")

rm(POInvoice, POItemSummary, POSummary)

#load commercial info####
commercialInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1mQtrJOnxDpHpokiKphn4uyXSE-aQM3TtBjQlYzd8I-U/",
                             range = "Seller List with KAM/BDM!A:K", col_names = T, na = "") %>%
   select(c("Shop Name", "Mother Company", "KAM Name", "BDM Name",
            "Campaign Type", "Category Head /Team Lead Name")) %>%
   dplyr::rename(ShopName = "Shop Name", MotherCompany = "Mother Company", KAM = "KAM Name",
                 BDM = "BDM Name", Campaign = "Campaign Type", CatHead = "Category Head /Team Lead Name") %>%
   filter(Campaign == "T10")

poDataValidation <- select(commercialInfo, c("ShopName", "KAM")) %>%
   cbind(Merchant = NA, PrimaryShop = NA)

#write data validation####

for (i in 1:length(poUrl$URL)) {
   sheet_write(poDataValidation, ss = poUrl[i,2], sheet = "Data Validation Source")
}

rm(i, temp)
