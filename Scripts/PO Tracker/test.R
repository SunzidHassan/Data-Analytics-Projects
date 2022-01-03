#libraries####
library(readxl)
library(googlesheets4)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)



#flood with formula
df <- data.frame(URL = "https://docs.google.com/spreadsheets/d/1xVlCUJHxwbuTYwNG7hArLRbWbMAiQJ92vNDyPVYzIvU/")

formula <- gs4_formula("=IF(A1<>'',IF(A1<>A2,SUMIFS(G:G,A:A,A1),''),'')")

for (i in 1:length(df)) {
                    range_flood(df[i,1],
                                sheet = "Test", range = "C2:C", cell = formula, reformat = TRUE)
}


#flood with formula
df <- data.frame(URL = "https://docs.google.com/spreadsheets/d/1xVlCUJHxwbuTYwNG7hArLRbWbMAiQJ92vNDyPVYzIvU/")

formula <- gs4_formula("=A2+B2")

for (i in 1:length(df)) {
                    range_flood(df[i,1],
                                sheet = "Test", range = "C2:C", cell = formula, reformat = TRUE)
}


#delete column
range_delete("https://docs.google.com/spreadsheets/d/1xVlCUJHxwbuTYwNG7hArLRbWbMAiQJ92vNDyPVYzIvU/",
             sheet = "Test", "B", shift = NULL)


