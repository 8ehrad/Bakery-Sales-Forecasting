library(tidyverse)
library(lubridate)
library(caret)
library(magrittr)
library(ltm)

#Uploading the data
setwd("~/Data science university/SCC 460/Project")

cleaned_data = read_csv("Cleaned_Data.csv" , 
                        col_types = 'fffcddfdddifffd') # uploading cleaned data set



Transaction_date = cleaned_data %>% # making the transaction date as dates 
  transmute(Transaction_date = ymd(Transaction_date))


cleaned_data %<>% # removing transactiong date
  dplyr::select(-Transaction_date)
  
# creating dummy variables for sub products
# making the column with dates with date format
dummy <- dummyVars(" ~ .", 
                   data = cleaned_data, 
                   levelsOnly = TRUE, 
                   fullRank = TRUE)
forecasting_data_tidy <- data.frame(predict(dummy, 
                                            newdata = cleaned_data))


forecasting_data_tidy = cbind(forecasting_data_tidy, 
                              Transaction_date) # adding back the transaction dates

#final data set is:
write.csv(forecasting_data_tidy, 
          "Cleaned_Data_Python.csv", 
          row.names = FALSE) # saving the data set as a csv
a = read_csv("Cleaned_Data_Python.csv")
a = a %>% mutate(Store_Number = as_factor(Store_Number))
a %>% count(Store_Number)





