library(hts)
library(tidyverse)
library(caret)
library(Metrics)
library(magrittr)
library(lubridate)
library(TSA)
library(xts)
library(bayesforecast)
library(timetk)


setwd("~/Data science university/SCC 460/Project")

cleaned_data = read_csv("Cleaned_Data.csv" , col_types = 'ddfcddfdddifffd')
cleaned_data %<>%
  mutate(Transaction_date = ymd(Transaction_date))
# deviding train data, test data and validation data
# validation data will be the last week
last_day_of_the_week = as.numeric(ymd("2022-08-24")) # day of cut off
validation_data = cleaned_data %>% filter(Transaction_date >= last_day_of_the_week)
train_test_data = cleaned_data %>% filter(Transaction_date < last_day_of_the_week)
# Training data and test data using caret
set.seed(21)
train_index = createDataPartition(train_test_data$Store_Number, p = .85, 
                                  list = FALSE, 
                                  times = 1)
train_data = train_test_data[train_index,]
test_data = train_test_data[-train_index,]

data_train = train_data %>%
  dplyr::select(Transaction_date, Full_Price_Sales_Quantity, Store_Number, Product_Code)  %>%
  arrange(Transaction_date)

data_train_ts = data_train %>% tk_xts()

a = data_train %>%
  filter(Product_Code == 673134, Store_Number == 3446) %>%
  dplyr::select(Transaction_date, Full_Price_Sales_Quantity) %>%
  arrange(Transaction_date)

a_ts = a %>% tk_xts()
  

b = data_train %>%
  arrange(Transaction_date) %>%
  dplyr::select(Store_Number, Product_Code)





auto.sarima(data_train_ts)
arimax(data_train_ts[,1], order = c(0, 0, 0), seasonal = 
         (order = c(0, 0, 0), period = NA) ,xreg = data_train_ts[,c(2,3)])

# SARIMAX Model
SARIMAX_model = pm.auto_arima(data_train$Full_Price_Sales_Quantity, exogenous= data_train$Store_Number + data_train$Product_Code,
                              start_p=1, start_q=1,
                              test='adf',
                              max_p=3, max_q=3, m=12,
                              start_P=0, seasonal=True,
                              d=None, D=1, 
                              trace=False,
                              error_action='ignore',  
                              suppress_warnings=True, 
                              stepwise=True)
