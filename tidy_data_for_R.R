library(tidyverse)
library(lubridate)
library(ggforce)
library(MASS)
library(magrittr)



forecasting_data = read_csv('LU Grp project data - Co-op ISB forecasting.csv', 
                            col_types = 'fffffciiffffffffffffiiiinin') #Uploading the data


# Making the dummy variables into one hot coded
forecasting_data = forecasting_data %>%
  mutate(Transaction_date = ymd(Transaction_date),
    WINTER = case_when(
    AUTUMN == 0 & SUMMER == 0 & SPRING == 0 ~ 1,
    TRUE ~ 0),
    WINTER = as_factor(WINTER), # Creating WINTER variable instead of having for spring, summer and winter all equal to 0
    SUNDAY = case_when(
    MONDAY == 0 & TUESDAY == 0 & WEDNESDAY == 0 & THURSDAY == 0 & FRIDAY == 0 & SATURDAY == 0 ~ 1,
    TRUE ~ 0),
    SUNDAY = as_factor(SUNDAY) # Does the same but for SUNDAY 
  )
# transforming the data set into a tidy data set
pivot_data = forecasting_data %>%
pivot_longer(cols = c('MONDAY', 
                      'TUESDAY', 
                      'WEDNESDAY', 
                      'THURSDAY', 
                      'FRIDAY', 
                      'SATURDAY', 
                      'SUNDAY'),
             names_to = 'day_of_the_week', 
             values_to = 'values', 
             names_transform = as.factor)%>%
  filter(values == 1) %>%
  dplyr::select(-values) %>%
  mutate(day_of_the_week = fct_relevel(day_of_the_week, 
                                       c('MONDAY', 
                                         'TUESDAY', 
                                         'WEDNESDAY', 
                                         'THURSDAY', 
                                         'FRIDAY', 
                                         'SATURDAY', 
                                         'SUNDAY'))) %>% # combining the columns monday to sunday into 1
  pivot_longer(cols = c('SPRING', 
                        'SUMMER', 
                        'AUTUMN', 
                        'WINTER'), 
               names_to = 'SEASON', 
               values_to = 'values', 
               names_transform = as.factor)%>% 
  filter(values == 1) %>%
  dplyr::select(-values) %>%
  mutate(SEASON = fct_relevel(SEASON, 
                              c('SPRING', 
                                'SUMMER', 
                                'AUTUMN', 
                                'WINTER'))) %>% # combining seasons into 1 column
  pivot_longer(cols = c('MIDWEEK', 
                        'WEEKEND'), 
               names_to = 'WEEK', 
               values_to = 'values',
               names_transform = as.factor)%>% #combining mid week and weekend into one column
  filter(values == 1) %>%
  dplyr::select(-values, 
                -Store_Name, 
                -Product_Description, 
                -Full_Price_Sales_Turnover, 
                -Reduced_To_Clear_Quantity, 
                -Reduced_To_Clear_Turnover) %>% # removing all variables that have a one to one relationship
  filter(Full_Price_Sales_Quantity >= 0)

# finding distinct poducts with product description to see if it a 
#one to one o a one to many variable
b = forecasting_data %>%
  distinct(Product_Code, 
           Product_Description)

# by doing the following transformation we have already transformed 
#from 29 variable to 19 variable since the data wasn't tidy before.

summary(pivot_data)
#filtering out the grouped data that has the 
#last week of quantity sales
# finding the distinct store and product code that had 
#any quantity sold on
last_day_of_the_week = as_date("2022-08-24") # last day to have 7 days

grouped_data_distinct = pivot_data %>%
  filter(Transaction_date >= last_day_of_the_week) %>%
  distinct(Store_Number, Product_Code)

# Keeping only products that had sales in the last week
forecasting_data_tidy_with_last_week_quantity = pivot_data %>%
  inner_join(grouped_data_distinct, by = c("Store_Number", "Product_Code"))


# creating function to normalise data
normalising_data = function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# normalising data for weather rain will use max as 32 since it is the biggest mm in a day ever recorded
normalising_data_weather_rain = function(x) {
  (x - 0) / (32 - 0)
}

# normalising data for weather temperature will use max as 50 (max was 42) and minimum -30 (-28)
normalising_data_weather_temperature = function(x) {
  (x - (-30)) / (50 - (-30))
}

# normalising data for date with min as the initial date but maximum as the beggining of 2023
normalising_data_date = function(x) {
  (x - min(x)) / (as.numeric(ymd("2023-01-01")) - min(x))
}
# normalising the data
forecasting_data_tidy_with_last_week_quantity %<>%
  mutate(Weather_Feels_Like = normalising_data_weather_temperature(Weather_Feels_Like),
         Weather_Rain = normalising_data_weather_rain(Weather_Rain),
         Weather_Cloud = normalising_data(Weather_Cloud),
         OPENING_TIME = normalising_data(OPENING_TIME),
         CLOSING_TIME = normalising_data(CLOSING_TIME),
         numerical_transaction_date = as.numeric(Transaction_date) - as.numeric(min(Transaction_date)),
         numerical_transaction_date = normalising_data_date(numerical_transaction_date))

#final data set is:
forecasting_data_tidy_with_last_week_quantity %>% count(Store_Number)
write.csv(forecasting_data_tidy_with_last_week_quantity, 
          "Cleaned_Data.csv", 
          row.names = FALSE)
a = read_csv("Cleaned_Data.csv")
a = a %>% mutate(Store_Number = as_factor(Store_Number))
a %>% count(Store_Number)



# extra analysis
# It shows a few important patterns but it also shows 
#that there were quantities bellow 0, which I would interpret 
#as impossible  at the moment with not further context But count data 
#can't be negative

# plotting all the time series from product store combination
pivot_data %>%
  ggplot(aes(x = Transaction_date, 
             y = Full_Price_Sales_Quantity, 
             col = Store_Number)) +
  geom_line() +
  facet_wrap_paginate(Store_Number ~ Product_Code, 
                      scale = 'free_y', 
                      nrow = 3, 
                      ncol = 1) +
  theme_classic() -> p

required_n_pages <- n_pages(p)

for(i in 1:required_n_pages){
  
  ggplot(pivot_data, aes(x = Transaction_date, y = Full_Price_Sales_Quantity, col = Store_Number)) +
    geom_line() +
    facet_wrap_paginate(Store_Number ~ Product_Code, 
                        scale = 'free_y', 
                        nrow = 3, 
                        ncol = 1,
                        page = i) +
    theme_classic() -> p
  
  print(p) 
  }

names(pivot_data)
a = glm.nb(Full_Price_Sales_Quantity ~ ., data = pivot_data)
summary(a)

plot(fitted(a), residuals(a))
