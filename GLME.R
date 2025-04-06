library(lme4)
library(tidyverse)
library(lmerTest)
library(caret)
library(Metrics)
library(MASS)
library(car)
library(magrittr)
library(lubridate)

setwd("~/Data science university/SCC 460/Project")

cleaned_data = read_csv("Cleaned_Data.csv" , col_types = 'fffcddfdddifffd')
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

# Product of choice : 673134 for deeper analysis
# all coveriates : ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + Transaction_date
#full_model = glmer.nb(formula = Full_Price_Sales_Quantity ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud  + numerical_transaction_date + (1|Store_Number) + (1|Store_Number:Product_Code),
#                            data = train_data) #to run the model
intercept_model = glmer.nb(formula = Full_Price_Sales_Quantity ~  1 + (1|Store_Number) + (1|Store_Number:Product_Code),
                      data = train_data) #to run the model
add1(intercept_model, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date, test = "Chisq")

# adding WEEKEND
model_with_1 = glmer.nb(formula = Full_Price_Sales_Quantity ~  WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                           data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_1, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")

# adding closing time
model_with_2 = glmer.nb(formula = Full_Price_Sales_Quantity ~  CLOSING_TIME + WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                        data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_2, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")

# adding Weather_Feels_Like
model_with_3 = glmer.nb(formula = Full_Price_Sales_Quantity ~  Weather_Feels_Like + CLOSING_TIME + WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                        data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_3, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")

# adding numerical_transaction_date
model_with_4 = glmer.nb(formula = Full_Price_Sales_Quantity ~  numerical_transaction_date + Weather_Feels_Like + CLOSING_TIME + WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                        data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_4, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")

# adding BH_FLAG
model_with_5 = glmer.nb(formula = Full_Price_Sales_Quantity ~ BH_FLAG + numerical_transaction_date + Weather_Feels_Like + CLOSING_TIME + WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                        data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_5, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")

# adding Weather_Cloud
model_with_6 = glmer.nb(formula = Full_Price_Sales_Quantity ~ Weather_Cloud + BH_FLAG + numerical_transaction_date + Weather_Feels_Like + CLOSING_TIME + WEEKEND + (1|Store_Number) + (1|Store_Number:Product_Code),
                        data = na.omit(train_data)) #to run the model

# ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud,
add1(model_with_6, ~ ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date,  test = "Chisq")


a_1 = predict(model_with_6, newdata = train_data, type = "response" )
a_2 = round(predict(model_with_6, newdata = test_data, type = "response" ))
a_3 = round(predict(model_with_6, newdata = validation_data, type = "response" ))
rmse(train_data$Full_Price_Sales_Quantity, a_1)
rmse(test_data$Full_Price_Sales_Quantity, a_2)
rmse(validation_data$Full_Price_Sales_Quantity, a_3)

b = cbind(test_data, a_2)

cleaned_data %>%
  filter(Product_Code == 673134) %>%
ggplot(aes(x = Transaction_date, y = Full_Price_Sales_Quantity, color = Store_Number)) +
  geom_line()

test_data %>%
  filter(Product_Code == 673134, Store_Number == 3446) %>%
  ggplot() +
  geom_line(aes(x = Transaction_date, y = Full_Price_Sales_Quantity)) +
  geom_line(aes(x = Transaction_date, y = a_2, color = "black"))

b %>%
  filter(Product_Code == 673134, Store_Number == 3446) %>%
  ggplot(aes(x = Transaction_date, y = Full_Price_Sales_Quantity, color = Store_Number)) +
  geom_line() +
  geom_line(aes(y = a_2, color = "black"))
  

model_glm = glm.nb(Full_Price_Sales_Quantity ~ Store_Number * Product_Code + ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date, data = train_data)

stepAIC(model_glm, scope = Full_Price_Sales_Quantity ~ Store_Number * Product_Code + ISB...ROLL + ISB...WMEAL.LOAVES + ISB...BAGUETTES + OPENING_TIME + CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + WEEKEND + BH_FLAG + SPRING + SUMMER + AUTUMN + Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date, direction = 'backward')

best_model = glm.nb(formula = Full_Price_Sales_Quantity ~ Store_Number + Product_Code + 
         CLOSING_TIME + MONDAY + TUESDAY + WEDNESDAY + THURSDAY + 
         FRIDAY + SATURDAY + BH_FLAG + SPRING + SUMMER + AUTUMN + 
         Weather_Feels_Like + Weather_Rain + Weather_Cloud + numerical_transaction_date + 
         Store_Number:Product_Code, data = train_data, init.theta = 20.58829854, 
       link = log)