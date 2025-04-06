library(lme4)
library(tidyverse)
library(lmerTest)
library(caret)
library(Metrics)
library(MASS)
library(car)
library(magrittr)
library(lubridate)


# setwd("~/Data science university/SCC 460/Project")

cleaned_data = read_csv("Cleaned_Data.csv", 
                        col_types = 'fffcddfdddifffd') # importing clan data set
cleaned_data %<>%
  mutate(Transaction_date = ymd(Transaction_date)) # making string into data type

# dividing train data, test data and validation data
# validation data will be the last week
last_day_of_the_week = as.numeric(ymd("2022-08-24")) # day of cut off
validation_data = cleaned_data %>% filter(Transaction_date >= last_day_of_the_week)
train_test_data = cleaned_data %>% filter(Transaction_date < last_day_of_the_week)
# Training data and test data using caret
set.seed(21)
train_index = createDataPartition(train_test_data$Store_Number, 
                                  p = .85, 
                                  list = FALSE, 
                                  times = 1)
train_data = train_test_data[train_index,]
test_data = train_test_data[-train_index,]


# fitting glm with all coveriates plus store_number and product number with interaction
model_glm = glm.nb(Full_Price_Sales_Quantity ~ (Product_Code %in% Store_Number) + SubSect_Description + OPENING_TIME * CLOSING_TIME + BH_FLAG + Weather_Feels_Like * Weather_Rain * Weather_Cloud * SEASON + day_of_the_week * WEEK + numerical_transaction_date, data = train_data)

stepAIC(model_glm, scope = Full_Price_Sales_Quantity ~ (Product_Code %in% Store_Number) + SubSect_Description + OPENING_TIME * CLOSING_TIME + BH_FLAG + Weather_Feels_Like * Weather_Rain * Weather_Cloud * SEASON + day_of_the_week * WEEK + numerical_transaction_date, direction = 'backward')

best_model = glm.nb(formula =  Full_Price_Sales_Quantity ~ (Product_Code %in% Store_Number) + 
                      CLOSING_TIME + BH_FLAG + Weather_Feels_Like + 
                      Weather_Rain + Weather_Cloud + SEASON + day_of_the_week + 
                      numerical_transaction_date + 
                      Weather_Feels_Like:Weather_Rain + Weather_Feels_Like:Weather_Cloud + 
                      Weather_Feels_Like:SEASON + Weather_Rain:SEASON + Weather_Cloud:SEASON + 
                      Weather_Feels_Like:Weather_Cloud:SEASON, 
                    data = train_data)


with(summary(best_model), 
     1 - log(deviance)/log(null.deviance)) # pseudo R^2
# finding RMSE for training data
rmse_training = rmse(train_data$Full_Price_Sales_Quantity, 
     predict(best_model, 
             newdata = train_data, 
             type = "response"))

# finding RMSE for test data
rmse_test = rmse(test_data$Full_Price_Sales_Quantity, 
     predict(best_model, newdata = test_data, type = "response")) 

# finding RMSE for validation data
rmse_validation = rmse(validation_data$Full_Price_Sales_Quantity, 
     predict(best_model, newdata = validation_data, type = "response")) # RMSE validation data

# mean quantity 
mean_quantity = mean(cleaned_data$Full_Price_Sales_Quantity) 

# relative rmses

rmse_training / mean_quantity * 100

rmse_test / mean_quantity * 100

rmse_validation / mean_quantity * 100

# plotting residuals to check overdispersion
plot(best_model$fitted.values, residuals(best_model, type="pearson"))


# extra analysis for single product in many stores
#choosing product number
product_number = 673134

# separating two sets with the product we want from all 11 stores
data_1 = validation_data %>%
  filter(Product_Code == product_number, Store_Number %in% c(3446, 8048, 7867, 5343))
data_2 = validation_data %>%
  filter(Product_Code == product_number, Store_Number %in% c(8042, 6002, 5751, 5147, 8051, 8093))

# predicting all 11 stores with GLM
a_1 = predict(best_model, newdata = data_1, type = "response")
a_2 = predict(best_model, newdata = data_2, type = "response")

# binding our result in a data frame
validation_prediction_1 = cbind(data_1, round(a_1))
validation_prediction_2 = cbind(data_2, round(a_2))

# plotting values from predicte against observed data
validation_prediction_1 %>%
  ggplot(aes(x = Transaction_date, y = Full_Price_Sales_Quantity)) +
  geom_point(aes(color = Store_Number)) +
  geom_point(aes(y = a_1)) +
  labs(title = "Plotting of Observed data vs predicted data", subtitle = "Black line is for the predicted values")+
  facet_grid(rows = vars(Store_Number), scale = "free_y")

validation_prediction_2 %>%
  ggplot(aes(x = Transaction_date, y = Full_Price_Sales_Quantity)) +
  geom_point(aes(color = Store_Number)) +
  geom_point(aes(y = a_2)) +
  labs(title = "Plotting of Observed data vs predicted data", subtitle = "Black line is for the predicted values")+
  facet_grid(rows = vars(Store_Number), scale = "free_y")

# train data
rmse(train_data$Full_Price_Sales_Quantity, predict(best_model, newdata = train_data, type = "response"))
# validation data
rmse(validation_data$Full_Price_Sales_Quantity, predict(best_model, newdata = validation_data, type = "response"))

# rmse for train data
rmse(train_data$Full_Price_Sales_Quantity, predict(best_model_1, newdata = train_data, type = "response"))
# rmse for validation data
rmse(validation_data$Full_Price_Sales_Quantity, predict(best_model_1, newdata = validation_data, type = "response"))
