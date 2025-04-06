library(tidyverse)
library(lmerTest)
library(caret)
library(Metrics)
library(MASS)
library(magrittr)
library(lubridate)
library(randomForest)
library(ranger)


setwd("~/Data science university/SCC 460/Project")

cleaned_data = read_csv("Cleaned_Data.csv", 
                        col_types = 'fffcddfdddifffd') # importing the data set
cleaned_data %<>%
  mutate(Transaction_date = ymd(Transaction_date)) # changing the data type from character to date
# dividing train data, test data and validation data
# validation data will be the last week
last_day_of_the_week = as.numeric(ymd("2022-08-24")) # day of cut off
validation_data = cleaned_data %>% filter(Transaction_date >= last_day_of_the_week) # validation set
train_test_data = cleaned_data %>% filter(Transaction_date < last_day_of_the_week) # test set
# Training data and test data using caret
set.seed(21)
train_index = createDataPartition(train_test_data$Store_Number, 
                                  p = .85, 
                                  list = FALSE, 
                                  times = 1)
train_data_full = train_test_data[train_index,]
train_data_feature = train_data_full %>% dplyr::select(-Full_Price_Sales_Quantity)
test_data_full = train_test_data[-train_index,]
test_data_feature = test_data_full %>% dplyr::select(-Full_Price_Sales_Quantity)
# separating validation variables and target values
validation_data_covariates = validation_data %>% dplyr::select(-Full_Price_Sales_Quantity)
validation_data_target = validation_data %>% dplyr::select(Full_Price_Sales_Quantity) %>% pull(1)

# fitting Random forest
set.seed(21)

hyper_grid <- expand.grid( # hyperparameters to test the best of them
  mtry = seq(2, 14, by = 1),
  max_depth = seq(0, 21, by = 3),
  RMSE_list = 0
)

for(i in 1:nrow(hyper_grid)) { # finding the optimal random forest from the grid above 
  
  # train model
  models <- ranger(
    formula = Full_Price_Sales_Quantity ~ ., 
    data = train_data_full, 
    num.trees = 500,
    mtry = hyper_grid$mtry[i], # changing mtry
    max.depth = hyper_grid$max_depth[i], # changing max depth
    oob.error = FALSE,
    write.forest = TRUE,
    importance = 'impurity',
    seed = 21
  )
  
  pred_valid <- predict(models, # predicting the test set
                        test_data_feature, 
                        type = "response")
  
  # add RMSE to grid
  hyper_grid$RMSE_list[i] = rmse(test_data_full$Full_Price_Sales_Quantity, 
                                 pred_valid$predictions)
}
#best hyperparameters
hyper_grid %>%
  arrange(RMSE_list) %>%
  slice(1)
# mtry 9 max depth 12  RMSE 2.411567

# creating the best model using the optimal hyperparameters 
#from the grid search
best_model = ranger( 
  formula = Full_Price_Sales_Quantity ~ ., 
  data = train_data_full, 
  num.trees = 2000,
  mtry = 9, # changing mtry
  max.depth = 12, # changing max depth
  write.forest = TRUE,
  importance = 'impurity',
  seed = 21
)
# predicting training set, test set and validation set
pred_training = predict(best_model, 
                        train_data_feature, 
                        type = "response")

pred_test = predict(best_model, 
                    test_data_feature, 
                    type = "response")

pred_valid = predict(best_model, 
                     validation_data_covariates, 
                     type = "response")




# Print the results
rmse(train_data_full$Full_Price_Sales_Quantity, 
     pred_training$predictions) # rmse for training set
rmse(test_data_full$Full_Price_Sales_Quantity, 
     pred_test$predictions) # rmse for test set
rmse(validation_data_target, 
     pred_valid$predictions) # rmse for validation set
best_model$r.squared # pseudo r^2 
# finding mean quantity to see how bad the rmse is compared to mean quantity
cleaned_data %>% summarise(mean(Full_Price_Sales_Quantity))

# Analysis from small data set
#choosing product number
product_number = 673134

# separating two sets with the product we want from all 11 stores
data_1 = validation_data %>%
  filter(Product_Code == product_number, 
         Store_Number %in% c(3446, 8048, 7867, 5343))
data_2 = validation_data %>%
  filter(Product_Code == product_number, 
         Store_Number %in% c(8042, 6002, 5751, 5147, 8051, 8093))

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
