library("tidyverse")
library("keras")
library("neuralnet")
library("devtools")
library("reticulate")
library(seriation)
library(caret)
library(FSelector)
library(dplyr)

# Read the train and test data
train_data <- read_csv("training_set.csv")
test_data <- read_csv("testing_set.csv")

# Factor train and test data
train_data <- train_data %>% mutate_if(is.character, factor)
train_data <- train_data %>% mutate_if(is.logical, factor)

test_data <- test_data %>% mutate_if(is.character, factor)
test_data <- test_data %>% mutate_if(is.logical, factor)


# Preprocess data by removing unnecessary variables
train_data <- train_data %>%
  select(-state, -county_name, -cases_per_10000, -deaths_per_10000, -death_per_case)

# Observe statistic of the data
str(train_data)
str(test_data)
test_data_rf_final <- test_data
test_data_nnet_final <- test_data

# Artificial Neural Networks
fit_nnet <- train_data %>%
  train(bad ~ .,
        data = . ,
        method = "nnet",
        tuneGrid = expand.grid(
          size = c(3, 5, 7),
          decay = c(0, 0.01, 0.001)
          
          ),
        #preProcess = c("center", "scale"),
        trControl = trainControl(method = "repeatedcv", number = 50)
  )

# View the trained model
fit_nnet

# Display nnet variable importance
varImp(fit_nnet)


table(complete.cases(test_data))
str(test_data)

test_data_nnet <- test_data %>% na.omit

# Preprocess test data by removing unnecessary variables
test_data_nnet <- test_data_nnet %>%
  select(-state, -county_name, -cases_per_10000, -deaths_per_10000, -death_per_case)

# Predicted test data
test_data_nnet_final$bad_predicted <- predict(fit_nnet, test_data_nnet)

# Displaying the accuracy
actual_outcomes_nnet <- test_data_nnet_final$bad
accuracy_nnet <- mean(test_data_nnet_final$bad_predicted == actual_outcomes_nnet) * 100
print(paste("nnet Accuracy:", round(accuracy_nnet, 2), "%"))


#map for nnet for Bad_predicted
counties <- as_tibble(map_data("county"))
map_data_predictions <- left_join(counties, test_data_nnet_final, by = c("region" = "state", "subregion" = "county_name"))
ggplot(map_data_predictions) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(bad_predicted)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey", "NA" = "white")) +
  coord_fixed(1.3) +
  labs(title = "Predicted 'bad' Classification Map for Neural Networks", fill = "Classification") +
  theme_minimal()

#map for nnet for bad
counties <- as_tibble(map_data("county"))
map_data_predictions <- left_join(counties, test_data_nnet_final, by = c("region" = "state", "subregion" = "county_name"))
ggplot(map_data_predictions) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(bad)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey", "NA" = "white")) +
  coord_fixed(1.3) +
  labs(title = "Actual 'bad' Classification Map for Neural Networks", fill = "Classification") +
  theme_minimal()



# Random Forest
fit_rf <- train_data %>%
  train(bad ~ .,
        data = . ,
        method = "rf",
        trControl = trainControl(method = "boot", number = 50)
  )

fit_rf

# rf variable importance
varImp(fit_rf)
fit_rf$finalModel

# Predicted test data
test_data_rf <- test_data %>% na.omit

# Preprocess test data by removing unnecessary variables
test_data_rf <- test_data_rf %>%
  select(-state, -county_name, -cases_per_10000, -deaths_per_10000, -death_per_case)

test_data_rf_final$bad_predicted <- predict(fit_rf, test_data_rf)

# Displaying the accuracy
actual_outcomes <- test_data_rf_final$bad
accuracy <- mean(test_data_rf_final$bad_predicted == actual_outcomes) * 100
print(paste("rf Accuracy:", round(accuracy, 2), "%"))

str(test_data_rf_final)

#map for rf for Bad_predicted
counties <- as_tibble(map_data("county"))
map_data_predictions <- left_join(counties, test_data_rf_final, by = c("region" = "state", "subregion" = "county_name"))
ggplot(map_data_predictions) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(bad_predicted)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey", "NA" = "white")) +
  coord_fixed(1.3) +
  labs(title = "Predicted 'bad' Classification Map for Random Forest", fill = "Classification") +
  theme_minimal()

#map for rf for bad
counties <- as_tibble(map_data("county"))
map_data_predictions <- left_join(counties, test_data_rf_final, by = c("region" = "state", "subregion" = "county_name"))
ggplot(map_data_predictions) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(bad)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey", "NA" = "white")) +
  coord_fixed(1.3) +
  labs(title = "Actual 'bad' Classification Map for Random Forest", fill = "Classification") +
  theme_minimal()




