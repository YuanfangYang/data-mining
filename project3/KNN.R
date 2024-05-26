library(caret)
library(dplyr)
library(ggplot2)
library(maps)

# Read the training data set
training_set <- read.csv("training_set.csv")

# Preprocess data by removing unnecessary variables
ts <- training_set %>%
  select(-state, -county_name, -cases_per_10000, -deaths_per_10000, -death_per_case) %>%
  mutate(
    bad = as.factor(bad),  # Ensure target variable is a factor
    total_pop = as.numeric(total_pop),
    male_pop = as.numeric(male_pop),
    female_pop = as.numeric(female_pop),
    white_pop = as.numeric(white_pop),
    black_pop = as.numeric(black_pop),
    asian_pop = as.numeric(asian_pop),
    hispanic_pop = as.numeric(hispanic_pop),
    amerindian_pop = as.numeric(amerindian_pop),
    commuters_by_public_transportation = as.numeric(commuters_by_public_transportation),
    median_income = as.numeric(median_income),
    housing_units = as.numeric(housing_units),
    vacant_housing_units = as.numeric(vacant_housing_units),
    employed_pop = as.numeric(employed_pop),
    unemployed_pop = as.numeric(unemployed_pop),
    in_undergrad_college = as.numeric(in_undergrad_college)
  )

# Set a seed for reproducibility
set.seed(123)

# Create folds for cross-validation
train_index <- createFolds(ts$bad, k = 10)

# Train a conditional inference tree
knnFit <- train(
  bad ~ .,
  method = "knn",
  data = ts,
  preProcess = "scale",
  tuneLength = 5,
  tuneGrid=data.frame(k= 1:10),
  trControl = trainControl(method = "cv", indexOut = train_index)
)
knnFit

knnFit$finalModel

# Prediction
full_dataset <- read.csv("testing_set.csv")
full_dataset_processed <- full_dataset %>%
  select(-state, -county_name, -cases_per_10000, -deaths_per_10000, -death_per_case) %>%
  mutate(
    total_pop = as.numeric(total_pop),
    male_pop = as.numeric(male_pop),
    female_pop = as.numeric(female_pop),
    white_pop = as.numeric(white_pop),
    black_pop = as.numeric(black_pop),
    asian_pop = as.numeric(asian_pop),
    hispanic_pop = as.numeric(hispanic_pop),
    amerindian_pop = as.numeric(amerindian_pop),
    commuters_by_public_transportation = as.numeric(commuters_by_public_transportation),
    median_income = as.numeric(median_income),
    housing_units = as.numeric(housing_units),
    vacant_housing_units = as.numeric(vacant_housing_units),
    employed_pop = as.numeric(employed_pop),
    unemployed_pop = as.numeric(unemployed_pop),
    in_undergrad_college = as.numeric(in_undergrad_college)
  )
predictions <- predict(knnFit, newdata = full_dataset_processed)
full_dataset$predictions <- predictions

# Map
counties <- as_tibble(map_data("county"))
map_data_predictions <- left_join(counties, full_dataset, by = c("region" = "state", "subregion" = "county_name"))
ggplot(map_data_predictions) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(predictions)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey", "NA" = "white")) +
  coord_fixed(1.3) +
  labs(title = "Predicted 'bad' Classification Map", fill = "Classification") +
  theme_minimal()

# Compute and print the confusion matrix
full_dataset$predictions <- factor(full_dataset$predictions, levels = c("TRUE", "FALSE"))
full_dataset$bad <- factor(full_dataset$bad, levels = c("TRUE", "FALSE"))
conf_matrix <- confusionMatrix(data = full_dataset$predictions, reference = full_dataset$bad)
print(conf_matrix)

