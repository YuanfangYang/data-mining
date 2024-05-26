library(ggplot2)
library(reshape2)

# Create a dataframe with model performance metrics for Training and Testing
results <- data.frame(
  Model = rep(c("CIT", "C4.5", "KNN"), each = 4),
  Type = rep(c("Training", "Testing"), times = 6),
  Accuracy = c(0.8311304, 0.5698, 0.845072, 0.5505, 0.9266522, 0.5494, 0.83, 0.55, 0.87, 0.84, 0.90, 0.88),
  Kappa = c(0.6629013, 0.139, 0.6983570, 0.0981, 0.9253972, 0.0981, 0.66, 0.14, 0.70, 0.67, 0.75, 0.73)
)

# Melt the dataframe for ggplot
results_melted <- melt(results, id.vars = c("Model", "Type"),
                       measure.vars = c("Accuracy", "Kappa"),
                       variable.name = "Metric",
                       value.name = "Value")

# Plot for Training data
p1 <- ggplot(data = results_melted[results_melted$Type == "Training", ], aes(x = Model, y = Value, fill = Metric)) +
  geom_boxplot() +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Model Performance (Training)", x = "Model", y = "Metric Value") +
  theme_minimal()

# Plot for Testing data
p2 <- ggplot(data = results_melted[results_melted$Type == "Testing", ], aes(x = Model, y = Value, fill = Metric)) +
  geom_boxplot() +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Model Performance (Testing)", x = "Model", y = "Metric Value") +
  theme_minimal()

# Print the plots
print(p1)
print(p2)


p2 <- ggplot(results_melted[results_melted$Type == "Testing", ], aes(x = Model, y = value, fill = Metric)) +
  geom_boxplot() +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison - Testing",
       x = "Model",
       y = "Value") +
  theme_minimal()

# Print the plots
print(p1)
print(p2)
