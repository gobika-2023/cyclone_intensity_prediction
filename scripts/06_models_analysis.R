library(ggplot2)

# Load model
best_model <- readRDS("models/best_model_Random_Forest.rds")

# Load dataset
data <- read.csv("data/cleaned/merged_dataset.csv")

# Select only columns used in training
data_model <- data[, c(
  "wind_speed",
  "latitude",
  "longitude",
  "pressure",
  "max_rainfall",
  "avg_rainfall"
)]

# Remove rows with missing values
data_model <- na.omit(data_model)

# Predict
pred <- predict(best_model, data_model)

# Create plotting dataframe
plot_data <- data.frame(
  actual = data_model$wind_speed,
  pred = pred
)

# Create plot
p <- ggplot(plot_data, aes(x = actual, y = pred)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(
    title = "Actual vs Predicted Cyclone Wind Speed",
    x = "Actual Wind Speed",
    y = "Predicted Wind Speed"
  )

# Print plot
dev.new()
print(p)

# Save plot
ggsave(
  "model_results/actual_vs_predicted.png",
  plot = p,
  width = 8,
  height = 6
)