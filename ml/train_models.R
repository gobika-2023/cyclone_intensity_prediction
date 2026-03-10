# ================================
# Cyclone Intensity Prediction
# Member 3 - ML Models (FINAL VERSION)
# ================================

library(randomForest)
library(e1071)
library(Metrics)
library(tidyverse)

cat("Starting Model Training...\n")

# -------------------------------
# 1. Load Dataset
# -------------------------------

data <- read.csv("data/clean_dataset.csv")

# Ensure intensity exists
if(!"intensity" %in% colnames(data)){
  stop("Target column 'intensity' not found!")
}

data$intensity <- as.numeric(data$intensity)

# Keep only numeric columns
data <- data[, sapply(data, is.numeric)]

# Remove zero variance columns
data <- data[, apply(data, 2, sd) > 0]

cat("Initial Dataset Shape:\n")
print(dim(data))

# -------------------------------
# 2. Feature Reduction (Top 50 by Variance)
# -------------------------------

# Compute variance ONLY on features (exclude target)
feature_vars <- apply(data[, colnames(data) != "intensity"], 2, var)

top_features <- names(sort(feature_vars, decreasing = TRUE))[1:50]

# Keep selected features + target
data <- data[, c(top_features, "intensity")]

cat("Dataset shape after feature reduction:\n")
print(dim(data))

# -------------------------------
# 3. Train-Test Split (80/20)
# -------------------------------

set.seed(123)

sample_size <- floor(0.8 * nrow(data))
train_index <- sample(seq_len(nrow(data)), size = sample_size)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

# -------------------------------
# 4. Feature Scaling (ONLY FEATURES)
# -------------------------------

# Separate features and target
train_x <- train_data[, colnames(train_data) != "intensity"]
train_y <- train_data$intensity

test_x  <- test_data[, colnames(test_data) != "intensity"]
test_y  <- test_data$intensity

# Scale features
train_means <- sapply(train_x, mean)
train_sds   <- sapply(train_x, sd)

train_x_scaled <- as.data.frame(scale(train_x,
                                      center = train_means,
                                      scale = train_sds))

test_x_scaled  <- as.data.frame(scale(test_x,
                                      center = train_means,
                                      scale = train_sds))

# Add intensity back (NOT scaled)
train_data_scaled <- cbind(train_x_scaled, intensity = train_y)
test_data_scaled  <- cbind(test_x_scaled, intensity = test_y)

# -------------------------------
# 5. Linear Regression
# -------------------------------

cat("Training Linear Regression...\n")

lm_model <- lm(intensity ~ ., data = train_data)
lm_pred  <- predict(lm_model, test_data)

# -------------------------------
# 6. Random Forest
# -------------------------------

cat("Training Random Forest...\n")

rf_model <- randomForest(
  intensity ~ .,
  data = train_data,
  ntree = 100,
  mtry = floor(sqrt(ncol(train_x))),
  importance = TRUE
)

rf_pred <- predict(rf_model, test_data)

# -------------------------------
# 7. Support Vector Machine (Regression)
# -------------------------------

cat("Training SVM...\n")

svm_model <- svm(
  intensity ~ .,
  data = train_data_scaled,
  type = "eps-regression",
  kernel = "radial"
)

svm_pred <- predict(svm_model, test_data_scaled)

# -------------------------------
# 8. Model Evaluation
# -------------------------------

cat("Evaluating Models...\n")

results <- data.frame(
  Model = c("Linear Regression", "Random Forest", "SVM"),
  RMSE = c(
    rmse(test_y, lm_pred),
    rmse(test_y, rf_pred),
    rmse(test_y, svm_pred)
  ),
  MAE = c(
    mae(test_y, lm_pred),
    mae(test_y, rf_pred),
    mae(test_y, svm_pred)
  )
)

print(results)

# -------------------------------
# 9. Save Results
# -------------------------------

if(!dir.exists("ml")) dir.create("ml")

write.csv(results, "ml/model_comparison.csv", row.names = FALSE)
saveRDS(svm_model, "ml/best_model.rds")

# -------------------------------
# 10. Feature Importance Plot
# -------------------------------

png("ml/feature_importance.png", width = 1000, height = 700)
varImpPlot(rf_model)
dev.off()

cat("Training Completed Successfully!\n")