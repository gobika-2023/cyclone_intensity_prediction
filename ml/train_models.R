# ================================
# Cyclone Intensity Prediction
# Member 3 - ML Models
# ================================

library(randomForest)
library(e1071)
library(Metrics)
library(tidyverse)

cat("Starting Model Training...\n")

# -------------------------------
# 1. Load dataset
# -------------------------------

data <- read.csv("data/clean_dataset.csv")

# Rename LAST column as intensity
colnames(data)[ncol(data)] <- "intensity"

# Ensure intensity is numeric
data$intensity <- as.numeric(data$intensity)

# Keep only numeric columns
data <- data[, sapply(data, is.numeric)]

# Remove zero variance columns
data <- data[, apply(data, 2, sd) > 0]

# -------------------------------
# 2. Reduce Features (IMPORTANT)
# -------------------------------

# Keep top 50 most varying features
variances <- apply(data, 2, var)
top_features <- names(sort(variances, decreasing = TRUE))[1:50]

data <- data[, unique(c(top_features, "intensity"))]

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
# 4. Feature Scaling (for SVM)
# -------------------------------

train_means <- sapply(train_data, mean)
train_sds   <- sapply(train_data, sd)

train_data_scaled <- as.data.frame(scale(train_data, center = train_means, scale = train_sds))
test_data_scaled  <- as.data.frame(scale(test_data, center = train_means, scale = train_sds))

# -------------------------------
# 5. Linear Regression
# -------------------------------

cat("Training Linear Regression...\n")

lm_model <- lm(intensity ~ ., data = train_data)
lm_pred  <- predict(lm_model, test_data)

# -------------------------------
# 6. Random Forest (Optimized)
# -------------------------------

cat("Training Random Forest...\n")

rf_model <- randomForest(
  intensity ~ .,
  data = train_data,
  ntree = 50,
  mtry = 5,
  importance = TRUE
)

rf_pred <- predict(rf_model, test_data)

# -------------------------------
# 7. Support Vector Machine
# -------------------------------

cat("Training SVM...\n")

svm_model <- svm(intensity ~ ., data = train_data_scaled)
svm_pred  <- predict(svm_model, test_data_scaled)

# -------------------------------
# 8. Model Evaluation
# -------------------------------

cat("Evaluating Models...\n")

results <- data.frame(
  Model = c("Linear Regression", "Random Forest", "SVM"),
  RMSE = c(
    rmse(test_data$intensity, lm_pred),
    rmse(test_data$intensity, rf_pred),
    rmse(test_data$intensity, svm_pred)
  ),
  MAE = c(
    mae(test_data$intensity, lm_pred),
    mae(test_data$intensity, rf_pred),
    mae(test_data$intensity, svm_pred)
  )
)
r2_score <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

results <- data.frame(
  Model = c("Linear Regression", "Random Forest", "SVM"),
  RMSE = c(
    rmse(test_data$intensity, lm_pred),
    rmse(test_data$intensity, rf_pred),
    rmse(test_data$intensity, svm_pred)
  ),
  MAE = c(
    mae(test_data$intensity, lm_pred),
    mae(test_data$intensity, rf_pred),
    mae(test_data$intensity, svm_pred)
  ),
  R2 = c(
    r2_score(test_data$intensity, lm_pred),
    r2_score(test_data$intensity, rf_pred),
    r2_score(test_data$intensity, svm_pred)
  )
)

print(results)

print(results)

# -------------------------------
# 9. Save Results
# -------------------------------

write.csv(results, "ml/model_comparison.csv", row.names = FALSE)
saveRDS(svm_model, "ml/best_model.rds")

# -------------------------------
# 10. Feature Importance Plot
# -------------------------------

png("ml/feature_importance.png", width = 1000, height = 700)
varImpPlot(rf_model)
dev.off()

cat("Training Completed Successfully!\n")