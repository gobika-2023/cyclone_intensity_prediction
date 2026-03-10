# -----------------------------------------
# Cyclone Intensity Prediction
# Machine Learning Script
# -----------------------------------------

library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)

set.seed(123)

# -----------------------------------------
# 1. Load Dataset
# -----------------------------------------

data <- read.csv("D:/cyclone_intensity_prediction_in_R/data/cleaned/merged_dataset.csv")

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# -----------------------------------------
# 2. Data Cleaning
# -----------------------------------------

data <- data %>%
  select(where(is.numeric))

data <- na.omit(data)

cat("Columns after cleaning:", ncol(data), "\n")

# -----------------------------------------
# 3. Target Variable
# -----------------------------------------

target <- "wind_speed"

if(!(target %in% colnames(data))){
  stop("Target variable 'wind_speed' not found")
}

# -----------------------------------------
# 4. Train-Test Split
# -----------------------------------------

train_index <- createDataPartition(
  data$wind_speed,
  p = 0.8,
  list = FALSE
)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

# -----------------------------------------
# 5. Training Control
# -----------------------------------------

ctrl <- trainControl(
  method = "cv",
  number = 5
)

# -----------------------------------------
# 6. Train Models
# -----------------------------------------

# Linear Regression
lm_model <- train(
  wind_speed ~ .,
  data = train_data,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

# Decision Tree
dt_model <- train(
  wind_speed ~ .,
  data = train_data,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 5
)

# Random Forest
rf_model <- train(
  wind_speed ~ .,
  data = train_data,
  method = "rf",
  ntree = 80,
  tuneLength = 2,
  trControl = ctrl,
  importance = TRUE
)

# Gradient Boosting
gbm_model <- train(
  wind_speed ~ .,
  data = train_data,
  method = "gbm",
  trControl = ctrl,
  verbose = FALSE,
  tuneLength = 5
)

# -----------------------------------------
# 7. Train SVM on Sample (to avoid freezing)
# -----------------------------------------

svm_train_data <- train_data %>%
  sample_n(min(10000, nrow(train_data)))

svm_model <- train(
  wind_speed ~ .,
  data = svm_train_data,
  method = "svmRadial",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 3
)

cat("All models trained successfully\n")

# -----------------------------------------
# 8. Predictions
# -----------------------------------------

lm_pred  <- predict(lm_model, test_data)
dt_pred  <- predict(dt_model, test_data)
rf_pred  <- predict(rf_model, test_data)
gbm_pred <- predict(gbm_model, test_data)
svm_pred <- predict(svm_model, test_data)

# -----------------------------------------
# 9. Evaluation Function
# -----------------------------------------

evaluate_model <- function(actual, predicted){
  
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- cor(actual, predicted)^2
  
  # Avoid division by zero
  actual_nonzero <- ifelse(actual == 0, NA, actual)
  
  mape <- mean(abs((actual - predicted)/actual_nonzero), na.rm = TRUE)*100
  
  return(c(RMSE = rmse, MAE = mae, R2 = r2, MAPE = mape))
}

# -----------------------------------------
# 10. Evaluate Models
# -----------------------------------------

lm_results  <- evaluate_model(test_data$wind_speed, lm_pred)
dt_results  <- evaluate_model(test_data$wind_speed, dt_pred)
rf_results  <- evaluate_model(test_data$wind_speed, rf_pred)
gbm_results <- evaluate_model(test_data$wind_speed, gbm_pred)
svm_results <- evaluate_model(test_data$wind_speed, svm_pred)

# -----------------------------------------
# 11. Model Comparison
# -----------------------------------------

results <- rbind(
  Linear_Regression = lm_results,
  Decision_Tree = dt_results,
  Random_Forest = rf_results,
  Gradient_Boosting = gbm_results,
  Support_Vector_Regression = svm_results
)

results <- as.data.frame(results)

print(results)

# -----------------------------------------
# 12. Save Results
# -----------------------------------------

if(!dir.exists("model_results")){
  dir.create("model_results")
}

write.csv(
  results,
  "model_results/model_comparison.csv",
  row.names = TRUE
)

cat("Model comparison saved\n")

# -----------------------------------------
# 13. Feature Importance
# -----------------------------------------

importance <- varImp(rf_model)

png(
  "model_results/feature_importance.png",
  width = 800,
  height = 600
)

plot(
  importance,
  main = "Feature Importance - Random Forest"
)

dev.off()

cat("Feature importance plot saved\n")

# -----------------------------------------
# 14. Find Best Model
# -----------------------------------------

best_model_name <- rownames(results)[which.min(results$RMSE)]

cat("Best Model Based on RMSE:", best_model_name, "\n")

# -----------------------------------------
# 15. Save Best Model
# -----------------------------------------

if(!dir.exists("models")){
  dir.create("models")
}

best_model <- switch(
  best_model_name,
  
  Linear_Regression = lm_model,
  Decision_Tree = dt_model,
  Random_Forest = rf_model,
  Gradient_Boosting = gbm_model,
  Support_Vector_Regression = svm_model
)

saveRDS(
  best_model,
  paste0("models/best_model_", best_model_name, ".rds")
)

cat("Best model saved successfully\n")