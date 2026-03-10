# ==========================================================
# Cyclone Intensity Prediction Project
# Preprocessing Script
# ==========================================================

# -------------------------------
# 1. Load Required Libraries
# -------------------------------
library(dplyr)
library(readr)
library(caret)

cat("Libraries Loaded Successfully\n\n")

# -------------------------------
# 2. Load Dataset
# -------------------------------
data_path <- "data/final_dataset.csv"

cyclone_data <- read_csv(data_path, show_col_types = FALSE)

cat("Dataset Loaded Successfully\n")
cat("Rows:", nrow(cyclone_data), "\n")
cat("Columns:", ncol(cyclone_data), "\n\n")

# -------------------------------
# 3. Rename Target Column
# -------------------------------
# Assuming last column is cyclone intensity
colnames(cyclone_data)[ncol(cyclone_data)] <- "intensity"

cat("Target column renamed to 'intensity'\n\n")

# -------------------------------
# 4. Check Missing Values
# -------------------------------
cat("Checking Missing Values...\n")
missing_values <- colSums(is.na(cyclone_data))
print(missing_values)
cat("\n")

# -------------------------------
# 5. Remove Missing Values (if any)
# -------------------------------
cyclone_data <- na.omit(cyclone_data)

cat("After Removing NA:\n")
cat("Rows:", nrow(cyclone_data), "\n\n")

# -------------------------------
# 6. Convert Character Columns to Factor
# -------------------------------
cyclone_data <- cyclone_data %>%
  mutate(across(where(is.character), as.factor))

cat("Categorical Variables Converted to Factor\n\n")

# -------------------------------
# 7. Feature Scaling (ONLY Features)
# -------------------------------

# Identify numeric columns
numeric_cols <- sapply(cyclone_data, is.numeric)

# Exclude target column from scaling
numeric_cols["intensity"] <- FALSE

# Apply standardization (center + scale)
preProcValues <- preProcess(
  cyclone_data[, numeric_cols],
  method = c("center", "scale")
)

cyclone_data[, numeric_cols] <- predict(
  preProcValues,
  cyclone_data[, numeric_cols]
)

cat("Feature Scaling Completed (Target NOT Scaled)\n\n")

# -------------------------------
# 8. Final Dataset Summary
# -------------------------------
cat("Final Dataset Summary (Target Variable):\n")
print(summary(cyclone_data$intensity))
cat("\n")

# -------------------------------
# 9. Save Clean Dataset
# -------------------------------
write_csv(cyclone_data, "data/clean_dataset.csv")

cat("Clean Dataset Saved Successfully as clean_dataset.csv\n")
cat("Preprocessing Completed Successfully!\n")