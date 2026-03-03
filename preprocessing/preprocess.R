# ==========================================================
# Cyclone Intensity Prediction Project
# Preprocessing Script
# Member 1
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

cyclone_data <- read_csv(data_path)

cat("Dataset Loaded Successfully\n")
cat("Rows:", nrow(cyclone_data), "\n")
cat("Columns:", ncol(cyclone_data), "\n\n")

# -------------------------------
# 3. Check Missing Values
# -------------------------------
cat("Checking Missing Values...\n")
print(colSums(is.na(cyclone_data)))
cat("\n")

# -------------------------------
# 4. Remove Missing Values (if any)
# -------------------------------
cyclone_data <- na.omit(cyclone_data)

cat("After Removing NA:\n")
cat("Rows:", nrow(cyclone_data), "\n\n")

# -------------------------------
# 5. Convert Character Columns to Factor
# -------------------------------
cyclone_data <- cyclone_data %>%
  mutate(across(where(is.character), as.factor))

cat("Categorical Variables Converted to Factor\n\n")

# -------------------------------
# 6. Feature Scaling (Normalize Numeric Columns)
# -------------------------------

# Identify numeric columns
numeric_cols <- sapply(cyclone_data, is.numeric)

# Exclude target column if named "intensity"
# (Modify if your target column name is different)
if("intensity" %in% colnames(cyclone_data)){
  numeric_cols["intensity"] <- FALSE
}

preProcValues <- preProcess(cyclone_data[, numeric_cols], 
                            method = c("center", "scale"))

cyclone_data[, numeric_cols] <- predict(preProcValues, 
                                        cyclone_data[, numeric_cols])

cat("Feature Scaling Completed\n\n")

# -------------------------------
# 7. Save Clean Dataset
# -------------------------------
write_csv(cyclone_data, "data/clean_dataset.csv")

cat("Clean Dataset Saved Successfully as clean_dataset.csv\n")
cat("Preprocessing Completed!\n")
