# Load dataset
data <- read.csv("data/clean_dataset.csv")

# Select numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Correlation matrix
cor_matrix <- cor(numeric_data)

# Load library
library(corrplot)

# Heatmap
corrplot(cor_matrix,
         method="color",
         type="upper",
         tl.cex=0.6)
