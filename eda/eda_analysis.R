# Load dataset
data <- read.csv("data/clean_dataset.csv")

# Year-wise cyclone count
year_count <- table(data$Year)

# Trend graph
plot(year_count,
     type="b",
     col="blue",
     main="Year-wise Cyclone Trend",
     xlab="Year",
     ylab="Number of Cyclones")

# Boxplot
boxplot(data$Feature1,
        main="Boxplot of Feature1",
        col="orange")

# Histogram
hist(data$Feature1,
     main="Distribution of Feature1",
     col="green",
     xlab="Feature1")
# Correlation matrix for important features
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Important Features based on Feature1
important_features <- names(
  sort(abs(cor_matrix["Feature1", ]), decreasing = TRUE)
)[1:5]

important_features
