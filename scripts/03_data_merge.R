# ============================================
# 03_data_merge.R
# Merge cyclone, rainfall, and weather datasets
# ============================================

library(dplyr)
library(lubridate)

# -------------------------------
# Step 1: Read cleaned CSVs
# -------------------------------
cyclone <- read.csv("../data/cleaned/cyclone_clean.csv", stringsAsFactors = FALSE)
rainfall <- read.csv("../data/cleaned/rainfall_clean.csv", stringsAsFactors = FALSE)
weather <- read.csv("../data/cleaned/weather_clean.csv", stringsAsFactors = FALSE)

# -------------------------------
# Step 2: Convert numeric safely
# -------------------------------
safe_numeric <- function(x) {
  as.numeric(gsub(",", "", x))
}

# Cyclone
cyclone <- cyclone %>%
  mutate(
    latitude = safe_numeric(latitude),
    longitude = safe_numeric(longitude),
    wind_speed = safe_numeric(wind_speed),
    pressure = safe_numeric(pressure),
    datetime = ymd_hms(datetime)
  )

# Rainfall
rainfall <- rainfall %>%
  mutate(
    latitude = safe_numeric(latitude),
    longitude = safe_numeric(longitude),
    max_rainfall = safe_numeric(max_rainfall),
    avg_rainfall = safe_numeric(avg_rainfall),
    datetime = ymd_hms(datetime)
  )

# Weather
weather <- weather %>%
  mutate(
    latitude = safe_numeric(latitude),
    longitude = safe_numeric(longitude),
    feels_like_celsius = safe_numeric(feels_like_celsius),
    feels_like_fahrenheit = safe_numeric(feels_like_fahrenheit),
    visibility_km = safe_numeric(visibility_km),
    uv_index = safe_numeric(uv_index),
    gust_kph = safe_numeric(gust_kph),
    datetime = ymd_hms(datetime)
  )

# -------------------------------
# Step 3: Merge datasets
# -------------------------------

# Merge cyclone + rainfall on cyclone_id, cyclone_name, datetime, latitude, longitude
cyclone_rainfall <- left_join(
  cyclone,
  rainfall,
  by = c("cyclone_id", "cyclone_name", "datetime", "latitude", "longitude")
)

# Merge with weather on latitude, longitude, and datetime
merged_dataset <- left_join(
  cyclone_rainfall,
  weather,
  by = c("latitude", "longitude", "datetime")
)

# -------------------------------
# Step 4: Save merged dataset
# -------------------------------
output_path <- "../data/cleaned/merged_dataset.csv"
write.csv(merged_dataset, output_path, row.names = FALSE)

cat("✅ Merged dataset saved to:", output_path, "\n")