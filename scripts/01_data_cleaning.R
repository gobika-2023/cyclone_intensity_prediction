# ==============================
# 01_data_cleaning.R
# Cyclone Project - Data Cleaning
# ==============================

# Load libraries
library(data.table)
library(dplyr)
library(tidyr)

# Ensure cleaned folder exists
dir.create("../data/cleaned", showWarnings = FALSE, recursive = TRUE)

# ------------------------------
# 1️⃣ Load Raw Datasets
# ------------------------------
cyclone <- fread("../data/raw/ibtracs.csv")
weather <- fread("../data/raw/weather.csv")
rainfall <- fread("../data/raw/rainfall.csv")

# ------------------------------
# 2️⃣ Clean Cyclone Dataset
# ------------------------------
cyclone_clean <- cyclone %>%
  select(
    SID, NAME, ISO_TIME, LAT, LON,
    USA_WIND, USA_PRES
  ) %>%
  rename(
    cyclone_id = SID,
    cyclone_name = NAME,
    datetime = ISO_TIME,
    latitude = LAT,
    longitude = LON,
    wind_speed = USA_WIND,
    pressure = USA_PRES
  ) %>%
  drop_na(latitude, longitude)

# Save cleaned cyclone dataset
fwrite(cyclone_clean, "../data/cleaned/cyclone_clean.csv")

# ------------------------------
# ------------------------------
# 3️⃣ Clean Weather Dataset (Fixed)
# ------------------------------

library(lubridate)
library(dplyr)

# Standardize column names
colnames(weather) <- gsub("-", ".", colnames(weather))
colnames(weather) <- gsub(" ", "_", colnames(weather))

# Columns we want
weather_cols <- c(
  "latitude", "longitude", "last_updated",
  "feels_like_celsius", "feels_like_fahrenheit", "visibility_km",
  "uv_index", "gust_kph",
  "air_quality_Carbon_Monoxide", "air_quality_Ozone",
  "air_quality_Nitrogen_dioxide", "air_quality_Sulphur_dioxide",
  "air_quality_PM2.5", "air_quality_PM10",
  "air_quality_us.epa.index", "air_quality_gb.defra.index"
)

# Keep only existing columns
weather_cols <- intersect(weather_cols, colnames(weather))

weather_clean <- weather %>%
  select(all_of(weather_cols)) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    datetime = parse_date_time(last_updated, orders = c("Y-m-d H:M", "Y-m-d H:M:S"))
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))  # only drop rows with invalid coordinates

# Save cleaned weather dataset
fwrite(weather_clean, "../data/cleaned/weather_clean.csv")

# ------------------------------
# 4️⃣ Clean Rainfall Dataset
# ------------------------------
rainfall_clean <- rainfall %>%
  select(
    SID, NAME, ISO_TIME, LAT, LON, max_precip, area_avg_TCP
  ) %>%
  rename(
    cyclone_id = SID,
    cyclone_name = NAME,
    datetime = ISO_TIME,
    latitude = LAT,
    longitude = LON,
    max_rainfall = max_precip,
    avg_rainfall = area_avg_TCP
  ) %>%
  drop_na(latitude, longitude)

# Save cleaned rainfall dataset
fwrite(rainfall_clean, "../data/cleaned/rainfall_clean.csv")

# ------------------------------
# ✅ Finished
# ------------------------------
cat("✅ Data cleaning completed!\n")
cat("Cleaned files saved to ../data/cleaned/\n")