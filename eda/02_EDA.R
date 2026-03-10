<<<<<<< 
# Load libraries
library(ggplot2)
library(dplyr)

# Load cleaned datasets
cyclone <- read.csv("data/cleaned/cyclone_clean.csv")
weather <- read.csv("data/cleaned/weather_clean.csv")
rainfall <- read.csv("data/cleaned/rainfall_clean.csv")

# Create year column
cyclone$year <- substr(cyclone$datetime, 1, 4)

# Cyclone frequency
cyclone_year <- cyclone %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(cyclone_year, aes(x = year, y = count)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Cyclone Frequency by Year")

ggsave("outputs/graphs/cyclone_frequency.png")

# Wind speed distribution
cyclone$wind_speed <- as.numeric(cyclone$wind_speed)

ggplot(cyclone, aes(x = wind_speed)) +
  geom_histogram(bins = 30, fill="orange") +
  labs(title="Wind Speed Distribution")

ggsave("outputs/graphs/wind_speed_distribution.png")

# Pressure distribution
cyclone$pressure <- as.numeric(cyclone$pressure)

ggplot(cyclone, aes(x = pressure)) +
  geom_histogram(bins = 30, fill="green") +
  labs(title="Pressure Distribution")

ggsave("outputs/graphs/pressure_distribution.png")

# Rainfall distribution
rainfall$max_rainfall <- as.numeric(rainfall$max_rainfall)

ggplot(rainfall, aes(x = max_rainfall)) +
  geom_histogram(bins = 30, fill="blue") +
  labs(title="Maximum Rainfall Distribution")

ggsave("outputs/graphs/rainfall_distribution.png")

ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red") +
  labs(title="Pressure vs Cyclone Wind Speed")

ggsave("outputs/graphs/pressure_vs_wind.png")

ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red", alpha=0.3) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Pressure vs Cyclone Wind Speed")

=======
<<<<<<< HEAD
# Load libraries
library(ggplot2)
library(dplyr)

# Load cleaned datasets
cyclone <- read.csv("data/cleaned/cyclone_clean.csv")
weather <- read.csv("data/cleaned/weather_clean.csv")
rainfall <- read.csv("data/cleaned/rainfall_clean.csv")

# Create year column
cyclone$year <- substr(cyclone$datetime, 1, 4)

# Cyclone frequency
cyclone_year <- cyclone %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(cyclone_year, aes(x = year, y = count)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Cyclone Frequency by Year")

ggsave("outputs/graphs/cyclone_frequency.png")

# Wind speed distribution
cyclone$wind_speed <- as.numeric(cyclone$wind_speed)

ggplot(cyclone, aes(x = wind_speed)) +
  geom_histogram(bins = 30, fill="orange") +
  labs(title="Wind Speed Distribution")

ggsave("outputs/graphs/wind_speed_distribution.png")

# Pressure distribution
cyclone$pressure <- as.numeric(cyclone$pressure)

ggplot(cyclone, aes(x = pressure)) +
  geom_histogram(bins = 30, fill="green") +
  labs(title="Pressure Distribution")

ggsave("outputs/graphs/pressure_distribution.png")

# Rainfall distribution
rainfall$max_rainfall <- as.numeric(rainfall$max_rainfall)

ggplot(rainfall, aes(x = max_rainfall)) +
  geom_histogram(bins = 30, fill="blue") +
  labs(title="Maximum Rainfall Distribution")

ggsave("outputs/graphs/rainfall_distribution.png")


ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red") +
  labs(title="Pressure vs Cyclone Wind Speed")

# STEP: Save pressure vs wind speed plot
ggsave("outputs/graphs/pressure_vs_wind.png")

ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red", alpha=0.3) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Pressure vs Cyclone Wind Speed")

# STEP: Save pressure vs wind speed relationship plot
=======
# Load libraries
library(ggplot2)
library(dplyr)

# Load cleaned datasets
cyclone <- read.csv("data/cleaned/cyclone_clean.csv")
weather <- read.csv("data/cleaned/weather_clean.csv")
rainfall <- read.csv("data/cleaned/rainfall_clean.csv")

# Create year column
cyclone$year <- substr(cyclone$datetime, 1, 4)

# Cyclone frequency
cyclone_year <- cyclone %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(cyclone_year, aes(x = year, y = count)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Cyclone Frequency by Year")

ggsave("outputs/graphs/cyclone_frequency.png")

# Wind speed distribution
cyclone$wind_speed <- as.numeric(cyclone$wind_speed)

ggplot(cyclone, aes(x = wind_speed)) +
  geom_histogram(bins = 30, fill="orange") +
  labs(title="Wind Speed Distribution")

ggsave("outputs/graphs/wind_speed_distribution.png")

# Pressure distribution
cyclone$pressure <- as.numeric(cyclone$pressure)

ggplot(cyclone, aes(x = pressure)) +
  geom_histogram(bins = 30, fill="green") +
  labs(title="Pressure Distribution")

ggsave("outputs/graphs/pressure_distribution.png")

# Rainfall distribution
rainfall$max_rainfall <- as.numeric(rainfall$max_rainfall)

ggplot(rainfall, aes(x = max_rainfall)) +
  geom_histogram(bins = 30, fill="blue") +
  labs(title="Maximum Rainfall Distribution")

ggsave("outputs/graphs/rainfall_distribution.png")


ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red") +
  labs(title="Pressure vs Cyclone Wind Speed")

# STEP: Save pressure vs wind speed plot
ggsave("outputs/graphs/pressure_vs_wind.png")

ggplot(cyclone, aes(x = pressure, y = wind_speed)) +
  geom_point(color="red", alpha=0.3) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Pressure vs Cyclone Wind Speed")

# STEP: Save pressure vs wind speed relationship plot
>>>>>>> 89d8697b762a1f7ae3013943772c30e0643eac84
>>>>>>> Stashed changes
ggsave("outputs/graphs/pressure_vs_wind_speed.png")