library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(maps)

cyclone <- read.csv("data/ibtracs.ALL.list.v04r01 (1).csv")

weather <- read.csv("data/GlobalWeatherRepository.csv")

rainfall <- read.csv("data/mstcp_main_dataset.csv")


head(cyclone)
head(weather)
head(rainfall)


colnames(cyclone)



cyclone_year <- cyclone %>%
  group_by(SEASON) %>%
  summarise(count = n())

ggplot(cyclone_year, aes(x = SEASON, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cyclone Frequency Per Year",
       x = "Year",
       y = "Number of Cyclones")

ggsave("outputs/cyclone_frequency.png")

cyclone$WMO_WIND <- as.numeric(cyclone$WMO_WIND)

ggplot(cyclone, aes(x = WMO_WIND)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Wind Speed Distribution of Cyclones",
       x = "Wind Speed",
       y = "Frequency")

ggsave("outputs/wind_speed_distribution.png")

rainfall$RA_over_threshold <- as.numeric(rainfall$RA_over_threshold)

ggplot(rainfall, aes(x = RA_over_threshold)) +
  geom_histogram(fill = "blue", bins = 30) +
  labs(title = "Rainfall Distribution During Cyclones",
       x = "Rainfall Amount",
       y = "Frequency")

ggsave("outputs/rainfall_distribution.png")


cyclone$LAT <- as.numeric(cyclone$LAT)
cyclone$LON <- as.numeric(cyclone$LON)




world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgray",
               color = "white") +
  geom_path(data = cyclone,
            aes(x = LON, y = LAT, group = SID),
            color = "red", alpha = 0.3) +
  labs(title = "Global Cyclone Paths",
       x = "Longitude",
       y = "Latitude")


ggsave("outputs/cyclone_paths_map.png")

