cat("Loading libraries...\n")

library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(gganimate)
library(viridis)

cat("Libraries loaded\n")

# ---------------------------
# LOAD DATA
# ---------------------------
cat("Loading cyclone dataset...\n")

cyclone <- read_csv("data/cleaned/cyclone_clean.csv", show_col_types = FALSE)

cat("Dataset loaded\n")

# ---------------------------
# CLEAN DATA
# ---------------------------
cyclone <- cyclone %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    wind_speed = as.numeric(gsub("[^0-9.]", "", wind_speed)),  # Remove non-numeric chars
    datetime = as.POSIXct(datetime)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(wind_speed))

cat("Data cleaned\n")

# ---------------------------
# CYCLONE PATH MAP
# ---------------------------
cat("Creating cyclone path map...\n")

world <- map_data("world")

path_map <- ggplot() +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    fill = "gray90",
    color = "gray40"
  ) +
  geom_path(
    data = cyclone,
    aes(
      longitude,
      latitude,
      group = cyclone_id,
      color = wind_speed
    ),
    linewidth = 0.6,
    alpha = 0.7
  ) +
  scale_color_viridis_c(option = "plasma") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Global Cyclone Tracks",
    x = "Longitude",
    y = "Latitude",
    color = "Wind Speed"
  )

ggsave(
  "outputs/maps/cyclone_paths.png",
  path_map,
  width = 12,
  height = 6
)

cat("Cyclone path map saved\n")

# ---------------------------
# HOTSPOT MAP
# ---------------------------
cat("Creating hotspot map...\n")

hotspot_map <- ggplot() +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    fill = "gray90",
    color = "gray40"
  ) +
  stat_density_2d(
    data = cyclone,
    aes(
      longitude,
      latitude,
      fill = after_stat(level)
    ),
    geom = "polygon",
    alpha = 0.5
  ) +
  scale_fill_viridis_c() +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Cyclone Hotspots",
    x = "Longitude",
    y = "Latitude",
    fill = "Density"
  )

ggsave(
  "outputs/maps/cyclone_hotspots.png",
  hotspot_map,
  width = 12,
  height = 6
)

cat("Hotspot map saved\n")

# ---------------------------
# ANIMATION DATA PREP
# ---------------------------
cat("Preparing animation data...\n")

cyclone_anim <- cyclone %>%
  arrange(cyclone_id, datetime) %>%
  group_by(cyclone_id) %>%
  slice(seq(1, n(), by = 5)) %>%  # Reduce number of points for animation speed
  ungroup()

# ---------------------------
# CYCLONE ANIMATION
# ---------------------------
cat("Creating cyclone animation...\n")

anim_plot <- ggplot() +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    fill = "gray90",
    color = "gray40"
  ) +
  geom_path(
    data = cyclone_anim,
    aes(
      longitude,
      latitude,
      group = cyclone_id
    ),
    color = "blue",
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_point(
    data = cyclone_anim,
    aes(
      longitude,
      latitude,
      group = cyclone_id
    ),
    color = "red",
    size = 2
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Cyclone Movement Across the World",
    subtitle = "Time: {frame_along}",
    x = "Longitude",
    y = "Latitude"
  ) +
  transition_reveal(datetime)

animate(
  anim_plot,
  width = 1000,
  height = 600,
  fps = 12,
  duration = 20,
  renderer = gifski_renderer("outputs/maps/cyclone_animation.gif")
)

cat("Cyclone animation saved\n")
cat("All visualizations completed successfully\n")
