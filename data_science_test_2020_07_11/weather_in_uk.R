# Virtual environment -----------------------------------------------------
# install.packages(renv)
# renv::init()
# renv::snapshot()
# renv::restore()


# Bug ---------------------------------------------------------------------
# The file may not source because of a special symbol on row 199 (Wind).


# ABOUT -------------------------------------------------------------------
# The visualisation shows features available in the weather data.
# Due to the size of the data, I have decided to focus on the UK –
# other countries are available, as well as focusing on a town level
# instead of a country level. This allows producing simple static
# visualisation showing the temperature, pressure, humidity,
# wind speed with direction, and clouds over the UK.

# Pros
# + Shows a detailed overview of the UK;
# + Allows the user comparison between different features of weather data;
# + Simple and understandable;
# + Fast to run (download time-dependent).
#
# Cons
# - Does not show other regions;
# - Does not show progress over time (an only specified time);
# - Not interactive.

# Packages ----------------------------------------------------------------
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)

library(tidyverse)
packageVersion("tidyverse") # ‘1.3.0’
library(jsonlite)
packageVersion("jsonlite") # ‘1.7.0’
library(janitor)
packageVersion("janitor") # ‘2.0.1’
library(maps)
packageVersion("maps") # ‘3.3.0’
library(showtext)
packageVersion("showtext") # ‘0.8.1’
library(viridis)
packageVersion("viridis") # ‘0.5.1’
library(patchwork)
packageVersion("patchwork") # ‘1.0.1’
library(Cairo)
packageVersion("Cairo") # ‘1.5.12.2’
library(ggrepel)
packageVersion("ggrepel") # ‘0.8.2’
library(gganimate)
packageVersion("gganimate") # ‘1.0.6’

# DATA_read ---------------------------------------------------------------
# The following code reads data directly from the URL
# I can't guarantee the source remains.

# url <- "https://www.whiteswandata.com/s/weatherjson.gz"
# raw_df <- stream_in(con = gzcon(con = url(url)), verbose = TRUE)

# It is a little download it every time, so I saved it in data folder as rds.
# saveRDS(object = raw_df, file = "data/weather.rds")

raw_df <- readRDS(file = "data/weather.rds")

weather_only <- raw_df %>%
  flatten() %>%
  select(city.id, weather) %>%
  unnest(cols = c(weather)) %>%
  select(city.id, main)

flat_df <- raw_df %>%
  flatten() %>%
  left_join(weather_only, by = "city.id") %>%
  select(-weather)

# DATA_coerce -------------------------------------------------------------
# The following prepares parsed data for visualisation
df <- flat_df %>%
  relocate(c(time, main), .after = last_col()) %>%
  as_tibble() %>%
  clean_names()

# map_world <- map_data("world") %>%
#         filter(region != "Antarctica")
#
# map_weather <- df %>%
#         filter(city_name != "Antarctica")

map_uk <- map_data("world") %>% # Available from {maps} package
  filter(region == "UK")

map_uk_weather <- df %>%
  filter(city_country == "GB") %>%
  filter(!str_detect(city_name, "Akrotiri|Kolossi|Xylotymbou|Pergamos"))


# This can be commented or uncommented to skip the parsing and load local data
# if (!dir.exists("data")) {dir.create(path = "data")} # ensure "/data" folder is in project
# saveRDS(df, file = "data/weather.rds")
# df <- readRDS(file = "data/weather.rds")

# Explore -----------------------------------------------------------------

# Optional basic data exploration.
# Uncomment as needed.

# glimpse(df)
# df %>% count(city_country, sort = TRUE) %>%
#         slice_max(n, n = 50)
#
# df %>% count(city_name, sort = TRUE) %>%
#         slice_max(n, n = 10)
#
# df %>% count(city_zoom, sort = TRUE)
#
# df %>% summarise(
#         avg_temp = mean(main_temp),
#         min_temp = min(main_temp),
#         max_temp = max(main_temp),
#         avg_pressure = mean(main_pressure),
#         min_pressure = min(main_pressure),
#         max_pressure = max(main_pressure),
#         avg_humidity = mean(main_humidity),
#         min_humidity = min(main_humidity),
#         max_humidity = max(main_humidity),
#         avg_wind_speed = mean(wind_speed),
#         min_wind_speed = min(wind_speed),
#         max_wind_speed = max(wind_speed),
#         avg_clouds_all = mean(clouds_all),
#         min_clouds_all = min(clouds_all),
#         max_clouds_all = max(clouds_all)
#         ) %>%
#         pivot_longer(cols = everything())

# Theme -------------------------------------------------------------------

# Prepare font
# Source: https://fonts.google.com/featured/Superfamilies
font_add_google("Nunito", "Nunito")
font_families()
showtext_auto()

# Prepare theme
theme_map_custom <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Nunito", color = "#000000"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      strip.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# Map visualisations ------------------------------------------------------
# The following visualises 6 variables from the dataset.
# The focus is on the UK.
# Data are also zoomed to city level 16.
# Time of weather is 1554462361:1554462365.


# Wind --------------------------------------------------------------------
ggwind <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_text(
    data = map_uk_weather %>%
      filter(city_zoom == 16) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = wind_speed, size = wind_speed,
      angle = wind_deg, alpha = wind_speed
    ), label = "→" # https://stackoverflow.com/a/42754519/3223143, causes issues when sourcing?
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Wind"
  ) +
  guides(
    colour = guide_legend("Speed &\ndirection"), size = guide_legend("Speed &\ndirection"),
    alpha = guide_legend("Speed &\ndirection")
  )

# Temp --------------------------------------------------------------------
ggtemp <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 16) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = main_temp, fill = main_temp, size = main_temp
    ),
    shape = 18, alpha = 0.2
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  scale_fill_viridis(
    option = "magma", end = 0.8,
    name = "Level",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, units = "mm"),
      barwidth = unit(2, units = "mm"),
      draw.ulim = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1,
      label.hjust = 0.5
    )
  ) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Temperature"
  ) +
  guides(colour = "none", size = "none")

# Pressure ----------------------------------------------------------------
ggpressure <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 16) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = main_pressure, fill = main_pressure,
      size = main_pressure
    ),
    shape = 22, alpha = 0.2
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  scale_fill_viridis(
    option = "magma", end = 0.8,
    name = "Level",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, units = "mm"),
      barwidth = unit(2, units = "mm"),
      draw.ulim = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1,
      label.hjust = 0.5
    )
  ) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Pressure"
  ) +
  guides(colour = "none", size = "none")

# Humidity ----------------------------------------------------------------
gghumid <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 16) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = main_humidity, fill = main_humidity,
      size = main_humidity
    ),
    shape = 17, alpha = 0.2
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  scale_fill_viridis(
    option = "magma", end = 0.8,
    name = "Level",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, units = "mm"),
      barwidth = unit(2, units = "mm"),
      draw.ulim = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1,
      label.hjust = 0.5
    )
  ) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Humidity"
  ) +
  guides(colour = "none", size = "none")

# Clouds ------------------------------------------------------------------
ggclouds <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.9
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.2
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 16) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = clouds_all, fill = clouds_all,
      size = clouds_all
    ),
    shape = 16, alpha = 0.2
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  scale_fill_viridis(
    option = "magma", end = 0.8,
    name = "Density",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, units = "mm"),
      barwidth = unit(2, units = "mm"),
      draw.ulim = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1,
      label.hjust = 0.5
    )
  ) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Clouds"
  ) +
  guides(colour = "none", size = "none")

# Weather -----------------------------------------------------------------
ggweather <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 7) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(x = city_coord_lon, y = city_coord_lat),
    size = 2, colour = "black"
  ) +
  geom_text_repel(
    data = map_uk_weather %>%
      filter(city_zoom == 7) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      label = main
    )
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  coord_map() +
  theme_map_custom() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Overall weather"
  )

# Patch & Save maps together ----------------------------------------------
CairoPNG(filename = "weather_output_uk.png", width = 1200, height = 1200)
gghumid + ggpressure + ggtemp + ggwind + ggclouds + ggweather +
  plot_annotation(
    title = "Weather data available for the United Kingdom",
    caption = "Time: 1554462361 to 1554462365; Zoom: 7 to 16\nVisualisation by: Martin Cadek"
  ) &
  theme(text = element_text("Nunito"))
dev.off()




# Animate -----------------------------------------------------------------

# Ani Weather -------------------------------------------------------------
ggweather_ani <- ggplot() + geom_polygon(
  data = map_uk, aes(x = long, y = lat, group = group),
  fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 7),
    aes(x = city_coord_lon, y = city_coord_lat),
    size = 2, colour = "black"
  ) +
  geom_text_repel(
    data = map_uk_weather %>%
      filter(city_zoom == 7) %>%
      filter(time %in% (1554462361:1554462365)),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      label = main
    )
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  coord_map() +
  theme_map_custom() +
  # Adding animation is as simple as using the following three lines.
  # Note that time now is not filtered upon but used in the animation below.
  labs(title = "Time: {frame_time}", subtitle = "Overall weather in the UK") +
  transition_time(time) +
  ease_aes("linear")


# Ani Temperature ---------------------------------------------------------
ggtemp_ani <- ggplot() +
  geom_polygon(
    data = map_uk, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.8
  ) +
  geom_path(
    data = map_uk, aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white", size = 0.1
  ) +
  geom_point(
    data = map_uk_weather %>%
      filter(city_zoom == 16),
    aes(
      x = city_coord_lon, y = city_coord_lat,
      colour = main_temp, fill = main_temp, size = main_temp
    ),
    shape = 18, alpha = 0.2
  ) +
  scale_colour_viridis(option = "magma", end = 0.8) +
  scale_fill_viridis(
    option = "magma", end = 0.8,
    name = "Level",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, units = "mm"),
      barwidth = unit(2, units = "mm"),
      draw.ulim = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1,
      label.hjust = 0.5
    )
  ) +
  coord_map() +
  theme_map_custom() +
  guides(colour = "none", size = "none") +
  labs(title = "Time: {frame_time}", subtitle = "Temperature in the UK") +
  transition_time(time) +
  ease_aes("linear")



# Save animation ----------------------------------------------------------
anim_save(filename = "weather_animated.gif", animation = ggweather_ani)
anim_save(filename = "temperature_animated.gif", animation = ggtemp_ani)
# Will throw warnings about the font.

# Note: It would be pretty to combine these simple animations. There's a way 
# look at https://github.com/dariyasydykova/open_projects/blob/master/ROC_animation/R/animate_PR.r
# It relies on magick package to stitch together separate png files to reconstruct
# the animation.