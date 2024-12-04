# install.packages(c("sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))

# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

publication_theme <- theme_classic() +
  theme(
    text = element_text(family = "sans", size = 12),  # Use sans-serif font
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Load the dataset (replace 'data-export.csv' with your actual file path)
data <- read_csv("data-export.csv", skip = 9) %>%
  select(Country, `Active users`) %>%
  rename(active_users = `Active users`) %>%
  filter(!is.na(active_users)) %>%
  mutate(Country = case_when(
    Country == "United States" ~ "USA",
    Country == "United Kingdom" ~ "UK",
    Country == "Réunion" ~ "Reunion",
    Country == "St. Helena" ~ "Saint Helena",
    # Add other corrections as needed
    TRUE ~ Country))

# Load world map data
world_centerpoints <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr:: mutate(name = case_when(
    name == "United States of America" ~ "USA",
    name == "United Kingdom" ~ "UK",
    TRUE ~ name))

plot(world_centerpoints[1])

unmatched <- data %>%
  filter(!Country %in% world_centerpoints$name)

world <- ggplot2::map_data("world") %>%
  dplyr::mutate(region = case_when(
    subregion == "Hong Kong" ~ "Hong Kong",
    # Add other corrections as needed
    TRUE ~ region))

# Check for unmatched country names
unmatched <- data %>%
  filter(!Country %in% world$region)

unmatched


# Match country names and merge data
world_data <- world %>%
  left_join(data, by = c("region" = "Country"))

# Prepare data for labels (mean longitude and latitude for each region)
label_data <- world_centerpoints %>%
  left_join(data, by = c("name" = "Country")) %>%
  filter(!is.na(active_users))

# Plot the map
ggplot() +
  # Add world polygons with gradient fill
  geom_polygon(data = world_data,
               aes(long, lat, group = group, fill = active_users), 
               # color = "black",
               size = 0.01) +  # Thin black outlines
  
  # Add labels at country centroids
  geom_text(data = label_data,
            aes(label = active_users, geometry = geometry),
            stat = "sf_coordinates", size = 3, color = "black", fontface = "bold") +  # Bigger text
  
  # Gradient fill from light blue to dark blue
  scale_fill_gradient(low = "#A7BCE0", high = "#263F6B", na.value = "grey", name = "Number of Users") +
  
  
  # Maintain aspect ratio
  coord_fixed(ratio = 1) + 
  labs(
    title = "CheckEM users",
    x = "Longitude",
    y = "Latitude"
  ) +
  publication_theme
  