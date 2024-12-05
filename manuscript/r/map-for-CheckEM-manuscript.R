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
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Load the dataset (replace 'data-export.csv' with your actual file path)
data <- read_csv("manuscript/data/data-export.csv", skip = 9) %>%
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
  left_join(data, by = c("region" = "Country")) %>%
  dplyr::filter(!region %in% "Antarctica")

# Prepare data for labels (mean longitude and latitude for each region)
label_data <- world_centerpoints %>%
  left_join(data, by = c("name" = "Country")) %>%
  filter(!is.na(active_users))

# Plot the map
map <- ggplot() +
  # Add world polygons with gradient fill
  # geom_sf(data = world_data_sf,
  #              aes(fill = active_users), 
  #              # color = "black",
  #              size = 0.01) +  # Thin black outlines
  
  geom_polygon(data = world_data,
               aes(long, lat, group = group, fill = active_users),
               color = "white",
               size = 0.35) +  # Thin black outlines
  
  # Add labels at country centroids
  geom_text(data = label_data,
            aes(label = active_users, geometry = geometry),
            stat = "sf_coordinates", size = 4, color = "black", fontface = "bold",
            family = "Times New Roman") +  # Bigger text
  
  # Gradient fill from light blue to dark blue
  scale_fill_gradient(low = "#A7BCE0", 
                      high = "#335591", 
                      na.value = "grey", 
                      name = "Number of Users",
                      breaks = c(1, 150, 300, 450, 600),  # Specify breaks
                      labels = c("1", "150", "300", "450", "600"))  +# Optionally customize labels
  
  
  # Crop the plot to remove areas below -55 latitude
  # coord_sf(ylim = c(-55, 90))  + # Set latitude range from -55 to 90
  
  # scale_y_continuous(breaks = c(-50, 0, 50)) + 
  # Maintain aspect ratio
  # coord_sf(default_crs = sf::st_crs(4326)) +
  
  # scale_y_continuous(breaks = c(-50, 0, 50)) + 
  # scale_x_continuous(breaks = seq(-84, -76, by = 1)) +

scale_y_continuous(breaks = c(-50, -25, 0, 25, 50, 75), labels = c("50°S", "25°S", "0°", "25°N", "50°N", "75°N")) +
  scale_x_continuous(breaks = c(-120, -60, 0, 60, 120), labels = c("120°W", "60°W", "0°", "60°E", "120°E"))+
  coord_fixed(ratio = 1, xlim = c(-155, 170)) +
  labs(
    # title = "CheckEM users",
    x = "Longitude",
    y = "Latitude"
  ) +
  publication_theme +
  theme(
    panel.grid.major = element_line(color = "gray80"),  # Add grid lines for both axes
    axis.text = element_text(size = 14),               # Make axis text visible
    axis.title = element_blank(),                      # Remove axis titles
    axis.ticks = element_line(size = 0.5),              # Add axis ticks
    legend.position = "bottom",            # Move legend below the plot
    text = element_text(family = "Times New Roman", size = 16)
  ) 

map

# Save the plot as a PNG with A4 width and adjusted height
ggsave(
  filename = "manuscript/plots/map_plot_a4.png",  # File name
  plot = map,                 # Use the last plotted ggplot object
  device = "png",                     # Save as PNG
  dpi = 600,                          # High resolution for publication
  width = 21,                       # Width of A4 page in inches
  height = 12,                         # Adjust height to fit the plot nicely
  units = "cm"                        # Specify dimensions in inches
)
