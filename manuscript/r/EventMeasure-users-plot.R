# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(CheckEM)

# Read in EM data ----
data <- read_xlsx("manuscript/data/eventmeasure_users_2008-11_2024.xlsx") %>%
  CheckEM::clean_names()

# Create the plot
em_users <- ggplot(data, aes(x = year)) +
  geom_line(aes(y = au_cumulative, color = "Australia"), size = 1) +
  geom_line(aes(y = int_cumulative, color = "International"), size = 1) +
  labs(x = "Year", y = "Cumulative Number\n of EventMeasure Licences") +
  scale_color_manual(values = c("Australia" = "#1f77b4", "International" = "#ff7f0e")) + # Updated colors
  scale_x_continuous(breaks = seq(2008, 2024, by = 2)) +  # Increased number of year breaks
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    text = element_text(family = "serif", size = 16),
    legend.text = element_text(size = 12)
  )

em_users

ggsave("manuscript/plots/event_measure_licences_plot.png", 
       plot = em_users, device = "png", 
       width = 21,                       # Width of A4 page in inches
       height = 8, 
       units = "cm", dpi = 300)
