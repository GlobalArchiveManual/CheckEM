library(CheckEM)
library(tidyverse)

large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes", 
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes", 
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  filter(!is.na(fb_length_at_maturity_cm))

unique(CheckEM::australia_life_history$class)
unique(CheckEM::australia_life_history$order)

unique(large_bodied_carnivores$order)

test <- large_bodied_carnivores %>%
  dplyr::group_by(order) %>%
  dplyr::summarise(n = n())

write.csv(large_bodied_carnivores, "annotation-schema/data/staging/large_bodied_carnivores.csv")

t <- CheckEM::maturity
