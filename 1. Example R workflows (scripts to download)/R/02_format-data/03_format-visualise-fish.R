rm(list = ls())

devtools::load_all("./")
library(tidyverse)

name <- "example-bruv-workflow"

metadata.bathy.derivatives <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/", 
                                             name, "_Metadata-bathymetry-derivatives.rds")) %>%
  glimpse()

habitat <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                      name, "_Tidy-habitat.rds")) %>%
  dplyr::mutate(number = number / total.points.annotated) %>%
  dplyr::select(campaignid, sample, longitude, latitude, mbdepth, slope, aspect, 
                TPI, TRI, roughness, detrended, habitat, number, 
                mean.relief, sd.relief) %>%
  pivot_wider(names_from = habitat, values_from = number, values_fill = 0) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(reef = consolidated_hard + macroalgae + 
                  seagrasses + sessile_invertebrates) %>%                       # Only Thalassodendron seagrass which grows on reef
  dplyr::select(-c(consolidated_hard, macroalgae, seagrasses, 
                   sessile_invertebrates, unconsolidated_soft)) %>%
  glimpse()

# Load species specific metrics
url <- 'https://docs.google.com/spreadsheets/d/176genWqd_pc3NDVQtP2CmfMh66Ui6uwWrLoJU1Ny_qw/edit?usp=sharing'

maturity <- googlesheets4::read_sheet(url, sheet = "fisheries Lm") %>%
  # dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50.mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50.mm)) %>%
  ungroup() %>%
  glimpse()

tidy.maxn <- readRDS(paste0("1. Example R workflows (scripts to download)/data/staging/", name, 
                       "_Complete-maxn.rds")) %>%
  dplyr::group_by(campaignid, sample, scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  pivot_wider(names_from = "scientific", values_from = maxn, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 3:(ncol(.))], na.rm = TRUE ),
                species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% 
  dplyr::select(campaignid, sample, total.abundance, species.richness) %>%
  pivot_longer(cols = c("total.abundance", "species.richness"),
               names_to = "response", values_to = "number") %>%
  dplyr::left_join(habitat) %>%
  glimpse()

saveRDS(tidy.maxn, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                   name, "_Tidy-maxn.rds"))

lengths <- readRDS(paste0("1. Example R workflows (scripts to download)/data/staging/", name, 
                           "_Expanded-length.rds")) %>%
  left_join(habitat) %>%
  left_join(maturity) %>%
  dplyr::mutate(number = 1) %>%
  glimpse()

indicator.species <- lengths %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::filter(scientific %in% c("Choerodon rubescens", "Chrysophrys auratus",
                                  "Glaucosoma hebraicum")) %>%
  glimpse()

ggplot() +
  geom_point(data = indicator.species, aes(x = longitude, y = latitude), alpha = 0.5) +
  coord_sf()

metadata.length <- lengths %>%
  distinct(campaignid, sample, latitude, longitude, status, mbdepth, slope, aspect,
           tpi, tri, roughness, detrended, mean_relief, reef) %>%
  glimpse()

greater.mat <- indicator.species %>%
  dplyr::filter(length > l50) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = ">Lm") %>%
  dplyr::glimpse()

smaller.mat <- indicator.species %>%
  dplyr::filter(length < l50) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "<Lm") %>%
  dplyr::glimpse()

tidy.lengths <- bind_rows(greater.mat, smaller.mat) %>%
  glimpse()

saveRDS(tidy.lengths, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                     name, "_Tidy-lengths.rds"))

# Visualise
sanctuaries <- st_read("1. Example R workflows (scripts to download)/data/spatial/shapefiles/marine-parks-all.shp") %>%
  st_transform(4326) %>%                                                        # Transform to match with your metadata CRS
  dplyr::filter(str_detect(ZoneName, "Sanctuary|National"))

# > Lm & < Lm
ggplot() +
  geom_sf(data = sanctuaries, aes(fill = ZoneName), alpha = 0.5) +
  scale_fill_manual(values = c("Marine National Park Zone" = "#7bbc63",
                               "National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054")) +
  geom_point(data = tidy.lengths, aes(x = longitude, y = latitude, 
                                      size = number), shape = 1) +
  coord_sf(xlim = c(min(tidy.lengths$longitude), max(tidy.lengths$longitude)),
           ylim = c(min(tidy.lengths$latitude), max(tidy.lengths$latitude))) +
  facet_wrap(~scientific)

# Total abundance
ggplot() +
  geom_sf(data = sanctuaries, aes(fill = ZoneName), alpha = 0.5) +
  scale_fill_manual(values = c("Marine National Park Zone" = "#7bbc63",
                               "National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054")) +
  geom_point(data = filter(tidy.maxn, response %in% "total.abundance"), 
             aes(x = longitude, y = latitude, size = number), shape = 1) +
  coord_sf(xlim = c(min(tidy.maxn$longitude), max(tidy.maxn$longitude)),
           ylim = c(min(tidy.maxn$latitude), max(tidy.maxn$latitude))) +
  labs(x = "Longitude", y = "Latitude", size = "Total abundance")

# Species richness
ggplot() +
  geom_sf(data = sanctuaries, aes(fill = ZoneName), alpha = 0.5) +
  scale_fill_manual(values = c("Marine National Park Zone" = "#7bbc63",
                               "National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054")) +
  geom_point(data = filter(tidy.maxn, response %in% "species.richness"), 
             aes(x = longitude, y = latitude, size = number), shape = 1) +
  coord_sf(xlim = c(min(tidy.maxn$longitude), max(tidy.maxn$longitude)),
           ylim = c(min(tidy.maxn$latitude), max(tidy.maxn$latitude))) +
  labs(x = "Longitude", y = "Latitude", size = "Species richness")

