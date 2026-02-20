##---------------------------
## Plot animals 2018, 2019 so that Sinead can add AA and AU from visual assessment
## data: juvenile HP 2018, 2019
## OL 27/01/26
##---------------------------

library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

df <- read_csv("csv/juveniles.csv", show_col_types = FALSE) %>%
  filter(Year %in% c(2018, 2019)) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

pts <- st_as_sf(df,
                coords = c("Longitude", "Latitude"),
                crs = 4326)

uk <- st_read("https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/administrative/gb/lad.json")
uk <- st_transform(uk, 4326)

ggplot() +
  geom_sf(data = uk,
          fill = "grey97",
          color = "grey60",
          linewidth = 0.25) +
  
  geom_sf(data = pts,
          size = 2,
          alpha = 0.9) +
  
  geom_text_repel(
    data = pts,
    aes(label = .data[["National.Reference"]], geometry = geometry),
    stat = "sf_coordinates",
    size = 2.6,
    box.padding = 0.25,
    point.padding = 0.2,
    min.segment.length = 0,
    max.overlaps = 40
  ) +
  
  facet_wrap(~Year) +      # nice separation
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold")
  )

ggsave("figs/all_animals_2018_2019.pdf", width = 7, height = 6)



