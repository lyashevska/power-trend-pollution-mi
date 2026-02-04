
library(sf)
library(ggplot2)


df <- readRDS("data.rds")
head(df)

pts <- st_as_sf(df,
                coords = c("Longitude", "Latitude"),
                crs = 4326)
uk <- st_read(
  "https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/administrative/gb/lad.json",
  quiet = TRUE
) |>
  st_transform(4326) |>
  st_simplify(1000)  

plot_year <- function(var, prefix, pts, uk) {
  
  years <- sort(unique(pts$Year))
  
  for (y in years) {
    
    p <- ggplot() +
      geom_sf(data = uk,
              fill = "grey97",
              color = "grey60",
              linewidth = 0.25) +
      
      geom_sf(data = pts[pts$Year == y, ],
              aes(color = .data[[var]]),
              size = 1.6,      # smaller = lighter
              alpha = 0.7) +
      
      coord_sf() +
      scale_color_brewer(palette = "Dark2") +
      theme_void(base_size = 12) +
      labs(title = paste(prefix, "-", y),
           color = prefix)
    
    ggsave(
      paste0("figs/by_year/", prefix, "_", y, ".png"),
      p,
      width = 6,
      height = 6,
      dpi = 300  
    )
  }
}

plot_year("HP.AU", "HP", pts, uk)
plot_year("Ospar.AA", "OSPAR", pts, uk)
