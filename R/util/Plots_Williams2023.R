library(ggplot2)
library(dplyr)
library(purrr)
library(MuMIn)     # for dredge
library(glue)

# Make sure dredge uses the right options
options(na.action = "na.fail")

df <- readRDS(here("data/processed/data.rds"))
names(df)

make_dredge_trend_plot <- function(data, area_name) {
  
  if (nrow(data) < 5) {
    return(NULL)   # skip tiny groups
  }
  
  # Global model 
  global_fit <- lm(
    log(TotalCBs) ~ Year + Rel.body.wt + Latitude,
    data = data
  )
 
  dd <- dredge(global_fit, fixed = "Year")
  best_mod <- get.models(dd, 1)[[1]]
  
  # Predictions with 95% CI on the response scale 
  newdata <- data.frame(
    Year = seq(min(data$Year), max(data$Year), length.out = 100),
    Rel.body.wt = mean(data$Rel.body.wt, na.rm = TRUE),
    Latitude   = mean(data$Latitude, na.rm = TRUE)
  )
  
  pred <- predict(best_mod, newdata = newdata, interval = "confidence", level = 0.95)
  newdata <- cbind(newdata, pred)
  
  # Slope and % change
  slope <- coef(best_mod)["Year"]
  pct_change <- (exp(slope) - 1) * 100
  
  # Significance
  pval <- summary(best_mod)$coefficients["Year", "Pr(>|t|)"]
  sig_star <- if (pval < 0.05) "*" else ""
  
  # Best model text for subtitle
  terms <- attr(terms(best_mod), "term.labels")
  model_txt <- if (length(terms) == 1) "Year" else paste(terms, collapse = " + ")
  
  # Plot
  ggplot() +
    geom_point(data = data, 
               aes(x = Year, y = log(TotalCBs)), 
               size = 2.2, alpha = 0.75, colour = "grey30") +
    geom_line(data = newdata, 
              aes(x = Year, y = fit), 
              colour = "black", linewidth = 1) +
    geom_ribbon(data = newdata, 
                aes(x = Year, ymin = lwr, ymax = upr), 
                alpha = 0.22, fill = "black") +
    labs(
      title = area_name,
      subtitle = glue("Best model: {model_txt}   |   {round(pct_change, 1)}% per year{sig_star}"),
      x = "Year",
      y = expression(log(ΣCBs))
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9.5, colour = "grey20")
    )
}

# ------------------------------------------------------------------
# Generate plots 
# ------------------------------------------------------------------
groupings <- c("HP.AU", "Ospar.AA")

for (group_var in groupings) {
  
  cat("Creating plots for", group_var, "...\n")
  
  plots_list <- df %>%
    filter(!is.na(TotalCBs), !is.na(Year)) %>%
    group_by(!!sym(group_var)) %>%
    group_split() %>%
    set_names(map_chr(., ~ as.character(unique(.x[[group_var]])))) %>%
    map(~ {
      area_name <- unique(.x[[group_var]])
      make_dredge_trend_plot(.x, area_name)
    }) %>%
    compact()   # remove any NULL plots
  
  # Combine into multi-panel figure
  combined <- wrap_plots(plots_list, ncol = 2)
  
  # Save
  filename <- glue("output/figs/williams_predicted_trends_{group_var}.png")
  ggsave(filename, 
         plot = combined, 
         width = 11, height = 9, dpi = 300, bg = "white")
}

