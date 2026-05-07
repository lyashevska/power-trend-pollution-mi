library(ggplot2)
library(dplyr)
library(purrr)
library(MuMIn)
library(glue)
library(patchwork)

options(na.action = "na.fail")

make_simple_trend_plot <- function(data, area_name) {
  
  df_plot <- data %>%
    filter(!is.na(TotalCBs), !is.na(Year)) %>%
    mutate(
      OriginalRow = .row_number   # This will be the original row number from df
    )
  
  if (nrow(df_plot) < 5) return(NULL)
  
  mod <- lm(log(TotalCBs) ~ Year + Rel.body.wt + Latitude, data = df_plot)
  
  newdata <- data.frame(
    Year = seq(min(df_plot$Year), max(df_plot$Year), length.out = 100),
    Rel.body.wt = mean(df_plot$Rel.body.wt, na.rm = TRUE),
    Latitude = mean(df_plot$Latitude, na.rm = TRUE)
  )
  pred <- predict(mod, newdata = newdata, interval = "confidence", level = 0.95)
  newdata <- cbind(newdata, pred)
  
  slope_pct <- (exp(coef(mod)["Year"]) - 1) * 100
  
  ggplot() +
    geom_point(data = df_plot, 
               aes(x = Year, y = log(TotalCBs)),
               size = 2.8, alpha = 0.85, colour = "grey30") +
    geom_text(data = df_plot, 
              aes(x = Year, y = log(TotalCBs), label = OriginalRow),
              size = 2.3, 
              colour = "darkblue", 
              fontface = "bold",
              check_overlap = TRUE) +
    geom_line(data = newdata, 
              aes(x = Year, y = fit), 
              colour = "black", linewidth = 1.1) +
    geom_ribbon(data = newdata, 
                aes(x = Year, ymin = lwr, ymax = upr), 
                alpha = 0.25, fill = "black") +
    labs(
      title = area_name,
      subtitle = glue("Trend: {round(slope_pct, 1)}% per year"),
      x = "Year",
      y = expression(log(Σ[25]*CBs)),
      caption = "Blue numbers = original row number in your df"
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# ====================== IMPORTANT: Add row numbers to df first ======================
df <- df %>%
  mutate(.row_number = row_number())     # ← Add original row index

# ====================== RUN ======================
groupings <- c("HP.AU", "Ospar.AA")

for (group_var in groupings) {
  cat("Creating plots for", group_var, "...\n")
  
  plot_list <- df %>%
    filter(!is.na(TotalCBs), !is.na(Year)) %>%
    group_by(!!sym(group_var)) %>%
    group_split() %>%
    map(~ {
      area_name <- unique(.x[[group_var]])
      p <- make_simple_trend_plot(.x, area_name)
      
      if (!is.null(p)) {
        safe_name <- gsub("[^A-Za-z0-9]", "_", area_name)
        ggsave(glue("output/figs/{safe_name}_originalRow.png"), 
               plot = p, width = 8, height = 6, dpi = 300)
      }
      p
    }) %>%
    compact()
  
  if (length(plot_list) > 0) {
    combined <- wrap_plots(plot_list, ncol = 2)
    ggsave(glue("output/figs/williams_predicted_trends_{group_var}_originalRow.png"),
           plot = combined, width = 15, height = 12, dpi = 300, bg = "white")
    cat("✓ Saved plots with original row numbers for", group_var, "\n")
  }
}