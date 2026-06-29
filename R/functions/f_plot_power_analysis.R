##---------------------------
## plotting functions for power analysis
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

library(ggplot2)
library(metR)

f_plot_power_heatmap <- function(sum_res, title, file) {
  ow.labs <- c("Monitoring for 6 years", "Monitoring for 10 years")
  names(ow.labs) <- c("6", "10")
  sampling_vlines <- c(6, 10, 18)
  label_df <- data.frame(
    x = sampling_vlines,
    y = rep(-16, 3),
    label = c("6/yr", "10/yr", "18/yr")
  )

  plot <- ggplot(sum_res,
                 aes(x = nobs_year,
                     y = pchange * 100,
                     fill = power)) +
    geom_tile() +
    facet_wrap(~ obs_window,
               labeller = labeller(obs_window = ow.labs)) +
    geom_vline(xintercept = sampling_vlines, linetype = "dashed") +
    geom_label(data = label_df,
               aes(x = x, y = y, label = label),
               fill = "white") +
    labs(title = title,
         x = "Number observations per year",
         y = "% annual change") +
    theme_bw() +
    scale_fill_gradientn(colours = c("lightgray", "red", "darkred")) +
    geom_contour(aes(z = power),
                 breaks = c(0.8, 0.9),
                 colour = "grey") +
    geom_text_contour(
      aes(z = power),
      breaks = c(0.8, 0.9),
      stroke = 0.2,
      skip = 0
    ) +
    scale_x_continuous(breaks = seq(2, 20, by = 2)) +
    scale_y_continuous(breaks = seq(-15, 15, by = 2))

  png(
    file = file,
    height = 8,
    width = 12,
    units = "in",
    res = 400
  )
  print(plot)
  dev.off()
}

f_plot_power_hist <- function(sum_res,
                              file,
                              area_name,
                              beta = NULL,
                              include_vline = TRUE,
                              x_breaks = seq(2, 20, by = 2)) {
  sampling_vlines <- c(6, 10, 18)
  if (!is.null(beta)) {
    title <- paste0(
      area_name,
      ifelse(sign(beta)["Year"] == -1, ", negative trend", ", positive trend")
    )
  } else {
    title <- area_name
  }

  plot <- ggplot(
    sum_res,
    aes(
      x = nobs_year,
      y = power,
      group = tailyrs
    )
  ) +
    geom_line(aes(color = as.factor(tailyrs))) +
    geom_point() +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    labs(
      title = title,
      x = "Number observations per year",
      y = "Power to detect",
      color = "Number of years"
    ) +
    scale_x_continuous(breaks = x_breaks) +
    theme_bw()

  if (include_vline) {
    plot <- plot + geom_vline(xintercept = sampling_vlines, linetype = "dashed")
  }

  png(file = file)
  print(plot)
  dev.off()
}