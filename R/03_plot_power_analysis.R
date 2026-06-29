##---------------------------
## plot power analysis results
## reads saved RDS outputs from 01_power_analysis_hist.R and 02_power_analysis.R
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

library(here)

dir.create(here("output/figs"), showWarnings = FALSE, recursive = TRUE)

source(here("R/functions/f_plot_power_analysis.R"))

rds_files <- list.files(here("output/rds"), pattern = "\\.rds$", full.names = TRUE)

heatmap_files <- rds_files[!grepl("_hist", basename(rds_files))]
hist_files <- rds_files[grepl("_hist", basename(rds_files))]

for (f in heatmap_files) {
  sum_res <- readRDS(f)
  fname <- basename(f)

  if (fname == "all_sum_res.rds") {
    title <- "All areas"
    out_file <- here("output/figs/All.png")
  } else if (grepl("^ospar_sum_res_", fname)) {
    area <- sub("^ospar_sum_res_(.*)\\.rds$", "\\1", fname)
    title <- area
    out_file <- here("output/figs", paste0("Ospar_", gsub("\\s", "_", area), ".png"))
  } else if (grepl("^hp_sum_res_", fname)) {
    area <- sub("^hp_sum_res_(.*)\\.rds$", "\\1", fname)
    title <- area
    out_file <- here("output/figs", paste0("HP_", gsub("\\s", "_", area), ".png"))
  } else {
    next
  }

  f_plot_power_heatmap(
    sum_res = sum_res,
    title = title,
    file = out_file
  )
}

for (f in hist_files) {
  sum_res <- readRDS(f)
  fname <- basename(f)
  beta <- attr(sum_res, "beta")

  if (fname == "all_sum_res_hist.rds") {
    area_name <- "All areas"
    out_file <- here("output/figs/All_hist.png")
    x_breaks <- 1:10
    include_vline <- FALSE
  } else if (grepl("^ospar_sum_res_hist_", fname)) {
    area_name <- sub("^ospar_sum_res_hist_(.*)\\.rds$", "\\1", fname)
    out_file <- here("output/figs", paste0("Ospar_", gsub("\\s", "_", area_name), "_hist.png"))
    x_breaks <- seq(2, 20, by = 2)
    include_vline <- TRUE
  } else if (grepl("^hp_sum_res_hist_", fname)) {
    area_name <- sub("^hp_sum_res_hist_(.*)\\.rds$", "\\1", fname)
    out_file <- here("output/figs", paste0("HP_", gsub("\\s", "_", area_name), "_hist.png"))
    x_breaks <- seq(2, 20, by = 2)
    include_vline <- TRUE
  } else {
    next
  }

  f_plot_power_hist(
    sum_res = sum_res,
    file = out_file,
    area_name = area_name,
    beta = beta,
    include_vline = include_vline,
    x_breaks = x_breaks
  )
}