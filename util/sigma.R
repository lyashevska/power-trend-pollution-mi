files <- list.files(
  path = "rds",
  pattern = "_sigma_",
  full.names = TRUE
)

sigma_values <- sapply(files, readRDS)

sigma_df <- data.frame(
  file  = sub("\\.rds$", "", basename(files)),
  sigma = as.numeric(sigma_values),
  row.names = NULL
)

sigma_df
