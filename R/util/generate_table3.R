##--------------------------
## Smoothing power surface
## CM, OL: 1/12/222
## find the pchange at min, low, medium, high, max
## nobs_year
## table 3
##-------------------------

library(ggplot2)
library(mgcv)

# to accumulate results
res <- NULL

files <-
  grep(
    list.files(path = "output/rds"),
    pattern = '(hist)',
    invert = TRUE,
    value = TRUE
  )

# loop over datasets
for (i in 1:length(files)) {
  # build path
  fpath <- paste0("output/rds/", files[i])
  dat <- readRDS(fpath)
  # remove parts of the string
  area <- gsub("_sum_res_|.rds", "", files[i], perl = TRUE)
  
  # loop over obs_window for each area
  for (j in unique(dat$obs_window)) {
    d <- subset(dat, obs_window == j)
    
    # ggplot(d, aes(x = nobs_year, y = pchange, fill = power)) +
    #   geom_raster() +
    #   geom_contour(aes(z = power), breaks = 0.8) +
    #   scale_fill_gradient(low = "white", high = "darkred")
    ## smooth a surface
    ## as we need smooth values for the power
    fit <-
      gam(power ~ te(nobs_year, pchange, k = c(10, 10)), data = d)
    # plot(fit)
    d$pred <- predict(fit)
    
    # p <- ggplot(d, aes(x = nobs_year, y = pchange)) +
    # geom_raster(aes(fill = pred)) +
    # geom_contour(aes(z = pred), breaks = 0.8) +
    # scale_fill_gradient(low = "white", high = "darkred")
    
    ## get minimum and maximum nobs where power > 0.8
    pred_df <-
      expand.grid(
        nobs_year = seq(min(d$nobs_year), max(d$nobs_year), length = 1e3),
        pchange = seq(min(d$pchange), 0, length = 1e3)
      )
    pred_df$power <- predict(fit, newdata = pred_df)
    
    idx <- which(pred_df$power >= 0.8)
    nobs_min <- ceiling(min(pred_df[idx, "nobs_year"]))
    nobs_max <- ceiling(max(pred_df[idx, "nobs_year"]))
    
    ## find the pchange at each of these (min, low, medium, high, max)
    nobs_set <-
      c(nobs_min, nobs_max, ceiling(c(30, 50, 100) / 6 + 1))
    
    pred_df <- expand.grid(nobs_year = nobs_set,
                           pchange = seq(min(d$pchange), 0, length = 1e4))
    pred_df$power <- predict(fit, newdata = pred_df)
    
    ## get all the results
    r <- data.frame(
      nobs = nobs_set,
      pchange_80 = NA,
      name = c("min", "max", "low", "medium", "high"),
      obs_window = j,
      area = area
    )
    
    for (k in 1:nrow(r)) {
      sub <- subset(pred_df, nobs_year == r$nobs[k])
      ## find closest one to 0.8
      if (any(sub$power >= 0.8)) {
        r$pchange_80[k] <- sub$pchange[which.min((sub$power - 0.8) ^ 2)]
      }
    }
    
    # p + geom_point(data = res, aes(x = nobs, y = pchange_80), size = 2)
    # accumulate res for obs_windows
    res <- rbind(res, r)
  }
  
}
write.csv(res, "output/tables/power_smoothing.csv", row.names = FALSE)

