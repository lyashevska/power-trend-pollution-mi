##--------------------------
## Find number of samples needed for each number of years available for power 0.8
## CM, OL: 10/02/2023
##-------------------------

library(mgcv)

# to accumulate results
res <- NULL

# select datasets
files <-
  grep(
    list.files(path = "rds"),
    pattern = '(hist)',
    invert = FALSE,
    value = TRUE
  )

# loop over datasets
for (i in 1:length(files)) {
  # build path
  fpath <- paste0("rds/", files[i])
  dat <- readRDS(fpath)
  
  # remove parts of the string
  area <- gsub("_sum_res_hist|.rds", "", files[i], perl = TRUE)
  
  # loop over number tailyrs for each area
  for (j in unique(dat$tailyrs)) {
    d <- subset(dat, tailyrs == j)
    fit <-
      gam(power ~ te(nobs_year, k = c(10, 10)), data = d)
    # plot(fit)
    d$pred <- predict(fit)
    
    
    ## get nobs where power > 0.8
    pred_df <-
      expand.grid(nobs_year = seq(min(d$nobs_year), max(d$nobs_year), length = 1e3),
                  tailyrs = d$tailyrs)
    pred_df$power <- predict(fit, newdata = pred_df)
    
    idx <- which(pred_df$power >= 0.8)
    
    # exception handling
    if (length(idx)!=0) {
      nobs <- min(pred_df[idx, "nobs_year"])
    } else {
      nobs <- NA   
    }
    
    ## find min number of obs for power >=0.8
    # nobs_min <- min(pred_df[idx, "nobs_year"])
    
    
    ## get all the results
    r <- data.frame(nobs = nobs,
                    tailyrs = j,
                    area = area)
    # accumulate res for tailyrs
    res <- rbind(res, r)
  }
}

write.csv(res, "csv/power_smoothing_hist.csv", row.names = FALSE)