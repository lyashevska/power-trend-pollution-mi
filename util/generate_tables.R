##--------------------------
## Find number of samples needed for each number of years available for power 0.8
## CM, OL: 10/02/2023
##-------------------------

## Generate the right side of table 1

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


## generate the left side of table 1
df <- readRDS("data.rds")

names(df)

df <- subset(
  df,
   Year <= 2017
)

calc_summary <- function(d){
  
  n <- nrow(d)
  
  mean_cb <- mean(d$TotalCBs, na.rm = TRUE)
  sd_cb   <- sd(d$TotalCBs, na.rm = TRUE)
  
  fit <- lm(log(TotalCBs) ~ Year, data = d)
  # mean(lat)
  # mean(long)
  # nutritional cond
  # dredge --> best model

  resid_sd <- sd(residuals(fit))
  slope <- coef(fit)["Year"]
  pct_decline <- (exp(slope) - 1) * 100
  
  data.frame(
    N = n,
    Mean = mean_cb,
    SD = sd_cb,
    Residual_SD = resid_sd,
    Sampling_range = paste(min(d$Year), max(d$Year), sep = "-"),
    Annual_decline_pct = pct_decline
  )
}

    
all_uk <- calc_summary(df)
all_uk$Assessment <- "ALL UK"

au <- split(df, df$HP.AU)

au_table <- do.call(
  rbind,
  lapply(names(au), function(name){
    out <- calc_summary(au[[name]])
    out$Assessment <- name
    out
  })
)

ospar <- split(df, df$Ospar.AA)

ospar_table <- do.call(
  rbind,
  lapply(names(ospar), function(name){
    out <- calc_summary(ospar[[name]])
    out$Assessment <- name
    out
  })
)

final_table <- rbind(all_uk, au_table, ospar_table)

rownames(final_table) <- NULL

final_table

