##---------------------------
## # function to detect trend
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

f_detect_trend <-
  function(beta_new, obs_window, nobs_year, data, mod) {
    ## beta_new: the slope over year
    ## obs_window: 6 or 10 years of observations
    ## nobs_year: the number of observations per year
    ## data: data
    ## mod: model
    ##-------------
    
    ## obs_window years of sampling starting in year zero
    years <- 0:(obs_window - 1)
    
    # simulate a design matrix
    year_samp <- rep(years, each = nobs_year)
    n <- length(year_samp)
    
    # relative body weight assuming normally distributed
    mu_rbw <- mean(data$Rel.body.wt)
    sd_rbw <- sd(data$Rel.body.wt)
    rbw_samp <- rnorm(n, mu_rbw, sd_rbw)
    
    # latitude - assuming uniform
    min_lat <- range(data$Latitude)[1]
    max_lat <- range(data$Latitude)[2]
    lat_samp <- runif(n, min_lat, max_lat)
    
    X_full <- data.frame(
      "Intercept" = 1,
      "Rel.body.wt" = rbw_samp,
      "Latitude" = lat_samp,
      "Year" = year_samp
    )
    
    # remove ( ) around Intercept
    names(beta)[1] <- "Intercept"
    
    # select betas which are present in the model
    X_samp <- as.matrix(X_full[, names(beta)])
    
    # residual standard deviation
    res_sd <- summary(mod)$sigma
    
    # update beta
    beta["Year"] <- beta_new
    # is slope negative or positive
    sign_beta <- sign(beta["Year"])
    
    y_samp <- rnorm(n, mean = X_samp %*% beta, sd = res_sd)
    fit <- lm(y_samp ~ -1 + X_samp)
    sfit <- summary(fit)
    est_beta <- sfit$coefficients["X_sampYear" , "Estimate"]
    p_trend <- sfit$coefficients["X_sampYear" , "Pr(>|t|)"]
    # was a significant negative trend detected?
    if (sign_beta == sign(est_beta) & p_trend <= 0.05) {
      return(1)
    } else{
      return(0)
    }
  }
