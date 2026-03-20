##--------------------------
## Find number of samples needed for each number of years available for power 0.8
## CM, OL: 10/02/2023
##-------------------------

# generate the right side of table 1 for publication

library(mgcv)
library(MuMIn)
library(here)

# Create output directories if they don't exist
dir.create(here("output/tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/rds"), showWarnings = FALSE, recursive = TRUE)

# to accumulate results
res <- NULL

# select datasets
files <-
  grep(
    list.files(path = here("output/rds")),
    pattern = '(hist)',
    invert = FALSE,
    value = TRUE
  )

# loop over datasets
for (i in 1:length(files)) {
  # build path
  fpath <- here("output/rds", files[i])
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

write.csv(res, here("output/tables/power_smoothing_hist.csv"), row.names = FALSE)

#################################################################

# generate the left side of table 1 for publication
df <- readRDS(here("data/processed/data.rds"))

names(df)

# subset for now to verify results
#df <- subset(
#  df,
#   Year <= 2017
#)

options(na.action = "na.fail")
calc_summary <- function(d) {
  
  if (nrow(d) < 5) return(NULL)  # safety for tiny groups
  
  n <- nrow(d)
  
  mean_cb <- mean(d$TotalCBs, na.rm = TRUE)
  sd_cb   <- sd(d$TotalCBs, na.rm = TRUE)
  
  # -----------------
  # global model
  # -----------------
  global_fit <- lm(
    log(TotalCBs) ~ Year + Rel.body.wt + Latitude,
    data = d
  )
  
  # dredge selection

  dd <- dredge(global_fit, fixed = "Year")
  
  best_mod <- get.models(dd, 1)[[1]]
  
  # metrics

  resid_sd <- sd(residuals(best_mod))
  
  slope <- coef(best_mod)["Year"]
  
  pct_decline <- (exp(slope) - 1) * 100
  
  best_formula <- formula(best_mod)
  
  best_terms <- attr(terms(best_mod), "term.labels")
  
  if (length(best_terms) == 1) {
    best_model_txt <- "Year"
  } else {
    best_model_txt <- paste(best_terms, collapse = " + ")
  }

  ## extract coefficients 
  beta_best <- coef(best_mod)
  beta_all <- coef(global_fit)
  beta_all[] <- 0
  beta_all[names(beta)] <- beta_best
  names(beta_all) <- paste0("beta_", names(beta_all))
 
  out <- data.frame(
    N = n,
    Mean = mean_cb,
    SD = sd_cb,
    Residual_SD = resid_sd,
    Sampling_range = paste(min(d$Year), max(d$Year), sep = "-"),
    Annual_decline_pct = pct_decline,
    Best_model = best_model_txt
  )
   out <- cbind(out, t(as.data.frame(beta_all)))
  
  rownames(out) <- NULL
  out
}

# ALL UK

all_uk <- calc_summary(df)
all_uk$Assessment <- "ALL UK"

# HP.AU 

au_table <- do.call(
  rbind,
  lapply(split(df, df$HP.AU), function(sub) {
    out <- calc_summary(sub)
    out$Assessment <- paste0("AU_", unique(sub$HP.AU))
    out
  })
)

# OSPAR 

ospar_table <- do.call(
  rbind,
  lapply(split(df, df$Ospar.AA), function(sub) {
    out <- calc_summary(sub)
    out$Assessment <- paste0("AA_", unique(sub$Ospar.AA))
    out
  })
)

res2 <- rbind(all_uk, au_table, ospar_table)
rownames(res2) <- NULL

res2

write.csv(res2, here("output/tables/table1.csv"), row.names = FALSE)

