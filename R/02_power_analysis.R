##---------------------------
## simulations for pollution power analysis
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

library(MuMIn)
library(here)

# Create output directories if they don't exist
dir.create(here("output/figs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/rds"), showWarnings = FALSE, recursive = TRUE)

start_time <- Sys.time()
# load prepared data
data <- readRDS(here("data/processed/data.rds"))

# load functions
source(here("R/functions/f_detect_trend.R"))
source(here("R/functions/f_plot_power_analysis.R"))

###################################
## Power analysis per Ospar.AA
###################################

# list of areas
ids <- unique(data$Ospar.AA)
# list of models
mods <- list()
# list to keep info
info <- vector('list', length(ids))

for (i in seq_along(ids)) {
  #  for (i in 1) {
  print(ids[i])
  temp <- data[data$Ospar.AA == ids[i],]
  
  temp$Year <- temp$Year - min(temp$Year)
  
  global_mod <- lm(log(TotalCBs) ~ Year +
                     Rel.body.wt +
                     Latitude,
                   na.action = na.fail,
                   data = temp)
  
  # evaluate all possible submodels but always keep at least Intercept and Year
  dredge_mod <- dredge(global_mod,
                       fixed = c("Year"))
  
  # select best
  mods[[i]] <- get.models(dredge_mod,
                          subset = delta == 0)[[1]]
  
  names(mods)[[i]] <- unique(temp$Ospar.AA)
  beta <- coef(mods[[i]])
  nsim <- 1000
  
  all_res <- expand.grid(
    ## sampling frequency
    nobs_year = seq(2, 20, 2),
    ## annual percentage change
    pchange = seq(-0.15, 0.15, by = 0.01),
    ## number of years to monitor (OSPAR, MSFD reporting windows)
    obs_window = c(6, 10),
    sim = 1:nsim,
    ## number of years used to estimate variability in data
    detect = NA
  )
  
  for (j in 1:nrow(all_res)) {
    all_res$detect[j] <- f_detect_trend(
      ## new beta to give required annual % change in intercept
      beta_new = log(1 + all_res$pchange[j]),
      obs_window = all_res$obs_window[j],
      nobs_year = all_res$nobs_year[j],
      data = temp,
      mod = mods[[i]]
    )
  }
  
  sum_res <-
    aggregate(
      x = detect ~ nobs_year + obs_window + pchange,
      FUN = sum,
      data = all_res
    )
  sum_res$power <- sum_res$detect / nsim
  
  
  f_plot_power_heatmap(
    sum_res = sum_res,
    title = names(mods)[[i]],
    file = here("output/figs", paste0("Ospar_", gsub("\\s", "_", names(mods)[[i]]), ".png"))
  )
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = here("output/rds", paste0("ospar_sum_res_", names(mods)[[i]] , ".rds")))
}

###################################
## Power analysis per HP.AU
###################################

ids <- unique(data$HP.AU)
mods <- list()
# list to keep info
info <- vector('list', length(ids))

for (i in seq_along(ids)) {
  # for (i in 1) {
  print(ids[i])
  temp <- data[data$HP.AU == ids[i],]
  temp$Year <- temp$Year - min(temp$Year)
  global_mod <- lm(log(TotalCBs) ~ Year +
                     Rel.body.wt +
                     Latitude,
                   na.action = na.fail,
                   data = temp)
  
  # evaluate all possible submodels but always keep at least Intercept and Year
  dredge_mod <- dredge(global_mod,
                       fixed = c("Year"))
  
  # select best
  mods[[i]] <- get.models(dredge_mod,
                          subset = delta == 0)[[1]]
  
  names(mods)[[i]] <- unique(temp$HP.AU)
  beta <- coef(mods[[i]])
  nsim <- 1000
  
  all_res <- expand.grid(
    ## sampling frequency
    nobs_year = seq(2, 20, 2),
    ## annual percentage change
    pchange = seq(-0.15, 0.15, by = 0.01),
    ## number of years to monitor (OSPAR, MSFD reporting windows)
    obs_window = c(6, 10),
    sim = 1:nsim,
    ## number of years used to estimate variability in data
    detect = NA
  )
  
  for (j in 1:nrow(all_res)) {
    all_res$detect[j] <- f_detect_trend(
      ## new beta to give required annual % change in intercept
      beta_new = log(1 + all_res$pchange[j]),
      obs_window = all_res$obs_window[j],
      nobs_year = all_res$nobs_year[j],
      data = temp,
      mod = mods[[i]]
    )
  }
  
  sum_res <-
    aggregate(
      x = detect ~ nobs_year + obs_window + pchange,
      FUN = sum,
      data = all_res
    )
  sum_res$power <- sum_res$detect / nsim
  
  f_plot_power_heatmap(
    sum_res = sum_res,
    title = names(mods)[[i]],
    file = here("output/figs", paste0("HP_", gsub("\\s", "_", names(mods)[[i]]), ".png"))
  )
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = here("output/rds", paste0("hp_sum_res_", names(mods)[[i]] , ".rds")))

}

###################################
## Power analysis all areas
###################################

mods <- lm(log(TotalCBs) ~ Year +
             Rel.body.wt +
             Latitude,
           data = data)

beta <- dput(coef(mods))
nsim <- 1000

all_res <- expand.grid(
  ## sampling frequency
  nobs_year = seq(2, 20, 2),
  ## annual percentage change
  pchange = seq(-0.15, 0.15, by = 0.01),
  ## number of years to monitor (OSPAR, MSFD reporting windows)
  obs_window = c(6, 10),
  sim = 1:nsim,
  ## number of years used to estimate variability in data
  detect = NA
)

for (j in 1:nrow(all_res)) {
  all_res$detect[j] <- f_detect_trend(
    ## new beta to give required annual % change in intercept
    beta_new = log(1 + all_res$pchange[j]),
    obs_window = all_res$obs_window[j],
    nobs_year = all_res$nobs_year[j],
    data = data,
    mod = mods
  )
}

sum_res <-
  aggregate(
    x = detect ~ nobs_year + obs_window + pchange,
    FUN = sum,
    data = all_res
  )
sum_res$power <- sum_res$detect / nsim


f_plot_power_heatmap(
  sum_res = sum_res,
  title = "All areas",
  file = here("output/figs/All.png")
)

#  save results
saveRDS(sum_res, file = here("output/rds/all_sum_res.rds"))
end_time <- Sys.time()
end_time - start_time
