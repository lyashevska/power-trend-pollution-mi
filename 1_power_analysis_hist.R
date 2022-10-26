##---------------------------
## simulations for pollution power analysis
## using historical data 
## slope of the trend does not change
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

library(ggplot2)
library(MuMIn)

# load prepared data
data <- readRDS("data.rds")

# load functions
source("f_detect_trend_hist.R")
source("f_tail_yrs.R")

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
  # for (i in 1) {
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
  
  # max number of years  available
  max_yrs_avail <- length(unique(temp$Year))
  
  all_res <- expand.grid(
    # sampling frequency
    nobs_year = seq(2, 20, 2),
    sim = 1:nsim,
    # number of years used to estimate variability in data
    tailyrs = f_tail_yrs(max_yrs_avail),
    detect = NA
  )
  
  for (j in 1:nrow(all_res)) {
    all_res$detect[j] <- f_detect_trend_hist(
      nobs_year = all_res$nobs_year[j],
      tailyrs = all_res$tailyrs[j],
      data = temp,
      mod = mods[[i]]
    )
  }
  
  sum_res <-
    aggregate(x = detect ~ nobs_year + tailyrs,
              FUN = sum,
              data = all_res)
  sum_res$power <- sum_res$detect / nsim
  
  png(file = sprintf("figs/Ospar_%s_hist.png", gsub("\\s", "_", names(mods)[[i]])))
  
  plot <- ggplot(sum_res,
                 aes(x = nobs_year,
                     y = power,
                     group = tailyrs)) +
    geom_line(aes(color = as.factor(tailyrs))) +
    geom_point() +
    geom_vline(xintercept = c(30, 50, 100) / 6, linetype = "dashed") +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    labs(
      title = paste0(
        names(mods)[[i]],
        ifelse(sign(beta)["Year"] == -1, ", negative trend", ", positive trend")
      ),
      x = "Number observations per year",
      y = "Power to detect",
      color = "Number of years"
    ) +
    theme_bw()
  print(plot)
  dev.off()
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = paste0("rds/ospar_sum_res_hist", names(mods)[[i]] , ".rds"))
}

###################################
## Power analysis per HP.AU
###################################

ids <- unique(data$HP.AU)
# list of models
mods <- list()
# list to keep info
info <- vector('list', length(ids))

for (i in seq_along(ids)) {
  # for (i in 1) {
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
  
  # max number of years  available
  max_yrs_avail <- length(unique(temp$Year))
  
  all_res <- expand.grid(
    # sampling frequency
    nobs_year = seq(2, 20, 2),
    sim = 1:nsim,
    # number of years used to estimate variability in data
    tailyrs = f_tail_yrs(max_yrs_avail),
    detect = NA
  )
  
  for (j in 1:nrow(all_res)) {
    all_res$detect[j] <- f_detect_trend_hist(
      nobs_year = all_res$nobs_year[j],
      tailyrs = all_res$tailyrs[j],
      data = temp,
      mod = mods[[i]]
    )
  }
  
  sum_res <-
    aggregate(x = detect ~ nobs_year + tailyrs,
              FUN = sum,
              data = all_res)
  sum_res$power <- sum_res$detect / nsim
  
  png(file = sprintf("figs/HP_%s_hist.png", gsub("\\s", "_", names(mods)[[i]])))
  
  plot <- ggplot(sum_res,
                 aes(x = nobs_year,
                     y = power,
                     group = tailyrs)) +
    geom_line(aes(color = as.factor(tailyrs))) +
    geom_point() +
    geom_vline(xintercept = c(30, 50, 100) / 6, linetype = "dashed") +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    labs(
      title = paste0(
        names(mods)[[i]],
        ifelse(sign(beta)["Year"] == -1, ", negative trend", ", positive trend")
      ),
      x = "Number observations per year",
      y = "Power to detect",
      color = "Number of years"
    ) +
    theme_bw()
  print(plot)
  dev.off()
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = paste0("rds/hp_sum_res_hist", names(mods)[[i]] , ".rds"))
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

# max number of years  available
max_yrs_avail <- length(unique(data$Year))

all_res <- expand.grid(
  nobs_year = 1:10,
  sim = 1:nsim,
  tailyrs = f_tail_yrs(max_yrs_avail),
  detect = NA
)

for (j in 1:nrow(all_res)) {
    all_res$detect[j] <- f_detect_trend_hist(
    nobs_year = all_res$nobs_year[j],
    tailyrs = all_res$tailyrs[j],
    data = data,
    mod = mods
  )
}

sum_res <-
  aggregate(x = detect ~ nobs_year + tailyrs,
            FUN = sum,
            data = all_res)
sum_res$power <- sum_res$detect / nsim

png(file = "figs/All_hist.png")

plot <- ggplot(sum_res,
               aes(x = nobs_year,
                   y = power,
                   group = tailyrs)) +
  geom_line(aes(color = as.factor(tailyrs))) +
  geom_point() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(
    title = "All areas",
    x = "Number observations per year",
    y = "Power to detect",
    color = "Number of years"
  ) +
  theme_bw()
print(plot)
dev.off()

#  save results
saveRDS(sum_res, file = "rds/all_sum_res_hist.rds")
