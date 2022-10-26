##---------------------------
## simulations for pollution power analysis
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

library(ggplot2)
library(MuMIn)
library(metR)

start_time <- Sys.time()
# load prepared data
data <- readRDS("data.rds")

# load functions
source("f_detect_trend.R")

###################################
## Power analysis per Ospar.AA
###################################

# list of areas
ids <- unique(data$Ospar.AA)
# list of models
mods <- list()
# list to keep info
info <- vector('list', length(ids))

# for (i in seq_along(ids)) {
  for (i in 1) {
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
  
  
  png(
    file = sprintf("figs/Ospar_%s.png", gsub("\\s", "_", names(mods)[[i]])),
    height = 8,
    width = 12,
    units = "in",
    res = 400
  )
  # new facet label names for obs_window variable
  ow.labs <- c("Monitoring for 6 years", "Monitoring for 10 years")
  names(ow.labs) <- c("6", "10")
  label_df <- data.frame(
    x = c(30, 50, 100) / 6 + 1,
    y = rep(-16, 3),
    label = c("Low", "Medium", "High")
  )
  
  plot <- ggplot(sum_res,
                 aes(x = nobs_year,
                     y = pchange * 100,
                     fill = power)) +
    geom_tile() +
    facet_wrap( ~ obs_window,
                labeller = labeller(obs_window = ow.labs)) +
    geom_vline(xintercept = c(30, 50, 100) / 6, linetype = "dashed") +
    geom_label(data = label_df,
               aes(x = x, y = y, label = label),
               fill = "white") +
    
    labs(title = names(mods)[[i]],
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
    scale_y_continuous(breaks = seq(-15, 15, by = 2))
  print(plot)
  dev.off()
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = paste0("rds/ospar_sum_res_", names(mods)[[i]] , ".rds"))
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
  
  png(
    file = sprintf("figs/HP_%s.png", gsub("\\s", "_", names(mods)[[i]])),
    height = 8,
    width = 12,
    units = "in",
    res = 400
  )
  # new facet label names for obs_window variable
  ow.labs <- c("Monitoring for 6 years", "Monitoring for 10 years")
  names(ow.labs) <- c("6", "10")
  label_df <- data.frame(
    x = c(30, 50, 100) / 6 + 1,
    y = rep(-16, 3),
    label = c("Low", "Medium", "High")
  )
  
  plot <- ggplot(sum_res,
                 aes(x = nobs_year,
                     y = pchange * 100,
                     fill = power)) +
    geom_tile() +
    facet_wrap( ~ obs_window,
                labeller = labeller(obs_window = ow.labs)) +
    geom_vline(xintercept = c(30, 50, 100) / 6, linetype = "dashed") +
    geom_label(data = label_df,
               aes(x = x, y = y, label = label),
               fill = "white") +
    
    labs(title = names(mods)[[i]],
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
    scale_y_continuous(breaks = seq(-15, 15, by = 2))
  print(plot)
  dev.off()
  
  #  collect results
  info[[i]] <- list(model = summary(mods[[i]]))
  
  #  save results
  saveRDS(sum_res, file = paste0("rds/hp_sum_res_", names(mods)[[i]] , ".rds"))
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


png(
  file = "figs/all.png",
  height = 8,
  width = 12,
  units = "in",
  res = 400
)

# new facet label names for obs_window variable
ow.labs <- c("Monitoring for 6 years", "Monitoring for 10 years")
names(ow.labs) <- c("6", "10")
label_df <- data.frame(
  x = c(30, 50, 100) / 6 + 1,
  y = rep(-16, 3),
  label = c("Low", "Medium", "High")
)

plot <- ggplot(sum_res,
               aes(x = nobs_year,
                   y = pchange * 100,
                   fill = power)) +
  geom_tile() +
  facet_wrap( ~ obs_window,
              labeller = labeller(obs_window = ow.labs)) +
  geom_vline(xintercept = c(30, 50, 100) / 6, linetype = "dashed") +
  geom_label(data = label_df,
             aes(x = x, y = y, label = label),
             fill = "white") +
  
  labs(title = "All areas",
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
  scale_y_continuous(breaks = seq(-15, 15, by = 2))
print(plot)
dev.off()

#  save results
saveRDS(sum_res, file = "rds/all_sum_res.rds")

end_time <- Sys.time()
end_time - start_time
