##---------------------------
## simulations for pollution power analysis
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

###################################
## Data preparation
###################################

library(dplyr)
library(readr)
library(here)

data <- read_csv(here("data/raw/juveniles.csv"), show_col_types = FALSE) %>%
  select(Year, Sex, Latitude, Longitude, AreaAU, Body.Weight..kg., 
         Age..yr., Length, AreaAA, sumCBs1) %>%
  filter(!is.na(Body.Weight..kg.), !is.na(Length)) %>%
  rename(
    HP.AU = AreaAU,
    Ospar.AA = AreaAA,
    Age = Age..yr.,
    Weight = Body.Weight..kg.,
    TotalCBs = sumCBs1
  )

cat("Final data dim:", dim(data), "\n")
# Final data dim: 415 10

# relative body weight
logwl <- lm(log(Weight) ~ log(Length),
            data = data,
            na.action = na.omit)

data$Rel.body.wt <- logwl$residuals

saveRDS(data, file = here("data/processed/data.rds"))
