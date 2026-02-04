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

# set working directory to source file location
setwd(system("pwd", intern = T))

data <- read.csv("juveniles.csv")

cat("Raw data dim:", dim(data), "\n")
cat("Year range:", range(data$Year), "\n")

# keep columns of interest
cnames <-
  c(
    "Year",
    "Sex",
    "Latitude",
    "Longitude",
    "AreaAU",
    "Body.Weight..kg.",
    "Age..yr.",
    "Length",
    "AreaAA",
    "sumCBs1"
  )

data <- data[,cnames]

data <- subset(
  data,
  !is.na(Body.Weight..kg.) & !is.na(Length)
)

# rename columns

data <- data %>%
  rename(
    HP.AU = AreaAU,
    Ospar.AA = AreaAA,
    Age = Age..yr.,
    Weight = Body.Weight..kg.,
    TotalCBs = sumCBs1
  )

cat("Final data dim:", dim(data), "\n")

# relative body weight
logwl <- lm(log(Weight) ~ log(Length),
            data = data,
            na.action = na.omit)

summary(logwl)
data$Rel.body.wt <- logwl$residuals

saveRDS(data, file = "data.rds")
