##---------------------------
## simulations for pollution power analysis
## title: Task 5: Assessing the statistical power to detect trends in HP SUM(PCBs)
## data: juvenile HP
## OL, CM: 3/8/22
##---------------------------

###################################
## Data preparation
###################################


# set working directory to source file location
setwd(system("pwd", intern = T))

data <- read.csv("juveniles.csv")

dim(data)
# 387 116

range(data$Year)
# [1] 1991 2017

# keep columns of interest
cnames <-
  c(
    "Year",
    "Sex",
    "Latitude",
    "Longitude",
    "Areaau",
    "Body.Weight..kg.",
    "Age..yr.",
    "Length",
    "AreaAU2",
    "TotalCBs"
  )
idcnames <- which(colnames(data) %in% cnames)
data <- data[, sort(idcnames)]
dim(data)
# [1] 387   10

# rename columns
names(data)[names(data) == 'Areaau'] <- 'HP.AU'
names(data)[names(data) == 'AreaAU2'] <- 'Ospar.AA'
names(data)[names(data) == 'Age..yr.'] <- 'Age'
names(data)[names(data) == 'Body.Weight..kg.'] <- 'Weight'

hist(data$Weight)
hist(data$Age)

plot(data$Age, data$Weight)
plot(data$Weight, data$Length)

#check distribution
hist(log(data$TotalCBs))
hist(data$TotalCBs)

# freq per year
plot(table(data$Year))

# total number of animals
nrow(data)
# 387

# missing length
table(is.na(data$Length))
# FALSE
# 387

# missing body weight
table(is.na(data$Weight))
# FALSE
# 387

# relative body weight
logwl <- lm(log(data$Weight) ~ log(data$Length))
summary(logwl)
data$Rel.body.wt <- logwl$residuals

# compare with Table 1 from paper
# OSPAR Assessment Area
aggregate(TotalCBs ~ Ospar.AA, data, function(x)
  c(
    number = length(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x)
  ))

#             Ospar.AA TotalCBs.number TotalCBs.mean TotalCBs.median TotalCBs.sd
# 1            Channel       32.000000     24.683956       14.441331   23.264256
# 2   Irish and Celtic      142.000000     16.195206       10.226376   17.434634
# 3 Northern North Sea      113.000000      8.511053        5.093827    8.465946
# 4 Southern North Sea       75.000000     16.371857       10.435000   22.876814
# 5   Western Scotland       25.000000      9.234992        6.301535    8.882160
# corrected
# Ospar.AA TotalCBs.number TotalCBs.mean TotalCBs.median TotalCBs.sd
# 1             Channel       32.000000     24.683956       14.441331   23.264256
# 2    Irish and Celtic      155.000000     15.970253       10.341928   16.948797
# 3  Northern North Sea      113.000000      8.511053        5.093827    8.465946
# 4  Southern North Sea       75.000000     16.371857       10.435000   22.876814
# 5 Western Scotland AA       12.000000      4.600399        4.293030    3.080549

# HP Assessment Unit
aggregate(TotalCBs ~ HP.AU, data, function(x)
  c(
    number = length(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x)
  ))
#               HP.AU TotalCBs.number TotalCBs.mean TotalCBs.median TotalCBs.sd
# 1        Celtic Sea       73.000000     19.522806       12.444369   19.503661
# 2 Greater North Sea      205.000000     12.713283        8.021111   17.308609
# 3         Irish Sea       84.000000     14.855431        9.612097   16.799348
# 4  Western Scotland       25.000000      9.234992        6.301535    8.882160

plot(data$Latitude, data$Longitude)

saveRDS(data, file = "data.rds")
