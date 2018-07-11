## trying out phenology github

dat <- read.csv("trees.csv")

## first clear up date
dat$AddDate <- as.Date(dat$date_of_addition)

range(dat$AddDate, na.rm=TRUE) ## earliest is 2010-06-01

table(is.na(dat$AddDate)) ## only 8 NAs

duration <- max(dat$AddDate) - min(dat$AddDate)
