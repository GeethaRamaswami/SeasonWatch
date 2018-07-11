## trying out phenology github

dat <- read.csv("trees.csv")

## first clear up date
dat$AddDate <- as.Date(dat$date_of_addition)

range(dat$AddDate, na.rm=TRUE) ## earliest is 2010-06-01

table(is.na(dat$AddDate)) ## only 8 NAs

## make histogram of add dates
hist(dat$AddDate, breaks = as.Date(c("2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01")))
