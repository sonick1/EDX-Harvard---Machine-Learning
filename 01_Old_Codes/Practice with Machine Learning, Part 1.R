library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


# Q1
table(sex = dat$sex,type = dat$type)

dat %>% group_by(type) %>% summarize(Prop_female = mean(sex == "Female"))

# Q2
y_hat <- ifelse(dat$type == "inclass","Female","Male")
mean(y == y_hat)

table(y_hat, y)

# Q3
sensitivity(data = as.factor(y_hat),reference = as.factor(dat$sex))

# Q4
specificity(data = as.factor(y_hat),reference = as.factor(dat$sex))

# Q5
nrow(dat[dat$sex == "Female",])/nrow(dat)
mean(y == "Female")
