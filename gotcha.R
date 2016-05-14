library(e1071)
library(rpart)
library(rpart.plot)
source("/home/jsullivan/Downloads/lin-regr-util.R")

set.seed(123)
splits = split_data(dat)
tr_dat = splits[[1]]
te_dat = splits[[2]]

fit = naiveBayes(pattern ~ timeline.incident.month + timeline.incident.year + victim.industry + victim.state + victim.employee_count, data = tr_dat)
predicts = predict(fit, newdata = te_dat)
actuals = te_dat$pattern
success = mean(predicts == actuals)