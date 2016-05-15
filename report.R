library(e1071)

source("lin-regr-util.R")

transform_row = function(dat, name)
{
  subframe = dat[,grep(name, colnames(dat))]
  sub = apply(subframe, 1, function(x) return(ifelse( is.numeric(which(x == TRUE)), substring(colnames(subframe)[which(x == TRUE)], nchar(name) + 1), NA)))
  return(sub)
}

transform_dot = function(dat, name)
{
  subframe = dat[,grep(name, colnames(dat))]
  sub = apply(subframe, 1, function(x) return(substring(colnames(subframe)[which(x == TRUE)], regexpr("\\.[^\\.]*$", colnames(subframe)[which(x == TRUE)]) + 1)))
  return(sub)
}


dat = read.csv("vcdb.csv")

dat = dat[,names(dat) != "X"]
dat = dat[,-grep("action.environmental", colnames(dat))]
dat = dat[,-grep("action.error", colnames(dat))]
dat = dat[,-grep("action.hacking", colnames(dat))]
dat = dat[,-grep("action.malware", colnames(dat))]
dat = dat[,-grep("action.misuse", colnames(dat))]
dat = dat[,-grep("action.physical", colnames(dat))]
dat = dat[,-grep("action.social", colnames(dat))]
dat = dat[,-grep("action.unknown", colnames(dat))]
dat = dat[,-grep("actor", colnames(dat))]
dat = dat[,-grep("campaign", colnames(dat))]
dat = dat[,-grep("confidence", colnames(dat))]
dat = dat[,-grep("control", colnames(dat))]
dat = dat[,-grep("corrective", colnames(dat))]
dat = dat[,-grep("cost", colnames(dat))]
dat = dat[,-grep("impact", colnames(dat))]
dat = dat[,-grep("plus", colnames(dat))]
dat = dat[,-grep("targeted", colnames(dat))]
dat = dat[,-grep("timeline.compromise", colnames(dat))]
dat = dat[,-grep("timeline.exfiltration", colnames(dat))]
dat = dat[,-grep("timeline.containment", colnames(dat))]
dat = dat[,-grep("security_incident", colnames(dat))]
dat = dat[,-grep("victim.revenue.iso_currency_code", colnames(dat))]
dat = dat[,-grep("victim.industry2", colnames(dat))]
dat = dat[,-grep("victim.industry3", colnames(dat))]
dat = dat[,-grep("pattern.", colnames(dat))]
dat = dat[,-grep("incident_id", colnames(dat))]
dat = dat[,-grep("notes", colnames(dat))]
dat = dat[,-grep("reference", colnames(dat))]
dat = dat[,-grep("schema_version", colnames(dat))]
dat = dat[,-grep("source_id", colnames(dat))]
dat = dat[,-grep("timeline.incident.day", colnames(dat))]
dat = dat[,-grep("timeline.incident.time", colnames(dat))]
dat = dat[,-grep("victim.locations_affected", colnames(dat))]
dat = dat[,-grep("victim.secondary", colnames(dat))]
dat = dat[,-grep("victim.region", colnames(dat))]
dat = dat[,-grep("victim.orgsize.", colnames(dat))]
dat = dat[,-grep("victim.revenue.amount", colnames(dat))]

dat$victim.country = transform_row(dat, "victim.country.")
dat = dat[,-grep("victim.country.", colnames(dat))]

dat = dat[dat$victim.country == "US",]
dat = dat[,-grep("victim.country", colnames(dat))]

dat$victim.employee_count = transform_dot(dat, "victim.employee_count.")
dat = dat[,-grep("victim.employee_count.", colnames(dat))]

dat$action = transform_row(dat,"action.")
dat = dat[,-grep("action.", colnames(dat))]

dat$target = transform_row(dat,"attribute.confidentiality.data.variety.")
dat$discovery = transform_row(dat,"discovery_method.")
dat$disclosed = transform_row(dat,"attribute.confidentiality.data_disclosure.")
dat$records = dat$attribute.confidentiality.data_total
dat$data_status = transform_row(dat,"attribute.confidentiality.state.")
dat$victim = dat$attribute.confidentiality.data_victim

dat = dat[,-grep("asset", colnames(dat))]
dat = dat[,-grep("attribute", colnames(dat))]
dat = dat[,-grep("discovery", colnames(dat))]

dat = dat[!is.na(dat$timeline.incident.month),]

dat = dat[as.numeric(dat$timeline.incident.year) >= 2007,]

names(dat)=c("summary","month","year","industry","state","name","type","pattern","employees","action","target","disclosed","records","data_status","victim")

dat$month = factor(dat$month)
dat$year = factor(dat$year)
dat$employees = factor(dat$employees)
dat$industry = factor(dat$industry)
dat$records = factor(dat$records)
dat$type = factor(dat$type)


barplot(table(dat$month,dat$year), main = "Incidents over time")


splits = split_data(dat,c(.8,.2))
tr_dat = splits[[1]]
te_dat = splits[[2]]

fit = naiveBayes(type ~ victim+employees, data=tr_dat)

barplot(fit$tables$month)

predicted = predict(fit, newdata = te_dat)
actuals = te_dat$type

mean(predicted==actuals)

table(predicted,actuals)
