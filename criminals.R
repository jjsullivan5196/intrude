transform_row = function(dat, name)
{
  subframe = dat[,grep(name, colnames(dat))]
  sub = apply(subframe, 1, function(x) return(substring(colnames(subframe)[which(x == TRUE)], nchar(name) + 1)))
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
dat = dat[,-grep("asset", colnames(dat))]
dat = dat[,-grep("attribute", colnames(dat))]
dat = dat[,-grep("campaign", colnames(dat))]
dat = dat[,-grep("confidence", colnames(dat))]
dat = dat[,-grep("control", colnames(dat))]
dat = dat[,-grep("corrective", colnames(dat))]
dat = dat[,-grep("cost", colnames(dat))]
dat = dat[,-grep("discovery", colnames(dat))]
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

dat = dat[!is.na(dat$timeline.incident.month),]

dat = dat[as.numeric(dat$timeline.incident.year) >= 2007,]

dat$timeline.incident.month = factor(dat$timeline.incident.month)
dat$timeline.incident.year = factor(dat$timeline.incident.year)
dat$victim.employee_count = factor(dat$victim.employee_count)

rownames(dat) = NULL