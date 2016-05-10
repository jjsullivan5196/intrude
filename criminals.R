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

#vic.countries = dat[,grep("victim.country.", colnames(dat))]
#vic.countries$country = apply(vic.countries, 1, function(x) return(substring(colnames(vic.countries)[which(x == TRUE)], 16)))
#dat = dat[,-grep("victim.country.", colnames(dat))]
#dat$victim.country = vic.countries$country

dat$victim.country = transform_row(dat, "victim.country.")

transform_row = function(dat, name)
{
  subframe = dat[,grep(name, colnames(dat))]
  sub = apply(subframe, 1, function(x) return(substring(colnames(subframe)[which(x == TRUE)], nchar(name) + 1)))
  return(sub)
}

