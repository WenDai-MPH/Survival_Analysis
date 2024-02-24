# read in date set
# replace datapath with your data directory
datapath = "~/Documents/P8110Fall2023/datasets/"
datafile = paste0(datapath,"framingham.csv")
data = read.csv(datafile,header=T)

# create norminal variable dbp.c
data$dbp.c = cut(data$dbp, 
                 c(min(data$dbp),60,70,80,90,100,
                   110,max(data$dbp)),
                 labels=1:7,include.lowest=T)

# Cox model with dbp.c
library(survival)
fit = coxph(Surv(followup,chdfate)~dbp.c,data=data,
            ties="efron")
summary(fit)
-2*logLik(fit)
fit0 = coxph(Surv(followup,chdfate)~1,data=data,
             ties="efron")
-2*logLik(fit0)
G = 2*(logLik(fit)-logLik(fit0))
1-pchisq(G,df=6)
AIC(fit)
BIC(fit)
AIC(fit0)
BIC(fit0)
fit$var

# contrast between different levels of dbp
library(rms)
ctr = contrast(cph(Surv(followup,chdfate)~dbp.c,
                   data=data,ties="efron"),
               list(dbp.c=7), list(dbp.c=2))
print(ctr, fun=exp) # use fun=exp to get hazard ratio
