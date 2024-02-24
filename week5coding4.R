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

# add potential confounders (model2)
library(survival)
data$sex = factor(data$sex,levels=c("2","1"))
#change sex into factor variable, and set 2 as the reference level.
fit2 = coxph(Surv(followup,chdfate)~dbp.c+sex+age+bmi,
             data=data,ties="efron")
summary(fit2)

-2*logLik(fit2)

# test interactions (model3)
fit3 = coxph(Surv(followup,chdfate)~dbp.c+sex+age
             +bmi+dbp.c*bmi,
             data=data,ties="efron")
summary(fit3)
-2*logLik(fit3)

# extract information for hand calculating HR
fit3$coefficients[c(6,15)]
fit3$var[6,6]
fit3$var[15,15]
fit3$var[6,15]


# hazard ratio
library(rms)
dd = datadist(data)
options(datadist="dd")
ctr = contrast(cph(Surv(followup,chdfate)~dbp.c+sex
                   +age+bmi+dbp.c*bmi,data=data,
                   ties="efron"),
               list(dbp.c=7,bmi=30), 
               list(dbp.c=1,bmi=30))
print(ctr, fun=exp)

# Estimate conditional survival function 
# survival curve at mean values or reference cell
library(ggsurvfit)
survfit2(fit3, newdata=data.frame(dbp.c="1",sex="2",
                                  age=46.0301,bmi=25.6317)) %>% 
  ggsurvfit() +
  add_confidence_interval() +
  scale_y_continuous(limits=c(0,1))

# output the survival table
fit3.newdata = survfit(fit3, newdata=data.frame(dbp.c="1",sex="2",
                                                 age=46.0301,bmi=25.6317))
summary(fit3.newdata)

# two predicted curves in one plot
library(survminer)
id1 = survfit(fit3, newdata=data.frame(dbp.c="1",sex="1",
                                       age=50,bmi=25))
id2 = survfit(fit3, newdata=data.frame(dbp.c="2",sex="2",
                                       age=60,bmi=30))
ggsurvplot_combine(list(id1, id2),censor=F,conf.int=T)

