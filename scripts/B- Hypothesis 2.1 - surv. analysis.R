
### H1/ SURVIVAL ANALYSIS PART

### Packages
library(survival)
library(survminer)
library(data.table)
library(AICcmodavg)
library(coxme)

#output

DATA=readRDS("output/07-intervals.Rds") 
bodymass=readRDS("output/6-all-dyad-data.Rds") 


#rearrangement
names(DATA)
names(bodymass)
# if they stayed together they survived, 0
# if there is an event, fission, 1
DATA[,stayedTogether:=ifelse(min2=='TRUE',0,1)]

#change name column start
colnames(DATA)[5] <- "begin"

dataS=merge(DATA,bodymass,by=c('dyadID','Year'))
names(dataS)
str(dataS)
dataS$Year=as.factor(dataS$Year)
dataS$stayedTogether=as.integer(dataS$stayedTogether)

str(dataS) # ok 


# Survival analysis
surv_object <-Surv(dataS$begin, dataS$stop, dataS$stayedTogether)


#test
fit0= coxme(surv_object~dyadPropOpenStop+ diff_sum_heart_length+ShannonStop + (1|dyadID), data=dataS)
fit0


#the rest does not matter
fit1=coxme(surv_object~(dyadPropOpenStop+ diff_sum_heart_length+ShannonStop)^2 +(1|dyadID), data=dataS)
fit1
#fit2=update(fit1,.~.-ShanIndex:pcontig)
#fit2
#fit3=update(fit2,.~.-diff_sum_heart_length:ShanIndex)
#fit3
#fit4=update(fit3,.~.-diff_sum_heart_length:pcontig)
#fit4
#fit5=update(fit4,.~.-dyadPropOpenStop:diff_sum_heart_length)
#fit
#fit6=update(fit5,.~.-dyadPropOpenStop:ShanIndex)
#fit6
#fit7=update(fit6,.~.-dyadPropOpenStop:pcontig)
#fit7
##fit8=update(fit7,.~.-ShanIndex)
fit8

