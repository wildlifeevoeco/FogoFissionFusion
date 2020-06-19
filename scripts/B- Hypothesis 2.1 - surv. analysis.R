
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


############### KM part #######


#input data
survi=readRDS("output/07-dyads.Rds")

head(survi)

#have to have 1 runcount for each runid

#need data when min=2 --> runCount =/= NA
survi=subset(survi, min2==TRUE)
survi[ ,.N,by= runCount]
survi=survi[,.(Year,dyadID,runCount,DyadDominantLC,censored,runid,mean_open)]

duplicated(survi)  
dim(survi)
survi=unique(survi)
dim(survi)

#censored=1 --> fission event happened, =0, last location is confounded with end of study period
survi[,.N,by=censored]

#check
str(survi)

# change class
survi$DyadDominantLC=as.factor(survi$DyadDominantLC)
survi$runCount=as.numeric(survi$runCount)

survi[ , runCount:=runCount*2]

##### FIRST ATTEMPT:fit survival analysis using the Kaplan-Meier method
##### UNIVARIATE:ONLY LANDCOVER (only categorical variable)

surv_object <- Surv(time = survi$runCount, event = survi$censored)
surv_object  # crosses= censored ind

fit <- survfit(surv_object ~DyadDominantLC , data = survi)
fit 

ggsurvplot(fit, data = survi,conf.int=TRUE, 
           pval = TRUE,legend.title = 'Habitat type',
           legend.labs=c('closed','open'),#surv.median.line = 'hv',
           ggtheme = theme_bw(),
           pval.coord = c(110, 0.8),pval.size=4)

# ***: stay longer together when in open habitat
#crosing curves because gret difference in variance between two groups
open=subset(survi,DyadDominantLC=='open')
var(open$runCount)
closed=subset(survi,DyadDominantLC=='closed')
var(closed$runCount)


#TEST SIGNIFICANCE
survdiff(surv_object ~ DyadDominantLC, data=survi)

ggplot(survi,aes(x = Year,fill = DyadDominantLC)) + 
  geom_bar(position = "fill") + labs(fill='Landcover type')+ ylab('Proportion')

plot(survi$mean_open~survi$runCount)
range(survi$mean_open, na.rm=TRUE)
