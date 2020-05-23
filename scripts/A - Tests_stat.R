

#input data
data=readRDS("output/6-all-dyad-data.Rds")

summary(data)
names(data)
str(data)
names(data)
### Packages
library(devtools)
devtools::install_github("cardiomoon/ggiraphExtra")
library(lme4)
library(ggplot2)
library(visreg)
library(ggiraphExtra)
library(ggeffects)

### H2/2: CARIBOU ASSOCIATE ACCORDING TO THEIR SIZE?


### We work with caribou in the same HR (udoi>1)
HRpos=subset(data,udoi>0)  

### Data exploration

hist(HRpos$sri, breaks = 20) #lot of zeros


plot2=ggplot(HRpos,aes(x=diff_sum_heart_length,y=sri, color=udoi))+geom_point()
plot2+scale_color_gradient(low="gold", high="dark red")


### Models, how varies SRI as a function of the diference in size and HR?
# with diff in (heart girth + total length)

mod1=lmer(sri~diff_sum_heart_length *udoi +(1|dyadID), data=HRpos)
mod1.1=lmer(log(sri+1)~diff_sum_heart_length*udoi +(1|dyadID), data=HRpos)
mod1.2=lmer(sqrt(sri)~diff_sum_heart_length*udoi +(1|dyadID), data=HRpos)

par(mfrow=c(1,3))
qqnorm(residuals(mod1),ylab="Residuals")   #lame
qqline(residuals(mod1))

qqnorm(residuals(mod1.1),ylab="Residuals")   #lame
qqline(residuals(mod1.1))

qqnorm(residuals(mod1.2),ylab="Residuals")   #great
qqline(residuals(mod1.2))

plot(mod1.2)    #NICE

# p-value
summary(mod1.2)
Vcov <- vcov(mod1.2, useScale = FALSE)
betas <- fixef(mod1.2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)

## udoi ***, delta NS, interaction almost *
##NB: interaction * with total length but not combined measurements or volume

# Remove interaction NS

mod1.3=lmer(sqrt(sri)~diff_sum_heart_length+udoi+ (1|dyadID), data=HRpos)
mod1.4=lmer(sqrt(sri)~scale(diff_volume)+udoi +(1|dyadID), data=HRpos)


# p value
summary(mod1.3)
Vcov <- vcov(mod1.3, useScale = FALSE)
betas <- fixef(mod1.3)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)

#                      betas    se   zval  pval
#(Intercept)           0.059 0.013  4.438 0.000
#diff_sum_heart_length 0.002 0.001  2.292 0.022  
#udoi                  0.246 0.014 18.010 0.000 

## both udoi and delta *** with similar p values for diff volume and diff in combined measurements
## ====> Caribou associate when in same HR and big difference in size

#plots= ...


### H1/ SURVIVAL ANALYSIS PART


### Packages
library(survival)
library(survminer)
library(data.table)
library(AICcmodavg)
#input data

datasurv=readRDS("output/07-dyads.Rds")
names(datasurv)

# merge and clean
survivaldata=merge(data, datasurv, by=c("dyadID",'Year'))
survi=survivaldata[,.(dyadID,Year,diff_sum_heart_length,DyadDominantLC,censored,runCount,sri)]

# remove duplicated rows
duplicated(survi)
dim(survi)
survi=unique(survi)
dim(survi)

# change class
survi$Year=as.factor(survi$Year)
survi$DyadDominantLC=as.factor(survi$DyadDominantLC)
survi=na.omit(survi)
dim(survi)
survi$censored=as.integer(survi$censored)
str(survi)


##### FIRST ATTEMPT:fit survival analysis using the Kaplan-Meier method
##### UNIVARIATE:ONLY LANDCOVER (only categorical variable)

surv_object <- Surv(time = survi$runCount, event = survi$censored)
surv_object  # crosses= censored ind

fit1 <- survfit(surv_object ~DyadDominantLC , data = survi)
summary(fit1) 

ggsurvplot(fit1, data = survi,conf.int=TRUE, 
           pval = TRUE,legend.title = 'Habitat type',
           legend.labs=c('closed','open'),surv.median.line = 'hv',
           ggtheme = theme_bw(),
           pval.coord = c(110, 0.8),pval.size=4)



# ***: stay longer together when in open habitat


#### SECOND ATTEMPTS: fit CoxPH 
#### MULTIVARIATE


surv_object <- Surv(time = survi$runCount, event = survi$censored)

# Full Model + model selection

fit0<-coxph(surv_object~(Year+DyadDominantLC+diff_sum_heart_length+sri)^2, data=survi)
fit0
fit1<-update(fit0,.~.-diff_sum_heart_length:sri)
fit1
fit2<-update(fit1,.~.-DyadDominantLC:sri)
fit2
fit3<-update(fit2,.~.-Year:sri) # we kncew that alread SRI is calculated accross years
fit3
fit4<-update(fit3,.~.-Year:diff_sum_heart_length)  # sure 
fit4
fit5<-update(fit4,.~.-Year:DyadDominantLC)
fit5
fit6<-coxph(surv_object~Year+DyadDominantLC+diff_sum_heart_length+sri, data=survi)

### Compare AIC 

fit<-list()
fit[[1]]<-fit1
fit[[2]]<-fit2
fit[[3]]<-fit3
fit[[4]]<-fit4
fit[[5]]<-fit5
fit[[6]]<-fit6

aictab(fit, 1:6)

##### WINNER: lowest AIC= model 5 

fit5 
# coxph(formula = surv_object ~ Year + DyadDominantLC + diff_sum_heart_length + 
# sri + DyadDominantLC:diff_sum_heart_length, data = survi)
## IF HR<1 = less risk that the dyad does not survive = stay longer together

# => spend more time together when:
# - open habitat
# - we are in 2019 (??)
# - have higher SRI
# - in open habitat we are of different sizes


# Make levels for SRI to visualize the forestplot

survi$sri=cut(survi$sri,3,labels=c("low","medium","high"))
ggforest(fit5, data = survi)

