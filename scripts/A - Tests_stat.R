data=readRDS("output/6-all-dyad-data.Rds")
summary(data)
names(data)
str(data)

### Packages

library(lme4)
library(ggplot2)
library(visreg)

### H2/2: CARIBOU ASSOCIATE ACCORDING TO THEIR SIZE?


### We work with caribou in the same HR (udoi>1)
HRpos=subset(data,udoi>0)  

### Data exploration

hist(HRpos$sri, breaks = 20) #lot of zeros

plot1=ggplot(HRpos,aes(x=udoi,y=sri, color=diff_sum_heart_length))+geom_point()
plot1+scale_color_gradient(low="gold", high="dark red")

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

mod1.3=lmer(sqrt(sri)~diff_sum_heart_length+udoi +(1|dyadID), data=HRpos)
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

##visualize interaction for diff in length
#visreg(mod1.2,"udoi" ,by="diff_total_length")
#visreg(mod1.2,"diff_total_length", by="udoi")


### H1/ SURVIVAL ANALYSIS PART


### Packages
library(survival)
library(survminer)

Survie=readRDS("output/07-dyads.Rds")
head(Survie)
names(Survie)
hist(Survie$runCount)


# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = Survie$runCount, event = Survie$censored)


#if data is closed and then open
fit1 <- survfit(surv_object ~ habitat, data = Survie)
summary(fit1)

ggsurvplot(fit1, data = Survie,conf.int=TRUE, pval = TRUE)


### to do: recheck which variable I take in the big data

