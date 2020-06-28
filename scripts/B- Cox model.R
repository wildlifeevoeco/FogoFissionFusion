# Packages

library(coxme)
library(data.table)
library(AICcmodavg)
library(ggplot2)


# input files
COX=readRDS('output/07-intervals.Rds')
body=readRDS('output/6-all-dyad-data.Rds')
cox=merge(COX,body, by=c('dyadID','Year'))
range(cox$diff_sum_heart_length)
# Fission event = 1
cox[ ,stayedTogether:=ifelse(stayedTogether==TRUE,0,1)]
cox[ , fission:= stayedTogether]
cox[ , diff_size:= diff_sum_heart_length]
range(cox$diff_size)

# remove NA
cox <- cox[!is.na(dyadPropOpen)]
cox <- cox[!is.na(ShanIndex)]
cox <- cox[!is.na(diff_size)]

# Survival analysis Cox PHM -----------------------------------------------

str(cox)
cox$fission=as.integer(cox$fission)
cox$Year=as.factor(cox$Year)

surv_object <-Surv(cox$start, cox$stop, cox$fission)

## IF HR<1 = less risk that the dyad does not survive = stay longer together
## exp(coeff) = hazard ratio in the output
fit1<-coxme(surv_object~dyadPropOpen+ShanIndex+diff_size+sri+ (1|dyadID), data=cox)
fit1
# (ERROR MESSAGE WITH YEAR)

#                   coef exp(coef)  se(coef)     z      p
#dyadPropOpen  0.396227251 1.4862070 0.215166056  1.84 0.066
#ShanIndex     0.419484289 1.5211769 0.188850131  2.22 0.026
#diff_size     0.004078801 1.0040871 0.007349129  0.56 0.580
#sri          -1.718785129 0.1792838 0.637172681 -2.70 0.007


#fit2<-update(fit1,.~.-dyadPropOpen:ShanIndex)
#fit2
#fit3<-update(fit2,.~.-dyadPropOpen:diff_size)
#fit3
#fit4<-update(fit3,.~.-ShanIndex:sri)  
#fit4
#fit5<-update(fit4,.~.-diff_size:sri)  
#fit5
#fit6<-update(fit5,.~.-ShanIndex:diff_size)  
#fit6
#fit7<-update(fit6,.~.-dyadPropOpen:sri) 
#fit7


### Compare AIC 

fit<-list()
fit[[1]]<-fit1
fit[[2]]<-fit2
fit[[3]]<-fit3
fit[[4]]<-fit4
fit[[5]]<-fit5
fit[[6]]<-fit6
fit[[7]]<-fit7

aictab(fit, 1:7)



#  Plot risk ratio --------------------------------------------------------
# try coxph = frailty to have approched predicted values

fit1<-coxph(surv_object~dyadPropOpen+ShanIndex+diff_size+sri+ frailty(dyadID), data=cox)

all_sd<-expand.grid(ShanIndex=min(cox$ShanIndex):max(cox$ShanIndex),
                    diff_size=mean(cox$diff_size), # cause want to nknow the effect
                    dyadPropOpen=mean(cox$dyadPropOpen),
                                      sri=mean(cox$sri)) #making new data with all values kept at their mean expect snow depth

all_sd$pred <- predict(fit1,newdata=all_sd, type="risk")
all_sd$pred.se <- predict(fit1, newdata=all_sd, type="risk", se.fit=TRUE)$se.fit

A=ggplot()+
  geom_line(data=all_sd, aes(x=ShanIndex,y=pred)) + 
  geom_ribbon(data=all_sd, aes(x=ShanIndex, ymin = pred-pred.se, ymax = pred+pred.se), colour = NA,alpha = 0.3, fill="blue") +
  ylab("Risk (above baseline)") +
  xlab("Shannon index") +
  theme_classic()+
  xlim(0,2)+
  ylim(0,3)+
  geom_hline(yintercept=1, lty=2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

all_sd1<-expand.grid(sri=min(cox$sri):max(cox$sri),  # cause want to nknow the effect
                    diff_size=mean(cox$diff_size), 
                    dyadPropOpen=mean(cox$dyadPropOpen),
                    ShanIndex=mean(cox$ShanIndex)) #making new data with all values kept at their mean

all_sd1$pred <- predict(fit1,newdata=all_sd1, type="risk")
all_sd1$pred.se <- predict(fit1, newdata=all_sd1, type="risk", se.fit=TRUE)$se.fit

B=ggplot()+
  geom_line(data=all_sd1, aes(x=sri,y=pred, group=1)) + 
  geom_ribbon(data=all_sd1, aes(x=sri, ymin = pred-pred.se, ymax = pred+pred.se), colour = NA,alpha = 0.3, fill="blue") +
  xlab("SRI") +
  theme_classic()+
  xlim(0,0.4)+
  ylim(0,4)+
  geom_hline(yintercept=1, lty=2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

all_sd2<-expand.grid(diff_size=min(cox$diff_size):max(cox$diff_size),  # cause want to nknow the effect
                     sri=mean(cox$sri), 
                     dyadPropOpen=mean(cox$dyadPropOpen),
                     ShanIndex=mean(cox$ShanIndex)) #making new data with all values kept at their mean

all_sd2$pred <- predict(fit1,newdata=all_sd2, type="risk")
all_sd2$pred.se <- predict(fit1, newdata=all_sd2, type="risk", se.fit=TRUE)$se.fit

C=ggplot()+
  geom_line(data=all_sd2, aes(x=diff_size,y=pred)) + 
  geom_ribbon(data=all_sd2, aes(x=diff_size, ymin = pred-pred.se, ymax = pred+pred.se), colour = NA,alpha = 0.3, fill="blue") +
  xlab("Difference in body size") +
  ylab("Risk (above baseline)") +
  theme_classic()+
  xlim(0,40)+
  ylim(0,3)+
  geom_hline(yintercept=1, lty=2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


all_sd3<-expand.grid(dyadPropOpen=min(cox$dyadPropOpen):max(cox$dyadPropOpen),  # cause want to nknow the effect
                     diff_size=mean(cox$diff_size), 
                     sri=mean(cox$sri),
                     ShanIndex=mean(cox$ShanIndex)) #making new data with all values kept at their mean

all_sd3$pred <- predict(fit1,newdata=all_sd3, type="risk")
all_sd3$pred.se <- predict(fit1, newdata=all_sd3, type="risk", se.fit=TRUE)$se.fit

D=ggplot()+
  geom_line(data=all_sd3, aes(x=dyadPropOpen,y=pred)) + 
  geom_ribbon(data=all_sd3, aes(x=dyadPropOpen, ymin = pred-pred.se, ymax = pred+pred.se), colour = NA,alpha = 0.3, fill="blue") +
  xlab("Habitat openness") +
  theme_classic()+
  xlim(0,1)+
  ylim(0,3)+
  geom_hline(yintercept=1, lty=2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

library(patchwork)

patchwork = A + B +C + D

# Remove title from second subplot
patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

#
# Remove title from third subplot
patchwork[[4]] = patchwork[[4]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )
patchwork

# Not many individuals in 2019
number=readRDS('output/01-prep-locs.Rds')
names(number)
number=number[ ,.(Year,ANIMAL_ID)]
number=unique(number)
number[ ,.N, by=Year]
range(cox$diff_size)

