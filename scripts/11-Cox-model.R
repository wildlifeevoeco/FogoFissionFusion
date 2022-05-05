# Packages

library(coxme)
library(data.table)
library(AICcmodavg)
library(ggplot2)
library(survival)


# input files
COX=readRDS('output/07-intervals.Rds')
body=readRDS('output/6-all-dyad-data.Rds')
cox=merge(COX,body, by=c('dyadID','Year'))

#COX[ ,.N, by=ED.value]


# Fission event = 1
cox[ ,stayedTogether:=ifelse(stayedTogether==TRUE,0,1)]
cox[ , fission:= stayedTogether]
cox[ , diff_size:= diff_sum_heart_length]

#same scale for contagion and openness
cox[,contag:= contag/100]

# remove NA
cox <- cox[!is.na(dyadPropOpen)]
cox <- cox[!is.na(ShanIndex)]
cox <- cox[!is.na(diff_size)]
cox <- cox[!is.na(contag)]

hist(cox$contag)

# Survival analysis Cox PHM -----------------------------------------------

str(cox)
cox$fission=as.integer(cox$fission)
cox$Year=as.factor(cox$Year)

surv_object <-Surv(cox$start, cox$stop, cox$fission)

## If HR<1 = less risk that the dyad does not survive = stay longer together
## exp(coeff) = hazard ratio in the output


# Backward selection from the interactions that make sense biologicaaly

m1<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+ sri*diff_size+sri*ShanIndex+sri*contag+
               sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)  
AIC(m1) #3506.501
# - sri*ShanIndex

m2<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               sri*diff_size+sri*contag+
               sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox) 
AIC(m2) # 3504.635
#- sri*contag

m3<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               sri*diff_size+
               sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox) 
AIC(m3)# 3502.772
# - sri*size

m4<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox) 

AIC(m4)# 3502.298
# -sri*open

m5<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox) 

AIC(m5)# 3500.976
# -size*ShanIndex

m6<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               diff_size*contag+(1|dyadID)+(1|Year), data=cox) 

AIC(m6)# 3500.554
#- size*contag

m7<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
               (1|dyadID)+(1|Year), data=cox)
AIC(m7)# 3499.792
# - size
# ===> final model


#Check of the proportional hazards assumptions

m7<-coxph(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen, data=cox)
cox.zph(m7)


# m7                  coef exp(coef)  se(coef)     z      p
#sri          -1.703383294 0.1820665 0.639112738 -2.67 0.0077
#diff_size     0.004127345 1.0041359 0.007372678  0.56 0.5800
#ShanIndex     0.516934180 1.6768788 0.222095625  2.33 0.0200
#contag        0.401085033 1.4934443 0.426264086  0.94 0.3500
#dyadPropOpen  0.400738669 1.4929271 0.216653649  1.85 0.0640

exp(confint(m7, level=0.95))
