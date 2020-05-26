

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


### H2/2: CARIBOU ASSOCIATE ACCORDING TO THEIR SIZE?


### We work with caribou in the same HR (udoi>1)
HRpos=subset(data,udoi>0)  

### Data exploration

hist(HRpos$sri, breaks = 20) #lot of zeros

plot2=ggplot(HRpos,aes(x=diff_sum_heart_length,y=sri, color=udoi))+geom_point()
plot2+scale_color_gradient(low="gold", high="dark red")

### Models, how varies SRI as a function of the difference in size and HR?
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


##### PLOTS

#Backtranformation of the sqrt of the response

square <- function(x){
  return(x**2)
}

# random effect removed for plot, otherwise, no CI on the graph

mod1.A=lm(sqrt(sri)~diff_sum_heart_length+udoi, data=HRpos)

#nCan't pu them on the same line with ggplot2 so two steps 

visreg(mod1.A,"udoi",trans= square, xlab="Home range overlap",ylab='SRI', partial=TRUE,fill.par=list(col="light blue"), gg=TRUE)
visreg(mod1.A,"diff_sum_heart_length",xlab="Difference in body mass (estimated) - cm",trans= square, ylab='SRI', partial=TRUE,fill=list(col="light blue"), gg=TRUE)




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
names(survi)

# remove duplicated rows
duplicated(survi)
dim(survi)
survi=unique(survi)
dim(survi)

# change class
survi$Year=as.factor(survi$Year)
survi$DyadDominantLC=as.factor(survi$DyadDominantLC)
#survi=na.omit(survi)
#dim(survi)
survi$censored=as.integer(survi$censored)
str(survi)


##### FIRST ATTEMPT:fit survival analysis using the Kaplan-Meier method
##### UNIVARIATE:ONLY LANDCOVER (only categorical variable)

surv_object <- Surv(time = survi$runCount, event = survi$censored)
surv_object  # crosses= censored ind

fit <- survfit(surv_object ~DyadDominantLC , data = survi)
summary(fit) 

ggsurvplot(fit, data = survi,conf.int=TRUE, 
           pval = TRUE,legend.title = 'Habitat type',
           legend.labs=c('closed','open'),surv.median.line = 'hv',
           ggtheme = theme_bw(),
           pval.coord = c(110, 0.8),pval.size=4)



# ***: stay longer together when in open habitat


#### SECOND ATTEMPTS: fit CoxPH 
#### MULTIVARIATE

surv_object <- Surv(time = survi$runCount, event = survi$censored)

# Full Model + model selection

fit7<-coxph(surv_object~(Year+DyadDominantLC+diff_sum_heart_length+sri)^2, data=survi)
fit7
fit1<-update(fit7,.~.-diff_sum_heart_length:sri)
fit1
fit2<-update(fit1,.~.-DyadDominantLC:sri)
fit2
fit3<-update(fit2,.~.-Year:diff_sum_heart_length)  # sure 
fit3
fit4<-update(fit3,.~.-Year:DyadDominantLC)
fit4
fit5<-coxph(surv_object~Year+DyadDominantLC+diff_sum_heart_length+sri, data=survi)

### Compare AIC 

fit<-list()
fit[[1]]<-fit1
fit[[2]]<-fit2
fit[[3]]<-fit3
fit[[4]]<-fit4
fit[[5]]<-fit5
fit[[6]]<-fit7
aictab(fit, 1:6)

##### WINNER: lowest AIC= model 4 

fit4
# coxph(formula = surv_object ~ Year+DyadDominantLC + diff_sum_heart_length + 
#sri + Year:sri+ DyadDominantLC:diff_sum_heart_length, data = survi)
## IF HR<1 = less risk that the dyad does not survive = stay longer together
## exp(coeff) = hazard ratio in the output

# => spend more time together when:
# - open habitat
# - have higher SRI + 2019 but makes no sense
# - higher SRI
# - in open habitat we are of different sizes
# => spend less time when:
# - big diiference in size in closed habitat?
# higher diff in size


# Make levels for SRI to visualize the forestplot

#survi$sri=cut(survi$sri,3,labels=c("low","medium","high"))
#fit4<-update(fit3,.~.-Year:DyadDominantLC)
#ggforest(fit4, data = survi)

visreg(fit4, "diff_sum_heart_length",by= "DyadDominantLC",overlay=TRUE, partial=FALSE, gg=TRUE, ylab="Hazard ratio",xlab="difference in estimated body mass", legend="Habitat type" )
ggsurvplot(fit4, data = survi,conf.int=TRUE) 
visreg(fit4, "diff_sum_heart_length", "DyadDominantLC", ylab="Log(Hazard ratio)",xlab="difference in estimated body mass", legend="Habitat type" )


### CHECKING ASSUMPTIONS

test=cox.zph(fit4)
#assumption violated 
#because significance of the relationship between residuals and time
# we want non significance
ggcoxzph(test)
#pattern of the bold line with time= asssumption not met


# coxme for mixed model

library(coxme)

test1=coxph(surv_object ~ Year+DyadDominantLC + diff_sum_heart_length + 
        Year*sri  + DyadDominantLC:diff_sum_heart_length + frailty(dyadID), data = survi)
cox.zph(test1) # does not work with frailty for some unknown reason

anova(fit4,test1) # Test 1 is better

test2=coxme(surv_object ~  Year+ DyadDominantLC + diff_sum_heart_length + 
             sri + Year:sri + DyadDominantLC:diff_sum_heart_length +(1|dyadID), data = survi)
anova(fit4,test2)  #Test 2 is better

###BUT no way to check asumptions with coxme


#plot cumulaive incidence curve because if cumulative risk differs in time, don't care
#about assumptions : ,ot the case here...
library(cmprsk)
cuminc(survi$runCount,survi$censored, group=survi$DyadDominantLC)
ci_fit <- 
  cuminc(
    ftime = survi$runCount, 
    fstatus = survi$censored, 
    
  )
plot(ci_fit)

#categoriaclization for the forest plot

#survi$sri=cut(survi$sri,3,labels=c("low","medium","high"))
#survi$diff_sum_heart_length=cut(survi$diff_sum_heart_length, c(0,10,20,40),labels=c("low","medium","high"))
#hist(survi$diff_sum_heart_length)
#table(survi$diff_sum_heart_length)
## --> CHANGES THE P VALUES TO PUT IN CATEGORICAL
ggforest(fit4, data = survi)
