

### Packages
library(lme4)
library(ggplot2)
library(visreg)

#input data
data=readRDS("output/6-all-dyad-data.Rds")

summary(data)
str(data)

# Does caribou associate according to size similarity? --------------------
# Expected = SRI is higher when diff in body size is small 

# We work with caribou in the same HR (udoi>0)
HRpos=subset(data,udoi>0)  

# Data exploration
hist(HRpos$sri, breaks = 20)

plot2=ggplot(HRpos,aes(x=diff_sum_heart_length,y=sri, color=udoi))+geom_point()
plot2+scale_color_gradient(low="gold", high="dark red")


#  GLMMs ------------------------------------------------------------------


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

plot(mod1.2)    #nice homoscedasticity

# p-value
Vcov <- vcov(mod1.2, useScale = FALSE)
betas <- fixef(mod1.2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)

# udoi ***, body mass NS, interaction almost *
# NB: interaction * with total length but not combined measurements or volume

# Remove interaction NS

mod1.3=lmer(sqrt(sri)~diff_sum_heart_length+udoi+ (1|dyadID), data=HRpos)

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

##### both udoi and delta *** with similar p values for diff volume and diff in combined 
# measurements
# ====> Caribou associate more often when HR overlap is bigger and big difference in size


# Plots of the fixed effects ----------------------------------------------


#Backtranformation of the sqrt of the response
square <- function(x){
  return(x**2)
}

# random effect removed for plot, otherwise, no CI on the graph
mod1.A=lm(sqrt(sri)~diff_sum_heart_length+udoi, data=HRpos)

# Can't put them on the same line with ggplot2 so two steps 
visreg(mod1.A,"udoi",trans= square, xlab="Home range overlap",ylab='SRI', 
       partial=TRUE,fill.par=list(col="light blue"), gg=TRUE)
visreg(mod1.A,"diff_sum_heart_length",xlab="Difference in body mass (estimated) - cm",
       trans= square, ylab='SRI', partial=TRUE,fill=list(col="light blue"), gg=TRUE)




