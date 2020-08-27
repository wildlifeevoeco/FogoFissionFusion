

### Packages
library(lme4)
library(ggplot2)
library(visreg)
library(sjstats)
library(data.table)

#input data
data=readRDS("output/6-all-dyad-data.Rds")

summary(data)
str(data)

# Does caribou associate according to size similarity? --------------------
# Expected = SRI is higher when diff in body size is small 

# We work with caribou in the same HR (udoi>0)
HRpos=subset(data,udoi>0)  
HRpos[ , Difference:= diff_total_length]

# Data exploration
hist(HRpos$sri, breaks = 20)

plot2=ggplot(HRpos,aes(x=diff_total_length,y=sri, color=udoi))+geom_point()
plot2+scale_color_gradient(low="gold", high="dark red")


#  GLMMs ------------------------------------------------------------------


# with diff in (heart girth + total length)
mod1=lmer(sri~diff_total_length *udoi +(1|dyadID), data=HRpos)
mod1.1=lmer(log(sri+1)~diff_total_length*udoi +(1|dyadID), data=HRpos)
mod1.2=lmer(sqrt(sri)~udoi*Difference +(1|dyadID), data=HRpos)

par(mfrow=c(1,3))
qqnorm(residuals(mod1),ylab="Residuals")   #lame
qqline(residuals(mod1))

qqnorm(residuals(mod1.1),ylab="Residuals")   #lame
qqline(residuals(mod1.1))

qqnorm(residuals(mod1.2),ylab="Residuals")   #great
qqline(residuals(mod1.2))

plot(mod1.2)    #nice homoscedasticity

shapiro.test((residuals(mod1.2)))
# p-value
Vcov <- vcov(mod1.2, useScale = FALSE)
betas <- fixef(mod1.2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)

r2(mod1.2)
#                 betas    se   zval  pval
#(Intercept)      0.092 0.018  5.246 0.000
#Difference      -0.002 0.002 -1.043 0.297
#udoi             0.198 0.021  9.388 0.000
#Difference:udoi  0.007 0.003  2.813 0.005



# ====> Caribou associate more often when HR overlap is bigger and big difference in size


# Plots of the fixed effects ----------------------------------------------

#Backtranformation of the sqrt of the response
square <- function(x){
  return(x**2)
}

# random effect removed for plot, otherwise, no CI on the graph
HRpos$UDOI=HRpos$udoi
mod1.A=lm(sqrt(sri)~Difference*UDOI, data=HRpos)

# Can't put them on the same line with ggplot2 so two steps 
v=visreg(mod1.A,"Difference", by='UDOI',trans= square, xlab="Difference in body size (cm)",
ylab='Simple ratio index', partial=FALSE, gg=TRUE, overlay=TRUE, legend=FALSE,frame.plot=FALSE,xpd = TRUE,points=list(size=0.8, pch=20))+theme_classic() 


black<- element_text( color = "black", size = 10)
v + theme(axis.text = black)




