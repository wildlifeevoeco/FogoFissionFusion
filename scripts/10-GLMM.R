# === Model - glmm --------------------------------------------------------



# Packages ----------------------------------------------------------------
library(lme4)
library(ggplot2)
library(visreg)
library(data.table)
library(sjstats)
library(performance)

#input data
data=readRDS("output/09-all-dyad-data.Rds")

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

mod1=lmer(sri~Difference *udoi + Difference+udoi+(1|dyadID)+(1|Year), data=HRpos)

# with log
mod1.1=lmer(log(sri+1)~Difference*udoi + Difference+udoi+(1|dyadID)+(1|Year), data=HRpos) 

# with sqrt
mod1.2=lmer(sqrt(sri)~udoi*Difference+udoi+ Difference +(1|dyadID)+(1|Year), data=HRpos) 


# par(mfrow=c(1,3))
qqnorm(residuals(mod1),ylab="Residuals")   
qqline(residuals(mod1))

qqnorm(residuals(mod1.1),ylab="Residuals")
qqline(residuals(mod1.1))

qqnorm(residuals(mod1.2),ylab="Residuals")
qqline(residuals(mod1.2))


plot(mod1.2)

shapiro.test((residuals(mod1.2)))

# p-value
Vcov <- vcov(mod1.2, useScale = FALSE)
betas <- fixef(mod1.2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)

r2(mod1.2)

# ====> Caribou associate more often when HR overlap is bigger and big difference in size



# Plots of the fixed effects ----------------------------------------------
# Backtranformation of the sqrt of the response
square <- function(x){
  return(x**2)
}

HRpos$UDOI=HRpos$udoi
mod1.A=lm(sqrt(sri)~Difference*UDOI, data=HRpos)
visreg(mod1.A,"Difference", by="UDOI", 
       
       breaks = c(0.1, 0.5, 1.2),
       
       trans=square,xlab="Difference in body size (cm)",
       ylab="Simple ratio index (SRI)", overlay = TRUE, partial = FALSE, rug=FALSE, 
       frame.plot=FALSE,legend=TRUE,
       line=list(lty=1:3, col="black", lwd=0.5))



# -------------------------------------------------------------------------

mod1

mod1.1

mod1.2
