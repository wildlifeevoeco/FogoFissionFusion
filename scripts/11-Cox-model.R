# === Model - cox ---------------------------------------------------------



# Packages ----------------------------------------------------------------
library(coxme)
library(data.table)
library(AICcmodavg)
library(ggplot2)
library(survival)
library(MuMIn)


# Data --------------------------------------------------------------------
# input files
COX = readRDS('output/08-intervals.Rds')
body = readRDS('output/09-all-dyad-data.Rds')
cox = merge(COX, body, by = c('dyadID', 'Year'))

# Fission event = 1
cox[, stayedTogether := ifelse(stayedTogether == TRUE, 0, 1)]
cox[, fission := stayedTogether]
cox[, diff_size := diff_sum_heart_length]

#same scale for contagion and openness
cox[, contag := value / 100]

# remove NA
cox <- cox[!is.na(dyadPropOpen)]
cox <- cox[!is.na(ShanIndex)]
cox <- cox[!is.na(diff_size)]
cox <- cox[!is.na(contag)]

hist(cox$contag)



# Survival analysis Cox PHM -----------------------------------------------

str(cox)
cox$fission = as.integer(cox$fission)
cox$Year = as.factor(cox$Year)

surv_object <- Surv(cox$start, cox$stop, cox$fission)

## If HR<1 = less risk that the dyad does not survive = stay longer together
## If the hazard ratio for a predictor is close to 1 then that predictor does not affect survival.
## exp(coeff) = hazard ratio in the output


# Backward selection from the interactions that make sense biologicaly

m1<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+ sri*diff_size+sri*ShanIndex+sri*contag+
            sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)  
AIC(m1)
AICc(m1)

m2<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            sri*diff_size+sri*contag+
            sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)
AIC(m2)
AICc(m2)

#- sri*contag

m3<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            sri*diff_size+
            sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)
AIC(m3)
AICc(m3)
# - sri*size

m4<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            sri*dyadPropOpen+diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)

AIC(m4)
AICc(m4)
# -sri*open

m5<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            diff_size*ShanIndex+diff_size*contag+(1|dyadID)+(1|Year), data=cox)

AIC(m5)
AICc(m5)
# -size*ShanIndex

m6<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            diff_size*contag+(1|dyadID)+(1|Year), data=cox)

AIC(m6)
AICc(m6)
#- size*contag

m7<-coxme(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen+
            (1|dyadID)+(1|Year), data=cox)
AIC(m7)
AICc(m7)
# - size

# ===> final model
m7

m8<-coxme(surv_object~ sri+ShanIndex+contag+dyadPropOpen+
            (1|dyadID)+(1|Year), data=cox)
m8
AIC(m8)
AICc(m8)

m9<-coxme(surv_object~ sri+ShanIndex+dyadPropOpen+
            (1|dyadID)+(1|Year), data=cox)
m9
AICc(m9)

m10<-coxme(surv_object~ sri+ShanIndex+
            (1|dyadID)+(1|Year), data=cox)
m10
AICc(m10)

#Check of the proportional hazards assumptions

mod10<-coxph(surv_object~ sri+diff_size+ShanIndex+contag+dyadPropOpen, data=cox)
cox.zph(mod10)


exp(confint(m9, level=0.95))


# -------------------------------------------------------------------------
m1

m2

m3

m4

m5

m6

m7

m8

m9

AICc(m1,m2,m3,m4,m5,m6,m7)



#### Variance inflation factor for mixed models. Run the code and then use vif.mer(model)

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif.mer(m7)
summary(m7)
