
### Packages
library(survival)
library(survminer)
library(data.table)

# Input data
SURVI=readRDS("output/07-dyads.Rds")



# Duration of a dyad acording to habitat openness (K-M curves) ------------

# one line = one dyad run
survi=SURVI[(start)]
names(survi)
survi=survi[,.(ANIMAL_ID, NN,dyadID,Year,DyadDominantLC,censored,runCount,dyadrunid)]
# remove duplicated rows
duplicated(survi)
dim(survi)
survi=unique(survi)
dim(survi)

# Put runcount into hours
survi[ ,runCount:= runCount*2]

str(survi)

# change class
survi$Year=as.factor(survi$Year)
survi$DyadDominantLC=as.factor(survi$DyadDominantLC)
survi$censored=as.integer(survi$censored)

#check
survi[ , .N, by=censored] # none are censored

# Survival analysis -------------------------------------------------------

surv_object <- Surv(time = survi$runCount, event = survi$censored)
surv_object  # crosses= censored ind

fit <- survfit(surv_object ~DyadDominantLC , data = survi)
fit 
# mediam = 6 hours
# 80 events in closed
# 1564 in open

survi[ ,.N , by = Year]


ggsurvplot(fit, data = survi,conf.int=TRUE, 
           pval = TRUE,legend.title = 'Habitat type',
           legend.labs=c('closed','open'),
           ggtheme = theme_bw(),
           pval.coord = c(110, 0.8),pval.size=4)

survdiff(surv_object ~ DyadDominantLC, data=survi)


# same duration

range(survi$runCount)
# 4 - 422 hours
