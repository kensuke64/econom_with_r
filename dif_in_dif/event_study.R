# dif in dif event study
# ref: https://www.aeaweb.org/articles?id=10.1257/jep.37.2.203

#------------------------------------------------------------------------------#
# ref:
# https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#------------------------------------------------------------------------------#
library(data.table) 
library(fixest)     

dat = fread("https://raw.githubusercontent.com/LOST-STATS/LOST-STATS.github.io/master/Model_Estimation/Data/Event_Study_DiD/bacon_example.csv") 
print(dat)

# control group: states with '_nfd' == 0
# make treatment variable
dat[, treat := ifelse(is.na(`_nfd`), 0, 1)]

# time_to_treat = t - treated year
# i(): treat a variable as a factor
# ref(= a charactor)->partial matching is applied to values
dat[, time_to_treat := ifelse(treat==1, year - `_nfd`, 0)]

mod_twfe = feols(asmrs ~ i(time_to_treat, treat, ref = -1) + # add leads n lags
                     pcinc + asmrh + cases |                 # covariates    
                     stfips + year,                          # fixed effects   
                 cluster = ~stfips,                          # clustered SEs
                 data = dat)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')
