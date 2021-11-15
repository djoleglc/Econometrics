library(readr)
library(dplyr)
library(car)
library(caret)
library(lmtest)
library(sandwich)
library(prais)

fish <- read_csv("Econometrics/fish.csv")

data = fish 
summary(data) #check outliers, no missing data

##Question a##
mod_a = lm(lavgprc ~ t + mon + tues + wed + thurs, data = data)
summary(mod1) # days look not relevant
mod_a2 = lm(lavgprc ~ t, data = data)
anova(mod_a2, mod_a) #test joint significance of days of the week -> not significant

##Question b##
mod_b = lm(lavgprc ~ t + mon + tues + wed + thurs + wave2 + wave3, data = data)
summary(mod_b)
anova(mod_b, mod_a) # test joint significance -> significant

##Question d##
#Breusch-Godfrey test#
bgtest(mod_b)

#try manually
e_t = residuals(mod_b)[2:97] #residual
e_t1 = residuals(mod_b)[1:96] # lagged residual
mod_BG = lm(e_t ~ e_t1 + t[2:97] + mon[2:97] + tues[2:97] + wed[2:97] + thurs[2:97] + wave2[2:97] + wave3[2:97], data = data)
summary(mod_BG) #R^2 = 0.404
K = 96 * 0.404
dchisq(K, 1)

##Question e##
V_NW = NeweyWest(mod_b, 4) #Newey-West covvariance matrix
View(V_NW)
t_wave2 = coefficients(mod_b)[7] / sqrt(V_NW[7,7]) #Calculation of the t stat #Is it true?
t_wave3 = coefficients(mod_b)[8] / sqrt(V_NW[8,8])
summary(mod_b)

##Question f##
mod_PW = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs + wave2 + wave3, data = data, data$t) #Prais-Winsten estimators with 8 iteration rho = 0.6874
summary(mod_PW)
mod_PW2 = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs, data = data, data$t)
"anova function doesn't work for prais"
