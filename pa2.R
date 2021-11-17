library(readr)
library(dplyr)
library(car)
library(caret)
library(lmtest)
library(sandwich)
library(prais)
library(AER)

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
1-pchisq(K, 1) # =0 -> rho!=0 auto regressive process

##Question e##
V_NW = NeweyWest(mod_b, 4) #Newey-West covariance matrix
View(V_NW)
t_wave2 = coefficients(mod_b)[7] / sqrt(V_NW[7,7]) #Calculation of the t stat #Is it true?
t_wave3 = coefficients(mod_b)[8] / sqrt(V_NW[8,8])
(1-pt(t_wave2, 91))*2
(1-pt(t_wave3, 91))*2
summary(mod_b)

##Question f##
mod_PW = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs + wave2 + wave3, data = data, data$t) #Prais-Winsten estimators with 8 iteration rho = 0.6874
summary(mod_PW) #R^2 = 0.1353
mod_PW2 = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs, data = data, data$t)
summary(mod_PW) #R^2 = 0.03649
"anova function doesnt work for prais"
R_0 = summary(mod_PW)$r.squared; R_1 = summary(mod_PW2)$r.squared
F = ((R_1 - R_0)/2) / ((1-R_1) / 89)
1-pf(F, 2, 89) # reject null hypothesis -> wave2 + wave3 relevant



##PART 2##
##Question g##
mod_g = lm(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t, data = data) #simultaneity price~quantity?
summary(mod_g)


##Question i##
mod_1st_step_i = lm(lavgprc ~ wave2 + mon + tues + wed + thurs + t, data = data)
summary(mod_1st_step) # t value = 4.758 > 3.16 -> strong instrument

#Durbin-Wu-Hausman test
Nu_hat = residuals(mod_1st_step)
mod_DWH = lm(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t + Nu_hat, data = data)
summary(mod_DWH) # price is exogenous, we can use OLS?

##Question j##
mod_IV_j = ivreg(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t| wave2 + mon + tues + wed + thurs + t, data = data)
summary(mod_IV_j)

##Question k##
mod_1st_step_k = lm(lavgprc ~ speed3 + mon + tues + wed + thurs + t, data = data)
summary(mod_1st_step_k) # t value = 2.565 < 3.16 -> weak instrument

mod_IV_k = ivreg(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t| speed3 + mon + tues + wed + thurs + t, data = data)
summary(mod_IV_k)

##Question l##
# We perform a F test on the 1 stage regression to decide whether wave +speed is a strong instrument #

mod_1st_step_l = lm(lavgprc ~ wave2 + speed3 + mon + tues + wed + thurs + t, data = data)
mod_aux = lm(lavgprc ~ mon + tues + wed + thurs + t, data = data)
anova(mod_aux, mod_1st_step_l) #F value = 12.469 > 10 -> strong instrument

mod_IV_l = ivreg(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t| wave2 + speed3 + mon + tues + wed + thurs + t, data = data)
summary(mod_IV_l)

##Question m##
#Sargan Test
mod_sargan = lm(residuals(mod_IV_l) ~ wave2 + speed3 + mon + tues + wed +thurs + t, data = data)
summary(mod_sargan) #R^2 = 0.02025
S = 97 * 0.02025
1-pchisq(S, 1) # = 0.16105 we do not reject the null hypothesis

##Question n##
# D-W-H test
Nu_hat_2 = residuals(mod_1st_step_l)
mod_DWH_2 = lm(ltotqty ~ lavgprc  + mon + tues + wed +thurs + t + Nu_hat_2, data = data)
summary(mod_DWH_2) # t value = 0.09653 we reject at 10% but not a 5%

##Bonus##
#modified Prais-winsten?
