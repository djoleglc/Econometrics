library(readr)
library(dplyr)
library(car)
library(caret)
library(lmtest)
library(sandwich)
library(prais)
library(plm)

head(fish)
 #ex1

#question a)
mod_a = lm(lavgprc ~ t + mon + tues + wed + thurs, data = fish)
mod_a%>% summary()

#in order to check that the day of the week is statistically 
#significant we have to do an F test

mod_a_HO = lm(lavgprc ~ t, data = fish)
#testing the day of the week 
anova(mod_a_HO, mod1, test="F")
#they seem to be useless in order to predict the log average price



#question b)
mod_b = lm(lavgprc ~ t + mon + tues + wed + thurs
          + wave2 + wave3, data = fish)
mod_b %>% summary()

#individually wave2 is statistically significant for every reasonable level 
#individually wave 3 is statistically significant at 0.05

#testing jointly significance

anova(mod_a, mod_b, test="F")

#the two variable are jointly significance
#we can see that the stormy see has effect on future 4 days.
#in fact we see that the situation of the see during last two days
#influence the price of the fish positively, if the average maximum height
#of the wave is bigger of 1m, the price is expected be bigger of 9% for 
#the next two days and also this will increment the 
#price of 3-4 days in the future by 4.7% 

#question d)
res = residuals(mod_b)
#we can visualize the residuals
plot(res)
#durbin watson test 
durbinWatsonTest(mod_b)
#we reject the hypothesis of rho equal to 0 
#we can also do the breusch godfrey test 

bgtest(mod_b)
#also this test give us the same result, the same conclusion 

#question e)
#calculating the newey west covariance and the t tests
cov = NeweyWest(mod_b, lag = 4, verbose=TRUE)
t_tests_newey_west = coeftest(mod_b, cov)


#question f)
#prais winsten estimates for the model 

mod_prais_winsten = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs
              + wave2 + wave3, data = fish,index=fish$t)
mod_prais_winsten_H0 = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs
                                   , data = fish,index=fish$t)

#we have to test joint significance 
res1 = mod_prais_winsten$residuals
res2 = mod_prais_winsten_H0$residuals
s0 = sum(res2^2)/96
s1 = sum(res1^2)/96
f_test = ((s0-s1)/s1) * (97-length(coef(mod_prais_winsten)))/2
pvalue = 1- pf(f_test, 2, 97-length(coef(mod_prais_winsten)))
#fom this solution I will reject the hypothesis that 
#the two lagged variable are not significant important 
#for explaining the average log price of fish 



#ex2 
#question a) 
head(fish)
mod_a_2 = lm(ltotqty ~ lavgprc +  mon +
               tues +wed + thurs + t, data = fish)
mod_a_2 %>% summary()
#interpret log(price)
coef(mod_a_2)
#the value is -0.548897
#what does it mean?
#this is the elasticity
#log on log 

#we couòd be concerned about the causal effect of price on quantity 
#considering the fact that the quantity sold depends on the price 
#of the price but maybe also the price of the fish depends
# on the quantity sold
#this could be a case of reverse causality in the case where 
#price affect quantity sold 
#but also quantity sold affect the price 
#(?)

#we could be in the case of omitted variable bias
#in fact the quantity of fish sold depend also on the quantity of fish 
#available

#the condition of the see inmpact the price and impact the quantity of 
#fish sold only through the price of fish 

# question b)
#in order to be a good instrument for the price of the fish 
# the past condition of the sea must be highly correlated with thr price 
#and impacts the quantity of the fish only through the effect of price
#and that should not be correlated with U, fish available ? 



#weak instrument or strong instrument?
#first stage regression 
first_stage = lm( lavgprc ~  wave2 +  mon +
                    tues + wed + thurs + t, data = fish)

first_stage %>% summary()
#we can check the t statistic and the f statistic
t_value = coeftest(first_stage)[2,3]
F_value = drop1(first_stage, test ="F")[2,c(1,5)] ; F_value

#the variable wave2 is a strong instrument variable 



#durbin wu hausman test 

r_first_stage = first_stage %>% residuals()

test_model = lm(ltotqty ~ lavgprc +  mon +
                            tues +wed + thurs + t + 
                             r_first_stage, data = fish)

coeftest(test_model)
#the parameter of the residuals is not significantly 
#distance from 0
#we do not reject the null hypothesis 
#we could proceed with the ols model 

#nstall.packages("AER")
library(AER)

iv_reg = ivreg(ltotqty ~ lavgprc +  mon +
               tues +wed + thurs + t | wave2 +  mon +
                 tues + wed + thurs + t , data = fish)

iv_reg %>% summary()
coeftest(iv_reg)[2,]
coeftest(mod_a_2)[2,]

#using speed3 instead of wave2
iv_reg_2 = ivreg(ltotqty ~ lavgprc +  mon +
                 tues +wed + thurs + t | speed3 +  mon +
                 tues + wed + thurs + t , data = fish)

iv_reg_2 %>% summary()
coeftest(iv_reg_2)[2,]
coeftest(mod_a_2)[2,]



#question k) 
first_stage_speed = lm( lavgprc ~  speed3 +  mon +
                          tues + wed + thurs + t, data = fish)
first_stage_speed %>% summary()
# speed is a weak instrument variable for t variable 
#let's see the F statistic 
drop1(first_stage_speed, test="F")[2,c(1,5)]
#also for the F value 


#question h) 
iv_reg_3 = ivreg(ltotqty ~ lavgprc +  mon +
                   tues +wed + thurs + t | wave2 + speed3 +  mon +
                   tues + wed + thurs + t , data = fish)

iv_reg_3 %>% summary()
coeftest(iv_reg_3)
coeftest(mod_a_2)

#let's try do the first stage regression with both 
first_stage_both = lm( lavgprc ~  wave2 + speed3 +  mon +
                          tues + wed + thurs + t, data = fish)
first_stage_both %>% summary()
#let's observe the f value for the joint significance 
first_stage_both_HO =  lm( lavgprc ~ mon + tues + wed + thurs + t, data = fish)
anova(first_stage_both_HO , first_stage_both, test = "F")
#the value of the F is equal to 12


library(modelsummary)
m = list(OLS= mod_a_2, IV = iv_reg_3)
modelplot(m)
#question m)
#testing overidentyfing restriction 
#and endogeneity 

summary(iv_reg_3, vcov = sandwich, diagnostics = TRUE)

#regress this residuals on the full set of exogenous variable


#how can we adjust the IV estimated for autocorrelation
summary(iv_reg_3, vcov = NeweyWest, diagnostics = TRUE)
#we can use the matrix of variance and covariance of Newey west 













