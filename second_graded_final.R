library(readr)
library(dplyr)
library(car)
library(caret)
library(lmtest)
library(sandwich)
library(prais)
library(AER)
library(plm)
library(gmm)



#we load the dataset 
# in order to do that we have to read an csv file 
#fish <- read_csv("../fish.csv")


#firstly we visualize our dataset to check that everything is ok
head(fish)
View()

# summary statistics about the dataset 
# we can see the quantile, the median 

summary(fish)


#question a)

#we calculate the model asked in point a) using an OLS regression 
# ( in R this is done with the function lm() ) 
mod_a = lm(lavgprc ~ t + mon + tues + wed + thurs, data = fish)
# we can visualize the results of the model, the estimation of coefficients
# the standard errors, the t statsitics, and the p-value
mod_a %>% summary()



#in order to check that the day of the week is statistically 
#significant we have to do an F test

# we estimate an OLS model without the dummies variable related to the days of the week
mod_a_HO = lm(lavgprc ~ t, data = fish)
#we can visualize the new estimations value of the models 
summary(mod_a_HO)

# we can do an F test using the function anova(), specifying test = "F"
#we put before the restricted model and then the unrestricted model 
f_test_days = anova(mod_a_HO, mod_a, test="F")
#we can visualize the results given by this test 
f_test_days


#question b)
# we calculate the OLS regression using also the variables wave2 and wave3
mod_b = lm(lavgprc ~ t + mon + tues + wed + thurs
           + wave2 + wave3, data = fish)

#we visualize the results of the estimation of the model
#coefficient, standard errors, t stat, pvalue
mod_b %>% summary()

#individually wave2 is statistically significant for every reasonable level 
#individually wave 3 is statistically significant at 0.05
#we can also calculate the covariance between t and wave2 and t and wave3

cor(fish$wave2, fish$t)
cor(fish$wave3, fish$t)

#testing jointly significance
#we can test the jointly significance of the variable wave2 and wave3
#by doing an F test, we do it with the command anova
#in this case the restricted model is just the model defined at point a 
f_test_waves = anova(mod_a, mod_b, test="F")

#we can visualize the results of this test 
f_test_waves 
#the two variable are jointly significance

#we can see that the stormy see has effect on future 4 days.
#in fact we see that the situation of the see during last two days
#influence the price of the fish positively, if the average maximum height
#of the wave is bigger of 1m, the price is expected be bigger of 9% for 
#the next two days and also this will increment the 
#price of 3-4 days in the future by 4.7% 


#question d)
#we calculate the residuals of the model defined at point b
#we can do it by using residuals()
res = residuals(mod_b)

#we can visualize the residuals
plot(res, type="l")

#durbin watson test 
#we do a durbin watson test for the model defined at point b
durbinWatsonTest(mod_b)
#we reject the hypothesis of rho equal to 0 

#we can also do the breusch godfrey test 
bgtest(mod_b)
#also this test give us the same result, the same conclusion 



#question e)
#we calculate the variance-covariance matrix of newey west for the model defined
#at point b
#the have to give a lag value, we have choosen 4
nw = NeweyWest(mod_b, lag = 4, verbose=FALSE, prewhite=FALSE, adjust=TRUE)

#using the newey west matrix we can calculate the t-statistics for the model b)
#we use the function coeftest defining vcov = nw, that specify the variance-covariance matrix
t_tests_newey_west = coeftest(mod_b, vcov = nw)

#we can see the result 
t_tests_newey_west


#question f)
#we estimate the model using prais winsten transformation 
#we can do it using the function prais winsten, and specifying the formula of the model and the time variable.
#This also gives a estimated rho.
mod_pw = prais_winsten(lavgprc ~ t + mon + tues + wed + thurs + wave2 + 
                         wave3, data = fish, fish$t) 
#we can visualize the estimations of the coefficients using prais winsten transformation
summary(mod_pw)


#now we want to test the jointly hypothesis of wave2 = wave3 = 0
#we do a Wald test 
#we can do the Wald test using the function linearHypothesis 
#specifying const we are assuming constant variances 
linearHypothesis(mod_pw, vcov = vcovHC(mod_pw, "const"),
                 c("wave2 = 0", "wave3 = 0"), 
                 test = "Chisq") 

#we can also do this test using a robust estimation of the covariance-variance matrix HC0
linearHypothesis(mod_pw, vcov = vcovHC(mod_pw, "HC0"),
                 c("wave2 = 0", "wave3 = 0"), 
                 test = "Chisq") 




#ex2 

#question g) 

#we estimate the model specified in point g) using an OLS regression
mod_g = lm(ltotqty ~ lavgprc +  mon + tues +wed + thurs + t, data = fish)

#we visualize the results of the estimation of the model 
mod_g %>% summary()

# in order to interpret the coefficient for log(price)
# we print the coefficient estimated by the model 
coef(mod_g)


# question i)

#weak instrument or strong instrument?
# we estimate the first stage regression using an OLS regression
first_stage = lm( lavgprc ~  wave2 +  mon + tues + wed + thurs + t, data = fish)

#results of the first stage regression, coefficient estimate, standard errors, tvalue, pvalue
first_stage %>% summary()

#we can check the t statistic and the f statistic
# we check the t-value relative to the variable wave2
#we take the tvalue given by the regression using the coeftest function
coeftest(first_stage)

#we calculate also the F-statistic relative to the hypothesis wave2 = 0
#we did it using the function drop1
# this function give us the F test relative to exclude from the model each variable singularly
drop1(first_stage, test ="F")[2,]

#the variable wave2 is a strong instrument variable 



#question j) 
#to estimate the model using instrumental variable we used the function ivreg
#in this function after the | we need to put the exogenous variables and the instrumental variables
#to use in the model
#we estimate the instrumental variable model using 2SLS
iv_reg = ivreg(ltotqty ~ lavgprc +  mon +
                 tues +wed + thurs + t | wave2 +  mon +
                 tues + wed + thurs + t , data = fish)

#results of the model
iv_reg %>% summary()

#we obtained the estimation of the standard errors using robust standard errors
#we can obtain the covariance-variance matrix using the function vcovHC
iv_robust = coeftest(iv_reg, vcov = vcovHC(iv_reg, "HC3"))
iv_robust


#we estimate the model also using iterative GMM
#we can do it using the function gmm
gmm_reg_iterative = gmm(ltotqty ~ lavgprc +  mon +
                          tues +wed + thurs + t  , ~ wave2 +  mon +
                          tues + wed + thurs + t , data = fish, type="iterative")
#visualize the results of the estimation of the model using gmm 
summary(gmm_reg_iterative)


#using speed3 as instrumental variable instead of wave 2
#we estimate the model as the previous point 
iv_reg_2 = ivreg(ltotqty ~ lavgprc +  mon +
                   tues +wed + thurs + t | speed3 +  mon +
                   tues + wed + thurs + t , data = fish)
#results of the estimation of the model 
iv_reg_2 %>% summary()

#estimate the standard errors using robust estimation
iv_robust_2 = coeftest(iv_reg_2, vcov = vcovHC(iv_reg_2, "HC3"))

#estimate the model using iterative GMM 
gmm_reg_iterative_2 = gmm(ltotqty ~ lavgprc +  mon +
                            tues +wed + thurs + t ,~speed3 +  mon +
                            tues + wed + thurs + t , data = fish,type="iterative")

#results of the estimation of the models
gmm_reg_iterative_2 %>% summary()




#question k) 
#we estimate the first stage regression using OLS regression including the variable speed3 instead of wave2
first_stage_speed = lm( lavgprc ~  speed3 +  mon +
                          tues + wed + thurs + t, data = fish)
#we can see the results using the summary function
first_stage_speed %>% summary()
#in the above written summary we can also check the t-stat for the variable speed3

# speed is a weak instrument variable for t variable 
#we can also see it using the F statistic 
#as before we use the function drop1()
drop1(first_stage_speed, test="F")[2,]

#we arrive at the same conclusion


#question h) 
# we can calculate as before the models using two instrumental variable 
#instrumental variable regression using wave2 and speed3 
# we estimate the model using 2SLS regression
iv_reg_3 = ivreg(ltotqty ~ lavgprc +  mon +
                   tues +wed + thurs + t | wave2 + speed3 +  mon +
                   tues + wed + thurs + t , data = fish)

#visualize the results of the estimation of the model 
iv_reg_3 %>% summary()

#as before we calculate the model using robust estimation of the standard errors
#we calculate the robust variance covariance matrix using the command HC3
coeftest(iv_reg_3, vcov = vcovHC(iv_reg_3, "HC3"))


#lastly we estimate the model using iteriative GMM
#we do it using gmm()
gmm_reg_iterative_3 = gmm(ltotqty ~ lavgprc +  mon +
                            tues +wed + thurs + t ,~ wave2 + speed3 +  mon +
                            tues + wed + thurs + t , data = fish, type="iterative")

#we can see the results of the GMM regression
summary(gmm_reg_iterative_3)



#first stage regression with both wave2 and speed3
#we estimate the model relative to the first stage regression using OLS regression

first_stage_both = lm( lavgprc ~  wave2 + speed3 +  mon +
                         tues + wed + thurs + t, data = fish)

#visualize the results of the estimation of the model 
first_stage_both %>% summary()


#let's observe the f value for the joint significance 
#we can see the F statistic relative to the H0 :  wave2 = speed3 = 0 using the function anova()
#we specify the restricted model and we estimate this model using OLS regression
first_stage_both_HO =  lm( lavgprc ~ mon + tues + wed + thurs + t, data = fish)

#use anova function to obtain the F statistics 
anova(first_stage_both_HO , first_stage_both, test = "F")
#the value of the F is equal to 12


#question m)
#testing overidentyfing restriction and endogeneity 

#sargan test 
#we can do the sargan test for testing overidentyfing restriction
#we can see the sargan test results in the summary of an iv reg object using diagnostics = TRUE
summary(iv_reg_3, diagnostic = TRUE)
#We can get the results of the diagnostic
summary(iv_reg_3, diagnostic = TRUE)$diagnostic

#correlation between instrument
cor(fish$wave2, fish$speed3)

#we can see the sargan test in summary of the gmm object 
#it is called J test 
summary(gmm_reg_iterative_3, diagnostic = TRUE)

#question n)
#Durbin-Wu-Haussman test
#First we need to extract the residuals of the 1 stage regression
Nu_hat = residuals(first_stage_both)
#Then we add this residual to the model and check its relevance using a t test
DWHtest = lm(ltotqty ~ lavgprc  + mon + tues + wed + thurs + t + Nu_hat, data = fish)
#t test
coeftest(DWHtest)
# reject the null hypothesis at 10% but not at 5% confidence



#bonus question 


#how can we adjust the IV standard errors estimated for autocorrelation
#we can do it using the Neweywest variance-covariance matrix
#we choose, to use lag = 4 using the rule of thumb 
nw = NeweyWest(iv_reg_3,lag=4, prewhite=FALSE, adjust =TRUE)

#we can now see the new estimated t statistic of the model 2SLS
coeftest(iv_reg_3, vcov =nw)















