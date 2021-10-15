#library used in this script 
install.packages("lmtest")
library(stargazer)
library(fRegression)
library(dplyr)
library(car)
library(quantreg)
library(fBasics)

#our data set 
data=sevs

#observing the dataset 
View(data)

#there are NA value 
anyNA(data)
#number of NA value in wph
NA_number = is.na(data$wph) %>% sum()
stargazer(NA_number)

#a lot of missing value in wph due to non worker people 
#install.packages("fBasics")


basicStats(data)
plot(data$jo, type="l")
table(data$jo)
plot(density(data$jo))
#high kurtorsis of jo 

data1 = na.omit(data)

boxplot(data$wph~data$sex)
boxplot(data1$wph)

mean(data1$wph[data1$sex==0])
mean(data1$wph[data1$sex==1])

median(data1$wph[data1$sex==0])
median(data1$wph[data1$sex==1])

plot(density(data1$wph[data1$sex==0]), ylim=c(0,0.07))
lines(density(data1$wph[data1$sex==1]), col=2)


exp2=data1$exp^2
mod = lm(log(wph) ~  edu + exp + exp2, data=data1)
summary(mod)




summary(rq(log(wph) ~  edu + exp + exp2, data=data1))
#edu positive regardless the experience
#exp difficult to interpret, we have to watch the slide
mod$coef*100

plot(fitted(mod),residuals(mod))

mod_no = lm(log(wph) ~ exp + exp2, data=data1)
mod_no

summary(mod_no)

p = function(x)
{
  return(mod_no$coefficients[1] + mod_no$coefficients[2]*x + mod_no$coefficients[3]*x^2)
}

plot(data1$exp, log(data1$wph))
#lines(p(-4:51), col=2, lwd=3)

summary(mod)
drop1(mod, test="F")

car::avPlots(mod)
data[1318,]
?avPlots
#maxime the function used to predict the log(price)

pers = data.frame(edu = 17, exp=1, exp2=1)
log_exp =predict(mod, pers) %>% as.numeric() 
res = residuals(mod)
s2 = var(res)
sum(res^2)/(dim(data1)[1]-1)

expect_wage = exp(log_exp + 0.5*s2);expect_wage
#we assume that residuals are normally 
#approximation of the real value 


#check what this command does 
?vif
vif(mod)
#check if it works with quadratic relationship 
mod_test = lm(log(wph) ~ exp +edu, data=data1)
vif(mod_test)


model.matrix(mod)
#why this happen ? 
basicStats(data1$exp)


m1 = lm(exp2 ~ exp + edu, data=data1)
m1 %>% summary()

m2 = lm(exp ~ exp2 + edu, data=data1)
m2 %>% summary()

#so the problem is the quadratic function used to estimate wage 
plot(data1$exp, exp2)
dfbeta(mod)
plot(dfbeta(mod)[,4],type="h")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)

which.max(dfbeta(mod)[,1])
which.max(dfbeta(mod)[,2])
which.max(dfbeta(mod)[,3])
which.max(dfbeta(mod)[,4])

#add quantile 90 95 5 
#do histograms


drop1(mod,scope=,  test="F")
add1()

#f statistic


# MECHANICAL APPLICATION NOT TO USE TOO EASILY

library(olsrr)
m=(lm(log(wph)~.,data=data1))
k = ols_step_forward_p(m)
plot(k)

b = ols_step_backward_p(m)
plot(b)

#g= ols_step_all_possible(m)
#plot(g)


p = ols_step_both_p(m)


ols_step_forward_aic(m)



plot(ols_step_backward_aic(m))



###
lmTest(mod, method="reset")







