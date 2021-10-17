#library used in this script 

library(stargazer)
library(fRegression)
library(dplyr)
library(car)
library(quantreg)
library(fBasics)
library(lmtest)

pred_log = function(data, model)
{
  p = predict(model, data) 
  s2 = residuals(mod) %>% var()
  return(exp(p + 0.5*s2))
}

#our data set 
data=sevs

#point a
#observing the dataset 
View(data)

#there are NA value 
anyNA(data)
#number of NA value in wph
NA_number = is.na(data$wph) %>% sum()

table(data$sex[is.na(data$wph)])
#a lot of missing value in wph due to non worker people 
#install.packages("fBasics")


basicStats(data)
plot(data$jo, type="l")
table(data$jo)
plot(density(data$jo))
#high kurtorsis of jo 

#omitting rows with NA
data1 = na.omit(data)


#point b 
#boxplot of men and women 
men_women = ifelse(data1$sex == 0, "Men", "Women")
boxplot(data1$wph~men_women, ylab="wph", main="Boxplot wph of men vs women ")

men = data1$sex==0

tab = data.frame( "Mean"=c(
  mean(data1$wph[data1$sex==0]),
  mean(data1$wph[data1$sex==1])), 
  "Median"=c(
    median(data1$wph[data1$sex==0]),
    median(data1$wph[data1$sex==1])))

xtable::xtable(tab, digits=3)


hist(data1$wph[men])
hist(data1$wph[!men])
plot(density(data1$wph[men]), ylim=c(0,0.07), main="Estimated densities")
lines(density(data1$wph[!men]), col=2)
legend("topright", c("Men", "Women"), lty=1, col=1:2)

summary(data1$wph[men])
summary(data1$wph[!men])

q_men = quantile(data1$wph[men], probs = seq(0, 1, 0.05))[c(2,20)]
q_women = quantile(data1$wph[!men], probs = seq(0, 1, 0.05))[c(2,20)]




#point c 
exp2=data1$exp^2
mod = lm(log(wph) ~  edu + exp + exp2, data=data1)
summary(mod)
stargazer(mod)



summary(rq(log(wph) ~  edu + exp + exp2, data=data1))
#edu positive regardless the experience
#exp difficult to interpret, we have to watch the slide
mod$coef*100
plot(fitted(mod),residuals(mod))

#lines(p(-4:51), col=2, lwd=3)

#point d
#testing experience 
#test F for both exp and exp2
mod_reduced = mod_no = lm(log(wph) ~ edu, data=data1)
anova(mod_reduced, mod)
summary(mod)

#point e
plot(data1$exp, log(data1$wph), main="Title")
abline(v=25.22534,col=2)

mod_no = lm(log(wph) ~ exp + exp2, data=data1)
opt_point = -coef(mod_no)[2]/(2*coef(mod_no)[3])
pred_log(data.frame(exp=opt_point, exp2=opt_point^2), mod_no)


summary(mod_no)

p = function(x)
{
  return(mod_no$coefficients[1] + mod_no$coefficients[2]*x + mod_no$coefficients[3]*x^2)
}

hist(data1$exp)

car::avPlots(mod)
data[1318,]
?avPlots
#maxime the function used to predict the log(price)

#point f
pers = data.frame(edu = 17, exp=1, exp2=1)
pred_log(pers, mod)

#we assume that residuals are normally 
#approximation of the real value 

#point g
#check what this command does 

vif(mod)
#check if it works with quadratic relationship 

exp_nm = (data1$exp-mean(data1$exp))
exp2_nm = (data1$exp-mean(data1$exp))^2
mod_test = lm(log(wph) ~ edu + exp_nm +exp2_nm  , data=data1)
vif(mod_test) %>% xtable(.,3)
#######

mod_test
summary(mod_test)
mod %>% summary()

#why this happen ? 
basicStats(data1$exp)


m1 = lm(exp2 ~ exp + edu, data=data1)
m1 %>% summary()

m2 = lm(exp ~ exp2 + edu, data=data1)
m2 %>% summary()

#so the problem is the quadratic function used to estimate wage 
plot(data1$exp, exp2)
dfbeta(mod) %>% summary() %>% xtable
plot(dfbeta(mod)[,1],type="h")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,2],type="h")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,3],type="h")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,4],type="h")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)

which.max(dfbeta(mod)[,1])
which.max(dfbeta(mod)[,2])
which.max(dfbeta(mod)[,3])
which.max(dfbeta(mod)[,4])
plot(mod)
#add quantile 90 95 5 
#do histograms

avPlots(mod)

data2= na.omit(data[-1318,])
exp2 = data2$exp^2
mod_no1318 = lm(log(wph) ~  edu + exp + exp2, data=data2)
summary(mod_no1318) %>% coef()
summary(mod) %>% coef()
avPlots(mod_no1318)

#reset
reset = resettest(mod, power = 3, "regressor")
reset
xtable(reset)

?reset 

#add interaction 
plot(data1$edu, log(data1$wph))
edu2 =  data1$edu^2
exp3 = data1$exp^3
exp4 = data1$exp^4
mod_int = lm(log(wph) ~  edu+ exp + exp2 + exp3, data=data1)
mod_int %>% summary()
stargazer(mod_int)

resettest(mod_int, power = 3, "regressor")
AIC(mod)
AIC(mod_int)
BIC(mod)
BIC(mod_int)

#point i
###sex 
data1$sex = ifelse(data1$sex==0, "Male", "Female")
data1$sex = factor(data1$sex, levels = c("Male","Female"))
mod_sex = mod = lm(log(wph) ~  edu + exp + exp2+sex, data=data1)
mod_sex = mod = lm(log(wph) ~ sex, data=data1)
summary(mod_sex)
stargazer(mod_sex)

#point j
#### chow test 
mod_chow = lm(log(wph) ~  edu + exp + exp2 +sex*(edu + exp + exp2), data=data1)
summary(mod_chow)
anova( mod, mod_chow)

###### bonus
#add exp3
#demeaning experience 
#add hi 


add1(mod_chow, ~. + hi, test="F")
mod_chow_2 = lm(log(wph) ~  edu + exp + exp2 
                +sex*(edu + exp + exp2) + hi, data=data1)
summary(mod_chow)


mod_chow_2_mod = lm(log(wph) ~  edu + exp + exp2 
                    +sex*(edu + exp + exp2) + hi -edu:sex, data=data1)

anova(mod_chow_2, mod_chow_2_mod)

mod_chow_2_int = lm(log(wph) ~  edu + exp + exp2 
                    +sex*(edu + exp + exp2+hi) , data=data1)
anova(mod_chow_2, mod_chow_2_int)


#trying jo

mod_chow_3 = lm(log(wph) ~  edu + exp + exp2 
                +sex*(edu + exp + exp2) +hi +jo , data=data1)
anova(mod_chow_2,mod_chow_3)




# MECHANICAL APPLICATION NOT TO USE TOO EASILY

library(olsrr)
m=(lm(log(wph)~.,data=data1))
k = ols_step_forward_p(m)
plot(k)

b = ols_step_backward_p(m)
plot(b)

#g= ols_step_all_possible(m)
#plot(g)

library(olsrr)
p = ols_step_both_p(m)


ols_step_forward_aic(mod_chow)




ols_step_backward_aic(m)

stargazer(mod)

###
reset = resettest(mod, power = 4)
reset



###Bonus using regsubsets
library(leaps)
data_b = subset(data1, select = -c(lms, age)) #have to delete the variable that cause colinearities
selection_b = regsubsets(log(wph)~., data_b)
summary(selection_b)
# should have nwi, sex, hi, edu, exp , mar , kt, kt512?
model_b_1 = lm(log(wph) ~ nwi + sex + hi + edu + exp +mar +kt + k512, data = data_b)
summary(model_b_1)
BIC(model_b_1)
AIC(model_b_1)

#reset test on model b_1
resettest(model_b_1, power = 2:3, type = 'regressor')

#let just keep variables up to quadratic power
nwi2=data_b$nwi^2
hi2=data_b$hi^2
edu2=data_b$edu^2
kt2=data_b$kt^2
k5122=data_b$k512^2

model_b_2 = lm(log(wph) ~ nwi + sex + hi + edu + exp +mar +kt + k512 + nwi2 + hi2 + edu2 + exp2 +kt2 + k5122, data = data_b)
summary(model_b_2)
BIC(model_b_2)
AIC(model_b_2)

#removing some variables
model_b_3 = lm(log(wph) ~ nwi + sex + hi + edu + exp +mar +kt +  nwi2 + hi2 + exp2 +kt2, data = data_b)
summary(model_b_3)
BIC(model_b_3)
AIC(model_b_3)

#adding interaction with sex
model_b_4 = lm(log(wph) ~ nwi + sex + hi + edu + exp +mar +kt + sex*(nwi + hi + edu + exp +mar +kt) + nwi2 + hi2 + exp2 +kt2, data = data_b)
summary(model_b_4)
BIC(model_b_4)
AIC(model_b_4)

#removing variables again
model_b_5 = lm(log(wph) ~ nwi + sex + hi + edu + exp +mar +kt + sex*(exp +mar +kt) + nwi2 + exp2, data = data_b)
summary(model_b_4)
BIC(model_b_5)
AIC(model_b_5)
# stop because bored