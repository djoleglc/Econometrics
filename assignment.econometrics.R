#library used in this script 

library(stargazer)
library(fRegression)
library(dplyr)
library(car)
library(quantreg)
library(fBasics)
library(lmtest)

sevs=read.csv("sevs.csv")
data=sevs

## point a ##
#observing the dataset 
View(data)

#there are NA value 
anyNA(data)
#number of NA value in wph
NA_number = is.na(data$wph) %>% sum()

table(data$sex[is.na(data$wph)])
#a lot of missing value in wph due to non worker people 


basicStats(data)
plot(data$jo, type="l")
table(data$jo)
plot(density(data$jo))
#high kurtorsis of jo 

#omitting rows with NA
data1 = na.omit(data)


## point b ## 
#boxplot of men and women 
men = subset(data1, sex == 0)
women = subset(data1, sex == 1)

boxplot(data1$wph~Sex, ylab="WPH", main="Boxplot",col=c(4,2)) 


tab = data.frame( "Mean"=c(
  mean(data1$wph[data1$sex==0]),
  mean(data1$wph[data1$sex==1])), 
  "Median"=c(
    median(data1$wph[data1$sex==0]),
    median(data1$wph[data1$sex==1])))
tab

##histogram
hist(men$wph)
hist(women$wph)

##density plot
plot(density(men$wph), ylim=c(0,0.07), main="Estimated densities")
lines(density(women$wph), col=2)
legend("topright", c("Men", "Women"), lty=1, col=1:2)

##mean, median
summary(men$wph)
summary(women$wph)

q_men = quantile(men$wph, probs = seq(0, 1, 0.05))[c(2,20)]
q_women = quantile(women$wph, probs = seq(0, 1, 0.05))[c(2,20)]


## point c ## 
exp2=data1$exp^2
## Mincer equation
mod = lm(log(wph) ~  edu + exp + exp2, data=data1)
summary(mod)
#stargazer(mod)


#edu positive regardless the experience
#exp difficult to interpret, we have to watch the slide
mod$coef*100
plot(fitted(mod),residuals(mod), xlab="fitted", ylab="residuals")

## point d ##
#testing experience 
#test F for both exp and exp2
mod_reduced =  lm(log(wph) ~ edu, data=data1)
anova(mod_reduced, mod,test="F")

## point e ##
coef(mod)
opt = coef(mod)[3]/(-2*coef(mod)[4])

## point f ##
pers = data.frame(edu = 17, exp=1, exp2=1)
p = predict(mod, pers) 
s2 = sum(residuals(mod)^2) / (dim(data1)[1]-(length(coef(model))))
estimation=exp(p + 0.5*s2);estimation

#we assume that residuals are normally 
#approximation of the real value 

## point g ##
#colinearity
vif(mod)
#demeaning
exp_nm = (data1$exp-mean(data1$exp))
exp2_nm = (data1$exp-mean(data1$exp))^2
vif(mod_test)
#they are highly correlated 
cor(exp2, data1$exp)
#demeaning correlation
cor(exp_nm, exp2_nm)


#outliers
car::avPlots(mod)
#observation that seem most influence 
#dfbeta
par(mfrow=c(2,2))

dfbeta(mod) %>% summary() %>% xtable
plot(dfbeta(mod)[,1],type="h",ylab="intercept", xlab="observation", 
     main="dfbeta")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,2],type="h",ylab="edu" , xlab="observation", main="dfbeta")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,3],type="h", ylab="exp" , xlab="observation", main="dfbeta")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)
plot(dfbeta(mod)[,4],type="h", ylab="exp_squared",xlab="observation", main="dfbeta")
abline(h=1, lty=2, col=2)
abline(h=-1, lty=2, col=2)

which.max(dfbeta(mod)[,1])
which.max(dfbeta(mod)[,2])
which.max(dfbeta(mod)[,3])
which.max(dfbeta(mod)[,4])


data2= na.omit(data[-1318,])
exp2_no1318 = data2$exp^2
mod_no1318 = lm(log(wph) ~  edu + exp + exp2_no1318, data=data2)
summary(mod_no1318) %>% coef()
summary(mod) %>% coef()
avPlots(mod)
avPlots(mod_no1318)

#reset
reset = resettest(mod, power = c(2,3,4), "fitted")
reset



## point i ##
data1$sex = ifelse(data1$sex==0, "Male", "Female")
data1$sex = factor(data1$sex, levels = c("Male","Female"))
mod_sex = lm(log(wph) ~  edu + exp + exp2+sex, data=data1)
summary(mod_sex)

## point j ##
mod_2 = lm(log(wph) ~  edu + exp + exp2 +sex*(edu + exp + exp2), data=data1)
summary(mod_2)
anova(mod, mod_2)


### bonus point #### 

BIC(mod_2)
AIC(mod_2)

mod_2_1 = lm(log(wph) ~  edu + exp + exp2 +sex*(  exp + exp2), data=data1)
anova(mod_2_1,mod_2)

mod_2_2 = lm(log(wph) ~  edu + exp + exp2 +sex*(  exp + exp2) +jo, data=data1)
summary(mod_2_2)

#the variable jo seems to be not significant diverse from 0 
#let's see children and also the interaction between children and sex

mod_2_3 = lm(log(wph) ~  edu + exp + exp2 +sex*(  exp + exp2) +kt, data=data1)
summary(mod_2_3)

mod_2_4 = lm(log(wph) ~  edu + exp + exp2 +sex*(  exp + exp2+kt) , data=data1)
summary(mod_2_4)


#effect of children on women 
coef(mod_2_4)[6] + coef(mod_2_4)[9] 


#### add hi 
mod_2_5 =  lm(log(wph) ~  edu + exp + exp2 +sex*(  exp + exp2+kt) + hi, data=data1)
summary(mod_2_5)
coef(mod_2_5)

anova(mod_2, mod_2_5)

BIC(mod_2_5)
AIC(mod_2_5)
