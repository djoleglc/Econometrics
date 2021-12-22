library(astsa)
library(tinytex)
library(forecast)
library(aTSA)
library(lmtest)
library(urca)
library(aTSA)
library(xtable)

#data 
data = vehiclesales
data = na.omit(data)

#point a
#we can plot the log of vehicle sales
time1 = seq(from=1976,to= 2018.75, by=0.25)
plot(time1, log(data$vehiclesales),type="l", ylab="log(vehicle sales)")
abline(lm(log(data$vehiclesales)~time1), col=2)

#does it seem stationarry ?
#It does not seem stationary, even if we consider a linear trends
#test with lag 1,2,3..., 12
#we are interested in test of type3, including both drift and linear trend





#point b
#augmented dickey fuller test with lag from 0, 2, 4..
lag = seq(0,12,by=2) + 1
adf = adf.test(log(data$vehiclesales), nlag=13)
adf$type3[lag,]
#we see consistency till the lag equal to 4
#this is due to the seasonality of the data 
#given the frequency of our observations 



#point c
#acf and pacf

par(mfrow=c(2,1))
acf(log(data$vehiclesales), main="ACF of log(sales)")
pacf(log(data$vehiclesales), main="PACF of log(sales)")
#we can think about a model ARMA(1,0)



#point d

#first model 
ar1 = arima(log(data$vehiclesales), order=c(1,0,0));ar1

#acf and pacf of residuals
acf(ar1$residuals, main="ACF of residuals")
pacf(ar1$residuals, main="PACF of residuals")
#coefficient of the model 
coeftest(ar1)

#ljung box test 
Box.test(ar1$residuals, lag=6, type="Ljung-Box", fitdf=1)
Box.test(ar1$residuals, lag=12, type="Ljung-Box", fitdf=1)


#we can check, and try to add an moving average component
arma1= arima(log(data$vehiclesales), order=c(1,0,1))
#coefficient and acf and pacf of the model 
coeftest(arma1)
acf(arma1$residuals)
pacf(arma1$residuals)



#while we can also try to estimate the model ar2 
#new model 
ar2 = arima(log(data$vehiclesales), order=c(2,0,0), 
            include.mean = TRUE)
#acf and pacf
acf(ar2$residuals, main="ACF of residuals")
pacf(ar2$residuals, main="PACF of residuals")

#ljung box test 
Box.test(ar2$residuals, lag=6, type="Ljung-Box", fitdf=2)
Box.test(ar2$residuals, lag=12, type="Ljung-Box", fitdf=2)
coeftest(ar2)




#we can check for unit root using a plot
#inverse AR root 
par(mfrow=c(1,1))
plot(ar2)
#check if the AR roots are outside the unit circle
#in this case we are observing the inverse of the AR root

#sum of autoregressive coefficient for stationarity
coeftest(ar2)
sarc = sum(coef(ar2)[c(1,2)])
sarc


#with time trend forecast 
ar2_t = Arima(log(data$vehiclesales), order=c(2,0,0), 
              include.mean = TRUE, xreg = data$year)

#checking the significance of the model 
coeftest(ar2_t)




#then we can test for overfitting 
#we do it by doing two models, add a order of autoregressive
#and for the second model we add an order for the ma part
#testing over fitting

ar3 =  arima(log(data$vehiclesales), order=c(3,0,0), 
             include.mean = TRUE)
#test the coefficient 
coeftest(ar3)


# we increase the order of the MA part 
arma2 =  arima(log(data$vehiclesales), order=c(2,0,1), 
               include.mean = TRUE)
acf(arma2$residuals, main="ACF of residuals")
pacf(arma2$residuals, main="PACF of residuals")
#test the coefficient
coeftest(arma2)




#aic and bic for all model we have found 
AIC(ar1,ar2,arma1,ar3,arma2, ar2_t)
BIC(ar1,ar2, arma1,ar3,arma2, ar2_t)
#we can see for al these steps that the best model is the ar2





#we have the realization of the real process in log 

real = c(3.95328, 3.964027, 3.969461, 3.953664,
         3.834667, 3.550938, 3.860077, 3.911062)
par(mfrow=c(1,1))
plot(seq(2019, 2020.75,by=0.25),real, type="l",
     ylab="log(vehicle sales)", xlab="Time", main="Realized sales")



      



#### BONUS 


delta = diff(log(data$vehiclesales))
par(mfrow=c(2,1))

#acf and pacf of the diff series 
acf(delta, main="ACF of delta_log(sales)")
pacf(delta, main="PACF of delta_log(sales)")



#first model that we have used 
arima1 = arima(log(data$vehiclesales), order=c(1,1,0))

acf(residuals(arima1), main="ACF of ARIMA(1,1,0)")
pacf(residuals(arima1), main="PACF of ARIMA(1,1,0)")

#test coefficients
coeftest(arima1)




#second model we have used
arima2=Arima(log(data$vehiclesales), order=c(0,1,1))

acf(residuals(arima2), main="ACF of ARIMA(0,1,1)")
pacf(residuals(arima2), main="PACF of ARIMA(0,1,1)")

coeftest(arima2)



#third model 
arima3 = Arima(log(data$vehiclesales), order=c(1,1,1))

acf(residuals(arima3))
pacf(residuals(arima3))
 
coeftest(arima3)



#fourth model 
arima4 = arima(log(data$vehiclesales), order=c(0,1,2))
acf(residuals(arima3)))
acf(residuals(arima4))
pacf(residuals(arima4))
coeftest(arima4)





#fifth model 
arima5 = arima(log(data$vehiclesales), order=c(2,1,0))
acf(residuals(arima3)))
acf(residuals(arima5))
pacf(residuals(arima5))
coeftest(arima5)




#aic of the various model that we have applied 
AIC(arima1,arima2,arima3,arima4,arima5)
BIC(arima1,arima2,arima3,arima4,arima5)
BIC(arima3,arima4,arima5)







####### FORECAST FOR ALL MODEL WE HAVE DONE 

######### forecast in log and then in levels
#we use the model ar2

f = forecast::forecast(ar2, h=8, level=95)

#visualize the forecast and the prediction interval 
plot(f)

#put eveything in a dataframe
#we can also obtain the prediction for the levels
#by taking the exponential transformation
ar2_forecast = f %>% as.data.frame()
ar2_forecast_level = exp(ar2_forecast[,1] + ar2$sigma2/2)

#intervals 
ar2_forecast




#######forecast time trend

f_t = forecast::forecast(ar2_t, h=8, level=95, 
                         xreg=(c(rep(2019,4), rep(2020,4))))

#visualize the forecast and the prediction interval 
plot(f_t)

#put eveything in a dataframe

#we can also obtain the prediction for the levels
#by taking the exponential transformation
ar2_forecast_t = f_t %>% as.data.frame()
ar2_forecast_level_t = exp(ar2_forecast_t)[,1] * exp(0.5*ar2_t$sigma2)

#intervals
ar2_forecast_t




# forecast for arima model 


f = forecast::forecast(arima1,h = 8, level = 95)
plot(f)
f = as.data.frame(f)
forecast_arima_exp = exp(f[,1])*exp(0.5*arima1$sigma2)

  

##### forecast in level of vehicle sales
f_vehicle_sales = data.frame(ar2_forecast_level, ar2_forecast_level_t, forecast_arima_exp)
xtable(f_vehicle_sales,digits=3)



