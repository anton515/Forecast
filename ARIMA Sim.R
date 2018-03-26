# ARIMA state space models Simulation

# ARIMA(1,1,1) model

set.seed(1)
sim1<- arima.sim(n=110, model = list (order = c(1,1,1), ar = 0.5, ma = 2, sd = sqrt(1)))
data <-ts(sim1[1:100])

plot(data) #looks like there is a trend, so we do the 1st diff
diff1<-diff(data)
plot(diff1) #first diff took away effects of the trend

acf(diff1) #could be q = 1,2
pacf(diff1) #could be p = 1,2

mod1<-arima(data, order = c(1,1,0))
mod2<-arima(data, order = c(0,1,1))
mod3<-arima(data, order = c(1,1,1)) #2nd best aic
mod4<-arima(data, order = c(1,1,2))
mod5<-arima(data, order = c(2,1,1)) #best aic
mod6<-arima(data, order = c(2,1,2)) #2nd best aic
aic<-c(mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic, mod6$aic)
aic #models 3,5,6 have the best aics

pred3<-as.vector(predict(mod3, n.ahead = 10)$pred)
pred5<-as.vector(predict(mod5, n.ahead = 10)$pred)
pred6<-as.vector(predict(mod6, n.ahead = 10)$pred)
pred<-as.vector(ts(sim1[101:110]))
df<-data.frame(pred,pred3,pred5,pred6)
df

pred.se<-(pred-pred)^2
pred3.se<-(pred-pred3)^2
pred5.se<-(pred-pred5)^2
pred6.se<-(pred-pred6)^2
df2<-data.frame(pred.se,pred3.se,pred5.se,pred6.se)
df2

pred3.mse<-print(mean(df2[,2])) #2nd best aic but has the lowest MSE.
pred5.mse<-print(mean(df2[,3]))
pred6.mse<-print(mean(df2[,4]))

# now we are turning to the state space rep. of a ARIMA (1,1,1) model
mod3$coef[1]
ss<-makeARIMA(phi = mod3$coef[1], theta = mod3$coef[2], Delta = 1)
KalmanLike(data,ss) #not really sure what to do with this... will learn next lecture
