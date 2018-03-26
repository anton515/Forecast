#simulation of arima model with phi1=0.2,phi2=-0.6, theta=2, var=2, I=2
sim.arima<-arima.sim(n=2000,model=list(order=c(2,2,1),ar=c(0.2,-0.6),ma=
                                        1.4,sd=sqrt(4)))
plot(sim.arima)
acf(sim.arima)
pacf(sim.arima)

#take the first dif
d1<-diff(sim.arima)
plot(d1)
acf(d1)
pacf(d1)

#take the 2nd diff
d2<-diff(d1)
plot(d2)
acf(d2) #d could be order 1,2,3,4
pacf(d2) #p could be 1,2,3,4 order
#looks like its an ARIMA(p,2,q)  model

#let's try to get the best fit

fit1<-arima(sim.arima,order=c(1,2,1)) #AIC = 7870.82
fit1
fit2<-arima(sim.arima,order=c(2,2,1)) #AIC = 7056.48, appears to be best
fit2
fit3<-arima(sim.arima,order=c(3,2,1)) #AIC = 7058.48
fit3
fit4<-arima(sim.arima,order=c(4,2,1)) #AIC = 7060.48
fit4
#the order for AR seems to be 2
fit5<-arima(sim.arima,order=c(2,2,2)) #AIC = 7058.58
fit5
fit6<-arima(sim.arima,order=c(2,2,3)) #AIC = 7060.48
fit6
#we see that the parameter estimates already begin to shrink to zero

#we select that the best mode would be ARIMA(2,2,1), as it also has
#the lowest AIC vlaue
bestfit<-arima(sim.arima,order=c(2,2,1))
bestfit
res<-bestfit$residuals
qqnorm(res) #residuals looks normal, like iid N(mu,sigma^2)
acf(res) #the residuals acf looks like WN, which is stationary
#the residuals looks good as well, we assume this is best model
