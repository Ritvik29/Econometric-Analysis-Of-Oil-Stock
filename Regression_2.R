library(astsa)
ExxonMobil1=ExxonMobil[-284,]
ExxonMobil1=ExxonMobil1[-283,]
ts.plot(ExxonMobil1$Close)
acf2(diff(log(ExxonMobil1$Close)),max.lag=48,main='ACF & PACF Plots of  first difference of log of 
     XOM Stock')
x=log(ExxonMobil1$Close)
difx=diff(x);lx = log(x); dlx = diff(lx); ddlx = diff(dlx, 14)
plot.ts(cbind(difx,x,lx,dlx,ddlx), yax.flip=TRUE, main="")
library(tseries)
adf.test(lx)


kk1=arima(lx,order=c(1,2,2))
acf2(kk1$residuals)

kk=arima(lx,order=c(1,1,0))
acf2(arima(lx,order=c(0,1,3))$residuals)

kk2=arima(lx,order=c(0,1,0))
acf2(arima(lx,order=c(3,1,14))$residuals,50)

sarima(lx,0,1,3,0,0,1,14)

adf.test(kk$residuals)


acf2(ddlx)


sarima(ExxonMobil1$Close,0,1,3,1,0,0,14)

sarima(ExxonMobil1$Close,2,1,1,0,1,0,12)
auto.arima(log(x),ic=c('aic'),trace=TRUE,d=1)
#----------------------------------------------------------------------------------------------

arima(lx,)

library(astsa)
tsplot(ExxonMobil$Close)
tsplot(diff(ExxonMobil$Close))
xxz=ts(XOI$Close, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(xxz,main='Plot of NYSE ACRA Oil and Gas Index')
tsplot(diff(log(xxz)),main='Plot of  first Difference of log of NYSE ACRA Oil and Gas Index')
#Plotting regression of closing price of Exxon vs market index

plot(ExxonMobil$Close~XOI$Close)
lines(lowess(XOI$Close,ExxonMobil$Close), col=4, lwd=2)
lag1.plot(ExxonMobil$Close, 36)
lag2.plot(XOI$Close,ExxonMobil$Close,12)

lm.fit1=lm(ExxonMobil$Close~XOI$Close)
plot(lm.fit1)
abline(lm.fit1)
res1=lm.fit1$residuals

residual1 = res1
residual1=ts(res1, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(residual1)

difx=diff(residual1); ddx_12 = diff(residual1, 12)
plot.ts(difx, yax.flip=TRUE, main=" Plot of first difference of Residuals of Regression Model 1")
plot.ts(residual1, yax.flip=TRUE, main=" Plot of Residuals of Regression Model 1")
#ts.intersect(res1)
lag1.plot(res1,12)

acf2(res1,main='ACF plot of residuals of model 1')

library(forecast)
auto.arima(res1)
tsplot(res1)
tsplot(diff(res1))
sarima(res1,0,1,0,0,0,0)
acf2(arima(lx,c(0,1,0))$residuals)


#Performing Regression Analysis---------------------------------------------------------------------------

auto.arima(ExxonMobil1$Close)
xom.fit1=lm(ExxonMobil1$Close~lag(ExxonMobil1$Close,-1))
summary(xom.fit1)



#computing Vars model for stock. This section ins wrong as the series is not stationary----------------------------------------------------------------
#wti_data$DCOILWTICO=ksmooth(time(wti_data$DCOILWTICO),wti_data$DCOILWTICO,'normal',bandwidth=1)
tsplot(wti_data$DCOILWTICO,main='WTI')

lag2.plot(wti_data$DCOILWTICO,ExxonMobil$Close,24)


sarima(ExxonMobil$Close,1,1,1,0,1,1,4)

acf2(ExxonMobil$Close,max.lag=36)
acf2(diff(log(wti_data$DCOILWTICO)),max.lag=48,main='ACF & PACF Plots of  first difference of log of 
     WTI')





comp1=data.frame(ExxonMobil1$Close,wti_data$DCOILWTICO)

library(vars)
VP<-VARselect(comp1, lag.max=24, type="both")

var_3=VAR(comp1, p =VP$selection[3], type = "both")

summary(var_3)
plot(var_3)

prd <- predict(var_3, n.ahead = 10, ci = 0.95, dumvar = NULL)
plot(prd)
plot(irf(var_3,impulse="wti_data.DCOILWTICO",response=c('ExxonMobil1.Close'),nstep=20))

#---------------------------------------------------------------------------------------------------
# Performing VAR analysis on difference data. 

x=wti_data$DCOILWTICO

x = ts(x, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(x, ylab = 'price', main = 'WTI Index')
difx=diff(x);lx = log(x); dlx = diff(lx); ddlx = diff(dlx, 12)
plot.ts(cbind(difx,x,lx,dlx,ddlx), yax.flip=TRUE, main="Transformed plots of WTI ")

#plot(irf(var_3,impulse="'wti_data.DCOILWTICO",response=c('ExxonMobil1.Close'),nstep=20))
ts.plot(diff(wti_data$DCOILWTICO))
acf(dlx,main='diff(log(WTI)')

y=ts(ExxonMobil1$Close, start=c(1995, 1), end=c(2018, 6), freq=12)
dly=diff(log(y))
ts.plot(dly,main='First Difference of Log Exxon Mobil Stock')
ts.plot(dlx,main='First Difference of Log WTI')

lag2.plot(dlx,dly,12)

d1=data.frame(dlx,dly)
VP_d<-VARselect(d1, lag.max=12, type="both")
var_d=VAR(d1,p=VP_d$selection[3],type='both')# select according to schwartz criteria or BIC

prd <- predict(var_d, n.ahead = 5, ci = 0.95, dumvar = NULL)
plot(prd)
plot(irf(var_d,impulse="dlx",response='dly',nstep=20))
plot(irf(var_d,impulse="dly",response='dlx',nstep=20))


#---------------------------------------------------------------------------------------------------
# ddly= diff(dly,12)
# d2=data.frame(ddlx,ddly)
# VP_d2<-VARselect(d2, lag.max=12, type="none")
# var_d12=VAR(d2,p=2,type='none') # select according to schwartz criteria or BIC
# summaryprd2 <- predict(var_d12, n.ahead = 5, ci = 0.95, dumvar = NULL)




# Try and plot the impulse response function. This is a major aspect of VAR modelling 
#plot(irf(var_d,impulse="'diff.wti_data.DCOILWTICO",response=c('diff.ExxonMobil1.Close'),nstep=20))
#------------------------------------------------------------------------------------------------------------------
panel_model=lm(ExxonMobil1$Close~wti_data$DCOILWTICO)
summary(panel_model)
plot(panel_model)
ACF=ARMAacf(ar=1,ma=1,50)

