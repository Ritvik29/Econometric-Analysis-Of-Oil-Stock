# Load data
setwd('D:/USA/Isen MS/Stat 626/project/data')
xom = data.frame(read.csv('XOM_new.csv'))
wti = data.frame(read.csv('wti_data.csv'))
xoi = data.frame(read.csv('XOI.csv'))

#check data types and update it, create date objects and other stuff
str(xom)
xom$Date = as.Date(xom$Date, "%Y-%m-%d")
xom$Close = as.numeric(as.character(xom$Close))
xoi$Date = as.Date(xoi$Date, "%Y-%m-%d")
wti$Date = as.Date(wti$Date, "%Y-%m-%d")

# Sanity checking
head(xom)
# 1st value is missing, ==>count missing values
sum(is.na(xom$Close))
sum(is.na(xoi$Close))
sum(is.na(wti$DCOILWTICO))
# only 1, so only the 1st row has null in XOM, drop it!
xom = xom[-c(1,283,284),]
xoi = xoi[-c(1,283,284),]
wti = wti[-1,]

# Sanity checking after cleaning
nrow(xom) # 281
nrow(xoi)
nrow(wti)

head(xom)
tail(xom)

head(xoi)
tail(xoi)

head(wti)
tail(wti)
###############################################
#############################################
#      PART - I : Time Series Analysis
#############################################
###############################################

library(astsa)
head(xom)
# Create TS Object & Plot
x = ts(xom$Close, start=c(1995, 2), end=c(2017, 12), freq=12)
tsplot(x, ylab = 'Closing price', main = 'ExxonMobile Stock price')

x1 = ts(wti$DCOILWTICO, start=c(1995, 2), end=c(2017, 12), freq=12)
tsplot(x1, ylab = 'Price', main = 'Barrel Price')

x2 = ts(xoi$Close, start=c(1995, 2), end=c(2017, 12), freq=12)
tsplot(x2, ylab = 'Closing price', main = 'NYSE Oil Index')
####################
summary(fit <- lm(x~time(x)))
abline(fit, col = 'blue')

par(mfrow=c(2,2))
plot(fit)
lines(lowess(x, f=.05), lwd=2, col=4)

lag1.plot(x, 12)

lx = log(x)
dx=diff(x)
dlx = diff(lx)

par(mfrow=c(2,2))
tsplot(x, main = 'Original data', ylab='')
tsplot(lx, main = 'Log transformed', ylab='')
tsplot(dx, main = 'First difference of original', ylab='')
tsplot(dlx, main = 'First diff of log transformed', ylab='')

acf2(dlx)
# Looks like a seasonality of 1.5 years or 18 months

#lag1.plot(x, 18)

par(mfrow=c(2,1))
tsplot(dlx, main = 'KERNEL SMOOTHING of First diff of log transformed', ylab='')
lines(ksmooth(time(dlx), dlx, "normal", bandwidth=3), lwd=2, col=4)
# Long term trend - 7.5 year cycle
tsplot(dlx, main = 'LOWESS of First diff of log transformed', ylab='')
lines(lowess(dlx, f=.05), lwd=2, col=4)
# Short term trend - 1 year cycle

ddlx1 = diff(dlx, 12)
ddlx2 = diff(dlx, 90)
tsplot(ddlx2, main = 'Long - term periodicity of 7.5 years')
tsplot(ddlx1, main = 'Short - term periodicity of 1 years')

# Decomposing
decompose(x)
plot(decompose(x))

# monthplot
par(mfrow=c(3,2))
monthplot(x, main = 'original')
monthplot(lx, main = 'log_trans')
monthplot(dx, main = '1st diff')
monthplot(dlx, main = '1st diff_log')
monthplot(ddlx1, main = 'short period removed')
monthplot(ddlx2, main = 'long period removed')

# ACF & PACF
acf2(ddlx1)

# SARIMA
sarima(lx, 2,1,1,0,1,0,12)
sarima.for(x,12,2,1,0,1,0,0,12)


#####################################
####################################
#    Part II - Multivariate Analysis
####################################
#####################################
#install.packages('vars')

#Scatter plots
cor(xoi$Close, xom$Close)  #0.9713638
cor(wti$DCOILWTICO, xom$Close)  #0.8264822

# BOX plot
lag2.plot(wti$DCOILWTICO,xom$Close,24)
lag2.plot(xoi$Close,xom$Close, 24)


plot(xoi$Close, xom$Close, ylab = 'ExxonMobil Stock prices',
     xlab = 'NYSE Market Index', main = 'Scatter Plot')
lines(lowess(xoi$Close,xom$Close), col=4, lwd=2)
plot(wti$DCOILWTICO, xom$Close, ylab = 'ExxonMobil Stock prices',
     xlab = 'Barrel Price', , main = 'Scatter Plot')
lines(lowess(wti$DCOILWTICO,xom$Close), col=4, lwd=2)

## Fitting Linear Regression to Time Series - WRONG! Assumptions not met

# XOM ~ t + XOI
lm_model = lm(xom$Close ~ time(xom$Close)+xoi$Close)
summary(lm_model)
par(mfrow=c(2,2))
plot(lm_model)
par(mfrow=c(2,1))
residuals = ts(lm_model$residuals, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(residuals, ylab = 'Residuals', 
       main = 'Residuals of Model 1')
acf(residuals)
acf2(residuals)
model_5 = sarima(xom$Close, 1,0,0, xreg=cbind(time(xom$Close), xoi$Close))
resd = resid(model_5$fit)
acf(resd)

fit = arima(lx, c(1,0,0))
forecast


# XOM ~ t + sqrt(WTI)
lm_model2 = lm(xom$Close ~ time(xom$Close)+sqrt(wti$DCOILWTICO))
summary(lm_model2)
par(mfrow=c(2,2))
plot(lm_model2)
par(mfrow=c(2,1))
residuals2 = ts(lm_model2$residuals, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(residuals2, ylab = 'Residuals', 
       main = 'Residuals of Model 2')
acf(residuals2)
acf2(residuals2)

sarima(xom$Close, 1,0,0, xreg=cbind(time(xom$Close), wti$DCOILWTICO))
# XOM ~ t + XOI + sqrt(WTI)
lm_model3 = lm(xom$Close ~ time(xom$Close)+xoi$Close+sqrt(wti$DCOILWTICO))
summary(lm_model3)
par(mfrow=c(2,2))
plot(lm_model3)
par(mfrow=c(2,1))
residuals3 = ts(lm_model3$residuals, start=c(1995, 1), end=c(2018, 6), freq=12)
tsplot(residuals3, ylab = 'Residuals', 
       main = 'Residuals of Model 3')
acf(residuals3)
acf2(residuals3)



# Vector auto regressive models
library(vars)
ExxonMobil_Stocks = x
Barrel_price = x1
NYSE_index = x2

X = cbind(ExxonMobil_Stocks, Barrel_price)
VARselect(X, lag.max = 10, type='both')
# z = as.data.frame(VARselect(X, lag.max = 10, type='both')$criteria)
# library(ggplot2)
# ggplot(z, aes(x=c(1:10), y = AIC(n), fill=C)) +
#   geom_tile() +
#   scale_fill_gradient(low="white", high="darkgreen", name="Your Legend"))
# lag 2 selected based on BIC/SC
fit_VAR = VAR(X, p=2, type='both')
summary(fit_VAR)
pred = predict(fit_VAR, n.ahead = 12, ci = 0.95, dumvar = NULL)
plot(pred)

plot(fit_VAR)
# tsplot(ts(pred, start=c(1995, 2), end=c(2018, 12), freq=12))
#############
###
x = ts(xom$Close, start=c(1995, 2), end=c(2017, 12), freq=12)
tsplot(x, ylab = 'Closing price', main = 'ExxonMobile Stock price')
lx = log(x)
dlx = diff(lx)
tsplot(dlx)
library(astsa)
# par(mfrow=c(2,4))
# 
# for (i in 1:4){
#   for (j in 1:4){
#     print(i)
#     print(j)
#     fit = arima(lx, c(i,1,j))
#     acf2(fit$residuals)
#   }
# }
acf2(arima(lx, c(14,1,4))$residuals)

ddlx3 = diff(dlx,12) 
acf2(ddlx3)

acf2(dlx)
ma4 = arima(lx, c(0,1,4))
acf2(ma4$residuals, 50)

cc=arima(lx, c(1,1,0))
acf2(cc$residuals,ylim=c(-1,1))
acf2(dlx)
kk=sarima(lx,1,1,4,1,0,0,14)

############## 
######### Changing ACF2 from GitHub Source code #######
#############
acf2 <-
  function(series, max.lag=NULL, plot=TRUE, 
           main=paste("Series: ",deparse(substitute(series))), 
           na.action = na.pass, ...){
    num=length(series)
    if (num > 59 & is.null(max.lag)) { 
      max.lag = max(ceiling(10+sqrt(num)), 4*frequency(series)) } 
    if (num < 60 & is.null(max.lag)) { max.lag = floor(5*log10(num+5)) }
    if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
    ACF=stats::acf(series, max.lag, plot=FALSE, na.action = na.action,...)$acf[-1]
    PACF=stats::pacf(series, max.lag, plot=FALSE, na.action = na.action, ...)$acf
    LAG=1:max.lag/stats::frequency(series)
    if(plot){
      par = graphics::par
      plot = graphics::plot
      grid = graphics::grid
      box = graphics::box
      abline = graphics::abline
      lines = graphics::lines
      frequency = stats::frequency
      minA=min(ACF)  
      maxA=max(ACF)
      minP=min(PACF)
      maxP=max(PACF)
      U=2/sqrt(num)
      L=-U
      minu=min(minA,minP,L)-.01
      maxu=min(max(maxA+.1, maxP+.1), 1)
      old.par <- par(no.readonly = TRUE)
      par(mfrow=c(2,1), mar = c(2.5,2.5,1.5,0.8), mgp = c(1.5,0.6,0), cex.main=1)
      plot(LAG, ACF, type="n", ylim=c(-1,1), main=main)
      ###
      grid(lty=1, col=gray(.9)); box()
      abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
      lines(LAG, ACF, type='h')
      plot(LAG, PACF, type="n", ylim=c(-1,1))
      grid(lty=1, col=gray(.9)); box()
      abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
      lines(LAG, PACF, type='h')
      on.exit(par(old.par))  
    } 
    ACF<-round(ACF,2); PACF<-round(PACF,2)    
    return(cbind(ACF, PACF)) 
  }
############################################
x =  ts(xom$Close, start=c(1995, 2), end=c(2016, 12), freq=12)
test = log(ts(xom$Close[255:277], start = c(2017,1), 
              end = c(2018,6), frequency = 12))
test1 = ts(xoi$Close[255:277], start = c(2017,1), 
           end = c(2018,6), frequency = 12)

# install.packages('TSPred')
library(TSPred)
RMSE <- function(y,y_hat){
  n = length(y)
  ans = 
}


lx = log(x)
acf2(diff(lx))
model1 = arima(lx, c(0,1,0))
acf2(arima(lx, c(0,1,0))$residuals)
predict(model1, n.ahead = 12)
exp(predict(model1, n.ahead = 12)$pred)
plotarimapred(test, model1, xlim = c(2010,2018.6), range.percent = 0.05)


model2 = arima(lx, c(3,1,0))
acf2(arima(lx, c(3,1,0))$residuals)
plotarimapred(test, model2, xlim = c(2010,2018.6), range.percent = 0.05)

model3 = arima(lx, c(0,1,3))
acf2(arima(lx, c(0,1,3))$residuals)
plotarimapred(test, model3, xlim = c(2010,2018.6), range.percent = 0.05)

model4 = arima(lx, c(3,1,3))
acf2(model4$residuals)
plotarimapred(test, model4, xlim = c(2010,2018.6), range.percent = 0.05)


library(forecast)
auto.arima(x, trace = T, ic = 'aic')
auto.arima(log(x), trace = F, ic = 'bic')

ddlx = diff(diff(lx),12)
acf2(ddlx)
sarima1 = sarima(lx, 2,1,1,0,1,1,12)
acf2(sarima1$residuals, 48)
#sarima.for(lx, 0,1,3,0,0,1,14, 12)

tsplot(diff(log(x)))


## Unit Root Test
library(tseries)
adf.test(xom$Close, k = 0)
adf.test(xom$Close)
pp.test(xom$Close)

adf.test(log(xom$Close), k = 0)
adf.test(log(xom$Close))
pp.test(log(xom$Close))
