# codes that might work

data=read.table("economicData.txt",header =T)
plot(data$DOW_Close,type='o')
reg2 <- lm(DOW_Close~
             poly(time(DATE),degree = 2,raw = T),data=data)
summary(reg2)
myPredict <- predict(reg2)
lines(x = as.vector(time(data$DOW_Close)), 
      y = myPredict, col=2, lwd=2 ) 
# Above graph shows that the data is not stationary
# Thus we cannot use time series model for residuals directly
# btw, my thought is that the residuals happen in non-shock period is natural fluctuation
# we need to transform data then use codes below

acf(resid(reg2))
mod1 <- arima(resid(reg2),order = c(1,0,0))
mod1
tsdiag(mod1)
coeftest(x = mod1)