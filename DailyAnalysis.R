library(lubridate)
data <- fullData
data <- data[data$gold!=".",]
data$gold <- as.numeric(data$gold)
data <- data[data$DGS10!=".",]
data$DGS10 <- as.numeric(data$DGS10)
Dow=as.ts(log(data$DOW[2:nrow(data)])-log(data$DOW[1:nrow(data)-1])) # Fixes the stationarity issues
sp=na.omit(as.ts(log(data$SP500[2:nrow(data)]) - log(data$SP500[1:nrow(data)-1])))
bond=na.omit(as.ts(log(data$DGS10[2:nrow(data)]) - log(data$DGS10[1:nrow(data)-1])))
gold=na.omit(as.ts(log(data$gold[2:nrow(data)]) - log(data$gold[1:nrow(data)-1])))
nyse=na.omit(as.ts(log(data$NYSE[2:nrow(data)]) - log(data$NYSE[1:nrow(data)-1])))

# Mean strucutre modelling

#regmodel=lm(Dow~netexp+unemployment+oil+gold+cpi+ffr+gdp+cci)
regmodel=lm(Dow~sp + bond + gold + nyse)
summary(regmodel)
plot(regmodel,which=1:4)
acf(residuals(regmodel))
pacf(residuals(regmodel))
outliersmonsig <- which(abs(regmodel$residuals) > 3*sqrt(var(regmodel$residuals)))

# probably AR(1)
arimamodel <- arima(residuals(regmodel),order = c(1,0,0))
arimamodel
tsdiag(arimamodel)
acf(resid(arimamodel))
pacf(resid(arimamodel))
plot(ymd(data$DATE)[2:nrow(data)], resid(arimamodel), ylab = "Residual", xlab = "Date")
points(ymd(data$DATE)[outliersmonsig], resid(arimamodel)[outliersmonsig], col = "red")
abline(h = -3*sqrt(var(arimamodel$residuals)), col = "red") # 3 standard deviations below
abline(h = 3*sqrt(var(arimamodel$residuals)), col = "red") # 3 standard deviations above