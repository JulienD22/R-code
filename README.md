# Download the data
gdp <- read.table("/cloud/project/data/gdp.dat")
inf <- read.table("/cloud/project/data/inf.dat")
tspread <- read.table("/cloud/project/data/tspread.dat")

# Part I - Basic Concepts in R 
n <- nrow(gdp) 
dates <- seq(as.Date("2002-01-01"), by = "3 months", length.out = n)

install.packages("zoo")
library(zoo)
gdp_zoo <- zoo(gdp[, 1], dates)
inf_zoo <- zoo(inf[, 1], dates)
tspread_zoo <- zoo(tspread[, 1], dates)

# Transform into quarterly frequencies
gdp_quarterly <- aggregate(gdp_zoo, as.yearqtr, mean)
inf_quarterly <- aggregate(inf_zoo, as.yearqtr, mean)
tspread_quarterly <- aggregate(tspread_zoo, as.yearqtr, mean)

# Check if the series have the same length
length(gdp_quarterly)
length(inf_quarterly)
length(tspread_quarterly)

# Log-difference for inflation and gdp 
gdp_logdiff <- diff(log(gdp_quarterly))
inf_logdiff <- diff(log(inf_quarterly))

# Plot + histograms
plot(index(gdp_quarterly)[-1], gdp_logdiff, type = "l", col = "blue", xlab = "Date", ylab = "Log Difference", main = "GDP Log Difference")
hist(gdp_logdiff, col = "blue", main = "Histogram of GDP Log Difference", xlab = "Log Difference")
plot(index(inf_quarterly)[-1], inf_logdiff, type = "l", col = "blue", xlab = "Date", ylab = "Log Difference", main = "Inflation Log Difference")
hist(inf_logdiff, col = "blue", main = "Histogram of Inflation Log Difference", xlab = "Log Difference")

#Part II - First Analysis
install.packages("tseries")
library(tseries)
jarque_bera_gdp <- jarque.bera.test(gdp_logdiff)
print(jarque_bera_gdp)
jarque_bera_inf <- jarque.bera.test(inf_logdiff)
print(jarque_bera_inf)
jarque_bera_tspread <- jarque.bera.test(tspread_quarterly)
print(jarque_bera_tspread)

# 2) Detect the outlier(s) and replace it(them) by an average value.
# For gdp
replace_outliers_with_mean <- function(x, multiplier = 1.5) {
  
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - multiplier * iqr
  upper_bound <- q[2] + multiplier * iqr
  
  outliers <- x < lower_bound | x > upper_bound
  
  non_outliers_mean <- mean(x[!outliers])
  x[outliers] <- non_outliers_mean
  
  return(x)
}

transformed_gdp <- replace_outliers_with_mean(gdp_logdiff)

cat("Original Data:", gdp_logdiff, "\n")
cat("Transformed Data:", transformed_gdp, "\n")

library(ggplot2)
df <- data.frame(Times = dates[-1], gdp_logdiff, transformed_gdp)
ggplot(df, aes(x = Times)) +
  geom_point(aes(y = gdp_logdiff, color = "gdp_logdiff"), size = 5) +
  geom_point(aes(y = transformed_gdp, color = "transformed_gdp"), size = 3) +
  labs(title = "Two Time Series", y = "Log difference", x = "Time") +
  scale_color_manual(values = c("gdp_logdiff" = "blue", "transformed_gdp" = "red"))

# For inflation
replace_outliers_with_mean <- function(x, multiplier = 1.5) {
  
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - multiplier * iqr
  upper_bound <- q[2] + multiplier * iqr
  
  outliers <- x < lower_bound | x > upper_bound
  
  non_outliers_mean <- mean(x[!outliers])
  x[outliers] <- non_outliers_mean
  
  return(x)
}

transformed_inf <- replace_outliers_with_mean(inf_logdiff)

cat("Original Data:", inf_logdiff, "\n")
cat("Transformed Data:", transformed_inf, "\n")

library(ggplot2)
df <- data.frame(Times = dates[-1], inf_logdiff, transformed_inf)
ggplot(df, aes(x = Times)) +
  geom_point(aes(y = inf_logdiff, color = "inf_logdiff"), size = 5) +
  geom_point(aes(y = transformed_inf, color = "transformed_inf"), size = 3) +
  labs(title = "Two Time Series", y = "Log difference", x = "Time") +
  scale_color_manual(values = c("inf_logdiff" = "blue", "transformed_inf" = "red"))

# For tspread
replace_outliers_with_mean <- function(x, multiplier = 1.5) {
  
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - multiplier * iqr
  upper_bound <- q[2] + multiplier * iqr
  
  outliers <- x < lower_bound | x > upper_bound
  
  non_outliers_mean <- mean(x[!outliers])
  x[outliers] <- non_outliers_mean
  
  return(x)
}

transformed_tspread <- replace_outliers_with_mean(tspread_quarterly)

cat("Original Data:", tspread_quarterly, "\n")
cat("Transformed Data:", transformed_tspread, "\n")

library(ggplot2)
df <- data.frame(Times = dates, tspread_quarterly, transformed_tspread)
ggplot(df, aes(x = Times)) +
  geom_point(aes(y = tspread_quarterly, color = "tspread_quarterly"), size = 5) +
  geom_point(aes(y = transformed_tspread, color = "transformed_tspread"), size = 3) +
  labs(title = "Two Time Series", y = "Log difference", x = "Time") +
  scale_color_manual(values = c("tspread_quarterly" = "blue", "transformed_tspread" = "red"))

# 3) Calculate and plot the cycle of each variable using the Baxter-King and the Hodrick-Prescott filter. Interpret the results.
install.packages("mFilter")
library(mFilter)
gdp_bkfilter <- bkfilter(transformed_gdp)
inf_bkfilter <- bkfilter(transformed_inf)
tspread_bkfilter <- bkfilter(transformed_tspread)

gdp_hpfilter <- hpfilter(transformed_gdp, freq = 4)
inf_hpfilter <- hpfilter(transformed_inf, freq = 4)
tspread_hpfilter <- hpfilter(transformed_tspread, freq = 4)

plot(gdp_bkfilter)
plot(gdp_hpfilter)

plot(inf_bkfilter)
plot(tspread_bkfilter)

plot(inf_hpfilter)
plot(tspread_hpfilter)


####### CE CODE fonctionne pas
par(mfrow = c(3, 2))
plot(index(gdp_quarterly)[-1], cbind(transformed_gdp, gdp_bkfilter$trend, gdp_hpfilter$cycle),
     col = c("green", "red", "blue"), type = "l", lty = 1,
     main = "GDP", ylab = "Log Difference")
legend("topright", legend = c("Original", "Baxter-King", "Hodrick-Prescott"), col = c("green", "red", "blue"), lty = 1)
plot(index(inf_quarterly)[-1], cbind(transformed_inf, na.approx(inf_bkfilter$trend), inf_hpfilter$cycle),
     col = c("blue", "red", "green"), type = "l", lty = 1,
     main = "Inflation", ylab = "Log Difference")
legend("topright", legend = c("Original", "Baxter-King", "Hodrick-Prescott"), col = c("blue", "red", "green"), lty = 1)
plot(index(tspread_quarterly)[-1], cbind(transformed_tspread, tspread_bkfilter$trend, tspread_hpfilter$cycle),
     col = c("purple", "red", "blue"), type = "l", lty = 1,
     main = "Term Spread", ylab = "Value")
legend("topright", legend = c("Original", "Baxter-King", "Hodrick-Prescott"), col = c("purple", "red", "blue"), lty = 1)
########

# Part III - Univariate Forecast 
library(forecast)
library(tseries)

# For gdp
acf_values <- acf(transformed_gdp, lag.max = 30)
pacf_values <- pacf(transformed_gdp, lag.max = 20)


result1 <- adf.test(transformed_gdp)
if (result1$p.value < 0.05) {
  cat("Reject the null hypothesis. The time series is stationary.\n")
} else {
  cat("Fail to reject the null hypothesis. The time series may be non-stationary.\n")
}
gdp_firstdiff <- diff(transformed_gdp)
result2 <- adf.test(gdp_firstdiff)
if (result2$p.value < 0.05) {
  cat("Reject the null hypothesis. The time series is stationary.\n")
} else {
  cat("Fail to reject the null hypothesis. The time series may be non-stationary.\n")
}

acf_values <- acf(gdp_firstdiff, lag.max = 30)
pacf_values <- pacf(gdp_firstdiff, lag.max = 20)

# According to the acf and pacf
p <- 1
q <- 7
fitted_model <- arma(gdp_firstdiff, order = c(p, q))

# Diagnostic Checking
residuals <- residuals(fitted_model)
Box.test(residuals, type = "Ljung-Box", lag = 20)  # Ljung-Box test for white noise
Acf(residuals)  # ACF of residuals
Pacf(residuals)  # PACF of residuals

# Forecasting
forecast_values <- forecast(fitted_model, h = 10)

# Plotting 10-quarter ahead forecast
plot(forecast_values, main = "ARMA Forecast for GDP Log Difference", xlab = "Time", ylab = "Value")

# Evaluation (you need to have actual values for the next 10 quarters)
actual_values <- c(...)  # Replace ... with actual values
accuracy(forecast_values, actual_values)

########DFfikjfifjifjfiji
