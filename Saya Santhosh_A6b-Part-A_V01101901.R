# Install required libraries if not already installed
required_packages <- c("quantmod", "rugarch", "ggplot2", "tseries", "gridExtra")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(quantmod)
library(rugarch)
library(ggplot2)
library(tseries)
library(gridExtra)

# Step 1: Download Historical Data of Amazon
ticker <- "AMZN"
getSymbols(ticker, src = "yahoo", from = "2021-04-01", to = "2024-03-31")

# Extract adjusted close price and calculate returns
data <- Ad(get(ticker))
returns <- 100 * diff(log(data))
returns <- na.omit(returns)  

# Check data structure
print(head(AMZN))
print(str(AMZN))

# Step 2: Calculate Returns
market <- Cl(AMZN)  # Adjusted Close prices
returns <- 100 * diff(log(market))  # Convert to percentage returns
returns <- na.omit(returns)

# Step 3: Fit an ARCH Model
print("\nFitting ARCH Model...")
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "norm")

arch_fit <- ugarchfit(spec = arch_spec, data = returns)
print("ARCH Model Summary:")
print(arch_fit)

# Plot the conditional volatility from the ARCH model
## Extract conditional volatility
cond_volatility <- sigma(arch_fit)

# Create a time series plot for conditional volatility
# Use the index of the returns, which is aligned with the conditional volatility
plot(index(returns), cond_volatility, type = 'l',
     main = 'Conditional Volatility from ARCH Model', 
     xlab = 'Date', ylab = 'Volatility', col = 'blue')
grid()

# Check residuals for autocorrelation
arch_residuals <- residuals(arch_fit)
arch_ljung_box <- Box.test(arch_residuals, lag = 10, type = "Ljung-Box")
print("\nLjung-Box Test for ARCH Model Residuals:")
print(arch_ljung_box)

data <- Ad(get(ticker))
returns <- 100 * diff(log(data))
returns <- na.omit(returns)
# Step 4: Fit a GARCH Model
print("\nFitting GARCH Model...")
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = returns)
print("GARCH Model Summary:")
print(garch_fit)

# Plot the conditional volatility from the GARCH model
# Extract conditional volatility from the fitted model
cond_volatility <- sigma(garch_fit)

# Plot the conditional volatility from the fitted GARCH model
plot(index(returns), cond_volatility, type = 'l',
     main = 'Conditional Volatility from GARCH Model',
     xlab = 'Date', ylab = 'Volatility', col = 'pink')
grid()

garch_forecast <- ugarchforecast(garch_fit, n.ahead = 90)

# Extract forecasted conditional volatility
forecast_volatility <- sigma(garch_forecast)

# Create a time series for forecast dates
forecast_dates <- seq(from = as.Date(tail(index(returns), 1)) + 1, 
                      by = "days", length.out = length(forecast_volatility))

# Plot the forecasted conditional volatility
plot(forecast_dates, forecast_volatility, type = 'l',
     main = '90-Day Forecasted Conditional Volatility from GARCH Model',
     xlab = 'Date', ylab = 'Volatility', col = 'violet')
grid()
# Check residuals for autocorrelation
garch_residuals <- residuals(garch_fit)
garch_ljung_box <- Box.test(garch_residuals, lag = 10, type = "Ljung-Box")
print("\nLjung-Box Test for GARCH Model Residuals:")
print(garch_ljung_box)
# Step 5: Fit GARCH Model with Additional Parameters
print("\nFitting GARCH Model with additional parameters...")

# Specify GARCH model with normal distribution
garch_spec_additional <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                    mean.model = list(armaOrder = c(0, 0)),
                                    distribution.model = "norm")

# Fit the model
garch_fit_additional <- ugarchfit(spec = garch_spec_additional, data = returns)

# Forecast details
garch_forecast_additional <- ugarchforecast(garch_fit_additional, n.ahead = 1)

# Extract forecast details
forecast_mean <- as.numeric(ugarchforecast(garch_fit_additional, n.ahead = 1)@forecast$seriesFor)
forecast_residual_variance <- as.numeric(ugarchforecast(garch_fit_additional, n.ahead = 1)@forecast$sigmaFor)
forecast_variance <- forecast_residual_variance^2

# Print forecast details for the last 3 periods
print("\nForecast Mean (last 3 periods):")
print(tail(forecast_mean, 3))
print("Forecast Residual Variance (last 3 periods):")
print(tail(forecast_residual_variance, 3))
print("Forecast Variance (last 3 periods):")
print(tail(forecast_variance, 3))

# Forecasting with a horizon of 90 days
print("\nForecasting 90 days ahead...")
forecasts <- ugarchforecast(garch_fit_additional, n.ahead = 90)

# Extract forecast residual variance and variance
forecast_residual_variance_90 <- as.numeric(forecasts@forecast$sigmaFor)
forecast_variance_90 <- forecast_residual_variance_90^2

# Create a sequence of dates for plotting the 90-day forecast
forecast_dates <- seq(from = as.Date(tail(index(returns), 1)) + 1, 
                      by = "days", length.out = 90)

# Print forecast residual variance for the 90-day horizon
print("\n90-day Forecast Residual Variance (last 3 periods):")
print(tail(forecast_residual_variance_90^2, 3))

# Step 6: Plot Forecasts
# Plot the 90-day variance forecast
forecast_variance_plot <- ggplot(data = data.frame(Date = forecast_dates,
                                                   Variance = forecast_variance_90),
                                 aes(x = Date, y = Variance)) +
  geom_line(color = 'green') +
  ggtitle('90-Day Variance Forecast') +
  xlab('Date') +
  ylab('Forecasted Variance') +
  theme_minimal()

# Display the plot
print(forecast_variance_plot)

# Plot the 90-day forecasted residual variance
forecast_residual_variance_plot <- ggplot(data = data.frame(Date = forecast_dates,
                                                            ResidualVariance = forecast_residual_variance_90^2),
                                          aes(x = Date, y = ResidualVariance)) +
  geom_line(color = 'maroon') +
  ggtitle('90-Day Forecasted Residual Variance') +
  xlab('Date') +
  ylab('Residual Variance') +
  theme_minimal()

# Arrange and display both plots side by side
grid.arrange(forecast_variance_plot, forecast_residual_variance_plot, ncol = 2)
