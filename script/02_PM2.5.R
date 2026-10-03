### Importing Package

library(forecast)
library(tseries)
library(officer)
library(flextable)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Importing the PM2.5 Data

df <- readxl::read_xlsx("data/pm.xlsx")

data <- ts(df[,"conc"], start = c(2000,1,1), frequency = 12)


# Basic Plotting

main_plot <- autoplot(data) + xlab("Year") + ylab("Concentration (µg/m³)") + theme_bw()

# Save the Plot

ggsave("figures/MainTimeSeries.jpg", plot = main_plot, width = 15, height = 8, dpi = 900)

# ggplot
ggplot(df, aes(time, conc)) + labs(x = "Year", y = "Concentration (µg/m³)") + geom_line(color = "black") + theme_bw()

## Decomposition
# Classical decomposition (assumes additive seasonality)
decomp <- decompose(data, type = "additive")

# Using Auto Plot
autoplot(decomp)

# ACF & PACF
par(mfrow = c(1, 2))  # side-by-side plots

Acf(data,
    main = "",
    xlab = "Lag",
    ylab = "ACF")

Pacf(data, main = "",
     xlab = "Lag",
     ylab = "PACF")

par(mfrow = c(1, 1))



# First-order Differencing

data_d1 <- diff(data, differences = 1)

# ACF & PACF of First Order Differenced Data
# ACF & PACF
par(mfrow = c(1, 2))  # side-by-side plots

Acf(data_d1,
    main = "ACF of Differenced Time Series",
    xlab = "Lag (Months)",
    ylab = "ACF")

Pacf(data_d1, main = "PACF of Differenced Time Series",
     xlab = "Lag (Months)",
     ylab = "PACF")

# Stationarity Test

# ADF

adf.test(data)
adf.test(data_d1, k = 2)

# Phillips-Perron

pp.test(data)
pp.test(data_d1)

# KPSS

kpss.test(data)

# Ljung-Box

Box.test(data, lag = 6, type = "Ljung")

# ---- Model Building ----
# Fit ARIMA

ARIMA <- auto.arima(data, seasonal = FALSE)

print(summary(ARIMA))

# Fit SARIMA

SARIMA <- auto.arima(data, seasonal = TRUE)

print(summary(SARIMA))

####



####

# Check Residuals

# ARIMA
checkresiduals(ARIMA)

# SARIMA
checkresiduals(SARIMA)


#---- Forecast ----
ARIMA_fcast <- forecast(ARIMA, h = 24)
print(summary(ARIMA_fcast))

SARIMA_fcast <- forecast(SARIMA, h = 24)
print(summary(SARIMA_fcast))

# ---- Combined Plot ----

df_all <- readxl::read_xlsx("data/pm2025.xlsx")
df_all$Time <- as.Date(df_all$Time, format = "%d/%m/%Y")

df_long <- df_all %>%
  pivot_longer(cols = c(Actual, ARIMA, SARIMA), 
               names_to = "Variable", 
               values_to = "Value")

ggplot(df_long, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "",
       x = "Months (2025)",
       y = "Concentration (µg/m³)") +
  theme_bw()

# ---- Forecast Plot ----
# Start of forecast = end of training data + 1 month
start_date <- as.Date("01/01/2000", format = "%d/%m/%Y")
forecast_start_date <- start_date %m+% months(length(ARIMA$fitted))
forecast_dates <- seq(forecast_start_date, by = "month", length.out = 24)

# ARIMA forecast dataframe
arima_df <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(ARIMA_fcast$mean),
  Lower = as.numeric(ARIMA_fcast$lower[, 2]),  # 95% CI
  Upper = as.numeric(ARIMA_fcast$upper[, 2]),
  Model = "ARIMA"
)

# SARIMA forecast dataframe
sarima_df <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(SARIMA_fcast$mean),
  Lower = as.numeric(SARIMA_fcast$lower[, 2]),
  Upper = as.numeric(SARIMA_fcast$upper[, 2]),
  Model = "SARIMA"
)

# Combine both
combined_df <- bind_rows(arima_df, sarima_df)

ggplot(combined_df, aes(x = Date, y = Forecast, color = Model)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Model), alpha = 0.2, linetype = 0) +
  scale_x_date(date_labels = "%b, %Y", date_breaks = "2 months") +
  labs(title = "ARIMA vs SARIMA Forecast (Next 24 Months)",
       x = "Month", y = "Forecasted Value") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5)
  )






















