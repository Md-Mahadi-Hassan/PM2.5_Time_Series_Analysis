### Importing Package

library(forecast)
library(tseries)
library(officer)
library(flextable)
library(dplyr)
library(ggplot2)

### Fetching Data From Online

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

st <- ts(souvenir, frequency=12, start=c(1987,1))


### ---- 01. ACF and PACF Plot For Correlation Diagnosis ----

par(mfrow = c(1, 2))  # side-by-side plots

Acf(ts_data,
    main = "ACF of Original Time Series",
    xlab = "Lag (Months)",
    ylab = "ACF")
Pacf(ts_data, main = "PACF of Original Time Series",
     xlab = "Lag (Months)",
     ylab = "PACF")

par(mfrow = c(1, 1))

# Export Using the Export from R Environment
# Size 1200 x 600 | PNG

### ---- 02. Cross-Validation for Finding Optimum (p,d,q) ----

# Storing the Time Series Data in a Variable

ts_data <- st 

# Generate all (p, d, q) combinations

model_grid <- expand.grid(p = 0:2, d = 0:2, q = 0:2)

# Store results

results <- data.frame()

for (i in 1:nrow(model_grid)) {
  p <- model_grid$p[i]
  d <- model_grid$d[i]
  q <- model_grid$q[i]
  
  cat("Fitting ARIMA(", p, d, q, ")...\n")
  
  fit <- tryCatch({
    Arima(ts_data, order = c(p, d, q))
  }, error = function(e) return(NULL))
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    fitted_vals <- fitted(fit)
    actual_vals <- ts_data
    
    # Only use non-NA values for metrics
    common_idx <- !is.na(residuals) & !is.na(actual_vals)
    res <- residuals[common_idx]
    act <- actual_vals[common_idx]
    
    rmse <- sqrt(mean(res^2))
    mae <- mean(abs(res))
    mape <- mean(abs(res / act)) * 100
    
    results <- rbind(results, data.frame(
      p = p,
      d = d,
      q = q,
      AIC = AIC(fit),
      BIC = BIC(fit),
      logLik = as.numeric(logLik(fit)),
      RMSE = rmse,
      MAE = mae,
      MAPE = mape
    ))
  }
}

# Sort and print models by AIC
results_sorted <- results[order(results$RMSE), ]

print(results_sorted)


### Importing the Results into a Word File

# Create a Word document
doc <- read_docx()

# Convert your results table to Flextable
ft <- flextable(results_sorted)

# Add table to Word document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = "ARIMA_model_selection_results.docx")

### ---- Sorting by (p,d,q) Format ----

results_sorted_by_pdq <- results_sorted[order(results_sorted$p, results_sorted$d, results_sorted$q), ]

## Importing into a Doc File

doc <- read_docx()
ft <- flextable(results_sorted_by_pdq)
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "ARIMA_results_sorted_by_pdq.docx")

### ---- 3. Checking For Stationarity For Selected Model/Models ----

# Suppose I Have Found the Top 5 Models (p,d,q)
# Loop through each selected (p,d,q) combination

# Step 1: Define list of selected (p,d,q) models

selected_models_list <- list(
  c(0, 1, 0),
  c(1, 1, 1),
  c(1, 2, 1),
  c(2, 1, 2),
  c(2, 2, 2)
)

# Step 2: Run ADF test for each model (on the original time series)

adf_results <- data.frame()

for (model in selected_models_list) {
  p <- model[1]
  d <- model[2]
  q <- model[3]
  
  adf_test <- suppressWarnings(adf.test(ts_data))  # No differencing applied
  
  adf_results <- rbind(adf_results, data.frame(
    p = p,
    d = d,
    q = q,
    ADF_p_value = adf_test$p.value
  ))
}

# Step 3: View results

print(adf_results)

# Here,
# p > 0.05 -> Data is Non-stationary (Appropriate for ARIMA)
# p < 0.05 -> Data is Stationary

# So, a p > 0.05 indicates the models usability -> proceed with the best model since all models qualify











































