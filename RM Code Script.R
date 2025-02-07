# ------------------------------------------------------------------------------

# Title: Research Methodology Assignment
# Author: Shreyas Urgunde
# Date: 6th February 2025 
# Description: This script performs a comprehensive analysis of currency returns 
#              for a set of developed and emerging market currencies. It begins by 
#              calculating log returns for each currency. Subsequently, it computes 
#              volatility and Value-at-Risk (VaR) forecasts using four distinct 
#              models. Finally, the script evaluates the performance of these 
#              models through backtesting methods to assess their accuracy and 
#              reliability in predicting risk.

# ------------------------------------------------------------------------------

# Load necessary libraries
library(dplyr)       # For data manipulation
library(readxl)      # For reading Excel files
library(zoo)         # For time series operations
library(rugarch)     # For GARCH modeling (if needed in later sections)

# Install and load the 'zoo' package if not already installed
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo)

# ------------------------------------------------------------------------------
## Load and Prepare the Data
# ------------------------------------------------------------------------------

# Define the file path to the dataset
file_path <- "C:/Users/Asus/Desktop/5582804/Data_File.xlsx"

# Read the data from the Excel file
currency_data <- read_excel(file_path)

# Clean the data to account for the burn-in period
cleaned_data <- currency_data[-c(1, 2), ]

# Convert the 'Date' column to Date format
cleaned_data <- cleaned_data %>%
  mutate(Date = as.Date(Date))

# Convert all currency columns to numeric format
currency_columns <- setdiff(names(cleaned_data), "Date")
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(currency_columns), as.numeric))

# ------------------------------------------------------------------------------

## Part 1: Calculate currency returns for each currency

# ------------------------------------------------------------------------------

currency_returns <- cleaned_data %>%
  mutate(across(all_of(currency_columns), 
                ~ -log(lead(.x)) + log(.x), 
                .names = "{.col}_return")) %>%
  select(Date, ends_with("_return"))

# Remove rows with NA values (due to the lead function)
currency_returns <- na.omit(currency_returns)

# Identify the columns for developed market currencies and the EM currency
developed_market_currencies <- names(currency_returns)[2:10]
emerging_market_currency <- names(currency_returns)[11]

# ------------------------------------------------------------------------------

## Part 3: Construct the dollar portfolio and calculate its returns

# ------------------------------------------------------------------------------

# Calculate the dollar portfolio returns as the average of the 9 developed market currency returns

dollar_portfolio_returns <- currency_returns %>%
  rowwise() %>%
  mutate(DOL_return = mean(c_across(all_of(developed_market_currencies)))) %>%
  ungroup() %>%
  select(Date, DOL_return)

# Extract the emerging market currency returns
emerging_market_returns <- currency_returns %>%
  select(Date, all_of(emerging_market_currency))

# Rename the emerging market column for clarity
colnames(emerging_market_returns)[2] <- "EM_return"

# Combine dollar portfolio and emerging market returns into one data frame
combined_returns <- dollar_portfolio_returns %>%
  left_join(emerging_market_returns, by = "Date")

# View the first few rows of the combined returns
head(combined_returns)

# ------------------------------------------------------------------------------
# Compare Statistical Properties of Dollar Portfolio and EM Portfolio
# ------------------------------------------------------------------------------

# load necessary libraries

install.packages("moments")
install.packages("tidyverse")
library(tseries)
library(tidyverse)
library(moments)  # For skewness and kurtosis
install.packages("forecast")
library(forecast)  # For autocorrelation functions (ACF)

# Specify the lag for ACF
acf_lag <- 1  # Lag-1 autocorrelation

# Calculate summary statistics for the dollar portfolio
dol_summary <- combined_returns %>%
  summarise(
    Mean = mean(DOL_return, na.rm = TRUE),
    Variance = var(DOL_return, na.rm = TRUE),
    StdDev = sd(DOL_return, na.rm = TRUE),
    Min = min(DOL_return, na.rm = TRUE),
    Max = max(DOL_return, na.rm = TRUE),
    Skewness = skewness(DOL_return, na.rm = TRUE),
    Kurtosis = kurtosis(DOL_return, na.rm = TRUE),
    ACF_Lag1 = acf(DOL_return, plot = FALSE, lag.max = acf_lag)$acf[2],
    ACF_Squared_Lag1 = acf(DOL_return^2, plot = FALSE, lag.max = acf_lag)$acf[2],
    JB_Stat = jarque.bera.test(DOL_return)$statistic,
    JB_p_Value = jarque.bera.test(DOL_return)$p.value
  )

# Calculate summary statistics for the emerging market currency
em_summary <- combined_returns %>%
  summarise(
    Mean = mean(EM_return, na.rm = TRUE),
    Variance = var(EM_return, na.rm = TRUE),
    StdDev = sd(EM_return, na.rm = TRUE),
    Min = min(EM_return, na.rm = TRUE),
    Max = max(EM_return, na.rm = TRUE),
    Skewness = skewness(EM_return, na.rm = TRUE),
    Kurtosis = kurtosis(EM_return, na.rm = TRUE),
    ACF_Lag1 = acf(EM_return, plot = FALSE, lag.max = acf_lag)$acf[2],
    ACF_Squared_Lag1 = acf(EM_return^2, plot = FALSE, lag.max = acf_lag)$acf[2],
    JB_Stat = jarque.bera.test(EM_return)$statistic,
    JB_p_Value = jarque.bera.test(EM_return)$p.value
  )

# Combine the summary statistics into a single data frame for comparison
combined_summary_stats <- bind_rows(
  dol_summary %>% mutate(Portfolio = "Dollar Portfolio"),
  em_summary %>% mutate(Portfolio = "Emerging Market Currency")
) %>%
  select(Portfolio, everything())  # Ensure Portfolio column is first

# Print the combined summary statistics
print("Combined Summary Statistics:")
print(combined_summary_stats)
View(combined_summary_stats)

# ------------------------------------------------------------------------------
# Visualize Plots in Three Separate Combined Graphs
# ------------------------------------------------------------------------------

library(ggplot2)
library(patchwork)  # For combining plots

# ------------------------------------------------------------------------------
# Create Individual Plots
# ------------------------------------------------------------------------------

# Time Series: DOL Returns
p1 <- ggplot(combined_returns, aes(x = Date, y = DOL_return)) +
  geom_line(color = "blue") +
  labs(title = "Dollar Portfolio Returns (Developed Markets)",
       x = "Date", y = "Daily Returns") +
  theme_minimal()

# Time Series: EM Returns
p2 <- ggplot(combined_returns, aes(x = Date, y = EM_return)) +
  geom_line(color = "red") +
  labs(title = "Emerging Market Currency Returns (Argentina)",
       x = "Date", y = "Daily Returns") +
  theme_minimal()

# Histogram: DOL Returns
p3 <- ggplot(combined_returns, aes(x = DOL_return)) +
  geom_histogram(binwidth = 0.001, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram: Dollar Portfolio",
       x = "Daily Returns", y = "Frequency") +
  theme_minimal()

# Histogram: EM Returns
p4 <- ggplot(combined_returns, aes(x = EM_return)) +
  geom_histogram(binwidth = 0.001, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram: Emerging Market Currency",
       x = "Daily Returns", y = "Frequency") +
  theme_minimal()

# QQ Plot: DOL Returns
p5 <- ggplot(combined_returns, aes(sample = DOL_return)) +
  stat_qq(color = "#1f77b4", alpha = 0.7) +
  stat_qq_line(color = "black", linetype = "dashed") +
  labs(title = "QQ Plot: Dollar Portfolio",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# QQ Plot: EM Returns
p6 <- ggplot(combined_returns, aes(sample = EM_return)) +
  stat_qq(color = "#d62728", alpha = 0.7) +
  stat_qq_line(color = "black", linetype = "dashed") +
  labs(title = "QQ Plot: Emerging Market Currency",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# ------------------------------------------------------------------------------
# Combine Plots into Three Separate Graphs
# ------------------------------------------------------------------------------

# Combined Time Series Plot
time_series_combined <- p1 | p2
time_series_combined <- time_series_combined + 
  plot_annotation(title = "Time Series of Developed vs. Emerging Market Currency Returns",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Combined Histogram Plot
histogram_combined <- p3 | p4
histogram_combined <- histogram_combined + 
  plot_annotation(title = "Histograms of Developed vs. Emerging Market Currency Returns",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Combined QQ Plot
qq_combined <- p5 | p6
qq_combined <- qq_combined + 
  plot_annotation(title = "QQ Plots of Developed vs. Emerging Market Currency Returns",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ------------------------------------------------------------------------------
# Display the Plots
# ------------------------------------------------------------------------------

print(time_series_combined)
print(histogram_combined)
print(qq_combined)

# ------------------------------------------------------------------------------

## Part 4: Calculation of Volatility Forecasts

# ------------------------------------------------------------------------------

## Load and install necessary packages

install.packages("rugarch") # For GARCH modeling
library(rugarch)  

# Filter data to start from January 2000 (burn-in period is 1999)
combined_returns_filtered <- combined_returns %>%
  filter(Date >= as.Date("2000-01-01"))

## Function to calculate Moving Average Volatility (10-week window)

calculate_ma_volatility <- function(returns, window = 50) {  # 50 days = 10 weeks
  volatility <- sqrt(rollapply(returns, width = window, FUN = var, fill = NA, align = "right"))
  return(volatility)
}

## Function to calculate EWMA Volatility

calculate_ewma_volatility <- function(returns, lambda = 0.94) {
  n <- length(returns)
  ewma_volatility <- numeric(n)
  ewma_volatility[1] <- var(returns, na.rm = TRUE)  # Initialize with variance of the first observation
  
  for (i in 2:n) {
    ewma_volatility[i] <- lambda * ewma_volatility[i-1] + (1 - lambda) * returns[i-1]^2
  }
  
  ewma_volatility <- sqrt(ewma_volatility)
  return(ewma_volatility)
}

## Information Criterion for the Best GARCH Model

test_garch_models <- function(returns, max_p = 3, max_q = 3) {
  # Initialize an empty dataframe to store model evaluation results
  results <- data.frame(
    Model = character(),   # Model name (GARCH(p,q))
    AIC = numeric(),       # Akaike Information Criterion (AIC)
    BIC = numeric(),       # Bayesian Information Criterion (BIC)
    LogLik = numeric(),    # Log-likelihood value
    Convergence = logical(), # Indicates if the model successfully converged
    stringsAsFactors = FALSE
  )
  
  # Display range of models being tested
  cat("Testing GARCH models from (1,1) to (", max_p, ",", max_q, ")\n", sep = "")
  
  # Iterate over all combinations of p and q within the specified range
  for(p in 1:max_p) {
    for(q in 1:max_q) {
      model_name <- paste0("GARCH(", p, ",", q, ")")
      cat("\nAttempting", model_name, "...")
      
      # Try fitting the GARCH model and catch potential errors
      tryCatch({
        spec <- ugarchspec(
          variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
          distribution.model = "norm"
        )
        
        fit <- ugarchfit(spec, data = returns, solver = "hybrid")
        
        # Check if the model successfully converged
        if(fit@fit$convergence == 0) {
          ic <- infocriteria(fit)  # Extract information criteria (AIC, BIC, etc.)
          results <- rbind(results, data.frame(
            Model = model_name,
            AIC = ic[1],
            BIC = ic[2],
            LogLik = ic[3],
            Convergence = TRUE
          ))
          cat(" Success!")
        } else {
          cat(" Convergence failed")
        }
      }, error = function(e) {
        cat(" Estimation failed:", conditionMessage(e))
      })
    }
  }
  
  # Display final results
  if(nrow(results) > 0) {
    cat("\n\nSuccessful Models:\n")
    print(results[order(results$AIC), ])  # Print models sorted by AIC (best first)
    
    best_model <- results[which.min(results$AIC), ]
    cat("\nBest Model:", best_model$Model, 
        "with AIC:", round(best_model$AIC, 4),
        "and BIC:", round(best_model$BIC, 4), "\n")
  } else {
    cat("\n\nNo models successfully converged. Try increasing max_p/max_q or check your data.\n")
  }
  
  return(results)
}

# Ensure your data is clean (remove NA values)
clean_returns <- na.omit(combined_returns_filtered$DOL_return)

# Run the GARCH model comparison with a specified range of (p, q)
garch_results <- test_garch_models(clean_returns, max_p = 2, max_q = 2)

# Store and simultaneously print results
(garch_results <- test_garch_models(clean_returns))

## Function to fit GARCH(2,1) and calculate volatility (as per the information criteria test)

calculate_garch_volatility <- function(returns) {
  # Specify GARCH(2,1) model
  garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                           distribution.model = "norm")
  
  # Fit GARCH model
  garch_fit <- ugarchfit(spec = garch_spec, data = returns)
  
  # Extract conditional volatility
  garch_volatility <- sigma(garch_fit)
  return(garch_volatility)
}

# Calculate volatility forecasts for the dollar portfolio (using filtered data)
dollar_volatility_forecasts <- combined_returns_filtered %>%
  mutate(
    DOL_MA_Volatility = calculate_ma_volatility(DOL_return),
    DOL_EWMA_Volatility = calculate_ewma_volatility(DOL_return),
    DOL_GARCH_Volatility = calculate_garch_volatility(DOL_return)
  ) %>%
  select(Date, starts_with("DOL_"))

# Calculate volatility forecasts for the emerging market currency (using filtered data)
em_volatility_forecasts <- combined_returns_filtered %>%
  mutate(
    EM_MA_Volatility = calculate_ma_volatility(EM_return),
    EM_EWMA_Volatility = calculate_ewma_volatility(EM_return),
    EM_GARCH_Volatility = calculate_garch_volatility(EM_return)
  ) %>%
  select(Date, starts_with("EM_"))

# Combine the volatility forecasts into a single data frame
volatility_forecasts <- dollar_volatility_forecasts %>%
  left_join(em_volatility_forecasts, by = "Date")

# View the volatility forecasts
View(volatility_forecasts)

# Export volatility forecasts to a CSV file
write.csv(volatility_forecasts, file = "volatility_forecasts.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Visualize the volatility forecasts 
# ------------------------------------------------------------------------------

# Load necessary libraries
library(scales)  # For better axis formatting
if (!requireNamespace("ggthemes", quietly = TRUE)) {
  install.packages("ggthemes")  # For professional themes
}
library(ggthemes)

# Custom theme for neat graphs
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "serif"),
    axis.title = element_text(size = 14, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    axis.title.x = element_blank(), 
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", family = "serif"),  
    legend.text = element_text(size = 10, family = "serif"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Define event periods for grey bars (Dollar Portfolio)
event_periods_dol <- data.frame(
  start = as.Date(c("2008-09-15", "2020-03-01")),
  end = as.Date(c("2009-06-30", "2020-12-31")),
  event = c("2008 Financial Crisis", "COVID-19 Pandemic")
)

# Plot volatility forecasts for the dollar portfolio
dollar_plot <- ggplot() +
  # Add grey bars for event periods
  geom_rect(data = event_periods_dol, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey80", alpha = 0.5) +
  # Add volatility lines
  geom_line(data = volatility_forecasts, aes(x = Date, y = DOL_MA_Volatility, color = "MA (10-week)"), size = 1, linetype = "solid") +
  geom_line(data = volatility_forecasts, aes(x = Date, y = DOL_EWMA_Volatility, color = "EWMA"), size = 1, linetype = "solid") +
  geom_line(data = volatility_forecasts, aes(x = Date, y = DOL_GARCH_Volatility, color = "GARCH"), size = 1, linetype = "solid") +
  labs(title = "Volatility Forecasts for Dollar Portfolio",
       x = NULL,
       y = "Volatility",
       color = "Model") +
  scale_color_manual(values = c("MA (10-week)" = "#1f77b4", "EWMA" = "#ff7f0e", "GARCH" = "#2ca02c")) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y")) +
  custom_theme +
  # Add label for 2008 Financial Crisis (no arrow)
  annotate("text", x = as.Date("2008-09-15"), y = 0.016, label = "2008 Financial Crisis", color = "black", size = 3, hjust = 0) +
  # Add label for COVID-19 Pandemic (moved slightly lower)
  annotate("text", x = as.Date("2020-03-01") + 100, y = 0.012, label = "COVID-19 Pandemic", color = "black", size = 3, hjust = 0)

# Define event periods for grey bars (Argentine Peso)
event_periods_em <- data.frame(
  start = as.Date(c("2001-12-01", "2018-08-01", "2023-01-01")),
  end = as.Date(c("2002-12-31", "2022-12-31", "2023-12-31")),
  event = c("1998–2002\nArgentine Great Depression", "2018–Present\nArgentine Monetary Crisis", "2018–Present\nArgentine Monetary Crisis")
)

# Plot volatility forecasts for the Argentine Peso
em_plot <- ggplot() +
  # Add grey bars for event periods
  geom_rect(data = event_periods_em[1, ], aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey80", alpha = 0.5) +
  geom_rect(data = event_periods_em[2, ], aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey80", alpha = 0.5) +
  geom_rect(data = event_periods_em[3, ], aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey80", alpha = 0.5) +
  # Add volatility lines
  geom_line(data = volatility_forecasts, aes(x = Date, y = EM_MA_Volatility, color = "MA (10-week)"), size = 1, linetype = "solid") +
  geom_line(data = volatility_forecasts, aes(x = Date, y = EM_EWMA_Volatility, color = "EWMA"), size = 1, linetype = "solid") +
  geom_line(data = volatility_forecasts, aes(x = Date, y = EM_GARCH_Volatility, color = "GARCH"), size = 1, linetype = "solid") +
  labs(title = "Volatility Forecasts for Argentine Peso",
       x = NULL,  
       y = "Volatility",
       color = "Model") +
  scale_color_manual(values = c("MA (10-week)" = "#1f77b4", "EWMA" = "#ff7f0e", "GARCH" = "#2ca02c")) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"), limits = c(as.Date("2000-01-01"), as.Date("2023-12-31"))) +  # Ensure x-axis ends at 2023
  scale_y_continuous(limits = c(0, 0.3)) +
  custom_theme +
  # Add horizontal labels for events with reduced font size and adjusted placement
  geom_text(data = event_periods_em[1, ], aes(x = start + (end - start) / 2, y = 0.15, label = event), 
            color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  geom_text(data = event_periods_em[2, ], aes(x = as.Date("2020-01-01"), y = 0.15, label = "2018–Present\nArgentine Monetary Crisis"), 
            color = "black", size = 3, hjust = 0.5, vjust = -0.5)


# Combine the two volatility plots into a single pane using patchwork
combined_volatility_plot <- dollar_plot / em_plot  # '/' stacks plots vertically

# Display the combined plot
print(combined_volatility_plot)

# Save the graph as a high-quality image
ggsave("combined_vol_plot.png", plot = combined_volatility_plot, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
  
### Part 5: Value-at-Risk forecasts

# ------------------------------------------------------------------------------
  
# Calculate mean returns for the dollar portfolio and emerging market currency
mean_dol_return <- mean(volatility_forecasts$DOL_return, na.rm = TRUE)
mean_em_return <- mean(volatility_forecasts$EM_return, na.rm = TRUE)

# Calculate VaR(1%) for the dollar portfolio
dollar_var <- volatility_forecasts %>%
  mutate(
    DOL_MA_VaR = -(mean_dol_return + (-2.33) * DOL_MA_Volatility),  # VaR for MA (10-week)
    DOL_EWMA_VaR = -(mean_dol_return + (-2.33) * DOL_EWMA_Volatility),  # VaR for EWMA
    DOL_GARCH_VaR = -(mean_dol_return + (-2.33) * DOL_GARCH_Volatility)  # VaR for GARCH
  ) %>%
  select(Date, starts_with("DOL_"))

# Calculate VaR(1%) for the emerging market currency
em_var <- volatility_forecasts %>%
  mutate(
    EM_MA_VaR = -(mean_em_return + (-2.33) * EM_MA_Volatility),  # VaR for MA (10-week)
    EM_EWMA_VaR = -(mean_em_return + (-2.33) * EM_EWMA_Volatility),  # VaR for EWMA
    EM_GARCH_VaR = -(mean_em_return + (-2.33) * EM_GARCH_Volatility)  # VaR for GARCH
  ) %>%
  select(Date, starts_with("EM_"))

# Combine VaR forecasts into a single data frame
var_forecasts <- dollar_var %>%
  left_join(em_var, by = "Date")

# View the first few rows of the VaR forecasts
head(var_forecasts)

## Historical Simulation for VaR(1%)

# Define the window size for historical simulation (2 years = 504 days)
HS_w <- 504

# Compute Historical VaR using rollapply with type = 1 to match professor's method
rolling_historical_var <- combined_returns %>%
  mutate(
    DOL_Historical_VaR = -rollapply(DOL_return, width = HS_w, 
                                    FUN = function(x) quantile(x, probs = 0.01, type = 1), 
                                    fill = NA, align = "right"),
    
    EM_Historical_VaR = -rollapply(EM_return, width = HS_w, 
                                   FUN = function(x) quantile(x, probs = 0.01, type = 1), 
                                   fill = NA, align = "right")
  ) %>%
  select(Date, DOL_Historical_VaR, EM_Historical_VaR)

# Merge with model-based VaR forecasts
final_var_forecasts <- var_forecasts %>%
  left_join(rolling_historical_var, by = "Date")

# View results
head(final_var_forecasts)
View(final_var_forecasts)

# Save the final VaR forecasts to a CSV file
write.csv(final_var_forecasts, file = "Final_VaR_Forecasts.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Visualize the VsR forecasts 
# ------------------------------------------------------------------------------

# Custom theme for the graphs
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "serif"),
    axis.title = element_text(size = 14, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    axis.title.x = element_blank(),  # Remove "Date" label
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", family = "serif"),  # Bold "Model" label
    legend.text = element_text(size = 10, family = "serif"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Plot VaR forecasts for the dollar portfolio
dollar_var_plot <- ggplot(final_var_forecasts, aes(x = Date)) +
  geom_line(aes(y = DOL_MA_VaR, color = "MA (10-week)"), size = 1, linetype = "solid") +
  geom_line(aes(y = DOL_EWMA_VaR, color = "EWMA"), size = 1, linetype = "solid") +
  geom_line(aes(y = DOL_GARCH_VaR, color = "GARCH"), size = 1, linetype = "solid") +
  geom_line(aes(y = DOL_Historical_VaR, color = "Historical Simulation"), size = 1, linetype = "solid") +
  labs(title = "VaR Forecasts for Dollar Portfolio (Developed Markets)",
       x = NULL,  # Remove "Date" label
       y = "VaR (1%)",
       color = "Model") +
  scale_color_manual(values = c("MA (10-week)" = "#1f77b4", "EWMA" = "#ff7f0e", "GARCH" = "#2ca02c", "Historical Simulation" = "#d62728")) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y")) +
  custom_theme

# Plot VaR forecasts for the emerging market currency
em_var_plot <- ggplot(final_var_forecasts, aes(x = Date)) +
  geom_line(aes(y = EM_MA_VaR, color = "MA (10-week)"), size = 1, linetype = "solid") +
  geom_line(aes(y = EM_EWMA_VaR, color = "EWMA"), size = 1, linetype = "solid") +
  geom_line(aes(y = EM_GARCH_VaR, color = "GARCH"), size = 1, linetype = "solid") +
  geom_line(aes(y = EM_Historical_VaR, color = "Historical Simulation"), size = 1, linetype = "solid") +
  labs(title = "VaR Forecasts for Emerging Market Currency (Argentina)",
       x = NULL,  # Remove "Date" label
       y = "VaR (1%)",
       color = "Model") +
  scale_color_manual(values = c("MA (10-week)" = "#1f77b4", "EWMA" = "#ff7f0e", "GARCH" = "#2ca02c", "Historical Simulation" = "#d62728")) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y")) +
  custom_theme

# Display the plots
print(dollar_var_plot)
print(em_var_plot)

# Combine the two plots into a single pane using patchwork
combined_plot <- dollar_var_plot / em_var_plot  # '/' stacks plots vertically

# Display the combined plot
print(combined_plot)

# Save the graphs as high-quality images
ggsave("combined_VaR_forecasts.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------

### Part 6: Backtesting the forecasts given by the four VaR models 

# ------------------------------------------------------------------------------

# Function for the Bernoulli Coverage Test 

bernoulli_coverage_test <- function(returns, var_forecasts, alpha = 0.01) {
  breaches <- returns < -var_forecasts
  n_breaches <- sum(breaches, na.rm = TRUE)
  n_obs <- length(returns)
  
  # Expected number of breaches
  expected_breaches <- alpha * n_obs
  
  # Bernoulli test statistic
  test_statistic <- -2 * log((1 - alpha)^(n_obs - n_breaches) * alpha^n_breaches) +
    2 * log((1 - n_breaches / n_obs)^(n_obs - n_breaches) * (n_breaches / n_obs)^n_breaches)
  
  # p-value using chi-squared distribution
  p_value <- 1 - pchisq(test_statistic, df = 1)
  
  return(list(n_breaches = n_breaches, p_value = p_value))
}

# Define a function for the Independence Test

independence_test <- function(returns, var_forecasts) {
  breaches <- returns < -var_forecasts
  n_obs <- length(returns)
  
  # Transition matrix for breaches
  transition_matrix <- table(factor(breaches[-n_obs], levels = c(FALSE, TRUE)),
                             factor(breaches[-1], levels = c(FALSE, TRUE)))
  
  # Calculate probabilities of transitions
  p00 <- transition_matrix[1, 1] / sum(transition_matrix[1, ])
  p01 <- transition_matrix[1, 2] / sum(transition_matrix[1, ])
  p10 <- transition_matrix[2, 1] / sum(transition_matrix[2, ])
  p11 <- transition_matrix[2, 2] / sum(transition_matrix[2, ])
  
  # Likelihood under independence
  likelihood_independence <- (1 - p01)^transition_matrix[1, 1] * p01^transition_matrix[1, 2] *
    (1 - p11)^transition_matrix[2, 1] * p11^transition_matrix[2, 2]
  
  # Likelihood under the null hypothesis of correct coverage
  likelihood_coverage <- (1 - mean(breaches))^sum(!breaches) * mean(breaches)^sum(breaches)
  
  # Test statistic
  test_statistic <- -2 * log(likelihood_coverage / likelihood_independence)
  
  # p-value using chi-squared distribution
  p_value <- 1 - pchisq(test_statistic, df = 1)
  
  return(list(p_value = p_value))
}

# Filter combined_returns to match the dates in final_var_forecasts
aligned_returns <- combined_returns %>%
  filter(Date %in% final_var_forecasts$Date)

# Backtest VaR models for the dollar portfolio
dollar_backtest_results <- list(
  MA = bernoulli_coverage_test(aligned_returns$DOL_return, final_var_forecasts$DOL_MA_VaR),
  EWMA = bernoulli_coverage_test(aligned_returns$DOL_return, final_var_forecasts$DOL_EWMA_VaR),
  GARCH = bernoulli_coverage_test(aligned_returns$DOL_return, final_var_forecasts$DOL_GARCH_VaR),
  Historical = bernoulli_coverage_test(aligned_returns$DOL_return, final_var_forecasts$DOL_Historical_VaR)
)

# Backtest VaR models for the emerging market currency
em_backtest_results <- list(
  MA = bernoulli_coverage_test(aligned_returns$EM_return, final_var_forecasts$EM_MA_VaR),
  EWMA = bernoulli_coverage_test(aligned_returns$EM_return, final_var_forecasts$EM_EWMA_VaR),
  GARCH = bernoulli_coverage_test(aligned_returns$EM_return, final_var_forecasts$EM_GARCH_VaR),
  Historical = bernoulli_coverage_test(aligned_returns$EM_return, final_var_forecasts$EM_Historical_VaR)
)

# Print backtest results for the dollar portfolio
print("Dollar Portfolio Backtest Results (Bernoulli Coverage Test):")
print(dollar_backtest_results)

# Print backtest results for the emerging market currency
print("Emerging Market Currency Backtest Results (Bernoulli Coverage Test):")
print(em_backtest_results)

# Create a comparison table for the results of back-testing
bernoulli_results <- data.frame(
  Model = c("MA", "EWMA", "GARCH", "Historical"),
  Dollar_Portfolio_Breaches = sapply(dollar_backtest_results, function(x) x$n_breaches),
  Dollar_Portfolio_p_value = sapply(dollar_backtest_results, function(x) x$p_value),
  Emerging_Market_Breaches = sapply(em_backtest_results, function(x) x$n_breaches),
  Emerging_Market_p_value = sapply(em_backtest_results, function(x) x$p_value)
)

# Print the table
print(bernoulli_results)
View(bernoulli_results)

# Perform independence tests for the dollar portfolio
dollar_independence_results <- list(
  MA = independence_test(aligned_returns$DOL_return, final_var_forecasts$DOL_MA_VaR),
  EWMA = independence_test(aligned_returns$DOL_return, final_var_forecasts$DOL_EWMA_VaR),
  GARCH = independence_test(aligned_returns$DOL_return, final_var_forecasts$DOL_GARCH_VaR),
  Historical = independence_test(aligned_returns$DOL_return, final_var_forecasts$DOL_Historical_VaR)
)

# Perform independence tests for the emerging market currency
em_independence_results <- list(
  MA = independence_test(aligned_returns$EM_return, final_var_forecasts$EM_MA_VaR),
  EWMA = independence_test(aligned_returns$EM_return, final_var_forecasts$EM_EWMA_VaR),
  GARCH = independence_test(aligned_returns$EM_return, final_var_forecasts$EM_GARCH_VaR),
  Historical = independence_test(aligned_returns$EM_return, final_var_forecasts$EM_Historical_VaR)
)

# Print independence test results for the dollar portfolio
print("Dollar Portfolio Independence Test Results:")
print(dollar_independence_results)

# Print independence test results for the emerging market currency
print("Emerging Market Currency Independence Test Results:")
print(em_independence_results)

# Create a data frame to store the independence test results
independence_results <- data.frame(
  Model = c("MA", "EWMA", "GARCH", "Historical"),
  Dollar_Portfolio_p_value = sapply(dollar_independence_results, function(x) x$p_value),
  Emerging_Market_p_value = sapply(em_independence_results, function(x) x$p_value)
)

# Print the table
print(independence_results)
View(independence_results)

# ------------------------------------------------------------------------------
### Part 6.1: Explore the additional backtesting methods
# ------------------------------------------------------------------------------

## Function: Christoffersen's Conditional Coverage Test

christoffersen_test <- function(returns, var_forecasts, alpha = 0.01) {
  # Calculate VaR breaches
  breaches <- returns < -var_forecasts
  n_breaches <- sum(breaches, na.rm = TRUE)
  n_obs <- length(returns)
  
  # Create a transition matrix for breaches
  transition_matrix <- table(factor(breaches[-n_obs], levels = c(FALSE, TRUE)),
                             factor(breaches[-1], levels = c(FALSE, TRUE)))
  
  # Calculate probabilities of transitions
  p00 <- transition_matrix[1, 1] / sum(transition_matrix[1, ])
  p01 <- transition_matrix[1, 2] / sum(transition_matrix[1, ])
  p10 <- transition_matrix[2, 1] / sum(transition_matrix[2, ])
  p11 <- transition_matrix[2, 2] / sum(transition_matrix[2, ])
  
  # Calculate the likelihood ratio for independence
  likelihood_independence <- (1 - p01)^transition_matrix[1, 1] * p01^transition_matrix[1, 2] *
    (1 - p11)^transition_matrix[2, 1] * p11^transition_matrix[2, 2]
  
  # Calculate the likelihood under the null hypothesis of correct coverage
  likelihood_coverage <- (1 - alpha)^sum(!breaches) * alpha^sum(breaches)
  
  # Calculate the test statistic
  test_statistic <- -2 * log(likelihood_coverage / likelihood_independence)
  
  # Calculate the p-value using the chi-squared distribution
  p_value <- 1 - pchisq(test_statistic, df = 1)
  
  # Return results
  return(list(n_breaches = n_breaches, p_value = p_value))
}

## Apply Christoffersen's Test to the Dollar Portfolio

dollar_backtest_results <- list(
  MA = christoffersen_test(aligned_returns$DOL_return, final_var_forecasts$DOL_MA_VaR),
  EWMA = christoffersen_test(aligned_returns$DOL_return, final_var_forecasts$DOL_EWMA_VaR),
  GARCH = christoffersen_test(aligned_returns$DOL_return, final_var_forecasts$DOL_GARCH_VaR),
  Historical = christoffersen_test(aligned_returns$DOL_return, final_var_forecasts$DOL_Historical_VaR)
)

# Print backtest results for the dollar portfolio
print("Dollar Portfolio Christoffersen Test Results:")
print(dollar_backtest_results)

## Apply Christoffersen's Test to the Emerging Market Currency

em_backtest_results <- list(
  MA = christoffersen_test(aligned_returns$EM_return, final_var_forecasts$EM_MA_VaR),
  EWMA = christoffersen_test(aligned_returns$EM_return, final_var_forecasts$EM_EWMA_VaR),
  GARCH = christoffersen_test(aligned_returns$EM_return, final_var_forecasts$EM_GARCH_VaR),
  Historical = christoffersen_test(aligned_returns$EM_return, final_var_forecasts$EM_Historical_VaR)
)

# Print backtest results for the emerging market currency
print("Emerging Market Currency Christoffersen Test Results:")
print(em_backtest_results)

# Create a summary table for backtest results

backtest_summary <- data.frame(
  Model = c("MA", "EWMA", "GARCH", "Historical"),
  Dollar_Christoffersen_p_value = sapply(dollar_backtest_results, function(x) x$p_value),
  EM_Christoffersen_p_value = sapply(em_backtest_results, function(x) x$p_value)
)

# Print the summary table
print(backtest_summary)
View(backtest_summary)
  