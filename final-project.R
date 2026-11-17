# *********************** DATA CLEANSING AND PREPARATION ***********************



# **** Data Import and Date Formatting ****
setwd("OneDrive/STAT438-final-project")

project_data <- read.csv("WM2NS.csv") # Data Import

project_data$observation_date <- as.Date(project_data$observation_date, format = "%Y-%m-%d")
# Date formatting

# **** Data Validation, Type Conversion, and Missing Value Removal ****

sum(is.na(project_data))
# Data Validation

str(project_data)

# Ensure that WM2NS is numeric and remove any rows that are missing. FRED uses "." for NAs 
project_data$WM2NS <- as.numeric(project_data$WM2NS)
project_data <- na.omit(project_data)

# Sort chronologically just in case 
project_data <- project_data[order(project_data$observation_date), ]

# **** Sample Summary and Time Series Object Construction ****

# Check final observation count and date range (needed for the EDA writeup)
nrow(project_data)
range(project_data$observation_date)

# Extract start year and week number for the ts() object
start_year <- as.numeric(format(project_data$observation_date[1], "%Y"))
start_week <- as.numeric(format(project_data$observation_date[1], "%U")) + 1

# Convert to a weekly time series object (frequency = 52)
wm2_ts <- ts(project_data$WM2NS, start = c(start_year, start_week), frequency = 52)

# The dataset "WM2NS" is apparently NOT seasonally adjusted and it spans several decades
# of manetary expansion, so I should expect to see the following:
# a. Exponential upward trend and increasing variance (where fluctuations get a lot wider
# as the dollar amount grows, especially after 2020)
# b. Annual seasonailty repeating every 52 weeks (tax season surges in April,
# holiday spending shifts, etc.)

# *********************** EXPLORATORY DATA ANALYSIS ***********************

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# **** Time Series Plot of Raw Data ****
plot(wm2_ts,
     main = "Weekly M2 Money Supply (Not Seasonally Adjusted)",
     ylab = "Billions of Dollars",
     xlab = "Year")

# Correlogram is the same as "Auto-Correlation Function"

# Correlogram (ACS) set lag.max to 156 (3 years worth the number of weeks) to see
# seasonal spikes at 52, 104, 156
acf(wm2_ts, lag.max = 156, main = "ACF of Raw WM2NS")

# Partial Correlogram (PACF)

pacf(wm2_ts, lag.max = 156, main = "PACF of Raw WM2NS")

par(mfrow = c(1, 1))

# *********************** DATA TRANSFORMATION AND STATIONARY VERIFICATION ***********************

# **** Stabilize Variance via Natural Log ****
log_wm2 <- log(wm2_ts)
plot(log_wm2, main = "Log-Transformed WM2NS", ylab = "log(Billions of Dollars)")

# **** Remove 52-week Annual Seasonality ****
seas_diff_wm2 <- diff(log_wm2, lag = 52, differences = 1)

# **** Remove Remaining Trend with a First Regular Difference ****
stat_wm2 <- diff(seas_diff_wm2, lag = 1, differences = 1)

# **** Plot Processed Series, ACF, and PACF to Verify Stationarity ****
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

plot(stat_wm2, 
     main = "Processed WM2NS (Log + Seasonal Diff [52] + First Diff [1])",
     ylab = "Differenced Log M2", 
     xlab = "Year")

acf(stat_wm2, lag.max = 156, main = "ACF of Processed WM2NS")
pacf(stat_wm2, lag.max = 156, main = "PACF of Processed WM2NS")

par(mfrow = c(1, 1))


