### LIBRARIES

library("tidyverse")
library("readxl")
library("tidyquant")

### LOADING DATA

# IDs
ids <- c(
  "GDPC1",     # Real GDP
  "INDPRO",    # Industrial Production
  "CMRMTSPL",  # Manufacturing & Trade Sales
  "PAYEMS",    # Nonfarm Payrolls
  "W875RX1"    # Real Personal Income less Transfers 
)

# Pulling the data via FRED api
raw_data <- tq_get(ids, get = "economic.data", from = "1969-10-01", to = "2026-01-01")

# Pivoting data in wide format
data_monthly <- raw_data %>%
  pivot_wider(names_from = symbol, values_from = price) %>%
  arrange(date)

# Aligning the GDP measurement
df_aligned <- data_monthly %>%
  arrange(date) %>%
  mutate(GDPC1 = lag(GDPC1, 2))

# Applying log differences to obtain the final dataset
df_clean <- df_aligned %>%
  arrange(date) %>%
  mutate(
    # Log-difference for the 4 monthly variables (lag = 1)
    across(c(INDPRO, CMRMTSPL, PAYEMS, W875RX1), 
           ~ (log(.x) - log(lag(.x, 1))) * 100),
    
    # Log-difference for the quarterly GDP (lag = 3)
    GDPC1 = (log(GDPC1) - log(lag(GDPC1, 3))) * 100
  ) %>%
  # Dropping the very first row since month-over-month lag creates an NA
  drop_na(PAYEMS)

### DEFINING THE DATA ###

# gdp data
gdp <- df_clean %>%
  select(GDPC1) %>%
  drop_na(GDPC1) %>%
  as.matrix()

# Matrix of indicators
y_raw <- df_clean %>%
  select(- date, - GDPC1) %>%
  as.matrix()

### NORMALIZING DATA ###

# demeaning the y time series and standardizing it
y <- scale(y_raw)

TT <- nrow(y) # number of observations
n <- ncol(y) # number of indicators

# normalizing the variance of w to 1
varQ <- 1

### MATRIX FUNCTION ###

# Function that obtains matrices given the input parameter vector th
# Note that th is the vector of parameters which likelihood will be maximized by the maximum likelihood function
# the ML function
matrices <- function(th, n, varQ) {
  
  ### H: measurement matrix
  
  # Manually defined matrix h1 (used to extract the errors from the h vector)
  h1 <- rbind(
    c(1, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 1, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 1, 0)
  )
  # combine the h1 matrix to the first four coefficients (lamba 1 to 4)
  Hmat <- cbind(th[1:n], matrix(0, nrow = n, ncol = 1), h1)
  
  ### F: transition matrix
  
  # Manually creating rows of the transition matrix
  # phi1 and phi2
  f1  <- c(th[n + 1], th[n + 2], 0, 0, 0, 0, 0, 0, 0, 0)
  f2  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  # psi11 and psi12
  f3  <- c(0, 0, th[n + 3], th[n + 4], 0, 0, 0, 0, 0, 0)
  f4  <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
  # psi21 and psi22
  f5  <- c(0, 0, 0, 0, th[n + 5], th[n + 6], 0, 0, 0, 0)
  f6  <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
  # psi31 and psi 32
  f7  <- c(0, 0, 0, 0, 0, 0, th[n + 7], th[n + 8], 0, 0)
  f8  <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
  # psi41 and psi42
  f9  <- c(0, 0, 0, 0, 0, 0, 0, 0, th[n + 9], th[n + 10])
  f10 <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  
  # Construct the final F matrix
  Fmat <- rbind(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  
  ### R: variance-covariance matrix of the observation equation idiosyncratic error w
  
  # In our case it's just a matrix of 0s given that the error got incorporated into 
  # the signal vector
  Rmat <- matrix(0, nrow = n, ncol = n)
  
  # Q matrix (variance-covariance matrix of state shocks)
  # Note that the th values here represent the standard deviations of the idiosyncratic
  # errors (w is the first one, and then epsilon 1 to 4)
  th2_val <- kronecker(th[(n + 11):(n + 14)]^2, matrix(c(1, 0), ncol = 1))
  # We create the final diagonal matrix (since correlation between the errors is 0)
  Qmat <- diag(c(varQ, 0, as.vector(th2_val)))
  
  # Finally, put all of the matrices into a list, which will be later unpacked by the 
  # function to compute the likelihood
  return(list(Rmat = Rmat, Qmat = Qmat, 
              Hmat = Hmat, Fmat = Fmat))
}

### KALMAN FILTER FUNCTION ###

# Objective function for the Likelihood
kalman <- function(th, y, n, TT, varQ) {
  
  # Initialization (pull matrices, set initial values, initialize vectors)
  
  # Get matrices
  mats <- matrices(th, n, varQ)
  Hmat <- mats$Hmat
  Fmat <- mats$Fmat
  Rmat <- mats$Rmat
  Qmat <- mats$Qmat
  
  # Initial state (initial value of the signal): h_{0|0} = 0
  h00 <- matrix(0, nrow = 10, ncol = 1) 
  # Initial MSE (initial value of the variance): P_{0|0} = I
  Pmat00 <- diag(10)
  
  # Initializing the vector to store the likelihood
  like <- matrix(0, nrow = TT, ncol = 1)
  
  # Initializing the vector to store the signal
  filter_mat <- matrix(0, nrow = TT, ncol = 10)
  
  # Kalman filter loop
  
  # for loop for each observation
  for(it in 1:TT) {
    
    # Step 1: prediction of ht given info at t-1
    h10 <- Fmat %*% h00                     
    
    # Step 2: prediction of Pt given info at t-1
    Pmat10 <- Fmat %*% Pmat00 %*% t(Fmat) + Qmat
    
    # Step 3: prediction of yt given info at t-1
    y10 <- Hmat %*% h10
    
    # Step 4: estimation of the forecast error
    e10 <- matrix(y[it, ], ncol = 1) - y10
    
    # Step 5: estimation of the variance of the forecast error
    Fmat10 <- Hmat %*% Pmat10 %*% t(Hmat) + Rmat                  
    
    # Log-Likelihood Calculation (see section 2.3.4)
    like[it] <- -0.5 * (log(2 * pi * det(Fmat10)) + (t(e10) %*% solve(Fmat10) %*% e10))
    
    # Step 6: Kalman gain
    Kmat <<- Pmat10 %*% t(Hmat) %*% solve(Fmat10)
    
    # Step 7: updating of filter and variance
    # filter
    h11 <- h10 + Kmat %*% e10
    # variance
    Pmat11 <- Pmat10 - Kmat %*% Hmat %*% Pmat10
    
    # Iterating
    filter_mat[it, ] <- t(h11)
    h00 <- h11
    Pmat00 <- Pmat11
  }
  
  # Returning a list containing both the likelihood and the extracted matrices
  return(list(
    neg_loglik = -sum(like),
    filter_mat = filter_mat,
    Fmat = Fmat
  ))
}

### FUNCTION THAT RETURNS ONLY THE LIKELIHOOD ###

# optim() requires a function that returns only the scalar to minimize
ofn <- function(th, y, n, TT, varQ) {
  res <- kalman(th, y, n, TT, varQ)
  return(res$neg_loglik)
}

### COEFFICIENT ESTIMATION ###

# Initial guesses for parameters
B <- matrix(c(0.9, 0.8, 0.7, 0.6), ncol = 1) # Loadings
phif <- matrix(0.3, nrow = 2, ncol = 1)      # Factor AR(2) coefficients
phiy <- matrix(0.3, nrow = n * 2, ncol = 1)  # Error AR(2) coefficients
v <- matrix(apply(y, 2, sd), ncol = 1)       # Error variances

# Vector of initial parameters
startval <- rbind(B, phif, phiy, v)

# Maximizing the likelihood function
opt_res <- optim(par = as.vector(startval), 
                 fn = ofn,
                 y = y, n = n, TT = TT, varQ = varQ,
                 method = "BFGS", 
                 hessian = TRUE,
                 control = list(maxit = 2000))

# Computing standard errors
cramerrao <- solve(opt_res$hessian)
std_err <- sqrt(diag(cramerrao))

# Printing the optimal parameters and the standard errors
print(cbind(Estimate = opt_res$par, StdError = std_err))

### EXTRACTING THE FINAL FACTOR (Post-Estimation) ###

# Running the filter again with the optimal parameters
final_kf <- kalman(opt_res$par, y, n, TT, varQ)

# Extracting the filer matrix
filter_mat <- final_kf$filter_mat

# Extracting the factor
factor <- filter_mat[, 1]

# Extracting Fmat for the forecast steps
Fmat <- final_kf$Fmat 

### COVERTING FACTOR TO QUARTERLY GROWTH RATE ###

# Converting monthly factor into a quarterly equivalent
factorQ <- (1/3) * factor[5:length(factor)] + 
  (2/3) * factor[4:(length(factor) - 1)] + 
  factor[3:(length(factor) - 2)] + 
  (2/3) * factor[2:(length(factor) - 3)] + 
  (1/3) * factor[1:(length(factor) - 4)]

# Extracting every 3rd month to match quarterly GDP
i <- 1
factgdp <- c()
while(i < (TT - 4)) {
  factgdp <- c(factgdp, factorQ[i])
  i <- i + 3
}

### FORECAST ###

# Extracting the last row of the filter matrix (hT|T)
hTT <- filter_mat[nrow(filter_mat), ]

# Forecasting

# Estimate the future factor values (just 5 quarters into the future)
hT1T <- Fmat %*% hTT
hT2T <- Fmat %*% hT1T
hT3T <- Fmat %*% hT2T
hT4T <- Fmat %*% hT3T
hT5T <- Fmat %*% hT4T

# Append the 5 new forecasted factors (1st element of each state vector) 
# to the historical factor vector
factor2 <- c(factor, hT1T[1], hT2T[1], hT3T[1], hT4T[1], hT5T[1])

# Converting to quarterly growth rates
factor2Q <- (1/3) * factor2[5:length(factor2)] + 
  (2/3) * factor2[4:(length(factor2) - 1)] + 
  factor2[3:(length(factor2) - 2)] + 
  (2/3) * factor2[2:(length(factor2) - 3)] + 
  (1/3) * factor2[1:(length(factor2) - 4)]

# Downsampling to extract every 3rd month
length <- length(factor2Q)
i <- 1
factgdp2 <- c()

while(i <= length) {
  fac1 <- factor2Q[i]
  factgdp2 <- c(factgdp2, fac1)
  i <- i + 3
}

# Estimating the GDP

# Defining the data
y_gdp <- gdp

# Creating the X matrix with a column of 1s for the intercept
x <- cbind(1, factgdp)

# Computing the OLS coefficients
b <- solve(t(x) %*% x) %*% t(x) %*% y_gdp

# Generating the forecast, predicting the next two quarters
yhat1 <- b[1] + b[2] * factgdp2[length(factgdp2) - 1]
yhat2 <- b[1] + b[2] * factgdp2[length(factgdp2)]

# Output the results
print(paste("Forecast 1 (Next Quarter):", yhat1))
print(paste("Forecast 2 (Quarter After):", yhat2))

### IN-SAMPLE ESTIMATES OF GDP AND PERFORMANCE MEASURES ###

# Translating all historical factors into estimated GDP 
gdp_fitted <- x %*% b

# Calculating the residuals
residuals <- y_gdp - gdp_fitted

# Calculating Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Calculating R-squared
ss_tot <- sum((y_gdp - mean(y_gdp))^2)
ss_res <- sum(residuals^2)
r_squared <- 1 - (ss_res / ss_tot)

# Printing the model evaluation metrics
print(paste("R-squared:", round(r_squared, 4)))
print(paste("Mean Squared Error (MSE):", round(mse, 4)))