# Estimating latent factors in dynamic models

### The Kalman filter under misspecification

Francesco Zucca and Michael Fehl, May 2026

## Overview

This project empirically studies the robustness of the Kalman filter as an estimator for dynamic factor models, using the Stock & Watson (1988) framework as the baseline specification. We examine two distinct forms of misspecification:

1.  **Distributional**: non-Gaussian shocks (heavy-tailed or skewed)
2.  **Structural misspecification**: number of latent factors is incorrectly specified

## Structure

| Chapter | Description |
|------------------------------|------------------------------------------|
| **Chapter 1** | Replication of the Stock & Watson (1988) single-factor model on recent FRED data |
| **Chapter 2** | Monte Carlo analysis of factor recovery under Gaussian, Student-*t* (ν = 5), and Skew-*t* (α = 5) shocks |
| **Chapter 3** | Extension to an 8-indicator dataset with a second latent factor: comparison of misspecified vs. correctly specified model |

------------------------------------------------------------------------

## Key Findings

-   The Kalman filter is **consistent** under non-Gaussian shocks (QMLE), but loses efficiency in finite samples as distributions become heavier-tailed or skewed
-   A single-factor model on heterogeneous data recovers only the **dominant signal**, discarding the rest as noise
-   The correctly specified two-factor model recovers both GDP and inflation

## Data

All macroeconomic series are sourced from [FRED](https://fred.stlouisfed.org/): `IPMAN`, `CMRMTSPL`, `PAYEMS`, `W875RX1`, `CPIAUCSL`, `CPILFESL`, `PCEPI`, `PPIACO`, `GDPC1`