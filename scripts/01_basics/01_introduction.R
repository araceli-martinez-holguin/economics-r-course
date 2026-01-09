# ==============================================================================
# Introduction to R for Economics
# Lesson 1: Basic Operations and Economic Calculations
# ==============================================================================

# Clear workspace
rm(list = ls())

# 1. BASIC ARITHMETIC FOR ECONOMICS ------------------------------------------
cat("=== 1. BASIC ECONOMIC CALCULATIONS ===\n\n")

# GDP Growth Rate
initial_gdp <- 1000  # in billions
final_gdp <- 1100
growth_rate <- ((final_gdp - initial_gdp) / initial_gdp) * 100
cat(sprintf("GDP Growth Rate: %.2f%%\n", growth_rate))

# Compound Annual Growth Rate (CAGR)
years <- 5
cagr <- ((final_gdp / initial_gdp)^(1/years) - 1) * 100
cat(sprintf("CAGR over %d years: %.2f%%\n", years, cagr))

# Inflation Calculation
cpi_initial <- 100
cpi_final <- 105
inflation <- ((cpi_final - cpi_initial) / cpi_initial) * 100
cat(sprintf("Inflation Rate: %.2f%%\n", inflation))

# 2. WORKING WITH VECTORS ----------------------------------------------------
cat("\n=== 2. WORKING WITH ECONOMIC DATA VECTORS ===\n\n")

# Create economic time series
years <- 2010:2020
gdp <- c(1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1500)
inflation <- c(2.1, 2.5, 2.3, 2.0, 1.8, 1.5, 1.2, 1.0, 1.5, 2.0, 2.5)

# Create data frame
economic_data <- data.frame(
  year = years,
  gdp = gdp,
  inflation = inflation
)

cat("Economic Data (2010-2020):\n")
print(economic_data)
