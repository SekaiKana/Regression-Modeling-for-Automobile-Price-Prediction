# Automobile Price Prediction - Statistical Analysis
# Original analysis and report by Sekai Kanamori
# R code reconstructed with assistance from Claude AI based on methodology 
# described in the original report
# Date: July 18th, 2025

# Load required libraries
library(ggplot2)
library(GGally)
library(car)
library(dplyr)

# Load data (replace with your actual data file)
# auto_data <- read.csv("automobile_data.csv")

# Data preprocessing
# Remove extreme outliers (as mentioned in report)
# auto_data_clean <- auto_data[auto_data$price < threshold, ]

# Log transformation of price (as shown in report)
# auto_data_clean$log_price <- log(auto_data_clean$price)

# ====== EXPLORATORY DATA ANALYSIS ======

# Figure 1.1: Histogram of price distribution
hist(auto_data$price, 
     main = "Histogram of Price Distribution",
     xlab = "Price ($)",
     breaks = 20,
     col = "lightblue")

# Figure 1.2: Histogram of log-transformed price
hist(log(auto_data$price), 
     main = "Histogram of Log-Transformed Price Distribution",
     xlab = "Log(Price)",
     breaks = 20,
     col = "lightgreen")

# Figure 2.1: Scatterplot matrix
# Select key variables for scatterplot matrix
key_vars <- c("log_price", "horsepower", "curb_weight", "length", 
              "highway_mpg", "city_mpg", "engine_size")

pairs(auto_data_clean[key_vars], 
      main = "Scatterplot Matrix of Key Variables")

# Alternative with GGally for better visualization
# ggpairs(auto_data_clean[key_vars])

# ====== MODEL DEVELOPMENT ======

# Model 1: Basic size-related variables
model1 <- lm(log_price ~ horsepower + curb_weight, data = auto_data_clean)
summary(model1)

# Model 2: Recommended model from report
model2 <- lm(log_price ~ horsepower + curb_weight + rear_wheel_drive + 
             luxury + hybrid, data = auto_data_clean)
summary(model2)

# Extract the final model equation (from report)
# log(price) = 0.0002207*hp + 0.0000157*weight + 0.01276*rearwd + 
#              0.02908*luxury + 0.009898*hybrid

# Model 3: Alternative model with MPG and drivetrain
model3 <- lm(log_price ~ horsepower + curb_weight + highway_mpg + 
             drivetrain_7over, data = auto_data_clean)
summary(model3)

# ====== MODEL DIAGNOSTICS ======

# Figure 1: Q-Q Plot of standardized residuals
qqnorm(rstandard(model2), main = "Normal Q-Q Plot of Standardized Residuals")
qqline(rstandard(model2), col = "red")

# Figure 2: Residuals vs Fitted Values
plot(fitted(model2), residuals(model2),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 1)
abline(h = 0, col = "red", lty = 2)

# Figure 3.1: Luxury variable analysis
plot(auto_data_clean$luxury_adjusted, log(auto_data_clean$price),
     main = "Adjusted Plot Diagram of Luxury and Log(Price)",
     xlab = "Luxury Adjusted for Weight + HP + Pickup + AWD + Rearwd",
     ylab = "Log(Price) Adjusted for Weight + HP + Pickup + AWD + Rearwd")

# Figure 3.2: Pickup vs Weight relationship
plot(auto_data_clean$pickup, auto_data_clean$curb_weight,
     main = "Plot Diagram of Pickup vs. Weight",
     xlab = "Pickup (Binary)",
     ylab = "Weight")

# Figure 3.3: Hybrid variable analysis
plot(auto_data_clean$hybrid_adjusted, log(auto_data_clean$price),
     main = "Adjusted Plot Diagram of Hybrid and Log(Price)",
     xlab = "Hybrid Adjusted for HP + Weight + AWD + Rearwd + Luxury",
     ylab = "Log(Price) Adjusted for HP + Weight + AWD + Rearwd + Luxury")

# Figure 3.4: Q-Q plot for Model 2
qqnorm(rstandard(model2), main = "A Q-Q Plot of Model 2")
qqline(rstandard(model2), col = "red")

# ====== MODEL COMPARISON ======

# Calculate R-squared values
r2_model1 <- summary(model1)$r.squared
r2_model2 <- summary(model2)$r.squared
r2_model3 <- summary(model3)$r.squared

cat("Model Comparison:\n")
cat("Model 1 R²:", round(r2_model1, 4), "\n")
cat("Model 2 R²:", round(r2_model2, 4), "\n")  # Should be ~0.8448
cat("Model 3 R²:", round(r2_model3, 4), "\n")

# Calculate Cp statistic for model selection
# Cp values from report: Model 1 = 120.71, Model 2 = 45.71, Model 3 = 84.59

# ====== OUTLIER ANALYSIS ======

# Identify outliers mentioned in report
# Porsche Boxster ($74,610), Audi A8 ($83,800), Dodge Challenger ($83,295)

# Create boxplot of residuals by fitted value segments
fitted_segments <- cut(fitted(model2), breaks = 5)
boxplot(residuals(model2) ~ fitted_segments,
        main = "Boxplot of Residuals vs. Fitted Values for Model 2",
        xlab = "Fitted Values",
        ylab = "Residuals")

# ====== FINAL MODEL SUMMARY ======

cat("\n=== FINAL RECOMMENDED MODEL ===\n")
cat("Model 2: log(price) = β₀ + β₁*horsepower + β₂*weight + β₃*rear_wheel_drive + β₄*luxury + β₅*hybrid\n")
cat("Adjusted R² = 0.8448\n")
cat("Explains 84.48% of variance in car prices\n")

# Print final model coefficients
print(summary(model2))
