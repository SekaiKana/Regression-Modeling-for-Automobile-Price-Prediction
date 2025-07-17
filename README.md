# Regression-Modeling-for-Automobile-Price-Prediction
Statistical analysis and regression modeling for automobile price prediction


# 🚗 Automobile Price Prediction Analysis

A comprehensive statistical analysis using multiple linear regression to predict car prices based on vehicle characteristics.

## 📊 Project Overview

This project develops an optimal regression model to predict automobile prices using key vehicle features. Through systematic variable selection and statistical validation, the analysis identifies the most influential factors affecting car pricing.

## 🎯 Key Results

- **Model Accuracy**: 84.48% variance explained (Adjusted R² = 0.8448)
- **Key Price Drivers**: Horsepower, curb weight, drivetrain type, luxury features, hybrid technology
- **Final Model**: `log(price) = 0.0002207×hp + 0.0000157×weight + 0.01276×rearwd + 0.02908×luxury + 0.009898×hybrid`

## 📁 Repository Contents

- `report.pdf` - Complete statistical analysis report
- `analysis.R` - R code for data analysis and modeling
- `README.md` - Project overview (this file)

## 🔧 Methodology

1. **Data Preprocessing**: Log transformation of price variable, outlier removal
2. **Exploratory Analysis**: Correlation analysis and variable relationships
3. **Model Development**: Systematic variable selection across 3 models
4. **Validation**: Residual analysis, Q-Q plots, and statistical diagnostics

## 📈 Business Impact

The model provides reliable price predictions for automotive industry applications including:
- Product development pricing strategies
- Market positioning analysis
- Competitive pricing benchmarks

## 📝 Code Attribution

Statistical analysis and methodology are original coursework. R code reconstructed with assistance from Claude AI based on the methods documented in the original report.

---
*Analysis completed for Stats for Business & Economics - 2025*
