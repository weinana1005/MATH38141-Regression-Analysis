# MATH38141 Coursework - Regression Analysis

## Overview
This project is a statistical analysis coursework for the **MATH38141** module. It involves applying **linear regression models** to analyze data related to **box office performance and financial indicators**. The analysis is implemented in **R** and covers multiple regression techniques, confidence interval estimation, and model comparisons.

## Files in This Repository

### 1. `38141coursework_Q1.R`
- Implements multiple linear regression to study the relationship between **Box Office revenue** and three explanatory variables:
  - **Production cost**
  - **Promotion budget**
  - **Books sold**
- Includes scatter plots, model fitting, residual analysis, and significance testing.
- Compares a full model (all variables) with a reduced model (excluding Books).
- Computes **R² statistic** and **ANOVA tables** to assess model fit.
- Tests statistical significance of Books' contribution to the regression model.

### 2. `38141coursework_Q2.R`
- Conducts multiple and simple linear regression on another dataset with different financial indicators.
- Compares a full model (with all explanatory variables) against a **simplified model** using ANOVA.
- Estimates confidence intervals for regression coefficients.
- Predicts outcomes for new data points and constructs confidence intervals for the differences between predictions.
- Examines the impact of excluding certain explanatory variables from the model.

### 3. `MATH38141_coursework.pdf`
- Provides detailed documentation of the statistical methods, formulas, and interpretations of results.
- Contains step-by-step derivations of the regression estimates, residual analysis, and hypothesis testing.
- Summarizes key findings and justifies model selection decisions.

## How to Run
1. **Install R and Required Packages**:
   Ensure you have **R installed** and necessary statistical libraries loaded.
2. **Execute the Scripts**:
   - Open **RStudio** or run `source("38141coursework_Q1.R")` and `source("38141coursework_Q2.R")`.
   - The scripts will generate **regression outputs, plots, and ANOVA tables**.
3. **Interpret the Results**:
   - Refer to **MATH38141_coursework.pdf** for theoretical explanations and conclusions.

## Summary of Findings
- **Production and Promotion** have a strong positive impact on **Box Office revenue**.
- **Books sales** show an insignificant effect when controlling for other variables.
- The reduced model (without Books) performs similarly to the full model.
- For the second dataset, certain financial indicators significantly influence the dependent variable, while others do not.

