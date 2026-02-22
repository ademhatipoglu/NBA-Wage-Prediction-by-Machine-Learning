# Predictive Sports Analytics: NBA Player Salary Estimation

## Executive Summary
This repository contains an end-to-end Machine Learning pipeline designed to predict the fair-market value (salaries) of NBA players based on their per-minute performance statistics. The project demonstrates advanced data engineering, web scraping, and the application of predictive regression models to evaluate player market values.

## Methodology & Pipeline

### 1. Data Engineering & Web Scraping
* **Automated Data Extraction:** Developed web scrapers using the `rvest` package to systematically extract real-time player statistics from *Basketball-Reference* and salary data from *Hoopshype*.
* **Data Preprocessing:** Performed robust data cleaning, handled duplicated entries, and merged disjoint datasets using `dplyr` to create a consolidated feature space for modeling.

### 2. Machine Learning Modeling
Instead of relying on a single algorithm, this project evaluates multiple advanced machine learning architectures to capture non-linear relationships in sports analytics:
* **Algorithms Implemented:** Random Forest (`randomForest`), Extreme Gradient Boosting (`xgboost`), Generalized Boosted Regression Models (`gbm`), and Support Vector Machines (`e1071`).
* **Model Selection:** Conducted rigorous comparative analysis across different models to determine the optimal algorithm for salary prediction.

### 3. Model Evaluation & Performance
* Evaluated models using quantitative metrics including Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and R-squared.
* The optimal model configurations successfully explained approximately **76% of the variance (R-squared ≈ 0.76)** in NBA player salaries, demonstrating strong predictive power.

## Technical Stack
* **Language:** R
* **Data Manipulation & Scraping:** `rvest`, `dplyr`
* **Machine Learning:** `randomForest`, `xgboost`, `gbm`, `e1071`, `rpart`
* **Visualization:** `ggplot2`
