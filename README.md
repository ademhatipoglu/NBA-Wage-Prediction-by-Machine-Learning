# NBA Player Wage Prediction

This repository contains code for predicting the wages of NBA players based on their performance statistics. The prediction is done using machine learning techniques, specifically the Random Forest algorithm.

## Data Sources
The data used for training and testing the model is scraped from two websites:
- [Basketball-Reference](https://www.basketball-reference.com/): The player statistics data is scraped from the NBA 2022 season per minute stats page and the NBA 2023 season per minute stats page.
- [Hoopshype](https://hoopshype.com/): The player wage data is scraped from the NBA player salaries page.

## Code Files
- `data_scraper.R`: This script scrapes the player statistics and wage data from the respective websites using the `rvest` package.
- `data_preparation.R`: This script prepares the scraped data for model training by cleaning and merging the datasets.
- `data_analysis.R`: This script performs exploratory data analysis on the prepared data and visualizes various relationships and distributions.
- `model_training.R`: This script trains the Random Forest model using the prepared data and makes predictions on the test data.
- `evaluation.R`: This script evaluates the performance of the trained model by calculating metrics such as MSE, RMSE, and R-squared.

## Dependencies
The code is written in R and requires the following R packages to be installed:
- `rvest`: For web scraping.
- `dplyr`: For data manipulation and transformation.
- `randomForest`: For building the Random Forest model.
- `ggplot2`: For data visualization.

## Usage
1. Clone the repository to your local machine.
2. Install the required R packages mentioned in the dependencies section.
3. Run the scripts in the following order:
   - `data_scraper.R`: This will scrape the player statistics and wage data.
   - `data_preparation.R`: This will clean and merge the scraped data.
   - `data_analysis.R`: This will perform exploratory data analysis and generate visualizations.
   - `model_training.R`: This will train the Random Forest model and make predictions.
   - `evaluation.R`: This will evaluate the model's performance.
4. Modify the code as per your requirements, such as changing the data sources or adding additional features.
5. Feel free to explore the code and adapt it for your own projects.

## Results
The trained Random Forest model predicts the wages of NBA players based on their performance statistics. The model's performance can be evaluated using metrics such as Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and R-squared. The evaluation results are printed in the console.

## License
This project is licensed under the MIT License. Feel free to use the code for educational and non-commercial purposes.

## Acknowledgments
- The code in this repository is based on the tutorials and examples provided by the `rvest`, `dplyr`, `randomForest`, and `ggplot2` package documentation.
- The data used in this project is scraped from Basketball-Reference and Hoopshype, which are valuable resources for NBA statistics and player information.
