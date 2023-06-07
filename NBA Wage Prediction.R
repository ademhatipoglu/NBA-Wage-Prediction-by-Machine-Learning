library(rvest)
library(dplyr)
library(randomForest)
library(rpart)
library(e1071)
library(gbm)
library(xgboost)
library(ggplot2)


# Specify the URL of the webpage to scrape
url <- "https://www.basketball-reference.com/leagues/NBA_2022_per_minute.html"

# Scrape the data from the webpage
page <- read_html(url)

# Extract the table containing the player statistics
table <- html_table(html_nodes(page, "table")[1])

# Convert the table to a data frame
player_stats_train <- as.data.frame(table)

# Print the resulting data frame
print(player_stats_train)

player_stats_train <- player_stats_train %>%
  filter(!is.na(as.numeric(Rk)))

player_stats_train <- player_stats_train %>%
  group_by(Player) %>%
  filter(!(Player %in% Player[duplicated(Player)]) | Tm == "TOT") %>%
  ungroup()


# Specify the URL of the webpage to scrape
url <- "https://hoopshype.com/salaries/players/2021-2022/"

# Scrape the data from the webpage
page <- read_html(url)

# Extract the table containing the wage statistics
table <- html_table(html_nodes(page, "table")[1])

# Convert the table to a data frame
wage_stats_train <- as.data.frame(table)

# Print the resulting data frame
print(wage_stats_train)

col_names <- wage_stats_train[1, ]
wage_stats_train <- wage_stats_train[-1, ]
colnames(wage_stats_train) <- col_names

# Display the first few rows of the modified wage_stats table
head(wage_stats_train)

wage_stats_train <- wage_stats_train[, c("Player", "2021/22")]

wage_stats_train$`2022/23` <- as.numeric(gsub("[$,]", "", wage_stats_train$`2021/22`))

# Merging data and wage_stats based on player names
train_data <- merge(player_stats_train, wage_stats_train, by.x = "Player", by.y = "Player", all.x = TRUE)


# Viewing the first few rows of the merged data
head(train_data)

# Renaming the variable from "2022/23" to "Wage"
train_data <- train_data %>%
  rename(Wage = `2022/23`)

# Viewing the first few rows of the updated merged data
head(train_data)

# Removing rows with NA in the "Wage" variable
train_data <- na.omit(train_data)

# Viewing the first few rows of the updated merged data
head(train_data)

# Select the desired columns from the original data
train_data <- train_data[c("Player", "Pos", "Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "Wage")]

# Set the desired columns to numeric
numeric_cols <- c("Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "Wage")
train_data[numeric_cols] <- lapply(train_data[numeric_cols], as.numeric)

# Load the required libraries
library(rvest)
library(dplyr)

# Scrape data from "https://www.basketball-reference.com/leagues/NBA_2023_per_minute.html"
url1 <- "https://www.basketball-reference.com/leagues/NBA_2023_per_minute.html"

# Scrape the data from the webpage
page1 <- read_html(url1)

# Extract the table containing the player statistics
table1 <- html_table(html_nodes(page1, "table")[1])

# Convert the table to a data frame
player_stats_test <- as.data.frame(table1)

# Remove rows with missing rank (Rk) values
player_stats_test <- player_stats_test %>%
  filter(!is.na(as.numeric(Rk)))

# Group by player and filter duplicates based on player names and team (Tm) values
player_stats_test <- player_stats_test %>%
  group_by(Player) %>%
  filter(!(Player %in% Player[duplicated(Player)]) | Tm == "TOT") %>%
  ungroup()

# Remove rows with missing values
train_data <- na.omit(train_data)


# Scrape data from "https://hoopshype.com/salaries/players/"
url2 <- "https://hoopshype.com/salaries/players/"

# Scrape the data from the webpage
page2 <- read_html(url2)

# Extract the table containing the wage statistics
table2 <- html_table(html_nodes(page2, "table")[1])

# Convert the table to a data frame
wage_stats_test <- as.data.frame(table2)

# Modify the column names of the wage statistics table
col_names <- wage_stats_test[1, ]
wage_stats_test <- wage_stats_test[-1, ]
colnames(wage_stats_test) <- col_names

# Select the desired columns from the wage statistics table
wage_stats_test <- wage_stats_test[, c("Player", "2022/23")]

# Convert the "2022/23" column to numeric by removing commas and dollar signs
wage_stats_test$`2022/23` <- as.numeric(gsub("[$,]", "", wage_stats_test$`2022/23`))

# Merge player statistics and wage statistics based on player names
test_data <- merge(player_stats_test, wage_stats_test, by.x = "Player", by.y = "Player", all.x = TRUE)

# Rename the "2022/23" column to "Wage"
test_data <- test_data %>%
  rename(Wage = `2022/23`)

# Remove rows with missing wage values
test_data <- na.omit(test_data)

# Select the desired columns from the test data
test_data <- test_data[c("Player", "Pos", "Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "Wage")]

# Set the desired columns to numeric
numeric_cols <- c("Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "Wage")
test_data[numeric_cols] <- lapply(test_data[numeric_cols], as.numeric)

# Remove rows with missing values
test_data <- na.omit(test_data)


# Display the first few rows of the test data
head(test_data)

library(ggplot2)

# Define color palette for different positions
pos_colors <- c("C" = "#FF0000", "PF" = "#00FF00", "SF" = "#0000FF", "SG" = "#FFFF00", "PG" = "#FF00FF")

# Create a box plot chart for wages grouped by position with different colors
ggplot(test_data, aes(x = Pos, y = Wage, fill = Pos)) +
  geom_boxplot() +
  scale_fill_manual(values = pos_colors) +
  labs(x = "Position", y = "Wage") +
  ggtitle("Box Plot of Wages by Position")

library(ggplot2)

ggplot(test_data, aes(x = Age)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(x = "Player Age", y = "Frequency") +
  ggtitle("Histogram of Player Age")

library(ggplot2)

# Create the bar chart
ggplot(test_data, aes(x = Pos)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Position", y = "Number of Players") +
  ggtitle("Distribution of Players by Position")

library(ggplot2)

# Create the scatter plot
ggplot(test_data, aes(x = Age, y = G)) +
  geom_point(color = "steelblue") +
  labs(x = "Age", y = "Number of Games Played") +
  ggtitle("Relationship between Age and Games Played")

library(ggplot2)

# Create the bar chart
ggplot(test_data, aes(x = Pos, y = PTS)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(x = "Position", y = "Average Points Scored") +
  ggtitle("Average Points Scored by Position")

library(ggplot2)

# Create the box plot
ggplot(test_data, aes(x = Pos, y = Age, fill = Pos)) +
  geom_boxplot() +
  labs(x = "Position", y = "Age") +
  ggtitle("Distribution of Player Ages by Position")

library(ggplot2)

# Create the line chart
ggplot(test_data, aes(x = Age, y = FG., group = 1)) +
  geom_line(color = "steelblue") +
  labs(x = "Age", y = "Field Goal Percentage") +
  ggtitle("Field Goal Percentage by Age")


library(ggplot2)

# Calculate the correlation matrix
cor_matrix <- cor(test_data[, -c(1, 2)])  # Exclude the first and second columns

# Create a data frame of the correlation matrix with row and column names
corr_df <- as.data.frame(as.table(cor_matrix))
names(corr_df) <- c("Variable 1", "Variable 2", "Correlation")

# Create the correlation heatmap
ggplot(corr_df, aes(x = `Variable 2`, y = `Variable 1`, fill = `Correlation`)) +
  geom_tile() +
  geom_text(aes(label = round(`Correlation`, 2)), color = "white", size = 1.6) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey50") +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8), plot.title = element_text(hjust = 0.5)) +
  coord_equal()

library(ggplot2)

# Sort the test_data dataframe by descending wage to get the top 10 highest-wage players
top_10_players <- test_data[order(-test_data$Wage), ][1:10, ]

# Create a ggplot chart with customized aesthetics
chart <- ggplot(top_10_players, aes(x = Wage, y = reorder(Player, Wage), fill = Wage)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Wage), hjust = -0.3, color = "black", size = 4, position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = Player), color = "black", hjust = 0, size = 4, position = position_stack(vjust = 0.0)) +
  labs(x = "Wage", y = "Player", title = "Top 10 Highest-Paid NBA Players") +
  scale_fill_gradient(low = "#1f77b4", high = "#ff7f0e") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5, color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# Print the chart
print(chart)

library(randomForest)

# Select the variables for training
selected_variables <- c("Age", "G", "GS", "MP", "PTS", "TRB", "AST", "STL", "BLK", "TOV")
train_predictors <- train_data[, selected_variables]
train_target <- train_data$Wage

# Train the random forest model
rf_model <- randomForest(train_predictors, train_target)

# Make predictions on the test data
test_predictors <- test_data[, selected_variables]
test_pred <- predict(rf_model, newdata = test_predictors)

# Calculate MSE, RMSE, and R-squared
mse <- mean((test_data$Wage - test_pred)^2)
rmse <- sqrt(mse)
rsq <- cor(test_data$Wage, test_pred)^2

# Print the results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq, "\n")


library(randomForest)

# Select the variables for training
selected_variables <- c("Age", "G", "GS", "MP", "PTS", "TRB", "AST", "STL", "BLK", "TOV")
train_predictors <- train_data[, selected_variables]
train_target <- train_data$Wage

# Train the random forest model
rf_model_1 <- randomForest(train_predictors, train_target)

# Make predictions on the test data
test_predictors <- test_data[, selected_variables]
test_pred_1 <- predict(rf_model_1, newdata = test_predictors)

# Calculate MSE, RMSE, and R-squared
mse_1 <- mean((test_data$Wage - test_pred_1)^2)
rmse_1 <- sqrt(mse_1)
rsq_1 <- cor(test_data$Wage, test_pred_1)^2

# Print the results
cat("MSE (Model 1):", mse_1, "\n")
cat("RMSE (Model 1):", rmse_1, "\n")
cat("R-squared (Model 1):", rsq_1, "\n")


library(randomForest)

# Preprocess the training data
encoded_train_data <- train_data

# Perform one-hot encoding for the "Pos" variable
encoded_pos <- model.matrix(~ Pos - 1, data = encoded_train_data)  # One-hot encoding
encoded_train_data <- cbind(encoded_train_data, encoded_pos)  # Append the encoded variables

# Separate the predictors and target variable for training data
train_predictors <- subset(encoded_train_data, select = -c(Wage, Player))  # Remove the "Wage" and "Player" columns
train_target <- encoded_train_data$Wage

# Train the random forest model
rf_model_2 <- randomForest(train_predictors, train_target)

# Preprocess the test data
encoded_test_data <- test_data

# Perform one-hot encoding for the "Pos" variable in test data
encoded_test_pos <- model.matrix(~ Pos - 1, data = encoded_test_data)
encoded_test_data <- cbind(encoded_test_data, encoded_test_pos)

# Ensure test data has the same variables as training data
missing_vars <- setdiff(names(train_predictors), names(encoded_test_data))
encoded_test_data[, missing_vars] <- 0  # Add missing variables as zeros

# Make predictions on the test data
test_pred_2 <- predict(rf_model_2, newdata = encoded_test_data)

# Calculate MSE, RMSE, and R-squared
mse_2 <- mean((test_data$Wage - test_pred_2)^2)
rmse_2 <- sqrt(mse_2)
rsq_2 <- cor(test_data$Wage, test_pred_2)^2

# Print the results
cat("MSE (Model 2):", mse_2, "\n")
cat("RMSE (Model 2):", rmse_2, "\n")
cat("R-squared (Model 2):", rsq_2, "\n")


library(randomForest)

# Select the variables for training
selected_variables <- c("Age", "G", "GS", "MP", "PTS", "TRB", "AST", "STL", "BLK", "TOV")
train_predictors <- train_data[, selected_variables]
train_target <- train_data$Wage

# Train the random forest model
rf_model_3 <- randomForest(train_predictors, train_target)

# Make predictions on the test data
test_predictors <- test_data[, selected_variables]
test_pred_3 <- predict(rf_model_3, newdata = test_predictors)

# Calculate MSE, RMSE, and R-squared
mse_3 <- mean((test_data$Wage - test_pred_3)^2)
rmse_3 <- sqrt(mse_3)
rsq_3 <- cor(test_data$Wage, test_pred_3)^2

# Print the results
cat("MSE (Model 3):", mse_3, "\n")
cat("RMSE (Model 3):", rmse_3, "\n")
cat("R-squared (Model 3):", rsq_3, "\n")

library(xgboost)

# Define the predictor variables for Model 4 (XGBoost)
predictor_vars_4 <- c("Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

# Train Model 4 (XGBoost)
model_4 <- xgboost(data = as.matrix(train_data[, predictor_vars_4]), label = train_data$Wage, nrounds = 100)

# Make predictions on the test data for Model 4 (XGBoost)
test_pred_4 <- predict(model_4, newdata = as.matrix(test_data[, predictor_vars_4]))

# Calculate RMSE, MSE, and R-squared for Model 4 (XGBoost)
rmse_4 <- sqrt(mean((test_data$Wage - test_pred_4)^2))
mse_4 <- mean((test_data$Wage - test_pred_4)^2)
rsquared_4 <- cor(test_data$Wage, test_pred_4)^2

# Print the evaluation metrics for Model 4 (XGBoost)
cat("Model 4 (XGBoost) Metrics (Test Data)\n")
cat("RMSE:", rmse_4, "\n")
cat("MSE:", mse_4, "\n")
cat("R-squared:", rsquared_4, "\n")

# Feature importance for Model 4 (XGBoost)
importance_4 <- xgb.importance(feature_names = predictor_vars_4, model = model_4)
xgb.plot.importance(importance_matrix = importance_4)

# Define the predictor variables for Model 5 (XGBoost)
predictor_vars_5 <- c("Age", "G", "GS", "MP", "PTS", "AST", "STL", "BLK", "TOV")

# Train Model 5 (XGBoost)
model_5 <- xgboost(data = as.matrix(train_data[, predictor_vars_5]), label = train_data$Wage, nrounds = 100)

# Make predictions on the test data for Model 5 (XGBoost)
test_pred_5 <- predict(model_5, newdata = as.matrix(test_data[, predictor_vars_5]))

# Calculate RMSE, MSE, and R-squared for Model 5 (XGBoost)
rmse_5 <- sqrt(mean((test_data$Wage - test_pred_5)^2))
mse_5 <- mean((test_data$Wage - test_pred_5)^2)
rsquared_5 <- cor(test_data$Wage, test_pred_5)^2

# Print the evaluation metrics for Model 5 (XGBoost)
cat("Model 5 (XGBoost) Metrics (Test Data)\n")
cat("RMSE:", rmse_5, "\n")
cat("MSE:", mse_5, "\n")
cat("R-squared:", rsquared_5, "\n")

library(gbm)

# Select the predictor variables
predictor_vars <- c("Age", "G", "MP", "PTS", "AST", "STL", "BLK", "TOV")

# Train data with the "Wage" column
train_X <- train_data[, c(predictor_vars, "Wage")]
train_Y <- train_data$Wage

# Test data without the "Wage" column
test_X <- test_data[, predictor_vars]
test_Y <- test_data$Wage

# Train the GBM model
gbm_model <- gbm(Wage ~ ., data = train_X, n.trees = 100, interaction.depth = 3)

# Make predictions on the test data
predictions_6 <- predict(gbm_model, newdata = test_X, n.trees = 100)

# Calculate evaluation metrics
mse <- mean((test_Y - predictions_6)^2)
rmse <- sqrt(mse)
rsq <- cor(test_Y, predictions_6)^2

# Print the evaluation metrics
cat("Model 6 (GBM) Metrics (Test Data)\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq, "\n")

library(gbm)

# Select the predictor variables
predictor_vars <- c("Age", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

# Train data with the "Wage" column
train_X <- train_data[, c(predictor_vars, "Wage")]
train_Y <- train_data$Wage

# Test data without the "Wage" column
test_X <- test_data[, predictor_vars]
test_Y <- test_data$Wage

# Train the GBM model
gbm_model <- gbm(Wage ~ ., data = train_X, n.trees = 100, interaction.depth = 3)

# Make predictions on the test data
predictions_7 <- predict(gbm_model, newdata = test_X, n.trees = 100)

# Calculate evaluation metrics
mse <- mean((test_Y - predictions_7)^2)
rmse <- sqrt(mse)
rsq <- cor(test_Y, predictions_7)^2

# Print the evaluation metrics
cat("Model 7 (GBM) Metrics (Test Data)\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq, "\n")

library(gbm)

# Define the variable sets for each position
variable_sets <- list(
  C = c("Age", "G", "GS", "MP", "PTS", "AST", "STL", "BLK", "TOV"),
  PF = c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV"),
  `PF-SF` = c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV"),
  PG = c("Age", "G", "MP", "PTS", "AST", "STL", "BLK", "TOV"),
  `PG-SG` = c("Age", "G", "MP", "PTS", "AST", "STL", "BLK", "TOV"),
  SF = c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "AST", "STL", "BLK", "TOV"),
  `SF-SG` = c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "AST", "STL", "BLK", "TOV"),
  SG = c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.", "AST", "STL", "BLK", "TOV")
)

# Create an empty list to store the models
models <- list()

# Create empty lists to store evaluation metrics
pos_rmse <- list()
pos_mse <- list()
pos_rsq <- list()

# Train models for each position
for (pos in names(variable_sets)) {
  # Check if the target variable is present in the data
  if ("Wage" %in% colnames(train_data)) {
    # Create the formula for the current position
    formula <- as.formula(paste("Wage ~", paste(variable_sets[[pos]], collapse = "+")))
    
    # Train the gbm model
    gbm_model <- gbm(formula, data = train_data, n.trees = 100, interaction.depth = 3)
    
    # Store the model in the models list
    models[[pos]] <- gbm_model
    
    # Make predictions on the test data
    pos_test_data <- test_data[test_data$Pos == pos, ]
    pos_predictions <- predict(gbm_model, newdata = pos_test_data, n.trees = 100)
    
    # Calculate evaluation metrics for test data
    pos_mse <- mean((pos_test_data$Wage - pos_predictions)^2)
    pos_rmse <- sqrt(pos_mse)
    pos_rsq <- cor(pos_test_data$Wage, pos_predictions)^2
    
    # Print evaluation metrics for the current position
    cat("Position:", pos, "(Test Data)\n")
    cat("MSE:", pos_mse, "\n")
    cat("RMSE:", pos_rmse, "\n")
    cat("R-squared:", pos_rsq, "\n")
    
    # Store the evaluation metrics for the current position
    pos_rmse[[pos]] <- pos_rmse
    pos_mse[[pos]] <- pos_mse
    pos_rsq[[pos]] <- pos_rsq
  } else {
    # Print a message if the target variable is not found in the data for the position
    cat("Target variable 'Wage' not found for position", pos, "\n")
  }
}


# Calculate evaluation metrics for the whole group
all_predictions <- unlist(lapply(models, function(model) predict(model, newdata = test_data, n.trees = 100)))
all_mse <- mean((test_data$Wage - all_predictions)^2)
all_rmse <- sqrt(all_mse)
# Convert test_data$Wage to a vector
test_wage <- as.vector(test_data$Wage)

# Repeat all_predictions to match the length of test_wage
all_predictions_rep <- rep(all_predictions, length.out = length(test_wage))

# Calculate R-squared for the whole group
all_rsq <- cor(test_wage, all_predictions_rep)^2

# Print evaluation metrics for the whole group
cat("All Positions (Test Data)\n")
cat("MSE:", all_mse, "\n")
cat("RMSE:", all_rmse, "\n")
cat("R-squared:", all_rsq, "\n")


# Create an empty data frame with the same number of rows as test_data
result_df <- data.frame(
  Player = character(length(test_data$Player)),
  Actual_Wage = numeric(length(test_data$Player)),
  Model_1_Prediction = numeric(length(test_data$Player)),
  Model_2_Prediction = numeric(length(test_data$Player)),
  Model_3_Prediction = numeric(length(test_data$Player)),
  Model_4_Prediction = numeric(length(test_data$Player)),
  Model_5_Prediction = numeric(length(test_data$Player)),
  Model_6_Prediction = numeric(length(test_data$Player)),
  Model_7_Prediction = numeric(length(test_data$Player)),
  Model_8_Prediction = numeric(length(test_data$Player))
)

result_df$Player <- test_data$Player
result_df$Actual_Wage <- test_data$Wage

result_df$Model_1_Prediction <- test_pred
result_df$Model_2_Prediction <- test_pred_2
result_df$Model_3_Prediction <- test_pred_3
result_df$Model_4_Prediction <- test_pred_4
result_df$Model_5_Prediction <- test_pred_5
result_df$Model_6_Prediction <- predictions_6
result_df$Model_7_Prediction <- predictions_7
result_df$Model_8_Prediction <- all_predictions_rep

# Calculate the average prediction
result_df$Average_Prediction <- rowMeans(result_df[, c("Model_1_Prediction", "Model_2_Prediction", "Model_3_Prediction", "Model_4_Prediction", "Model_5_Prediction", "Model_6_Prediction", "Model_7_Prediction", "Model_8_Prediction")])
# Calculate MSE
mse <- mean((result_df$Average_Prediction - result_df$Actual_Wage)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate R-squared
ss_total <- sum((result_df$Actual_Wage - mean(result_df$Actual_Wage))^2)
ss_residual <- sum((result_df$Average_Prediction - result_df$Actual_Wage)^2)
r2 <- 1 - (ss_residual / ss_total)

# Print the results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")

# Set the seed for reproducibility
set.seed(123)

# Select the specified models
selected_models <- result_df[, c("Model_3_Prediction", "Model_4_Prediction", "Model_5_Prediction", "Model_7_Prediction")]

# Calculate the average prediction for the specified models
result_df$Average_Prediction_2 <- rowMeans(selected_models)

# Calculate the R-squared
ss_total <- sum((result_df$Actual_Wage - mean(result_df$Actual_Wage))^2)
ss_residual <- sum((result_df$Average_Prediction_2 - result_df$Actual_Wage)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Calculate the MSE
mse <- mean((result_df$Average_Prediction_2 - result_df$Actual_Wage)^2)

# Calculate the RMSE
rmse <- sqrt(mse)

# Print the results
cat("Selected Models: Model_3, Model_4, Model_5, Model_7\n")
cat("Average Prediction:", result_df$Average_Prediction_2, "\n")
cat("R-squared:", r_squared, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# Create a new dataframe for best predictions
best_predictions_df <- data.frame(Player = result_df$Player,
                                  Actual_Wage = result_df$Actual_Wage,
                                  Best_Prediction = result_df$Model_1_Prediction)

# Calculate the absolute difference between actual wage and each model's prediction
for (i in 3:12) {
  model_prediction <- result_df[, i]
  abs_diff <- abs(model_prediction - result_df$Actual_Wage)
  
  # Update the best prediction if the current model has a smaller absolute difference
  best_predictions_df$Best_Prediction <- ifelse(abs_diff < abs(best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage),
                                                model_prediction,
                                                best_predictions_df$Best_Prediction)
}

# Calculate the absolute difference between actual wage and average predictions
avg_abs_diff <- abs(result_df$Average_Prediction - result_df$Actual_Wage)

# Update the best prediction if the average prediction has a smaller absolute difference
best_predictions_df$Best_Prediction <- ifelse(avg_abs_diff < abs(best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage),
                                              result_df$Average_Prediction,
                                              best_predictions_df$Best_Prediction)

# Print the new dataframe with the best predictions
print(best_predictions_df)

# Calculate the squared differences between the best predictions and actual wage
squared_diff <- (best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage)^2

# Calculate the MSE
mse <- mean(squared_diff)

# Calculate the RMSE
rmse <- sqrt(mse)

# Calculate the total sum of squares
ss_total <- sum((best_predictions_df$Actual_Wage - mean(best_predictions_df$Actual_Wage))^2)

# Calculate the residual sum of squares
ss_residual <- sum(squared_diff)

# Calculate the R-squared
r_squared <- 1 - (ss_residual / ss_total)

# Print the results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Create a new dataframe for best predictions
best_predictions_df <- data.frame(Player = result_df$Player,
                                  Actual_Wage = result_df$Actual_Wage,
                                  Best_Prediction = result_df$Model_1_Prediction)

# Calculate the absolute difference between actual wage and each model's prediction
for (i in 3:12) {
  model_prediction <- result_df[, i]
  abs_diff <- abs(model_prediction - result_df$Actual_Wage)
  
  # Update the best prediction if the current model has a smaller absolute difference
  best_predictions_df$Best_Prediction <- ifelse(abs_diff < abs(best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage),
                                                model_prediction,
                                                best_predictions_df$Best_Prediction)
}

# Calculate the absolute difference between actual wage and average predictions
avg_abs_diff <- abs(result_df$Average_Prediction - result_df$Actual_Wage)

# Update the best prediction if the average prediction has a smaller absolute difference
best_predictions_df$Best_Prediction <- ifelse(avg_abs_diff < abs(best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage),
                                              result_df$Average_Prediction,
                                              best_predictions_df$Best_Prediction)

# Print the new dataframe with the best predictions
print(best_predictions_df)

# Calculate the squared differences between the best predictions and actual wage
squared_diff <- (best_predictions_df$Best_Prediction - best_predictions_df$Actual_Wage)^2

# Calculate the MSE
mse <- mean(squared_diff)

# Calculate the RMSE
rmse <- sqrt(mse)

# Calculate the total sum of squares
ss_total <- sum((best_predictions_df$Actual_Wage - mean(best_predictions_df$Actual_Wage))^2)

# Calculate the residual sum of squares
ss_residual <- sum(squared_diff)

# Calculate the R-squared
r_squared <- 1 - (ss_residual / ss_total)

# Print the results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")


# Calculate the difference between actual price and predicted best price
best_predictions_df$Difference <- best_predictions_df$Actual_Wage - best_predictions_df$Best_Prediction

# Sort the data frame by the difference in descending order (overrated)
overrated_df <- best_predictions_df[order(-best_predictions_df$Difference), ]

# Sort the data frame by the difference in ascending order (underrated)
underrated_df <- best_predictions_df[order(best_predictions_df$Difference), ]

# Extract the performance metrics for all models and average predictions
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Average", "Average 2")
mse_values <- c(mean((result_df$Model_1_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_2_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_3_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_4_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_5_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_6_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_7_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_8_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Average_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Average_Prediction_2 - result_df$Actual_Wage)^2))

# Define the colors for each model
colors <- c("steelblue", "darkorange", "forestgreen", "firebrick", "purple", "gold", "darkorchid", "salmon", "skyblue", "gray")

# Create the performance comparison plot
barplot(mse_values, names.arg = model_names, xlab = "Models", ylab = "Mean Squared Error (MSE)",
        main = "Performance Comparison of Models", col = colors, ylim = c(0, max(mse_values) * 1.1))

r_squared_values <- c(summary(lm(Actual_Wage ~ Model_1_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_2_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_3_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_4_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_5_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_6_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_7_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_8_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Average_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Average_Prediction_2, data = result_df))$r.squared)

# Create the performance comparison plot for R-squared values
barplot(r_squared_values, names.arg = model_names, xlab = "Models", ylab = "R-squared",
        main = "R-squared Comparison of Models", col = colors, ylim = c(0, 1))
# Extract the performance metrics for all models and average predictions
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Average", "Average 2")
mse_values <- c(mean((result_df$Model_1_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_2_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_3_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_4_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_5_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_6_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_7_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Model_8_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Average_Prediction - result_df$Actual_Wage)^2),
                mean((result_df$Average_Prediction_2 - result_df$Actual_Wage)^2))

# Calculate the R-squared values
r_squared_values <- c(summary(lm(Actual_Wage ~ Model_1_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_2_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_3_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_4_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_5_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_6_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_7_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Model_8_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Average_Prediction, data = result_df))$r.squared,
                      summary(lm(Actual_Wage ~ Average_Prediction_2, data = result_df))$r.squared)

# Create a data frame for the performance metrics
performance_df <- data.frame(Model = model_names, MSE = mse_values, R_squared = r_squared_values)

# Print the performance metrics table
print(performance_df)

# Create scatter plots for each model
for (i in 3:12) {
  # Get the column name of the prediction variable
  pred_col <- colnames(result_df)[i]
  
  # Create a new plot
  plot(result_df$Actual_Wage, result_df[, pred_col],
       xlab = "Actual Wage", ylab = "Predicted Value",
       main = paste("Model:", model_names[i-2]),
       col = colors[i-2], pch = 16)
  
  # Add a 45-degree reference line
  abline(a = 0, b = 1, col = "gray")
}

# Create scatter plot for best predictions
plot(best_predictions_df$Actual_Wage, best_predictions_df$Best_Prediction,
     xlab = "Actual Wage", ylab = "Best Predicted Value",
     main = "Best Predictions",
     col = "steelblue", pch = 16)

# Add a 45-degree reference line
abline(a = 0, b = 1, col = "gray")


# Create empty lists to store MSE values for each model
mse_train_list <- vector("list", length = 9)
mse_test_list <- vector("list", length = 9)

# Define the range of training sample sizes
sample_sizes <- seq(50, nrow(result_df), by = 50)

# Iterate over each model and average predictions
for (i in 1:9) {
  # Initialize empty vectors to store MSE values for the current model
  mse_train <- numeric()
  mse_test <- numeric()
  
  # Iterate over different training sample sizes
  for (size in sample_sizes) {
    # Subset the data for the current sample size
    subset_data <- result_df[1:size, ]
    
    if (i <= 8) {
      # Train the model using the subset data
      model <- lm(Actual_Wage ~ get(paste0("Model_", i, "_Prediction")), data = subset_data)
      
      # Make predictions on the training set
      train_predictions <- predict(model, subset_data)
      
      # Calculate the MSE on the training set
      mse_train <- c(mse_train, mean((subset_data$Actual_Wage - train_predictions)^2))
      
      # Make predictions on the full dataset
      test_predictions <- predict(model, result_df)
      
      # Calculate the MSE on the full dataset
      mse_test <- c(mse_test, mean((result_df$Actual_Wage - test_predictions)^2))
    } else {
      # Make predictions using the average prediction columns
      train_predictions <- subset_data[, grep("Average_Prediction", colnames(subset_data))]
      test_predictions <- result_df[, grep("Average_Prediction", colnames(result_df))]
      
      # Calculate the MSE on the training set
      mse_train <- c(mse_train, mean((subset_data$Actual_Wage - rowMeans(train_predictions))^2))
      
      # Calculate the MSE on the full dataset
      mse_test <- c(mse_test, mean((result_df$Actual_Wage - rowMeans(test_predictions))^2))
    }
  }
  
  # Store the MSE values in the corresponding lists
  mse_train_list[[i]] <- mse_train
  mse_test_list[[i]] <- mse_test
}

# Plot the learning curves for each model and average predictions
plot(NULL, xlim = c(0, max(sample_sizes)), ylim = c(0, max(unlist(mse_train_list, mse_test_list))),
     xlab = "Training Sample Size", ylab = "MSE", main = "Learning Curves - Mean Squared Error")

# Add learning curves for each model
colors <- rainbow(9)
legend_labels <- c(paste0("Model ", 1:8), "Average", "Average 2")
for (i in 1:9) {
  lines(sample_sizes, mse_train_list[[i]], col = colors[i])
  lines(sample_sizes, mse_test_list[[i]], col = colors[i], lty = 2)
}

# Add legend
legend("topright", legend = legend_labels, col = colors, lty = c(1, 2), bty = "n")


# Function to create residual plot for a model
create_residual_plot <- function(model_name, prediction_column) {
  # Calculate the residuals
  residuals <- result_df$Actual_Wage - result_df[[prediction_column]]
  
  # Plot the residuals against the predicted values
  plot(result_df[[prediction_column]], residuals,
       xlab = "Predicted Values", ylab = "Residuals",
       main = paste("Residual Plot -", model_name))
  
  # Add a horizontal line at y = 0
  abline(h = 0, col = "red", lty = 2)
}

# Iterate over the models in result_df and create residual plots
for (i in 3:ncol(result_df)) {
  model_name <- colnames(result_df)[i]
  prediction_column <- colnames(result_df)[i]
  create_residual_plot(model_name, prediction_column)
}
