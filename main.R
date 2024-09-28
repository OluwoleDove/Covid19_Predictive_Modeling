if (!require(readxl)) {
  install.packages("readxl")
}
library(dplyr)
library(readxl)
library(caret)
library(randomForest)

# Define the main directory and subdirectory
main_dir <- "C:/Users/ladma/OneDrive/Documents/GitHub_Repos/Covid19_Predictive_Modeling"
sub_dir_exists <- "datasets"

# Create the subdirectory if it doesn't exist
dir.create(file.path(main_dir, sub_dir_exists), showWarnings = FALSE)

# Construct the full file paths
google_trend_path <- file.path(main_dir, sub_dir_exists, "GoogleTrend_Latest.xlsx")
train_data_path <- file.path(main_dir, sub_dir_exists, "Train_dataset.xlsx")
test_data_path <- file.path(main_dir, sub_dir_exists, "Test_dataset.xlsx")

# Load the data from Excel files
google_trend <- read_excel(google_trend_path, sheet = "GoogleTrend_Latest")
train_data <- read_excel(train_data_path, sheet = "Train_dataset")
test_data <- read_excel(test_data_path, sheet = "Test_dataset")


head(google_trend)
head(train_data)

str(google_trend)
str(train_data)
str(test_data)

summary(google_trend)
summary(train_data)
summary(test_data)

sum(is.na(google_trend))
sum(is.na(train_data))
sum(is.na(test_data))

library(ggplot2)

# Time series plot for Google Trends data
ggplot(google_trend, aes(x = as.Date(date, "%m/%d/%Y"))) +
  geom_line(aes(y = coronavirus, color = "coronavirus")) +
  geom_line(aes(y = covid19, color = "covid19")) +
  geom_line(aes(y = lockdown, color = "lockdown")) +
  geom_line(aes(y = symptoms, color = "symptoms")) +
  geom_line(aes(y = socialdistance, color = "socialdistance")) +
  labs(title = "Google Trend Data", x = "Date", y = "Search Popularity")


# Removing rows with missing values
google_trend <- na.omit(google_trend)
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)


train_data$Gender <- as.factor(train_data$Gender)
train_data$Occupation <- as.factor(train_data$Occupation)
test_data$Gender <- as.factor(test_data$Gender)
test_data$Occupation <- as.factor(test_data$Occupation)

# Ensure both datasets have the same date format
google_trend$date <- as.Date(google_trend$date, "%m/%d/%Y")

# Merge the datasets by date and region
combined_data <- merge(train_data, google_trend, by = "Region")

set.seed(123)
trainIndex <- createDataPartition(combined_data$Infect_Prob, p = 0.8, list = FALSE)
train_set <- combined_data[trainIndex, ]
test_set <- combined_data[-trainIndex, ]

# Train the random forest model
model_rf <- train(Infect_Prob ~ ., data = train_set, method = "rf", trControl = trainControl(method = "cv", number = 5))

# View the model
print(model_rf)

# Make predictions
predictions <- predict(model_rf, newdata = test_set)

# Calculate accuracy
postResample(predictions, test_set$Infect_Prob)

# Predict on new test data
test_predictions <- predict(model_rf, newdata = test_data)

# View predictions
head(test_predictions)

# Model evaluation using RMSE
rmse <- sqrt(mean((test_predictions - test_data$Infect_Prob)^2))
print(rmse)

saveRDS(model_rf, file = "random_forest_model.rds")

loaded_model <- readRDS("random_forest_model.rds")

# Use the model to make predictions again
new_predictions <- predict(loaded_model, newdata = test_data)


#print(google_trend)
#colnames(google_trend)


