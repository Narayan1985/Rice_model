setwd("C:/Users/Julian Valencia/OneDrive/Escritorio/Rice model")

# Step 1: Load necessary libraries
library(readxl) # For reading Excel files
library(corrplot) # For correlation plot
library(dplyr)    # For data manipulation
library(caret)  # For data preprocessing and model training
library(openxlsx) # For exporting results to Excel

# Step 2: Load the dataset
data <- read_excel("data_base_RF1.xlsx")
View(data)

# Step 3: Check the structure and summary statistics
str(data)
summary(data)

# Step 4: Data preprocessing (if needed)
sum(is.na(data))
data <- na.omit(data)

# Step 5: Check for correlation between variables
data_numeric <- data[, sapply(data, is.numeric)]  # Select only numeric columns

# Step 6: Check for missing values again
sum(is.na(data_numeric))

# Check for correlation between variables
num_data <- data %>% select_if(is.numeric)
cor_matrix <- cor(num_data)
View(cor_matrix)
corrplot(cor_matrix, method = "square", type = "full", 
         order = "original", tl.col = "black", tl.srt = 45, number.font=2, pch=1, 
         number.cex =0.5, tl.cex=0.5, mar = c(0, 0, 0, 0) )
write.xlsx(cor_matrix, "cor_matrix.xlsx")

# Step 5: Split the data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data$Yield, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Step 6: Fit a regression model
model <- lm(Yield ~ ., data = train_data)

# Step 7: Evaluate the model

test_data <- read_excel("test_data.xlsx")
predictions <- predict(model, newdata = test_data)
performance <- postResample(predictions, test_data$Yield)
print(paste("RMSE:", performance[["RMSE"]]))

# Step 8: Export the results to Excel
model_output <- data.frame(Actual = test_data$Yield, Predicted = predictions)
write.xlsx(model_output, "model_rice_output.xlsx")

# Summary of the model
summary(model)