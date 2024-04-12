#Define the work folder
setwd("C:/Users/Julian Valencia/OneDrive/Escritorio/Rice model")

# Load necessary libraries
library(readxl) # For reading Excel files
library(corrplot) # For correlation plot
library(dplyr)    # For data manipulation
library(caret)  # For data preprocessing and model training
library(openxlsx) # For exporting results to Excel

# Load the dataset
data <- read_excel("data_base_RF1.xlsx")
View(data)

# Check the structure and summary statistics
str(data)
summary(data)

# Data preprocessing (if needed)
sum(is.na(data))
data <- na.omit(data)

# Check for correlation between variables
data_numeric <- data[, sapply(data, is.numeric)]  # Select only numeric columns

# Check for missing values again
sum(is.na(data_numeric))

# Check for correlation between variables
num_data <- data %>% select_if(is.numeric)
cor_matrix <- cor(num_data)
View(cor_matrix)
corrplot(cor_matrix, method = "square", type = "full", 
         order = "original", tl.col = "black", tl.srt = 45, number.font=2, pch=1, 
         number.cex =0.5, tl.cex=0.5, mar = c(0, 0, 0, 0) )

# Split the data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data$Yield, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Fit a regression model
model <- lm(Yield ~ ., data = train_data)

# Evaluate the model
predictions <- predict(model, newdata = test_data)
performance <- postResample(predictions, test_data$Yield)
mse <- mean((test_data$Yield - predictions)^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error
print(paste("RMSE:", rmse, performance[["RMSE"]]))

# Summary of the model
summary(model)

plot(model)

# Feature importance
correlation <- cor(num_data, data$Yield)
View(cor_matrix)
write.xlsx(cor_matrix, "matriz_correlacion.xlsx", row.names = TRUE)
corrplot(cor_matrix, method = "square", type = "full", 
         order = "original", tl.col = "black", tl.srt = 45, number.font=2, pch=1, 
         number.cex =0.5, tl.cex=0.5, mar = c(0, 0, 0, 0) )

coeficientes <- coef(model)[-1]  # Exclude intercept
importance <- abs(coeficientes)
importance[is.na(importance)] <- 0  # Manage values NA
importance <- importance / sum(importance) 

# Create a vector to delete features 
caracteristicas_a_eliminar <- c("SeasonB", "site1_Huila_Campoalegre", "Site2_Huila_Agrado", "Site3_Huila_Campoalegre", "Site4_Huila_Villavieja",
                                "Site5_Huila_Villavieja", "Site6_Huila_Palermo", "Site7_Huila_Campoalegre", "Site8_Huila_Baraya", "Site9_Huila_Neiva",
                                "Site10_Huila_Villavieja", "Site11_Tolima_Ibague", "Site12_Tolima_Coello", "Site13_Tolima_Espinal", "Site14_Tolima_Lerida",
                                "Site15_Tolima_Melgar", "Site16_Tolima_Purificacion", "Site17_Tolima_Prado", "Site18_Tolima_Guamo", "Site19_Tolima_Ambalema",
                                "Site20_Tolima_Coyaima", "Site21_Tolima_Flandes", "Site22_Tolima_Sanluis", "Site23_Tolima_Piedras", "Site24_Tolima_Lerida",
                                "Site25_Tolima_Armero", "Site26_Tolima_Honda")

# Delete the mportance vector features 
importance <- importance[!(names(importance) %in% caracteristicas_a_eliminar)]

# Normalice the importance values
importance <- importance / sum(importance)

importance_df <- data.frame(Feature = names(importance), Importance = importance, Correlation_with_Yield = correlation[-ncol(correlation)])

importance_df <- importance_df[order(abs(importance_df$Correlation_with_Yield), decreasing = TRUE), ]

ggplot(importance_df, aes(x = reorder(Feature, Correlation_with_Yield), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Value Weight") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(importance_df)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(x = "Feature", y = "Value Weight") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Export the results to Excel
model_output <- data.frame(Actual = test_data$Yield, Predicted = predictions)
View(model_output)
plot(model_output)
write.xlsx(model_output, "model_rice_output.xlsx")
