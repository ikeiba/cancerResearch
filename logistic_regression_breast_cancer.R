# LINEAR REGRESSION MODEL OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# We load the necessary libraries to perform the analysis:
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualizations
library(car)         # For additional diagnostic functions
library(pROC)        # For ROC curve and AUC

###################################
# DATASET LOADING & CHECKING
###################################

# Load the dataset from the CSV file
data <- read.csv("C:/path_to_your_file/breast_cancer_data.csv") 

# View the first six rows to inspect the dataset
head(data)

# Check the structure of the dataset: variable names, types, and first values
str(data)

###################################
# DATASET MODIFICATION
###################################

# Removing the 'id' column which is not needed for our model
data <- data %>% select(-id)

# Converting 'diagnosis' variable into a factor (Benign or Malignant)
data$diagnosis <- factor(data$diagnosis)
levels(data$diagnosis) <- c("benign", "malign") # Rename levels for clarity

# Let's check the levels of the diagnosis to confirm our changes
summary(data$diagnosis)

# We will calculate the correlation matrix to help identify any highly correlated predictors
cor_matrix <- cor(data %>% select(-diagnosis))  # Exclude diagnosis for correlation calculation

# View the correlation matrix
View(cor_matrix)

###################################
# LOGISTIC REGRESSION MODEL
###################################

# Fit the logistic regression model using selected predictors:
# We use predictors with no high correlation and no multicollinearity issues
modelo_logistico <- glm(diagnosis ~ texture_mean + smoothness_mean + compactness_mean + 
                          concave.points_mean + symmetry_mean + fractal_dimension_mean, 
                        data = data, family = binomial)

# Summary of the logistic regression model:
summary(modelo_logistico)

# Interpretation of the results:
# The p-values of the predictors indicate which variables are significant. 
# A p-value less than 0.05 suggests the predictor is significantly contributing to the model.
# Look at the coefficients to understand the effect of each predictor on the likelihood of malignancy.

###################################
# PREDICTIONS AND CONVERTING PROBABILITIES TO CLASSIFICATIONS
###################################

# Use the model to predict probabilities of malignancy (probability of being malign)
data$prob_maligno <- predict(modelo_logistico, type = "response")

# Convert probabilities to class labels: 1 for malign, 0 for benign based on a 0.5 threshold
data$pred_diagnosis <- ifelse(data$prob_maligno > 0.5, "malign", "benign")

# Convert predicted diagnosis to factor type for comparison with actual diagnosis
data$pred_diagnosis <- factor(data$pred_diagnosis, levels = c("benign", "malign"))

# View the first few predictions and compare with the actual diagnosis
head(data[, c("diagnosis", "prob_maligno", "pred_diagnosis")])

###################################
# EVALUATION OF THE MODEL
###################################

# Confusion matrix: compares predicted labels with actual labels
library(caret)  # Load caret for confusion matrix
conf_matrix <- confusionMatrix(data$pred_diagnosis, data$diagnosis)

# Print confusion matrix to check accuracy, sensitivity, and specificity
print(conf_matrix)

# Calculate accuracy, sensitivity, and specificity manually:
accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table) # Accuracy = (TP+TN) / Total
sensitivity <- conf_matrix$byClass["Sensitivity"] # Sensitivity = TP / (TP + FN)
specificity <- conf_matrix$byClass["Specificity"] # Specificity = TN / (TN + FP)

# Print the calculated values
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

###################################
# ROC CURVE AND AUC  -  ASK GUILIO IF IT'S NECESSARY
###################################

# Create the ROC curve and calculate AUC (Area Under the Curve)
roc_curve <- roc(data$diagnosis, data$prob_maligno)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate and print the AUC
cat("AUC:", auc(roc_curve), "\n")

###################################
# CONCLUSIONS
###################################

# From the confusion matrix and the calculated metrics, we can conclude:
# - The model's accuracy indicates how often the model is correct in predicting the diagnosis.
# - Sensitivity (recall) is crucial for identifying malignant tumors correctly, which is important for medical decisions.
# - Specificity tells us how well the model identifies benign cases.
# - A higher AUC value (closer to 1) means the model is better at distinguishing between the classes.

# In this case, based on the results (e.g., p-values, accuracy, AUC), we can conclude that the chosen predictors provide significant information for classifying tumors as benign or malignant. 

# The model is performing reasonably well in terms of classification performanc.

