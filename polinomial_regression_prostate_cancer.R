# LINEAR REGRESSION MODEL OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante


# We load the libraries we will used to create the model:
library(dplyr)
library(pastecs)
library(ggplot2)
library(lattice)
library(car)


###################################
# DATASET LOADING & CHECKING
###################################

data <- read.csv("./Prostate_Cancer.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

###################################
# DATASET MODIFICATION 
###################################

# We make some of the modificatons previously done in the descriptive analysis part:

data <- data %>% select(-id) # We remove id

data$diagnosis_result <- factor(data$diagnosis_result) # Create a factor from the diagnosis variabl:
levels(data$diagnosis_result) <- c("benign", "malign") # Instead of using 'b' & 'm', use more descriptive labels
summary(data$diagnosis_result) # We check that previously steps worked properly

###################################
# RESPONSE SELECTION
###################################

# We have selected the Diagnosis Result as the response, being the reason that it is the only
# categorycal variable that we have in this dataset.

######################################
# SOME PLOTS FOR PREDICTOR SELECTION #
######################################

# Let´s take a look in the prediction vs the other parameters

# Create violin plots using ggplot
# Diagnosis Result vs Radius: it is not a good predictor because os the similarity of the distribution betweenn bening and maling
ggplot(data, aes(x = diagnosis_result, y = radius, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Radius",
       title = "Violin Plot: Radius vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs
ggplot(data, aes(x = diagnosis_result, y = texture, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Texture",
       title = "Violin Plot: Texture vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs this good?
ggplot(data, aes(x = diagnosis_result, y = perimeter, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Perimeter",
       title = "Violin Plot: Perimeter vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs this good?
ggplot(data, aes(x = diagnosis_result, y = area, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Area",
       title = "Violin Plot: Area vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 
# Diagnosis Result vs
ggplot(data, aes(x = diagnosis_result, y = smoothness, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Smoothness",
       title = "Violin Plot: Smoothness vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs this good?
ggplot(data, aes(x = diagnosis_result, y = compactness, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Compactness",
       title = "Violin Plot: Compactness vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs
ggplot(data, aes(x = diagnosis_result, y = symmetry, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Symmetry",
       title = "Violin Plot: Symmetry vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs
ggplot(data, aes(x = diagnosis_result, y = fractal_dimension, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Fractal Dimension",
       title = "Violin Plot: Fractal Dimension vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 


# Now let's plot the probability of diagnosis_result vs some of the predictors 
# and fit it using a LR and a logistic regression model
# Add the probability of :
data$prob_diagnosis_result <- ifelse(data$diagnosis_result == "benign", 1, 0)


# To start we are going to use as the predictor the perimeter: as we checked in the PREDICTOR SELECTION,
# the perimeter is a good predictor and we can see it in this scatter plot thanks to the curve of the
# logistic model which matches

# Create scatter plot with a logistic linear regression fit
# Fit a logistic regression model
logit_model <- glm(prob_diagnosis_result ~ perimeter, data = data, family = binomial())

# Predict probabilities using the model
data$predicted_probs <- predict(logit_model, type = "response")

# Create scatter plot with logistic regression curve
Plot_PerimVsDef_LogFit <- 
  ggplot(data, aes(x = perimeter, y = prob_default)) +
  geom_point(aes(color = "Observations")) +  # Scatter plot
  geom_line(aes(y = predicted_probs, color = "Logistic Model")) +  # Log regression
  scale_color_manual(values = c("Observations" = "cornflowerblue", 
                                "Logistic Model" = "brown1"),
                     breaks = c("Observations", 
                                "Logistic Model")) +  # order legend items
  labs(x = "Perimeter", y = "",
       title = "Scatter Plot: Diagnosis Result vs. Perimeter",
       color = "Legend") +
  theme_minimal()


# Now let´s try with the radius: we are seeing in the graph that as we notice in the PREDICTOR SELECTION
# radius is not a good predictor, now we can see it because of the straight line that does not fit the data

# Create scatter plot with a logistic linear regression fit
# Fit a logistic regression model
logit_model <- glm(prob_diagnosis_result ~ radius, data = data, family = binomial())

# Predict probabilities using the model
data$predicted_probs <- predict(logit_model, type = "response")

# Create scatter plot with logistic regression curve
Plot_RadiusVsDef_LogFit <- 
  ggplot(data, aes(x = radius, y = prob_default)) +
  geom_point(aes(color = "Observations")) +  # Scatter plot
  geom_line(aes(y = predicted_probs, color = "Logistic Model")) +  # Log regression
  scale_color_manual(values = c("Observations" = "cornflowerblue", 
                                "Logistic Model" = "brown1"),
                     breaks = c("Observations", 
                                "Logistic Model")) +  # order legend items
  labs(x = "Radius", y = "",
       title = "Scatter Plot: Radius vs. Perimeter",
       color = "Legend") +
  theme_minimal()


# We will check one last time with the compactness which should give us a good result: [ASK Giulio]

# Create scatter plot with a logistic linear regression fit
# Fit a logistic regression model
logit_model <- glm(prob_diagnosis_result ~ compactness, data = data, family = binomial())

# Predict probabilities using the model
data$predicted_probs <- predict(logit_model, type = "response")

# Create scatter plot with logistic regression curve
Plot_CompactVsDef_LogFit <- 
  ggplot(data, aes(x = radius, y = prob_default)) +
  geom_point(aes(color = "Observations")) +  # Scatter plot
  geom_line(aes(y = predicted_probs, color = "Logistic Model")) +  # Log regression
  scale_color_manual(values = c("Observations" = "cornflowerblue", 
                                "Logistic Model" = "brown1"),
                     breaks = c("Observations", 
                                "Logistic Model")) +  # order legend items
  labs(x = "Compactness", y = "",
       title = "Scatter Plot: Compactness vs. Perimeter",
       color = "Legend") +
  theme_minimal()
#########################
# CLASSIFICATION TABLES # [ASK Giulio]
#########################
