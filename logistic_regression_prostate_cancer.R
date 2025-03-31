# LOGISTIC REGRESSION MODEL OF THE PROSTATE CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante


# We load the libraries we will used to create the model:
library(dplyr)
library(pastecs)
library(ggplot2)
library(lattice)
library(car)
library(caret)
library(pROC)
library(ResourceSelection) # for the Hosmer-Lemeshow test


##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("./Prostate_Cancer.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


########################
# DATASET MODIFICATION #
########################

# We make some of the modificatons previously done in the descriptive analysis part:

data <- data %>% select(-id) # We remove id

data$diagnosis_result <- factor(data$diagnosis_result) # Create a factor from the diagnosis variabl:
levels(data$diagnosis_result) <- c("benign", "malign") # Instead of using 'b' & 'm', use more descriptive labels
summary(data$diagnosis_result) # We check that previously steps worked properly


######################
# RESPONSE SELECTION #
######################

# We have selected the Diagnosis Result as the response, being the reason that it is the only
# categorycal variable that we have in this dataset.


######################################
# SOME PLOTS FOR PREDICTOR SELECTION #
######################################

# Let´s take a look in the prediction vs the other parameters

# CREATE VIOLIN PLOTS USING GGPLOT

# Diagnosis Result vs Radius: it is not a good predictor because os the similarity of the distribution between bening and maling
ggplot(data, aes(x = diagnosis_result, y = radius, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Radius",
       title = "Violin Plot: Radius vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Texture: it is also not a good predictor due to the similar distribution in both bening and malign
ggplot(data, aes(x = diagnosis_result, y = texture, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Texture",
       title = "Violin Plot: Texture vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Perimeter: this is a good predictor, we can see the difference in the distibution of the results on the diagnosis result
ggplot(data, aes(x = diagnosis_result, y = perimeter, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Perimeter",
       title = "Violin Plot: Perimeter vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Area: as expected due to the relation with the perimeter value, this is a good predictor
ggplot(data, aes(x = diagnosis_result, y = area, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Area",
       title = "Violin Plot: Area vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal()

# Diagnosis Result vs Smoothness: this is not a good predictor
ggplot(data, aes(x = diagnosis_result, y = smoothness, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Smoothness",
       title = "Violin Plot: Smoothness vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Compactness: this is a good predictor as we can see in the distribution showed in the violin plot
ggplot(data, aes(x = diagnosis_result, y = compactness, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Compactness",
       title = "Violin Plot: Compactness vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Symmetry: symmetry is not a good predictor
ggplot(data, aes(x = diagnosis_result, y = symmetry, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Symmetry",
       title = "Violin Plot: Symmetry vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 

# Diagnosis Result vs Fractalm Dimension: fractal dimension is neither a good predictor
ggplot(data, aes(x = diagnosis_result, y = fractal_dimension, fill = diagnosis_result)) +
  geom_violin(trim=FALSE)+
  labs(x = "Diagnosis Result", y = "Fractal Dimension",
       title = "Violin Plot: Fractal Dimension vs. Diagnosis Result") +
  geom_boxplot(width=0.25, fill = NA, color = "black")+
  scale_fill_manual(values = c("brown1", "cornflowerblue")) +
  theme_minimal() 


#############################
# PLOT WITH SOME PREDICTORS #
#############################

# Now let's plot the probability of diagnosis_result vs some of the predictors 
# and fit it using a LR and a logistic regression model
# Add the probability of :
data$prob_diagnosis_result <- ifelse(data$diagnosis_result == "benign", 1, 0)

# - PERIMETER - #

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
  ggplot(data, aes(x = perimeter, y = prob_diagnosis_result)) +
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

# - RADIUS - #

# Now let´s try with the radius: we are seeing in the graph that as we notice in the PREDICTOR SELECTION
# radius is not a good predictor, now we can see it because of the straight line that generates ought to
# the even distribution of the results

# Create scatter plot with a logistic linear regression fit
# Fit a logistic regression model
logit_model <- glm(prob_diagnosis_result ~ radius, data = data, family = binomial())

# Predict probabilities using the model
data$predicted_probs <- predict(logit_model, type = "response")

# Create scatter plot with logistic regression curve
Plot_RadiusVsDef_LogFit <- 
  ggplot(data, aes(x = radius, y = prob_diagnosis_result)) +
  geom_point(aes(color = "Observations")) +  # Scatter plot
  geom_line(aes(y = predicted_probs, color = "Logistic Model")) +  # Log regression
  scale_color_manual(values = c("Observations" = "cornflowerblue", 
                                "Logistic Model" = "brown1"),
                     breaks = c("Observations", 
                                "Logistic Model")) +  # order legend items
  labs(x = "Radius", y = "",
       title = "Scatter Plot: Diagnosis Result vs. Radius",
       color = "Legend") +
  theme_minimal()


# We will check one last time with the compactness which should give us a good result: as we predicted,
# the compactness is a good predictor

# Create scatter plot with a logistic linear regression fit
# Fit a logistic regression model
logit_model <- glm(prob_diagnosis_result ~ compactness, data = data, family = binomial())

# Predict probabilities using the model
data$predicted_probs <- predict(logit_model, type = "response")

# Create scatter plot with logistic regression curve
Plot_CompactnessVsDef_LogFit <- 
  ggplot(data, aes(x = compactness, y = prob_diagnosis_result)) +
  geom_point(aes(color = "Observations")) +  # Scatter plot
  geom_line(aes(y = predicted_probs, color = "Logistic Model")) +  # Log regression
  scale_color_manual(values = c("Observations" = "cornflowerblue", 
                                "Logistic Model" = "brown1"),
                     breaks = c("Observations", 
                                "Logistic Model")) +  # order legend items
  labs(x = "Compactness", y = "",
       title = "Scatter Plot: Diagnosis Result vs. Compactness",
       color = "Legend") +
  theme_minimal()


#######################
# LOGISTIC REGRESSION #
#######################

# Model syntax #
# We are now ready to fit our model
mod0 <- glm(diagnosis_result ~ texture+perimeter+smoothness+compactness+symmetry+fractal_dimension,
            family=binomial(),
            data=data)
summary(mod0)

# Now we create a new model without the symmetry and check again
mod1 <- glm(diagnosis_result ~ texture+perimeter+smoothness+compactness+fractal_dimension,
            family=binomial(),
            data=data)
summary(mod1)

# Now we create a new model without the smoothness and check again
mod2 <- glm(diagnosis_result ~ texture+perimeter+compactness+fractal_dimension,
            family=binomial(),
            data=data)
summary(mod2)

# Now we create a new model without the perimeter and check again
mod3 <- glm(diagnosis_result ~ texture+compactness+fractal_dimension,
            family=binomial(),
            data=data)
summary(mod3)

# Finally, we create a new model without the perimeter and check again
mod4 <- glm(diagnosis_result ~ compactness+fractal_dimension,
            family=binomial(),
            data=data)
summary(mod4)


#########################
# CLASSIFICATION TABLES #
#########################

# The table is produced with two variables: data of the response and predictions
# The response is:
response <- data$prob_diagnosis_result

# In order to compute our predictions we need to fix the cut-off point
# Let's select cp=0.5
cp <- 0.5

# Let us now use the cut-off point to obtain our predictions
prediction <- ifelse(mod4$fitted.values < cp,0,1) 

# We can now create the table
tab <- confusionMatrix(as.factor(prediction), as.factor(response), positive="1")
tab

# Have a look at the table and compute the accuracy, sensitivity and specificity

ac <- (tab$table[1]+tab$table[4])/nrow(data)
sn <- tab$table[4]/(tab$table[3]+tab$table[4])
sp <- tab$table[1]/(tab$table[1]+tab$table[2])
c(ac,sn,sp)

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
par(pty="s")
Roc <- roc(response,mod4$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

# OPTIMAL cut-off POINT
# The optimal cut-off point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="cut-off points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.7,y=0.05,labels = "Sensitivity")
text(x=0.7,y=0.95,labels = "Specificity")
abline(v=0.084,col="gray",lty=2)
