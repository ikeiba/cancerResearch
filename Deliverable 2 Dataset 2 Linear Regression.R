# LINEAR REGRESSION MODEL OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante


# We load the libraries we will used to create the model:
# install.packages("moments") # install if needed
library(dplyr)
library(pastecs)
library(ggplot2)
library(lattice)
library(car)
library(moments)

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

data$diagnosis <- factor(data$diagnosis_result) # Create a factor from the diagnosis variabl:
levels(data$diagnosis) <- c("benign", "malign") # Instead of using 'b' & 'm', use more descriptive labels
summary(data$diagnosis) # We check that previously steps worked properly

# We divide the dataset into two parts: one to train the model (80%) and the other to test it (20%)
data_test <- data[81:100, ] # Data to test the model
data_train <- data[1:80, ] # Data to test the model


###################################
# RESPONSE SELECTION
###################################

# We have selected the Compactness as the response to calculate. This decisions has been made after 
# reviewing again the descriptive analysis and confirming that the Compactness is a good parameter
# to classify a tumor as Benign or Malign (We know this will be done in Logistic Regression, but we 
# understand that the highest-level goal of such a study is to differentiate the diagnosis of the tumor, 
# and as the Compactness does it, the selection of the response follows that logic)

# This is how we have seen that radius mean is important for predicting the diagnosis:

# First we plot the boxplots for the radius mean based on the diagnosis
ggplot(data, aes(x = diagnosis , y = compactness, fill = diagnosis)) + 
  geom_boxplot() + 
  ggtitle("Comparison of Compactness for Benign vs Malignant Tumors") 

# Then we perform an ANOVA test using the function aov() (not anova()), to see whether there is a 
# significant difference in the variation within the groups and between the groups.

# Fit the ANOVA model
model_aov <- aov(compactness ~ diagnosis, data = data)

# Check overall significance
anova_result <- summary(model_aov)
p_value <- anova_result[[1]]$"Pr(>F)"[1]
p_value # The p-value is 5.104951e-08, confirming that the is a significant difference between 
# the compactness for each diagnosis group.

# Thus, the selection of Compactness as the response makes sense taking into account our objective


###################################
# PREDICTOR SELECTION
###################################

# We start by thinking what variables can't be selected as predictors, taking into account the 
# correlations calculated during the descriptive analysis (obviously we are not going to use the diagnosis
# as a predictor, since we have stated that our final objective is to calculate it from the compactness):

# We create the linear model with all the predictors that can be included taking into account our analysis
mod1 <- lm(compactness~radius+texture+perimeter+area+smoothness+compactness+symmetry+fractal_dimension, data = data)


###################################
#### Test of all predictors (F test)

# Check the pvalue
mod1s = summary(mod1)
mod1s # The pvalue is very small (<2.2e-16)


# We can compute the pvalue 
# TSS
TSS <- sum((data$compactness-mean(data$compactness))^2)
# RSS
RSS <- deviance(mod1)
# F statistic

Fstat <- ((TSS-RSS)/(8)) / (RSS/df.residual(mod1))
Fstat

# F_alpha(p,n-(p+1)) # setting alpha = 0.05
F_alpha = qf(0.05, 8, 559, lower.tail = FALSE) # n-p(+1) = df.residual(mod1) = dim(x)[2]-1

# Reject H0 if Fstat > F_alpha
Fstat > F_alpha

# to obtain the pvalue we have to compute P(F_alpha>Fstat) to obtain the pvalue
pf(Fstat, 8, df.residual(mod1), lower.tail = FALSE)

# Given that the pvalue is very small the null hypothesis is rejected, meaning that the model with 
# 8 variables is better than the model given by the average.

###################################
#### Test of a single predictor (T-Test)

# We just have to check the t statistic for each predictor
mod1s # High pvalues mean that the null hypothesis is not rejected and that the predictor is not
# relevant for the model

# It can also be computed directly
Tstat <- mod1s$coefficients[,1]/mod1s$coefficients[,2]
# this computes the estimator divided by the standard error, i.e., the statistic
Tstat

# t_alpha/2(n-(p+1)) # setting alpha = 0.05
t_alpha = qt(0.05/2, mod1$df.residual, lower.tail = FALSE)

# Reject H0 if 
abs(Tstat) > t_alpha

# to obtain the pvalue we have to compute P(|t|>Tstat) to obtain the pvalue
pval_t = 2*pt(abs(Tstat), mod1$df.residual, lower.tail = FALSE)
pval_t

###################################
#### Test of a subset of predictors / nested models (ANOVA Test)

# We select a subset of predictors based on the previously computed T-Test
mod_nested <- lm(compactness~perimeter+area+symmetry+fractal_dimension, data=data)

# In this case, H0 is that both models perform similarly, while H1 is that they perform differently
anova(mod_nested,mod1) # As the pvalue is large, we cannot reject H0, meaning that We can consider 
# the simpler model, since they perform similarly


###################################
# MODEL SELECTION
###################################

# Having in mind the restrictions mentioned in the predictor selection part, we will start the predictor 
# selection using Backward Elimination:
# First we select alpha = 0.05 as the significance level

# We create the first model, which will contain all the variables that haven't been logically discarded.
mod10 <- lm(compactness~radius+texture+perimeter+area+smoothness+symmetry+fractal_dimension, data = data_train)
summary(mod10)
# The texture has the highest pvalue higher than the critical alpha

# We delete texture and create a new model
mod11 <- lm(compactness~radius+perimeter+area+smoothness+symmetry+fractal_dimension, data = data_train)
summary(mod11) # The R squared is the same even though we have removed a variable
# Now radius has the highest pvalue above the threshold

# We delete radius and create a new model
mod12 <- lm(compactness~perimeter+area+smoothness+symmetry+fractal_dimension, data = data_train)
summary(mod12)# The R squared is almost the same after removing another variable
# Now smoothness has the highest pvalue above the threshold

# We delete smoothness and create a new model
mod13 <- lm(compactness~perimeter+area+symmetry+fractal_dimension, data = data_train)
summary(mod13)# The R squared is almost the same after removing another variable


###################################
# We perform a quick diagnosis to confirm everything is correct
plot(mod13)

###################################


###################################
# PARAMETER CONFIDENCE INTERVAL
###################################

# We compute the confidence interval for the coefficients of all the predictors for alpha = 0.05
confint(mod13) # As none of the intervals contain 0, we can conclude with 95% probability that none of 
# We visualize that some points may be problematic, so we try removing them
data_train <- data_train[c(-1), ]
# PREDICTING THE RESPONSE: CONFIDENCE & PREDICTION INTERVALS
###################################

###################################
#### Confidence Interval

# Prediction with the data used to train the model (confidence interval) #
confidence_interval <- data.frame(compactness = data_train$compactness)
confidence_interval <- cbind(confidence_interval, as.data.frame(
  predict(mod13, data_train, interval = "confidence", level = 0.95)))

# Add a column to check whether the predicted values is in the range or not
confidence_interval$in_range <- ifelse(confidence_interval$compactness > confidence_interval$lwr & 
                                         confidence_interval$compactness < confidence_interval$upr, 
                                       TRUE, FALSE)
confidence_interval # In this case, as we are using the data used to train the model, the SE is low

# We can check the percentage of predictions that were in the given range of the confidence interval
sum(confidence_interval$in_range)/nrow(confidence_interval)
# The percentage seems pretty low, just about 30%.

###################################
#### Prediction Interval

# Prediction with "new" data, not the one used to train the model (prediction interval) #
prediction_interval <- data.frame(compactness = data_test$compactness)
prediction_interval <- cbind(prediction_interval, as.data.frame(
  predict(mod13, data_test, interval = "prediction")))

# Add a column to check whether the predicted values is in the range or not
prediction_interval$in_range <- ifelse(prediction_interval$compactness > prediction_interval$lwr & 
                                         prediction_interval$compactness < prediction_interval$upr, 
                                       TRUE, FALSE)
prediction_interval # In this case, as there is more variability in predicting a new response, the 
# SE (difference between lower and upper) is larger than in the case of the confidence interval

# We can check the percentage of predictions that were in the given range of the prediction intervals
sum(prediction_interval$in_range)/nrow(prediction_interval)
# The percentage 100% which means that we are predicting the response for the given 
# values of the predictors well.