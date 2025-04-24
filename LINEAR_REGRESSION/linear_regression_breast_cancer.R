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

data <- read.csv("./breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


###################################
# DATASET MODIFICATION 
###################################

# We make some of the modificatons previously done in the descriptive analysis part:

data <- data %>% select(-id) # We remove id

data$diagnosis <- factor(data$diagnosis) # Create a factor from the diagnosis variabl:
levels(data$diagnosis) <- c("benign", "malign") # Instead of using 'b' & 'm', use more descriptive labels
summary(data$diagnosis) # We check that previously steps worked properly

# We divide the dataset into two parts: one to train the model (80%) and the other to test it (20%)
data_test <- data[473:569, ] # Data to test the model
data_train <- data[1:472, ] # Data to test the model


###################################
# RESPONSE SELECTION
###################################

# We have selected the Radius Mean as the response to calculate. This decisions has been made after 
# reviewing again the descriptive analysis and confirming that the radius mean is a good parameter
# to classify a tumor as Benign or Malign (We know this will be done in Logistic Regression, but we 
# understand that the highest-level goal of such a study is to differentiate the diagnosis of the tumor, 
# and as the Radius Mean does it, the selection of the response follows that logic)

# This is how we have seen that radius mean is important for predicting the diagnosis:

# First we plot the boxplots for the radius mean based on the diagnosis
ggplot(data, aes(x = diagnosis , y = radius_mean, fill = diagnosis)) + 
  geom_boxplot() + 
  ggtitle("Comparison of Radius Mean for Benign vs Malignant Tumors") 

# Then we perform an ANOVA test using the function aov() (not anova()), to see whether there is a 
# significant difference in the variation within the groups and between the groups.

# Fit the ANOVA model
model_aov <- aov(radius_mean ~ diagnosis, data = data)

# Check overall significance
anova_result <- summary(model_aov)
p_value <- anova_result[[1]]$"Pr(>F)"[1]
p_value # The p-value is 8.465941e-96, confirming that the is a significant difference between 
# the radius mean for each diagnosis group.

# Thus, the selection of Radius Mean as the response makes sense taking into account our objective


###################################
# PREDICTOR SELECTION
###################################

# We start by thinking what variables can't be selected as predictors, taking into account the 
# correlations calculated during the descriptive analysis (obviously we are not going to use the diagnosis
# as a predictor, since we have stated that our final objective is to calculate it from the radius):

    # radius_se & radius_worst: as they are other measurements of the response
    # variables regarding area & perimeter: as they are mathematically dependent on radius (obviously related)
    # x_worst: as they are highly correlated (over 0.7) with x_mean, introducing multicollinearity
    # some x_se: as they are correlated with x_mean, introducing multicollinearity
    # The ones with a correlation below 0.5 are: texture, smoothness & symmetry.
    # concavity_mean & concave.points_mean: we can't select both as they are highly correlated (over 0.9).
    # Thus, we choose the one more correlated to radius_mean, in this case concave.points_mean.
      cor(data$radius_mean, data$concavity_mean)
      cor(data$radius_mean, data$concave.points_mean)

# *x_se, x_worst & x_mean refer to the generalised way of expressing all the variables including 
# se, worst or mean


# We create the linear model with all the predictors that can be included taking into account our analysis
mod1 <- lm(radius_mean~texture_mean+smoothness_mean+compactness_mean
           +concave.points_mean+symmetry_mean+fractal_dimension_mean
           +texture_se+smoothness_se+symmetry_se, data = data)


###################################
#### Test of all predictors (F test)

# Check the pvalue
mod1s = summary(mod1)
mod1s # The pvalue is very small (<2.2e-16)


# We can compute the pvalue 
# TSS
TSS <- sum((data$radius_mean-mean(data$radius_mean))^2)
# RSS
RSS <- deviance(mod1)
# F statistic

Fstat <- ((TSS-RSS)/(9)) / (RSS/df.residual(mod1))
Fstat

# F_alpha(p,n-(p+1)) # setting alpha = 0.05
F_alpha = qf(0.05, 9, 559, lower.tail = FALSE) # n-p(+1) = df.residual(mod1) = dim(x)[2]-1

# Reject H0 if Fstat > F_alpha
Fstat > F_alpha

# to obtain the pvalue we have to compute P(F_alpha>Fstat) to obtain the pvalue
pf(Fstat, 9, df.residual(mod1), lower.tail = FALSE)

# Given that the pvalue is very small the null hypothesis is rejected, meaning that the model with 
# 9 variables is better than the model given by the average.

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
mod_nested <- lm(radius_mean~smoothness_mean+compactness_mean
                 +concave.points_mean+symmetry_mean+fractal_dimension_mean
                 +smoothness_se, data = data)

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
mod10 <- lm(radius_mean~texture_mean+smoothness_mean+compactness_mean
           +concave.points_mean+symmetry_mean+fractal_dimension_mean
           +texture_se+smoothness_se+symmetry_se, data = data_train)
summary(mod10)
# The symmetry_se has the highest pvalue higher than the critical alpha

# We delete symmetry_se and create a new model
mod11 <- lm(radius_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se, data = data_train)
summary(mod11) # The R squared is the same even though we have removed a variable
# Now texture_se has the highest pvalue above the threshold

# We delete texture_se and create a new model
mod12 <- lm(radius_mean~smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_mean+smoothness_se, data = data_train)
summary(mod12)# The R squared is almost the same after removing another variable
# Now texture_mean has the highest pvalue above the threshold

# We delete texture_mean and create a new model
mod13 <- lm(radius_mean~smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +smoothness_se, data = data_train)
summary(mod13)# The R squared is almost the same after removing another variable


###################################
# We perform a quick diagnosis to confirm everything is correct
plot(mod13)

# We visualize that some points may be problematic, so we try removing them
new_data <- data_train[c(-214,-213, -276), ] # The highest cook's distance (leverage) & highest residuals

# The new model without that observations 
mod20 <- lm(radius_mean~smoothness_mean+compactness_mean+concave.points_mean
            +symmetry_mean+fractal_dimension_mean+smoothness_se, data = new_data)

# We do the diagnosis from the model without the previously mentioned points
plot(mod20)

# We compare the summary statistics
summary(mod13)
summary(mod20)

# All the parameters have very similar estimates. The only big difference is the smoothness_se, where
# the estimate is reduced from -44 to -64 and its pvalue is 10 times smaller
# However, the adjusted R-squared value is extremely similar in both cases

###################################


###################################
# PARAMETER CONFIDENCE INTERVAL
###################################

# We compute the confidence interval for the coefficients of all the predictors for alpha = 0.05
confint(mod13) # As none of the intervals contain 0, we can conclude with 95% probability that none of 
# the variables are equal to 0, meaning that they are relevant and necessary in the model.


###################################
# PREDICTING THE RESPONSE: CONFIDENCE & PREDICTION INTERVALS
###################################

###################################
#### Confidence Interval

# Prediction with the data used to train the model (confidence interval) #
confidence_interval <- data.frame(radius_mean = data_train$radius_mean)
confidence_interval <- cbind(confidence_interval, as.data.frame(
                             predict(mod13, data_train, interval = "confidence", level = 0.99)))

# Add a column to check whether the predicted values is in the range or not
confidence_interval$in_range <- ifelse(confidence_interval$radius_mean > confidence_interval$lwr & 
                                       confidence_interval$radius_mean < confidence_interval$upr, 
                                       TRUE, FALSE)
confidence_interval # In this case, as we are using the data used to train the model, the SE is low

# We can check the percentage of predictions that were in the given range of the confidence interval
sum(confidence_interval$in_range)/nrow(confidence_interval)
# The percentage seems pretty low, just about 20%. (Explanation for this results in the report)

###################################
#### Prediction Interval

# Prediction with "new" data, not the one used to train the model (prediction interval) #
prediction_interval <- data.frame(radius_mean = data_test$radius_mean)
prediction_interval <- cbind(prediction_interval, as.data.frame(
                             predict(mod13, data_test, interval = "prediction")))

# Add a column to check whether the predicted values is in the range or not
prediction_interval$in_range <- ifelse(prediction_interval$radius_mean > prediction_interval$lwr & 
                                         prediction_interval$radius_mean < prediction_interval$upr, 
                                       TRUE, FALSE)
prediction_interval # In this case, as there is more variability in predicting a new response, the 
# SE (difference between lower and upper) is larger than in the case of the confidence interval

# We can check the percentage of predictions that were in the given range of the prediction intervals
sum(prediction_interval$in_range)/nrow(prediction_interval)
# The percentage is very high, over 90%, which means that we are predicting the response for the given 
# values of the predictors well.


###################################
# LINEAR REGRESSION MODEL DIAGNOSTICS & ASSUMPTIONS
###################################

# We will carry out the model diagnosis visually by plotting the model (we will use the selected model)
plot(mod13)

###################################
### Heteroscedasticity and Linearity

# We visualize the first plot (residual plot) to check the residuals against the fitted values 
# This allows us to see if the variance is constant across the error terms (homoscedasticity)
# and that the errors are symmetric vertically about zero (linearity)
plot(mod13, 1) # The plot shows that both characteristics are fulfilled, meaning that the model 
# is homoscedastic and linear

###################################
### Normality

# We visualize the second plot (Q-Q plot) to check the distribution of the error terms
plot(mod13, 2) # Even though it is not perfectly normal, it's not considerably skewed or tailed, so
# we can consider that it fulfills the normality requirement.

# To ensure the above, we calculate the skewness of the distribution of residuals
skewness(residuals(mod13)) # The result is 0.3282 which means that is very close to symmetry

# Note: 
# We know that the Shapiro-Wilk test is used to check the normality of the distribution.
# However we have seen that it is very sensitive to small changes when used with many 
# observations.
shapiro.test(residuals(mod13)) # In theory, as the pvalue is lower than 0.05 we should reject
# the null hypothesis, meaning that the distribution is not normal. But taking into account
# what we have mentioned (the skewness & qq plot), we check that most residuals follow a 
# normal distribution.

###################################
### Unusual observations

# We check high leverage points by computing Cook's distance
sort(cooks.distance(mod13))

# We visualize the fifth plot (residual vs leverage points)
plot(mod13, 5) # We see that most point have a very low leverage, except for 214. Still, it is 
# inside the established limits for the Cook's distance.

## Similar to what we have done in the model selection part ##

# However, we try computing the model without that particular observation 
new_data_1 <- data[-214, ] # Excluding the point with highest leverage
# We create the new new model
mod21 <- lm(radius_mean~smoothness_mean+compactness_mean+concave.points_mean
            +symmetry_mean+fractal_dimension_mean+smoothness_se, data = new_data_1)

# We compare the two models (with and without observation number 214)
summary(mod13) # Observation 214 included
summary(mod21) # Observation 214 excluded

# We compare the two plots (with and without observation number 214)
plot(mod13, 5) # Observation 214 included
plot(mod21, 5) # Observation 214 excluded


# Again, all the parameters have very similar estimates. The only big difference is the smoothness_se, where
# the estimate is reduced from -44 to -72 and its pvalue is much smaller.
# However, the adjusted R-squared value is extremely similar in both cases, as well as the plotted graphs, 
# so the observation 214 wasn't that relevant.




