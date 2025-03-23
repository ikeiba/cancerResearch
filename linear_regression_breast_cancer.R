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
# correlations calculated during the descriptive analysis:

    # radius_se & radius_worst: as they are other measurements of the response
    # variables regarding area & perimeter: as they are mathematically dependent on radius (obviously related)
    # x_worst: as they are highly correlated (over 0.7) with x_mean, possibly introducing multicollinearity
    # some x_se: as they are correlated (over 0.5) with x_mean, possibly introducing multicollinearity
    # The ones with a correlation below 0.5 are: texture, smoothness & symmetry.
    # concavity_mean & concave.points_mean: we can't select both as they are highly correlated (over 0.9).
    # Thus, we choose the one more correlated to radius_mean, in this case concave.points_mean.


# *x_se, x_worst & x_mean refer to the generalised way of expressing all the variables including 
# se, worst or mean

# Having in mind those restrictions, we will start the predictor selection using Backward Elimination:
# First we select alpha = 0.15 as the significance level

# We create the first model, which will contain all the variables that haven't been logically discarded.
mod10 <- lm(radius_mean~texture_mean+smoothness_mean+compactness_mean
           +concave.points_mean+symmetry_mean+fractal_dimension_mean
           +texture_se+smoothness_se+symmetry_se, data = data)
summary(mod10)
# The symmetry_se has the highest pvalue higher than the critical alpha

# We delete symmetry_se and create a new model
mod11 <- lm(radius_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se, data = data)
summary(mod11) # The R squared is the same even though we have removed a variable
# Now texture_mean has the highest pvalue above the threshold

# We delete texture_mean and create a new model
mod12 <- lm(radius_mean~smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se, data = data)
summary(mod12)# The R squared is almost the same after removing another variable
# Now texture_se has the highest pvalue above the threshold

# We delete texture_se and create a new model
mod13 <- lm(radius_mean~smoothness_mean+compactness_mean+concave.points_mean
            +symmetry_mean+fractal_dimension_mean+smoothness_se, data = data)
summary(mod13)# The R squared is still almost the same after having removed three variables

# There are no variables with a p-value higher than 0.15, so apparently all the variables 
# should be in the model

##############################################################################################
# CONSULT WITH THE TEACHER
# We perform a diagnosis to confirm everything is correct
plot(mod13)

new_data <- data[-214, ] # The highest cook's distance (leverage)
new_data <- data[c(-214,-213,-569, -276), ] # The ones that standout in graphs
# We delete texture_se and create a new model
mod23 <- lm(radius_mean~smoothness_mean+compactness_mean+concave.points_mean
            +symmetry_mean+fractal_dimension_mean+smoothness_se, data = new_data)
summary(mod23)# The R squared is still almost the same after having removed three variables

plot(mod13,1)
plot(mod23,1)
##############################################################################################

AIC(mod10, mod11, mod12, mod13)
BIC(mod10, mod11, mod12, mod13)

