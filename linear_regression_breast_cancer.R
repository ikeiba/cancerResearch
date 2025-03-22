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

data <- read.csv("./breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

###################################
# DATASET MODIFICATION (we make some of the modificatons previously done in the descriptive analysis part)

data <- data %>% select(-id) # We remove id

data$diagnosis <- factor(data$diagnosis) # Create a factor from the diagnosis variabl:
levels(data$diagnosis) <- c("benign", "malign") # Instead of using 'b' and 'm',  change to more descriptive labels
summary(data$diagnosis) # We check that previously steps worked properly

###################################
# CHOOSING RESPONSE

# We have selected the Radius Mean as the response to calculate. This decisions has been made after 
# reviewing again the descriptive analysis and confirming that the radius mean is an excellent parameter
# to classify a tumor as Benign or Malign (We know this will be done in Logistic Regression, but we 
# understand that the highest-level goal of such a study is to differentiate the diagnosis of the tumor, 
# and as the Radius Mean does it, the selection of the response follows that logic)

