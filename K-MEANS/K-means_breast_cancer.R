# CLUSTERING (K-MEANS) OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# We load the libraries we will used to create the model:
#install.packages("FactoMineR")
library(FactoMineR)


###################################
# DATASET LOADING & CHECKING
###################################

data <- read.csv("./breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


###################################
# DATASET MODIFICATION
###################################

# We remove the variables that are not continuous:

#* Before removing it we will save the diagnosis in a variable, as it will be useful 
# at the end of the analysis
diagnosis <- as.factor(data$diagnosis)  # Replace with your actu

data <- data %>% select(-id, -diagnosis) # We remove & diganosis


###################################
# APPROACH SELECTION
###################################

# In this case, we have decided to study the two approaches proposed in the guidelines:

    # Apply K-means to the components selected during PCA.

    # Apply K-means directly to the original variables.


###################################
# K-MEANS APPLICATION
###################################






# Verify that there are two groups
# Subtypes of maligns