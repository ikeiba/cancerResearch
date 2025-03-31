# DESCRIPTIVE ANALYSIS OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Description of the dataset:
# The prostate cancer dataset consists of 100 observations and 10 variables, and it was made for
#  implementing the machine learning algorithm and thereby interpret results

# The following variables were recorded:
#   * Id
#   * Radius
#   * Texture
#   * Perimeter
#   * Area
#   * Smoothness
#   * Compactness
#   * diagnosis_result
#   * Symmetry
#   * Fractal dimension

# First of all, we load the libraries we are going to use to perform the analysis
library(dplyr)
library(pastecs)
library(ggplot2)
library(lattice)
library(car)

###################################
# START
# We read the dataset from the csv file
data <- read.csv("./Prostate_Cancer.csv")

# We get some general information from the dataset, ensuring that everything has been loaded correctly
head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

# After this two first steps, we detect that there is a variable called id, which will not be useful for us
data <- data %>% select(-id) # We remove id

# We check the dimensions of the dataset (100 rows and 9 columns)
dim(data) 

# Now we get the numerical summary for some variable(s)
summary(data) #[,] # We can add the [] to select specific variables

# The variable diagnosis_result is categorical (binary). So we will create a factor from it:
data$diagnosis_result <- factor(data$diagnosis_result)
# We check that know the data is interpreted as a factor rather than a simple char
summary(data$diagnosis_result)   # There are have 38b benign tumor cases and 62 malign tumour cases

# Instead of using just 'b' and 'm', we change to more descriptive labels:
levels(data$diagnosis_result) <- c("benign", "malign")
# We check again that this change has been executed appropriately
summary(data$diagnosis_result)

# We are going to check as well for the existence of na observations (missing values)
colSums(is.na(data)) # There are no missing values

# We could use this function as well to check for some descriptive statistics and missing values
stat.desc(data)

###################################
# Metrics such as minimum, maximum, median, mean and 1st and 3rd quartiles are already computed with the summary() function
# However, if at any moment we would like to compute each of them separately, we could do it in the following way:
# *we have omitted the na.rm=TRUE parameter as we have previously checked that there are no missing values
variable_to_analyse <- data$area

min(variable_to_analyse)
max(variable_to_analyse)
median(variable_to_analyse)
mean(variable_to_analyse)
quantile(variable_to_analyse, 0.25) # First quartile 
quantile(variable_to_analyse, 0.75) # Third quartile

# Additionally, we could compute some other metrics (we haven't include the mode as the variables are continuous)
range(variable_to_analyse) # Range of the variable
quantile(variable_to_analyse, 0.4) # Any percentile of the variable
IQR(variable_to_analyse) # Interquartile range (this would be later visualize via boxplots)
sd(variable_to_analyse) # Standard deviation
var(variable_to_analyse) # Variance

# In case we would want to use one of this functions over a subset of variables:
subset_to_analyse <- data[, 1:31] # Select the subset you want
lapply(subset_to_analyse, mean) # Modify the second parameter for whatever function you want

###################################
# Correlation
# In this step we will do a deeper analysis in some of our variables:
cor(data$texture, data$smoothness) # 0.1 small positive correlation
cor(data$texture, data$symmetry) # 0.07 even smaller correlation
cor(data$texture, data$fractal_dimension) # 0.13 small correlation

cor(data$smoothness, data$compactness) # 0.46 normal correlation
cor(data$smoothness, data$symmetry)  # 0.42 normal correlation
cor(data$smoothness, data$fractal_dimension)  # 0.36 smaller but still, normal correlation

cor(data$compactness, data$symmetry) # 0.68 high correlation between this two variables
cor(data$compactness, data$fractal_dimension) # 0.64 still a high positive correlation

cor(data$symmetry, data$fractal_dimension) # 0.56 high positive correlation

cor(data$area, data$compactness) # 0.42 positive correlation, expected to be higher though
cor(data$area, data$smoothness) # 0.2 small correlation
cor(data$area, data$fractal_dimension) # -0.27 small negative correlation
###################################