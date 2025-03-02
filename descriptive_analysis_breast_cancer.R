# DESCRIPTIVE ANALYSIS OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Description of the dataset:


# First of all, we load the libraries we are going to use to perform the analysis
library(dplyr)
library(pastecs)


###################################
# START
# We read the dataset from the csv file
data <- read.csv("./data.csv")

# We get some general information from the dataset, ensuring that everything has been loaded correctly
head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

# After this two first steps, we detect that there is a variable called id, which will not be useful for us
data <- data %>% select(-id) # We remove id

# We check the dimensions of the dataset (569 rows and 31 columns)
dim(data) 

# Now we get the numerical summary for some variable(s)
summary(data) #[,] # We can add the [] to select specific variables

# After performing this analysis, we detected that the minimum value of some variables is 0 (the ones related to concavity).
# As we are not experts in this field (medicine, precisely breast cancer), at first we could not tell
# whether this observations could be correct or maybe something had gone wrong during the measurements.
# We decided to do some research and it seems that this is uncommon, yet possible (especially in benign tumors).
# This finding might help us later in order to find correlations and patterns

#As we know that is unlikely, we check which is the proportion of the observations that have concavity_mean = 0
sum(data$concavity_mean == 0, na.rm = TRUE) / dim(data)[1] # Around 0.02 (2%) which could make sense

# The variable diagnosis is categorical (binary). So we will create a factor from it:
data$diagnosis <- factor(data$diagnosis)
# We check that know the data is interpreted as a factor rather than a simple char
summary(data$diagnosis)   # There are have 357  benign tumor cases and 212  malign tumour cases

# Instead of using just 'b' and 'm', we change to more descriptive labels:
levels(data$diagnosis) <- c("benign", "malign")
# We check again that this change has been executed appropriately
summary(data$diagnosis)

# We are going to check as well for the existence of na observations (missing values)
colSums(is.na(data)) # There are no missing values

# We could use this function as well to check for some descriptive statistics and missing values
stat.desc(data)

###################################
# Metrics such as minimum, maximum, median, mean and 1st and 3rd quartiles are already computed with the summary() function
# However, if at any moment we would like to compute each of them separately, we could do it in the following way:
# *we have omitted the na.rm=TRUE parameter as we have previously checked that there are no missing values
variable_to_analyse <- data$radius_mean

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
# In this step we will do a deeper analysis of our variables: we already knew that there were "just" 10 different metrics
# (radius, texture, perimeter, area, smoothness, compactness, concavity, concave points, symmetry and fractal dimension),
# each computed in three different ways (mean, se (standard error) and worst). 

# Now, we will check if there is any correlation between each metric's mean and se.
cor(data$radius_mean, data$radius_se)
cor(data$texture_mean, data$texture_se)
cor(data$perimeter_mean, data$perimeter_se)
cor(data$area_mean, data$area_se)
cor(data$smoothness_mean, data$smoothness_se)
cor(data$compactness_mean, data$compactness_se)
cor(data$concavity_mean, data$concavity_se)
cor(data$concave.points_mean, data$concave.points_se)
cor(data$symmetry_mean, data$symmetry_se)
cor(data$fractal_dimension_mean, data$fractal_dimension_se)
# In most cases the correlation has been between 0.6 and 0.7 (a positive correlation but not very strong)
# In other cases such as the smoothness, texture symmetry the correlation has been weaker (around 0.3-0.4)
# Finally, in the case of area the correlation has been strong (0.8)

# We are going to check the correlation between each metric's mean and worst (in this case we can already suspect that 
#the correlation is going to be strong, as higher means should have a higher worst value)
cor(data$radius_mean, data$radius_worst)
cor(data$texture_mean, data$texture_worst)
cor(data$perimeter_mean, data$perimeter_worst)
cor(data$area_mean, data$area_worst)
cor(data$smoothness_mean, data$smoothness_worst)
cor(data$compactness_mean, data$compactness_worst)
cor(data$concavity_mean, data$concavity_worst)
cor(data$concave.points_mean, data$concave.points_worst)
cor(data$symmetry_mean, data$symmetry_worst)
cor(data$fractal_dimension_mean, data$fractal_dimension_worst)
# As suspected, in most cases the correlation was very strong (>0.85). In the cases of symmetry, smoothness and
# fractal_dimension the correlation was lower but still positive and considerably strong.

# Finally we compute the correlation between standard errors and worst, obtaining similar results to the ones 
# comparing mean and se
cor(data$radius_se, data$radius_worst)
cor(data$texture_se, data$texture_worst)
cor(data$perimeter_se, data$perimeter_worst)
cor(data$area_se, data$area_worst)
cor(data$smoothness_se, data$smoothness_worst)
cor(data$compactness_se, data$compactness_worst)
cor(data$concavity_se, data$concavity_worst)
cor(data$concave.points_se, data$concave.points_worst)
cor(data$symmetry_se, data$symmetry_worst)
cor(data$fractal_dimension_se, data$fractal_dimension_worst)

# We check the correlation of some variables 
cor(data$radius_mean, data$texture_mean) # Low correlation (0.32)
cor(data$radius_mean, data$smoothness_mean) # Very low correlation (0.17)
cor(data$radius_mean, data$concave.points_mean) # Strong correlation (0.82)

# For instance, we know that the correlation of radius & perimeter, radius & area, and perimeter & area are going 
# to be extremely strong, as there is a linear relationship between them (thus, we couldn't use both for a linear model)
cor(data$radius_mean, data$perimeter_mean) # Very strong correlation (over 0.99, almost perfect correlation)
cor(data$radius_mean, data$area_mean) # Very strong correlation (over 0.98, near perfect correlation)
cor(data$perimeter_mean, data$area_mean) # Very strong correlation (over 0.98, near perfect correlation)













data_new <- data[,1:10]
numeric_df <- data_new[, sapply(data_new, is.numeric)]
# Create multiscatter plot
pairs(numeric_df)



