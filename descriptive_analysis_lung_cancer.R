# DESCRIPTIVE ANALYSIS OF THE LUNG CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Description of the dataset:


# First of all, we load the libraries we are going to use to perform the analysis
library(dplyr)
library(pastecs)


###################################
# START
# We read the dataset from the csv file
data <- read.csv("./lung_cancer_data.csv")

# We get some general information from the dataset, ensuring that everything has been loaded correctly
head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

# After this two first steps, we detect that there is a variable called Patient_ID, which will not be useful for us
data <- data %>% select(-Patient_ID) # We remove Patient_ID

# We check the dimensions of the dataset (23658 rows and 37 columns)
dim(data) 

# Now we get the numerical summary for some variable(s)
summary(data) #[,] # We can add the [] to select specific variables

# After performing this analysis, we did not detected any particular thing that might be wrong with the data.

# The variable Gender, Comorbidity_Diabetes, Comorbidity_Hypertension,
# Comorbidity_Heart_Disease, Comorbidity_Chronic_Lung_Disease, Comorbidity_Kidney_Disease,
# Comorbidity_Autoimmune_Disease, Comorbidity_Other are categorical (binary). So we will create a factor from it:
data$Gender <- factor(data$Gender)
data[,12] <- factor(data[,12])
data[,13] <- factor(data[,13])
data[,14] <- factor(data[,14])
data[,15] <- factor(data[,15])
data[,16] <- factor(data[,16])
data[,17] <- factor(data[,17])
data[,18] <- factor(data[,18])

# We check that know the data is interpreted as a factor rather than a simple char
summary(data$Gender)   # There are 1174 female cases and 11924 male
summary(data[,12])     # There are 11897 with comorbidity diabetes and 11761 who do not have
summary(data[,13])     # There are 11798 with comorbidity hypertension and 11860 who do not have
summary(data[,14])     # There are 11915 with comorbidity heart disease and 11743 who do not have
summary(data[,15])     # There are 11780 with comorbidity chronic lung disease and 11878 who do not have
summary(data[,16])     # There are 11921 with comorbidity kidney disease and 11737 who do not have
summary(data[,17])     # There are 11845 with comorbidity autoimmune disease and 11813 who do not have
summary(data[,18])     # There are 11882 with other type of comorbidity and 11776 who do not have

# We are going to check as well for the existence of na observations (missing values)
colSums(is.na(data)) # There are no missing values

# We could use this function as well to check for some descriptive statistics and missing values
stat.desc(data)

###################################
# Metrics such as minimum, maximum, median, mean and 1st and 3rd quartiles are already computed with the summary() function
# However, if at any moment we would like to compute each of them separately, we could do it in the following way:
# *we have omitted the na.rm=TRUE parameter as we have previously checked that there are no missing values
variable_to_analyse <- data$Hemoglobin_Level

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
subset_to_analyse <- data[, 1:37] # Select the subset you want
lapply(subset_to_analyse, mean) # Modify the second parameter for whatever function you want

###################################
# Correlation
# In this step we will do a deeper analysis of our variables: 

cor(data$Tumor_Size_mm, data$Smoking_Pack_Years)     # We look if there is a correlation between the size of the tumour and the spoking years of the patient     
cor(data$Tumor_Size_mm, data$Survival_Months)     # We look if there is a correlation between the tumour size and the survival months
cor(data$Hemoglobin_Level, data$Tumor_Size_mm)     # We look if there is a correlation between the hemoglovin level and the tumour size
cor(data$Blood_Pressure_Systolic, data$Survival_Months)     # We look if there is a correlation between the blood pressure systolic and the survival months
cor(data$Blood_Pressure_Systolic, data$Blood_Pressure_Diastolic)     # We look if there is a correlation between the blood pressure systolic and the blood pressure diastolic

data_means <- data[,30:35]
numeric_df_means <- data_means[, sapply(data_means, is.numeric)]
pairs(numeric_df_means) # for the means
# In most cases the correlation is very low, beeing the biggest correlation 0.0122 (positive but not very strong)














data_new <- data[,1:10]
numeric_df <- data_new[, sapply(data_new, is.numeric)]
# Create multiscatter plot
pairs(numeric_df)