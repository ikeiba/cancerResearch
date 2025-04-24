## PCA Analysis in the Prostate Cancer Dataset

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(FactoMineR)


##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("C:/Users/carlos.firvida/OneDrive - Universidad de Deusto/Documentos/Estadistica Avanzada/Teamwork Proyect - Cancer/data/Prostate_Cancer.csv")

# View the structure of the dataset
str(data)

# Remove non-numeric or ID variables
data <- data %>% select(-id)
data_pca <- data[, sapply(data, is.numeric)] # Only the numeric data

# View the first few rows of the selected data
head(data_pca)


## STANDARDIZING THE DATA
# PCA requires standardization if variables are on different scales

data_scaled <- scale(data_pca)


################
# PCA ANALYSIS #
################

# Perform Principal Component Analysis
pca_result <- PCA(data_scaled, graph = FALSE)

# View the eigenvalues (variances explained by each component)
pca_result$eig

# Cumulative variance
cumulative_var <- cumsum(pca_result$eig[, 2])
cumulative_var