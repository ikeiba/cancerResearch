# K-MEANS CLUSTERING OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Load necessary libraries
library(FactoMineR)
library(scico)
library(plotrix)
library(dplyr)
library(factoextra)
library(patchwork)
library(plotly)
library(fields)


##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("C:/Users/carlos.firvida/OneDrive - Universidad de Deusto/Documentos/Estadistica Avanzada/Teamwork Proyect - Cancer/data/Prostate_Cancer.csv")

# We will approach to apply k-means to the original dataset and after applying PCA

# Remove non-numeric or ID variables
data <- data %>% select(-id)
data_pca <- data[, sapply(data, is.numeric)] # Only the numeric data for the PCA
data_pca <- scale(data_pca) # PCA requires standardization if variables are on different scales

# View the structure of the dataset
head(data)
head(data_pca)