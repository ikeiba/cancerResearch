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

## PCA ANALYSIS
data <- data[, sapply(data, is.numeric)] # Only the numeric data for the PCA
data_pca <- scale(data) # PCA requires standardization if variables are on different scales
# Perform Principal Component Analysis
pca_result <- PCA(data_pca)
plot.PCA(pca_result, choix = "var", axes = c(2, 3))

# We visualize the summary, to check every section of PCA
summary(pca_result)
pca_result$eig    # Only eigenvalues of components 1 and 2 are above 1 representing
                # 63% of the data, but component 3 is near with 0.98 so we will take it
                # into account representing 75% of the total data

pca_data <- pca_result$ind$coord[,1:3] # This stores the dataset after applying pca

# View the 6 first lines of each dataset
head(data)
head(pca_data)
