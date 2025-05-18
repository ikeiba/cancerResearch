# PCA, K-means, and HCA on the breast cancer dataset
# Adapted authors: you + original authors

# Load required libraries
library(FactoMineR)
library(dplyr)

###############
# DATA LOADING
###############

# Read the dataset
data <- read.csv("sample_data/breast_cancer_data.csv")

# Remove non-numeric columns (e.g., 'id', or categorical variables if present)
data <- data %>% select(where(is.numeric))

# Standardize the numeric data
data_scaled <- scale(data)

####################################
# PRINCIPAL COMPONENT ANALYSIS (PCA)
####################################

# Run PCA
pca_result <- PCA(data_scaled, graph = FALSE)  # graph=FALSE prevents popup window

# View PCA summary
summary(pca_result)

# View variable contributions to PCA components
plot.PCA(pca_result, choix = "var", axes = c(1, 2))

# Extract individual coordinates in PCA space
pca_data <- pca_result$ind$coord[, 1:2]  # Use the first two principal components

###############
# K-MEANS CLUSTERING
###############

set.seed(123)  # For reproducibility
k <- 3         # Number of clusters to use

kmeans_result <- kmeans(pca_data, centers = k, nstart = 10)

# View cluster sizes and centers
print(kmeans_result$size)
print(kmeans_result$centers)

######################################
# HIERARCHICAL AGGLOMERATIVE CLUSTERING (HCA)
######################################

# Compute Euclidean distance matrix
dist_matrix <- dist(pca_data, method = "euclidean")

# Apply hierarchical clustering with the "complete" linkage method
hclust_result <- hclust(dist_matrix, method = "complete")

# Cut the dendrogram into k groups
hc_clusters <- cutree(hclust_result, k = k)

##############################
# COMPARISON: K-MEANS VS. HCLUST
##############################

# Compare K-means results with hierarchical clustering
comparison_table <- table(KMeans = kmeans_result$cluster, HClust = hc_clusters)
print(comparison_table)

# Optional: visualize dendrogram
plot(hclust_result, labels = FALSE, main = "HCA Dendrogram - Breast Cancer")
rect.hclust(hclust_result, k = k, border = 2:4)

# Conclusion:
#
# In this analysis, we applied PCA to reduce the dimensionality of the breast cancer dataset and visualize its main components.
# We then performed K-means clustering with k = 3 on the PCA-transformed data to identify potential groups among the observations.
# Additionally, we used Hierarchical Clustering with complete linkage to create a dendrogram and cut it into 3 clusters.
# Comparing both clustering methods showed some agreement, but also highlighted differences due to their distinct algorithms.
# Overall, combining PCA with clustering allowed us to explore the structure of the dataset and better understand hidden patterns.

