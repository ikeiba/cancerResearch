# K-MEANS CLUSTERING OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Required packages (uncomment to install if necessary)
# install.packages("FactoMineR")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("factoextra")
# install.packages("reshape2")
# install.packages("scico")

# Load libraries
library(FactoMineR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(reshape2)
library(scico)

##############################
# LOADING & PREPARING DATASET #
##############################

data <- read.csv("sample_data/breast_cancer_data.csv")

# The aim is to apply k-means both to the original dataset and to its PCA-transformed version

# Remove non-numeric or identifier variables
data <- data %>% select(-id)
data

## PCA ANALYSIS
data <- data[, sapply(data, is.numeric)] # Retain only numeric features for PCA
data_pca <- scale(data) # Standardize variables to equalize scale
pca_result <- PCA(data_pca)
plot.PCA(pca_result, choix = "var", axes = c(2, 3))

# Overview of PCA output
summary(pca_result)
pca_result$eig  # First 2 components have eigenvalues > 1 (63%), and the third is close (0.98)
# So we include the third to cover ~75% of total variance

# Store PCA-transformed data (first 3 principal components)
pca_data <- pca_result$ind$coord[,1:3]

# Preview both datasets
head(data)
head(pca_data)

##############################################
# APPLYING CLUSTERING TO ORIGINAL AND PCA DATA #
##############################################

## Hierarchical Clustering ##

# Determine number of clusters using the elbow method
fviz_nbclust(pca_data, hcut, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 4, color = "Elbow Point"), linetype = "dashed", size = 1)

fviz_nbclust(data, hcut, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 2, color = "Elbow Point"), linetype = "dashed", size = 1)

# Silhouette method as alternative for cluster validation
fviz_nbclust(pca_data, hcut, method = "silhouette", k.max = 20)
fviz_nbclust(data, hcut, method = "silhouette", k.max = 20)

# Compute distance matrices (Euclidean)
distance_matrix_pca <- dist(pca_data, method = "euclidean")
distance_matrix_og <- dist(data, method = "euclidean")

# Perform hierarchical clustering
hc_pca <- hclust(distance_matrix_pca, method = "complete")
hc_og <- hclust(distance_matrix_og, method = "complete")

# Plot dendrograms with clusters
plot(hc_pca)
rect.hclust(hc_pca, k = 4, border = 2:10)

plot(hc_og)
rect.hclust(hc_og, k = 2, border = 2:10)

# Assign clusters using horizontal cut (height)
cutree(hc_pca, h=5)
cutree(hc_og, h=5)

# Assign clusters using vertical cut (k clusters)
clusters_pca <- cutree(hc_pca, k=4)
clusters_og <- cutree(hc_og, k=2)

###########################
# CLUSTER COMPARISON #
###########################

# Cross-tabulate cluster assignments
cluster_comparison <- table(
  Original = clusters_pca,
  PCA = clusters_og
)

# Format the table for visualization
cluster_df <- melt(cluster_comparison)
names(cluster_df) <- c("Original", "PCA", "Common_points")

# Create a heatmap to compare cluster overlaps
heatmap_plot <- ggplot(cluster_df, aes(x = PCA, y = Original, fill = Common_points)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = seq(1, 4, by = 1)) +
  scale_y_continuous(breaks = seq(1, 2, by = 1)) +
  labs(
    title = "Comparison of clusters: Original Variables vs PCA",
    x = "Clusters based on original variables",
    y = "Clusters based on PCA",
    fill = "Common points"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = Common_points), color = "black")

# Display the heatmap
print(heatmap_plot)

# The heatmap reveals the relationship between the clustering results.
# Although we expected to see a clearer mapping between clusters from both models,
# the different number of clusters prevents this. It does seem, however, that:
# - Clusters 1 and 2 from PCA correspond mostly to Cluster 1 in the original model.
# - Clusters 3 and 4 from PCA do not align clearly with any original clusters.
# This could suggest some hidden structure captured only through PCA.

##########################
# CLUSTER CHARACTERIZATION #
##########################

# Append cluster labels to data
data_pca <- as.data.frame(data_pca)
data_pca$cluster <- as.factor(clusters_pca)

data$cluster <- as.factor(clusters_og)

# Compute cluster-wise means
data_pca %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

# Reshape data for plotting
data_long_pca <- data_pca %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

data_long_og <- data %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

cluster_colors <- scico(4, palette = "lajolla")

# Violin plots showing distribution of variables per cluster
pca_plot <- ggplot(data_long_pca, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

og_plot <- ggplot(data_long_og, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# According to the violin plots:
# - In the original model, major distinctions arise in area and perimeter.
# - In PCA, clusters 1 and 2 have higher values in area/perimeter, 
#   while clusters 1 and 3 show higher values in compactness, smoothness, symmetry, etc.
# - Radius and texture seem relatively constant across all PCA clusters.

## Correlation analysis between clusters and variables
vars_og <- data[, sapply(data, is.numeric)]
n_clusters_og <- length(unique(clusters_og))

vars_pca <- data_pca[, sapply(data_pca, is.numeric)]
n_clusters_pca <- length(unique(clusters_pca))

# Initialize correlation matrices
cor_matrix_og <- matrix(NA, nrow = ncol(vars_og), ncol = n_clusters_og)
rownames(cor_matrix_og) <- colnames(vars_og)
colnames(cor_matrix_og) <- paste0("Cluster_", 1:n_clusters_og)

cor_matrix_pca <- matrix(NA, nrow = ncol(vars_pca), ncol = n_clusters_pca)
rownames(cor_matrix_pca) <- colnames(vars_pca)
colnames(cor_matrix_pca) <- paste0("Cluster_", 1:n_clusters_pca)

# Compute correlation between each cluster (as binary vector) and variables
for (k in 1:n_clusters_og) {
  cluster_bin_og <- as.numeric(clusters_og == k)
  cor_matrix_og[, k] <- sapply(vars_og, function(x) cor(x, cluster_bin_og))
}
cor_matrix_og

for (k in 1:n_clusters_pca) {
  cluster_bin_pca <- as.numeric(clusters_pca == k)
  cor_matrix_pca[, k] <- sapply(vars_pca, function(x) cor(x, cluster_bin_pca))
}
cor_matrix_pca

# This confirms what was seen in the violin plots:
# - For the original model, area and perimeter have the strongest influence.
# - For PCA, while those variables remain important, others like compactness (Cluster 4)
#   and fractal dimension (Cluster 3) also show high correlation.
