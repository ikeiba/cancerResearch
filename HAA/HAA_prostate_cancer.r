# K-MEANS CLUSTERING OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Load necessary libraries
library(FactoMineR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(reshape2)
library(scico)

##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("./Prostate_Cancer.csv")

# We will approach to apply k-means to the original dataset and after applying PCA

# Remove non-numeric or ID variables
data <- data %>% select(-id)
data

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

##############################################
# Algorithm Application for Original and PCA #
##############################################

## Hierarchical clustering ##

# Elbow plot for hierarchical clustering
fviz_nbclust(pca_data, hcut, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 4, color = "Elbow Point"), linetype = "dashed", size = 1)

fviz_nbclust(data, hcut, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 2, color = "Elbow Point"), linetype = "dashed", size = 1)

# In case of dubious results, change the method to "silhouette"
fviz_nbclust(pca_data, hcut, method = "silhouette", k.max = 20)
fviz_nbclust(data, hcut, method = "silhouette", k.max = 20)

# We first have to compute a Distance Matrix, where
# we can choose among different distances, e.g. euclidean, manhattan:
distance_matrix_pca <- dist(pca_data, method = "euclidean")
distance_matrix_pca

distance_matrix_og <- dist(data, method = "euclidean")
distance_matrix_og

# Then, we apply the hclust function choosing the method (complete/single)
hc_pca <-  hclust(distance_matrix_pca, method = "complete")
hc_pca

hc_og <-  hclust(distance_matrix_og, method = "complete")
hc_og

# We can plot the dendrogram
plot(hc_pca)
rect.hclust(hc_pca, k = 4, border = 2:10) #We draw rectangles for the clusters

plot(hc_og)
rect.hclust(hc_og, k = 2, border = 2:10) #We draw rectangles for the clusters

# To determine the cluster for each observation associated 
# with a given cut of the dendrogram, we can use the cutree() function.
# If we want to cut horizontally:
cutree(hc_pca, h=5)  # setting the height
cutree(hc_og, h=5)  # setting the height

# If we want to cut vertically:
clusters_pca <- cutree(hc_pca, k=4)  # setting the number of clusters
clusters_og <- cutree(hc_og, k=2)  # setting the number of clusters

###########################
# Association of clusters #
###########################

# We create a contingency table
cluster_comparison <- table(
  Original = clusters_pca,
  PCA = clusters_og
)

# We change the format of the table to print it
cluster_df <- melt(cluster_comparison)
names(cluster_df) <- c("Original", "PCA", "Common_points")

# We create a heatmap to see how related the clusters are
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
  # We add the exact values to the tiles
  geom_text(aes(label = Common_points), color = "black")

# We visualize the heatmap
print(heatmap_plot)

# In the heat map we can see how the clusters from the original model and from the PCA model
# are related. To be honest, this is not what we were expecting. We expected to see, more
# or less clearly, how the different clusters of each model where associated (something
# like "cluster x from the original model represents clearly cluster y from the PCA model").
# However, considering that the amount of clusters is different, we cannot see a pattern that would
# indicate that the extra 2 clusters in the PCA model are unnecesary, we can see that:
# Clusters 1 and 2 of the PCA model fall within cluster 1 of the original model.
# Clusters 3 and 4 of the PCA model do not fall within any of the clusters of the original model.

# This might indicate that despite our original results being only 2 possible results, there exists a
# possible hidden difference between them.

##########################
# Cluster Interpretation #
##########################

# Add cluster info to original data
data_pca <- as.data.frame(data_pca)
data_pca$cluster <- as.factor(clusters_pca)

data$cluster <- as.factor(clusters_og)

# Compute mean per cluster
data_pca %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

# Reshape data
data_long_pca <- data_pca %>%
  pivot_longer(cols = -cluster, 
               names_to = "variable", 
               values_to = "value")

data_long_og <- data %>%
  pivot_longer(cols = -cluster, 
               names_to = "variable", 
               values_to = "value")

cluster_colors <- scico(4, palette = "lajolla")

# Violin plot, one column per variable, different colors for each cluster
ggplot(data_long_pca, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +  # Adjust ncol to your liking
  scale_fill_manual(values = cluster_colors) +        # Apply color palette
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggplot(data_long_og, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +  # Adjust ncol to your liking
  scale_fill_manual(values = cluster_colors) +        # Apply color palette
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# As we can observe in the Violin plots, in the case of the original model, the major
# distinctions between the clusters occur between the area and the perimeter, whereas in the 
# PCA model in the case of the area and perimeter clusters 1 and 2 are higher than 3 and 4,
# in the case of compactness, fractal_dimension, smoothness and symmetry clusters 1 and 3 
# are higher than 2 and 4 and in the case of radius and texture there is basically no difference
# between the clusters.

## We compute the correlation between the variables and each cluster
vars_og <- data[, sapply(data, is.numeric)]
n_clusters_og <- length(unique(clusters_og))

vars_pca <- data_pca[, sapply(data_pca, is.numeric)]
n_clusters_pca <- length(unique(clusters_pca))

# Matrix to store correlations
cor_matrix_og <- matrix(NA, nrow = ncol(vars_og), ncol = n_clusters_og)
rownames(cor_matrix_og) <- colnames(vars_og)
colnames(cor_matrix_og) <- paste0("Cluster_", 1:n_clusters_og)

cor_matrix_pca <- matrix(NA, nrow = ncol(vars_pca), ncol = n_clusters_pca)
rownames(cor_matrix_pca) <- colnames(vars_pca)
colnames(cor_matrix_pca) <- paste0("Cluster_", 1:n_clusters_pca)

# Compute correlations
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

# Just like we observed in our Violin Plots, the biggest differences in the original model
# occur in the area and perimeter, whereas in the PCA model, we can also see those
# differences, but they are not as prevalent, the highest ones that we can see with a 0.6
# correlation or higher, are compactness and cluster 4 and fractal_dimension and cluster 3.
