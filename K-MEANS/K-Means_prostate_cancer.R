# K-MEANS CLUSTERING OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Load necessary libraries
library(FactoMineR)
library(dplyr)
library(plotrix)
library(tidyr)
library(ggplot2)
library(factoextra)
library(reshape2)
library(patchwork)
library(plotly)
library(fields)
library(scico)


##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("C:/Users/carlos.firvida/OneDrive - Universidad de Deusto/Documentos/Estadistica Avanzada/Teamwork Proyect - Cancer/data/Prostate_Cancer.csv")

# We will approach to apply k-means to the original dataset and after applying PCA

# Remove non-numeric or ID variables
data <- data %>% select(-id)

## PCA ANALYSIS
data <- data[, sapply(data, is.numeric)] # Only the numeric data for the PCA
data_scaled <- scale(data) # PCA requires standardization if variables are on different scales

# Perform Principal Component Analysis
pca_result <- PCA(data_scaled)
plot.PCA(pca_result, choix = "var", axes = c(2, 3))

# We visualize the summary, to check every section of PCA
summary(pca_result)
pca_result$eig    # Only eigenvalues of components 1 and 2 are above 1 representing
                  # 63% of the data

pca_data <- pca_result$ind$coord[,1:2] # This stores the dataset after applying pca

# View the 6 first lines of each dataset
head(data_scaled)
head(pca_data)


#################
# K-MEANS (PCA) #
#################

# Elbow plot to determine the optimal number of clusters
fviz_nbclust(pca_data, kmeans, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 3, color = "Elbow Point"), linetype = "dashed", size = 1)

# The optimal number of clusters is 3 as is the point at which the graph forms an elbow

# Calculate number of clusters with the silhouette method: The result is also 3
fviz_nbclust(pca_data, kmeans, method = "silhouette", k.max = 20)

k <- 3
km.pca <- kmeans(pca_data, 
                   k, 
                   nstart = 10 # how many initial conditions are created
) 

# nstart parameter is an argument that specifies 
# the number of initial random cluster assignments to be tried. 
# It is used to find the best initial set of cluster centers 
# by repeatedly running the clustering process 
# with different initializations and selecting the one with 
# the lowest total within-cluster sum of squares (WCSS).
# 
# Note: The default value for nstart is 1, meaning the algorithm 
# is run only once with a single set of initial cluster centers. 
# We can increase nstart to a larger number (e.g., 10, 20, etc.) 
# to improve the chances of finding a better clustering solution.

par(mfrow = c(2, 1))
plot(pca_data, col = (km.pca$cluster + 1), ylim = c(-3,2),
     main = paste("K-Means Clustering Results with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)

# show again the observations as function of the PCs, using the clusters
# from k-means and showing the names of the states
par(mfrow = c(1, 1))
plot(pca_data, col = (km.pca$cluster + 1), ylim = c(-3,2),
     main = paste("K-Means Clustering Results with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)


#######################
# K-MEANS (BASE DATA) #
#######################

# Elbow plot to determine the optimal number of clusters
fviz_nbclust(data_scaled, kmeans, method = "wss", k.max = 20) +
  geom_vline(aes(xintercept = 3, color = "Elbow Point"), linetype = "dashed", size = 1)

# The optimal number of clusters is 3 as is the point at which the graph forms an elbow

# Calculate number of clusters with the silhouette method: The result is also 3
fviz_nbclust(data_scaled, kmeans, method = "silhouette", k.max = 20)

km.original <- kmeans(data_scaled, 
                 k, 
                 nstart = 10 # how many initial conditions are created
) 

# Eventhough we have succesfully calculated the number of clusters, we are not going to plot
# the k-means, because it doesn´t make sense to do it with 8 dimensions


###############
# ASSOCIATION #
###############

# We create a contingency table
cluster_comparison <- table(
  Original = km.original$cluster,
  PCA = km.pca$cluster
)

# We change the format of the table to print it
cluster_df <- melt(cluster_comparison)
names(cluster_df) <- c("Original", "PCA", "Common_points")

# We create a heatmap to see how related the clusters are
heatmap_plot <- ggplot(cluster_df, aes(x = PCA, y = Original, fill = Common_points)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  # 
  scale_x_continuous(breaks = seq(1, k, by = 1)) +
  scale_y_continuous(breaks = seq(1, k, by = 1)) +
  labs(
    title = "Comparison of clusters: Original Variables vs PCA",
    x = "Clusters based on PCA",
    y = "Clusters based on original variables",
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

# Although there is no perfect match between the clusters formed  
# using the original variables and those based on principal components,  
# there are consistent patterns that can be observed.  

# This indicates that PCA is able to partially preserve  
# the original clustering structure in the reduced space.  

# Therefore, PCA can be considered a valid approach in this case,  
# although it might not be the best option when high clustering precision  
# is strictly required.


##########################################
# K-MEANS VISUALIZATION & INTERPRETATION #
##########################################

## K-means visualization

# We will use the rainbow function as used in the "NCI60" script to assign a color to 
# each cluster
colors <- rainbow(15)

# We visualize the clusters in the case of PCA (as we can visualize it in 2D)
plot(pca_data, col = colors[km.pca$cluster],
     ylim = c(-3, 2),
     main = paste("K-Means Clustering Results with K =", k),
     xlab = "", ylab = "", pch = 20, cex = 2)

# As we are plotting the clusters in the case of the PCA model, and we want to intrepet 
# the results based on the original variables, we will recall the plot showing the 
# relationship between the origninal variables and the first two Principal Components
fviz_pca_var(pca_result)

## K-means interpretation

# Add cluster info to original data
clusters <- km.pca$cluster
data$cluster <- as.factor(clusters)

# Compute mean per cluster
data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

# reshape data
data_long <- data %>%
  pivot_longer(cols = -cluster, 
               names_to = "variable", 
               values_to = "value")

cluster_colors <- scico(3, palette = "lajolla")

# Violin plot, one column per variable, different colors for each cluster
ggplot(data_long, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +  # Adjust ncol to your liking
  scale_fill_manual(values = cluster_colors) +        
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

## We compute the correlation between the variables and each cluster

vars <- data[, sapply(data, is.numeric)]
n_clusters <- length(unique(clusters))
# Matrix to store correlations
cor_matrix <- matrix(NA, nrow = ncol(vars), ncol = n_clusters)
rownames(cor_matrix) <- colnames(vars)
colnames(cor_matrix) <- paste0("Cluster_", 1:n_clusters)

# Compute correlations
for (k in 1:n_clusters) {
  cluster_bin <- as.numeric(clusters == k)
  cor_matrix[, k] <- sapply(vars, function(x) cor(x, cluster_bin))
}
cor_matrix

# In this case there are some high correlations between some of the clusters and variables, 
# for instance in the case of cluster 3 and area and cluster 1 and symetry.
