# CLUSTERING (K-MEANS) OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# We load the libraries we will used to create the model:
library(FactoMineR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(reshape2)
library(scico)


###################################
# DATASET LOADING & CHECKING
###################################

data <- read.csv("./breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


###################################
# DATASET MODIFICATION
###################################

# We remove the variables that are not continuous:

#* Before removing it we will save the diagnosis in a variable, as it will be useful 
# at the end of the analysis
diagnosis <- as.factor(data$diagnosis) 

data <- data %>% select(-id, -diagnosis) # We remove

# We standardize the variables to have mean zero and standard deviation one.
data_scaled <- as.data.frame(scale(data))


###################################
# APPROACH SELECTION
###################################

# In this case, we have decided to study the two approaches proposed in the guidelines:

    # Apply K-means to the components selected during PCA. As we found during the PCA 
    # analysis, the model with just the 'mean' variables is more than enough to get 
    # most of the information from the data, so that is the one we have selected. 

    # Apply K-means directly to the original variables. In this case, we will also use
    # the mean variables, leaving out the ones that have been proved to be redundant or 
    # not so meaningful.


# We select the variables that measure the means (this data will be used in both approaches,
# the one for PCA and the one for the original variables).
mean_vars <- grep("_mean$", names(data_scaled), value = TRUE)
data_mean <- data_scaled[, mean_vars]
data_mean <- cbind(data_mean, diagnosis)

# We also divide the benign and malign diagnosis
data_mean_benign <- data_mean %>% filter(diagnosis == "B")
data_mean_malign <- data_mean %>% filter(diagnosis == "M")

# We remove the diagnosis as it is a categorical variable
data_mean <- data_mean %>% select(-diagnosis) # We remove
data_mean_benign <- data_mean_benign %>% select(-diagnosis) # We remove
data_mean_malign <- data_mean_malign %>% select(-diagnosis) # We remove

# We compute the PCA
pca_mean <- PCA(data_mean, scale.unit = TRUE) 
data_pca <- pca_mean$ind$coord[,1:2] # After checking different number of components,
# we have seen that 2 components are enough, so we keep them.


###################################
# K-MEANS APPLICATION
###################################

###################################
#### Selecting K

# First of all, we will decide the number of clusters. We know that there are two different
# groups in our data, the ones which are malign tumors and the ones that are benign.
# However, we will create an elbow plot to select the appropiate number of clusters.

# We set the seed for reproducibility
set.seed(123456)

# We create the data frames where we will store the results
result_mean = data.frame()
result_pca = data.frame()
result_mean_benign = data.frame()
result_mean_malign = data.frame()

# We run kmeans with 50 different k so that we can visualize the elbow plot and select
# an appropiate number of clusters (we do this for all the cases)
for (i in 1:50){
  # For the mean 'model'
  fit_mean = kmeans(data_mean, i,nstart = 10)
  result_mean = rbind(result_mean,data.frame(k=i,withinss=fit_mean$tot.withinss))
  
  # For the benign 'model'
  fit_mean_benign = kmeans(data_mean_benign, i,nstart = 10)
  result_mean_benign = rbind(result_mean_benign,data.frame(k=i,withinss=fit_mean_benign$tot.withinss))
  
  # For the malign 'model'
  fit_mean_malign = kmeans(data_mean_malign, i,nstart = 10)
  result_mean_malign = rbind(result_mean_malign,data.frame(k=i,withinss=fit_mean_malign$tot.withinss))
  
  # For the PCA 'model'
  fit_pca = kmeans(data_pca, i,nstart = 10)
  result_pca = rbind(result_pca,data.frame(k=i,withinss=fit_pca$tot.withinss))
}

# We plot the elbow plot for all the variables
ggplot(data=result_mean,aes(x=k,y=withinss))+
  geom_line()+
  geom_point()

# We plot the elbow plot for all the benign variables
ggplot(data=result_mean_benign,aes(x=k,y=withinss))+
  geom_line()+
  geom_point()

# We plot the elbow plot for all the malign variables
ggplot(data=result_mean_malign,aes(x=k,y=withinss))+
  geom_line()+
  geom_point()

# We plot the elbow plot for PCA
ggplot(data=result_pca,aes(x=k,y=withinss))+
  geom_line()+
  geom_point()

# As we have seen that the elbow plot is very similar in the cases of the mean model and
# the benign & malign ones, we will continues by keeping just the mean one

# We apply the silhouette method to all the 'models'
fviz_nbclust(data_mean, hcut, method = "silhouette", k.max = 100)
fviz_nbclust(data_pca, hcut, method = "silhouette", k.max = 100)

# The silhouette method shows a result that is not alligned with the elbow plot. In this 
# case, we will keep what the elbow plot says: even though elbow plots are not completely 
# clear, we believe that in both cases the number of cluster could be around 15. As we
# want to compare the two models (all variables and PCA), we will select K = 15.


###################################
#### Running K-means

# The chosen number of clusters
k <- 15

# We apply K-means to the mean model
km_mean <- kmeans(data_mean, k, nstart = 10) 
clusters_mean <- km_mean$cluster

# We apply K-means to the PCA model
km_pca <- kmeans(data_pca, k, nstart = 10)
clusters_pca <- km_pca$cluster

###################################
#### Association of clusters

# We create a contingency table
cluster_comparison <- table(
  Original = clusters_mean,
  PCA = clusters_pca
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

# In the heatmap we can see how the clusters from the 'mean' model and from the PCA model
# are related. This is what we were expecting: to see, more or less clearly, how the different 
# clusters of each model were associated. Although in some cases most points of a cluster 
# are distributed across several clusters of the other model, in most clusters the relationship
# between a cluster of the mean model and another one (or two) of the PCA model are clear.
# The most clear patterns/relations we have been able to identify are the following:

    # Cluster 15 of the mean model is most similar to cluster 8.
    # Cluster 12 of the mean model is  mostly a combination of 1 and 12.
    # Cluster 10 of the mean model is mostly a combination of 2 and 14.
    # Cluster 8 of the mean model is most similar to cluster 3.
    # Cluster 7 of the mean model is most similar to cluster 1.
    # Cluster 6 of the mean model is EXACTLY cluster 5.
    # Cluster 5 of the mean model is  mostly a combination of 11 and 13.
    # Cluster 4 of the mean model is most similar to cluster 2.
    # Cluster 3 of the mean model is EXACTLY cluster 4.
    # Cluster 2 of the mean model is most similar to cluster 6.
    # Cluster 1 of the mean model is  mostly a combination of 10 and 15.

    # Clusters 9, 11, 13, 14 of the mean model are more spread across multiple clusters of the
    # PCA model.

# This means that there is a clear correspondence between the clustering of the mean 
# and the PCA models. 
# As we know that PCA significantly alters the structure of the data, and taking into account 
# that we are reducing the dimensionality of the data from 10 to just 2, the fact that most 
# relationships can still be detected are good news, meaning we are still keeping somehow the 
# original structure.


###################################
# K-MEANS VISUALIZATION & INTERPRETATION
###################################

###################################
#### K-means visualization

# We will use the rainbow function as used in the "NCI60" script to assign a color to 
# each cluster
colors <- rainbow(15)

# We visualize the clusters in the case of PCA (as we can visualize it in 2D)
x_range <- range(data_pca[, 1])
y_range <- range(data_pca[, 2])

# We to zoom out
padding_x <- diff(x_range) * 0.05
padding_y <- diff(y_range) * 0.05

plot(data_pca, 
     col = colors[km_pca$cluster],
     xlim = c(x_range[1] - padding_x, x_range[2] + padding_x),
     ylim = c(y_range[1] - padding_y, y_range[2] + padding_y),
     main = paste("K-Means Clustering Results with K =", k),
     xlab = "", ylab = "", 
     pch = 20, cex = 2)

# As we are plotting the clusters in the case of the PCA model, and we want to intrepet 
# the results based on the original variables, we will recall the plot showing the 
# relationship between the origninal variables and the first two Principal Components
fviz_pca_var(pca_mean)


###################################
#### K-means interpretation

# We add cluster info to original data. We use the pca model as we want to use the plot of the
# clusters to understand the relationships between the original variables and the clusters.
data <- data[, mean_vars]
data$cluster <- as.factor(clusters_pca)

# We remove some variables that don't add extra information for an easier interpretation
data <- data %>% select(-c("area_mean", "perimeter_mean"))

# We compute mean per cluster
mean_of_clusters <- data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

# reshape data
data_long <- data %>%
  pivot_longer(cols = -cluster, 
               names_to = "variable", 
               values_to = "value")

# we select the colors for the clusters
cluster_colors <- scico(15, palette = "lajolla")

# Violin plot, one column per variable, different colors for each cluster
ggplot(data_long, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +  # Adjust ncol to your liking
  scale_fill_manual(values = cluster_colors) +        # Apply color palette
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Variable Distribution per Cluster") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# To extract the final conclusions, we will use the plot of the clusters, the pca plot, the mean
# of each variable per cluster and the violin plot. We visualize again everything: 

# Plot of clusters:
x_range <- range(data_pca[, 1])
y_range <- range(data_pca[, 2])

# We to zoom out
padding_x <- diff(x_range) * 0.05
padding_y <- diff(y_range) * 0.05

plot(data_pca, 
     col = colors[km_pca$cluster],
     xlim = c(x_range[1] - padding_x, x_range[2] + padding_x),
     ylim = c(y_range[1] - padding_y, y_range[2] + padding_y),
     main = paste("K-Means Clustering Results with K =", k),
     xlab = "", ylab = "", 
     pch = 20, cex = 2)

# PCA plot:
fviz_pca_var(pca_mean)

# Mean of variables per cluster (to print all columns):
print(mean_of_clusters, width = Inf)


# Now, we will try to identify the clusters in the plot (we will mention the most representative 
# clusters):
    
    # Cluster 4 is probably the yellowish green cluster on the bottom right corner. We know this
    # because it has the highest radius mean values (which we know that are "represented" in that
    # direction of PCA) and a relatively low value for fractal dimension.

    # On the contrary, cluster 7 is probably the dark blue one on the top middle left part, as it has
    # the smallest value for the radius, and significantly low values for texture and concavity.

    # Cluster 1 is probably the red cluster on top and in the middle. This is beacause its
    # fractal dimension is the highest one, meaning that it is exactly in the direction of the 
    # fractal dimension in the PCA plot.

    # Finally, we could say that cluster 8 is the turquoise blue on the bottom left corner. We
    # believe this because it has the smallest values in both smoothness and symmetry.

    # The remaining clusters are very closed in the center of the PCA dimension, being difficult
    # to distinguish based on our data.


# Finally, we compute the correlation between the variables and each cluster

vars <- data[, sapply(data, is.numeric)]
n_clusters <- length(unique(clusters_mean))

# Matrix to store correlations
cor_matrix <- matrix(NA, nrow = ncol(vars), ncol = n_clusters)
rownames(cor_matrix) <- colnames(vars)
colnames(cor_matrix) <- paste0("Cluster_", 1:n_clusters)

# Compute correlations
for (k in 1:n_clusters) {
  cluster_bin <- as.numeric(clusters_mean == k)
  cor_matrix[, k] <- sapply(vars, function(x) cor(x, cluster_bin))
}
cor_matrix

# In this case there are not very high correlations between any of the clusters and variables. The 
# highest correlations observed have been around 0.4-0.5, for instance in the case of cluster 7 and
# fractal dimension or in the case of cluster 6 and compactness. This is clearly what we have been
# able to see in the violin plots, where cluster 7 was the one having the highest values for fractal
# dimension and cluster 6 the one having the highest values for compactness.

# Finally, we will  check how the different diagnosis are spread across the clusters
data$diagnosis <- as.factor(diagnosis)

x_range <- range(data_pca[, 1])
y_range <- range(data_pca[, 2])

# We to zoom out
padding_x <- diff(x_range) * 0.05
padding_y <- diff(y_range) * 0.05

plot(data_pca, 
     col = colors[km_pca$cluster],
     xlim = c(x_range[1] - padding_x, x_range[2] + padding_x),
     ylim = c(y_range[1] - padding_y, y_range[2] + padding_y),
     main = paste("K-Means Clustering Results with K =", k),
     xlab = "", ylab = "", 
     pch = 20, cex = 2)


# Add diagnosis labels on top of each point
text(data_pca[, 1], data_pca[, 2], labels = data$diagnosis, cex = 0.7, pos = 3)

# Interpretation:

    # There are 5 different clusters for malign tumors, the ones positioned on the right side. This
    # could mean that, within the malign tumors there are 5 subgroups.

    # Then, there are 6 different clusters for benign tumors, the ones positioned on the left side. 
    # This could mean that, within the benign tumors there are 5 subgroups.

    # Finally, there are 4 different clusters where benign and malign tumors are mixed. 
