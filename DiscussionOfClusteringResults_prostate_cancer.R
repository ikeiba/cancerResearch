# DISCUSSION OF CLUSTERING RESULTS OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("C:/Users/carlos.firvida/OneDrive - Universidad de Deusto/Documentos/Estadistica Avanzada/Teamwork Proyect - Cancer/data/Prostate_Cancer.csv")

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
# 63% of the data, but component 3 is near with 0.98

pca_data <- pca_result$ind$coord[,1:2] # This stores the dataset after applying


#################
# K-MEANS (PCA) #
#################

k <- 3
km.pca <- kmeans(pca_data, 
                 k, 
                 nstart = 10 # how many initial conditions are created
)


#######################################
# HIERARCHICAL AGGLOMERATIVE ALGORITH #
#######################################

# We first have to compute a Distance Matrix, where
# we can choose among different distances, e.g. euclidean, manhattan:
distance_matrix_pca <- dist(pca_data, method = "euclidean")
distance_matrix_pca

# Then, we apply the hclust function choosing the method (complete/single)
hc.pca <-  hclust(distance_matrix_pca, method = "complete")
hc.pca

hc.clusters <- cutree(hc.pca , k = 3)

##############
# COMPARISON #
##############

# We claimed that K-means clustering and hierarchical
# clustering with the dendrogram cut to obtain the same number
# of clusters can yield very different results. How do these hierarchical
# clustering results compare to what we get if we perform K-means clustering
# with K = 3?

set.seed(2)
table(km.pca$cluster , hc.clusters)

# We see that the four clusters obtained using hierarchical clustering and Kmeans
# clustering are somewhat different. Cluster 1 in K-means clustering is
# identical to cluster 2 in hierarchical clustering. However, the other clusters
# differ: for instance, cluster 2 in K-means clustering contains one observation
# assigned to cluster 1 by hierarchical clustering, as well as 26 of all the
# observations assigned to cluster 2 by hierarchical clustering.