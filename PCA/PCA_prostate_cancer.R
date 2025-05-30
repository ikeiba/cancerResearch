# PRINCIPAL COMPONENT ANALYSIS (PCA) OF THE PROSTATE CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Load necessary libraries
library(FactoMineR)
library(plotrix)
library(dplyr)
library(factoextra)
library(patchwork)
library(plotly)
library(fields)  # For image.plot


##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("./Prostate_Cancer.csv")

# View the structure of the dataset
str(data)

#* Before removing it we will save the diagnosis in a variable, as it will be useful 
# at the end of the analysis
diagnosis <- as.factor(data$diagnosis)

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
pca_result <- PCA(data_scaled)

# We visualize the summary, to check every section of PCA
summary(pca_result)

# We visualize the scree plot
scree_all <- fviz_eig(pca_result, main = "All Variables", addlabels = TRUE, ylim = c(0, 100)) # Create plot for all

# We check the eigenvalues as well (we take into account that if the eigenvalue is lower
# than 1, is not "improving" a original variable, so it will probably not be worth 
# including it).
pca_result$eig # Only eigenvalues of components 1 and 2 are above 1 representing 63% of the data

# In this case, as we can see at the 'elbow' in the 
# plot we can see that it is at dimesions 3. This is beacause percentage of explained 
# variance drops by a half from component 2 to component 3, and the curve starts (slowly) 
# to flatten out afterwards. 
# In this case we will consider the first two components, as there is a significant 
# difference between the percentage of variance explained by them and the percentage 
# explained by the following components. The first two components sum up to explain 
# approximately 63% of the variance of the data.


####################################
# PCA: CORRELATIONS WITH VARIABLES #
####################################

# We check the correlations of the original variables with the selected components:

pca_result$var$coord[ , 1:2] # The first component shows stronger correlations with more variables 
# compared to the second component, however, the difference between the two components is not very 
# pronounced. This is not surprising, given that the first component accounts for 37% of the
# total variance, whereas the second explains 26% resulting in a difference of only 11%.

# High correlations (|correlation| > 0.8):

# Component 1: compactness, but perimeter and symmetry are near (both 0.71 approximately)

# Component 2: although there is no parameter in this component with a cor > 0.8, the area
# and the fractal_dimension are near (-0.74 and 0.78 respectively)


##################################
# PCA: CORRELATION VISUALITATION #
##################################

# We can visualize the correlation with the following plot:
var_result <- plot(pca_result,choix="var")

# Graph analysis detailed in the main document


################################
# PCA: INDIVIDUALS & VARIABLES #
################################


## Individual Coordinates

# We start by checking the individual coordinates in the first two dimensions (this is,
# taking into account the first two components)
pca_result$ind$coord[ , 1:2] 

# As it is hard to interpret this data just by looking at the 'raw' numbers, we plot
# the individuals
ind_result <- plot(pca_result,choix="ind")

## Duality

# Duality is for checking the relation between a variable and a specific individual:

# When an individual point is located in the direction of a variable arrow, 
# the individual has a high value on that variable. If it is in the opposite direction,
# it will have a low value. 

# The distance is also important: the farther along the line an individual is, 
# the larger their value for that variable and viceversa.


# For the full model:

# Create the scatter plot with vectors
plot(pca_result$var$coord, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (37.04%)", ylab = "Component 2 (25.89%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_result$var$coord[, 1], y1 = pca_result$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(pca_result$var$coord[, 1] + 0.1, pca_result$var$coord[, 2] + 0.1, 
     labels = row.names(pca_result$var$coord), cex = 0.6)

# Overlay the scatter plot of individual data points
points(pca_result$ind$coord[, 1], pca_result$ind$coord[, 2], pch = 16, col = "blue")
text(pca_result$ind$coord[, 1], pca_result$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "PCA Analysis: Individuals and Vectors")

# With this plot we can see how each individual is related to different variables.
# However, the amount of individuals and variables (mostly in the case of the full
# model) makes an interpretation hard. To make things easier, we will visualize
# a plot that shows that the previously mentioned 'principles' of duality are fulfilled:

# The plot colors all the points that are above the mean of a variable, to prove that
# the direction of a variable’s arrow is related to an individual's value for that 
# variable.

# Select the variable
variable_to_check <- data$compactness # select whatever variable you want

# Calculate the mean of the variable
mean_fractal <- mean(variable_to_check)

# Check if the values are above or below the mean
above_mean <- variable_to_check > mean_fractal

# Create the scatter plot with vectors
plot(pca_result$var$coord, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (37.04%)", ylab = "Component 2 (25.89%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_result$var$coord[, 1], y1 = pca_result$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(pca_result$var$coord[, 1] + 0.1, pca_result$var$coord[, 2] + 0.1, 
     labels = row.names(pca_result$var$coord), cex = 0.6)

# Change the color depending on the value of the radius mean (above or below mean)
points(pca_result$ind$coord[, 1], pca_result$ind$coord[, 2], pch = 16, 
       col = ifelse(above_mean, "red", "blue"))  # Red for above mean, blue for below mean
text(pca_result$ind$coord[, 1], pca_result$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "PCA Analysis: importance of direction")
legend("topright", legend = c("Above mean compactness", "Below mean compactness"),
       col = c("red", "blue"),          
       title = "Compactness Category")


## Individual Contribution

# We can aslo check each individual's contribution (in %) to each principle component
pca_result$ind$contrib[ , 1:2]

# As the contribution measures how much an individual 'influences' the component, the 
# contributions that are farther from the center will have a higher contribution, while
# those closer to the center will have a lower contribution.

# We create a plot to visually show that this happens in our case:

# We get the individual coordinates 
coord <- pca_result$ind$coord

# We extract the contribution for the selected component
contrib <- pca_result$ind$contrib[, 1]  # Choose component 1 or 2

# We create color scale (from light yellow to dark red)
col_scale <- colorRampPalette(c("lightyellow", "red"))

# We scale the contributions (as they are already %, we just have to divide by 100)
point_colors <- col_scale(100)[as.numeric(cut(contrib / 100, breaks = 100))]

# As previously, we set the plot limits
pca_x_min <- min(coord[, 1]) - 1
pca_x_max <- max(coord[, 1]) + 1
pca_y_min <- min(coord[, 2]) - 1
pca_y_max <- max(coord[, 2]) + 1

# We plot the variable vectors
plot(pca_result$var$coord, xlim = c(pca_x_min, pca_x_max), 
     ylim = c(pca_y_min, pca_y_max), pch = 16, cex = 0.3,
     xlab = "Component 1 (37.04%)", ylab = "Component 2 (25.89%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_result$var$coord[, 1], y1 = pca_result$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)

# We plot individuals with gradient colors based on their contribution to PC1
points(coord[, 1], coord[, 2], pch = 16, col = point_colors)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Means' PCA Analysis: Contribution to selected PC")

# Now, we are going to add a label to visualize the top and bottom contributors:

# Get row indices of top 6 and bottom 6 contributors
top6 <- order(contrib, decreasing = TRUE)[1:6]
bottom6 <- order(contrib, decreasing = FALSE)[1:6]

# Coordinates for labeling
coord <- pca_result$ind$coord
top_coords <- coord[top6, ]
bottom_coords <- coord[bottom6, ]

# Add text labels in red for top contributors
text(top_coords[, 1], top_coords[, 2], 
     labels = rownames(coord)[top6], 
     pos = 3, cex = 0.6, col = "red")

# Add text labels in blue for bottom contributors
text(bottom_coords[, 1], bottom_coords[, 2], 
     labels = rownames(coord)[bottom6], 
     pos = 3, cex = 0.6, col = "blue")

# We can visualize the exact value of the individuals that contribute more to each 
# component:

# Component 1:
head(sort(pca_result$ind$contrib[ , 1], decreasing = T))

# Component 2:
head(sort(pca_result$ind$contrib[ , 2], decreasing = T))

# We can visualize as well the exact value of the individuals that contribute less to each 
# component:

# Component 1:
head(sort(pca_result$ind$contrib[ , 1]))

# Component 2:
head(sort(pca_result$ind$contrib[ , 2]))

# We can also check the individuals whose contribution is larger than the average 
# contribution (observations whose contribution is larger than 1/I ).

# We get the number of individuals
ind_count <- dim(pca_result$ind$contrib)[1]

# We calculate the 'thoretical' average contribution
avg_contrib <- 1 / ind_count * 100  

# We get individuals with contribution to PC1 that are higher than the average
high_contrib_ind <- which(pca_result$ind$contrib[, 1] > avg_contrib) # Select 1 or 2 for the
# desired component

# Visualize the individuals with higher contribution than the average (sorted)
sort(pca_result$ind$contrib[high_contrib_ind, 1], decreasing = T)


# Finally, we verify that each principle component's contributions sum up to 100%:
colSums(pca_result$ind$contrib[ , 1:2])


## Outliers

# In this dataset, the data was not standarized, that why
# we used the scale() function now we have to slightly modify the approach

# Calculate the mean of each variable in the original data
center <- colMeans(data_pca)

# Compute the Euclidean distance of each individual from the center. We have to substract 
# the center of each variable, and as the data is not standarized this is the mean of each
# variable (with standarized data that will be 0)
distances <- sqrt(apply(data_pca, MARGIN = 1, FUN = function(x) sum((x - center)^2)))

# We compute the mean and standard deviation of the distances
mean_dist <- mean(distances)
sd_dist <- sd(distances)

# We set a threshold for outliers (e.g., 2 standard deviations away from the mean)
threshold <- mean_dist + 2 * sd_dist

# We find the outliers based on our treshold (obviously a different criterion would
# detect different outliers)
outliers <- which(distances > threshold)

# Visualize the outliers 
outliers


###########################################
# PCA: PRINCIPAL COMPONENTS AND DIAGNOSIS #
###########################################

# Finally, we can't forget that we are analysing a dataset related to cancer, so we will
# check if the first two components have somehow separate cancer diagnosis (benign 
# and malign):

# Plot PCA individuals colored by diagnosis
fviz_pca_ind(pca_result, 
             col.ind = diagnosis, 
             palette = c("blue", "red"),  # Malignant=red, Benign=blue
             legend.title = "Diagnosis",
             title = "PCA: Individuals Colored by Diagnosis") +
  theme_minimal()

# While Component 2 does not significantly separate benign and malignant
# tumors, Component 1 shows a clear separation: benign cases are associated
# with negative PC1 scores, while malignant cases are associated with positive
# PC1 scores. This indicates that the variables most correlated with Component 1
# are crucial for distinguishing tumor types.


# We will confirm this by getting the mean values of the coordinates by diagnosis:

# We create a dataframe with coordinates and diagnosis
pca_data <- data.frame(
  PC1 = pca_result$ind$coord[, 1],
  PC2 = pca_result$ind$coord[, 2],
  Diagnosis = diagnosis
)

# We filter the dataset to get separate variables
benign_coords <- pca_data %>% filter(Diagnosis == "B")
malign_coords <- pca_data %>% filter(Diagnosis == "M")

# We get the mean values for the first component
mean(benign_coords$PC1)
mean(malign_coords$PC1)

# We get the mean values for the second component
mean(benign_coords$PC2)
mean(malign_coords$PC2)

# As we can see, the difference is much greater in the first component.


# We can even check this difference with a violin plot:

# Violin plot for first component
ggplot(pca_data, aes(x = diagnosis, y = PC1, fill = diagnosis)) + 
  geom_violin() + 
  ggtitle("Distribution of PC1 by Diagnosis")

# Violin plot for second component
ggplot(pca_data, aes(x = diagnosis, y = PC2, fill = diagnosis)) + 
  geom_violin() + 
  ggtitle("Distribution of PC2 by Diagnosis")

# The difference is clear in this case as well


# Finally, to confirm this using an statistical test, we will perform an ANOVA test using 
# the function aov() (not anova()), to see whether there is a significant difference in 
# the variation within the groups and between the groups.

# Fit the ANOVA model with the first component
model_aov <- aov(PC1 ~ diagnosis, data = pca_data)

# Check overall significance
anova_result <- summary(model_aov)
p_value <- anova_result[[1]]$"Pr(>F)"[1]
p_value # The p-value is 6.151029e-09, confirming that the is a significant difference 
# between the PC1 for each diagnosis group.

# Fit the ANOVA model with the second component
model_aov2 <- aov(PC2 ~ diagnosis, data = pca_data)

# Check overall significance
anova_result2 <- summary(model_aov2)
p_value2 <- anova_result2[[1]]$"Pr(>F)"[1]
p_value2 # The p-value is 0.004426355, confirming that the is a significant difference 
# between the PC2 for each diagnosis group.

# Although both statistical tests confirm that there is a difference between the
# diagnosis group in both PC1 and PC2, the p-values show that in the case of PC1 this
# difference is much bigger.
