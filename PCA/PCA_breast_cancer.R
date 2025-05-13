# PRINCIPAL COMPONENT ANALYSIS (PCA) OF THE WISCONSIN BREAST CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante


# We load the libraries we will used to create the model:
#install.packages("FactoMineR")
#install.packages("plotrix")
#install.packages("factoextra")
#install.packages("patchwork")
#install.packages("fieldss")
library(FactoMineR)
library(plotrix)
library(dplyr)
library(factoextra)
library(patchwork)
library(plotly)
library(fields)  # For image.plot


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

data <- data %>% select(-id, -diagnosis) # We remove & diganosis


###################################
# PCA: VARIABLE SELECTION
###################################

# Although our dataset has 30 continuous variables, they come from 10 different
# measurements, each having been measured in three different ways (mean, se & worst). 
# In that situation, we had the doubt whether to use all the variables or just a variable 
# for each measurement.

# After consulting with the professor, we have reached the conclusion that both approaches
# have their own positive and negative aspects: 

    # Using just a variable per measurement can lead to an easier interpretation of the 
    # results, as well as reducing noise introduced by variables that are extremely similar
    # or very highly correlated.
    
    # Using all the variables might be closer to a real-world scenario where we would use 
    # PCA, in which we have a higher number of variables and we want to reduce it.

# So, to see the difference between each approach, we will apply PCA with both cases (using
# just the variables measuring the mean and with all the variables)


# We select the variables that measure the means
mean_vars <- grep("_mean$", names(data), value = TRUE)
data_mean <- data[, mean_vars]

# We check that both data frames are correct:
head(data)
head(data_mean)


###################################
# PCA: COMPONENT SELECTION
###################################

# We apply the PCA using the PCA function
pca_all <- PCA(data, scale.unit = TRUE)
pca_mean <- PCA(data_mean, scale.unit = TRUE) 

# We visualize the summary, to check every section of PCA
summary(pca_all)
summary(pca_mean)


# We check the eigenvalues as well (we take into account that if the eigenvalue is lower
# than 1, is not "improving" a original variable, so it will probably not be worth 
# including it).
pca_all$eig # From Comp1 to Comp6 the eigenvalues are above 1
pca_mean$eig # Just in Comp1 and Comp 2 the eigenvalues are above 1 


# We visualize the scree plots
scree_all <- fviz_eig(pca_all, main = "All Variables", addlabels = TRUE, ylim = c(0, 100)) # Create plot for all
scree_mean <- fviz_eig(pca_mean, main = "'Mean' Variables", addlabels = TRUE, ylim = c(0, 100)) #Create plot for mean

scree_all + scree_mean # Visualize them side by side


# In the first case (the one with all the variables), when we look for the 'elbow' in the 
# plot we can see that it is at dimesions 3. This is beacause percentage of explained 
# variance drops by more than a half from component 2 to component 3, and the curve starts 
# to flatten out afterwards. 
# In this case we will consider the first two components, as there is a significant 
# difference between the percentage of variance explained by them and the percentage 
# explained by the following components. The first two components sum up to explain 
# approximately 63% of the variance of the data.
# Although this number could seem small, we have to take into account that we are reducing 
# the amount of variables from 30 to JUST 2, AND STILL KEEPING 63% OF THE TOTAL VARIANCE.


# In the second case (the one with just the 'means'), when we look for the 'elbow' in the 
# plot we can see that it is at dimesions 3. In this case, it has been easier to detect
# as the percentage of explained variance drops by more than a third from component 2 
# to component 3, and the curve considerably flattens out afterwards. 
# In this case we will just consider the first two components, as they sum up to explain 
# approximately 80% of the variance of the data.


# COMPARISON: Just by looking at the percentage of variance explained by each component
# in the two distinct 'models' (full and means) we can see that there are some clear  
# differences: 

    # The 'main' components (first & second) of the 'means' model explain more variance
    # than the ones of the full model. 

    # To explain the same percentage of variance, we have to consider more components
    # from the full model than from the 'means' model.

# Does this make sense? YES:

    # As the full model is considering more variables, the explained variance spreads out
    # over more components, leading to a smaller percentage of variance explained by the
    # first components.

    # The 'means' model is more compact as it doesn't add as much noise or redundancy as
    # the full model does (we already knew this, as in the full model we where including
    # some variables that were very highly correlated).


###################################
# PCA: CORRELATIONS WITH VARIABLES
###################################

# We check the correlations of the original variables with the selected components:

# In the case of the full model, we obtain the following results:
pca_all$var$coord[ , 1:2] # We can see that the first component has high correlations
# with more variables than the second component does (this is something we could have  
# expected since the first component explains 44% of the total variance while the 
# second one just explains 19%). 

# High correlations (|correlation| > 0.8):

    # Component 1: perimeter_mean, area_mean, compactness_mean, concavity_mean, 
    # concave.points_mean, perimeter_worst, area_worst, radius_worst, concavity_worst, 
    # concave.points_worst (10 variables, 33.33% of the total variables, but in most 
    # cases it includes different statistical summaries of the same measurement).

    # Component 2: fractal_dimension_mean (just 1 variable, 3.33% of the total variables)    


# In the case of the 'means' model, we obtain the following results:
pca_mean$var$coord[ , 1:2] # We can see that, as it happened with the full model, the 
# first component has high correlations with more variables than the second component 
# does (this is again something we could have expected since the first component explains 
# 55% of the total variance while the second one just explains 25%). 

# High correlations (|correlation| > 0.8): 

    # Component 1: radius_mean, perimeter_mean, area_mean, compactness_mean, 
    # concavity_mean, concave.points_mean (6 variables, 60% of the total variables, in
    # this case it is highly correlated with variables that are correlated between them, 
    # which makes sense).

    # Component 2: fractal_dimension_mean (just 1 variable, but 10% of the total variables)


# In conclusion, even though the full model is considering 20 variables more, the primary
# components of both models are highly correlated with almost the same variables (in the
# case of the second component it is exactly the same, and in the case of the first
# component, the full model just includes different statistical summaries of the same 
# measurements). This means that even after reducing the number of variables, the essential
# patterns of the data are still there and that the reduced model might be enough to
# 'explain' the data. 


# *This can be visualize using the following plots, where the closer an arrow is to
# the edge of the circle, the stronger its correlation with the principal components 
# (and the closer it is to the origin, the weaker the correlation). Arrows going to 
# the right, show that the variables are positively correlated with the first component, 
# while arrows going to the left, indicate negative correlation. On the other hand, 
# arrows going up show variables that are positively correlated with the second component, 
# while arrows going down, show negative corrrelation.

var_all <- plot(pca_all,choix="var") 
var_mean <- plot(pca_mean,choix="var") 

var_all + var_mean # Visualize them side by side
# As we can see the directions and length of the arrows are very similar in both cases:

# Radius, perimeter and area are pointing right and down. Strong positive correlation, 
# 0.8/0.9, with the first component and moderate negative correlation, -0.4/-0.5, with the
# second component. It makes sense that the three of them are close to each other since
# they are highly related between them.
# *In the case of smoothness and symmetry, even though they are not strongly correlated,
# their arrows are very similar. This could mean that they are linked through an 
# underlying pattern in the data or that they have a similar relationship with different
# variables.

# Concave points mean is pointing to the right with very little variation in the y axis, 
# and that is way the correlation with the first component is very high, 0.95, and 
# almost 0, with the second one.

# Finally, the fractal dimension mean is pointing up and just a bit to the right. This
# makes the correlation with the first component fairly low, 0.2, and the correlation
# with the second component very strong, 0.9.

# This just confirms the analysis done with the numerical values.

# *We have seen this as well, but is the same concept/idea (just to mention it):
# For a variable cos2 is the square of the correlation
(pca_all$var$cor)^2 #=cos2 for variables
(pca_mean$var$cor)^2 #=cos2 for variables


###################################
# PCA: INDIVIDUALS & VARIABLES
###################################

###################################
#### Individual Coordinates

# We start by checking the individual coordinates in the first two dimensions (this is,
# taking into account the first two components)
pca_all$ind$coord[ , 1:2] 
pca_mean$ind$coord[ , 1:2]

# As it is hard to interpret this data just by looking at the 'raw' numbers, we plot
# the individuals
ind_all <- plot(pca_all,choix="ind")
ind_mean <- plot(pca_mean,choix="ind")

ind_all + ind_mean # Visualize them side by side


###################################
#### Duality

# Duality is for checking the relation between a variable and a specific individual:

    # When an individual point is located in the direction of a variable arrow, 
    # the individual has a high value on that variable. If it is in the opposite direction,
    # it will have a low value. 

    # The distance is also important: the farther along the line an individual is, 
    # the larger their value for that variable and viceversa.


# For the full model:

# Create the scatter plot with vectors
plot(pca_all$var$coord, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (44.27%)", ylab = "Component 2 (18.97%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_all$var$coord[, 1], y1 = pca_all$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(pca_all$var$coord[, 1] + 0.1, pca_all$var$coord[, 2] + 0.1, 
     labels = row.names(pca_all$var$coord), cex = 0.6)

# Overlay the scatter plot of individual data points
points(pca_all$ind$coord[, 1], pca_all$ind$coord[, 2], pch = 16, col = "blue")
text(pca_all$ind$coord[, 1], pca_all$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Full' PCA Analysis: Individuals and Vectors")


# For the 'mean' model:

# Create the scatter plot with vectors
plot(pca_mean$var$coord, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (54.79%)", ylab = "Component 2 (25.19%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_mean$var$coord[, 1], y1 = pca_mean$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(pca_mean$var$coord[, 1] + 0.1, pca_mean$var$coord[, 2] + 0.1, 
     labels = row.names(pca_mean$var$coord), cex = 0.6)

# Overlay the scatter plot of individual data points
points(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2], pch = 16, col = "blue")
text(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Means' PCA Analysis: Individuals and Vectors")


# With this two plots we can see how each individual is related to different variables.
# However, the amount of individuals and variables (mostly in the case of the full
# model) makes an interpretation hard. To make things easier, we will visualize
# two plots to show that the previously mentioned 'principles' of duality are fulfilled:

# FIRST PLOT: color all the points that are above the mean of a variable, to prove that
# the direction of a variable’s arrow is related to an individual's value for that 
# variable.

# Select the variable
variable_to_check <- data$radius_mean # select whatever variable you want

# Calculate the mean of the variable
mean_fractal <- mean(variable_to_check)

# Check if the values are over or below the mean
above_mean <- variable_to_check > mean_fractal

# Create the scatter plot with vectors
plot(pca_mean$var$coord, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (54.79%)", ylab = "Component 2 (25.19%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_mean$var$coord[, 1], y1 = pca_mean$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(pca_mean$var$coord[, 1] + 0.1, pca_mean$var$coord[, 2] + 0.1, 
     labels = row.names(pca_mean$var$coord), cex = 0.6)

# Change the color depending on the value of the radius mean (above or below mean)
points(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2], pch = 16, 
       col = ifelse(above_mean, "red", "blue"))  # Red for above mean, blue for below mean
text(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Means' PCA Analysis: importance of direction")
legend("topright", legend = c("Above mean radius", "Below mean radius"),
       col = c("red", "blue"),          
       title = "Radius Category")


# SECOND PLOT: use a color scale to show that the farther an individual along the 
# line/arrow of a variable is, the larger its value for that variable, and viceversa.

# Calculate the mean for the previously selected variable
mean <- mean(variable_to_check)

# Create a color scale (the higher the value, the more red)
col_scale <- colorRampPalette(c("blue", "red"))

# Normalize the variable between 0 and 1 for coloring
norm_values <- (variable_to_check - min(variable_to_check)) / 
  (max(variable_to_check) - min(variable_to_check))

# For some reason, the plot doesn't visualize every point (itn is like zoomed), 
# so we have to manually change the limits of the plot:

# To change the axis limits
pca_x_min <- min(pca_mean$ind$coord[, 1])
pca_x_max <- max(pca_mean$ind$coord[, 1])
pca_y_min <- min(pca_mean$ind$coord[, 2])
pca_y_max <- max(pca_mean$ind$coord[, 2])

# Create the scatter plot with vectors
plot(pca_mean$var$coord, xlim = c(pca_x_min, pca_x_max), 
     ylim = c(pca_y_min, pca_y_max), pch = 16, cex = 0.3,
     xlab = "Component 1 (54.79%)", ylab = "Component 2 (25.19%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_mean$var$coord[, 1], y1 = pca_mean$var$coord[, 2], length = 0.1)

abline(h = 0, v = 0, lty = 2)
text(pca_mean$var$coord[, 1] + 0.1, pca_mean$var$coord[, 2] + 0.1, 
     labels = row.names(pca_mean$var$coord), cex = 0.6)

# Create the scatter plot with continuous color scale
points(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2], pch = 16, 
       col = col_scale(100)[as.numeric(cut(norm_values, breaks = 100))]) 
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Means' PCA Analysis: importance of distance")

# Create the legend
image.plot(legend.only = TRUE, 
           zlim = c(min(variable_to_check), max(variable_to_check)),
           col = col_scale(100),
           legend.width = 1,
           legend.shrink = 0.3,
           horizontal = TRUE,
           legend.mar = 2,
           smallplot = c(0.55, 0.85, 0.80, 0.83))  # Position inside plot area

# text(pca_mean$ind$coord[, 1], pca_mean$ind$coord[, 2] + 0.1, labels = rownames(data), cex = 0.4)
# we don't visualize the number of each individual as it doesn't help to see the 'trend'

# NOTE: they idea of creating this kind of plots has been ours (we realized that it 
# helped to understand the duality and relationship between individuals and variables).
# However, we had a limitation: we didn't know how to do it in R. So we used ChatGPT,
# and by explaining our situation and what we wanted, we were able to do it.


###################################
#### Individual Contribution

# We can aslo check each individual's contribution (in %) to each principle component
pca_all$ind$contrib[ , 1:2]
pca_mean$ind$contrib[ , 1:2]

# As the contribution measures how much an individual 'influences' the component, the 
# contributions that are farther from the center will have a higher contribution, while
# those closer to the center will have a lower contribution.

# We create a plot to visually show that this happens in our case:

# We get the individual coordinates 
coord <- pca_mean$ind$coord

# We extract the contribution for the selected component
contrib <- pca_mean$ind$contrib[, 1]  # Choose component 1 or 2

# We create the color scale
col_scale <- colorRampPalette(c("lightyellow", "red"))

# We scale the contributions (as they are already %, we just have to divide by 100)
point_colors <- col_scale(100)[as.numeric(cut(contrib / 100, breaks = 100))]

# We set the plot limits has we have the same problem has before 
pca_x_min <- min(coord[, 1]) - 1
pca_x_max <- max(coord[, 1]) + 1
pca_y_min <- min(coord[, 2]) - 1
pca_y_max <- max(coord[, 2]) + 1

# We plot the variable vectors
plot(pca_mean$var$coord, xlim = c(pca_x_min, pca_x_max), 
     ylim = c(pca_y_min, pca_y_max), pch = 16, cex = 0.3,
     xlab = "Component 1 (54.79%)", ylab = "Component 2 (25.19%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_mean$var$coord[, 1], y1 = pca_mean$var$coord[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)

# It is not necessary to visualize the names of the variables, as they don't provide 
# important information for this visualization
# text(pca_mean$var$coord[, 1] + 0.1, pca_mean$var$coord[, 2] + 0.1, 
#     labels = row.names(pca_mean$var$coord), cex = 0.6)

# We plot individuals with gradient colors based on their contribution to PC1
points(coord[, 1], coord[, 2], pch = 16, col = point_colors)
abline(h = 0, v = 0, lty = 2, col = "lightgray")
title(main = "'Means' PCA Analysis: Contribution to selected PC")

# Now, we are going to add a label to visualize the top and bottom contributors:

# Get row indices of top 6 and bottom 6 contributors
top6 <- order(contrib, decreasing = TRUE)[1:6]
bottom6 <- order(contrib, decreasing = FALSE)[1:6]

# Coordinates for labeling
coord <- pca_mean$ind$coord
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

# The most influential points (higher contributors) of component 1 are the ones that are 
# far from the center (origin), but precisely the ones that are far in the x axis.
# This is exactly what we were expecting, since the Component 1 is in the x axis. 

# On the other hand, the most influential points (higher contributors) of component 2 
# are the ones that are far from the center (origin), but now the ones that are far 
# in the y axis. Again, this is exactly what we were expecting, since the Component 2 
# is in the y axis.


# We can visualize the exact value of the individuals that contribute more to each 
# component:

# Full model, Component 1:
head(sort(pca_all$ind$contrib[ , 1], decreasing = T))

# Mean model, Component 1:
head(sort(pca_mean$ind$contrib[ , 1], decreasing = T))

# Full model, Component 2:
head(sort(pca_all$ind$contrib[ , 2], decreasing = T))

# Mean model, Component 2:
head(sort(pca_mean$ind$contrib[ , 2], decreasing = T))

# As we can see, the individuals with a higher contribution are almost identical in  
# both models.


# We can visualize as well the exact value of the individuals that contribute less to each 
# component:

# Full model, Component 1:
head(sort(pca_all$ind$contrib[ , 1]))

# Mean model, Component 1:
head(sort(pca_mean$ind$contrib[ , 1]))

# Full model, Component 2:
head(sort(pca_all$ind$contrib[ , 2]))

# Mean model, Component 2:
head(sort(pca_mean$ind$contrib[ , 2]))

# In this case, the individuals with a lower contribution are not similar in  
# both models


# Does it make sense that top contributors are almost identical but low contributors
# are not? YES:

# As top contributors are 'important' points that highly influence the data, they are
# constant in both models. On the other hand, low influential points contribute very
# little to the components, being more sensitive to small changes and noise.


# We can also check the individuals whose contribution is larger than the average 
# contribution (observations whose contribution is larger than 1/I ).

# We get the number of individuals
ind_count <- dim(pca_all$ind$contrib)[1]

# We calculate the 'thoretical' average contribution
avg_contrib <- 1 / ind_count * 100  

# We get individuals with contribution to PC1 that are higher than the average
high_contrib_ind <- which(pca_all$ind$contrib[, 1] > avg_contrib) # Select 1 or 2 for the
# desired component

# Visualize the individuals with higher contribution than the average (sorted)
sort(pca_all$ind$contrib[high_contrib_ind, 1], decreasing = T)


# Finally, we verify that each principle component's contributions sum up to 100%:
colSums(pca_all$ind$contrib[ , 1:2])
colSums(pca_mean$ind$contrib[ , 1:2])


###################################
#### Outliers

# In our case, the data is not standarized, so we have to slightly modify the approach

# Calculate the mean of each variable in the original data
center <- colMeans(data)

# Compute the Euclidean distance of each individual from the center. We have to substract 
# the center of each variable, and as the data is not standarized this is the mean of each
# variable (with standarized data that will be 0)
distances <- sqrt(apply(data, MARGIN = 1, FUN = function(x) sum((x - center)^2)))

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


###################################
#### cos2 For Individuals

# cos2 is the squared cosine for each principle component, calculated as (Dim.X/Dist)^2. 
# The closer it is to 1 for a given principle component, the better that principle 
# component is at capturing all the characteristics of that individual

# For all the individuals
cos_all <- (pca_all$ind$coord/pca_all$ind$dist)^2  #=cos2 for individuals
cos_mean <- (pca_mean$ind$coord/pca_mean$ind$dist)^2  #=cos2 for individuals

formatC(cos_all[ , 1:2], format = "f", digits = 4)
formatC(cos_mean[ , 1:2], format = "f", digits = 4)

# Individuals that are better represented with the first component
sorted_cos_all1 <- sort(cos_all[ , 1], decreasing = T)
head(sorted_cos_all1)

sorted_cos_mean1 <- sort(cos_mean[ , 1], decreasing = T)
head(sorted_cos_mean1)

# Individuals that are better represented with the second component
sorted_cos_all2 <- sort(cos_all[ , 2], decreasing = T)
head(sorted_cos_all2)

sorted_cos_mean2 <- sort(cos_mean[ , 2], decreasing = T)
head(sorted_cos_mean2)

# We can also compare their mean values
mean(sorted_cos_all1)
mean(sorted_cos_mean1)

mean(sorted_cos_all2)
mean(sorted_cos_mean2)

# Finally, we can check the total of the first two components combine (and their means)
cos_all_2D_total <- rowSums(cos_all[ , 1:2])
cos_mean_2D_total <- rowSums(cos_mean[ , 1:2])

mean(cos_all_2D_total)
mean(cos_mean_2D_total)


# As the first components of the 'means' model explains a higher percentage of the 
# variance than the first component of the full model, it makes sense that their values are closer 
# to 1 (same thing with the second component). Moreover, in both cases as the first 
# component explains more percentage of variance than the second component (that is why
# they are first and second components respectively), its values are closer to 1.


###################################
# PCA: PRINCIPAL COMPONENTS AND DIAGNOSIS
###################################

# Finally, we can't forget that we are analysing a dataset related to cancer, so we will
# check if the first two components have somehow separate cancer diagnosis (benign 
# and malign):

# Plot PCA individuals colored by diagnosis
fviz_pca_ind(pca_mean, 
             col.ind = diagnosis, 
             palette = c("blue", "red"),  # Malignant=red, Benign=blue
             legend.title = "Diagnosis",
             title = "PCA: Individuals Colored by Diagnosis (Mean Variables)") +
  theme_minimal()

# While component 2 doesn't seem to make a lof of difference regarding the diagnosis, we 
# can see that there is a clear separation between bening and maling diagnosis in the first 
# component: bening diagnosis are clearly related to low (negative) values of component 1.
# This makes sense, since the first component is highly correlated to variables such as
# radius or perimeter, which we already saw (during the descriptive analysis) that are 
# good indicators of the diagnosis (high radius was related to malign tumours).


# We will confirm this by getting the mean values of the coordinates by diagnosis:

# We create a dataframe with coordinates and diagnosis
pca_data <- data.frame(
  PC1 = pca_mean$ind$coord[, 1],
  PC2 = pca_mean$ind$coord[, 2],
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
p_value # The p-value is 3.134668e-120, confirming that the is a significant difference 
# between the PC1 for each diagnosis group.

# Fit the ANOVA model with the second component
model_aov2 <- aov(PC2 ~ diagnosis, data = pca_data)

# Check overall significance
anova_result2 <- summary(model_aov2)
p_value2 <- anova_result2[[1]]$"Pr(>F)"[1]
p_value2 # The p-value is 0.0008018806, confirming that the is a significant difference 
# between the PC2 for each diagnosis group.

# Although both statistical tests confirm that there is a difference between the
# diagnosis group in both PC1 and PC2, the p-values show that in the case of PC1 this
# difference is much bigger.

