# CORRESPONDENCE ANALYSIS (CA) OF THE WISCONSIN BREAST CANCER DATASET
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# Motivation:
# We want to study the association between two categorical variables in the breast cancer dataset:
# 'radius_mean_cat' and 'smoothness_mean_cat', both categorized into four groups (very low, low, high, very high).
# We will use Correspondence Analysis (CA) to reduce the dimensionality and visualize their relationships.


# We load the libraries we will used to create the model:
install.packages("FactoMineR")
library(FactoMineR)


###################################
# DATASET LOADING & CHECKING
###################################

data <- read.csv("sample_data/breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


###################################
# CATEGORY CREATION (QUARTILES)
###################################

# We transform the continuous variables into categorical ones using quartiles
# so we can use them in Correspondence Analysis.

# Create a function to label categories based on quartiles
categorize_quartiles_radius <- function(x) {
  cut(x,
      breaks = quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("very low radius", "low radius", "high radius", "very high radius"),
      include.lowest = TRUE)
}

categorize_quartiles_smoothness <- function(x) {
  cut(x,
      breaks = quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("very smooth", "smooth", "Almost not smooth", "Not Smooth"),
      include.lowest = TRUE)
}

# Apply the categorization to the selected variables
data$radius_mean_cat <- categorize_quartiles_radius(data$radius_mean)
data$smoothness_mean_cat <- categorize_quartiles_smoothness(data$smoothness_mean)


###################################
# CA: VARIABLE SELECTION
###################################

# In this part of the analysis, we are focusing on the association between two categorical
# variables: 'radius_mean_cat' and 'smoothness_mean_cat'. Each of these variables classifies
# continuous measurements into four categories (very low, low, high, very high).

# Initially, we considered different options for the CA:
# - We could have used other categorical variables or even discretized more variables
#   into categories.
# - However, we chose 'radius_mean_cat' and 'smoothness_mean_cat' because they reflect
#   key physical characteristics of the tumor (size and texture) and were already
#   categorized, making them suitable for correspondence analysis.

# By choosing variables that are derived from fundamental measurements, we ensure that
# the interpretation of the association is meaningful in the medical context.

# Therefore, our procedure is:
# - Build a contingency table of the two selected variables.
# - Perform a Chi-square test to check for independence.
# - Apply CA to visualize and interpret the relationships between the categories.
# Explore the structure of relevant variables

# We do a quick observation of bot variables we are gonna use
str(data$radius_mean_cat)
str(data$smoothness_mean_cat)

# The categories ("very low", "low", "high", "very high") for each variable were created
# using quartiles. Here's what they mean:

# For 'radius_mean_cat':
# - "very low" = values in the lowest 25% of radius_mean (smallest tumors)
# - "low"      = values between the 25th and 50th percentile
# - "high"     = values between the 50th and 75th percentile
# - "very high"= values in the highest 25% of radius_mean (largest tumors)

# For 'smoothness_mean_cat':
# - "very low" = values in the lowest 25% of smoothness_mean (least textured tumors)
# - "low"      = values between the 25th and 50th percentile
# - "high"     = values between the 50th and 75th percentile
# - "very high"= values in the highest 25% of smoothness_mean (most textured tumors)

# These categories help us interpret the Correspondence Analysis in terms of real
# physical tumor characteristics. For example:
# - A point labeled "very high" in radius means the tumor has a very large mean radius.
# - A point labeled "low" in smoothness means the tumor surface is relatively smooth.

###############################################
# DATASET CONTINGENCY TABLE & EXPECTED TABLE
###############################################

contingency_table <- table(data$radius_mean_cat, data$smoothness_mean_cat)
print(contingency_table)


expected_table <- chisq.test(contingency_table)$expected
print(expected_table)

# To understand the association between the two variables, we compare the observed
# contingency table with the expected table under the assumption of independence.

# - If an observed count is higher than the expected count, it suggests that the two
#   categories are positively associated (they "attract" each other).
# - If an observed count is lower than the expected count, it suggests that the two
#   categories are negatively associated (they "repel" or "oppose" each other).
# - If the observed count is similar to the expected count, it indicates little to no
#   association between those two categories.

# These patterns help us interpret the chi-square test result and also prepare for
# the graphical interpretation through Correspondence Analysis (CA).

# Comparing the observed contingency table with the expected table, we can draw some conclusions:

# - For the pair (very high, very high), the observed value (45) is clearly higher than the expected (35.44),
#   suggesting a strong positive association between radius_mean_cat = very high and smoothness_mean_cat = very high.

# - Conversely, for the pair (very high, very low), the observed value (17) is much lower than expected (35.69),
#   indicating a negative association: it is unlikely for samples with a very high radius mean to have a very low smoothness mean.

# - Similarly, the pair (low, very low) shows a slight positive association (observed 51 vs expected 35.69),
#   and the pair (very low, very high) shows a negative association (observed 33 vs expected 35.69).

# - Other cells are closer to the expected values, implying weaker or no strong association between the categories.

# These differences may be more clear when we perform the Chi-square test and check the p-value.


####################
# Chi-square Test
####################

chi_test <- chisq.test(contingency_table)
print(chi_test)

# Interpretation of Chi-square test:
# The p-value is very small (p < 0.05), so we reject the null hypothesis of independence.
# This suggests that there is a significant relationship between 'radius_mean_cat' and 'smoothness_mean_cat'.



##################################
# Apply correspondence analysis
##################################

ca_result <- CA(contingency_table, graph = FALSE)

summary(ca_result)

# Step 5: Plot the projections
plot(ca_result)

# Interpretation of the CA plot:
# - Categories that are close together are more frequently associated.
# - Distant categories are less related.
# - We check how much variance (inertia) is explained by the first two dimensions.
#   If it is high (e.g., >60%), then a 2D plot is sufficient for interpretation.

# Main conclusions:
# - The CA shows clear groupings between certain categories of 'radius_mean_cat' and 'smoothness_mean_cat'.
# - The Chi-square test confirms that these variables are not independent.
# - The visualization suggests specific associations that can guide further analysis or model building.
