# CORRESPONDANCE ANALYSIS (PCA) OF THE PROSTATE CANCER DATASET 
# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

#############
# LIBRARIES #
#############

# We load & install the libraries we will use to create the model:
# install.packages("FactoMineR")
library(FactoMineR)

##############################
# DATASET LOADING & CHECKING #
##############################

data <- read.csv("./Prostate_Cancer.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)

##########################################
# VARIABLE SELECTION & CONTINGENCY TABLE #
##########################################

# In order to perform correspondance analysis we need to create new variables that is fit for it.
# To do this, we will select variables from our original dataset and transform them into categorial
# variables that can be used in correspondance analysis.
# As to how this will be done, we will select 3 variables, radius, smoothness and perimeter, then we will transform
# these variables into levels ("low", "medium" and "high"), this will be achieved by using what we have
# determined to be a reasonable method to separate those categories, that method is using the standard
# deviation of the data for the column and separating at the point where 1 stand deviation is reached on
# the low and high ends of the data, giving us "low" for everything below -1 standard deviation, "normal"
# for everything between -1 and 1 standard deviations and "high" for everything above 1 standard deviation

# We select the 3 variables from the dataset
sdata <- data[, c("radius", "smoothness", "perimeter")]
sdata

# We create a function to transform the values into levels
toLevels <- function(x) {
  u <- mean(x)
  o <- sd(x)
  result <- ifelse(x < u - o, "low", ifelse(x > u + o, "high", "normal"))
  return(result)
}

# We apply the function
sdata2 <- as.data.frame(lapply(sdata, toLevels))
sdata2

# We have successfully transfromed the values into levels, now we will count each of those values
# to transform them into categorical values and create a contingency table.

# We create a function to count the times each value appears
toCategoricalTable <- function(x) {
  as.vector(table(factor(x, levels = c("low", "normal", "high"))))
}

# Apply the function and transpose to the correct form
cadata <- t(as.data.frame(lapply(sdata2, toCategoricalTable)))

# Change column names
colnames(cadata) <- c("low", "normal", "high")
cadata

# CHI SQUARE TEST
# Remember in this test we check whether the data is different from the independence model.
# If the data is similar to the independence model, variables are independent, and the opposite
x2test<-chisq.test(cadata)

# The expected values given by the independent model are:
round(x2test$expected,0)
# The observed values are:
x2test$observed
# We can see that there is considerable difference.

# Is the difference big enough?
# Let's compute the pvalue:
p.val <- x2test$p.value
p.val
# The pvalue is tiny, thus the test fails, meaning that the independence model cannot explain the data.
# Therefore the variables are not independent.

# This is the contribution of each cell to the chisquare
x2test$residuals^2
# And this is the relative contribution
(x2test$residuals^2)/x2test$statistic


# ROW AND COLUMN PROFILES

# Let us now compute the row and col profiles

# Row profiles
dd <- rbind(cadata,apply(cadata[,1: nrow(cadata)],2,sum))
rownames(dd)[nrow(cadata) + 1] <- "Mean profile"
round(prop.table(as.matrix(dd),margin=1),2)

# Col profiles
dd <- cbind(cadata,apply(cadata[,1: ncol(cadata)],1,sum))
colnames(dd)[ncol(cadata) + 1] <- "Mean profile"
round(prop.table(as.matrix(dd),margin=2),2)


# FULL CA
# The function to perform Correspondence Analysis is simply CA
cawom <- CA(cadata)

# we visualize both row and column profiles by 
plot(cawom)

# One can visualize only the rows or only the columns with
plot(cawom,invisible="col")
plot(cawom,invisible="row")

# The main indicators can be obtained with
summary(cawom)
